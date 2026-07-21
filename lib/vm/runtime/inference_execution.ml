(*
Octra Labs 2026

Lite node, for internal use only (pre-release build 0x1067dzc2)

Include at startup:
- compiler
- env-constructor
- binary-proto consensus for updates
- PVAC (optimized version, build 0f24dd-2025)
- libp2p
- gRPC (version 9738fdy44-2025)
*)


type result = {
  effort_used : int;
  output_root : string;
  candidate_root : string;
}

type error =
  | Entrypoint_unsupported of string
  | Entrypoint_missing of int
  | Opaque_value
  | Invalid_output of string
  | Missing_output_cell of int
  | Output_limit_exceeded of int * int
  | Execution_failed

let sha256 raw =
  Digestif.SHA256.(digest_string raw |> to_hex)

let entrypoint_pc code label =
  let rec loop pc =
    if pc = Array.length code then None
    else
      match code.(pc) with
      | Contract_vm.JDEST value when value = label -> Some pc
      | _ -> loop (pc + 1)
  in
  loop 0

let length_prefix value =
  string_of_int (String.length value) ^ ":" ^ value

let value_payload = function
  | Contract_vm.VInt value -> Ok ("int:" ^ Z.to_string value)
  | Contract_vm.VBool value -> Ok ("bool:" ^ if value then "1" else "0")
  | Contract_vm.VString value -> Ok ("string:" ^ length_prefix value)
  | Contract_vm.VBytes value -> Ok ("bytes:" ^ length_prefix value)
  | Contract_vm.VBytes32 value -> Ok ("bytes32:" ^ length_prefix value)
  | Contract_vm.VU64 value -> Ok ("u64:" ^ Z.to_string value)
  | Contract_vm.VU128 value -> Ok ("u128:" ^ Z.to_string value)
  | Contract_vm.VU256 value -> Ok ("u256:" ^ Z.to_string value)
  | Contract_vm.VAddr value -> Ok ("address:" ^ length_prefix value)
  | Contract_vm.VCipher _
  | Contract_vm.VPubKey _ -> Error Opaque_value

let sorted_bindings table compare_key =
  Hashtbl.fold (fun key value values -> (key, value) :: values) table []
  |> List.sort (fun (left, _) (right, _) -> compare_key left right)

let bindings_payload ~key_payload bindings value_payload =
  let rec loop acc = function
    | [] -> Ok (String.concat "|" (List.rev acc))
    | (key, value) :: rest ->
      (match value_payload value with
       | Error error -> Error error
       | Ok value ->
         loop
           ((length_prefix (key_payload key) ^ ":" ^ value) :: acc)
           rest)
  in
  loop [] bindings

let memory_payload state =
  let bindings = sorted_bindings state.Contract_vm.memory.data compare in
  bindings_payload
    ~key_payload:string_of_int
    bindings
    (fun value -> value_payload value)

let register_payload state =
  let bindings =
    Array.to_list (Array.mapi (fun index value -> index, value) state.Contract_vm.regs)
  in
  bindings_payload
    ~key_payload:string_of_int
    bindings
    (fun value -> value_payload value)

let storage_payload state =
  let bindings = sorted_bindings state.Contract_vm.storage String.compare in
  bindings_payload
    ~key_payload:(fun value -> value)
    bindings
    (fun value -> Ok ("storage:" ^ length_prefix value))

let blob_payload state =
  let bindings = sorted_bindings state.Contract_vm.blobs String.compare in
  bindings_payload
    ~key_payload:(fun value -> value)
    bindings
    (fun value ->
      Ok
        ("blob:" ^ string_of_int (String.length value) ^ ":" ^ sha256 value))

let candidate_payload state =
  match
    memory_payload state,
    register_payload state,
    storage_payload state,
    blob_payload state
  with
  | Ok memory, Ok registers, Ok storage, Ok blobs ->
    Ok
      (String.concat
         "|"
         [
           "pc=" ^ string_of_int state.Contract_vm.pc;
           "effort=" ^ string_of_int state.Contract_vm.effort_used;
           "memory=" ^ memory;
           "registers=" ^ registers;
           "storage=" ^ storage;
           "blobs=" ^ blobs;
         ])
  | Error error, _, _, _
  | _, Error error, _, _
  | _, _, Error error, _
  | _, _, _, Error error -> Error error

let candidate_root ~target_root payload =
  Digestif.SHA256.(
    digest_string
      ("octra:inference:candidate\000" ^ target_root ^ "\000" ^ payload)
    |> to_hex)

let output_integer = function
  | Contract_vm.VInt value
  | Contract_vm.VU64 value
  | Contract_vm.VU128 value
  | Contract_vm.VU256 value
    when Z.fits_int value -> Some (Z.to_int value)
  | _ -> None

(* The session ABI reserves r0 for the output base and r1 for the number of
   memory cells in the canonical output. Scratch memory and other registers
   remain candidate state, not output state. *)
let output_payload state ~max_bytes =
  match output_integer state.Contract_vm.regs.(0),
        output_integer state.Contract_vm.regs.(1) with
  | None, _
  | _, None -> Error (Invalid_output "r0/r1 must contain integer output bounds")
  | Some base, Some length when base < 0 || length < 0 ->
    Error (Invalid_output "output bounds must be non-negative")
  | Some base, Some length when base > max_int - length ->
    Error (Invalid_output "output span overflows the VM address space")
  | Some _, Some length when length > max_bytes ->
    Error (Output_limit_exceeded (length, max_bytes))
  | Some base, Some length ->
    let rec read_values index acc =
      if index = length then Ok (List.rev acc)
      else
        let cell = base + index in
        match Hashtbl.find_opt state.Contract_vm.memory.data cell with
        | None -> Error (Missing_output_cell cell)
        | Some value ->
          (match value_payload value with
           | Error error -> Error error
           | Ok value -> read_values (index + 1) (value :: acc))
    in
    (match read_values 0 [] with
     | Error error -> Error error
     | Ok values ->
       let payload =
         String.concat
           "|"
           [
             "base=" ^ string_of_int base;
             "length=" ^ string_of_int length;
             "values=" ^ String.concat "," values;
           ]
       in
       if String.length payload > max_bytes then
         Error (Output_limit_exceeded (String.length payload, max_bytes))
       else Ok payload)

let output_root ~target_root ~session_abi_root payload =
  Digestif.SHA256.(
    digest_string
      ("octra:inference:output\000"
       ^ target_root ^ "\000" ^ session_abi_root ^ "\000" ^ payload)
    |> to_hex)

let add_pins state pins =
  List.iter
    (fun (range : Inference_store.pinned_range) ->
      Hashtbl.replace state.Contract_vm.blobs range.range_root range.bytes)
    (Inference_store.ranges pins)

let run ~plan () =
  let admitted = Inference_plan.admitted plan in
  let target = Inference_plan.target plan in
  let request = Inference_plan.request plan in
  let pins = Inference_plan.pins plan in
  match Inference_target.entry_label target request.entrypoint with
  | None -> Error (Entrypoint_unsupported request.entrypoint)
  | Some label ->
    (match entrypoint_pc (Admission.code admitted) label with
     | None -> Error (Entrypoint_missing label)
     | Some pc ->
       let state =
         Contract_vm.create_state
           ~limit:request.Inference_request.max_advance_effort
           ~strict_values:true
           ~strict_blobs:true
           ~caller:"oct11111111111111111111111111111111111111111111"
           ~origin:"oct11111111111111111111111111111111111111111111"
           ~address:"oct22222222222222222222222222222222222222222222"
           ~value:Z.zero
           ~storage:(Hashtbl.create 16)
           ()
       in
       add_pins state pins;
       Hashtbl.replace
         state.Contract_vm.blobs
         request.Inference_request.input_root
         (Inference_plan.input plan);
       let fixed = Contract.fix_jumps (Admission.code admitted) in
       state.Contract_vm.pc <- pc;
       if not (Contract_vm.run state fixed) || state.Contract_vm.reverted then
         Error Execution_failed
       else
        (match
           output_payload
             state
             ~max_bytes:request.Inference_request.max_output_bytes,
           candidate_payload state
         with
         | Ok output, Ok candidate ->
            Ok {
              effort_used = state.Contract_vm.effort_used;
              output_root =
                output_root
                  ~target_root:(Inference_target.root target)
                  ~session_abi_root:target.Inference_target.session_abi_root
                  output;
              candidate_root =
                candidate_root
                  ~target_root:(Inference_target.root target)
                  candidate;
            }
          | Error error, _
          | _, Error error -> Error error))

let error_message = function
  | Entrypoint_unsupported name ->
    Printf.sprintf "unsupported inference entrypoint: %s" name
  | Entrypoint_missing label ->
    Printf.sprintf "missing inference entrypoint label: %d" label
  | Opaque_value ->
    "inference execution encountered an opaque value"
  | Invalid_output error -> "invalid inference output: " ^ error
  | Missing_output_cell cell ->
    Printf.sprintf "inference output cell is not initialized: %d" cell
  | Output_limit_exceeded (required, available) ->
    Printf.sprintf
      "inference output exceeds limit: required %d available %d"
      required available
  | Execution_failed -> "inference execution failed"
