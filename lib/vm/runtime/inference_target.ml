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


type entrypoint = {
  entry_name : string;
  entry_label : int;
}

type t = {
  program_root : string;
  requirement_root : string;
  model_root : string;
  execution_descriptor_root : string;
  store_root : string;
  session_abi_root : string;
  entrypoints : entrypoint list;
}

type error =
  | Bad_root of string
  | Bad_name of string
  | Bad_entrypoint of string * int
  | Program_provenance_unsupported of Admission.provenance
  | Session_abi_root_mismatch of string * string
  | Missing_advance_entrypoint
  | Unexpected_entrypoint of string * int
  | Advance_label_mismatch of int * int
  | Program_root_mismatch of string * string
  | Missing_requirement
  | Requirement_root_mismatch of string * string

let hex = function
  | '0' .. '9'
  | 'a' .. 'f' -> true
  | _ -> false

let valid_root value =
  String.length value = 64 && String.for_all hex value

let name_char = function
  | 'a' .. 'z'
  | '0' .. '9'
  | '.'
  | '_'
  | '-' -> true
  | _ -> false

let valid_name value =
  value <> "" && String.for_all name_char value

let check_root value =
  if valid_root value then Ok () else Error (Bad_root value)

let check_name value =
  if valid_name value then Ok () else Error (Bad_name value)

let entrypoint_order left right =
  match String.compare left.entry_name right.entry_name with
  | 0 -> compare left.entry_label right.entry_label
  | order -> order

let sort_entrypoints entrypoints =
  List.sort entrypoint_order entrypoints

let entrypoint_json entrypoint =
  `Assoc [
    "name", `String entrypoint.entry_name;
    "label", `Int entrypoint.entry_label;
  ]

let to_json target =
  `Assoc [
    "program_root", `String target.program_root;
    "requirement_root", `String target.requirement_root;
    "model_root", `String target.model_root;
    "execution_descriptor_root", `String target.execution_descriptor_root;
    "store_root", `String target.store_root;
    "session_abi_root", `String target.session_abi_root;
    "entrypoints",
    `List (List.map entrypoint_json (sort_entrypoints target.entrypoints));
  ]

let root target =
  let payload = Yojson.Safe.to_string (to_json target) in
  Digestif.SHA256.(
    digest_string ("octra:inference:target\000" ^ payload) |> to_hex)

let program_root admitted =
  Digestif.SHA256.(
    digest_string
      ("octra:inference:program\000"
       ^ Bytecode.encode (Admission.code admitted))
    |> to_hex)

let rec check_entrypoint_names = function
  | [] -> Ok ()
  | entrypoint :: rest ->
    match check_name entrypoint.entry_name with
    | Error error -> Error error
    | Ok () ->
      if entrypoint.entry_label < 0 then
        Error (Bad_entrypoint (entrypoint.entry_name, entrypoint.entry_label))
      else
        check_entrypoint_names rest

let check_advance_entrypoint entrypoints =
  let canonical entrypoint =
    String.equal entrypoint.entry_name Inference_session_abi.advance_entrypoint
    && entrypoint.entry_label = Inference_session_abi.advance_label
  in
  match check_entrypoint_names entrypoints with
  | Error error -> Error error
  | Ok () ->
    match entrypoints with
    | [] -> Error Missing_advance_entrypoint
    | [{ entry_name; entry_label }]
      when String.equal entry_name Inference_session_abi.advance_entrypoint ->
      if entry_label = Inference_session_abi.advance_label then Ok ()
      else
        Error
          (Advance_label_mismatch
             (entry_label, Inference_session_abi.advance_label))
    | [{ entry_name; entry_label }] ->
      Error (Unexpected_entrypoint (entry_name, entry_label))
    | entrypoint :: _ ->
      let entrypoint =
        match
          List.find_opt
            (fun entrypoint -> not (canonical entrypoint))
            entrypoints
        with
        | Some entrypoint -> entrypoint
        | None -> entrypoint
      in
      Error
        (Unexpected_entrypoint
           (entrypoint.entry_name, entrypoint.entry_label))

let validate target =
  match check_root target.program_root with
  | Error error -> Error error
  | Ok () ->
    (match check_root target.requirement_root with
     | Error error -> Error error
     | Ok () ->
       (match check_root target.model_root with
        | Error error -> Error error
        | Ok () ->
          (match check_root target.execution_descriptor_root with
           | Error error -> Error error
           | Ok () ->
             (match check_root target.store_root with
              | Error error -> Error error
              | Ok () ->
                (match check_root target.session_abi_root with
                 | Error error -> Error error
                 | Ok () ->
                   if not
                       (Inference_session_abi.supported_root
                          target.session_abi_root)
                   then
                     Error
                       (Session_abi_root_mismatch
                          ( target.session_abi_root,
                            Inference_session_abi.supported_root_message ))
                   else
                     check_advance_entrypoint target.entrypoints)))))

let jdest_labels code =
  Array.fold_left
    (fun labels instr ->
      match instr with
      | Contract_vm.JDEST label -> label :: labels
      | _ -> labels)
    []
    code

let check_advance_label code =
  let labels = jdest_labels code in
  if List.mem Inference_session_abi.advance_label labels then Ok ()
  else
    Error
      (Bad_entrypoint
         (Inference_session_abi.advance_entrypoint,
          Inference_session_abi.advance_label))

let check ~admitted target =
  match validate target with
  | Error error -> Error error
  | Ok () ->
    let actual_program_root = program_root admitted in
    if not (String.equal target.program_root actual_program_root) then
      Error (Program_root_mismatch (target.program_root, actual_program_root))
    else
      match Admission.requirement admitted with
      | None -> Error Missing_requirement
      | Some requirement ->
        let actual_requirement_root = Execution_requirement.root requirement in
        if not (String.equal target.requirement_root actual_requirement_root) then
          Error (Requirement_root_mismatch
                   (target.requirement_root, actual_requirement_root))
        else
          match Admission.provenance admitted with
          | Admission.Raw_code ->
            Error (Program_provenance_unsupported Admission.Raw_code)
          | Admission.Checked_envelope
          | Admission.Attested_envelope ->
            check_advance_label (Admission.code admitted)

let error_message = function
  | Bad_root root -> Printf.sprintf "invalid root: %s" root
  | Bad_name name -> Printf.sprintf "invalid entrypoint name: %s" name
  | Bad_entrypoint (name, label) ->
    Printf.sprintf "invalid entrypoint %s: %d" name label
  | Program_provenance_unsupported provenance ->
    Printf.sprintf
      "unsupported inference program provenance: %s"
      (Admission.provenance_name provenance)
  | Session_abi_root_mismatch (actual, expected) ->
    Printf.sprintf
      "target session ABI root mismatch: actual %s expected %s"
      actual expected
  | Missing_advance_entrypoint ->
    "missing advance entrypoint"
  | Unexpected_entrypoint (name, label) ->
    Printf.sprintf "unexpected entrypoint %s: %d" name label
  | Advance_label_mismatch (actual, expected) ->
    Printf.sprintf
      "advance label mismatch: actual %d expected %d"
      actual expected
  | Program_root_mismatch (expected, actual) ->
    Printf.sprintf
      "target program root mismatch: expected %s actual %s"
      expected actual
  | Missing_requirement ->
    "admitted program has no execution requirement"
  | Requirement_root_mismatch (expected, actual) ->
    Printf.sprintf
      "target requirement root mismatch: expected %s actual %s"
      expected actual
