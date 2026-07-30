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

module VM = Octra_vm.Contract_vm

let template_index = ref None
let strict_effort = ref false

let fail message =
  prerr_endline message;
  exit 1

let args = [
  "--template-index",
  Arg.String (fun value -> template_index := Some value),
  "producer template index json";
  "--strict-effort",
  Arg.Set strict_effort,
  "fail when observed VM effort differs from expected_effort";
]

let usage =
  "inference_conformance_run --template-index <path> [--strict-effort]"

let read_file path =
  let input = open_in_bin path in
  Fun.protect
    ~finally:(fun () -> close_in_noerr input)
    (fun () ->
       let len = in_channel_length input in
       really_input_string input len)

let read_json path =
  try Yojson.Safe.from_file path with
  | Sys_error message -> fail message
  | Yojson.Json_error message ->
    fail ("invalid json " ^ path ^ ": " ^ message)

let sha256 raw =
  Digestif.SHA256.(digest_string raw |> to_hex)

let field name fields =
  match List.filter (fun (key, _) -> String.equal key name) fields with
  | [(_, value)] -> Some value
  | _ -> None

let string_field name fields =
  match field name fields with
  | Some (`String value) -> value
  | _ -> fail ("missing string field: " ^ name)

let int_field name fields =
  match field name fields with
  | Some (`Int value) -> value
  | Some (`Intlit value) -> int_of_string value
  | _ -> fail ("missing int field: " ^ name)

let assoc_field name fields =
  match field name fields with
  | Some (`Assoc values) -> values
  | _ -> fail ("missing object field: " ^ name)

let list_field name fields =
  match field name fields with
  | Some (`List values) -> values
  | _ -> fail ("missing list field: " ^ name)

let opt_string_field name fields =
  match field name fields with
  | Some (`String value) -> Some value
  | _ -> None

let opt_int_field name fields =
  match field name fields with
  | Some (`Int value) -> Some value
  | Some (`Intlit value) -> Some (int_of_string value)
  | Some `Null
  | None -> None
  | _ -> fail ("invalid int field: " ^ name)

let register_index value =
  let len = String.length value in
  if len < 2 || value.[0] <> 'r' then
    fail ("invalid register name: " ^ value);
  let index = int_of_string (String.sub value 1 (len - 1)) in
  if index < 0 || index > 63 then
    fail ("register out of range: " ^ value);
  index

let int64_le raw offset =
  let value = ref 0L in
  for byte = 0 to 7 do
    value :=
      Int64.logor
        !value
        (Int64.shift_left
           (Int64.of_int (Char.code raw.[offset + byte]))
           (byte * 8))
  done;
  !value

let put_int64_le buffer index value =
  for byte = 0 to 7 do
    Bytes.set
      buffer
      ((index * 8) + byte)
      (Char.chr
         (Int64.to_int
            (Int64.logand
               (Int64.shift_right_logical value (byte * 8))
               0xffL)))
  done

let set_f64le state base cells raw =
  if String.length raw <> cells * 8 then
    fail
      (Printf.sprintf
         "f64 fixture length mismatch at base %d: cells %d bytes %d"
         base
         cells
         (String.length raw));
  for index = 0 to cells - 1 do
    Hashtbl.replace
      state.VM.memory.data
      (base + index)
      (VM.VInt (Z.of_int64 (int64_le raw (index * 8))))
  done

let output_bytes state base cells =
  let raw = Bytes.create (cells * 8) in
  for index = 0 to cells - 1 do
    let bits =
      match Hashtbl.find_opt state.VM.memory.data (base + index) with
      | Some (VM.VInt value) when Z.fits_int64 value -> Z.to_int64 value
      | _ ->
        fail
          (Printf.sprintf
             "missing output cell: base %d index %d"
             base
             index)
    in
    put_int64_le raw index bits
  done;
  Bytes.to_string raw

let state () =
  VM.create_state
    ~limit:1_000_000_000
    ~strict_values:true
    ~caller:"caller"
    ~origin:"origin"
    ~address:"contract"
    ~value:Z.zero
    ~storage:(Hashtbl.create 0)
    ()

let set_int_reg state reg value =
  state.VM.regs.(reg) <- VM.VInt (Z.of_int value)

let set_raw_reg state reg raw =
  state.VM.regs.(reg) <- VM.VString raw

let value_for name values =
  int_field name values

let reg_for name registers =
  register_index (string_field name registers)

let load_inputs root_dir state fields registers =
  list_field "input_memory_ranges" fields
  |> List.iter (function
    | `Assoc range_fields ->
      let name = string_field "name" range_fields in
      let source = assoc_field "source" range_fields in
      let path = Filename.concat root_dir (string_field "path" source) in
      let expected_bytes = int_field "bytes" source in
      let expected_sha = string_field "sha256" source in
      let raw =
        try read_file path with
        | Sys_error message -> fail message
      in
      if String.length raw <> expected_bytes then
        fail
          (Printf.sprintf
             "%s: byte length expected %d actual %d"
             name
             expected_bytes
             (String.length raw));
      let actual_sha = sha256 raw in
      if not (String.equal actual_sha expected_sha) then
        fail
          (Printf.sprintf
             "%s: sha256 expected %s actual %s"
             name
             expected_sha
             actual_sha);
      let memory = assoc_field "vm_memory" range_fields in
      (match opt_int_field "length_f64_cells" memory with
       | Some cells ->
         set_f64le state (int_field "base_address" memory) cells raw
       | None ->
         set_raw_reg state (reg_for name registers) raw)
    | _ -> fail "input_memory_ranges entries must be objects")

let set_registers state registers values =
  List.iter
    (fun (name, reg_value) ->
       let reg =
         match reg_value with
         | `String value -> register_index value
         | _ -> fail ("register binding must be a string: " ^ name)
       in
       match field name values with
       | Some (`Int value) -> set_int_reg state reg value
       | Some (`Intlit value) -> set_int_reg state reg (int_of_string value)
       | None -> ()
       | _ -> fail ("register value must be an int: " ^ name))
    registers

let op_linear registers =
  VM.LINEAR_Q1_G128_FP
    (reg_for "dst" registers,
     reg_for "lhs" registers,
     reg_for "q1_owner" registers,
     reg_for "byte_offset" registers,
     reg_for "m" registers,
     reg_for "k" registers,
     reg_for "n" registers)

let op_rmsnorm registers =
  VM.RMSNORM_FP_EPS
    (reg_for "addr" registers,
     reg_for "count" registers,
     reg_for "gamma" registers,
     reg_for "epsilon_bits" registers)

let op_l2norm registers =
  VM.L2NORM_FP
    (reg_for "addr" registers,
     reg_for "count" registers,
     reg_for "epsilon_bits" registers)

let op_softmax registers =
  VM.SOFTMAX_FP
    (reg_for "dst" registers,
     reg_for "scores" registers,
     reg_for "count" registers)

let op_gated_delta registers =
  VM.GATED_DELTA_RULE_FP
    (reg_for "output" registers,
     reg_for "state_dst" registers,
     reg_for "q" registers,
     reg_for "k" registers,
     reg_for "v" registers,
     reg_for "log_decay" registers,
     reg_for "beta" registers,
     reg_for "state" registers,
     reg_for "timesteps" registers,
     reg_for "q_heads" registers,
     reg_for "k_heads" registers,
     reg_for "v_heads" registers,
     reg_for "key_dim" registers,
     reg_for "value_dim" registers)

let op_for opcode registers =
  match opcode with
  | "LINEAR_Q1_G128_FP" -> op_linear registers
  | "RMSNORM_FP_EPS" -> op_rmsnorm registers
  | "L2NORM_FP" -> op_l2norm registers
  | "SOFTMAX_FP" -> op_softmax registers
  | "GATED_DELTA_RULE_FP" -> op_gated_delta registers
  | value -> fail ("unsupported opcode: " ^ value)

let subspan_result state value =
  match value with
  | `Assoc fields ->
    let name = string_field "name" fields in
    let base = int_field "base_address" fields in
    let cells = int_field "length_f64_cells" fields in
    let expected_sha = string_field "sha256" fields in
    let expected_root = opt_string_field "root" fields in
    let raw = output_bytes state base cells in
    let actual_sha = sha256 raw in
    let matched = String.equal actual_sha expected_sha in
    matched,
    `Assoc [
      "name", `String name;
      "base_address", `Int base;
      "length_f64_cells", `Int cells;
      "expected_sha256", `String expected_sha;
      "observed_sha256", `String actual_sha;
      "expected_root",
      (match expected_root with None -> `Null | Some root -> `String root);
      "matched", `Bool matched;
    ]
  | _ -> fail "output subspan must be an object"

let execute_template root_dir entry =
  let opcode = string_field "opcode" entry in
  let template_path = string_field "vm_execution_template" entry in
  let full_template_path = Filename.concat root_dir template_path in
  let template =
    match read_json full_template_path with
    | `Assoc fields -> fields
    | _ -> fail (full_template_path ^ ": template must be an object")
  in
  let expected_effort = int_field "expected_effort" template in
  let params = assoc_field "parameter_addresses_and_scalar_params" template in
  let registers = assoc_field "registers" params in
  let values = assoc_field "values" params in
  let state = state () in
  load_inputs root_dir state template registers;
  set_registers state registers values;
  let op = op_for opcode registers in
  let ran = VM.run state [|op; VM.STOP|] in
  let output = assoc_field "output" template in
  let subspans = list_field "subspans" output in
  let span_results =
    if ran then List.map (subspan_result state) subspans
    else
      List.map
        (function
          | `Assoc fields ->
            false,
            `Assoc [
              "name", `String (string_field "name" fields);
              "matched", `Bool false;
              "error", `String "vm_run_failed";
            ]
          | _ -> fail "output subspan must be an object")
        subspans
  in
  let spans_matched = List.for_all fst span_results in
  let effort_match = state.VM.effort_used = expected_effort in
  let accepted =
    ran
    && spans_matched
    && ((not !strict_effort) || effort_match)
  in
  accepted,
  `Assoc [
    "opcode", `String opcode;
    "template_path", `String template_path;
    "status", `String (if accepted then "accepted" else "rejected");
    "vm_run", `String (if ran then "accepted" else "rejected");
    "output_status",
    `String (if spans_matched then "matched" else "mismatch");
    "expected_effort", `Int expected_effort;
    "observed_effort", `Int state.VM.effort_used;
    "effort_match", `Bool effort_match;
    "strict_effort", `Bool !strict_effort;
    "subspans", `List (List.map snd span_results);
  ]

let run_index path =
  let root_dir = Filename.dirname path in
  let index =
    match read_json path with
    | `Assoc fields -> fields
    | _ -> fail (path ^ ": index must be an object")
  in
  let entries =
    list_field "templates" index
    |> List.map (function
      | `Assoc fields -> fields
      | _ -> fail "template index entries must be objects")
  in
  let results = List.map (execute_template root_dir) entries in
  let accepted = List.for_all fst results in
  let status = if accepted then "accepted" else "rejected" in
  `Assoc [
    "status", `String status;
    "diagnostic_only", `Bool true;
    "execution_mode", `String "positive_template_vm_execution";
    "template_index", `String path;
    "template_count", `Int (List.length results);
    "accepted_count",
    `Int (List.length (List.filter fst results));
    "rejected_count",
    `Int (List.length (List.filter (fun (ok, _) -> not ok) results));
    "results", `List (List.map snd results);
  ]

let () =
  Arg.parse args (fun value -> fail ("unexpected argument: " ^ value)) usage;
  match !template_index with
  | None -> fail usage
  | Some path ->
    let report = run_index path in
    print_endline (Yojson.Safe.pretty_to_string report);
    (match report with
     | `Assoc fields ->
       (match field "status" fields with
        | Some (`String "accepted") -> ()
        | _ -> exit 1)
     | _ -> exit 1)
