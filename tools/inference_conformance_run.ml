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
module Profile = Octra_vm.Inference_numerical_profile

let template_index = ref None
let p0_plus_pack = ref None
let strict_effort = ref false
let include_failures = ref false

let fail message =
  prerr_endline message;
  exit 1

let args = [
  "--template-index",
  Arg.String (fun value -> template_index := Some value),
  "producer template index json";
  "--p0-plus-pack",
  Arg.String (fun value -> p0_plus_pack := Some value),
  "producer P0-plus fixture pack json";
  "--strict-effort",
  Arg.Set strict_effort,
  "fail when observed VM effort differs from expected_effort";
  "--include-failures",
  Arg.Set include_failures,
  "execute definitive failure/atomicity cases where the direct VM runner can";
]

let usage =
  "inference_conformance_run --template-index <path> [--strict-effort] \
   [--include-failures]\n\
   or inference_conformance_run --p0-plus-pack <path>"

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

let z_of_unsigned_i64_string value =
  let z = Z.of_string value in
  let max_i64 = Z.of_int64 Int64.max_int in
  if Z.gt z max_i64 then Z.sub z (Z.shift_left Z.one 64) else z

let z_field name fields =
  match field name fields with
  | Some (`Int value) -> Z.of_int value
  | Some (`Intlit value) -> z_of_unsigned_i64_string value
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

let profile_gate_json opcode fields =
  match opt_string_field "profile" fields with
  | None -> `Null
  | Some profile ->
    (match Profile.validate_for_opcode ~opcode ~profile with
     | Ok gate -> Profile.to_json_for_opcode ~opcode gate
     | Error error ->
       fail (Profile.error_message error))

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

let u64_bytes value =
  let raw = Bytes.create 8 in
  put_int64_le raw 0 (Z.to_int64 value);
  Bytes.to_string raw

let state ?(limit = 1_000_000_000) () =
  VM.create_state
    ~limit
    ~strict_values:true
    ~caller:"caller"
    ~origin:"origin"
    ~address:"contract"
    ~value:Z.zero
    ~storage:(Hashtbl.create 0)
    ()

let set_int_reg state reg value =
  state.VM.regs.(reg) <- VM.VInt (Z.of_int value)

let set_z_reg state reg value =
  state.VM.regs.(reg) <- VM.VInt value

let set_raw_reg state reg raw =
  state.VM.regs.(reg) <- VM.VString raw

let reg_z state reg =
  match state.VM.regs.(reg) with
  | VM.VInt value
  | VM.VU64 value
  | VM.VU128 value
  | VM.VU256 value -> value
  | _ -> fail ("register is not an integer: r" ^ string_of_int reg)

let value_for name values =
  int_field name values

let reg_for name registers =
  register_index (string_field name registers)

type input_binding = {
  input_name : string;
  base : int;
  cells : int option;
  raw_register : int option;
}

let load_inputs root_dir state fields registers =
  list_field "input_memory_ranges" fields
  |> List.map (function
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
      let base = int_field "base_address" memory in
      (match opt_int_field "length_f64_cells" memory with
       | Some cells ->
         set_f64le state base cells raw;
         { input_name = name; base; cells = Some cells; raw_register = None }
       | None ->
         let raw_register = reg_for name registers in
         set_raw_reg state raw_register raw;
         { input_name = name; base; cells = None; raw_register = Some raw_register })
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
       | Some (`Int _)
       | Some (`Intlit _) -> set_z_reg state reg (z_field name values)
       | None -> ()
       | _ -> fail ("register value must be an int: " ^ name))
    registers

let find_input name inputs =
  match List.find_opt (fun input -> String.equal input.input_name name) inputs with
  | Some input -> input
  | None -> fail ("unknown input target: " ^ name)

let set_f64_cell_bits state addr bits =
  Hashtbl.replace state.VM.memory.data addr (VM.VInt bits)

let hex_value = function
  | '0'..'9' as c -> Char.code c - Char.code '0'
  | 'a'..'f' as c -> 10 + Char.code c - Char.code 'a'
  | 'A'..'F' as c -> 10 + Char.code c - Char.code 'A'
  | c -> fail (Printf.sprintf "invalid hex: %c" c)

let bytes_of_hex value =
  let compact =
    value
    |> String.to_seq
    |> Seq.filter (function ' ' | '\n' | '\r' | '\t' -> false | _ -> true)
    |> String.of_seq
  in
  if String.length compact mod 2 <> 0 then fail "odd hex length";
  String.init (String.length compact / 2) (fun index ->
    Char.chr
      ((hex_value compact.[index * 2] lsl 4)
       lor hex_value compact.[(index * 2) + 1]))

let replace_raw_prefix state reg replacement =
  match state.VM.regs.(reg) with
  | VM.VString raw ->
    if String.length raw < String.length replacement then
      fail "raw range too short for mutation";
    let bytes = Bytes.of_string raw in
    String.iteri (fun index char -> Bytes.set bytes index char) replacement;
    state.VM.regs.(reg) <- VM.VString (Bytes.to_string bytes)
  | _ -> fail "mutation target is not raw bytes"

let float_bits value =
  Z.of_int64 (Int64.bits_of_float value)

let apply_mutation state registers values inputs mutation =
  match mutation with
  | `Assoc fields ->
    let name = string_field "mutation" fields in
    let target = string_field "target" fields in
    (match name with
     | "replace_first_f64_input_cell" ->
       let input = find_input target inputs in
       set_f64_cell_bits state input.base (z_field "value_bits" fields);
       `Executed
     | "replace_all_score_cells" ->
       let input = find_input target inputs in
       let cells =
         match input.cells with
         | Some cells -> cells
         | None -> fail "score mutation target must be f64 cells"
       in
       let bits = z_field "value_bits" fields in
       for index = 0 to cells - 1 do
         set_f64_cell_bits state (input.base + index) bits
       done;
       `Executed
     | "replace_scores_with_large_finite_values" ->
       let input = find_input target inputs in
       let values_json = list_field "values_decimal" fields in
       let values =
         List.map
           (function
             | `String value -> float_of_string value
             | _ -> fail "values_decimal entries must be strings")
           values_json
       in
       List.iteri
         (fun index value -> set_f64_cell_bits state (input.base + index) (float_bits value))
         values;
       `Executed
     | "set_count_to_zero" ->
       set_int_reg state (reg_for "count" registers) 0;
       `Executed
     | "set_epsilon_bits" ->
       set_z_reg state (reg_for "epsilon_bits" registers) (z_field "value_bits" fields);
       `Executed
     | "set_scalar_param" ->
       let param =
         match List.rev (String.split_on_char '.' target) with
         | param :: _ -> param
         | [] -> fail "bad scalar mutation target"
       in
       set_int_reg state (reg_for param registers) (int_field "value" fields);
       `Executed
     | "set_state_dst_to_output_base" ->
       set_int_reg
         state
         (reg_for "state_dst" registers)
         (value_for "output" values);
       `Executed
     | "set_output_base_to_first_input_base" ->
       let first =
         match inputs with
         | input :: _ -> input
         | [] -> fail "no inputs available for alias mutation"
       in
       let output_param =
         if List.mem_assoc "dst" registers then Some "dst"
         else if List.mem_assoc "output" registers then Some "output"
         else if List.mem_assoc "addr" registers then Some "addr"
         else None
       in
       (match output_param with
        | Some param -> set_int_reg state (reg_for param registers) first.base
        | None -> ());
       `Executed
     | "replace_q1_scale_bits" ->
       let input = find_input "q1_owner" inputs in
       (match input.raw_register with
        | Some reg ->
          replace_raw_prefix state reg (bytes_of_hex (string_field "value_hex_le" fields));
          `Executed
        | None -> fail "q1_owner must be raw bytes")
     | "truncate_input_manifest" -> `Ingress_rejected
     | "lower_effort_limit" ->
       `Executed
     | _ -> fail ("unsupported mutation: " ^ name))
  | _ -> fail "mutation must be an object"

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

let starts_with prefix value =
  String.length value >= String.length prefix
  && String.sub value 0 (String.length prefix) = prefix

let span_fields value =
  match value with
  | `Assoc fields -> fields
  | _ -> fail "span must be an object"

let seed_span_if_missing state base cells =
  for index = 0 to cells - 1 do
    if not (Hashtbl.mem state.VM.memory.data (base + index)) then
      set_f64_cell_bits
        state
        (base + index)
        (float_bits (42.0 +. float_of_int index))
  done

let capture_span state span =
  let fields = span_fields span in
  let name = string_field "name" fields in
  let base = int_field "base_address" fields in
  let cells = int_field "length_f64_cells" fields in
  seed_span_if_missing state base cells;
  name, base, cells, output_bytes state base cells

let unchanged_result state (name, base, cells, before) =
  let after = output_bytes state base cells in
  let matched = String.equal before after in
  matched,
  `Assoc [
    "name", `String name;
    "base_address", `Int base;
    "length_f64_cells", `Int cells;
    "before_sha256", `String (sha256 before);
    "after_sha256", `String (sha256 after);
    "unchanged", `Bool matched;
  ]

let failure_expectation expected =
  if starts_with "reject_before_write" expected then `Must_reject
  else if starts_with "reject_or_documented_safe_copy" expected then `Observation
  else `Observation

let failure_case_result root_dir opcode template registers values op case =
  match case with
  | `Assoc fields ->
    let case_name = string_field "case" fields in
    let expected = string_field "expected" fields in
    let mutations = list_field "executable_mutations" fields in
    let effort_limit =
      List.fold_left
        (fun limit mutation ->
           match mutation with
           | `Assoc mutation_fields
             when String.equal
                    (string_field "mutation" mutation_fields)
                    "lower_effort_limit" ->
             int_field "value" mutation_fields
           | _ -> limit)
        1_000_000_000
        mutations
    in
    let state = state ~limit:effort_limit () in
    let inputs = load_inputs root_dir state template registers in
    set_registers state registers values;
    let mutation_results =
      List.map (apply_mutation state registers values inputs) mutations
    in
    let ingress_rejected =
      List.exists (( = ) `Ingress_rejected) mutation_results
    in
    let unchanged_spans =
      list_field "unchanged_spans" fields
      |> List.map (capture_span state)
    in
    let ran = if ingress_rejected then false else VM.run state [|op; VM.STOP|] in
    let unchanged =
      List.map (unchanged_result state) unchanged_spans
    in
    let unchanged_ok = List.for_all fst unchanged in
    let observed =
      if ingress_rejected then "ingress_rejected"
      else if ran then "vm_accepted"
      else "vm_rejected"
    in
    let expectation = failure_expectation expected in
    let counted, passed =
      match expectation with
      | `Must_reject -> true, ((not ran) && unchanged_ok)
      | `Observation -> false, true
    in
    passed,
    counted,
    `Assoc [
      "opcode", `String opcode;
      "case", `String case_name;
      "expected", `String expected;
      "status", `String (if passed then "accepted" else "rejected");
      "counted", `Bool counted;
      "observed", `String observed;
      "unchanged_status",
      `String (if unchanged_ok then "matched" else "changed");
      "unchanged_spans", `List (List.map snd unchanged);
    ]
  | _ -> fail "failure case must be an object"

let failure_case_results root_dir opcode template registers values op =
  if not !include_failures then []
  else
    match field "expected_failure_atomicity_behavior" template with
    | Some (`List cases) ->
      List.map
        (failure_case_result root_dir opcode template registers values op)
        cases
    | _ -> []

let execute_template root_dir entry =
  let opcode = string_field "opcode" entry in
  let template_path = string_field "vm_execution_template" entry in
  let full_template_path = Filename.concat root_dir template_path in
  let template =
    match read_json full_template_path with
    | `Assoc fields -> fields
    | _ -> fail (full_template_path ^ ": template must be an object")
  in
  let profile_gate = profile_gate_json opcode template in
  let expected_effort = int_field "expected_effort" template in
  let params = assoc_field "parameter_addresses_and_scalar_params" template in
  let registers = assoc_field "registers" params in
  let values = assoc_field "values" params in
  let state = state () in
  ignore (load_inputs root_dir state template registers);
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
  let failure_results =
    failure_case_results root_dir opcode template registers values op
  in
  let counted_failures =
    List.filter (fun (_, counted, _) -> counted) failure_results
  in
  let failure_passed =
    List.for_all (fun (passed, _, _) -> passed) counted_failures
  in
  let accepted = accepted && failure_passed in
  accepted,
  `Assoc [
    "opcode", `String opcode;
    "template_path", `String template_path;
    "profile_gate", profile_gate;
    "status", `String (if accepted then "accepted" else "rejected");
    "vm_run", `String (if ran then "accepted" else "rejected");
    "output_status",
    `String (if spans_matched then "matched" else "mismatch");
    "expected_effort", `Int expected_effort;
    "observed_effort", `Int state.VM.effort_used;
    "effort_match", `Bool effort_match;
    "strict_effort", `Bool !strict_effort;
    "subspans", `List (List.map snd span_results);
    "failure_cases_included", `Bool !include_failures;
    "failure_case_count", `Int (List.length failure_results);
    "counted_failure_case_count", `Int (List.length counted_failures);
    "accepted_counted_failure_case_count",
    `Int
      (List.length
         (List.filter (fun (passed, _, _) -> passed) counted_failures));
    "failure_cases", `List (List.map (fun (_, _, json) -> json) failure_results);
  ]

let find_manifest list_name fixture name =
  match
    list_field list_name fixture
    |> List.find_opt (function
      | `Assoc fields -> String.equal (string_field "name" fields) name
      | _ -> false)
  with
  | Some (`Assoc fields) -> fields
  | Some _ -> fail "manifest must be an object"
  | None -> fail ("missing manifest: " ^ list_name ^ "." ^ name)

let load_manifest_raw root_dir manifest =
  let path = Filename.concat root_dir (string_field "path" manifest) in
  let expected_bytes = int_field "bytes" manifest in
  let expected_sha = string_field "sha256" manifest in
  let raw =
    try read_file path with
    | Sys_error message -> fail message
  in
  if String.length raw <> expected_bytes then
    fail
      (Printf.sprintf
         "%s: expected %d bytes actual %d"
         path
         expected_bytes
         (String.length raw));
  let actual_sha = sha256 raw in
  if not (String.equal actual_sha expected_sha) then
    fail
      (Printf.sprintf
         "%s: expected sha256 %s actual %s"
         path
         expected_sha
         actual_sha);
  raw

let load_input_raw root_dir fixture name =
  load_manifest_raw root_dir (find_manifest "input_byte_manifests" fixture name)

let compare_expected_raw root_dir fixture name raw =
  let manifest = find_manifest "expected_output_byte_manifests" fixture name in
  let expected = load_manifest_raw root_dir manifest in
  let matched = String.equal raw expected in
  matched,
  `Assoc [
    "name", `String name;
    "status", `String (if matched then "matched" else "mismatch");
    "expected_sha256", `String (sha256 expected);
    "observed_sha256", `String (sha256 raw);
    "expected_root", `String (string_field "root" manifest);
    "bytes", `Int (String.length raw);
  ]

let producer_only_expected root_dir fixture name =
  let manifest = find_manifest "expected_output_byte_manifests" fixture name in
  let raw = load_manifest_raw root_dir manifest in
  `Assoc [
    "name", `String name;
    "status", `String "producer_only";
    "expected_sha256", `String (sha256 raw);
    "expected_root", `String (string_field "root" manifest);
    "bytes", `Int (String.length raw);
  ]

let set_manifest_f64 root_dir fixture name state base =
  let raw = load_input_raw root_dir fixture name in
  if String.length raw mod 8 <> 0 then
    fail ("input is not f64le: " ^ name);
  set_f64le state base (String.length raw / 8) raw;
  String.length raw / 8

let int_list_field name fields =
  list_field name fields
  |> List.map (function
    | `Int value -> value
    | `Intlit value -> int_of_string value
    | _ -> fail ("field must be an int list: " ^ name))

let set_position_cells state base values =
  List.iteri
    (fun index value ->
       Hashtbl.replace
         state.VM.memory.data
         (base + index)
         (VM.VInt (Z.of_int value)))
    values

let p0_plus_softmax root_dir fixture params =
  let st = state () in
  let scores = 100 in
  let dst = 10000 in
  ignore (set_manifest_f64 root_dir fixture "scores" st scores);
  set_int_reg st 0 dst;
  set_int_reg st 1 scores;
  set_int_reg st 2 (int_field "count" params);
  let ran = VM.run st [|VM.SOFTMAX_FP (0, 1, 2); VM.STOP|] in
  let raw =
    if ran then output_bytes st dst (int_field "count" params) else ""
  in
  let matched, result =
    if ran then compare_expected_raw root_dir fixture "expected_probabilities" raw
    else false, `Assoc ["name", `String "expected_probabilities"; "status", `String "vm_rejected"]
  in
  ran, matched, [result], st.VM.effort_used

let p0_plus_attention_scores root_dir fixture params =
  let st = state () in
  let query = 100 in
  let key = 1000 in
  let dst = 10000 in
  ignore (set_manifest_f64 root_dir fixture "query" st query);
  ignore (set_manifest_f64 root_dir fixture "keys" st key);
  set_int_reg st 0 dst;
  set_int_reg st 1 query;
  set_int_reg st 2 key;
  set_int_reg st 3 (int_field "key_count" params);
  set_int_reg st 4 (int_field "head_dim" params);
  let ran = VM.run st [|VM.ATTENTION_SCORES_FP (0, 1, 2, 3, 4); VM.STOP|] in
  let raw =
    if ran then output_bytes st dst (int_field "key_count" params) else ""
  in
  let matched, result =
    if ran then compare_expected_raw root_dir fixture "expected_scores" raw
    else false, `Assoc ["name", `String "expected_scores"; "status", `String "vm_rejected"]
  in
  ran, matched, [result], st.VM.effort_used

let p0_plus_weighted_sum root_dir fixture params =
  let st = state () in
  let weights = 100 in
  let values = 1000 in
  let dst = 10000 in
  ignore (set_manifest_f64 root_dir fixture "weights" st weights);
  ignore (set_manifest_f64 root_dir fixture "values" st values);
  set_int_reg st 0 dst;
  set_int_reg st 1 weights;
  set_int_reg st 2 values;
  set_int_reg st 3 (int_field "key_count" params);
  set_int_reg st 4 (int_field "head_dim" params);
  let ran =
    VM.run st [|VM.ATTENTION_WEIGHTED_SUM_FP (0, 1, 2, 3, 4); VM.STOP|]
  in
  let raw =
    if ran then output_bytes st dst (int_field "head_dim" params) else ""
  in
  let matched, result =
    if ran then compare_expected_raw root_dir fixture "expected_output" raw
    else false, `Assoc ["name", `String "expected_output"; "status", `String "vm_rejected"]
  in
  ran, matched, [result], st.VM.effort_used

let p0_plus_rope root_dir fixture params =
  let st = state () in
  let addr = 100 in
  let positions = 1000 in
  ignore (set_manifest_f64 root_dir fixture "input" st addr);
  set_position_cells st positions (int_list_field "pair_positions" params);
  set_int_reg st 0 addr;
  set_int_reg st 1 (int_field "count" params);
  set_int_reg st 2 (int_field "head_dim" params);
  set_int_reg st 3 (int_field "rotary_dim" params);
  set_int_reg st 4 positions;
  set_z_reg st 5 (z_field "freq_base_bits" params);
  let ran = VM.run st [|VM.ROPE_APPLY_INDEXED_FP (0, 1, 2, 3, 4, 5); VM.STOP|] in
  let raw =
    if ran then output_bytes st addr (int_field "count" params) else ""
  in
  let matched, result =
    if ran then compare_expected_raw root_dir fixture "expected_output" raw
    else false, `Assoc ["name", `String "expected_output"; "status", `String "vm_rejected"]
  in
  ran, matched, [result], st.VM.effort_used

let p0_plus_argmax root_dir fixture params =
  let st = state () in
  let logits = 100 in
  ignore (set_manifest_f64 root_dir fixture "logits" st logits);
  set_int_reg st 0 logits;
  set_int_reg st 1 (int_field "count" params);
  let ran = VM.run st [|VM.ARGMAX_FP (2, 0, 1); VM.STOP|] in
  let selected = if ran then u64_bytes (reg_z st 2) else "" in
  let matched, selected_result =
    if ran then compare_expected_raw root_dir fixture "selected_index_u64le" selected
    else false, `Assoc ["name", `String "selected_index_u64le"; "status", `String "vm_rejected"]
  in
  let top5 = producer_only_expected root_dir fixture "top5_indices_u64le" in
  ran, matched, [selected_result; top5], st.VM.effort_used

let p0_plus_logits_tail root_dir fixture params =
  let st = state () in
  let hidden = 100 in
  let gamma = 7000 in
  let logits = 10000 in
  let q1 = load_input_raw root_dir fixture "lm_head_q1_owner" in
  ignore (set_manifest_f64 root_dir fixture "final_hidden" st hidden);
  ignore (set_manifest_f64 root_dir fixture "final_norm_gamma" st gamma);
  set_int_reg st 0 hidden;
  set_int_reg st 1 (int_field "hidden_dim" params);
  set_int_reg st 3 gamma;
  set_z_reg st 4 (z_field "epsilon_bits" params);
  set_int_reg st 5 logits;
  set_raw_reg st 6 q1;
  set_int_reg st 7 0;
  set_int_reg st 8 1;
  set_int_reg st 9 (int_field "hidden_dim" params);
  set_int_reg st 10 (int_field "vocab_slice" params);
  set_int_reg st 11 logits;
  set_int_reg st 12 (int_field "vocab_slice" params);
  let program = [|
    VM.RMSNORM_FP_EPS (0, 1, 3, 4);
    VM.LINEAR_Q1_G128_FP (5, 0, 6, 7, 8, 9, 10);
    VM.ARGMAX_FP (13, 11, 12);
    VM.STOP;
  |] in
  let ran = VM.run st program in
  let norm_raw =
    if ran then output_bytes st hidden (int_field "hidden_dim" params) else ""
  in
  let logits_raw =
    if ran then output_bytes st logits (int_field "vocab_slice" params) else ""
  in
  let selected_raw = if ran then u64_bytes (reg_z st 13) else "" in
  let results =
    if not ran then [
      `Assoc ["name", `String "final_norm_output"; "status", `String "vm_rejected"];
      `Assoc ["name", `String "logits"; "status", `String "vm_rejected"];
      `Assoc ["name", `String "selected_index_u64le"; "status", `String "vm_rejected"];
      producer_only_expected root_dir fixture "top5_indices_u64le";
    ]
    else
      let _, norm_result =
        compare_expected_raw root_dir fixture "final_norm_output" norm_raw
      in
      let _, logits_result =
        compare_expected_raw root_dir fixture "logits" logits_raw
      in
      let _, selected_result =
        compare_expected_raw root_dir fixture "selected_index_u64le" selected_raw
      in
      [
        norm_result;
        logits_result;
        selected_result;
        producer_only_expected root_dir fixture "top5_indices_u64le";
      ]
  in
  let matched =
    List.for_all
      (function
        | `Assoc fields ->
          (match string_field "status" fields with
           | "matched"
           | "producer_only" -> true
           | _ -> false)
        | _ -> false)
      results
  in
  ran, matched, results, st.VM.effort_used

let execute_p0_plus_fixture root_dir entry =
  let manifest_path = string_field "manifest" entry in
  let full_manifest_path = Filename.concat root_dir manifest_path in
  let fixture =
    match read_json full_manifest_path with
    | `Assoc fields -> fields
    | _ -> fail (full_manifest_path ^ ": fixture must be an object")
  in
  let opcode = string_field "opcode" fixture in
  let primitive = string_field "primitive" fixture in
  let case_name = string_field "case" fixture in
  let params = assoc_field "parameters" fixture in
  let ran, matched, outputs, effort =
    match opcode with
    | "SOFTMAX_FP" -> p0_plus_softmax root_dir fixture params
    | "ATTENTION_SCORES_FP" -> p0_plus_attention_scores root_dir fixture params
    | "ATTENTION_WEIGHTED_SUM_FP" -> p0_plus_weighted_sum root_dir fixture params
    | "ROPE_APPLY_INDEXED_FP" -> p0_plus_rope root_dir fixture params
    | "ARGMAX_FP" -> p0_plus_argmax root_dir fixture params
    | "LOGITS_TAIL_PATH" -> p0_plus_logits_tail root_dir fixture params
    | value -> fail ("unsupported P0-plus opcode: " ^ value)
  in
  let accepted = ran && matched in
  accepted,
  `Assoc [
    "case", `String case_name;
    "opcode", `String opcode;
    "primitive", `String primitive;
    "manifest", `String manifest_path;
    "status", `String (if accepted then "accepted" else "rejected");
    "vm_run", `String (if ran then "accepted" else "rejected");
    "output_status", `String (if matched then "matched" else "mismatch");
    "observed_effort", `Int effort;
    "outputs", `List outputs;
  ]

let run_p0_plus_pack path =
  let root_dir = Filename.dirname path in
  let pack =
    match read_json path with
    | `Assoc fields -> fields
    | _ -> fail (path ^ ": fixture pack must be an object")
  in
  let entries =
    list_field "fixtures" pack
    |> List.map (function
      | `Assoc fields -> fields
      | _ -> fail "P0-plus fixture entries must be objects")
  in
  let results = List.map (execute_p0_plus_fixture root_dir) entries in
  let accepted = List.for_all fst results in
  `Assoc [
    "status", `String (if accepted then "accepted" else "rejected");
    "diagnostic_only", `Bool true;
    "execution_mode", `String "p0_plus_fixture_pack_vm_execution";
    "fixture_pack", `String path;
    "fixture_count", `Int (List.length results);
    "accepted_count", `Int (List.length (List.filter fst results));
    "rejected_count",
    `Int (List.length (List.filter (fun (ok, _) -> not ok) results));
    "results", `List (List.map snd results);
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
  let report =
    match !template_index, !p0_plus_pack with
    | Some _, Some _ -> fail "choose only one input mode"
    | None, None -> fail usage
    | Some path, None -> run_index path
    | None, Some path -> run_p0_plus_pack path
  in
    print_endline (Yojson.Safe.pretty_to_string report);
    (match report with
     | `Assoc fields ->
       (match field "status" fields with
        | Some (`String "accepted") -> ()
        | _ -> exit 1)
     | _ -> exit 1)
