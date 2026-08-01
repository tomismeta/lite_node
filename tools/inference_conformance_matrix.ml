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

module Template = Octra_vm.Inference_conformance_template
module Signature = Octra_vm.Inference_conformance_signature
module Fp64 = Octra_vm.Inference_fp64

let runner_reports = ref []
let min_platforms = ref 2
let require_validator_readiness = ref false
let requested_opcodes = ref []

let result_signature_schema =
  Signature.schema

let fail message =
  prerr_endline message;
  exit 1

let args = [
  "--runner-report",
  Arg.String (fun value -> runner_reports := value :: !runner_reports),
  "executed inference_conformance_run report json";
  "--min-platforms",
  Arg.Set_int min_platforms,
  "minimum distinct platform observations required for matrix acceptance";
  "--opcode",
  Arg.String (fun value -> requested_opcodes := value :: !requested_opcodes),
  "require cross-platform evidence to cover one opcode; may be repeated";
  "--require-validator-readiness",
  Arg.Set require_validator_readiness,
  "exit nonzero unless the matrix also proves validator readiness";
]

let usage =
  "inference_conformance_matrix --runner-report <path> \
   [--runner-report <path> ...] [--min-platforms <n>] \
   [--opcode <opcode> ...] [--require-validator-readiness]"

let read_file path =
  let input = open_in_bin path in
  Fun.protect
    ~finally:(fun () -> close_in_noerr input)
    (fun () ->
       let len = in_channel_length input in
       really_input_string input len)

let parse_json path raw =
  try Yojson.Safe.from_string raw with
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

let bool_field name fields =
  match field name fields with
  | Some (`Bool value) -> value
  | _ -> fail ("missing bool field: " ^ name)

let list_field name fields =
  match field name fields with
  | Some (`List values) -> values
  | _ -> fail ("missing list field: " ^ name)

let string_list_field name fields =
  match field name fields with
  | Some (`List values) ->
    List.map
      (function
        | `String value -> value
        | _ -> fail ("invalid string list field: " ^ name))
      values
  | _ -> fail ("missing list field: " ^ name)

let assoc_field name fields =
  match field name fields with
  | Some (`Assoc fields) -> fields
  | _ -> fail ("missing object field: " ^ name)

let opt_assoc_field name fields =
  match field name fields with
  | Some (`Assoc fields) -> Some fields
  | _ -> None

let opt_string_field name fields =
  match field name fields with
  | Some (`String value) -> Some value
  | Some `Null
  | None -> None
  | _ -> fail ("invalid string field: " ^ name)

let json_intlike_string name fields =
  match field name fields with
  | Some (`Int value) -> Some (string_of_int value)
  | Some (`Intlit value) -> Some value
  | _ -> None

let hex_nibble = function
  | '0' .. '9' as value -> Some (Char.code value - Char.code '0')
  | 'a' .. 'f' as value -> Some (10 + Char.code value - Char.code 'a')
  | 'A' .. 'F' as value -> Some (10 + Char.code value - Char.code 'A')
  | _ -> None

let hex_string value =
  String.length value = 64
  && String.for_all (fun ch -> Option.is_some (hex_nibble ch)) value

let q1_fp16_bits_from_hex_le value =
  if String.length value <> 4 then None
  else
    match
      hex_nibble value.[0],
      hex_nibble value.[1],
      hex_nibble value.[2],
      hex_nibble value.[3]
    with
    | Some lo_a, Some lo_b, Some hi_a, Some hi_b ->
      let lo = lo_a lsl 4 lor lo_b in
      let hi = hi_a lsl 4 lor hi_b in
      Some (lo lor (hi lsl 8))
    | _ -> None

let checked_mul left right =
  if left < 0 || right < 0 then
    None
  else if left <> 0 && right > max_int / left then
    None
  else
    Some (left * right)

let checked_add left right =
  if left < 0 || right < 0 then
    None
  else if left > max_int - right then
    None
  else
    Some (left + right)

let json_int_value = function
  | Some (`Int value) -> Some value
  | Some (`Intlit value) ->
    (try Some (int_of_string value) with Failure _ -> None)
  | _ -> None

let json_int_field name fields =
  json_int_value (field name fields)

let json_field_or_null name fields =
  match field name fields with
  | Some value -> value
  | None -> `Null

let add_if condition value values =
  if condition then value :: values else values

let blocker_matches pattern blocker =
  if String.equal pattern "q1_failure_case_" then
    let pattern_len = String.length pattern in
    String.length blocker >= pattern_len
    && String.equal (String.sub blocker 0 pattern_len) pattern
  else
    String.equal pattern blocker

let next_readiness_blocker blockers =
  let priority =
    [
      "schema_rejected";
      "execution_rejected";
      "execution_not_run";
      "strict_effort_not_enabled";
      "effort_mismatch";
      "effort_not_run";
      "q1_failure_case_";
      "uncounted_failure_cases";
      "punitive_failure_cases_not_accepted";
      "punitive_failure_cases_not_run";
      "required_failure_case_contract_rejected";
      "runner_report_rejected";
      "report_rejected";
      "vm_semantics_root_binding_not_proven";
      "unbound_vm_semantics_roots";
      "vm_semantics_binding_mismatch";
      "vm_semantics_binding_rejected";
      "abi_declaration_binding_not_proven";
      "unbound_abi_declaration_binding";
      "abi_declaration_binding_mismatch";
      "abi_declaration_binding_rejected";
      "executable_abi_binding_not_proven";
      "executable_abi_binding_mismatch";
      "executable_abi_binding_not_run";
      "consensus_candidate_profile_gates";
      "unbound_profile_roots";
      "profile_root_binding_mismatch";
      "profile_root_binding_rejected";
      "cross_platform_conformance_missing";
      "missing_cross_platform_matrix";
      "required_opcode_missing";
      "insufficient_distinct_platforms";
      "insufficient_distinct_runner_executables";
      "result_mismatch_across_platforms";
    ]
  in
  match
    List.find_map
      (fun pattern ->
         List.find_opt (blocker_matches pattern) blockers)
      priority
  with
  | Some blocker -> Some blocker
  | None ->
    (match blockers with
     | [] -> None
     | blocker :: _ -> Some blocker)

let next_blocker_json blockers =
  match next_readiness_blocker blockers with
  | Some blocker -> `String blocker
  | None -> `Null

let opt_status_is_accepted name fields =
  match opt_string_field name fields with
  | Some "accepted" -> true
  | _ -> false

let opt_string_from_assoc name fields =
  match field name fields with
  | Some (`String value) -> Some value
  | _ -> None

let unique values =
  List.sort_uniq String.compare values

let starts_with prefix value =
  String.length value >= String.length prefix
  && String.sub value 0 (String.length prefix) = prefix

let required_opcodes () =
  List.rev !requested_opcodes |> unique

type report_summary = {
  accepted : bool;
  platform_key : string;
  runner_executable_sha256 : string option;
  signature : string;
  result_opcodes : string list;
  selected_opcodes : string list;
  profile_catalog_root : string option;
  template_corpus_root : string option;
  validator_readiness_blockers : string list;
  blockers : string list;
  json : Yojson.Safe.t;
}

let platform_key platform =
  String.concat
    "|"
    [
      string_field "ocaml_version" platform;
      string_field "os_type" platform;
      string_field "system_name" platform;
      string_field "system_release" platform;
      string_field "machine" platform;
      string_of_int (int_field "word_size" platform);
      string_of_bool (bool_field "big_endian" platform);
      string_field "backend_type" platform;
    ]

let result_opcode = function
  | `Assoc fields -> string_field "opcode" fields
  | _ -> fail "result must be an object"

let opcode_requires_failure_case_contract opcode =
  String.equal opcode "LINEAR_Q1_G128_FP"

let q1_required_failure_expectations_match fields =
  match field "expectations" fields with
  | Some (`List expectations) ->
    let parsed =
      List.map
        (function
          | `Assoc fields ->
            string_field "case" fields,
            string_field "expected_prefix" fields
          | _ -> fail "failure contract expectation must be an object")
        expectations
    in
    List.length parsed = List.length Template.q1_required_failure_expectations
    &&
    List.for_all
      (fun required -> List.exists (( = ) required) parsed)
      Template.q1_required_failure_expectations
  | _ -> false

let required_failure_case_contract_bound opcode fields =
  if not (opcode_requires_failure_case_contract opcode) then
    true
  else
    match field "contract" fields with
    | Some (`Assoc contract_fields) ->
      String.equal
        (string_field "opcode" contract_fields)
        opcode
      && q1_required_failure_expectations_match contract_fields
    | _ -> false

let required_failure_case_contract_accepted fields =
  let opcode = string_field "opcode" fields in
  match opt_assoc_field "required_failure_case_contract" fields with
  | Some contract_fields ->
    let blockers_empty =
      match field "blockers" contract_fields with
      | Some (`List []) -> true
      | _ -> false
    in
    (match opt_string_field "status" contract_fields with
     | Some "accepted" ->
       blockers_empty
       && required_failure_case_contract_bound opcode contract_fields
     | Some "not_applicable" ->
       (not (opcode_requires_failure_case_contract opcode)) && blockers_empty
     | _ -> false)
  | None -> false

let q1_failure_mutation_shapes_accepted fields =
  let opcode = string_field "opcode" fields in
  if not (String.equal opcode "LINEAR_Q1_G128_FP") then
    true
  else
    match field "failure_cases" fields with
    | Some (`List cases) ->
      cases <> []
      &&
      List.for_all
        (function
          | `Assoc case_fields ->
            (match opt_string_field "mutation_shape_status" case_fields,
                   field "mutation_shape_blockers" case_fields with
             | Some "accepted", Some (`List []) -> true
             | _ -> false)
          | _ -> false)
        cases
    | _ -> false

let q1_contract_shape_consistent fields =
  let opcode = string_field "opcode" fields in
  if not (String.equal opcode "LINEAR_Q1_G128_FP") then
    true
  else
    match opt_assoc_field "q1_contract_shape" fields with
    | Some shape ->
      let shape_int name = json_int_field name shape in
      let output_subspan_has_span base cells =
        match field "subspans" fields with
        | Some (`List subspans) ->
          List.exists
            (function
              | `Assoc subspan ->
                json_int_field "base_address" subspan = Some base
                && json_int_field "length_f64_cells" subspan = Some cells
                &&
                (match field "matched" subspan with
                 | Some (`Bool true) -> true
                 | _ -> false)
              | _ -> false)
            subspans
        | _ -> false
      in
      let executable_abi =
        match opt_assoc_field "executable_abi_binding" fields with
        | Some abi ->
          (match json_int_field "expected_r0" abi,
                 json_int_field "observed_r0" abi,
                 json_int_field "expected_r1" abi,
                 json_int_field "observed_r1" abi,
                 field "registers_match" abi,
                 opt_string_field "output_payload_status" abi,
                 field "output_payload_error" abi,
                 field "output_payload_sha256" abi with
           | Some expected_base,
             Some observed_base,
             Some expected_count,
             Some observed_count,
             Some (`Bool registers_match),
             Some payload_status,
             Some `Null,
             Some (`String payload_sha256) ->
             Some
               (expected_base,
                observed_base,
                expected_count,
                observed_count,
                registers_match,
                payload_status,
                payload_sha256)
           | _ -> None)
        | None -> None
      in
      (match
         shape_int "m",
         shape_int "k",
         shape_int "n",
         shape_int "byte_offset",
         shape_int "lhs_cells",
         shape_int "output_cells",
         shape_int "q1_owner_source_bytes",
         shape_int "q1_required_owner_bytes",
         shape_int "expected_effort",
         json_int_field "expected_effort" fields,
         json_int_field "observed_effort" fields,
         json_int_field "expected_program_effort" fields,
         json_int_field "observed_program_effort" fields,
         json_int_field "expected_opcode_effort" fields,
         json_int_field "observed_opcode_effort" fields,
         executable_abi
       with
       | Some m,
         Some k,
         Some n,
         Some byte_offset,
         Some lhs_cells,
         Some output_cells,
         Some source_bytes,
         Some required_bytes,
         Some shape_expected_effort,
         Some expected_effort,
         Some observed_effort,
         Some expected_program_effort,
         Some observed_program_effort,
         Some expected_opcode_effort,
         Some observed_opcode_effort,
         Some (expected_abi_base,
               observed_abi_base,
               expected_abi_count,
               observed_abi_count,
               registers_match,
               payload_status,
               payload_sha256) ->
         let expected_required_bytes =
           match checked_mul n (k / 128) with
           | Some blocks -> checked_mul blocks 18
           | None -> None
         in
         let expected_opcode_effort_from_shape =
           match checked_mul m n with
           | Some mn -> checked_mul mn k
           | None -> None
         in
         let expected_output_cells = checked_mul m n in
         let required_source_span = checked_add byte_offset required_bytes in
         let expected_total_effort =
           match expected_opcode_effort_from_shape with
           | Some cells -> Some (201 + (cells / 512))
           | None -> None
         in
         m > 0
         && k > 0
         && n > 0
         && m <= 32768
         && k <= 32768
         && n <= 32768
         && byte_offset >= 0
         && k mod 128 = 0
         && checked_mul m k = Some lhs_cells
         && expected_output_cells = Some output_cells
         && expected_abi_base = observed_abi_base
         && registers_match
         && String.equal payload_status "accepted"
         && hex_string payload_sha256
         && output_subspan_has_span expected_abi_base output_cells
         && expected_abi_count = output_cells
         && observed_abi_count = output_cells
         && expected_required_bytes = Some required_bytes
         &&
         (match required_source_span with
          | Some span -> source_bytes >= span
          | None -> false)
         && shape_expected_effort = expected_effort
         && observed_effort = expected_effort
         && expected_program_effort = expected_effort
         && observed_program_effort = expected_effort
         &&
         (match expected_opcode_effort_from_shape with
          | Some cells -> expected_opcode_effort = 200 + (cells / 512)
          | None -> false)
         && observed_opcode_effort = expected_opcode_effort
         && expected_total_effort = Some expected_effort
       | _ -> false)
    | None -> false

let q1_failure_mutation_payload_shape_ok result_fields case_fields =
  let shape_int name =
    match opt_assoc_field "q1_contract_shape" result_fields with
    | Some shape -> (match field name shape with Some (`Int value) -> Some value | _ -> None)
    | None -> None
  in
  let mutation_matches name target mutation_fields =
    opt_string_field "mutation" mutation_fields = Some name
    && opt_string_field "target" mutation_fields = Some target
  in
  let scalar_param param predicate mutation_fields =
    mutation_matches
      "set_scalar_param"
      ("parameter_addresses_and_scalar_params.values." ^ param)
      mutation_fields
    &&
    match field "value" mutation_fields with
    | Some (`Int value) -> predicate value
    | _ -> false
  in
  let case = string_field "case" case_fields in
  match field "executable_mutations" case_fields with
  | Some (`List [`Assoc mutation_fields]) ->
    (match case with
     | "nonfinite_input_nan" ->
       mutation_matches "replace_first_f64_input_cell" "lhs" mutation_fields
       && json_intlike_string "value_bits" mutation_fields
          = Some "9221120237041090560"
     | "nonfinite_input_infinity" ->
       mutation_matches "replace_first_f64_input_cell" "lhs" mutation_fields
       && json_intlike_string "value_bits" mutation_fields
          = Some "9218868437227405312"
     | "output_input_aliasing" ->
       (mutation_matches
          "set_output_base_to_first_input_base"
          "output.base_address"
          mutation_fields
        ||
        (mutation_matches
           "set_output_base_to_first_input_base_plus"
           "output.base_address"
           mutation_fields
         &&
         match field "offset_cells" mutation_fields with
         | Some (`Int 0) -> true
         | _ -> false))
     | "partial_output_input_aliasing" ->
       mutation_matches
         "set_output_base_to_first_input_base_plus"
         "output.base_address"
         mutation_fields
       &&
       (match field "offset_cells" mutation_fields, shape_int "lhs_cells" with
        | Some (`Int value), Some lhs_cells -> value > 0 && value < lhs_cells
        | _ -> false)
     | "k_not_multiple_of_128" ->
       scalar_param "k" (fun value -> value > 0 && value mod 128 <> 0) mutation_fields
     | "bad_q1_owner_length" ->
       mutation_matches "truncate_input_manifest" "q1_owner" mutation_fields
       &&
       (match field "truncate_bytes" mutation_fields with
        | Some (`Int value) -> value > 0
        | _ -> false)
     | "negative_byte_offset" ->
       scalar_param "byte_offset" (fun value -> value < 0) mutation_fields
     | "byte_offset_out_of_bounds" ->
       (match shape_int "q1_owner_source_bytes" with
        | Some source_bytes ->
          scalar_param "byte_offset" (fun value -> value > source_bytes) mutation_fields
        | None -> false)
     | "byte_offset_truncated_span" ->
       (match shape_int "q1_owner_source_bytes",
             shape_int "q1_required_owner_bytes" with
        | Some source_bytes, Some required_bytes ->
          scalar_param
            "byte_offset"
            (fun value ->
               value >= 0
               && value <= source_bytes
               && required_bytes > source_bytes - value)
            mutation_fields
        | _ -> false)
     | "nonfinite_fp16_scale" ->
       mutation_matches "replace_q1_scale_bits" "q1_owner[0..2]" mutation_fields
       &&
       (match field "value_hex_le" mutation_fields with
        | Some (`String value) ->
          (match q1_fp16_bits_from_hex_le value with
           | Some bits -> Fp64.of_binary16 bits = None
           | None -> false)
        | _ -> false)
     | "lower_effort_limit" ->
       mutation_matches "lower_effort_limit" "effort" mutation_fields
       &&
       (match field "value" mutation_fields, shape_int "expected_effort" with
        | Some (`Int value), Some expected_effort -> value < expected_effort
        | _ -> false)
     | _ -> true)
  | _ -> false

let q1_required_failure_rows_accepted fields =
  let opcode = string_field "opcode" fields in
  if not (String.equal opcode "LINEAR_Q1_G128_FP") then
    true
  else
    match field "failure_cases" fields with
    | Some (`List cases) ->
      let row_for case =
        List.filter
          (function
            | `Assoc case_fields ->
              (match opt_string_field "case" case_fields with
               | Some actual -> String.equal actual case
               | None -> false)
            | _ -> false)
          cases
      in
      List.for_all
        (fun (case, expected_prefix) ->
           match row_for case with
           | [`Assoc case_fields] ->
             string_field "opcode" case_fields = "LINEAR_Q1_G128_FP"
             && starts_with expected_prefix (string_field "expected" case_fields)
             && opt_status_is_accepted "status" case_fields
             &&
             (match field "counted" case_fields with
              | Some (`Bool true) -> true
              | _ -> false)
             &&
             (match opt_string_field "mutation_shape_status" case_fields,
                    field "mutation_shape_blockers" case_fields with
             | Some "accepted", Some (`List []) -> true
             | _ -> false)
             &&
             (match field "executable_mutations" case_fields with
              | Some (`List (_ :: _)) -> true
              | _ -> false)
             && q1_failure_mutation_payload_shape_ok fields case_fields
           | _ -> false)
        Template.q1_required_failure_expectations
    | _ -> false

let q1_failure_mutation_payloads_present fields =
  let opcode = string_field "opcode" fields in
  if not (String.equal opcode "LINEAR_Q1_G128_FP") then
    true
  else
    match field "failure_cases" fields with
    | Some (`List cases) ->
      cases <> []
      &&
      List.for_all
        (function
          | `Assoc case_fields ->
            (match field "executable_mutations" case_fields with
             | Some (`List (_ :: _)) -> true
             | _ -> false)
          | _ -> false)
        cases
    | _ -> false

let optional_string_list_field name fields =
  match field name fields with
  | Some (`List values) ->
    List.map
      (function
        | `String value -> value
        | _ -> fail ("invalid string list field: " ^ name))
      values
  | Some `Null
  | None -> []
  | _ -> fail ("invalid list field: " ^ name)

let validator_readiness_summary fields =
  match opt_assoc_field "validator_readiness_gate" fields with
  | Some gate ->
    let status =
      match opt_string_field "status" gate with
      | Some status -> status
      | None -> "missing"
    in
    status, string_list_field "blockers" gate
  | None ->
    "missing", ["missing_validator_readiness_gate"]

let execution_mode_matrix_blocker = function
  | "positive_template_vm_execution" -> None
  | "p0_plus_fixture_pack_vm_execution" ->
    Some "p0_plus_matrix_not_supported"
  | _ -> Some "not_p0_runner_report"

let report_summary path =
  let raw_report = read_file path in
  let report_sha256 = sha256 raw_report in
  match parse_json path raw_report with
  | `Assoc fields ->
    let platform = assoc_field "platform" fields in
    let execution_mode =
      match opt_string_field "execution_mode" fields with
      | Some value -> value
      | None -> "not_runner"
    in
    let failure_gate = opt_assoc_field "failure_case_gate" fields in
    let results =
      if String.equal execution_mode "positive_template_vm_execution" then
        list_field "results" fields
      else []
    in
    let result_fields =
      List.map
        (function
          | `Assoc fields -> fields
          | _ -> fail "result must be an object")
        results
    in
    let has_results = result_fields <> [] in
    let strict_effort =
      List.for_all
        (fun fields ->
           match field "strict_effort" fields with
           | Some (`Bool value) -> value
           | _ -> false)
        result_fields
    in
    let output_matched =
      List.for_all
        (fun fields -> String.equal (string_field "output_status" fields) "matched")
        result_fields
    in
    let effort_matched =
      List.for_all
        (fun fields -> bool_field "effort_match" fields)
        result_fields
    in
    let opcode_effort_matched =
      List.for_all
        (fun fields -> bool_field "opcode_effort_match" fields)
        result_fields
    in
    let profile_catalog_root =
      opt_string_field "profile_catalog_root" fields
    in
    let template_corpus_root =
      opt_string_field "template_corpus_root" fields
    in
    let profile_root_binding_accepted =
      match opt_assoc_field "profile_root_binding_gate" fields with
      | Some gate_fields -> opt_status_is_accepted "status" gate_fields
      | None -> false
    in
    let profile_root_bindings_matched =
      List.for_all
        (fun fields ->
           match opt_assoc_field "profile_root_binding" fields with
           | Some binding_fields ->
             (match opt_string_field "status" binding_fields with
              | Some "matched" -> true
              | _ -> false)
           | None -> false)
        result_fields
    in
    let vm_semantics_binding_accepted =
      match opt_assoc_field "vm_semantics_binding_gate" fields with
      | Some gate_fields -> opt_status_is_accepted "status" gate_fields
      | None -> false
    in
    let vm_semantics_bindings_matched =
      List.for_all
        (fun fields ->
           match opt_assoc_field "vm_semantics_binding" fields with
           | Some binding_fields ->
             (match opt_string_field "status" binding_fields with
              | Some "matched" -> true
              | _ -> false)
           | None -> false)
        result_fields
    in
    let abi_declaration_binding_accepted =
      match opt_assoc_field "abi_declaration_binding_gate" fields with
      | Some gate_fields -> opt_status_is_accepted "status" gate_fields
      | None -> false
    in
    let abi_declaration_bindings_matched =
      List.for_all
        (fun fields ->
           match opt_assoc_field "abi_declaration_binding" fields with
           | Some binding_fields ->
             (match opt_string_field "status" binding_fields with
              | Some "matched" -> true
              | _ -> false)
           | None -> false)
        result_fields
    in
    let executable_abi_bindings_matched =
      List.for_all
        (fun fields ->
           match opt_assoc_field "executable_abi_binding" fields with
           | Some binding_fields ->
             (match opt_string_field "status" binding_fields with
              | Some "matched" -> true
              | _ -> false)
           | None -> false)
        result_fields
    in
    let required_failure_case_contracts_accepted =
      List.for_all required_failure_case_contract_accepted result_fields
    in
    let q1_failure_mutation_shapes_accepted =
      List.for_all q1_failure_mutation_shapes_accepted result_fields
    in
    let q1_contract_shapes_consistent =
      List.for_all q1_contract_shape_consistent result_fields
    in
    let q1_required_failure_rows_accepted =
      List.for_all q1_required_failure_rows_accepted result_fields
    in
    let q1_failure_mutation_payloads_present =
      List.for_all q1_failure_mutation_payloads_present result_fields
    in
    let runner_executable_sha256 =
      opt_string_from_assoc "runner_executable_sha256" platform
    in
    let accepted =
      String.equal execution_mode "positive_template_vm_execution"
      && opt_status_is_accepted "status" fields
      && opt_status_is_accepted "execution_status" fields
      && has_results
      && (match failure_gate with
          | Some fields -> opt_status_is_accepted "status" fields
          | None -> false)
      && required_failure_case_contracts_accepted
      && q1_contract_shapes_consistent
      && q1_required_failure_rows_accepted
      && q1_failure_mutation_shapes_accepted
      && q1_failure_mutation_payloads_present
      && strict_effort
      && output_matched
      && effort_matched
      && opcode_effort_matched
      && profile_root_binding_accepted
      && profile_root_bindings_matched
      && vm_semantics_binding_accepted
      && vm_semantics_bindings_matched
      && abi_declaration_binding_accepted
      && abi_declaration_bindings_matched
      && executable_abi_bindings_matched
      && Option.is_some template_corpus_root
      && Option.is_some runner_executable_sha256
    in
    let blockers =
      []
      |> (fun blockers ->
          match execution_mode_matrix_blocker execution_mode with
          | None -> blockers
          | Some blocker -> blocker :: blockers)
      |> add_if (not (opt_status_is_accepted "status" fields)) "report_rejected"
      |> add_if
           (not (opt_status_is_accepted "execution_status" fields))
           "execution_rejected"
      |> add_if (not has_results) "missing_results"
      |> add_if
           (not
              (match failure_gate with
               | Some fields -> opt_status_is_accepted "status" fields
               | None -> false))
           "punitive_failure_cases_rejected"
      |> add_if
           (not required_failure_case_contracts_accepted)
           "required_failure_case_contract_rejected"
      |> add_if
           (not q1_contract_shapes_consistent)
           "q1_contract_shape_rejected"
      |> add_if
           (not q1_required_failure_rows_accepted)
           "required_q1_failure_cases_rejected"
      |> add_if
           (not q1_failure_mutation_shapes_accepted)
           "q1_failure_case_mutation_shape_rejected"
      |> add_if
           (not q1_failure_mutation_payloads_present)
           "missing_failure_case_executable_mutations"
      |> add_if (not strict_effort) "strict_effort_not_enabled"
      |> add_if (not output_matched) "output_mismatch"
      |> add_if (not effort_matched) "effort_mismatch"
      |> add_if (not opcode_effort_matched) "opcode_effort_mismatch"
      |> add_if
           (not profile_root_binding_accepted)
           "profile_root_binding_rejected"
      |> add_if
           (not profile_root_bindings_matched)
           "profile_root_binding_mismatch"
      |> add_if
           (not vm_semantics_binding_accepted)
           "vm_semantics_binding_rejected"
      |> add_if
           (not vm_semantics_bindings_matched)
           "vm_semantics_binding_mismatch"
      |> add_if
           (not abi_declaration_binding_accepted)
           "abi_declaration_binding_rejected"
      |> add_if (not abi_declaration_bindings_matched) "abi_declaration_binding_mismatch"
      |> add_if
           (not executable_abi_bindings_matched)
           "executable_abi_binding_mismatch"
      |> add_if
           (Option.is_none template_corpus_root)
           "missing_template_corpus_root"
      |> add_if
           (Option.is_none runner_executable_sha256)
           "missing_runner_executable_sha256"
    in
    let signature = Signature.signature_json results in
    let signature_sha256 = sha256 signature in
    let result_opcodes = unique (List.map result_opcode results) in
    let selected_opcodes =
      match optional_string_list_field "selected_opcodes" fields with
      | [] -> result_opcodes
      | values -> unique values
    in
    let validator_readiness_status, validator_readiness_blockers =
      validator_readiness_summary fields
    in
    {
      accepted;
      platform_key = platform_key platform;
      runner_executable_sha256;
      signature;
      result_opcodes;
      selected_opcodes;
      profile_catalog_root;
      template_corpus_root;
      validator_readiness_blockers;
      blockers;
      json =
    `Assoc [
      "path", `String path;
      "runner_report_sha256", `String report_sha256;
      "result_signature_sha256", `String signature_sha256;
      "accepted", `Bool accepted;
      "platform", `Assoc platform;
      "platform_key", `String (platform_key platform);
      "runner_executable_sha256",
      (match runner_executable_sha256 with
       | Some digest -> `String digest
       | None -> `Null);
      "selected_opcodes",
      `List (List.map (fun value -> `String value) selected_opcodes);
      "result_opcodes",
      `List (List.map (fun value -> `String value) result_opcodes);
      "result_signature_schema", `String Signature.schema;
      "execution_mode", `String execution_mode;
      "status",
      `String
        (match opt_string_field "status" fields with
         | Some value -> value
         | None -> "missing");
      "execution_status",
      `String
        (match opt_string_field "execution_status" fields with
         | Some value -> value
         | None -> "missing");
      "failure_case_status",
      `String
        (match failure_gate with
         | Some fields ->
           (match opt_string_field "status" fields with
            | Some value -> value
            | None -> "missing")
         | None -> "missing");
      "required_failure_case_contract_status",
      `String
        (if required_failure_case_contracts_accepted then
           "accepted"
         else
           "rejected");
      "profile_catalog_root",
      (match profile_catalog_root with
       | Some root -> `String root
       | None -> `Null);
      "template_corpus_root",
      (match template_corpus_root with
       | Some root -> `String root
       | None -> `Null);
      "profile_consensus_status_counts",
      json_field_or_null "profile_consensus_status_counts" fields;
      "profile_root_binding_status_counts",
      json_field_or_null "profile_root_binding_status_counts" fields;
      "profile_root_binding_gate",
      json_field_or_null "profile_root_binding_gate" fields;
      "profile_root_binding_status",
      `String (if profile_root_bindings_matched then "matched" else "mismatch");
      "vm_semantics_binding_gate",
      json_field_or_null "vm_semantics_binding_gate" fields;
      "abi_declaration_binding_gate",
      json_field_or_null "abi_declaration_binding_gate" fields;
      "validator_readiness_status", `String validator_readiness_status;
      "validator_readiness_blockers",
      `List
        (List.map
           (fun blocker -> `String blocker)
           validator_readiness_blockers);
      "validator_readiness_gate",
      json_field_or_null "validator_readiness_gate" fields;
      "strict_effort", `Bool strict_effort;
      "output_status", `String (if output_matched then "matched" else "mismatch");
      "effort_status", `String (if effort_matched then "matched" else "mismatch");
      "opcode_effort_status",
      `String (if opcode_effort_matched then "matched" else "mismatch");
      "vm_semantics_binding_status",
      `String (if vm_semantics_bindings_matched then "matched" else "mismatch");
      "abi_declaration_binding_status",
      `String (if abi_declaration_bindings_matched then "matched" else "mismatch");
      "executable_abi_binding_status",
      `String (if executable_abi_bindings_matched then "matched" else "mismatch");
      "blockers", `List (List.map (fun blocker -> `String blocker) blockers);
    ];
    }
  | _ -> fail (path ^ ": runner report must be an object")

let without_cross_platform_blocker blockers =
  List.filter
    (fun blocker -> not (String.equal blocker "cross_platform_conformance_missing"))
    blockers

let matrix_report paths =
  let summaries = List.map report_summary paths in
  let report_count = List.length summaries in
  let accepted_reports =
    List.length
      (List.filter (fun summary -> summary.accepted) summaries)
  in
  let platforms =
    unique (List.map (fun summary -> summary.platform_key) summaries)
  in
  let signatures =
    unique (List.map (fun summary -> summary.signature) summaries)
  in
  let runner_executable_sha256s =
    summaries
    |> List.filter_map (fun summary -> summary.runner_executable_sha256)
    |> unique
  in
  let profile_catalog_roots =
    summaries
    |> List.filter_map (fun summary -> summary.profile_catalog_root)
    |> unique
  in
  let missing_profile_catalog_root_count =
    List.length
      (List.filter
         (fun summary -> summary.accepted && summary.profile_catalog_root = None)
         summaries)
  in
  let template_corpus_roots =
    summaries
    |> List.filter_map (fun summary -> summary.template_corpus_root)
    |> unique
  in
  let missing_template_corpus_root_count =
    List.length
      (List.filter
         (fun summary -> summary.accepted && summary.template_corpus_root = None)
         summaries)
  in
  let per_report_matrix_blockers =
    summaries
    |> List.map (fun summary -> summary.blockers)
    |> List.concat
    |> unique
  in
  let per_report_validator_blockers =
    summaries
    |> List.map (fun summary -> summary.validator_readiness_blockers)
    |> List.concat
    |> without_cross_platform_blocker
    |> unique
  in
  let result_opcodes =
    summaries
    |> List.map (fun summary -> summary.result_opcodes)
    |> List.concat
    |> unique
  in
  let selected_opcodes =
    summaries
    |> List.map (fun summary -> summary.selected_opcodes)
    |> List.concat
    |> unique
  in
  let required_opcodes = required_opcodes () in
  let opcode_coverage_ready =
    List.for_all
      (fun opcode -> List.exists (String.equal opcode) result_opcodes)
      required_opcodes
  in
  let matrix_signature =
    signatures
    |> List.sort String.compare
    |> String.concat "\n"
  in
  let matrix_signature_sha256 =
    if signatures = [] then None else Some (sha256 matrix_signature)
  in
  let matrix_accepted =
    report_count > 0
    && accepted_reports = report_count
    && List.length platforms >= !min_platforms
    && List.length runner_executable_sha256s >= !min_platforms
    && List.length signatures = 1
    && missing_profile_catalog_root_count = 0
    && List.length profile_catalog_roots = 1
    && missing_template_corpus_root_count = 0
    && List.length template_corpus_roots = 1
    && opcode_coverage_ready
  in
  let blockers =
    []
    |> add_if (report_count = 0) "no_runner_reports"
    |> add_if
         (accepted_reports <> report_count)
         "runner_report_rejected"
    |> add_if
         (List.length platforms < !min_platforms)
         "insufficient_distinct_platforms"
    |> add_if
         (List.length runner_executable_sha256s < !min_platforms)
         "insufficient_distinct_runner_executables"
    |> add_if
         (List.length signatures <> 1)
         "result_mismatch_across_platforms"
    |> add_if
         (missing_profile_catalog_root_count > 0)
         "missing_profile_catalog_root"
    |> add_if
         (List.length profile_catalog_roots > 1)
         "profile_catalog_mismatch"
    |> add_if
         (missing_template_corpus_root_count > 0)
         "missing_template_corpus_root"
    |> add_if
         (List.length template_corpus_roots > 1)
         "template_corpus_mismatch"
    |> add_if
         (not opcode_coverage_ready)
         "required_opcode_missing"
  in
  let validator_readiness_blockers =
    unique (blockers @ per_report_matrix_blockers @ per_report_validator_blockers)
  in
  let validator_readiness_accepted =
    matrix_accepted && validator_readiness_blockers = []
  in
  let validator_readiness_gate =
    `Assoc [
      "diagnostic_only", `Bool true;
      "required", `Bool !require_validator_readiness;
      "status",
      `String (if validator_readiness_accepted then "accepted" else "rejected");
      "runner_report_status",
      `String
        (if report_count > 0 && accepted_reports = report_count then
           "accepted"
         else
           "rejected");
      "distinct_platform_status",
      `String
        (if List.length platforms >= !min_platforms then "accepted" else "rejected");
      "distinct_runner_executable_status",
      `String
        (if List.length runner_executable_sha256s >= !min_platforms then
           "accepted"
         else
           "rejected");
      "result_signature_status",
      `String (if List.length signatures = 1 then "accepted" else "rejected");
      "profile_catalog_status",
      `String
        (if missing_profile_catalog_root_count = 0
            && List.length profile_catalog_roots = 1 then
           "accepted"
         else
           "rejected");
      "template_corpus_status",
      `String
        (if missing_template_corpus_root_count = 0
            && List.length template_corpus_roots = 1 then
           "accepted"
         else
           "rejected");
      "opcode_coverage_status",
      `String (if opcode_coverage_ready then "accepted" else "rejected");
      "cross_platform_status",
      `String (if matrix_accepted then "accepted" else "rejected");
      "next_blocker", next_blocker_json validator_readiness_blockers;
      "blockers",
      `List
        (List.map
           (fun blocker -> `String blocker)
           validator_readiness_blockers);
    ]
  in
  `Assoc [
    "status", `String (if matrix_accepted then "accepted" else "rejected");
    "diagnostic_only", `Bool true;
    "schema", `String "octra.inference.conformance.matrix.v1";
    "report_count", `Int report_count;
    "accepted_report_count", `Int accepted_reports;
    "required_distinct_platform_count", `Int !min_platforms;
    "distinct_platform_count", `Int (List.length platforms);
    "required_distinct_runner_executable_count", `Int !min_platforms;
    "distinct_runner_executable_count",
    `Int (List.length runner_executable_sha256s);
    "cross_platform_status",
    `String (if matrix_accepted then "accepted" else "rejected");
    "validator_readiness_required", `Bool !require_validator_readiness;
    "blockers", `List (List.map (fun blocker -> `String blocker) blockers);
    "validator_readiness_status",
    `String (if validator_readiness_accepted then "accepted" else "rejected");
    "next_validator_readiness_blocker",
    next_blocker_json validator_readiness_blockers;
    "validator_readiness_blockers",
    `List
      (List.map
         (fun blocker -> `String blocker)
         validator_readiness_blockers);
    "validator_readiness_gate", validator_readiness_gate;
    "platform_keys", `List (List.map (fun value -> `String value) platforms);
    "runner_executable_sha256s",
    `List (List.map (fun value -> `String value) runner_executable_sha256s);
    "selected_opcodes",
    `List (List.map (fun value -> `String value) selected_opcodes);
    "required_opcodes",
    `List (List.map (fun value -> `String value) required_opcodes);
    "opcode_coverage_status",
    `String (if opcode_coverage_ready then "accepted" else "rejected");
    "result_opcodes",
    `List (List.map (fun value -> `String value) result_opcodes);
    "result_signature_schema", `String result_signature_schema;
    "result_signature_count", `Int (List.length signatures);
    "profile_catalog_root_count", `Int (List.length profile_catalog_roots);
    "profile_catalog_roots",
    `List (List.map (fun value -> `String value) profile_catalog_roots);
    "template_corpus_root_count", `Int (List.length template_corpus_roots);
    "template_corpus_roots",
    `List (List.map (fun value -> `String value) template_corpus_roots);
    "matrix_signature_sha256",
    (match matrix_signature_sha256 with
     | None -> `Null
     | Some value -> `String value);
    "reports",
    `List (List.map (fun summary -> summary.json) summaries);
  ]

let () =
  Arg.parse args (fun value -> fail ("unexpected argument: " ^ value)) usage;
  if !min_platforms <= 0 then fail "--min-platforms must be positive";
  let paths = List.rev !runner_reports in
  let report = matrix_report paths in
  print_endline (Yojson.Safe.pretty_to_string report);
  let accepted =
    match report with
    | `Assoc fields ->
      let matrix_accepted =
        match field "status" fields with
        | Some (`String "accepted") -> true
        | _ -> false
      in
      let validator_readiness_accepted =
        (not !require_validator_readiness)
        ||
        match field "validator_readiness_status" fields with
        | Some (`String "accepted") -> true
        | _ -> false
      in
      matrix_accepted && validator_readiness_accepted
    | _ -> false
  in
  if not accepted then exit 1
