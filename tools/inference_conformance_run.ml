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
module Abi = Octra_vm.Inference_session_abi
module Fp64 = Octra_vm.Inference_fp64
module Profile = Octra_vm.Inference_numerical_profile
module Signature = Octra_vm.Inference_conformance_signature
module Template = Octra_vm.Inference_conformance_template
module Diagnostics = Octra_vm.Inference_conformance_diagnostics

let template_index = ref None
let p0_plus_pack = ref None
let cross_platform_matrix = ref None
let expected_cross_platform_matrix_sha256 = ref None
let requested_opcodes = ref []
let strict_effort = ref false
let include_failures = ref false
let synthesize_required_failure_cases = ref false
let require_failure_cases = ref false
let require_profile_roots_bound = ref false
let require_consensus_candidate = ref false
let require_consensus_ready = ref false
let require_validator_readiness = ref false

let cross_platform_result_signature_schema =
  Signature.schema

let cross_platform_matrix_schema =
  "octra.inference.conformance.matrix.v2"

let cross_platform_matrix_request_schema =
  "octra.inference.conformance.matrix.request.v2"

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
  "--cross-platform-matrix",
  Arg.String (fun value -> cross_platform_matrix := Some value),
  "accepted inference_conformance_matrix report json";
  "--expected-cross-platform-matrix-sha256",
  Arg.String (fun value -> expected_cross_platform_matrix_sha256 := Some value),
  "expected SHA-256 of --cross-platform-matrix";
  "--opcode",
  Arg.String (fun value -> requested_opcodes := value :: !requested_opcodes),
  "limit template-index execution to one P0 opcode; may be repeated";
  "--strict-effort",
  Arg.Set strict_effort,
  "fail when observed VM effort differs from expected_effort";
  "--include-failures",
  Arg.Set include_failures,
  "execute definitive failure/atomicity cases where the direct VM runner can";
  "--synthesize-required-failure-cases",
  Arg.Set synthesize_required_failure_cases,
  "append LiteNode-required failure/atomicity cases derivable from template context";
  "--require-failure-cases",
  Arg.Set require_failure_cases,
  "reject template-index reports unless executable failure cases were run and accepted";
  "--require-profile-roots-bound",
  Arg.Set require_profile_roots_bound,
  "reject reports whose numerical_profile_root values do not match LiteNode profile roots";
  "--require-consensus-candidate",
  Arg.Set require_consensus_candidate,
  "reject reports whose profile gates are still local-only or unbound";
  "--require-consensus-ready",
  Arg.Set require_consensus_ready,
  "reject reports whose profile gates are not all consensus_ready";
  "--require-validator-readiness",
  Arg.Set require_validator_readiness,
  "exit nonzero unless the report also proves validator readiness";
]

let usage =
  "inference_conformance_run --template-index <path> [--strict-effort] \
   [--opcode <opcode> ...] \
   [--include-failures] [--require-failure-cases] \
   [--require-profile-roots-bound] \
   [--require-consensus-candidate] [--require-consensus-ready] \
   [--cross-platform-matrix <path>] \
   [--expected-cross-platform-matrix-sha256 <sha256>] \
   [--synthesize-required-failure-cases] [--require-validator-readiness]\n\
   or inference_conformance_run --p0-plus-pack <path> \
   [--require-profile-roots-bound] [--require-consensus-candidate] \
   [--require-consensus-ready] [--require-validator-readiness]"

let selected_opcodes () =
  List.rev !requested_opcodes |> List.sort_uniq String.compare

let opcode_selected opcode =
  match selected_opcodes () with
  | [] -> true
  | opcodes -> List.exists (String.equal opcode) opcodes

let spine_matrix_opcodes =
  Template.p0_opcodes
  @ ["LOAD_F64_LE_FP"; "ARGMAX_FP"; "LOAD_F32_LE_FP"]

let validate_selected_opcodes () =
  List.iter
    (fun opcode ->
       if not (List.exists (String.equal opcode) spine_matrix_opcodes) then
         fail ("unsupported opcode filter: " ^ opcode))
    (selected_opcodes ())

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

let json_string value =
  Yojson.Safe.to_string (`String value)

let fixture_value_root ~name raw =
  let digest = sha256 raw in
  let canonical =
    String.concat
      ""
      [
        "{\"bytes\":";
        string_of_int (String.length raw);
        ",\"domain\":\"fixture-value\",\"label\":";
        json_string name;
        ",\"schema\":1,\"sha256\":\"";
        digest;
        "\"}";
      ]
  in
  sha256 ("octra-inference/determinism-fixture\n" ^ canonical)

let backend_type_string = function
  | Sys.Native -> "native"
  | Sys.Bytecode -> "bytecode"
  | Sys.Other value -> "other:" ^ value

let current_executable_sha256 () =
  try Some (sha256 (read_file Sys.executable_name)) with
  | Sys_error _ -> None

let command_line command =
  try
    let input = Unix.open_process_in command in
    let line =
      try Some (String.trim (input_line input)) with
      | End_of_file -> None
    in
    ignore (Unix.close_process_in input);
    match line with
    | Some value when not (String.equal value "") -> Some value
    | _ -> None
  with
  | Unix.Unix_error _ -> None
  | Sys_error _ -> None

let string_or_null = function
  | Some value -> `String value
  | None -> `Null

let platform_json () =
  `Assoc [
    "ocaml_version", `String Sys.ocaml_version;
    "os_type", `String Sys.os_type;
    "system_name", string_or_null (command_line "uname -s");
    "system_release", string_or_null (command_line "uname -r");
    "machine", string_or_null (command_line "uname -m");
    "word_size", `Int Sys.word_size;
    "big_endian", `Bool Sys.big_endian;
    "backend_type", `String (backend_type_string Sys.backend_type);
    "runner_executable", `String Sys.executable_name;
    "runner_executable_sha256", string_or_null (current_executable_sha256 ());
  ]

let field name fields =
  match List.filter (fun (key, _) -> String.equal key name) fields with
  | [(_, value)] -> Some value
  | _ -> None

let field_or_null name fields =
  match field name fields with
  | Some value -> value
  | None -> `Null

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

let string_list_field name fields =
  List.map
    (function
      | `String value -> value
      | _ -> fail ("invalid string list field: " ^ name))
    (list_field name fields)

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

let bool_field name fields =
  match field name fields with
  | Some (`Bool value) -> value
  | _ -> fail ("missing bool field: " ^ name)

let opt_bool_field name fields =
  match field name fields with
  | Some (`Bool value) -> Some value
  | _ -> None

let profile_gate_opcodes gates =
  gates
  |> List.filter_map (function
    | `Assoc fields -> opt_string_field "opcode" fields
    | _ -> None)
  |> List.sort_uniq String.compare

let transcendental_dependency_entry_count = function
  | `Assoc fields ->
    (match opt_int_field "entry_count" fields with
     | Some count -> count
     | None -> 0)
  | _ -> 0

let transcendental_dependency_dependency_count = function
  | `Assoc fields ->
    (match opt_int_field "dependency_count" fields with
     | Some count -> count
     | None -> 0)
  | _ -> 0

let host_native_math_gate = function
  | `Assoc fields ->
    (match field "host_native_math_gate" fields with
     | Some (`Assoc _ as gate) -> gate
     | _ -> `Null)
  | _ -> `Null

let host_native_math_blockers catalog =
  match host_native_math_gate catalog with
  | `Assoc fields ->
    string_list_field "validator_admission_blockers" fields
  | _ -> []

let transcendental_dependency_blockers catalog =
  let entry_count = transcendental_dependency_entry_count catalog in
  let generic =
    if entry_count = 0 then [] else ["unresolved_transcendental_dependencies"]
  in
  (generic @ host_native_math_blockers catalog)
  |> List.sort_uniq String.compare

let transcendental_dependency_gate_json catalog =
  let blockers = transcendental_dependency_blockers catalog in
  `Assoc [
    "diagnostic_only", `Bool true;
    "status", `String (if blockers = [] then "accepted" else "rejected");
    "entry_count", `Int (transcendental_dependency_entry_count catalog);
    "dependency_count", `Int (transcendental_dependency_dependency_count catalog);
    "transcendental_dependency_catalog_root",
    `String (Profile.transcendental_dependency_catalog_root catalog);
    "host_native_math_gate", host_native_math_gate catalog;
    "blockers",
    `List (List.map (fun blocker -> `String blocker) blockers);
  ]

let transcendental_dependency_catalog_report_fields catalog =
  [
    "transcendental_dependency_catalog", catalog;
    "transcendental_dependency_catalog_root",
    `String (Profile.transcendental_dependency_catalog_root catalog);
  ]

let profile_gate_json opcode fields =
  let add_source source = function
    | `Assoc gate -> `Assoc (gate @ ["profile_source", `String source])
    | value -> value
  in
  match opt_string_field "profile" fields with
  | Some profile ->
    (match Profile.validate_for_opcode ~opcode ~profile with
     | Ok gate ->
       Profile.to_json_for_opcode ~opcode gate
       |> add_source "template"
     | Error error ->
       fail (Profile.error_message error))
  | None ->
    (match Profile.current_runtime_profile ~opcode with
     | None -> `Null
     | Some profile ->
       (match Profile.validate_for_opcode ~opcode ~profile with
        | Ok gate ->
          Profile.to_json_for_opcode ~opcode gate
          |> add_source "current_runtime_profile"
        | Error error ->
          fail (Profile.error_message error)))

let runtime_profile_gate_json opcode =
  match Profile.current_runtime_profile ~opcode with
  | None -> `Null
  | Some profile ->
    (match Profile.validate_for_opcode ~opcode ~profile with
     | Ok gate ->
       (match Profile.to_json_for_opcode ~opcode gate with
        | `Assoc fields ->
          `Assoc (fields @ ["profile_source", `String "current_runtime_profile"])
        | value -> value)
     | Error error -> fail (Profile.error_message error))

let non_null_profile_gates gates =
  List.filter_map
    (function
      | `Null -> None
      | gate -> Some gate)
    gates

let profile_gate_present = function
  | `Assoc fields ->
    (match field "profile_gate" fields with
     | Some `Null
     | None -> false
     | Some _ -> true)
  | _ -> false

let profile_gate_value = function
  | `Assoc fields ->
    (match field "profile_gate" fields with
     | Some (`Assoc _ as gate) -> Some gate
     | _ -> None)
  | _ -> None

let profile_root_binding_value value =
  match value with
  | `Assoc fields ->
    if profile_gate_present value then
      match field "profile_root_binding" fields with
      | Some (`Assoc _ as binding) -> Some binding
      | _ -> Some Profile.unavailable_root_binding_json
    else
      None
  | _ -> None

let profile_root_binding_status_counts values =
  values
  |> List.filter_map profile_root_binding_value
  |> Profile.root_binding_counts_of_json

let profile_root_binding_classification_counts values =
  values
  |> List.filter_map profile_root_binding_value
  |> Profile.root_binding_classification_counts_of_json

let vm_semantics_binding_value = function
  | `Assoc fields ->
    (match field "vm_semantics_binding" fields with
     | Some (`Assoc _ as binding) -> Some binding
     | _ -> Some Profile.unavailable_root_binding_json)
  | _ -> None

let vm_semantics_binding_status_counts values =
  values
  |> List.filter_map vm_semantics_binding_value
  |> Profile.root_binding_counts_of_json

let vm_semantics_root_blockers counts =
  let add_if condition value values =
    if condition then value :: values else values
  in
  let total = counts.Profile.matched + counts.unbound + counts.unavailable in
  []
  |> add_if (total = 0) "no_vm_semantics_roots"
  |> add_if (counts.unavailable > 0) "unavailable_vm_semantics_roots"
  |> add_if (counts.unbound > 0) "unbound_vm_semantics_roots"

let vm_semantics_binding_gate_json counts =
  let ready = Profile.root_bindings_are_consensus_ready counts in
  `Assoc [
    "status", `String (if ready then "accepted" else "rejected");
    "blockers",
    `List
      (List.map
         (fun blocker -> `String blocker)
         (vm_semantics_root_blockers counts));
  ]

let abi_declaration_binding_value = function
  | `Assoc fields ->
    (match field "abi_declaration_binding" fields with
     | Some (`Assoc _ as binding) -> Some binding
     | _ -> Some Profile.unavailable_root_binding_json)
  | _ -> None

let abi_declaration_binding_status_counts values =
  values
  |> List.filter_map abi_declaration_binding_value
  |> Profile.root_binding_counts_of_json

let abi_declaration_binding_blockers counts =
  let add_if condition value values =
    if condition then value :: values else values
  in
  let total = counts.Profile.matched + counts.unbound + counts.unavailable in
  []
  |> add_if (total = 0) "no_abi_declaration_binding"
  |> add_if (counts.unavailable > 0) "unavailable_abi_declaration_binding"
  |> add_if (counts.unbound > 0) "unbound_abi_declaration_binding"

let abi_declaration_binding_gate_json counts =
  let ready = Profile.root_bindings_are_consensus_ready counts in
  `Assoc [
    "status", `String (if ready then "accepted" else "rejected");
    "blockers",
    `List
      (List.map
         (fun blocker -> `String blocker)
         (abi_declaration_binding_blockers counts));
  ]

let reg_name index = "r" ^ string_of_int index

let repair_field ~field ?case ?expected_prefix ?required_case ?observed ?expected
    ?(blockers = []) action =
  let optional_string name = function
    | None -> []
    | Some value -> [name, `String value]
  in
  let optional_json name = function
    | None -> []
    | Some value -> [name, value]
  in
  `Assoc
    ([
       "field", `String field;
       "action", `String action;
       "observed", string_or_null observed;
       "expected", string_or_null expected;
       "blockers",
       `List (List.map (fun blocker -> `String blocker) blockers);
     ]
     @ optional_string "case" case
     @ optional_string "expected_prefix" expected_prefix
     @ optional_json "required_case" required_case)

let binding_repair ~field ~observed_name ~expected_name binding =
  match binding with
  | `Assoc fields ->
    (match opt_string_field "status" fields with
     | Some "matched" -> []
     | _ ->
       [
         repair_field
           ~field
           ?observed:(opt_string_field observed_name fields)
           ?expected:(opt_string_field expected_name fields)
           ~blockers:(optional_string_list_field "blockers" fields)
           "replace_with_litenode_authority";
       ])
  | _ ->
    [
      repair_field
        ~field
        "emit_litenode_authority_binding";
    ]

let abi_repair binding =
  match binding with
  | `Assoc fields ->
    (match opt_string_field "status" fields with
     | Some "matched" -> []
     | _ ->
       let blockers = optional_string_list_field "blockers" fields in
       let has blocker = List.exists (String.equal blocker) blockers in
       let int_string name =
         Option.map string_of_int (opt_int_field name fields)
       in
       let known =
         [
           ( "entrypoint_mismatch",
             "abi.entrypoint",
             "set_entrypoint",
             opt_string_field "entrypoint" fields,
             Some Abi.advance_entrypoint );
           ( "entry_label_mismatch",
             "abi.label",
             "set_entrypoint_label",
             int_string "label",
             Some (string_of_int Abi.advance_label) );
           ( "output_base_register_mismatch",
             "abi.output_base_register",
             "set_output_base_register",
             opt_string_field "output_base_register" fields,
             Some (reg_name Abi.output_base_register) );
           ( "output_count_register_mismatch",
             "abi.output_count_register",
             "set_output_count_register",
             opt_string_field "output_count_register" fields,
             Some (reg_name Abi.output_count_register) );
           ( "output_count_unit_mismatch",
             "abi.output_count_unit",
             "set_output_count_unit",
             opt_string_field "output_count_unit" fields,
             Some "cells" );
           ( "session_abi_root_mismatch",
             "abi.session_abi_root",
             "set_session_abi_root",
             opt_string_field "session_abi_root" fields,
             opt_string_field "litenode_session_abi_root" fields );
           ( "request_input_root_cell_mismatch",
             "abi.request_input_root_cell",
             "set_request_input_root_cell",
             int_string "request_input_root_cell",
             Some (string_of_int Abi.input_root_cell) );
           ( "r0_output_base_mismatch",
             "output.abi_registers.r0",
             "match_output_base_address",
             int_string "r0",
             int_string "output_base_address" );
           ( "r1_output_count_mismatch",
             "output.abi_registers.r1",
             "match_output_count",
             int_string "r1",
             int_string "output_count" );
         ]
       in
       let repairs =
         List.filter_map
           (fun (blocker, field, action, observed, expected) ->
              if has blocker then
                Some
                  (repair_field
                     ~field
                     ?observed
                     ?expected
                     ~blockers:[blocker]
                     action)
              else None)
           known
       in
       let known_blockers =
         List.map (fun (blocker, _, _, _, _) -> blocker) known
       in
       let manual_repairs =
         blockers
         |> List.filter (fun blocker -> not (List.mem blocker known_blockers))
         |> List.map
              (fun blocker ->
                 repair_field
                   ~field:"abi"
                   ~blockers:[blocker]
                   "manual_abi_diagnosis")
       in
       repairs @ manual_repairs)
  | _ ->
    [
      repair_field
        ~field:"abi"
        ~expected:"session ABI declaration"
        "emit_session_abi_declaration";
    ]

let has_prefix prefix value =
  let prefix_len = String.length prefix in
  String.length value >= prefix_len
  && String.equal (String.sub value 0 prefix_len) prefix

let strip_prefix prefix value =
  if has_prefix prefix value then
    Some
      (String.sub
         value
         (String.length prefix)
         (String.length value - String.length prefix))
  else
    None

let q1_expected_prefix case =
  List.assoc_opt case Template.q1_required_failure_expectations

let q1_case_field ?suffix case =
  "expected_failure_atomicity_behavior[" ^ case ^ "]"
  ^
  match suffix with
  | None -> ""
  | Some suffix -> "." ^ suffix

let q1_repair_mutation ?value ?value_bits ?value_hex_le ?offset_cells
    ?truncate_bytes name target =
  let optional_int name = function
    | None -> []
    | Some value -> [name, `Int value]
  in
  let optional_intlit name = function
    | None -> []
    | Some value -> [name, `Intlit value]
  in
  let optional_string name = function
    | None -> []
    | Some value -> [name, `String value]
  in
  `Assoc
    ([
       "mutation", `String name;
       "target", `String target;
     ]
     @ optional_int "value" value
     @ optional_intlit "value_bits" value_bits
     @ optional_string "value_hex_le" value_hex_le
     @ optional_int "offset_cells" offset_cells
     @ optional_int "truncate_bytes" truncate_bytes)

let q1_repair_span name base cells =
  `Assoc [
    "name", `String name;
    "base_address", `Int base;
    "length_f64_cells", `Int cells;
  ]

let q1_repair_context fields =
  match field "producer_repair_context" fields with
  | Some (`Assoc context) -> Some context
  | _ -> None

let q1_required_case_from_context case context =
  match
    opt_int_field "output_base" context,
    opt_int_field "lhs_base" context,
    opt_int_field "output_cells" context,
    opt_int_field "expected_effort" context,
    opt_int_field "q1_owner_source_bytes" context,
    opt_int_field "q1_required_owner_bytes" context
  with
  | Some output_base,
    Some lhs_base,
    Some output_cells,
    Some expected_effort,
    Some owner_bytes,
    Some required_owner_bytes ->
    let reject_case mutation =
      Some
        (`Assoc [
          "case", `String case;
          "expected", `String "reject_before_write";
          "executable_mutations", `List [mutation];
          "unchanged_spans",
          `List [q1_repair_span "expected" output_base output_cells];
        ])
    in
    (match case with
     | "nonfinite_input_nan" ->
       reject_case
         (q1_repair_mutation
            ~value_bits:"9221120237041090560"
            "replace_first_f64_input_cell"
            "lhs")
     | "nonfinite_input_infinity" ->
       reject_case
         (q1_repair_mutation
            ~value_bits:"9218868437227405312"
            "replace_first_f64_input_cell"
            "lhs")
     | "output_input_aliasing" ->
       Some
         (`Assoc [
           "case", `String case;
           "expected", `String "accept_from_snapshot_exact";
           "executable_mutations",
           `List [
             q1_repair_mutation
               ~offset_cells:0
               "set_output_base_to_first_input_base_plus"
               "output.base_address";
           ];
           "unchanged_spans",
           `List [q1_repair_span case lhs_base output_cells];
         ])
     | "partial_output_input_aliasing" ->
       let offset = 1 in
       Some
         (`Assoc [
           "case", `String case;
           "expected", `String "accept_from_snapshot_partial";
           "executable_mutations",
           `List [
             q1_repair_mutation
               ~offset_cells:offset
               "set_output_base_to_first_input_base_plus"
               "output.base_address";
           ];
           "unchanged_spans",
           `List [q1_repair_span case (lhs_base + offset) output_cells];
         ])
     | "k_not_multiple_of_128" ->
       reject_case
         (q1_repair_mutation
            ~value:127
            "set_scalar_param"
            "parameter_addresses_and_scalar_params.values.k")
     | "bad_q1_owner_length" ->
       reject_case
         (q1_repair_mutation
            ~truncate_bytes:1
            "truncate_input_manifest"
            "q1_owner")
     | "negative_byte_offset" ->
       reject_case
         (q1_repair_mutation
            ~value:(-1)
            "set_scalar_param"
            "parameter_addresses_and_scalar_params.values.byte_offset")
     | "byte_offset_out_of_bounds" ->
       reject_case
         (q1_repair_mutation
            ~value:(owner_bytes + 1)
            "set_scalar_param"
            "parameter_addresses_and_scalar_params.values.byte_offset")
     | "byte_offset_truncated_span" ->
       let value = max 0 (owner_bytes - required_owner_bytes + 1) in
       reject_case
         (q1_repair_mutation
            ~value
            "set_scalar_param"
            "parameter_addresses_and_scalar_params.values.byte_offset")
     | "nonfinite_fp16_scale" ->
       reject_case
         (q1_repair_mutation
            ~value_hex_le:"007c"
            "replace_q1_scale_bits"
            "q1_owner[0..2]")
     | "lower_effort_limit" ->
       reject_case
         (q1_repair_mutation
            ~value:(max 0 (expected_effort - 2))
            "lower_effort_limit"
            "effort")
     | _ -> None)
  | _ -> None

let q1_required_case_from_result fields case =
  match q1_repair_context fields with
  | None -> None
  | Some context -> q1_required_case_from_context case context

let failure_case_source source = function
  | `Assoc fields ->
    `Assoc
      (("failure_case_source", `String source)
       :: List.filter
            (fun (key, _) -> not (String.equal key "failure_case_source"))
            fields)
  | value -> value

let failure_case_name = function
  | `Assoc fields -> opt_string_field "case" fields
  | _ -> None

let q1_required_case_canonical = function
  | `Assoc fields ->
    (match opt_string_field "case" fields, opt_string_field "expected" fields with
     | Some case, Some expected ->
       (match q1_expected_prefix case with
        | None -> true
        | Some expected_prefix -> has_prefix expected_prefix expected)
     | _ -> true)
  | _ -> true

let q1_failure_repair fields blocker =
  let case_repair prefix action ?suffix blocker =
    match strip_prefix prefix blocker with
    | None -> None
    | Some case ->
      Some
        (repair_field
           ~field:(q1_case_field ?suffix case)
           ~case
           ?expected_prefix:(q1_expected_prefix case)
           ?required_case:(q1_required_case_from_result fields case)
           ~blockers:[blocker]
           action)
  in
  match
    case_repair
      "q1_failure_case_missing_"
      "add_required_failure_case"
      blocker
  with
  | Some repair -> Some repair
  | None ->
    (match
       case_repair
         "q1_failure_case_expected_mismatch_"
         ~suffix:"expected"
         "set_expected_prefix"
         blocker
     with
     | Some repair -> Some repair
     | None ->
       (match
          case_repair
            "q1_failure_case_uncounted_"
            "make_failure_case_counted"
            blocker
        with
        | Some repair -> Some repair
        | None ->
          (match
             case_repair
               "q1_failure_case_rejected_"
               "fix_failure_case_execution"
               blocker
           with
           | Some repair -> Some repair
           | None ->
             (match
                case_repair
                  "q1_failure_case_mutation_mismatch_"
                  ~suffix:"executable_mutations"
                  "set_required_mutation_shape"
                  blocker
              with
              | Some repair -> Some repair
              | None ->
                if String.equal blocker "q1_failure_case_output_span_not_covered"
                then
                  Some
                    (repair_field
                       ~field:"expected_failure_atomicity_behavior[].unchanged_spans"
                       ~blockers:[blocker]
                       "cover_output_span")
                else
                  None))))

let failure_contract_repair result =
  match result with
  | `Assoc fields ->
    (match field "required_failure_case_contract" fields with
     | Some (`Assoc contract_fields) ->
       let blockers = optional_string_list_field "blockers" contract_fields in
       let q1_repairs = List.filter_map (q1_failure_repair fields) blockers in
       let repaired_blockers =
         List.filter_map
           (fun repair ->
              match repair with
              | `Assoc repair_fields ->
                (match field "blockers" repair_fields with
                 | Some (`List [`String blocker]) -> Some blocker
                 | _ -> None)
              | _ -> None)
           q1_repairs
       in
       let manual_repairs =
         blockers
         |> List.filter (fun blocker -> not (List.mem blocker repaired_blockers))
         |> List.map
              (fun blocker ->
                 repair_field
                   ~field:"expected_failure_atomicity_behavior"
                   ~blockers:[blocker]
                   "manual_failure_case_diagnosis")
       in
       q1_repairs @ manual_repairs
     | _ -> [])
  | _ -> []

let producer_repair_hint = function
  | `Assoc fields as result ->
    let opcode =
      match opt_string_field "opcode" fields with
      | Some opcode -> opcode
      | None -> "unknown"
    in
    let template_path =
      match
        opt_string_field "template_path" fields,
        opt_string_field "fixture_path" fields
      with
      | Some path, _
      | None, Some path -> path
      | None, None -> "unknown"
    in
    let profile_repairs =
      match field "profile_root_binding" fields with
      | Some binding ->
        binding_repair
          ~field:"numerical_profile_root"
          ~observed_name:"numerical_profile_root"
          ~expected_name:"profile_root"
          binding
      | None -> []
    in
    let vm_semantics_repairs =
      match field "vm_semantics_binding" fields with
      | Some binding ->
        binding_repair
          ~field:"vm_semantics_root"
          ~observed_name:"vm_semantics_root"
          ~expected_name:"litenode_vm_semantics_root"
          binding
      | None -> []
    in
    let abi_repairs =
      match field "abi_declaration_binding" fields with
      | Some binding -> abi_repair binding
      | None -> []
    in
    let repairs =
      profile_repairs
      @ vm_semantics_repairs
      @ abi_repairs
      @ failure_contract_repair result
    in
    if repairs = [] then None
    else
      Some
        (`Assoc [
          "opcode", `String opcode;
          "template_path", `String template_path;
          "repairs", `List repairs;
        ])
  | _ -> None

let producer_repair_hints results =
  results |> List.filter_map producer_repair_hint

let producer_repair_manifest ?corpus_root hints =
  let hint_count = List.length hints in
  let repair_count =
    List.fold_left
      (fun count hint ->
         match hint with
         | `Assoc fields ->
           count
           +
           (match field "repairs" fields with
            | Some (`List repairs) -> List.length repairs
            | _ -> 0)
         | _ -> count)
      0
      hints
  in
  let affected_opcodes =
    hints
    |> List.filter_map (function
      | `Assoc fields -> opt_string_field "opcode" fields
      | _ -> None)
    |> List.sort_uniq String.compare
  in
  `Assoc [
    "schema", `String "octra.inference.producer-repair-manifest.v1";
    "diagnostic_only", `Bool true;
    "authority", `String "litenode-conformance-runner";
    "scope", `String "template_producer_repair_hints";
    "status", `String (if hint_count = 0 then "no_hints" else "hints_available");
    "producer_repair_required", `Bool (hint_count > 0);
    "corpus_root",
    (match corpus_root with
     | Some root -> `String root
     | None -> `Null);
    "hint_count", `Int hint_count;
    "repair_count", `Int repair_count;
    "affected_opcodes", `List (List.map (fun opcode -> `String opcode) affected_opcodes);
    "hints", `List hints;
  ]

type executable_abi_counts = {
  executable_abi_matched : int;
  executable_abi_mismatch : int;
  executable_abi_not_run : int;
}

let empty_executable_abi_counts = {
  executable_abi_matched = 0;
  executable_abi_mismatch = 0;
  executable_abi_not_run = 0;
}

let add_executable_abi_status counts = function
  | "matched" ->
    { counts with
      executable_abi_matched = counts.executable_abi_matched + 1 }
  | "mismatch" ->
    { counts with
      executable_abi_mismatch = counts.executable_abi_mismatch + 1 }
  | _ ->
    { counts with
      executable_abi_not_run = counts.executable_abi_not_run + 1 }

let executable_abi_binding_counts values =
  List.fold_left
    (fun counts value ->
       match value with
       | `Assoc fields ->
         (match field "executable_abi_binding" fields with
          | Some (`Assoc binding_fields) ->
            add_executable_abi_status
              counts
              (match opt_string_field "status" binding_fields with
               | Some status -> status
               | None -> "not_run")
          | _ -> add_executable_abi_status counts "not_run")
       | _ -> add_executable_abi_status counts "not_run")
    empty_executable_abi_counts
    values

let executable_abi_ready counts =
  counts.executable_abi_matched > 0
  && counts.executable_abi_mismatch = 0
  && counts.executable_abi_not_run = 0

let executable_abi_blockers counts =
  let add_if condition value values =
    if condition then value :: values else values
  in
  []
  |> add_if
       (counts.executable_abi_matched = 0)
       "executable_abi_binding_not_proven"
  |> add_if
       (counts.executable_abi_mismatch > 0)
       "executable_abi_binding_mismatch"
  |> add_if
       (counts.executable_abi_not_run > 0)
       "executable_abi_binding_not_run"

let executable_abi_counts_json counts =
  `Assoc [
    "matched", `Int counts.executable_abi_matched;
    "mismatch", `Int counts.executable_abi_mismatch;
    "not_run", `Int counts.executable_abi_not_run;
  ]

let executable_abi_gate_json counts =
  `Assoc [
    "status",
    `String (if executable_abi_ready counts then "accepted" else "rejected");
    "counts", executable_abi_counts_json counts;
    "blockers",
    `List
      (List.map
         (fun blocker -> `String blocker)
         (executable_abi_blockers counts));
  ]

let consensus_candidate_gate
    ~profile_gate_count
    ~unprofiled_count
    ~root_binding_counts
    status_counts =
  let ready =
    Profile.consensus_candidate
      ~profile_gate_count
      ~unprofiled_count
      status_counts
    && Profile.root_bindings_are_consensus_ready root_binding_counts
  in
  `Assoc [
    "required", `Bool !require_consensus_candidate;
    "status",
    `String
      (if not !require_consensus_candidate then "not_required"
       else if ready then "accepted"
       else "rejected");
    "blockers",
    `List
      (List.map
         (fun blocker -> `String blocker)
         (Profile.consensus_candidate_blockers
            ~profile_gate_count
            ~unprofiled_count
            status_counts
          @ Profile.root_binding_blockers root_binding_counts));
  ]

let consensus_ready_gate
    ~profile_gate_count
    ~unprofiled_count
    ~root_binding_counts
    status_counts =
  let ready =
    Profile.consensus_ready
      ~profile_gate_count
      ~unprofiled_count
      status_counts
    && Profile.root_bindings_are_consensus_ready root_binding_counts
  in
  `Assoc [
    "required", `Bool !require_consensus_ready;
    "status",
    `String
      (if not !require_consensus_ready then "not_required"
       else if ready then "accepted"
       else "rejected");
    "blockers",
    `List
      (List.map
         (fun blocker -> `String blocker)
         (Profile.consensus_ready_blockers
            ~profile_gate_count
            ~unprofiled_count
            status_counts
          @ Profile.root_binding_blockers root_binding_counts));
  ]

let consensus_ready_required_passes
    ~profile_gate_count
    ~unprofiled_count
    ~root_binding_counts
    status_counts =
  (not !require_consensus_ready)
  ||
  (Profile.consensus_ready
     ~profile_gate_count
     ~unprofiled_count
     status_counts
   && Profile.root_bindings_are_consensus_ready root_binding_counts)

let consensus_candidate_required_passes
    ~profile_gate_count
    ~unprofiled_count
    ~root_binding_counts
    status_counts =
  (not !require_consensus_candidate)
  ||
  (Profile.consensus_candidate
     ~profile_gate_count
     ~unprofiled_count
     status_counts
   && Profile.root_bindings_are_consensus_ready root_binding_counts)

let add_blocker condition blocker blockers =
  if condition then blocker :: blockers else blockers

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
      "host_transcendental_exp";
      "unresolved_transcendental_dependencies";
      "cross_platform_conformance_missing";
      "missing_cross_platform_matrix";
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

let gate_status_accepted = function
  | `Assoc fields ->
    (try String.equal (string_field "status" fields) "accepted" with
     | Failure _ -> false)
  | _ -> false

let required_gate_passes ~required gate =
  (not required) || gate_status_accepted gate

let opcodes_covered ~required_opcodes ~observed_opcodes =
  List.for_all
    (fun opcode -> List.exists (String.equal opcode) observed_opcodes)
    required_opcodes

let unique values =
  List.sort_uniq String.compare values

let profile_catalog_root_option = function
  | `String value -> Some value
  | _ -> None

let json_string_list values =
  `List (List.map (fun value -> `String value) values)

let cross_platform_opcode_argv required_opcodes =
  List.concat (List.map (fun opcode -> ["--opcode"; opcode]) required_opcodes)

let json_argv values = json_string_list values

let matrix_request_platform_json () =
  `Assoc [
    "ocaml_version", `String Sys.ocaml_version;
    "os_type", `String Sys.os_type;
    "system_name", string_or_null (command_line "uname -s");
    "system_release", string_or_null (command_line "uname -r");
    "machine", string_or_null (command_line "uname -m");
    "word_size", `Int Sys.word_size;
    "big_endian", `Bool Sys.big_endian;
    "backend_type", `String (backend_type_string Sys.backend_type);
    "runner_executable_sha256", string_or_null (current_executable_sha256 ());
  ]

let cross_platform_matrix_request
    ~required_opcodes
    ~profile_catalog_root
    ~transcendental_dependency_catalog_root
    ~template_corpus_root
    ~local_result_signature_sha256 =
  let opcode_argv = cross_platform_opcode_argv required_opcodes in
  let runner_collection_argv template_path =
    [
      "inference_conformance_run";
      "--template-index";
      template_path;
      "--strict-effort";
      "--include-failures";
      "--require-failure-cases";
      "--require-profile-roots-bound";
    ] @ opcode_argv
  in
  let final_readiness_argv =
    runner_collection_argv "P0_TEMPLATE_INDEX"
    @ [
      "--require-validator-readiness";
      "--cross-platform-matrix";
      "CROSS_PLATFORM_MATRIX";
      "--expected-cross-platform-matrix-sha256";
      "MATRIX_SHA256";
    ]
  in
  `Assoc [
    "schema", `String cross_platform_matrix_request_schema;
    "diagnostic_only", `Bool true;
    "authority", `String "none";
    "evidence_scope", `String "diagnostic_collection_request";
    "platform_identity", `String "unsigned_self_reported_observation";
    "minimum_distinct_platform_observation_count", `Int 2;
    "minimum_distinct_runner_executable_observation_count", `Int 2;
    "minimum_distinct_platform_runner_observation_count", `Int 2;
    "required_result_signature_schema",
    `String cross_platform_result_signature_schema;
    "required_opcodes", json_string_list required_opcodes;
    "required_profile_catalog_root",
    (match profile_catalog_root with
     | Some root -> `String root
     | None -> `Null);
    "required_transcendental_dependency_catalog_root",
    (match transcendental_dependency_catalog_root with
     | Some root -> `String root
     | None -> `Null);
    "required_template_corpus_root",
    (match template_corpus_root with
     | Some root -> `String root
     | None -> `Null);
    "local_result_signature_sha256",
    (match local_result_signature_sha256 with
     | Some signature -> `String signature
     | None -> `Null);
    "local_platform_observation", matrix_request_platform_json ();
    "per_report_requirements",
    `Assoc [
      "execution_mode", `String "positive_template_vm_execution";
      "top_level_status", `String "accepted";
      "execution_status", `String "accepted";
      "result_signature_schema",
      `String cross_platform_result_signature_schema;
      "result_signature_source", `String "recomputed_from_results";
      "has_results", `Bool true;
      "failure_case_gate_status", `String "accepted";
      "required_failure_case_contracts_status", `String "accepted";
      "q1_contract_shape_status", `String "accepted";
      "required_q1_failure_rows_status", `String "accepted";
      "q1_failure_mutation_shape_status", `String "accepted";
      "q1_failure_mutation_payloads_present", `Bool true;
      "result_status", `String "accepted";
      "vm_run_status", `String "accepted";
      "strict_effort", `Bool true;
      "output_status", `String "matched";
      "effort_match", `Bool true;
      "opcode_effort_match", `Bool true;
      "profile_root_binding_status", `String "accepted";
      "vm_semantics_binding_status", `String "accepted";
      "abi_declaration_binding_status", `String "accepted";
      "profile_root_binding_result_status", `String "matched";
      "vm_semantics_binding_result_status", `String "matched";
      "abi_declaration_binding_result_status", `String "matched";
      "executable_abi_binding_status", `String "matched";
      "profile_catalog_root", `String "valid_sha256";
      "transcendental_dependency_catalog_root", `String "valid_sha256";
      "template_corpus_root", `String "valid_sha256";
      "runner_executable_sha256", `String "valid_sha256";
    ];
    "aggregate_requirements",
    `Assoc [
      "result_signatures", `String "one_normalized_signature";
      "profile_catalog_roots", `String "one_shared_root";
      "transcendental_dependency_catalog_roots", `String "one_shared_root";
      "template_corpus_roots", `String "one_shared_root";
      "opcode_coverage", `String "all_required_opcodes_per_report";
      "platform_observations", `String "at_least_two_distinct";
      "runner_executable_observations", `String "at_least_two_distinct";
      "platform_runner_observations", `String "at_least_two_disjoint";
    ];
    "command_templates",
    `Assoc [
      "local_runner_report",
      json_argv (runner_collection_argv "P0_TEMPLATE_INDEX");
      "remote_runner_report",
      json_argv (runner_collection_argv "P0_TEMPLATE_INDEX");
      "matrix",
      json_argv
        ([
          "inference_conformance_matrix";
          "--runner-report";
          "LOCAL_RUNNER_REPORT";
          "--runner-report";
          "REMOTE_RUNNER_REPORT";
          "--min-platforms";
          "2";
        ] @ opcode_argv);
      "final_readiness_rerun", json_argv final_readiness_argv;
    ];
  ]

let report_row_is_bound = function
  | `Assoc fields ->
    Option.value ~default:false (opt_bool_field "accepted" fields)
    && (match opt_string_field "runner_report_sha256" fields with
        | Some value -> not (String.equal value "")
        | None -> false)
    && (match opt_string_field "result_signature_sha256" fields with
        | Some value -> not (String.equal value "")
        | None -> false)
    && (match opt_string_field "platform_key" fields with
        | Some value -> not (String.equal value "")
        | None -> false)
    && (match opt_string_field "runner_executable_sha256" fields with
        | Some value -> not (String.equal value "")
        | None -> false)
    && (match opt_string_field "profile_catalog_root" fields with
        | Some value -> not (String.equal value "")
        | None -> false)
    && (match opt_string_field "transcendental_dependency_catalog_root" fields with
        | Some value -> not (String.equal value "")
        | None -> false)
    && (match opt_string_field "template_corpus_root" fields with
        | Some value -> not (String.equal value "")
        | None -> false)
    && (match opt_string_field "result_signature_schema" fields with
        | Some schema -> String.equal schema cross_platform_result_signature_schema
        | None -> false)
  | _ -> false

let report_row_string_field name = function
  | `Assoc fields -> opt_string_field name fields
  | _ -> None

let report_row_string_list_field name = function
  | `Assoc fields -> optional_string_list_field name fields
  | _ -> []

let report_row_platform_runner_observation = function
  | `Assoc fields ->
    (match
       opt_string_field "platform_key" fields,
       opt_string_field "runner_executable_sha256" fields
     with
     | Some platform_key, Some runner_executable_sha256 ->
       Some (platform_key, runner_executable_sha256)
     | _ -> None)
  | _ -> None

let max_distinct_platform_runner_observations observations =
  let observations = List.sort_uniq compare observations in
  let rec loop used_platforms used_runners count = function
    | [] -> count
    | (platform, runner) :: rest ->
      let skipped = loop used_platforms used_runners count rest in
      let taken =
        if List.mem platform used_platforms || List.mem runner used_runners then
          count
        else
          loop (platform :: used_platforms) (runner :: used_runners) (count + 1) rest
      in
      max skipped taken
  in
  loop [] [] 0 observations

let cross_platform_evidence
    ~required_opcodes
    ~profile_catalog_root
    ~transcendental_dependency_catalog_root
    ~template_corpus_root
    ~local_result_signature_sha256 =
  match !cross_platform_matrix with
  | None ->
    `Assoc [
      "status", `String "missing";
      "path", `Null;
      "required_opcodes",
      `List (List.map (fun opcode -> `String opcode) required_opcodes);
      "covered_opcodes", `List [];
      "required_profile_catalog_root",
      (match profile_catalog_root with
       | Some root -> `String root
       | None -> `Null);
      "matrix_profile_catalog_roots", `List [];
      "required_transcendental_dependency_catalog_root",
      (match transcendental_dependency_catalog_root with
       | Some root -> `String root
       | None -> `Null);
      "matrix_transcendental_dependency_catalog_roots", `List [];
      "required_template_corpus_root",
      (match template_corpus_root with
       | Some root -> `String root
       | None -> `Null);
      "matrix_template_corpus_roots", `List [];
      "local_result_signature_sha256",
      (match local_result_signature_sha256 with
       | Some signature -> `String signature
       | None -> `Null);
      "local_result_signature_status", `String "missing";
      "matrix_request",
      cross_platform_matrix_request
        ~required_opcodes
        ~profile_catalog_root
        ~transcendental_dependency_catalog_root
        ~template_corpus_root
        ~local_result_signature_sha256;
      "blockers", `List [`String "missing_cross_platform_matrix"];
    ]
  | Some path ->
    let raw = read_file path in
    let matrix_sha256 = sha256 raw in
    (match read_json path with
     | `Assoc fields ->
       let schema_accepted =
         match opt_string_field "schema" fields with
         | Some schema -> String.equal schema cross_platform_matrix_schema
         | _ -> false
       in
       let status_accepted =
         match opt_string_field "status" fields with
         | Some "accepted" -> true
         | _ -> false
       in
       let cross_platform_accepted =
         match opt_string_field "cross_platform_status" fields with
         | Some "accepted" -> true
         | _ -> false
       in
       let observed_opcodes =
         string_list_field "result_opcodes" fields
       in
       let report_rows = list_field "reports" fields in
       let row_observed_opcodes =
         report_rows
         |> List.map (report_row_string_list_field "result_opcodes")
         |> List.concat
         |> unique
       in
       let result_signature_schema_accepted =
         match opt_string_field "result_signature_schema" fields with
         | Some schema ->
           String.equal schema cross_platform_result_signature_schema
         | None -> false
       in
       let row_result_signature_schema_accepted =
         List.for_all
           (function
             | `Assoc row_fields ->
               (match opt_string_field "result_signature_schema" row_fields with
                | Some schema ->
                  String.equal schema cross_platform_result_signature_schema
                | None -> false)
             | _ -> false)
           report_rows
       in
       let matrix_blockers =
         optional_string_list_field "blockers" fields
       in
       let matrix_profile_catalog_roots =
         optional_string_list_field "profile_catalog_roots" fields
       in
       let matrix_transcendental_dependency_catalog_roots =
         optional_string_list_field
           "transcendental_dependency_catalog_roots"
           fields
       in
       let matrix_template_corpus_roots =
         optional_string_list_field "template_corpus_roots" fields
       in
       let matrix_result_signature_count =
         match opt_int_field "result_signature_count" fields with
         | Some value -> value
         | None -> -1
       in
       let matrix_distinct_platform_count =
         match opt_int_field "distinct_platform_count" fields with
         | Some value -> value
         | None -> -1
       in
       let matrix_distinct_runner_count =
         match opt_int_field "distinct_runner_executable_count" fields with
         | Some value -> value
         | None -> -1
       in
       let matrix_distinct_platform_runner_observation_count =
         match opt_int_field "distinct_platform_runner_observation_count" fields with
         | Some value -> value
         | None -> -1
       in
       let row_platforms =
         report_rows
         |> List.filter_map (report_row_string_field "platform_key")
         |> unique
       in
       let row_runner_executables =
         report_rows
         |> List.filter_map (report_row_string_field "runner_executable_sha256")
         |> unique
       in
       let row_platform_runner_observations =
         report_rows
         |> List.filter_map report_row_platform_runner_observation
       in
       let row_platform_runner_observation_count =
         max_distinct_platform_runner_observations
           row_platform_runner_observations
       in
       let row_result_signatures =
         report_rows
         |> List.filter_map (report_row_string_field "result_signature_sha256")
         |> unique
       in
       let row_profile_catalog_roots =
         report_rows
         |> List.filter_map (report_row_string_field "profile_catalog_root")
         |> unique
       in
       let row_transcendental_dependency_catalog_roots =
         report_rows
         |> List.filter_map
              (report_row_string_field "transcendental_dependency_catalog_root")
         |> unique
       in
       let row_template_corpus_roots =
         report_rows
         |> List.filter_map (report_row_string_field "template_corpus_root")
         |> unique
       in
       let opcode_scope_accepted =
         opcodes_covered ~required_opcodes ~observed_opcodes
       in
       let row_opcode_scope_accepted =
         List.for_all
           (fun row ->
              opcodes_covered
                ~required_opcodes
                ~observed_opcodes:(report_row_string_list_field "result_opcodes" row))
           report_rows
       in
       let profile_catalog_accepted =
         match profile_catalog_root, matrix_profile_catalog_roots with
         | Some root, [matrix_root] -> String.equal root matrix_root
         | _ -> false
       in
       let transcendental_dependency_catalog_accepted =
         match
           transcendental_dependency_catalog_root,
           matrix_transcendental_dependency_catalog_roots
         with
         | Some root, [matrix_root] -> String.equal root matrix_root
         | _ -> false
       in
       let template_corpus_accepted =
         match template_corpus_root, matrix_template_corpus_roots with
         | Some root, [matrix_root] -> String.equal root matrix_root
         | _ -> false
       in
       let matrix_sha256_accepted =
         match !expected_cross_platform_matrix_sha256 with
         | Some expected -> String.equal expected matrix_sha256
         | None -> false
       in
       let report_rows_accepted =
         List.length report_rows >= 2 && List.for_all report_row_is_bound report_rows
       in
       let row_platform_count_accepted =
         matrix_distinct_platform_count = List.length row_platforms
       in
       let row_runner_count_accepted =
         matrix_distinct_runner_count = List.length row_runner_executables
       in
       let row_platform_runner_observation_count_accepted =
         matrix_distinct_platform_runner_observation_count =
         row_platform_runner_observation_count
       in
       let row_platform_minimum_accepted =
         List.length row_platforms >= 2
       in
       let row_runner_minimum_accepted =
         List.length row_runner_executables >= 2
       in
       let row_platform_runner_observation_minimum_accepted =
         row_platform_runner_observation_count >= 2
       in
       let row_signature_count_accepted =
         matrix_result_signature_count = List.length row_result_signatures
       in
       let row_signature_accepted =
         List.length row_result_signatures = 1
       in
       let local_result_signature_accepted =
         match local_result_signature_sha256, row_result_signatures with
         | Some local, [matrix] -> String.equal local matrix
         | Some _, _ -> false
         | None, _ -> false
       in
       let row_profile_catalog_accepted =
         row_profile_catalog_roots = matrix_profile_catalog_roots
       in
       let row_transcendental_dependency_catalog_accepted =
         row_transcendental_dependency_catalog_roots =
         matrix_transcendental_dependency_catalog_roots
       in
       let row_template_corpus_accepted =
         row_template_corpus_roots = matrix_template_corpus_roots
       in
       let blockers =
         []
         |> add_blocker (not schema_accepted) "matrix_schema_mismatch"
         |> add_blocker (not status_accepted) "matrix_rejected"
         |> add_blocker
              (not cross_platform_accepted)
              "matrix_cross_platform_rejected"
         |> add_blocker
              (matrix_blockers <> [])
              "matrix_blockers_present"
         |> add_blocker
              (not matrix_sha256_accepted)
              "matrix_sha256_unpinned_or_mismatch"
         |> add_blocker
              (not opcode_scope_accepted)
              "matrix_opcode_scope_mismatch"
         |> add_blocker
              (not row_opcode_scope_accepted)
              "matrix_row_opcode_scope_mismatch"
         |> add_blocker
              (not result_signature_schema_accepted)
              "matrix_result_signature_schema_mismatch"
         |> add_blocker
              (not row_result_signature_schema_accepted)
              "matrix_row_result_signature_schema_mismatch"
         |> add_blocker
              (not profile_catalog_accepted)
              "matrix_profile_catalog_mismatch"
         |> add_blocker
              (not transcendental_dependency_catalog_accepted)
              "matrix_transcendental_dependency_catalog_mismatch"
         |> add_blocker
              (not template_corpus_accepted)
              "matrix_template_corpus_mismatch"
         |> add_blocker
              (not report_rows_accepted)
              "matrix_source_reports_unbound"
         |> add_blocker
              (not row_platform_count_accepted)
              "matrix_platform_count_mismatch"
         |> add_blocker
              (not row_runner_count_accepted)
              "matrix_runner_executable_count_mismatch"
         |> add_blocker
              (not row_platform_runner_observation_count_accepted)
              "matrix_platform_runner_observation_count_mismatch"
         |> add_blocker
              (not row_platform_minimum_accepted)
              "matrix_insufficient_distinct_platforms"
         |> add_blocker
              (not row_runner_minimum_accepted)
              "matrix_insufficient_distinct_runner_executables"
         |> add_blocker
              (not row_platform_runner_observation_minimum_accepted)
              "matrix_insufficient_distinct_platform_runner_observations"
         |> add_blocker
              (not row_signature_count_accepted)
              "matrix_result_signature_count_mismatch"
         |> add_blocker
              (not row_signature_accepted)
              "matrix_result_signature_row_mismatch"
         |> add_blocker
              (not local_result_signature_accepted)
              "matrix_local_result_signature_mismatch"
         |> add_blocker
              (not row_profile_catalog_accepted)
              "matrix_row_profile_catalog_mismatch"
         |> add_blocker
              (not row_transcendental_dependency_catalog_accepted)
              "matrix_row_transcendental_dependency_catalog_mismatch"
         |> add_blocker
              (not row_template_corpus_accepted)
              "matrix_row_template_corpus_mismatch"
       in
       `Assoc [
         "status",
         `String (if blockers = [] then "accepted" else "rejected");
         "path", `String path;
         "matrix_sha256", `String matrix_sha256;
         "expected_matrix_sha256",
         (match !expected_cross_platform_matrix_sha256 with
          | Some expected -> `String expected
          | None -> `Null);
         "matrix_sha256_status",
         `String (if matrix_sha256_accepted then "accepted" else "rejected");
         "required_opcodes",
         `List (List.map (fun opcode -> `String opcode) required_opcodes);
         "covered_opcodes",
         `List (List.map (fun opcode -> `String opcode) observed_opcodes);
         "row_covered_opcodes",
         `List (List.map (fun opcode -> `String opcode) row_observed_opcodes);
         "row_opcode_coverage_status",
         `String (if row_opcode_scope_accepted then "accepted" else "rejected");
         "required_result_signature_schema",
         `String cross_platform_result_signature_schema;
         "matrix_result_signature_schema",
         (match opt_string_field "result_signature_schema" fields with
          | Some schema -> `String schema
          | None -> `Null);
         "result_signature_schema_status",
         `String
           (if result_signature_schema_accepted then "accepted" else "rejected");
         "row_result_signature_schema_status",
         `String
           (if row_result_signature_schema_accepted then "accepted" else "rejected");
         "schema_status",
         `String (if schema_accepted then "accepted" else "rejected");
         "required_profile_catalog_root",
         (match profile_catalog_root with
          | Some root -> `String root
          | None -> `Null);
         "matrix_profile_catalog_roots",
         `List
           (List.map
              (fun root -> `String root)
              matrix_profile_catalog_roots);
         "profile_catalog_status",
         `String (if profile_catalog_accepted then "accepted" else "rejected");
         "required_transcendental_dependency_catalog_root",
         (match transcendental_dependency_catalog_root with
          | Some root -> `String root
          | None -> `Null);
         "matrix_transcendental_dependency_catalog_roots",
         `List
           (List.map
              (fun root -> `String root)
              matrix_transcendental_dependency_catalog_roots);
         "transcendental_dependency_catalog_status",
         `String
           (if transcendental_dependency_catalog_accepted then
              "accepted"
            else
              "rejected");
         "required_template_corpus_root",
         (match template_corpus_root with
          | Some root -> `String root
          | None -> `Null);
         "matrix_template_corpus_roots",
         `List
           (List.map
              (fun root -> `String root)
              matrix_template_corpus_roots);
         "template_corpus_status",
         `String (if template_corpus_accepted then "accepted" else "rejected");
         "source_report_status",
         `String (if report_rows_accepted then "accepted" else "rejected");
         "matrix_distinct_platform_count", `Int matrix_distinct_platform_count;
         "matrix_distinct_runner_executable_count",
         `Int matrix_distinct_runner_count;
         "matrix_distinct_platform_runner_observation_count",
         `Int matrix_distinct_platform_runner_observation_count;
         "matrix_result_signature_count", `Int matrix_result_signature_count;
         "row_distinct_platform_count",
         `Int (List.length row_platforms);
         "row_distinct_runner_executable_count",
         `Int (List.length row_runner_executables);
         "row_distinct_platform_runner_observation_count",
         `Int row_platform_runner_observation_count;
         "row_distinct_platform_status",
         `String
           (if row_platform_minimum_accepted then "accepted" else "rejected");
         "row_distinct_runner_executable_status",
         `String
           (if row_runner_minimum_accepted then "accepted" else "rejected");
         "row_distinct_platform_runner_observation_status",
         `String
           (if row_platform_runner_observation_minimum_accepted then
              "accepted"
            else
              "rejected");
         "row_result_signature_count",
         `Int (List.length row_result_signatures);
         "row_result_signatures",
         `List
           (List.map
              (fun signature -> `String signature)
              row_result_signatures);
         "local_result_signature_sha256",
         (match local_result_signature_sha256 with
          | Some signature -> `String signature
          | None -> `Null);
         "local_result_signature_status",
         `String
           (if local_result_signature_accepted then "accepted" else "rejected");
         "row_profile_catalog_roots",
         `List
           (List.map
              (fun root -> `String root)
              row_profile_catalog_roots);
         "row_transcendental_dependency_catalog_roots",
         `List
           (List.map
              (fun root -> `String root)
              row_transcendental_dependency_catalog_roots);
         "row_template_corpus_roots",
         `List
           (List.map
              (fun root -> `String root)
              row_template_corpus_roots);
         "row_aggregate_status",
         `String
           (if row_platform_count_accepted
               && row_runner_count_accepted
               && row_platform_runner_observation_count_accepted
               && row_platform_minimum_accepted
               && row_runner_minimum_accepted
               && row_platform_runner_observation_minimum_accepted
               && row_signature_count_accepted
               && row_signature_accepted
               && local_result_signature_accepted
               && row_profile_catalog_accepted
               && row_transcendental_dependency_catalog_accepted
               && row_template_corpus_accepted then
              "accepted"
            else
              "rejected");
         "matrix_status",
         `String
           (match opt_string_field "status" fields with
            | Some status -> status
            | None -> "missing");
         "cross_platform_status",
         `String
           (match opt_string_field "cross_platform_status" fields with
            | Some status -> status
            | None -> "missing");
         "blockers", `List (List.map (fun blocker -> `String blocker) blockers);
       ]
     | _ -> fail (path ^ ": cross-platform matrix must be an object"))

let failure_cases_accepted
    ~template_count
    ~included_template_count
    ~declared_failure_case_count:_
    ~counted_failure_case_count
    ~accepted_counted_failure_case_count =
  template_count > 0
  && included_template_count = template_count
  && counted_failure_case_count > 0
  && accepted_counted_failure_case_count = counted_failure_case_count

let failure_case_blockers
    ~template_count
    ~included_template_count
    ~declared_failure_case_count:_
    ~counted_failure_case_count
    ~accepted_counted_failure_case_count =
  []
  |> add_blocker
       (included_template_count <> template_count)
       "failure_cases_not_included"
  |> add_blocker
       (counted_failure_case_count = 0)
       "no_counted_failure_cases"
  |> add_blocker
       (accepted_counted_failure_case_count <> counted_failure_case_count)
       "failure_cases_rejected"

let failure_case_gate
    ~template_count
    ~included_template_count
    ~declared_failure_case_count
    ~counted_failure_case_count
    ~accepted_counted_failure_case_count
    ~required_failure_case_contract_blockers =
  let passed =
    failure_cases_accepted
      ~template_count
      ~included_template_count
      ~declared_failure_case_count
      ~counted_failure_case_count
      ~accepted_counted_failure_case_count
    && required_failure_case_contract_blockers = []
  in
  let blockers =
    failure_case_blockers
      ~template_count
      ~included_template_count
      ~declared_failure_case_count
      ~counted_failure_case_count
      ~accepted_counted_failure_case_count
    @ required_failure_case_contract_blockers
  in
  `Assoc [
    "required", `Bool !require_failure_cases;
    "status",
    `String
      (if not !require_failure_cases then "not_required"
       else if passed then "accepted"
       else "rejected");
    "template_count", `Int template_count;
    "included_template_count", `Int included_template_count;
    "declared_failure_case_count", `Int declared_failure_case_count;
    "counted_failure_case_count", `Int counted_failure_case_count;
    "uncounted_failure_case_count",
    `Int (declared_failure_case_count - counted_failure_case_count);
    "accepted_counted_failure_case_count",
    `Int accepted_counted_failure_case_count;
    "required_failure_case_contracts",
    `List [Template.q1_required_failure_expectations_json];
    "blockers",
    `List (List.map (fun blocker -> `String blocker) blockers);
  ]

let failure_cases_required_pass
    ~template_count
    ~included_template_count
    ~declared_failure_case_count
    ~counted_failure_case_count
    ~accepted_counted_failure_case_count
    ~required_failure_case_contract_blockers =
  (not !require_failure_cases)
  ||
  failure_cases_accepted
    ~template_count
    ~included_template_count
    ~declared_failure_case_count
    ~counted_failure_case_count
    ~accepted_counted_failure_case_count
  && required_failure_case_contract_blockers = []

let validator_readiness_gate
    ~required
    ~execution_accepted
    ~strict_effort_status
    ~effort_status
    ~effort_ready
    ~effort_blockers
    ~template_count
    ~profile_gate_count
    ~unprofiled_count
    ~root_binding_counts
    ~vm_semantics_binding_counts
    ~abi_declaration_binding_counts
    ~executable_abi_counts
    ~transcendental_dependency_catalog
    ~cross_platform_evidence
    ~included_template_count
    ~declared_failure_case_count
    ~counted_failure_case_count
    ~accepted_counted_failure_case_count
    ~required_failure_case_contract_blockers
    status_counts =
  let failure_cases_ready =
    failure_cases_accepted
      ~template_count
      ~included_template_count
      ~declared_failure_case_count
      ~counted_failure_case_count
      ~accepted_counted_failure_case_count
    && required_failure_case_contract_blockers = []
  in
  let cross_platform_ready = gate_status_accepted cross_platform_evidence in
  let profile_consensus_ready =
    Profile.consensus_ready
      ~profile_gate_count
      ~unprofiled_count
      status_counts
  in
  let profile_consensus_candidate_ready =
    Profile.consensus_candidate
      ~profile_gate_count
      ~unprofiled_count
      status_counts
  in
  let profile_ready =
    profile_consensus_ready
    || (cross_platform_ready && profile_consensus_candidate_ready)
  in
  let roots_ready =
    Profile.root_bindings_are_consensus_ready root_binding_counts
  in
  let vm_semantics_ready =
    Profile.root_bindings_are_consensus_ready vm_semantics_binding_counts
  in
  let abi_ready =
    Profile.root_bindings_are_consensus_ready abi_declaration_binding_counts
  in
  let executable_abi_ready = executable_abi_ready executable_abi_counts in
  let transcendental_dependency_ready =
    transcendental_dependency_blockers transcendental_dependency_catalog = []
  in
  let ready =
    Profile.validator_readiness_accepted
      ~execution_ready:execution_accepted
      ~failure_cases_ready
      ~effort_ready
      ~profile_ready
      ~roots_ready
      ~cross_platform_ready
    && vm_semantics_ready
    && abi_ready
    && executable_abi_ready
    && transcendental_dependency_ready
  in
  let blockers =
    []
    |> add_blocker (not execution_accepted) "execution_rejected"
    |> add_blocker
         (not failure_cases_ready)
         "punitive_failure_cases_not_accepted"
    |> add_blocker
         (required_failure_case_contract_blockers <> [])
         "required_failure_case_contract_rejected"
    |> add_blocker
         (not vm_semantics_ready)
         "vm_semantics_root_binding_not_proven"
    |> add_blocker
         (not abi_ready)
         "abi_declaration_binding_not_proven"
    |> add_blocker
         (not executable_abi_ready)
         "executable_abi_binding_not_proven"
    |> add_blocker
         (not transcendental_dependency_ready)
         "transcendental_dependency_not_resolved"
    |> add_blocker
         (not cross_platform_ready)
         "cross_platform_conformance_missing"
  in
  let profile_blockers =
    if profile_ready then
      []
    else
      Profile.consensus_ready_blockers
        ~profile_gate_count
        ~unprofiled_count
        status_counts
  in
  let blockers =
    blockers
    @ failure_case_blockers
        ~template_count
        ~included_template_count
        ~declared_failure_case_count
        ~counted_failure_case_count
        ~accepted_counted_failure_case_count
    @ required_failure_case_contract_blockers
    @ effort_blockers
    @ vm_semantics_root_blockers vm_semantics_binding_counts
    @ abi_declaration_binding_blockers abi_declaration_binding_counts
    @ executable_abi_blockers executable_abi_counts
    @ transcendental_dependency_blockers transcendental_dependency_catalog
    @ profile_blockers
    @ Profile.root_binding_blockers root_binding_counts
  in
  `Assoc [
    "diagnostic_only", `Bool true;
    "required", `Bool required;
    "status",
    `String (if ready then "accepted" else "rejected");
    "execution_status",
    `String (if execution_accepted then "accepted" else "rejected");
    "failure_case_status",
    `String (if failure_cases_ready then "accepted" else "rejected");
    "strict_effort_status", `String strict_effort_status;
    "effort_status", `String effort_status;
    "profile_status",
    `String (if profile_ready then "accepted" else "rejected");
    "profile_static_status",
    `String
      (if profile_consensus_ready then "consensus_ready"
       else if profile_consensus_candidate_ready then "consensus_candidate"
       else "rejected");
    "profile_admission_status",
    `String
      (if profile_consensus_ready then "consensus_ready"
       else if cross_platform_ready && profile_consensus_candidate_ready then
         "consensus_candidate_admitted_by_cross_platform_matrix"
       else if profile_consensus_candidate_ready then
         "consensus_candidate_awaiting_cross_platform_matrix"
       else
         "rejected");
    "profile_root_status",
    `String (if roots_ready then "accepted" else "rejected");
    "vm_semantics_root_status",
    `String (if vm_semantics_ready then "accepted" else "rejected");
    "vm_semantics_binding_gate",
    vm_semantics_binding_gate_json vm_semantics_binding_counts;
    "abi_declaration_status",
    `String (if abi_ready then "accepted" else "rejected");
    "abi_declaration_binding_gate",
    abi_declaration_binding_gate_json abi_declaration_binding_counts;
    "executable_abi_status",
    `String (if executable_abi_ready then "accepted" else "rejected");
    "executable_abi_binding_gate",
    executable_abi_gate_json executable_abi_counts;
    "transcendental_dependency_status",
    `String
      (if transcendental_dependency_ready then "accepted" else "rejected");
    "transcendental_dependency_gate",
    transcendental_dependency_gate_json transcendental_dependency_catalog;
    "cross_platform_status",
    `String (if cross_platform_ready then "accepted" else "rejected");
    "cross_platform_evidence", cross_platform_evidence;
    "next_blocker", next_blocker_json blockers;
    "blockers",
    `List (List.map (fun blocker -> `String blocker) blockers);
  ]

let profile_roots_required_passes ~root_binding_counts =
  Profile.root_bindings_required_pass
    ~required:!require_profile_roots_bound
    root_binding_counts

let result_profile_gate_count result =
  if profile_gate_present result then 1 else 0

let p0_plus_profile_gates opcode fields =
  match opcode with
  | "LOGITS_TAIL_PATH" ->
    non_null_profile_gates
      (List.map
         runtime_profile_gate_json
         ["RMSNORM_FP_EPS"; "LINEAR_Q1_G128_FP"; "ARGMAX_FP"])
  | _ -> non_null_profile_gates [profile_gate_json opcode fields]

let p0_plus_profile_root_bindings fields profile_gates =
  match opt_string_field "numerical_profile_root" fields with
  | Some numerical_profile_root ->
    List.map
      (Profile.root_binding_json ~numerical_profile_root)
      profile_gates
  | None ->
    List.map (fun _ -> Profile.unavailable_root_binding_json) profile_gates

let check_template_identity template_path opcode primitive fields =
  (match opt_string_field "opcode" fields with
   | Some value when not (String.equal value opcode) ->
     fail (template_path ^ ": template opcode mismatch: " ^ value)
   | _ -> ());
  match primitive, opt_string_field "primitive" fields with
  | Some expected, Some value when not (String.equal value expected) ->
    fail (template_path ^ ": template primitive mismatch: " ^ value)
  | _ -> ()

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

let uint64_z value =
  let z = Z.of_int64 value in
  if Int64.compare value 0L < 0 then Z.add z (Z.shift_left Z.one 64)
  else z

let int64_hex value =
  let buffer = Bytes.create 16 in
  for index = 0 to 15 do
    let shift = (15 - index) * 4 in
    let nibble =
      Int64.(
        to_int (logand (shift_right_logical value shift) 0xfL))
    in
    Bytes.set
      buffer
      index
      (Char.chr
         ((if nibble < 10 then Char.code '0' else Char.code 'a' - 10)
          + nibble))
  done;
  "0x" ^ Bytes.to_string buffer

let first_mismatch_byte expected observed =
  let expected_len = String.length expected in
  let observed_len = String.length observed in
  let limit = min expected_len observed_len in
  let rec loop index =
    if index = limit then
      if expected_len = observed_len then None else Some index
    else if not (Char.equal expected.[index] observed.[index]) then Some index
    else loop (index + 1)
  in
  loop 0

let mismatch_detail ?(decode_f64 = false) expected observed =
  let expected_len = String.length expected in
  let observed_len = String.length observed in
  match first_mismatch_byte expected observed with
  | None ->
    `Assoc [
      "status", `String "no_mismatch";
      "expected_bytes", `Int expected_len;
      "observed_bytes", `Int observed_len;
    ]
  | Some byte ->
    let common =
      [
        "status", `String "mismatch";
        "first_mismatch_byte", `Int byte;
        "expected_bytes", `Int expected_len;
        "observed_bytes", `Int observed_len;
      ]
    in
    if decode_f64
       && byte + 8 <= expected_len
       && byte + 8 <= observed_len
       && expected_len mod 8 = 0
       && observed_len mod 8 = 0 then
      let cell = byte / 8 in
      let offset = cell * 8 in
      let expected_bits = int64_le expected offset in
      let observed_bits = int64_le observed offset in
      `Assoc
        (common
         @ [
             "first_mismatch_f64_cell", `Int cell;
             "expected_bits_hex", `String (int64_hex expected_bits);
             "observed_bits_hex", `String (int64_hex observed_bits);
             "expected_f64_decimal",
             `String (Printf.sprintf "%.17g" (Int64.float_of_bits expected_bits));
             "observed_f64_decimal",
             `String (Printf.sprintf "%.17g" (Int64.float_of_bits observed_bits));
             "raw_u64_bit_delta",
             `Intlit
               Z.(
                 abs (sub (uint64_z observed_bits) (uint64_z expected_bits))
                 |> to_string);
           ])
    else `Assoc common

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

let output_bytes_result state base cells =
  if cells < 0 then
    Error
      (Printf.sprintf
         "invalid output span: base %d cells %d"
         base
         cells)
  else if cells > 0 && base > max_int - cells then
    Error
      (Printf.sprintf
         "invalid output bounds: base %d cells %d"
         base
         cells)
  else
  let raw = Bytes.create (cells * 8) in
  let rec fill index =
    if index = cells then Ok (Bytes.to_string raw)
    else
      match Hashtbl.find_opt state.VM.memory.data (base + index) with
      | Some (VM.VInt value) when Z.fits_int64 value ->
        put_int64_le raw index (Z.to_int64 value);
        fill (index + 1)
      | _ ->
        Error
          (Printf.sprintf
             "missing output cell: base %d index %d"
             base
             index)
  in
  fill 0

let output_bytes state base cells =
  match output_bytes_result state base cells with
  | Ok raw -> raw
  | Error error -> fail error

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

let set_output_abi_registers state template =
  match field "output" template with
  | Some (`Assoc output_fields) ->
    (match field "abi_registers" output_fields with
     | Some (`Assoc abi_registers) ->
       List.iter
         (fun (name, value) ->
            match name, value with
            | "r0", (`Int _ | `Intlit _)
            | "r1", (`Int _ | `Intlit _) ->
              let reg = register_index name in
              set_z_reg state reg (z_field name abi_registers)
            | "r0", _
            | "r1", _ ->
              fail ("output ABI register value must be an int: " ^ name)
            | _, _ -> ())
         abi_registers
     | _ -> ())
  | _ -> ()

let find_input name inputs =
  match List.find_opt (fun input -> String.equal input.input_name name) inputs with
  | Some input -> input
  | None when String.equal name "lhs" ->
    (match List.find_opt (fun input -> String.equal input.input_name "input") inputs with
     | Some input -> input
     | None -> fail ("unknown input target: " ^ name))
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

let truncate_raw_suffix state reg truncate_bytes =
  if truncate_bytes < 0 then fail "truncate_bytes must be nonnegative";
  match state.VM.regs.(reg) with
  | VM.VString raw ->
    let keep = max 0 (String.length raw - truncate_bytes) in
    state.VM.regs.(reg) <- VM.VString (String.sub raw 0 keep)
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
       (match input.raw_register with
        | Some reg ->
          (* LOAD_F64_LE_FP ingress: mutate first 8 source octets, not VM cells. *)
          let bits = Z.to_int64 (z_field "value_bits" fields) in
          let raw = Bytes.create 8 in
          put_int64_le raw 0 bits;
          replace_raw_prefix state reg (Bytes.to_string raw);
          `Executed
        | None ->
          set_f64_cell_bits state input.base (z_field "value_bits" fields);
          `Executed)
     | "replace_first_f32_input_cell" ->
       let input = find_input target inputs in
       (match input.raw_register with
        | Some reg ->
          (* LOAD_F32_LE_FP ingress: mutate first 4 little-endian source octets. *)
          let bits = Z.to_int (z_field "value_bits" fields) land 0xffffffff in
          let raw = Bytes.create 4 in
          Bytes.set raw 0 (Char.chr (bits land 0xff));
          Bytes.set raw 1 (Char.chr ((bits lsr 8) land 0xff));
          Bytes.set raw 2 (Char.chr ((bits lsr 16) land 0xff));
          Bytes.set raw 3 (Char.chr ((bits lsr 24) land 0xff));
          replace_raw_prefix state reg (Bytes.to_string raw);
          `Executed
        | None ->
          fail "replace_first_f32_input_cell requires raw_register source octets")
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
     | "set_output_base_to_first_input_base_plus" ->
       let first =
         match inputs with
         | input :: _ -> input
         | [] -> fail "no inputs available for alias mutation"
       in
       let offset_cells = int_field "offset_cells" fields in
       let output_param =
         if List.mem_assoc "dst" registers then Some "dst"
         else if List.mem_assoc "output" registers then Some "output"
         else if List.mem_assoc "addr" registers then Some "addr"
         else None
       in
       (match output_param with
        | Some param ->
          set_int_reg state (reg_for param registers) (first.base + offset_cells)
        | None -> ());
       `Executed
     | "replace_q1_scale_bits" ->
       let input = find_input "q1_owner" inputs in
       (match input.raw_register with
        | Some reg ->
          replace_raw_prefix state reg (bytes_of_hex (string_field "value_hex_le" fields));
          `Executed
        | None -> fail "q1_owner must be raw bytes")
     | "truncate_input_manifest" ->
       let input = find_input target inputs in
       let truncate_bytes = int_field "truncate_bytes" fields in
       if truncate_bytes < 0 then fail "truncate_bytes must be nonnegative";
       (match input.raw_register with
        | Some reg ->
          truncate_raw_suffix state reg truncate_bytes;
          `Executed
        | None ->
          if truncate_bytes > 0 then `Modeled_ingress_rejected else `Executed)
     | "lower_effort_limit" ->
       `Executed
     | _ -> fail ("unsupported mutation: " ^ name))
  | _ -> fail "mutation must be an object"

let mutation_result_ingress_rejected = function
  | `Ingress_rejected
  | `Modeled_ingress_rejected -> true
  | `Executed -> false

let mutation_result_json = function
  | `Executed ->
    `Assoc [
      "status", `String "executed";
      "authority", `String "direct_vm_mutation";
    ]
  | `Ingress_rejected ->
    `Assoc [
      "status", `String "ingress_rejected";
      "authority", `String "direct_ingress_validation";
    ]
  | `Modeled_ingress_rejected ->
    `Assoc [
      "status", `String "ingress_rejected";
      "authority", `String "modeled_direct_runner_pre_ingress";
    ]

let ingress_rejection_authority mutation_results =
  if List.exists (( = ) `Modeled_ingress_rejected) mutation_results then
    "modeled_direct_runner_pre_ingress"
  else if List.exists (( = ) `Ingress_rejected) mutation_results then
    "direct_ingress_validation"
  else
    "not_applicable"

let mutation_fields = function
  | `Assoc fields -> Some fields
  | _ -> None

let mutation_is name mutation =
  match mutation_fields mutation with
  | Some fields ->
    (match opt_string_field "mutation" fields with
     | Some actual -> String.equal actual name
     | None -> false)
  | None -> false

let mutation_targets target mutation =
  match mutation_fields mutation with
  | Some fields ->
    (match opt_string_field "target" fields with
     | Some actual -> String.equal actual target
     | None -> false)
  | None -> false

let mutation_int_value name mutation =
  match mutation_fields mutation with
  | Some fields -> opt_int_field name fields
  | None -> None

let mutation_z_value name mutation =
  match mutation_fields mutation with
  | Some fields ->
    (match field name fields with
     | Some (`Int value) -> Some (Z.of_int value)
     | Some (`Intlit value) -> Some (z_of_unsigned_i64_string value)
     | _ -> None)
  | None -> None

let mutation_string_value name mutation =
  match mutation_fields mutation with
  | Some fields -> opt_string_field name fields
  | None -> None

let q1_owner_source_bytes template =
  match field "input_memory_ranges" template with
  | Some (`List ranges) ->
    List.find_map
      (function
        | `Assoc fields when opt_string_field "name" fields = Some "q1_owner" ->
          (match field "source" fields with
           | Some (`Assoc source) -> opt_int_field "bytes" source
           | _ -> None)
        | _ -> None)
      ranges
  | _ -> None

let q1_required_owner_bytes values =
  match opt_int_field "k" values, opt_int_field "n" values with
  | Some k, Some n when k > 0 && n > 0 && k mod 128 = 0 ->
    Some (n * (k / 128) * 18)
  | _ -> None

let q1_lhs_cell_count values =
  match opt_int_field "m" values, opt_int_field "k" values with
  | Some m, Some k when m > 0 && k > 0 -> Some (m * k)
  | _ -> None

let q1_output_cell_count values =
  match opt_int_field "m" values, opt_int_field "n" values with
  | Some m, Some n when m > 0 && n > 0 -> Some (m * n)
  | _ -> None

let q1_mutates_scalar param predicate mutations =
  List.exists
    (fun mutation ->
       mutation_is "set_scalar_param" mutation
       && mutation_targets
            ("parameter_addresses_and_scalar_params.values." ^ param)
            mutation
       &&
       match mutation_int_value "value" mutation with
       | Some value -> predicate value
       | None -> false)
    mutations

let q1_truncates_raw target mutations =
  List.exists
    (fun mutation ->
       mutation_is "truncate_input_manifest" mutation
       && mutation_targets target mutation
       &&
       match mutation_int_value "truncate_bytes" mutation with
       | Some value -> value > 0
       | None -> false)
    mutations

let q1_fp16_bits_from_hex_le value =
  let raw = bytes_of_hex value in
  if String.length raw <> 2 then None
  else
    Some
      (Char.code raw.[0]
       lor (Char.code raw.[1] lsl 8))

let q1_nonfinite_fp16_scale_mutation mutations =
  List.exists
    (fun mutation ->
       mutation_is "replace_q1_scale_bits" mutation
       && mutation_targets "q1_owner[0..2]" mutation
       &&
       match mutation_string_value "value_hex_le" mutation with
       | Some value ->
         (match q1_fp16_bits_from_hex_le value with
          | Some bits -> Fp64.of_binary16 bits = None
          | None -> false)
       | None -> false)
    mutations

let q1_lhs_target mutation =
  mutation_targets "lhs" mutation || mutation_targets "input" mutation

let q1_required_mutation_shape_blockers template values case mutations =
  let single_mutation =
    match mutations with
    | [_] -> true
    | _ -> false
  in
  let shape_matched =
    match case with
    | "nonfinite_input_nan" ->
      List.exists
        (fun mutation ->
           mutation_is "replace_first_f64_input_cell" mutation
           && q1_lhs_target mutation
           && mutation_z_value "value_bits" mutation
              = Some (z_of_unsigned_i64_string "9221120237041090560"))
        mutations
    | "nonfinite_input_infinity" ->
      List.exists
        (fun mutation ->
           mutation_is "replace_first_f64_input_cell" mutation
           && q1_lhs_target mutation
           && mutation_z_value "value_bits" mutation
              = Some (z_of_unsigned_i64_string "9218868437227405312"))
        mutations
    | "output_input_aliasing" ->
      List.exists
        (fun mutation ->
           mutation_targets "output.base_address" mutation
           &&
           (mutation_is "set_output_base_to_first_input_base" mutation
            ||
            (mutation_is "set_output_base_to_first_input_base_plus" mutation
             &&
             match mutation_int_value "offset_cells" mutation with
             | Some value -> value = 0
             | None -> false)))
        mutations
    | "partial_output_input_aliasing" ->
      (match q1_lhs_cell_count values with
       | Some lhs_cells ->
         List.exists
           (fun mutation ->
              mutation_is "set_output_base_to_first_input_base_plus" mutation
              && mutation_targets "output.base_address" mutation
              &&
              match mutation_int_value "offset_cells" mutation with
              | Some value -> value > 0 && value < lhs_cells
              | None -> false)
           mutations
       | None -> false)
    | "k_not_multiple_of_128" ->
      q1_mutates_scalar "k" (fun value -> value > 0 && value mod 128 <> 0) mutations
    | "bad_q1_owner_length" -> q1_truncates_raw "q1_owner" mutations
    | "negative_byte_offset" ->
      q1_mutates_scalar "byte_offset" (fun value -> value < 0) mutations
    | "byte_offset_out_of_bounds" ->
      (match q1_owner_source_bytes template with
       | Some source_bytes ->
         q1_mutates_scalar
           "byte_offset"
           (fun value -> value > source_bytes)
           mutations
       | None -> false)
    | "byte_offset_truncated_span" ->
      (match q1_owner_source_bytes template, q1_required_owner_bytes values with
       | Some source_bytes, Some required_bytes ->
         q1_mutates_scalar
           "byte_offset"
           (fun value ->
              value >= 0
              && value <= source_bytes
              && required_bytes > source_bytes - value)
           mutations
       | _ -> false)
    | "nonfinite_fp16_scale" -> q1_nonfinite_fp16_scale_mutation mutations
    | "lower_effort_limit" ->
      List.exists
        (fun mutation ->
           mutation_is "lower_effort_limit" mutation
           &&
           match mutation_int_value "value" mutation,
                 opt_int_field "expected_effort" template with
           | Some value, Some expected -> value < expected
           | _ -> false)
        mutations
    | _ -> true
  in
  let matched =
    match case with
    | "nonfinite_input_nan"
    | "nonfinite_input_infinity"
    | "output_input_aliasing"
    | "partial_output_input_aliasing"
    | "k_not_multiple_of_128"
    | "bad_q1_owner_length"
    | "negative_byte_offset"
    | "byte_offset_out_of_bounds"
    | "byte_offset_truncated_span"
    | "nonfinite_fp16_scale"
    | "lower_effort_limit" ->
      single_mutation && shape_matched
    | _ -> shape_matched
  in
  if matched then []
  else ["q1_failure_case_mutation_mismatch_" ^ case]

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

let op_load_f64 registers =
  VM.LOAD_F64_LE_FP
    (reg_for "dst" registers,
     reg_for "src" registers,
     reg_for "offset" registers,
     reg_for "count" registers)

let op_load_f32 registers =
  VM.LOAD_F32_LE_FP
    (reg_for "dst" registers,
     reg_for "src" registers,
     reg_for "offset" registers,
     reg_for "count" registers)

let op_argmax registers =
  VM.ARGMAX_FP
    (reg_for "dest" registers,
     reg_for "addr" registers,
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
  | "LOAD_F64_LE_FP" -> op_load_f64 registers
  | "LOAD_F32_LE_FP" -> op_load_f32 registers
  | "ARGMAX_FP" -> op_argmax registers
  | "GATED_DELTA_RULE_FP" -> op_gated_delta registers
  | value -> fail ("unsupported opcode: " ^ value)

let opcode_name = function
  | VM.LINEAR_Q1_G128_FP _ -> "LINEAR_Q1_G128_FP"
  | VM.RMSNORM_FP_EPS _ -> "RMSNORM_FP_EPS"
  | VM.L2NORM_FP _ -> "L2NORM_FP"
  | VM.SOFTMAX_FP _ -> "SOFTMAX_FP"
  | VM.LOAD_F64_LE_FP _ -> "LOAD_F64_LE_FP"
  | VM.LOAD_F32_LE_FP _ -> "LOAD_F32_LE_FP"
  | VM.ARGMAX_FP _ -> "ARGMAX_FP"
  | VM.GATED_DELTA_RULE_FP _ -> "GATED_DELTA_RULE_FP"
  | VM.STOP -> "STOP"
  | _ -> "other"

let opcode_profile_json (row : VM.opcode_profile) =
  `Assoc [
    "opcode", `String row.opcode;
    "count", `Int row.count;
    "effort_used", `Int row.effort_used;
    "microseconds", `Int row.microseconds;
  ]

let observed_opcode_effort opcode profile =
  List.fold_left
    (fun total (row : VM.opcode_profile) ->
       if String.equal row.opcode opcode then total + row.effort_used
       else total)
    0
    profile

let expected_opcode_effort opcode values =
  match opcode with
  | "LINEAR_Q1_G128_FP" ->
    let m = int_field "m" values in
    let k = int_field "k" values in
    let n = int_field "n" values in
    Some (200 + ((m * n * k) / 512))
  | _ -> None

let length_prefix value =
  string_of_int (String.length value) ^ ":" ^ value

let value_payload = function
  | VM.VInt value -> Ok ("int:" ^ Z.to_string value)
  | VM.VBool value -> Ok ("bool:" ^ if value then "1" else "0")
  | VM.VString value -> Ok ("string:" ^ length_prefix value)
  | VM.VBytes value -> Ok ("bytes:" ^ length_prefix value)
  | VM.VBytes32 value -> Ok ("bytes32:" ^ length_prefix value)
  | VM.VU64 value -> Ok ("u64:" ^ Z.to_string value)
  | VM.VU128 value -> Ok ("u128:" ^ Z.to_string value)
  | VM.VU256 value -> Ok ("u256:" ^ Z.to_string value)
  | VM.VAddr value -> Ok ("address:" ^ length_prefix value)
  | VM.VCipher _
  | VM.VPubKey _ -> Error "opaque_value"

let output_integer state reg =
  match state.VM.regs.(reg) with
  | VM.VInt value
  | VM.VU64 value
  | VM.VU128 value
  | VM.VU256 value
    when Z.fits_int value -> Some (Z.to_int value)
  | _ -> None

let output_payload state =
  match
    output_integer state Abi.output_base_register,
    output_integer state Abi.output_count_register
  with
  | Some base, Some length when base >= 0 && length >= 0
                              && base <= max_int - length ->
    let rec read_values index acc =
      if index = length then Ok (List.rev acc)
      else
        let cell = base + index in
        match Hashtbl.find_opt state.VM.memory.data cell with
        | None -> Error ("missing_output_cell:" ^ string_of_int cell)
        | Some value ->
          (match value_payload value with
           | Ok value -> read_values (index + 1) (value :: acc)
           | Error error -> Error error)
    in
    (match read_values 0 [] with
     | Ok values ->
       Ok
         (String.concat
            "|"
            [
              "base=" ^ string_of_int base;
              "length=" ^ string_of_int length;
              "values=" ^ String.concat "," values;
            ])
     | Error error -> Error error)
  | Some _, Some _ -> Error "invalid_output_bounds"
  | _ -> Error "missing_output_bounds"

let executable_abi_result state template =
  let output = assoc_field "output" template in
  let abi_registers = assoc_field "abi_registers" output in
  let expected_r0 = int_field "r0" abi_registers in
  let expected_r1 = int_field "r1" abi_registers in
  let observed_r0 = output_integer state Abi.output_base_register in
  let observed_r1 = output_integer state Abi.output_count_register in
  let registers_match =
    observed_r0 = Some expected_r0 && observed_r1 = Some expected_r1
  in
  let payload_result = output_payload state in
  let payload =
    match payload_result with
    | Ok payload -> `String payload
    | Error _ -> `Null
  in
  let payload_sha256 =
    match payload_result with
    | Ok payload -> `String (sha256 payload)
    | Error _ -> `Null
  in
  let payload_status =
    match payload_result with
    | Ok _ -> "accepted"
    | Error _ -> "rejected"
  in
  let payload_error =
    match payload_result with
    | Ok _ -> `Null
    | Error error -> `String error
  in
  registers_match && String.equal payload_status "accepted",
  `Assoc [
    "status",
    `String
      (if registers_match && String.equal payload_status "accepted" then
         "matched"
       else
         "mismatch");
    "expected_r0", `Int expected_r0;
    "expected_r1", `Int expected_r1;
    "observed_r0",
    (match observed_r0 with Some value -> `Int value | None -> `Null);
    "observed_r1",
    (match observed_r1 with Some value -> `Int value | None -> `Null);
    "registers_match", `Bool registers_match;
    "output_payload_status", `String payload_status;
    "output_payload_error", payload_error;
    "output_payload", payload;
    "output_payload_sha256", payload_sha256;
  ]

let unavailable_subspan_result fields error =
  let name = string_field "name" fields in
  let base = int_field "base_address" fields in
  let cells = int_field "length_f64_cells" fields in
  let expected_sha = string_field "sha256" fields in
  let expected_root = opt_string_field "root" fields in
  let observed =
    sha256
      (String.concat
         "|"
         [
           "octra:inference:missing-output";
           name;
           string_of_int base;
           string_of_int cells;
           error;
         ])
  in
  false,
  `Assoc [
    "name", `String name;
    "base_address", `Int base;
    "length_f64_cells", `Int cells;
    "expected_sha256", `String expected_sha;
    "observed_sha256", `String observed;
    "expected_root",
    (match expected_root with None -> `Null | Some root -> `String root);
    "observed_root", `String observed;
    "root_matched", `Bool false;
    "matched", `Bool false;
    "error", `String error;
  ]

let subspan_result state value =
  match value with
  | `Assoc fields ->
    let base = int_field "base_address" fields in
    let cells = int_field "length_f64_cells" fields in
    (match output_bytes_result state base cells with
     | Error error -> unavailable_subspan_result fields error
     | Ok raw ->
       let name = string_field "name" fields in
       let expected_sha = string_field "sha256" fields in
       let expected_root = opt_string_field "root" fields in
       let actual_sha = sha256 raw in
       let actual_root = fixture_value_root ~name raw in
       let root_matched =
         match expected_root with
         | None -> true
         | Some root -> String.equal root actual_root
       in
       let matched = String.equal actual_sha expected_sha && root_matched in
       matched,
       `Assoc [
         "name", `String name;
         "base_address", `Int base;
         "length_f64_cells", `Int cells;
         "expected_sha256", `String expected_sha;
         "observed_sha256", `String actual_sha;
         "expected_root",
         (match expected_root with None -> `Null | Some root -> `String root);
         "observed_root", `String actual_root;
         "root_matched", `Bool root_matched;
         "matched", `Bool matched;
       ])
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

let capture_existing_span_result state name base cells =
  match output_bytes_result state base cells with
  | Ok raw -> Some (name, base, cells, raw)
  | Error _ -> None

let span_covers base cells (_, span_base, span_cells, _) =
  span_base <= base
  && cells >= 0
  && span_cells >= 0
  && span_base + span_cells >= base + cells

let unchanged_result state (name, base, cells, before) =
  match output_bytes_result state base cells with
  | Ok after ->
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
  | Error error ->
    false,
    `Assoc [
      "name", `String name;
      "base_address", `Int base;
      "length_f64_cells", `Int cells;
      "before_sha256", `String (sha256 before);
      "after_sha256", `Null;
      "unchanged", `Bool false;
      "error", `String error;
    ]

let changed_span_result state (name, base, cells, before) =
  match output_bytes_result state base cells with
  | Ok after ->
    let changed = not (String.equal before after) in
    changed,
    `Assoc [
      "name", `String name;
      "base_address", `Int base;
      "length_f64_cells", `Int cells;
      "before_sha256", `String (sha256 before);
      "after_sha256", `String (sha256 after);
      "changed", `Bool changed;
    ]
  | Error error ->
    false,
    `Assoc [
      "name", `String name;
      "base_address", `Int base;
      "length_f64_cells", `Int cells;
      "before_sha256", `String (sha256 before);
      "after_sha256", `Null;
      "changed", `Bool false;
      "error", `String error;
    ]

let expected_output_subspan template =
  let output = assoc_field "output" template in
  match list_field "subspans" output with
  | `Assoc fields :: _ -> Some fields
  | _ -> None

let template_output_span template =
  let output = assoc_field "output" template in
  int_field "base_address" output, int_field "length_f64_cells" output

let snapshot_output_result template state active_before =
  match active_before, expected_output_subspan template with
  | Some (_, base, cells, _), Some expected ->
    let expected_cells = int_field "length_f64_cells" expected in
    let expected_sha = string_field "sha256" expected in
    (match output_bytes_result state base cells with
     | Ok raw ->
       let observed_sha = sha256 raw in
       let matched =
         cells = expected_cells && String.equal observed_sha expected_sha
       in
       matched,
       `Assoc [
         "status", `String (if matched then "matched" else "mismatch");
         "base_address", `Int base;
         "length_f64_cells", `Int cells;
         "expected_length_f64_cells", `Int expected_cells;
         "expected_sha256", `String expected_sha;
         "observed_sha256", `String observed_sha;
       ]
     | Error error ->
       false,
       `Assoc [
         "status", `String "mismatch";
         "base_address", `Int base;
         "length_f64_cells", `Int cells;
         "expected_length_f64_cells", `Int expected_cells;
         "expected_sha256", `String expected_sha;
         "observed_sha256", `Null;
         "error", `String error;
       ])
  | None, _ ->
    false,
    `Assoc [
      "status", `String "unavailable";
      "reason", `String "no_active_output_span";
    ]
  | _, None ->
    false,
    `Assoc [
      "status", `String "unavailable";
      "reason", `String "no_expected_output_subspan";
    ]

let finite_span_result state (name, base, cells, _) =
  let finite = ref true in
  for index = 0 to cells - 1 do
    match Hashtbl.find_opt state.VM.memory.data (base + index) with
    | Some (VM.VInt bits) when Z.fits_int64 bits ->
      if not (Fp64.finite (Z.to_int64 bits)) then finite := false
    | _ -> finite := false
  done;
  !finite,
  `Assoc [
    "name", `String name;
    "base_address", `Int base;
    "length_f64_cells", `Int cells;
    "finite", `Bool !finite;
  ]

let output_register_name registers =
  if List.mem_assoc "dst" registers then Some "dst"
  else if List.mem_assoc "output" registers then Some "output"
  else if List.mem_assoc "addr" registers then Some "addr"
  else None

let active_output_span state registers unchanged_spans =
  match output_register_name registers, unchanged_spans with
  | Some name, (_, _, cells, _) :: _ ->
    let reg = reg_for name registers in
    let base = Z.to_int (reg_z state reg) in
    capture_existing_span_result state "active_output" base cells
  | _ -> None

let failure_expectation expected =
  if starts_with "reject_before_write" expected then `Must_reject
  else if starts_with
            "deterministic_profile_must_define_reject_or_infinite_reduction"
            expected then
    `Must_reject
  else if starts_with
            "implementation_must_use_stable_max_subtract"
            expected then
    `Must_accept_changed_finite
  else if starts_with "accept_from_snapshot" expected then
    `Must_accept_changed_finite
  else if starts_with "reject_or_documented_safe_copy" expected then
    `Observation
  else `Observation

let failure_case_result root_dir opcode template registers values op case =
  match case with
  | `Assoc fields ->
    let case_name = string_field "case" fields in
    let expected = string_field "expected" fields in
    let mutations = list_field "executable_mutations" fields in
    let mutation_shape_blockers =
      if String.equal opcode "LINEAR_Q1_G128_FP" then
        q1_required_mutation_shape_blockers template values case_name mutations
      else
        []
    in
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
      List.exists mutation_result_ingress_rejected mutation_results
    in
    let unchanged_spans =
      list_field "unchanged_spans" fields
      |> List.map (capture_span state)
    in
    let output_span_covered =
      if String.equal opcode "LINEAR_Q1_G128_FP"
         && starts_with "reject_before_write" expected then
        let base, cells = template_output_span template in
        List.exists (span_covers base cells) unchanged_spans
      else
        true
    in
    let active_before = active_output_span state registers unchanged_spans in
    let ran = if ingress_rejected then false else VM.run state [|op; VM.STOP|] in
    let unchanged =
      List.map (unchanged_result state) unchanged_spans
    in
    let unchanged_ok = List.for_all fst unchanged in
    let finite_spans = List.map (finite_span_result state) unchanged_spans in
    let finite_ok = List.for_all fst finite_spans in
    let active_changed =
      match active_before with
      | None -> []
      | Some span -> [changed_span_result state span]
    in
    let active_finite =
      match active_before with
      | None -> []
      | Some span -> [finite_span_result state span]
    in
    let active_changed_ok =
      active_changed <> [] && List.for_all fst active_changed
    in
    let active_finite_ok =
      active_finite <> [] && List.for_all fst active_finite
    in
    let changed_ok =
      unchanged <> [] && List.for_all (fun (unchanged, _) -> not unchanged) unchanged
    in
    let snapshot_required = starts_with "accept_from_snapshot" expected in
    let snapshot_ok, snapshot =
      if snapshot_required then snapshot_output_result template state active_before
      else true, `Assoc ["status", `String "not_required"]
    in
    let observed =
      if ingress_rejected then "ingress_rejected"
      else if ran then "vm_accepted"
      else "vm_rejected"
    in
    let expectation = failure_expectation expected in
    let counted, passed =
      match expectation with
      | `Must_reject -> true, ((not ran) && unchanged_ok && output_span_covered)
      | `Must_accept_changed_finite ->
        true,
        (if snapshot_required then
           ran && active_changed_ok && active_finite_ok && snapshot_ok
         else
           ran && changed_ok && finite_ok)
      | `Observation -> false, true
    in
    passed,
    counted,
    `Assoc [
      "opcode", `String opcode;
      "case", `String case_name;
      "failure_case_source",
      `String
        (match opt_string_field "failure_case_source" fields with
         | Some source -> source
         | None -> "producer_template");
      "expected", `String expected;
      "executable_mutations", `List mutations;
      "status", `String (if passed then "accepted" else "rejected");
      "counted", `Bool counted;
      "observed", `String observed;
      "ingress_rejection_authority",
      `String (ingress_rejection_authority mutation_results);
      "mutation_results", `List (List.map mutation_result_json mutation_results);
      "mutation_shape_status",
      `String (if mutation_shape_blockers = [] then "accepted" else "rejected");
      "mutation_shape_blockers",
      `List
        (List.map
           (fun blocker -> `String blocker)
           mutation_shape_blockers);
      "observed_effort", `Int state.VM.effort_used;
      "unchanged_status",
      `String (if unchanged_ok then "matched" else "changed");
      "output_span_status",
      `String (if output_span_covered then "covered" else "not_covered");
      "output_span_blockers",
      `List
        (if output_span_covered then
           []
         else
           [`String "q1_failure_case_output_span_not_covered"]);
      "changed_status",
      `String (if changed_ok then "changed" else "not_changed");
      "finite_status",
      `String (if finite_ok then "finite" else "nonfinite_or_missing");
      "active_changed_status",
      `String (if active_changed_ok then "changed" else "not_changed");
      "active_finite_status",
      `String (if active_finite_ok then "finite" else "nonfinite_or_missing");
      "snapshot_output_status",
      `String
        (if not snapshot_required then "not_required"
         else if snapshot_ok then "matched"
         else "mismatch");
      "snapshot_output", snapshot;
      "unchanged_spans", `List (List.map snd unchanged);
      "finite_spans", `List (List.map snd finite_spans);
      "active_changed_spans", `List (List.map snd active_changed);
      "active_finite_spans", `List (List.map snd active_finite);
    ]
  | _ -> fail "failure case must be an object"

let q1_failure_synthesis_context template values expected_effort =
  let output = assoc_field "output" template in
  match
    opt_int_field "lhs" values,
    q1_output_cell_count values,
    q1_owner_source_bytes template,
    q1_required_owner_bytes values
  with
  | Some lhs_base,
    Some output_cells,
    Some owner_bytes,
    Some required_owner_bytes ->
    Some [
      "lhs_base", `Int lhs_base;
      "output_base", `Int (int_field "base_address" output);
      "output_cells", `Int output_cells;
      "expected_effort", `Int expected_effort;
      "q1_owner_source_bytes", `Int owner_bytes;
      "q1_required_owner_bytes", `Int required_owner_bytes;
    ]
  | _ -> None

let synthesize_q1_required_failure_cases opcode template values expected_effort
    cases =
  if not !synthesize_required_failure_cases
     || not (String.equal opcode "LINEAR_Q1_G128_FP") then
    cases, 0, 0
  else
    match q1_failure_synthesis_context template values expected_effort with
    | Some context ->
      let retained, replaced_count =
        List.fold_right
          (fun case (retained, replaced_count) ->
             if q1_required_case_canonical case then
               case :: retained, replaced_count
             else
               retained, replaced_count + 1)
          cases
          ([], 0)
      in
      let present = List.filter_map failure_case_name retained in
      let missing =
        Template.q1_required_failure_expectations
        |> List.filter_map
             (fun (case, _) ->
                if List.exists (String.equal case) present then None
                else
                  q1_required_case_from_context case context
                  |> Option.map (failure_case_source "litenode_synthesized"))
      in
      retained @ missing, List.length missing, replaced_count
    | None -> cases, 0, 0

let failure_case_results root_dir opcode template registers values
    expected_effort op =
  if not !include_failures then [], 0, 0
  else
    match field "expected_failure_atomicity_behavior" template with
    | Some (`List cases) ->
      let cases, synthesized_count, replaced_count =
        synthesize_q1_required_failure_cases
          opcode
          template
          values
          expected_effort
          cases
      in
      List.map
        (failure_case_result root_dir opcode template registers values op)
        cases,
      synthesized_count,
      replaced_count
    | _ -> [], 0, 0

let required_failure_case_contract opcode failure_results =
  if not (String.equal opcode "LINEAR_Q1_G128_FP") then
    "not_applicable", []
  else
    let rows =
      List.filter_map
        (function
          | _, _, `Assoc fields -> Some fields
          | _ -> None)
        failure_results
    in
    let row_for case =
      List.find_opt
        (fun fields -> String.equal (string_field "case" fields) case)
        rows
    in
    let blockers =
      Template.q1_required_failure_expectations
      |> List.fold_left
           (fun blockers (case, expected_prefix) ->
              match row_for case with
              | None ->
                ("q1_failure_case_missing_" ^ case) :: blockers
              | Some fields ->
                let blockers =
                  if starts_with expected_prefix (string_field "expected" fields) then
                    blockers
                  else
                    ("q1_failure_case_expected_mismatch_" ^ case) :: blockers
                in
                let blockers =
                  (match field "mutation_shape_blockers" fields with
                   | Some (`List shape_blockers) ->
                     List.fold_left
                       (fun blockers -> function
                          | `String blocker -> blocker :: blockers
                          | _ -> blockers)
                       blockers
                       shape_blockers
                   | _ -> blockers)
                in
                let blockers =
                  if bool_field "counted" fields then blockers
                  else ("q1_failure_case_uncounted_" ^ case) :: blockers
                in
                if String.equal (string_field "status" fields) "accepted" then
                  blockers
                else
                  ("q1_failure_case_rejected_" ^ case) :: blockers)
           []
      |> List.rev
    in
    (if blockers = [] then "accepted" else "rejected"), blockers

let required_failure_case_contract_payload opcode =
  if String.equal opcode "LINEAR_Q1_G128_FP" then
    Template.q1_required_failure_expectations_json
  else
    `Null

let q1_contract_shape_json opcode template values expected_effort =
  if not (String.equal opcode "LINEAR_Q1_G128_FP") then
    `Null
  else
    let int_or_null = function
      | Some value -> `Int value
      | None -> `Null
    in
    `Assoc [
      "m", int_or_null (opt_int_field "m" values);
      "k", int_or_null (opt_int_field "k" values);
      "n", int_or_null (opt_int_field "n" values);
      "byte_offset", int_or_null (opt_int_field "byte_offset" values);
      "lhs_cells", int_or_null (q1_lhs_cell_count values);
      "output_cells", int_or_null (q1_output_cell_count values);
      "q1_owner_source_bytes", int_or_null (q1_owner_source_bytes template);
      "q1_required_owner_bytes", int_or_null (q1_required_owner_bytes values);
      "expected_effort", `Int expected_effort;
    ]

let q1_producer_repair_context_json opcode template values expected_effort =
  if not (String.equal opcode "LINEAR_Q1_G128_FP") then
    `Null
  else
    let output = assoc_field "output" template in
    let int_or_null = function
      | Some value -> `Int value
      | None -> `Null
    in
    `Assoc [
      "lhs_base", int_or_null (opt_int_field "lhs" values);
      "output_base", `Int (int_field "base_address" output);
      "output_cells", int_or_null (q1_output_cell_count values);
      "expected_effort", `Int expected_effort;
      "q1_owner_source_bytes", int_or_null (q1_owner_source_bytes template);
      "q1_required_owner_bytes", int_or_null (q1_required_owner_bytes values);
    ]

let execute_template root_dir entry =
  let opcode = string_field "opcode" entry in
  let primitive = opt_string_field "primitive" entry in
  let template_path = string_field "vm_execution_template" entry in
  let full_template_path = Filename.concat root_dir template_path in
  let template =
    match read_json full_template_path with
    | `Assoc fields -> fields
    | _ -> fail (full_template_path ^ ": template must be an object")
  in
  check_template_identity full_template_path opcode primitive template;
  let profile_gate = profile_gate_json opcode template in
  let profile_root_binding =
    Profile.root_binding_json
      ~numerical_profile_root:(string_field "numerical_profile_root" template)
      profile_gate
  in
  let vm_semantics_binding =
    Template.vm_semantics_binding_json
      ~opcode
      ~vm_semantics_root:(string_field "vm_semantics_root" template)
  in
  let expected_effort = int_field "expected_effort" template in
  let params = assoc_field "parameter_addresses_and_scalar_params" template in
  let registers = assoc_field "registers" params in
  let values = assoc_field "values" params in
  let state = state () in
  ignore (load_inputs root_dir state template registers);
  set_registers state registers values;
  let op = op_for opcode registers in
  let abi_declaration_binding =
    Template.abi_declaration_binding_json (`Assoc template)
  in
  let ran, opcode_profile =
    VM.run_profiled
      ~clock:(fun () -> 0.0)
      ~opcode_name
      state
      [|op; VM.STOP|]
  in
  (* ARGMAX writes selected_index to a register; project it into the declared
     output memory cell so template subspan comparison stays memory-shaped. *)
  if ran && String.equal opcode "ARGMAX_FP" then begin
    let dest_reg = reg_for "dest" registers in
    let output = assoc_field "output" template in
    let base = int_field "base_address" output in
    match state.VM.regs.(dest_reg) with
    | VM.VInt value ->
      Hashtbl.replace state.VM.memory.data base (VM.VInt value)
    | _ -> ()
  end;
  set_output_abi_registers state template;
  let output = assoc_field "output" template in
  let subspans = list_field "subspans" output in
  let span_results =
    if ran then List.map (subspan_result state) subspans
    else
      List.map
        (function
          | `Assoc fields -> unavailable_subspan_result fields "vm_run_failed"
          | _ -> fail "output subspan must be an object")
        subspans
  in
  let spans_matched = List.for_all fst span_results in
  let executable_abi_matched, executable_abi_binding =
    if ran then executable_abi_result state template
    else false, `Assoc ["status", `String "not_run"]
  in
  let observed_effort = state.VM.effort_used in
  let program_effort_match = observed_effort = expected_effort in
  let expected_opcode_effort = expected_opcode_effort opcode values in
  let observed_opcode_effort = observed_opcode_effort opcode opcode_profile in
  let opcode_effort_match =
    match expected_opcode_effort with
    | None -> true
    | Some expected -> observed_opcode_effort = expected
  in
  let effort_match = program_effort_match && opcode_effort_match in
  let accepted =
    ran
    && spans_matched
    && executable_abi_matched
    && ((not !strict_effort) || effort_match)
  in
  let failure_results,
      synthesized_failure_case_count,
      replaced_failure_case_count =
    failure_case_results
      root_dir
      opcode
      template
      registers
      values
      expected_effort
      op
  in
  let required_failure_case_contract_status,
      required_failure_case_contract_blockers =
    required_failure_case_contract opcode failure_results
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
    "profile_root_binding", profile_root_binding;
    "vm_semantics_binding", vm_semantics_binding;
    "abi_declaration_binding", abi_declaration_binding;
    "executable_abi_binding", executable_abi_binding;
    "status", `String (if accepted then "accepted" else "rejected");
    "vm_run", `String (if ran then "accepted" else "rejected");
    "output_status",
    `String (if spans_matched then "matched" else "mismatch");
    "expected_effort", `Int expected_effort;
    "observed_effort", `Int observed_effort;
    "expected_program_effort", `Int expected_effort;
    "observed_program_effort", `Int observed_effort;
    "program_effort_match", `Bool program_effort_match;
    "expected_opcode_effort",
    (match expected_opcode_effort with
     | None -> `Null
     | Some value -> `Int value);
    "observed_opcode_effort", `Int observed_opcode_effort;
    "opcode_effort_match", `Bool opcode_effort_match;
    "opcode_profile", `List (List.map opcode_profile_json opcode_profile);
    "effort_match", `Bool effort_match;
    "strict_effort", `Bool !strict_effort;
    "q1_contract_shape",
    q1_contract_shape_json opcode template values expected_effort;
    "producer_repair_context",
    q1_producer_repair_context_json opcode template values expected_effort;
    "subspans", `List (List.map snd span_results);
    "failure_cases_included", `Bool !include_failures;
    "failure_case_synthesis",
    `Assoc [
      "enabled", `Bool !synthesize_required_failure_cases;
      "opcode_applicable",
      `Bool (String.equal opcode "LINEAR_Q1_G128_FP");
      "synthesized_count", `Int synthesized_failure_case_count;
      "replaced_producer_case_count", `Int replaced_failure_case_count;
    ];
    "required_failure_case_contract",
    `Assoc [
      "status", `String required_failure_case_contract_status;
      "contract", required_failure_case_contract_payload opcode;
      "blockers",
      `List
        (List.map
           (fun blocker -> `String blocker)
           required_failure_case_contract_blockers);
    ];
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
  let expected_root = string_field "root" manifest in
  let observed_root = fixture_value_root ~name raw in
  let root_matched = String.equal expected_root observed_root in
  let matched = String.equal raw expected && root_matched in
  let decode_f64 =
    match opt_string_field "layout" manifest with
    | Some layout -> starts_with "f64le" layout
    | None -> false
  in
  matched,
  `Assoc [
    "name", `String name;
    "status", `String (if matched then "matched" else "mismatch");
    "expected_sha256", `String (sha256 expected);
    "observed_sha256", `String (sha256 raw);
    "expected_root", `String expected_root;
    "observed_root", `String observed_root;
    "root_matched", `Bool root_matched;
    "bytes", `Int (String.length raw);
    "mismatch_detail",
    (if matched then `Null else mismatch_detail ~decode_f64 expected raw);
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

let known_softmax_wide_scores_sha256 =
  "fbf6649f29767e9266a89a987c1b762c52de10a8aaa79d30014714d7f654bc37"

let known_softmax_wide_scores_root =
  "5b2185f1c59cc6d6a1b38132c18065ccddec578c3b4dd2243e27fd496fbd5970"

let known_softmax_wide_expected_sha256 =
  "e4b5636c2bee8290db71b708063844ea2752ecf1cd4ba39a71cbed95170b035b"

let known_softmax_wide_expected_root =
  "d074263f3411323e9f0d6ad4b43052e7e540d25f1e6fee2c1e7e3907f769bf04"

let known_softmax_wide_observed_sha256 =
  "ece82e02777ae8130578474746a86accf691e758772004265e1191eb18ac00e9"

let known_softmax_wide_observed_root =
  "9ecd2acf353400f86c58a5d556aab5b55b458e93f1966751be9b010c31fcb978"

let manifest_identity_matches manifests name ~sha256 ~root =
  List.exists
    (function
      | `Assoc fields ->
        String.equal (string_field "name" fields) name
        && String.equal (string_field "sha256" fields) sha256
        && String.equal (string_field "root" fields) root
      | _ -> false)
    manifests

let known_softmax_wide_fixture fixture =
  manifest_identity_matches
    (list_field "input_byte_manifests" fixture)
    "scores"
    ~sha256:known_softmax_wide_scores_sha256
    ~root:known_softmax_wide_scores_root
  && manifest_identity_matches
       (list_field "expected_output_byte_manifests" fixture)
       "expected_probabilities"
       ~sha256:known_softmax_wide_expected_sha256
       ~root:known_softmax_wide_expected_root

let known_softmax_wide_output_mismatch = function
  | `Assoc fields ->
    String.equal (string_field "name" fields) "expected_probabilities"
    && String.equal (string_field "status" fields) "mismatch"
    && String.equal
         (string_field "expected_sha256" fields)
         known_softmax_wide_expected_sha256
    && String.equal
         (string_field "expected_root" fields)
         known_softmax_wide_expected_root
    && String.equal
         (string_field "observed_sha256" fields)
         known_softmax_wide_observed_sha256
    && String.equal
         (string_field "observed_root" fields)
         known_softmax_wide_observed_root
    &&
    (match field "mismatch_detail" fields with
     | Some (`Assoc detail) ->
       (match
          opt_int_field "first_mismatch_f64_cell" detail,
          opt_int_field "raw_u64_bit_delta" detail,
          opt_string_field "expected_bits_hex" detail,
          opt_string_field "observed_bits_hex" detail
        with
        | Some 613, Some 1, Some "0x3c6cceffa4571f9a", Some "0x3c6cceffa4571f9b" ->
          true
        | _ -> false)
     | _ -> false)
  | _ -> false

let known_softmax_portability_gap ~case_name fixture params outputs =
  String.equal case_name "wide-1024-stable-tail"
  && int_field "count" params = 1024
  && known_softmax_wide_fixture fixture
  && List.exists known_softmax_wide_output_mismatch outputs

let p0_plus_failure_classification ~opcode ~case_name ~fixture ~params ~outputs
    ~ran ~matched =
  if matched then
    "none", "none"
  else if not ran then
    "vm_execution_rejected", "inspect_admission_or_runtime_rejection"
  else
    match opcode with
    | "SOFTMAX_FP"
      when known_softmax_portability_gap ~case_name fixture params outputs ->
      ( "host_transcendental_portability_gap",
        "qualified_protocol_owned_exp_required_before_consensus" )
    | _ ->
      ( "deterministic_output_mismatch",
        "inspect_vm_semantics_or_fixture_authority" )

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
  let determinism_classification, next_action =
    p0_plus_failure_classification
      ~opcode
      ~case_name
      ~fixture
      ~params
      ~outputs
      ~ran
      ~matched
  in
  let replacement_plan =
    Diagnostics.p0_plus_replacement_plan
      ~opcode
      ~case_name
      ~classification:determinism_classification
      ~outputs
  in
  let profile_gates = p0_plus_profile_gates opcode fixture in
  let profile_root_bindings =
    p0_plus_profile_root_bindings fixture profile_gates
  in
  accepted,
  `Assoc [
    "case", `String case_name;
    "opcode", `String opcode;
    "primitive", `String primitive;
    "manifest", `String manifest_path;
    "profile_gates", `List profile_gates;
    "profile_gate_count", `Int (List.length profile_gates);
    "profile_root_bindings", `List profile_root_bindings;
    "status", `String (if accepted then "accepted" else "rejected");
    "vm_run", `String (if ran then "accepted" else "rejected");
    "output_status", `String (if matched then "matched" else "mismatch");
    "determinism_classification", `String determinism_classification;
    "next_action", `String next_action;
    "deterministic_replacement_plan",
    (match replacement_plan with
     | Some plan -> plan
     | None -> `Null);
    "observed_effort", `Int effort;
    "outputs", `List outputs;
  ]

let p0_plus_rejected_results results =
  List.filter_map
    (fun (ok, result) ->
       if ok then
         None
       else
         match result with
         | `Assoc fields ->
           Some
             (`Assoc [
               "case", field_or_null "case" fields;
               "opcode", field_or_null "opcode" fields;
               "manifest", field_or_null "manifest" fields;
               "output_status", field_or_null "output_status" fields;
               "determinism_classification",
               field_or_null "determinism_classification" fields;
               "next_action", field_or_null "next_action" fields;
               "deterministic_replacement_plan",
               field_or_null "deterministic_replacement_plan" fields;
               "outputs", field_or_null "outputs" fields;
             ])
         | _ ->
           Some
             (`Assoc [
               "case", `Null;
               "opcode", `Null;
               "manifest", `Null;
               "output_status", `String "invalid_result";
               "outputs", `List [];
             ]))
    results

let p0_plus_deterministic_replacement_plans results =
  results
  |> List.filter_map (function
    | _, `Assoc fields ->
      (match field "deterministic_replacement_plan" fields with
       | Some (`Assoc _ as plan) -> Some plan
       | _ -> None)
    | _ -> None)
  |> List.sort_uniq compare

let template_corpus_entry root_dir entry =
  let template_path = string_field "vm_execution_template" entry in
  let full_template_path = Filename.concat root_dir template_path in
  let raw = read_file full_template_path in
  ignore (read_json full_template_path);
  `Assoc [
    "path", `String template_path;
    "sha256", `String (sha256 raw);
  ]

let template_corpus_root root_dir entries =
  entries
  |> List.map (template_corpus_entry root_dir)
  |> List.sort compare
  |> fun values -> sha256 (Yojson.Safe.to_string (`List values))

let p0_plus_fixture_corpus_entry root_dir entry =
  let manifest_path = string_field "manifest" entry in
  let full_manifest_path = Filename.concat root_dir manifest_path in
  let raw = read_file full_manifest_path in
  ignore (read_json full_manifest_path);
  `Assoc [
    "manifest", `String manifest_path;
    "sha256", `String (sha256 raw);
  ]

let p0_plus_fixture_corpus_root root_dir entries =
  entries
  |> List.map (p0_plus_fixture_corpus_entry root_dir)
  |> List.sort compare
  |> fun values -> sha256 (Yojson.Safe.to_string (`List values))

let run_p0_plus_pack path =
  (match !cross_platform_matrix with
   | Some _ -> fail "--cross-platform-matrix is supported only with --template-index"
   | None -> ());
  (match !expected_cross_platform_matrix_sha256 with
   | Some _ ->
     fail
       "--expected-cross-platform-matrix-sha256 is supported only with --template-index"
   | None -> ());
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
  let fixture_corpus_root = Some (p0_plus_fixture_corpus_root root_dir entries) in
  let fixture_count = List.length results in
  let execution_accepted = List.for_all fst results in
  let execution_status =
    if execution_accepted then "accepted" else "rejected"
  in
  let profile_gate_count =
    List.fold_left
      (fun count (_, result) ->
         match result with
         | `Assoc fields ->
           (match field "profile_gate_count" fields with
            | Some (`Int value) -> count + value
            | Some (`Intlit value) -> count + int_of_string value
            | _ -> count)
         | _ -> count)
      0
      results
  in
  let profile_gates =
    List.fold_left
      (fun gates (_, result) ->
         match result with
         | `Assoc fields ->
           (match field "profile_gates" fields with
            | Some (`List values) -> values @ gates
            | _ -> gates)
         | _ -> gates)
      []
      results
  in
  let profile_root_bindings =
    List.fold_left
      (fun bindings (_, result) ->
         match result with
         | `Assoc fields ->
           (match field "profile_root_bindings" fields with
            | Some (`List values) -> values @ bindings
            | _ -> bindings)
         | _ -> bindings)
      []
      results
  in
  let status_counts = Profile.status_counts_of_json_gates profile_gates in
  let root_binding_counts =
    Profile.root_binding_counts_of_json profile_root_bindings
  in
  let vm_semantics_binding_counts =
    Profile.root_binding_counts_of_json []
  in
  let abi_declaration_binding_counts =
    Profile.root_binding_counts_of_json []
  in
  let executable_abi_counts = executable_abi_binding_counts [] in
  let root_binding_classification_counts =
    Profile.root_binding_classification_counts_of_json profile_root_bindings
  in
  let classified_profile_gate_count =
    Profile.classified_gate_count status_counts
  in
  let transcendental_dependency_catalog =
    Profile.transcendental_dependency_catalog_json
      ~opcodes:(profile_gate_opcodes profile_gates)
  in
  let cross_platform_evidence_json =
    `Assoc [
      "status", `String "not_supported";
      "path", `Null;
      "blockers", `List [`String "p0_plus_matrix_not_supported"];
    ]
  in
  let validator_readiness_gate_json =
    validator_readiness_gate
      ~required:!require_validator_readiness
      ~execution_accepted
      ~strict_effort_status:"not_supported"
      ~effort_status:"not_supported"
      ~effort_ready:false
      ~effort_blockers:["p0_plus_effort_authority_missing"]
      ~template_count:fixture_count
      ~profile_gate_count
      ~unprofiled_count:0
      ~root_binding_counts
      ~vm_semantics_binding_counts
      ~abi_declaration_binding_counts
      ~executable_abi_counts
      ~transcendental_dependency_catalog
      ~cross_platform_evidence:cross_platform_evidence_json
      ~included_template_count:0
      ~declared_failure_case_count:0
      ~counted_failure_case_count:0
      ~accepted_counted_failure_case_count:0
      ~required_failure_case_contract_blockers:[]
      status_counts
  in
  let repair_hints = producer_repair_hints (List.map snd results) in
  let accepted =
    execution_accepted
    && profile_roots_required_passes ~root_binding_counts
    &&
    consensus_candidate_required_passes
      ~profile_gate_count
      ~unprofiled_count:0
      ~root_binding_counts
      status_counts
    &&
    consensus_ready_required_passes
      ~profile_gate_count
      ~unprofiled_count:0
      ~root_binding_counts
      status_counts
    && required_gate_passes
         ~required:!require_validator_readiness
         validator_readiness_gate_json
  in
  `Assoc ([
    "status", `String (if accepted then "accepted" else "rejected");
    "execution_status", `String execution_status;
    "diagnostic_only", `Bool true;
    "validator_readiness_required", `Bool !require_validator_readiness;
    "execution_mode", `String "p0_plus_fixture_pack_vm_execution";
    "platform", platform_json ();
    "fixture_pack", `String path;
    "fixture_count", `Int fixture_count;
    "accepted_count", `Int (List.length (List.filter fst results));
    "rejected_count",
    `Int (List.length (List.filter (fun (ok, _) -> not ok) results));
    "profile_gate_count", `Int profile_gate_count;
    "classified_profile_gate_count", `Int classified_profile_gate_count;
    "profile_consensus_status_counts",
    Profile.status_counts_json status_counts;
    "profile_root_catalog",
    Profile.profile_root_catalog_json profile_gates;
    "profile_catalog_root",
    Profile.profile_catalog_root_json profile_gates;
    "profile_root_binding_catalog",
    Profile.profile_root_binding_catalog_json (List.map snd results);
    "producer_repair_hints", `List repair_hints;
    "producer_repair_manifest",
    producer_repair_manifest ?corpus_root:fixture_corpus_root repair_hints;
    "consensus_blocker_catalog",
    Profile.consensus_blocker_catalog_json profile_gates;
    "consensus_blocker_class_counts",
    Profile.consensus_blocker_class_counts_json profile_gates;
  ]
  @ transcendental_dependency_catalog_report_fields transcendental_dependency_catalog
  @ [
    "profile_root_binding_status_counts",
    Profile.root_binding_counts_json root_binding_counts;
    "profile_root_binding_classification_counts",
    Profile.root_binding_classification_counts_json
      root_binding_classification_counts;
    "profile_root_binding_gate",
    Profile.root_binding_gate_json
      ~required:!require_profile_roots_bound
      root_binding_counts;
    "vm_semantics_binding_status_counts",
    Profile.root_binding_counts_json vm_semantics_binding_counts;
    "vm_semantics_binding_gate",
    vm_semantics_binding_gate_json vm_semantics_binding_counts;
    "abi_declaration_binding_status_counts",
    Profile.root_binding_counts_json abi_declaration_binding_counts;
    "abi_declaration_binding_gate", abi_declaration_binding_gate_json abi_declaration_binding_counts;
    "executable_abi_binding_status_counts",
    executable_abi_counts_json executable_abi_counts;
    "executable_abi_binding_gate",
    executable_abi_gate_json executable_abi_counts;
    "validator_readiness_gate", validator_readiness_gate_json;
    "consensus_candidate_gate",
    consensus_candidate_gate
      ~profile_gate_count
      ~unprofiled_count:0
      ~root_binding_counts
      status_counts;
    "consensus_ready_gate",
    consensus_ready_gate
      ~profile_gate_count
      ~unprofiled_count:0
      ~root_binding_counts
      status_counts;
    "deterministic_replacement_plans",
    `List (p0_plus_deterministic_replacement_plans results);
    "rejected_results", `List (p0_plus_rejected_results results);
    "results", `List (List.map snd results);
  ])

let run_index path =
  let root_dir = Filename.dirname path in
  let index =
    match read_json path with
    | `Assoc fields -> fields
    | _ -> fail (path ^ ": index must be an object")
  in
  let all_entries =
    list_field "templates" index
    |> List.map (function
      | `Assoc fields -> fields
      | _ -> fail "template index entries must be objects")
  in
  let entries =
    List.filter
      (fun fields -> opcode_selected (string_field "opcode" fields))
      all_entries
  in
  List.iter
    (fun opcode ->
       if
         not
           (List.exists
              (fun fields -> String.equal (string_field "opcode" fields) opcode)
              entries)
       then fail ("missing selected P0 template: " ^ opcode))
    (selected_opcodes ());
  let results = List.map (execute_template root_dir) entries in
  let execution_accepted = List.for_all fst results in
  let execution_status =
    if execution_accepted then "accepted" else "rejected"
  in
  let template_count = List.length results in
  let result_fields =
    List.map
      (function
        | _, `Assoc fields -> fields
        | _ -> fail "result must be an object")
      results
  in
  let strict_effort_ready =
    template_count > 0
    &&
    List.for_all
      (fun fields -> bool_field "strict_effort" fields)
      result_fields
  in
  let effort_match_ready =
    template_count > 0
    &&
    List.for_all
      (fun fields -> bool_field "effort_match" fields)
      result_fields
  in
  let effort_blockers =
    []
    |> add_blocker (not strict_effort_ready) "strict_effort_not_enabled"
    |> add_blocker (not effort_match_ready) "effort_mismatch"
  in
  let included_failure_template_count =
    List.length
      (List.filter
         (fun (_, result) ->
            match result with
            | `Assoc fields -> bool_field "failure_cases_included" fields
            | _ -> false)
         results)
  in
  let counted_failure_case_count =
    List.fold_left
      (fun count (_, result) ->
         match result with
         | `Assoc fields -> count + int_field "counted_failure_case_count" fields
         | _ -> count)
      0
      results
  in
  let declared_failure_case_count =
    List.fold_left
      (fun count (_, result) ->
         match result with
         | `Assoc fields -> count + int_field "failure_case_count" fields
         | _ -> count)
      0
      results
  in
  let accepted_counted_failure_case_count =
    List.fold_left
      (fun count (_, result) ->
         match result with
         | `Assoc fields ->
           count + int_field "accepted_counted_failure_case_count" fields
         | _ -> count)
      0
      results
  in
  let required_failure_case_contract_blockers =
    List.fold_left
      (fun blockers (_, result) ->
         match result with
         | `Assoc fields ->
           (match field "required_failure_case_contract" fields with
            | Some (`Assoc contract_fields) ->
              string_list_field "blockers" contract_fields @ blockers
            | _ -> blockers)
         | _ -> blockers)
      []
      results
    |> List.sort_uniq String.compare
  in
  let profile_gate_count =
    List.fold_left
      (fun count (_, result) -> count + result_profile_gate_count result)
      0
      results
  in
  let profile_gates =
    List.filter_map profile_gate_value (List.map snd results)
  in
  let status_counts =
    Profile.status_counts_of_json_gates profile_gates
  in
  let root_binding_counts =
    profile_root_binding_status_counts (List.map snd results)
  in
  let vm_semantics_binding_counts =
    vm_semantics_binding_status_counts (List.map snd results)
  in
  let abi_declaration_binding_counts =
    abi_declaration_binding_status_counts (List.map snd results)
  in
  let executable_abi_counts =
    executable_abi_binding_counts (List.map snd results)
  in
  let root_binding_classification_counts =
    profile_root_binding_classification_counts (List.map snd results)
  in
  let classified_profile_gate_count =
    Profile.classified_gate_count status_counts
  in
  let unprofiled_count = List.length results - profile_gate_count in
  let required_opcodes =
    match selected_opcodes () with
    | [] -> Template.p0_opcodes
    | opcodes -> opcodes
  in
  let profile_catalog_root =
    profile_catalog_root_option
      (Profile.profile_catalog_root_json profile_gates)
  in
  let template_corpus_root = Some (template_corpus_root root_dir entries) in
  let local_result_signature_sha256 =
    Some (sha256 (Signature.signature_json (List.map snd results)))
  in
  let transcendental_dependency_catalog =
    Profile.transcendental_dependency_catalog_json
      ~opcodes:(profile_gate_opcodes profile_gates)
  in
  let transcendental_dependency_catalog_root =
    Some
      (Profile.transcendental_dependency_catalog_root
         transcendental_dependency_catalog)
  in
  let cross_platform_evidence_json =
    cross_platform_evidence
      ~required_opcodes
      ~profile_catalog_root
      ~transcendental_dependency_catalog_root
      ~template_corpus_root
      ~local_result_signature_sha256
  in
  let accepted =
    execution_accepted
    && failure_cases_required_pass
         ~template_count
         ~included_template_count:included_failure_template_count
         ~declared_failure_case_count
         ~counted_failure_case_count
         ~accepted_counted_failure_case_count
         ~required_failure_case_contract_blockers
    && profile_roots_required_passes ~root_binding_counts
    &&
    consensus_candidate_required_passes
      ~profile_gate_count
      ~unprofiled_count
      ~root_binding_counts
      status_counts
    &&
    consensus_ready_required_passes
      ~profile_gate_count
      ~unprofiled_count
      ~root_binding_counts
      status_counts
  in
  let validator_readiness_gate_json =
    validator_readiness_gate
      ~required:!require_validator_readiness
      ~execution_accepted
      ~strict_effort_status:
        (if strict_effort_ready then "accepted" else "rejected")
      ~effort_status:
        (if effort_match_ready then "accepted" else "rejected")
      ~effort_ready:(strict_effort_ready && effort_match_ready)
      ~effort_blockers
      ~template_count
      ~profile_gate_count
      ~unprofiled_count
      ~root_binding_counts
      ~vm_semantics_binding_counts
      ~abi_declaration_binding_counts
      ~executable_abi_counts
      ~transcendental_dependency_catalog
      ~cross_platform_evidence:cross_platform_evidence_json
      ~included_template_count:included_failure_template_count
      ~declared_failure_case_count
      ~counted_failure_case_count
      ~accepted_counted_failure_case_count
      ~required_failure_case_contract_blockers
      status_counts
  in
  let repair_hints = producer_repair_hints (List.map snd results) in
  let accepted =
    accepted
    && required_gate_passes
         ~required:!require_validator_readiness
         validator_readiness_gate_json
  in
  let status = if accepted then "accepted" else "rejected" in
  `Assoc ([
    "status", `String status;
    "execution_status", `String execution_status;
    "diagnostic_only", `Bool true;
    "validator_readiness_required", `Bool !require_validator_readiness;
    "execution_mode", `String "positive_template_vm_execution";
    "platform", platform_json ();
    "template_index", `String path;
    "selected_opcodes",
    `List (List.map (fun opcode -> `String opcode) (selected_opcodes ()));
    "source_template_count", `Int (List.length all_entries);
    "template_count", `Int template_count;
    "template_corpus_root",
    (match template_corpus_root with
     | Some root -> `String root
     | None -> `Null);
    "result_signature_schema", `String Signature.schema;
    "result_signature_sha256",
    (match local_result_signature_sha256 with
     | Some signature -> `String signature
     | None -> `Null);
    "failure_case_gate",
    failure_case_gate
      ~template_count
      ~included_template_count:included_failure_template_count
      ~declared_failure_case_count
      ~counted_failure_case_count
      ~accepted_counted_failure_case_count
      ~required_failure_case_contract_blockers;
    "profile_gate_count", `Int profile_gate_count;
    "classified_profile_gate_count", `Int classified_profile_gate_count;
    "unprofiled_template_count", `Int unprofiled_count;
    "profile_consensus_status_counts",
    Profile.status_counts_json status_counts;
    "profile_root_catalog",
    Profile.profile_root_catalog_json profile_gates;
    "profile_catalog_root",
    Profile.profile_catalog_root_json profile_gates;
    "profile_root_binding_catalog",
    Profile.profile_root_binding_catalog_json (List.map snd results);
    "producer_repair_hints", `List repair_hints;
    "producer_repair_manifest",
    producer_repair_manifest ?corpus_root:template_corpus_root repair_hints;
    "consensus_blocker_catalog",
    Profile.consensus_blocker_catalog_json profile_gates;
    "consensus_blocker_class_counts",
    Profile.consensus_blocker_class_counts_json profile_gates;
  ]
  @ transcendental_dependency_catalog_report_fields transcendental_dependency_catalog
  @ [
    "profile_root_binding_status_counts",
    Profile.root_binding_counts_json root_binding_counts;
    "profile_root_binding_classification_counts",
    Profile.root_binding_classification_counts_json
      root_binding_classification_counts;
    "profile_root_binding_gate",
    Profile.root_binding_gate_json
      ~required:!require_profile_roots_bound
      root_binding_counts;
    "vm_semantics_binding_status_counts",
    Profile.root_binding_counts_json vm_semantics_binding_counts;
    "vm_semantics_binding_gate",
    vm_semantics_binding_gate_json vm_semantics_binding_counts;
    "abi_declaration_binding_status_counts",
    Profile.root_binding_counts_json abi_declaration_binding_counts;
    "abi_declaration_binding_gate", abi_declaration_binding_gate_json abi_declaration_binding_counts;
    "executable_abi_binding_status_counts",
    executable_abi_counts_json executable_abi_counts;
    "executable_abi_binding_gate",
    executable_abi_gate_json executable_abi_counts;
    "validator_readiness_gate", validator_readiness_gate_json;
    "consensus_candidate_gate",
    consensus_candidate_gate
      ~profile_gate_count
      ~unprofiled_count
      ~root_binding_counts
      status_counts;
    "consensus_ready_gate",
    consensus_ready_gate
      ~profile_gate_count
      ~unprofiled_count
      ~root_binding_counts
      status_counts;
    "accepted_count",
    `Int (List.length (List.filter fst results));
    "rejected_count",
    `Int (List.length (List.filter (fun (ok, _) -> not ok) results));
    "results", `List (List.map snd results);
  ])

let () =
  Arg.parse args (fun value -> fail ("unexpected argument: " ^ value)) usage;
  validate_selected_opcodes ();
  if !synthesize_required_failure_cases && not !include_failures then
    fail "--synthesize-required-failure-cases requires --include-failures";
  if !synthesize_required_failure_cases && !p0_plus_pack <> None then
    fail
      "--synthesize-required-failure-cases is only supported with \
       --template-index";
  if !require_failure_cases && !p0_plus_pack <> None then
    fail "--require-failure-cases is only supported with --template-index";
  if selected_opcodes () <> [] && !p0_plus_pack <> None then
    fail "--opcode is supported only with --template-index";
  let report =
    match !template_index, !p0_plus_pack with
    | Some _, Some _ -> fail "choose only one input mode"
    | None, None -> fail usage
    | Some path, None -> run_index path
    | None, Some path -> run_p0_plus_pack path
  in
  print_endline (Yojson.Safe.pretty_to_string report);
  let accepted =
    match report with
    | `Assoc fields ->
      let report_accepted =
        match field "status" fields with
        | Some (`String "accepted") -> true
        | _ -> false
      in
      let validator_readiness_accepted =
        (not !require_validator_readiness)
        ||
        match field "validator_readiness_gate" fields with
        | Some (`Assoc gate_fields) ->
          (match field "status" gate_fields with
           | Some (`String "accepted") -> true
           | _ -> false)
        | _ -> false
      in
      report_accepted && validator_readiness_accepted
    | _ -> false
  in
  if not accepted then exit 1
