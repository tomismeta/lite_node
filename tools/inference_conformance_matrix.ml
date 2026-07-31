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

let runner_reports = ref []
let min_platforms = ref 2
let require_validator_readiness = ref false
let requested_opcodes = ref []

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
      "executable_abi_binding_mismatch";
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

let subspan_signature = function
  | `Assoc fields ->
    `Assoc [
      "name", `String (string_field "name" fields);
      "length_f64_cells", `Int (int_field "length_f64_cells" fields);
      "expected_sha256", `String (string_field "expected_sha256" fields);
      "observed_sha256", `String (string_field "observed_sha256" fields);
      "expected_root",
      (match opt_string_field "expected_root" fields with
       | None -> `Null
       | Some root -> `String root);
      "observed_root", `String (string_field "observed_root" fields);
      "root_matched", `Bool (bool_field "root_matched" fields);
      "matched", `Bool (bool_field "matched" fields);
    ]
  | _ -> fail "subspan must be an object"

let failure_span_signature = function
  | `Assoc fields ->
    `Assoc [
      "name", `String (string_field "name" fields);
      "base_address", `Int (int_field "base_address" fields);
      "length_f64_cells", `Int (int_field "length_f64_cells" fields);
      "before_sha256", `String (string_field "before_sha256" fields);
      "after_sha256", `String (string_field "after_sha256" fields);
    ]
  | _ -> fail "failure span must be an object"

let finite_span_signature = function
  | `Assoc fields ->
    `Assoc [
      "name", `String (string_field "name" fields);
      "base_address", `Int (int_field "base_address" fields);
      "length_f64_cells", `Int (int_field "length_f64_cells" fields);
      "finite", `Bool (bool_field "finite" fields);
    ]
  | _ -> fail "finite span must be an object"

let failure_snapshot_signature = function
  | `Assoc fields ->
    `Assoc [
      "status", `String (string_field "status" fields);
      "base_address", `Int (int_field "base_address" fields);
      "length_f64_cells", `Int (int_field "length_f64_cells" fields);
      "expected_length_f64_cells",
      `Int (int_field "expected_length_f64_cells" fields);
      "expected_sha256", `String (string_field "expected_sha256" fields);
      "observed_sha256", `String (string_field "observed_sha256" fields);
    ]
  | _ -> fail "failure snapshot must be an object"

let optional_failure_snapshot_signature fields =
  match string_field "snapshot_output_status" fields with
  | "not_required" -> `Assoc ["status", `String "not_required"]
  | _ ->
    (match field "snapshot_output" fields with
     | Some value -> failure_snapshot_signature value
     | None -> fail "missing snapshot_output")

let failure_case_signature = function
  | `Assoc fields ->
    `Assoc [
      "case", `String (string_field "case" fields);
      "expected", `String (string_field "expected" fields);
      "status", `String (string_field "status" fields);
      "counted", `Bool (bool_field "counted" fields);
      "observed", `String (string_field "observed" fields);
      "observed_effort", `Int (int_field "observed_effort" fields);
      "unchanged_status", `String (string_field "unchanged_status" fields);
      "changed_status", `String (string_field "changed_status" fields);
      "finite_status", `String (string_field "finite_status" fields);
      "active_changed_status",
      `String (string_field "active_changed_status" fields);
      "active_finite_status",
      `String (string_field "active_finite_status" fields);
      "snapshot_output_status",
      `String (string_field "snapshot_output_status" fields);
      "snapshot_output", optional_failure_snapshot_signature fields;
      "unchanged_spans",
      `List
        (List.map
           failure_span_signature
           (list_field "unchanged_spans" fields));
      "finite_spans",
      `List
        (List.map
           finite_span_signature
           (list_field "finite_spans" fields));
      "active_changed_spans",
      `List
        (List.map
           failure_span_signature
           (list_field "active_changed_spans" fields));
      "active_finite_spans",
      `List
        (List.map
           finite_span_signature
           (list_field "active_finite_spans" fields));
    ]
  | _ -> fail "failure case must be an object"

let result_signature = function
  | `Assoc fields ->
    `Assoc [
      "opcode", `String (string_field "opcode" fields);
      "profile_root_binding",
      (match field "profile_root_binding" fields with
       | Some (`Assoc _ as binding) -> binding
       | _ -> fail "missing profile_root_binding");
      "vm_semantics_binding",
      (match field "vm_semantics_binding" fields with
       | Some (`Assoc _ as binding) -> binding
       | _ -> fail "missing vm_semantics_binding");
      "abi_declaration_binding",
      (match field "abi_declaration_binding" fields with
       | Some (`Assoc _ as binding) -> binding
       | _ -> fail "missing abi_declaration_binding");
      "executable_abi_binding",
      (match field "executable_abi_binding" fields with
       | Some (`Assoc _ as binding) -> binding
       | _ -> fail "missing executable_abi_binding");
      "status", `String (string_field "status" fields);
      "vm_run", `String (string_field "vm_run" fields);
      "output_status", `String (string_field "output_status" fields);
      "expected_effort", `Int (int_field "expected_effort" fields);
      "observed_effort", `Int (int_field "observed_effort" fields);
      "expected_opcode_effort",
      (match field "expected_opcode_effort" fields with
       | Some value -> value
       | None -> fail "missing expected_opcode_effort");
      "observed_opcode_effort",
      `Int (int_field "observed_opcode_effort" fields);
      "opcode_effort_match",
      `Bool (bool_field "opcode_effort_match" fields);
      "effort_match", `Bool (bool_field "effort_match" fields);
      "strict_effort", `Bool (bool_field "strict_effort" fields);
      "required_failure_case_contract",
      json_field_or_null "required_failure_case_contract" fields;
      "subspans", `List (List.map subspan_signature (list_field "subspans" fields));
      "failure_cases",
      `List
        (list_field "failure_cases" fields
         |> List.map failure_case_signature
         |> List.sort compare);
    ]
  | _ -> fail "result must be an object"

let result_opcode = function
  | `Assoc fields -> string_field "opcode" fields
  | _ -> fail "result must be an object"

let opcode_requires_failure_case_contract opcode =
  String.equal opcode "LINEAR_Q1_G128_FP"

let required_failure_case_contract_bound opcode fields =
  if not (opcode_requires_failure_case_contract opcode) then
    true
  else
    match field "contract" fields with
    | Some (`Assoc contract_fields) ->
      String.equal
        (string_field "opcode" contract_fields)
        opcode
      &&
      (match field "expectations" contract_fields with
       | Some (`List (_ :: _)) -> true
       | _ -> false)
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

let signature_json results =
  results
  |> List.map result_signature
  |> List.sort compare
  |> fun values -> Yojson.Safe.to_string (`List values)

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
    let signature = signature_json results in
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
