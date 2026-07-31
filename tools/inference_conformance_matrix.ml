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
]

let usage =
  "inference_conformance_matrix --runner-report <path> \
   [--runner-report <path> ...] [--min-platforms <n>]"

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

let add_if condition value values =
  if condition then value :: values else values

let opt_status_is_accepted name fields =
  match opt_string_field name fields with
  | Some "accepted" -> true
  | _ -> false

let platform_key platform =
  String.concat
    "|"
    [
      string_field "ocaml_version" platform;
      string_field "os_type" platform;
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
      "matched", `Bool (bool_field "matched" fields);
    ]
  | _ -> fail "subspan must be an object"

let result_signature = function
  | `Assoc fields ->
    `Assoc [
      "opcode", `String (string_field "opcode" fields);
      "status", `String (string_field "status" fields);
      "vm_run", `String (string_field "vm_run" fields);
      "output_status", `String (string_field "output_status" fields);
      "expected_effort", `Int (int_field "expected_effort" fields);
      "observed_effort", `Int (int_field "observed_effort" fields);
      "effort_match", `Bool (bool_field "effort_match" fields);
      "strict_effort", `Bool (bool_field "strict_effort" fields);
      "subspans", `List (List.map subspan_signature (list_field "subspans" fields));
    ]
  | _ -> fail "result must be an object"

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
    let accepted =
      String.equal execution_mode "positive_template_vm_execution"
      && opt_status_is_accepted "status" fields
      && opt_status_is_accepted "execution_status" fields
      && (match failure_gate with
          | Some fields -> opt_status_is_accepted "status" fields
          | None -> false)
      && strict_effort
      && output_matched
      && effort_matched
    in
    let blockers =
      []
      |> add_if
           (not (String.equal execution_mode "positive_template_vm_execution"))
           "not_p0_runner_report"
      |> add_if (not (opt_status_is_accepted "status" fields)) "report_rejected"
      |> add_if
           (not (opt_status_is_accepted "execution_status" fields))
           "execution_rejected"
      |> add_if
           (not
              (match failure_gate with
               | Some fields -> opt_status_is_accepted "status" fields
               | None -> false))
           "punitive_failure_cases_rejected"
      |> add_if (not strict_effort) "strict_effort_not_enabled"
      |> add_if (not output_matched) "output_mismatch"
      |> add_if (not effort_matched) "effort_mismatch"
    in
    let signature = signature_json results in
    let signature_sha256 = sha256 signature in
    let validator_readiness_status, validator_readiness_blockers =
      validator_readiness_summary fields
    in
    accepted,
    platform_key platform,
    signature,
    validator_readiness_blockers,
    `Assoc [
      "path", `String path;
      "runner_report_sha256", `String report_sha256;
      "result_signature_sha256", `String signature_sha256;
      "accepted", `Bool accepted;
      "platform", `Assoc platform;
      "platform_key", `String (platform_key platform);
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
      "validator_readiness_status", `String validator_readiness_status;
      "validator_readiness_blockers",
      `List
        (List.map
           (fun blocker -> `String blocker)
           validator_readiness_blockers);
      "strict_effort", `Bool strict_effort;
      "output_status", `String (if output_matched then "matched" else "mismatch");
      "effort_status", `String (if effort_matched then "matched" else "mismatch");
      "blockers", `List (List.map (fun blocker -> `String blocker) blockers);
    ]
  | _ -> fail (path ^ ": runner report must be an object")

let unique values =
  List.sort_uniq String.compare values

let without_cross_platform_blocker blockers =
  List.filter
    (fun blocker -> not (String.equal blocker "cross_platform_conformance_missing"))
    blockers

let matrix_report paths =
  let summaries = List.map report_summary paths in
  let report_count = List.length summaries in
  let accepted_reports =
    List.length (List.filter (fun (accepted, _, _, _, _) -> accepted) summaries)
  in
  let platforms =
    unique (List.map (fun (_, platform, _, _, _) -> platform) summaries)
  in
  let signatures =
    unique (List.map (fun (_, _, signature, _, _) -> signature) summaries)
  in
  let per_report_validator_blockers =
    summaries
    |> List.map (fun (_, _, _, blockers, _) -> blockers)
    |> List.concat
    |> without_cross_platform_blocker
    |> unique
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
    && List.length signatures = 1
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
         (List.length signatures <> 1)
         "result_mismatch_across_platforms"
  in
  let validator_readiness_blockers =
    unique (blockers @ per_report_validator_blockers)
  in
  let validator_readiness_accepted =
    matrix_accepted && validator_readiness_blockers = []
  in
  `Assoc [
    "status", `String (if matrix_accepted then "accepted" else "rejected");
    "diagnostic_only", `Bool true;
    "schema", `String "octra.inference.conformance.matrix.v1";
    "report_count", `Int report_count;
    "accepted_report_count", `Int accepted_reports;
    "required_distinct_platform_count", `Int !min_platforms;
    "distinct_platform_count", `Int (List.length platforms);
    "cross_platform_status",
    `String (if matrix_accepted then "accepted" else "rejected");
    "blockers", `List (List.map (fun blocker -> `String blocker) blockers);
    "validator_readiness_status",
    `String (if validator_readiness_accepted then "accepted" else "rejected");
    "validator_readiness_blockers",
    `List
      (List.map
         (fun blocker -> `String blocker)
         validator_readiness_blockers);
    "platform_keys", `List (List.map (fun value -> `String value) platforms);
    "result_signature_count", `Int (List.length signatures);
    "matrix_signature_sha256",
    (match matrix_signature_sha256 with
     | None -> `Null
     | Some value -> `String value);
    "reports", `List (List.map (fun (_, _, _, _, json) -> json) summaries);
  ]

let () =
  Arg.parse args (fun value -> fail ("unexpected argument: " ^ value)) usage;
  if !min_platforms <= 0 then fail "--min-platforms must be positive";
  let paths = List.rev !runner_reports in
  let report = matrix_report paths in
  print_endline (Yojson.Safe.pretty_to_string report);
  match report with
  | `Assoc fields ->
    (match field "status" fields with
     | Some (`String "accepted") -> ()
     | _ -> exit 1)
  | _ -> exit 1
