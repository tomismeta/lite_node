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

type p0_status = {
  schedule_operation : string;
  current_litenode_opcode : string;
  recommendation : string;
  next_action : string;
}

type differential_status = {
  candidate : string;
  selected_token_changes : int;
  top_k_order_changes : int;
}

type t = {
  corpus_type : string;
  operation_count : int option;
  fixture_count : int;
  failure_case_count : int;
  cutpoint_count : int;
  logit_cutpoint_count : int option;
  selected_token_changes_under_q16_16_candidate : int option;
  first_divergent_cutpoint : string option;
  differential_reports : differential_status list;
  p0_worklist : p0_status list;
}

type error =
  | Json_error of string
  | Corpus_error of string

type operation = {
  schedule_operation : string;
  current_litenode_opcode : string;
  recommendation : string;
}

type fixture_case = {
  primitive : string;
  opcode : string option;
}

type p0_requirement = {
  schedule_operation : string;
  opcode : string;
  fixture_primitives : string list;
}

let error_message = function
  | Json_error message -> "invalid determinism corpus json: " ^ message
  | Corpus_error message -> "invalid determinism corpus: " ^ message

let ( let* ) result f =
  match result with
  | Ok value -> f value
  | Error error -> Error error

let assoc name = function
  | `Assoc fields -> Ok fields
  | _ -> Error (Json_error ("expected object: " ^ name))

let field name fields =
  match List.filter (fun (key, _) -> String.equal key name) fields with
  | [(_, value)] -> Ok value
  | [] -> Error (Json_error ("missing field: " ^ name))
  | _ -> Error (Json_error ("duplicate field: " ^ name))

let optional_field name fields =
  match List.filter (fun (key, _) -> String.equal key name) fields with
  | [] -> Ok None
  | [(_, value)] -> Ok (Some value)
  | _ -> Error (Json_error ("duplicate field: " ^ name))

let string_field name fields =
  match field name fields with
  | Ok (`String value) -> Ok value
  | Ok _ -> Error (Json_error ("field must be a string: " ^ name))
  | Error error -> Error error

let optional_string_field name fields =
  match optional_field name fields with
  | Ok None -> Ok None
  | Ok (Some (`String value)) -> Ok (Some value)
  | Ok (Some `Null) -> Ok None
  | Ok (Some _) -> Error (Json_error ("field must be a string: " ^ name))
  | Error error -> Error error

let int_field name fields =
  match field name fields with
  | Ok (`Int value) -> Ok value
  | Ok _ -> Error (Json_error ("field must be an int: " ^ name))
  | Error error -> Error error

let optional_int_field name fields =
  match optional_field name fields with
  | Ok None -> Ok None
  | Ok (Some (`Int value)) -> Ok (Some value)
  | Ok (Some _) -> Error (Json_error ("field must be an int: " ^ name))
  | Error error -> Error error

let list_field name fields =
  match field name fields with
  | Ok (`List values) -> Ok values
  | Ok _ -> Error (Json_error ("field must be a list: " ^ name))
  | Error error -> Error error

let require_string expected name fields =
  let* actual = string_field name fields in
  if String.equal actual expected then Ok ()
  else
    Error
      (Corpus_error
         (Printf.sprintf
            "field %s expected %s actual %s"
            name
            expected
            actual))

let rec parse_list parse acc = function
  | [] -> Ok (List.rev acc)
  | value :: rest ->
    let* parsed = parse value in
    parse_list parse (parsed :: acc) rest

let parse_operation value =
  let* fields = assoc "operation" value in
  let* schedule_operation = string_field "schedule_operation" fields in
  let* current_litenode_opcode = string_field "current_litenode_opcode" fields in
  let* recommendation = string_field "recommendation" fields in
  Ok { schedule_operation; current_litenode_opcode; recommendation }

let parse_qualification_fixture value =
  let* fields = assoc "fixture case" value in
  let* primitive = string_field "primitive" fields in
  Ok { primitive; opcode = None }

let parse_ingestion_fixture value =
  let* fields = assoc "ingestion fixture" value in
  let* primitive = string_field "primitive" fields in
  let* opcode = string_field "opcode" fields in
  Ok { primitive; opcode = Some opcode }

let p0_requirements = [
  {
    schedule_operation = "q1_g128_projection";
    opcode = "LINEAR_Q1_G128_FP";
    fixture_primitives = ["q1_g128_projection"; "linear_q1_0_g128_fp"];
  };
  {
    schedule_operation = "rmsnorm_explicit_epsilon";
    opcode = "RMSNORM_FP_EPS";
    fixture_primitives = ["rmsnorm_fp_eps"];
  };
  {
    schedule_operation = "l2norm_explicit_epsilon";
    opcode = "L2NORM_FP";
    fixture_primitives = ["l2norm_fp_eps"; "l2norm_fp"];
  };
  {
    schedule_operation = "softmax";
    opcode = "SOFTMAX_FP";
    fixture_primitives = ["softmax_fp"];
  };
  {
    schedule_operation = "gated_delta_rule";
    opcode = "GATED_DELTA_RULE_FP";
    fixture_primitives = ["gated_delta_rule_fp"];
  };
]

let next_action = function
  | "deterministic_software_fp_required" ->
    "write deterministic math contract, scalar oracle, and VM conformance gate"
  | "wider_fixed_point_needed" ->
    "evaluate wider fixed-point representation against host-FP reference"
  | "q16_exact_viable"
  | "q16_exact_viable_for_selection_only" ->
    "prove ordering/root preservation before validator admission"
  | "host_fp_local_only" ->
    "keep local-only until deterministic profile exists"
  | "fixture_ingested" ->
    "add VM conformance execution for this minimized P0 fixture"
  | _ ->
    "classify recommendation before VM work"

let operation_by_name operations name =
  List.find_opt
    (fun (operation : operation) ->
       String.equal operation.schedule_operation name)
    operations

let fixture_matches requirement fixture =
  List.exists
    (String.equal fixture.primitive)
    requirement.fixture_primitives
  ||
  match fixture.opcode with
  | Some opcode -> String.equal opcode requirement.opcode
  | None -> false

let fixture_for_requirement fixtures requirement =
  List.find_opt (fixture_matches requirement) fixtures

let build_p0_from_operations operations fixtures =
  let rec loop acc = function
    | [] -> Ok (List.rev acc)
    | requirement :: rest ->
      (match operation_by_name operations requirement.schedule_operation with
       | None ->
         Error
           (Corpus_error
              ("missing P0 operation mapping: " ^ requirement.schedule_operation))
       | Some operation ->
         (match fixture_for_requirement fixtures requirement with
          | None ->
            Error
              (Corpus_error
                 ("missing P0 fixture primitive: "
                  ^ List.hd requirement.fixture_primitives))
          | Some _ ->
            let status = {
              schedule_operation = operation.schedule_operation;
              current_litenode_opcode = operation.current_litenode_opcode;
              recommendation = operation.recommendation;
              next_action = next_action operation.recommendation;
            } in
            loop (status :: acc) rest))
  in
  loop [] p0_requirements

let build_p0_from_fixtures fixtures =
  let rec loop acc = function
    | [] -> Ok (List.rev acc)
    | requirement :: rest ->
      (match fixture_for_requirement fixtures requirement with
       | None ->
         Error
           (Corpus_error
              ("missing P0 ingestion fixture: " ^ requirement.opcode))
       | Some _ ->
         let status = {
           schedule_operation = requirement.schedule_operation;
           current_litenode_opcode = requirement.opcode;
           recommendation = "fixture_ingested";
           next_action = next_action "fixture_ingested";
         } in
         loop (status :: acc) rest)
  in
  loop [] p0_requirements

let failure_case_count = function
  | `List cases -> Ok (List.length cases)
  | `Assoc groups ->
    List.fold_left
      (fun acc (_, value) ->
         match acc, value with
         | Error _, _ -> acc
         | Ok total, `List cases -> Ok (total + List.length cases)
         | Ok _, _ -> Error (Json_error "failure case group must be a list"))
      (Ok 0)
      groups
  | _ -> Error (Json_error "failure cases must be a list or object")

let parse_differential_report value =
  let* fields = assoc "differential report" value in
  let* candidate = string_field "candidate" fields in
  let* selected_token_changes = int_field "selected_token_changes" fields in
  let* top_k_order_changes = int_field "top_k_order_changes" fields in
  Ok { candidate; selected_token_changes; top_k_order_changes }

let parse_differential_summary = function
  | None -> Ok []
  | Some json ->
    let* fields = assoc "differential summary" json in
    let* reports = list_field "reports" fields in
    parse_list parse_differential_report [] reports

let q16_16_selected reports =
  match
    List.find_opt
      (fun report ->
         String.equal
           report.candidate
           "signed_q16_16_round_to_nearest_saturating")
      reports
  with
  | None -> None
  | Some report -> Some report.selected_token_changes

let parse_qualification
    ?differential_summary
    ~summary_fields
    ~operation_mapping
    ~fixture_fields
    ~failure_cases
    () =
  let* () =
    require_string
      "determinism_qualification_corpus"
      "type"
      summary_fields
  in
  let* () = require_string "emitted" "status" summary_fields in
  let* operation_mapping =
    match operation_mapping with
    | Some value -> Ok value
    | None -> Error (Corpus_error "missing operation mapping")
  in
  let* mapping_fields = assoc "operation mapping" operation_mapping in
  let* operation_values = list_field "operations" mapping_fields in
  let* fixture_values = list_field "cases" fixture_fields in
  let* summary_fixture_count = int_field "fixture_count" summary_fields in
  let* summary_failure_count =
    optional_int_field "failure_case_count" summary_fields
  in
  let* fixture_failure_count =
    optional_int_field "failure_case_count" fixture_fields
  in
  let* cutpoint_count =
    match optional_int_field "schedule_cutpoint_count" summary_fields with
    | Ok (Some count) -> Ok count
    | Ok None -> int_field ("b" ^ "onsai_cutpoint_count") summary_fields
    | Error error -> Error error
  in
  let* case_count = int_field "case_count" fixture_fields in
  let* recommendation_summary =
    optional_field "recommendation_summary" summary_fields
  in
  let* actual_failure_count = failure_case_count failure_cases in
  if summary_fixture_count <> case_count then
    Error
      (Corpus_error
         (Printf.sprintf
            "fixture count mismatch: summary %d corpus %d"
            summary_fixture_count
            case_count))
  else
    let declared_failure_count =
      match summary_failure_count with
      | Some count -> count
      | None ->
        (match fixture_failure_count with
         | Some count -> count
         | None -> actual_failure_count)
    in
    if declared_failure_count <> actual_failure_count then
      Error
        (Corpus_error
           (Printf.sprintf
              "failure case count mismatch: declared %d corpus %d"
              declared_failure_count
              actual_failure_count))
    else
      let* operations = parse_list parse_operation [] operation_values in
      let* fixtures = parse_list parse_qualification_fixture [] fixture_values in
      let* p0_worklist = build_p0_from_operations operations fixtures in
      let* differential_reports = parse_differential_summary differential_summary in
      let selected_token_changes, first_divergent_cutpoint =
        match recommendation_summary with
        | Some (`Assoc fields) ->
          let selected =
            match
              optional_int_field
                "selected_token_changes_under_q16_16_candidate"
                fields
            with
            | Ok value -> value
            | Error _ -> None
          in
          let divergent =
            match optional_string_field "first_divergent_cutpoint" fields with
            | Ok value -> value
            | Error _ -> None
          in
          selected, divergent
        | _ -> q16_16_selected differential_reports, None
      in
      Ok {
        corpus_type = "determinism_qualification_corpus";
        operation_count = Some (List.length operations);
        fixture_count = summary_fixture_count;
        failure_case_count = actual_failure_count;
        cutpoint_count;
        logit_cutpoint_count = None;
        selected_token_changes_under_q16_16_candidate =
          selected_token_changes;
        first_divergent_cutpoint;
        differential_reports;
        p0_worklist;
      }

let parse_ingestion
    ?differential_summary
    ~summary_fields
    ~fixture_fields
    ~failure_cases
    () =
  let* () =
    require_string "determinism_ingestion_corpus" "type" summary_fields
  in
  let* () = require_string "emitted" "status" summary_fields in
  let* () =
    require_string
      "p0_litenode_ingestion_fixture_pack"
      "type"
      fixture_fields
  in
  let* summary_fixture_count = int_field "fixture_count" summary_fields in
  let* pack_fixture_count = int_field "fixture_count" fixture_fields in
  let* summary_failure_count = int_field "failure_case_count" summary_fields in
  let* cutpoint_count = int_field "differential_cutpoint_count" summary_fields in
  let* logit_cutpoint_count =
    optional_int_field "logit_cutpoint_count" summary_fields
  in
  let* fixture_values = list_field "fixtures" fixture_fields in
  let* actual_failure_count = failure_case_count failure_cases in
  if summary_fixture_count <> pack_fixture_count then
    Error
      (Corpus_error
         (Printf.sprintf
            "fixture count mismatch: summary %d pack %d"
            summary_fixture_count
            pack_fixture_count))
  else if summary_failure_count <> actual_failure_count then
    Error
      (Corpus_error
         (Printf.sprintf
            "failure case count mismatch: summary %d corpus %d"
            summary_failure_count
            actual_failure_count))
  else
    let* fixtures = parse_list parse_ingestion_fixture [] fixture_values in
    let* p0_worklist = build_p0_from_fixtures fixtures in
    let* differential_reports = parse_differential_summary differential_summary in
    Ok {
      corpus_type = "determinism_ingestion_corpus";
      operation_count = None;
      fixture_count = summary_fixture_count;
      failure_case_count = actual_failure_count;
      cutpoint_count;
      logit_cutpoint_count;
      selected_token_changes_under_q16_16_candidate =
        q16_16_selected differential_reports;
      first_divergent_cutpoint = None;
      differential_reports;
      p0_worklist;
    }

let of_json
    ?operation_mapping
    ?differential_summary
    ~summary
    ~fixture_corpus
    ~failure_cases
    () =
  let* summary_fields = assoc "summary" summary in
  let* fixture_fields = assoc "fixture corpus" fixture_corpus in
  let* corpus_type = string_field "type" summary_fields in
  match corpus_type with
  | "determinism_qualification_corpus" ->
    parse_qualification
      ?differential_summary
      ~summary_fields
      ~operation_mapping
      ~fixture_fields
      ~failure_cases
      ()
  | "determinism_ingestion_corpus" ->
    parse_ingestion
      ?differential_summary
      ~summary_fields
      ~fixture_fields
      ~failure_cases
      ()
  | _ -> Error (Corpus_error ("unsupported corpus type: " ^ corpus_type))

let p0_status_json (status : p0_status) =
  `Assoc [
    "schedule_operation", `String status.schedule_operation;
    "current_litenode_opcode", `String status.current_litenode_opcode;
    "recommendation", `String status.recommendation;
    "next_action", `String status.next_action;
  ]

let differential_json (status : differential_status) =
  `Assoc [
    "candidate", `String status.candidate;
    "selected_token_changes", `Int status.selected_token_changes;
    "top_k_order_changes", `Int status.top_k_order_changes;
  ]

let optional_int_json = function
  | None -> `Null
  | Some value -> `Int value

let optional_string_json = function
  | None -> `Null
  | Some value -> `String value

let to_json corpus =
  `Assoc [
    "status", `String "accepted";
    "diagnostic_only", `Bool true;
    "corpus_type", `String corpus.corpus_type;
    "operation_count", optional_int_json corpus.operation_count;
    "fixture_count", `Int corpus.fixture_count;
    "failure_case_count", `Int corpus.failure_case_count;
    "cutpoint_count", `Int corpus.cutpoint_count;
    "logit_cutpoint_count", optional_int_json corpus.logit_cutpoint_count;
    "selected_token_changes_under_q16_16_candidate",
    optional_int_json corpus.selected_token_changes_under_q16_16_candidate;
    "first_divergent_cutpoint",
    optional_string_json corpus.first_divergent_cutpoint;
    "differential_reports",
    `List (List.map differential_json corpus.differential_reports);
    "p0_worklist", `List (List.map p0_status_json corpus.p0_worklist);
  ]
