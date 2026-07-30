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

type t = {
  operation_count : int;
  fixture_count : int;
  failure_case_count : int;
  bonsai_cutpoint_count : int;
  selected_token_changes_under_q16_16_candidate : int option;
  first_divergent_cutpoint : string option;
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
}

let error_message = function
  | Json_error message -> "invalid determinism corpus json: " ^ message
  | Corpus_error message -> "invalid determinism corpus: " ^ message

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
  match string_field name fields with
  | Error error -> Error error
  | Ok actual when String.equal actual expected -> Ok ()
  | Ok actual ->
    Error
      (Corpus_error
         (Printf.sprintf
            "field %s expected %s actual %s"
            name
            expected
            actual))

let parse_operation value =
  match assoc "operation" value with
  | Error error -> Error error
  | Ok fields ->
    (match
       string_field "schedule_operation" fields,
       string_field "current_litenode_opcode" fields,
       string_field "recommendation" fields
     with
     | Ok schedule_operation, Ok current_litenode_opcode, Ok recommendation ->
       Ok { schedule_operation; current_litenode_opcode; recommendation }
     | Error error, _, _
     | _, Error error, _
     | _, _, Error error -> Error error)

let parse_fixture_case value =
  match assoc "fixture case" value with
  | Error error -> Error error
  | Ok fields ->
    match string_field "primitive" fields with
    | Ok primitive -> Ok { primitive }
    | Error error -> Error error

let rec parse_list parse acc = function
  | [] -> Ok (List.rev acc)
  | value :: rest ->
    (match parse value with
     | Error error -> Error error
     | Ok parsed -> parse_list parse (parsed :: acc) rest)

let operation_by_name operations name =
  List.find_opt
    (fun operation -> String.equal operation.schedule_operation name)
    operations

let has_fixture cases primitive =
  List.exists (fun fixture -> String.equal fixture.primitive primitive) cases

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
  | _ ->
    "classify recommendation before VM work"

let p0_requirements = [
  ("q1_g128_projection", "q1_g128_projection");
  ("rmsnorm_explicit_epsilon", "rmsnorm_fp_eps");
  ("l2norm_explicit_epsilon", "l2norm_fp_eps");
  ("softmax", "softmax_fp");
  ("gated_delta_rule", "gated_delta_rule_fp");
]

let build_p0_worklist operations fixture_cases =
  let rec loop acc = function
    | [] -> Ok (List.rev acc)
    | (operation_name, fixture_primitive) :: rest ->
      (match operation_by_name operations operation_name with
       | None ->
         Error
           (Corpus_error
              ("missing P0 operation mapping: " ^ operation_name))
       | Some operation ->
         if not (has_fixture fixture_cases fixture_primitive) then
           Error
             (Corpus_error
                ("missing P0 fixture primitive: " ^ fixture_primitive))
         else
           let status = {
             schedule_operation = operation.schedule_operation;
             current_litenode_opcode = operation.current_litenode_opcode;
             recommendation = operation.recommendation;
             next_action = next_action operation.recommendation;
           } in
           loop (status :: acc) rest)
  in
  loop [] p0_requirements

let failure_case_count = function
  | `List cases -> Ok (List.length cases)
  | _ -> Error (Json_error "failure cases must be a list")

let ( let* ) result f =
  match result with
  | Ok value -> f value
  | Error error -> Error error

let of_json ~summary ~operation_mapping ~fixture_corpus ~failure_cases =
  let* summary_fields = assoc "summary" summary in
  let* mapping_fields = assoc "operation mapping" operation_mapping in
  let* fixture_fields = assoc "fixture corpus" fixture_corpus in
  let* () =
    require_string
      "determinism_qualification_corpus"
      "type"
      summary_fields
  in
  let* () = require_string "emitted" "status" summary_fields in
  let* operation_values = list_field "operations" mapping_fields in
  let* fixture_values = list_field "cases" fixture_fields in
  let* summary_fixture_count = int_field "fixture_count" summary_fields in
  let* summary_failure_count =
    optional_int_field "failure_case_count" summary_fields
  in
  let* fixture_failure_count =
    optional_int_field "failure_case_count" fixture_fields
  in
  let* bonsai_cutpoint_count = int_field "bonsai_cutpoint_count" summary_fields in
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
    let* fixture_cases = parse_list parse_fixture_case [] fixture_values in
    let* p0_worklist = build_p0_worklist operations fixture_cases in
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
      | _ -> None, None
    in
    Ok {
      operation_count = List.length operations;
      fixture_count = summary_fixture_count;
      failure_case_count = actual_failure_count;
      bonsai_cutpoint_count;
      selected_token_changes_under_q16_16_candidate =
        selected_token_changes;
      first_divergent_cutpoint;
      p0_worklist;
    }

let p0_status_json (status : p0_status) =
  `Assoc [
    "schedule_operation", `String status.schedule_operation;
    "current_litenode_opcode", `String status.current_litenode_opcode;
    "recommendation", `String status.recommendation;
    "next_action", `String status.next_action;
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
    "operation_count", `Int corpus.operation_count;
    "fixture_count", `Int corpus.fixture_count;
    "failure_case_count", `Int corpus.failure_case_count;
    "bonsai_cutpoint_count", `Int corpus.bonsai_cutpoint_count;
    "selected_token_changes_under_q16_16_candidate",
    optional_int_json corpus.selected_token_changes_under_q16_16_candidate;
    "first_divergent_cutpoint",
    optional_string_json corpus.first_divergent_cutpoint;
    "p0_worklist", `List (List.map p0_status_json corpus.p0_worklist);
  ]
