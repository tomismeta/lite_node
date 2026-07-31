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

let schema =
  "octra.inference.conformance.result-signature.v2"

let fail message =
  failwith message

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

let opt_string_field name fields =
  match field name fields with
  | Some (`String value) -> Some value
  | Some `Null
  | None -> None
  | _ -> fail ("invalid string field: " ^ name)

let string_list_field name fields =
  match field name fields with
  | Some (`List values) ->
    List.map
      (function
        | `String value -> value
        | _ -> fail ("invalid string list field: " ^ name))
      values
  | _ -> fail ("missing list field: " ^ name)

let json_field_or_null name fields =
  match field name fields with
  | Some value -> value
  | None -> `Null

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
      "ingress_rejection_authority",
      `String (string_field "ingress_rejection_authority" fields);
      "mutation_shape_status",
      `String (string_field "mutation_shape_status" fields);
      "mutation_shape_blockers",
      `List
        (List.map
           (fun blocker -> `String blocker)
           (string_list_field "mutation_shape_blockers" fields));
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

let signature_json results =
  results
  |> List.map result_signature
  |> List.sort compare
  |> fun values -> Yojson.Safe.to_string (`List values)
