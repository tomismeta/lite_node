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

let check label condition =
  if not condition then failwith label

let hex_root char =
  String.make 64 char

let assoc_value name fields =
  match List.assoc_opt name fields with
  | Some value -> value
  | None -> failwith ("missing json field: " ^ name)

let string_value name fields =
  match assoc_value name fields with
  | `String value -> value
  | _ -> failwith ("json field must be a string: " ^ name)

let string_list_value name fields =
  match assoc_value name fields with
  | `List values ->
    List.map
      (function
        | `String value -> value
        | _ -> failwith ("json field must be a string list: " ^ name))
      values
  | _ -> failwith ("json field must be a string list: " ^ name)

let read_all input =
  let buffer = Buffer.create 4096 in
  (try
     while true do
       Buffer.add_string buffer (input_line input);
       Buffer.add_char buffer '\n'
     done
   with End_of_file -> ());
  Buffer.contents buffer

let write_json path json =
  let output = open_out_bin path in
  Fun.protect
    ~finally:(fun () -> close_out_noerr output)
    (fun () -> output_string output (Yojson.Safe.to_string json))

let tool_path () =
  let candidates =
    [
      "_build/default/tools/inference_conformance_matrix.exe";
      "../tools/inference_conformance_matrix.exe";
    ]
  in
  match List.find_opt Sys.file_exists candidates with
  | Some path -> path
  | None -> failwith "missing inference_conformance_matrix.exe"

let run_matrix ?(args = []) reports =
  let command =
    String.concat
      " "
      ((Filename.quote (tool_path ()) :: args)
       @ List.concat
           (List.map
              (fun path -> ["--runner-report"; Filename.quote path])
              reports))
  in
  let input = Unix.open_process_in command in
  let raw = read_all input in
  let status = Unix.close_process_in input in
  let code =
    match status with
    | Unix.WEXITED code -> code
    | Unix.WSIGNALED signal -> 128 + signal
    | Unix.WSTOPPED signal -> 128 + signal
  in
  code, Yojson.Safe.from_string raw

let platform ?runner_sha system machine =
  let fields =
    [
      "ocaml_version", `String "4.14.2";
      "os_type", `String "Unix";
      "system_name", `String system;
      "system_release", `String "1.0";
      "machine", `String machine;
      "word_size", `Int 64;
      "big_endian", `Bool false;
      "backend_type", `String "native";
    ]
  in
  let fields =
    match runner_sha with
    | Some sha -> fields @ ["runner_executable_sha256", `String sha]
    | None -> fields
  in
  `Assoc fields

let executable_mutation name target fields =
  `Assoc (["mutation", `String name; "target", `String target] @ fields)

let failure_case
    ?(case = "nonfinite_input_nan")
    ?(expected = "reject_before_write")
    ?(executable_mutations =
      [
        executable_mutation
          "replace_first_f64_input_cell"
          "lhs"
          ["value_bits", `Intlit "9221120237041090560"];
      ])
    ?(observed = "vm_rejected")
    ?(ingress_rejection_authority = "not_applicable")
    ?(mutation_shape_status = "accepted")
    ?(mutation_shape_blockers = [])
    ?(observed_effort = 200)
    ?(finite_spans = `List [])
    ?(active_finite_spans = `List [])
    ?snapshot_sha
    () =
  let snapshot_output_status =
    match snapshot_sha with
    | None -> "not_required"
    | Some _ -> "matched"
  in
  let snapshot_output =
    match snapshot_sha with
    | None -> `Assoc ["status", `String "not_required"]
    | Some observed_sha ->
      `Assoc [
        "status", `String "matched";
        "base_address", `Int 20000;
        "length_f64_cells", `Int 6;
        "expected_length_f64_cells", `Int 6;
        "expected_sha256", `String (hex_root 'a');
        "observed_sha256", `String observed_sha;
      ]
  in
  `Assoc [
    "opcode", `String "LINEAR_Q1_G128_FP";
    "case", `String case;
    "expected", `String expected;
    "executable_mutations", `List executable_mutations;
    "status", `String "accepted";
    "counted", `Bool true;
    "observed", `String observed;
    "ingress_rejection_authority", `String ingress_rejection_authority;
    "mutation_shape_status", `String mutation_shape_status;
    "mutation_shape_blockers",
    `List (List.map (fun blocker -> `String blocker) mutation_shape_blockers);
    "observed_effort", `Int observed_effort;
    "unchanged_status", `String "matched";
    "changed_status", `String "not_changed";
    "finite_status", `String "finite";
    "active_changed_status", `String "not_changed";
    "active_finite_status", `String "finite";
    "snapshot_output_status", `String snapshot_output_status;
    "snapshot_output", snapshot_output;
    "unchanged_spans",
    `List [
      `Assoc [
        "name", `String "output";
        "base_address", `Int 10000;
        "length_f64_cells", `Int 6;
        "before_sha256", `String (hex_root 'a');
        "after_sha256", `String (hex_root 'a');
        "unchanged", `Bool true;
      ];
    ];
    "finite_spans", finite_spans;
    "active_changed_spans", `List [];
    "active_finite_spans", active_finite_spans;
  ]

let finite_span ?(finite = true) name base cells =
  `Assoc [
    "name", `String name;
    "base_address", `Int base;
    "length_f64_cells", `Int cells;
    "finite", `Bool finite;
  ]

let q1_required_failure_expectations =
  [
    "nonfinite_input_nan", "reject_before_write";
    "nonfinite_input_infinity", "reject_before_write";
    "output_input_aliasing", "accept_from_snapshot";
    "partial_output_input_aliasing", "accept_from_snapshot";
    "k_not_multiple_of_128", "reject_before_write";
    "bad_q1_owner_length", "reject_before_write";
    "negative_byte_offset", "reject_before_write";
    "byte_offset_out_of_bounds", "reject_before_write";
    "byte_offset_truncated_span", "reject_before_write";
    "nonfinite_fp16_scale", "reject_before_write";
    "lower_effort_limit", "reject_before_write";
  ]

let q1_failure_contract_json expectations =
  `Assoc [
    "opcode", `String "LINEAR_Q1_G128_FP";
    "expectations",
    `List
      (List.map
         (fun (case, expected_prefix) ->
            `Assoc [
              "case", `String case;
              "expected_prefix", `String expected_prefix;
            ])
         expectations);
  ]

let executable_mutation_for_case = function
  | "nonfinite_input_nan" ->
    executable_mutation
      "replace_first_f64_input_cell"
      "lhs"
      ["value_bits", `Intlit "9221120237041090560"]
  | "nonfinite_input_infinity" ->
    executable_mutation
      "replace_first_f64_input_cell"
      "lhs"
      ["value_bits", `Intlit "9218868437227405312"]
  | "output_input_aliasing" ->
    executable_mutation
      "set_output_base_to_first_input_base"
      "output.base_address"
      []
  | "partial_output_input_aliasing" ->
    executable_mutation
      "set_output_base_to_first_input_base_plus"
      "output.base_address"
      ["offset_cells", `Int 1]
  | "k_not_multiple_of_128" ->
    executable_mutation
      "set_scalar_param"
      "parameter_addresses_and_scalar_params.values.k"
      ["value", `Int 127]
  | "bad_q1_owner_length" ->
    executable_mutation
      "truncate_input_manifest"
      "q1_owner"
      ["truncate_bytes", `Int 1]
  | "negative_byte_offset" ->
    executable_mutation
      "set_scalar_param"
      "parameter_addresses_and_scalar_params.values.byte_offset"
      ["value", `Int (-1)]
  | "byte_offset_out_of_bounds" ->
    executable_mutation
      "set_scalar_param"
      "parameter_addresses_and_scalar_params.values.byte_offset"
      ["value", `Int 1_000_000]
  | "byte_offset_truncated_span" ->
    executable_mutation
      "set_scalar_param"
      "parameter_addresses_and_scalar_params.values.byte_offset"
      ["value", `Int 1]
  | "nonfinite_fp16_scale" ->
    executable_mutation
      "replace_q1_scale_bits"
      "q1_owner[0..2]"
      ["value_hex_le", `String "007c"]
  | "lower_effort_limit" ->
    executable_mutation
      "lower_effort_limit"
      "effort"
      ["value", `Int 201]
  | case -> failwith ("missing executable mutation for case: " ^ case)

let q1_required_failure_cases () =
  List.map
    (fun (case, expected) ->
       failure_case
         ~case
         ~expected
         ~executable_mutations:[executable_mutation_for_case case]
         ())
    q1_required_failure_expectations

let replace_failure_case replacement cases =
  match replacement with
  | `Assoc replacement_fields ->
    let replacement_case = string_value "case" replacement_fields in
    List.map
      (function
        | `Assoc fields
          when String.equal (string_value "case" fields) replacement_case ->
          replacement
        | value -> value)
      cases
  | _ -> failwith "replacement failure case must be an object"

let result
    ?(opcode = "LINEAR_Q1_G128_FP")
    ?(observed_opcode_effort = 201)
    ?(opcode_effort_match = true)
    ?(profile_root_status = "matched")
    ?(vm_semantics_status = "matched")
    ?(abi_declaration_status = "matched")
    ?(executable_abi_status = "matched")
    ?(required_failure_contract_status = "accepted")
    ?required_failure_contract_blockers
    ?(include_required_contract_payload = true)
    ?required_failure_contract_payload
    ?failure
    () =
  let failure_cases =
    match failure, String.equal opcode "LINEAR_Q1_G128_FP" with
    | Some failure, true ->
      replace_failure_case failure (q1_required_failure_cases ())
    | Some failure, false -> [failure]
    | None, true -> q1_required_failure_cases ()
    | None, false -> [failure_case ()]
  in
  let required_failure_contract_blockers =
    match required_failure_contract_blockers with
    | Some blockers -> blockers
    | None ->
      if
        String.equal required_failure_contract_status "accepted"
        || String.equal required_failure_contract_status "not_applicable"
      then
        []
      else
        ["q1_failure_case_missing_nonfinite_fp16_scale"]
  in
  let required_failure_contract_fields =
    [
      "status", `String required_failure_contract_status;
      "blockers",
      `List
        (List.map
           (fun blocker -> `String blocker)
           required_failure_contract_blockers);
    ]
  in
  let required_failure_contract_fields =
    if include_required_contract_payload then
      ("contract",
       (match required_failure_contract_payload with
        | Some payload -> payload
        | None when String.equal opcode "LINEAR_Q1_G128_FP" ->
          q1_failure_contract_json q1_required_failure_expectations
        | None ->
          `Null))
      :: required_failure_contract_fields
    else
      required_failure_contract_fields
  in
  `Assoc [
    "opcode", `String opcode;
    "profile_root_binding",
    `Assoc [
      "status", `String profile_root_status;
      "classification",
      `String
        (if String.equal profile_root_status "matched" then
           "none"
         else
           "profile_root_mismatch");
      "numerical_profile_root", `String (hex_root '3');
      "profile_root",
      `String
        (if String.equal profile_root_status "matched" then
           hex_root '3'
         else
           hex_root '8');
    ];
    "vm_semantics_binding",
    `Assoc [
      "status", `String vm_semantics_status;
      "classification",
      `String
        (if String.equal vm_semantics_status "matched" then
           "none"
         else
           "vm_semantics_root_mismatch");
      "vm_semantics_root", `String (hex_root '5');
      "litenode_vm_semantics_root",
      `String
        (if String.equal vm_semantics_status "matched" then
           hex_root '5'
         else
           hex_root '7');
    ];
    "abi_declaration_binding",
    `Assoc [
      "status", `String abi_declaration_status;
      "classification",
      `String
        (if String.equal abi_declaration_status "matched" then
           "none"
         else
           "abi_declaration_mismatch");
      "session_abi_root", `String (hex_root '6');
      "litenode_session_abi_root", `String (hex_root '6');
      "evidence_scope", `String "template_declaration";
      "entrypoint", `String "advance";
      "label", `Int 100;
      "output_base_register", `String "r0";
      "output_count_register", `String "r1";
      "output_count_unit", `String "cells";
      "request_input_root_cell", `Int 1000;
      "r0", `Int 10000;
      "r1", `Int 6;
      "blockers",
      `List
        (if String.equal abi_declaration_status "matched" then
           []
        else
           [`String "r1_output_count_mismatch"]);
    ];
    "executable_abi_binding",
    `Assoc [
      "status", `String executable_abi_status;
      "expected_r0", `Int 10000;
      "expected_r1", `Int 6;
      "observed_r0", `Int 10000;
      "observed_r1", `Int 6;
      "registers_match", `Bool true;
      "output_payload_status", `String "accepted";
      "output_payload_error", `Null;
      "output_payload_sha256", `String (hex_root '9');
    ];
    "status", `String "accepted";
    "vm_run", `String "accepted";
    "output_status", `String "matched";
    "expected_effort", `Int 202;
    "observed_effort", `Int 202;
    "expected_program_effort", `Int 202;
    "observed_program_effort", `Int 202;
    "program_effort_match", `Bool true;
    "expected_opcode_effort", `Int 201;
    "observed_opcode_effort", `Int observed_opcode_effort;
    "opcode_effort_match", `Bool opcode_effort_match;
    "effort_match", `Bool true;
    "strict_effort", `Bool true;
    "q1_contract_shape",
    (if String.equal opcode "LINEAR_Q1_G128_FP" then
       `Assoc [
         "m", `Int 1;
         "k", `Int 128;
         "n", `Int 6;
         "byte_offset", `Int 0;
         "lhs_cells", `Int 128;
         "output_cells", `Int 6;
         "q1_owner_source_bytes", `Int 108;
         "q1_required_owner_bytes", `Int 108;
         "expected_effort", `Int 202;
       ]
     else
       `Null);
    "required_failure_case_contract",
    `Assoc required_failure_contract_fields;
    "subspans",
    `List [
      `Assoc [
        "name", `String "expected";
        "base_address", `Int 10000;
        "length_f64_cells", `Int 6;
        "expected_sha256", `String (hex_root 'a');
        "observed_sha256", `String (hex_root 'a');
        "expected_root", `String (hex_root 'b');
        "observed_root", `String (hex_root 'b');
        "root_matched", `Bool true;
        "matched", `Bool true;
      ];
    ];
    "failure_cases", `List failure_cases;
  ]

let report
    ?runner_sha
    ?(opcode = "LINEAR_Q1_G128_FP")
    ?(corpus = hex_root 'd')
    ?observed_opcode_effort
    ?opcode_effort_match
    ?(profile_root_status = "matched")
    ?(profile_gate_status = "accepted")
    ?(vm_semantics_status = "matched")
    ?(vm_semantics_gate_status = "accepted")
    ?(abi_declaration_status = "matched")
    ?(abi_gate_status = "accepted")
    ?(executable_abi_status = "matched")
    ?(required_failure_contract_status = "accepted")
    ?required_failure_contract_blockers
    ?include_required_contract_payload
    ?required_failure_contract_payload
    ?(validator_readiness_blockers = ["cross_platform_conformance_missing"])
    ?failure
    system
    machine =
  `Assoc [
    "status", `String "accepted";
    "execution_status", `String "accepted";
    "execution_mode", `String "positive_template_vm_execution";
    "platform", platform ?runner_sha system machine;
    "selected_opcodes", `List [`String opcode];
    "template_corpus_root", `String corpus;
    "profile_catalog_root", `String (hex_root 'c');
    "failure_case_gate", `Assoc ["status", `String "accepted"];
    "profile_root_binding_gate", `Assoc ["status", `String profile_gate_status];
    "vm_semantics_binding_gate", `Assoc ["status", `String vm_semantics_gate_status];
    "abi_declaration_binding_gate", `Assoc ["status", `String abi_gate_status];
    "validator_readiness_gate",
    `Assoc [
      "status", `String "rejected";
      "blockers", `List (List.map (fun blocker -> `String blocker) validator_readiness_blockers);
    ];
    "results",
    `List [
      result
        ~opcode
        ?observed_opcode_effort
        ?opcode_effort_match
        ~profile_root_status
        ~vm_semantics_status
        ~abi_declaration_status
        ~executable_abi_status
        ~required_failure_contract_status
        ?required_failure_contract_blockers
        ?include_required_contract_payload
        ?required_failure_contract_payload
        ?failure
        ();
    ];
  ]

let write_report dir name json =
  let path = Filename.concat dir name in
  write_json path json;
  path

let replace_assoc_field name value = function
  | `Assoc fields ->
    `Assoc
      ((name, value)
       :: List.filter (fun (key, _) -> not (String.equal key name)) fields)
  | _ -> failwith "report must be object"

let remove_result_field name = function
  | `Assoc fields ->
    let results =
      match assoc_value "results" fields with
      | `List results ->
        `List
          (List.map
             (function
               | `Assoc result_fields ->
                 `Assoc
                   (List.filter
                      (fun (key, _) -> not (String.equal key name))
                      result_fields)
               | value -> value)
             results)
      | _ -> failwith "results must be a list"
    in
    replace_assoc_field "results" results (`Assoc fields)
  | _ -> failwith "report must be object"

let replace_result_field name value = function
  | `Assoc fields ->
    let results =
      match assoc_value "results" fields with
      | `List results ->
        `List
          (List.map
             (function
               | `Assoc result_fields ->
                 `Assoc
                   ((name, value)
                    :: List.filter
                         (fun (key, _) -> not (String.equal key name))
                         result_fields)
               | value -> value)
             results)
      | _ -> failwith "results must be a list"
    in
    replace_assoc_field "results" results (`Assoc fields)
  | _ -> failwith "report must be object"

let replace_q1_contract_shape_field name value = function
  | `Assoc fields as report ->
    let results =
      match assoc_value "results" fields with
      | `List results ->
        `List
          (List.map
             (function
               | `Assoc result_fields ->
                 let shape =
                   match assoc_value "q1_contract_shape" result_fields with
                   | `Assoc shape_fields ->
                     `Assoc
                       ((name, value)
                        :: List.filter
                             (fun (key, _) -> not (String.equal key name))
                             shape_fields)
                   | _ -> failwith "q1_contract_shape must be an object"
                 in
                 `Assoc
                   (("q1_contract_shape", shape)
                    :: List.filter
                         (fun (key, _) -> not (String.equal key "q1_contract_shape"))
                         result_fields)
               | value -> value)
             results)
      | _ -> failwith "results must be a list"
    in
    replace_assoc_field "results" results report
  | _ -> failwith "report must be object"

let replace_result_subspan_field name value = function
  | `Assoc fields as report ->
    let results =
      match assoc_value "results" fields with
      | `List results ->
        `List
          (List.map
             (function
               | `Assoc result_fields ->
                 let subspans =
                   match assoc_value "subspans" result_fields with
                   | `List subspans ->
                     `List
                       (List.map
                          (function
                            | `Assoc subspan_fields ->
                              `Assoc
                                ((name, value)
                                 :: List.filter
                                      (fun (key, _) -> not (String.equal key name))
                                      subspan_fields)
                            | value -> value)
                          subspans)
                   | _ -> failwith "subspans must be a list"
                 in
                 `Assoc
                   (("subspans", subspans)
                    :: List.filter
                         (fun (key, _) -> not (String.equal key "subspans"))
                         result_fields)
               | value -> value)
             results)
      | _ -> failwith "results must be a list"
    in
    replace_assoc_field "results" results report
  | _ -> failwith "report must be object"

let remove_failure_field name = function
  | `Assoc fields ->
    let results =
      match assoc_value "results" fields with
      | `List results ->
        `List
          (List.map
             (function
               | `Assoc result_fields ->
                 let failure_cases =
                   match assoc_value "failure_cases" result_fields with
                   | `List failures ->
                     `List
                       (List.map
                          (function
                            | `Assoc failure_fields ->
                              `Assoc
                                (List.filter
                                   (fun (key, _) -> not (String.equal key name))
                                   failure_fields)
                            | value -> value)
                          failures)
                   | _ -> failwith "failure_cases must be a list"
                 in
                 `Assoc
                   (("failure_cases", failure_cases)
                    :: List.filter
                         (fun (key, _) -> not (String.equal key "failure_cases"))
                         result_fields)
               | value -> value)
             results)
      | _ -> failwith "results must be a list"
    in
    replace_assoc_field "results" results (`Assoc fields)
  | _ -> failwith "report must be object"

let remove_failure_case case = function
  | `Assoc fields ->
    let results =
      match assoc_value "results" fields with
      | `List results ->
        `List
          (List.map
             (function
               | `Assoc result_fields ->
                 let failure_cases =
                   match assoc_value "failure_cases" result_fields with
                   | `List failures ->
                     `List
                       (List.filter
                          (function
                            | `Assoc failure_fields ->
                              not
                                (String.equal
                                   (string_value "case" failure_fields)
                                   case)
                            | _ -> true)
                          failures)
                   | _ -> failwith "failure_cases must be a list"
                 in
                 `Assoc
                   (("failure_cases", failure_cases)
                    :: List.filter
                         (fun (key, _) -> not (String.equal key "failure_cases"))
                         result_fields)
               | value -> value)
             results)
      | _ -> failwith "results must be a list"
    in
    replace_assoc_field "results" results (`Assoc fields)
  | _ -> failwith "report must be object"

let with_temp_dir f =
  let dir = Filename.temp_file "octra-matrix-test" "" in
  Sys.remove dir;
  Unix.mkdir dir 0o700;
  Fun.protect
    ~finally:(fun () ->
      Sys.readdir dir
      |> Array.iter (fun name -> Sys.remove (Filename.concat dir name));
      Unix.rmdir dir)
    (fun () -> f dir)

let blockers fields =
  string_list_value "blockers" fields

let check_matrix_accepts_bound_reports () =
  with_temp_dir (fun dir ->
    let a =
      write_report
        dir
        "a.cjson"
        (report ~runner_sha:(hex_root '1') "Darwin" "arm64")
    in
    let b =
      write_report
        dir
        "b.cjson"
        (report ~runner_sha:(hex_root '2') "Linux" "x86_64")
    in
    let code, json = run_matrix [a; b] in
    check "accepted matrix exits zero" (code = 0);
    match json with
    | `Assoc fields ->
      check "matrix accepted" (String.equal (string_value "status" fields) "accepted");
      check
        "matrix carries q1 opcode"
        (string_list_value "result_opcodes" fields = ["LINEAR_Q1_G128_FP"]);
      check
        "matrix carries result signature schema"
        (String.equal
           (string_value "result_signature_schema" fields)
           "octra.inference.conformance.result-signature.v5");
      check
        "matrix carries runner hashes"
        (List.length (string_list_value "runner_executable_sha256s" fields) = 2)
    | _ -> failwith "matrix output must be object")

let check_matrix_rejects_reused_runner_hash () =
  with_temp_dir (fun dir ->
    let runner_sha = hex_root '1' in
    let a =
      write_report
        dir
        "a.cjson"
        (report ~runner_sha "Darwin" "arm64")
    in
    let b =
      write_report
        dir
        "b.cjson"
        (report ~runner_sha "Linux" "x86_64")
    in
    let code, json = run_matrix [a; b] in
    check "reused runner hash matrix exits nonzero" (code = 1);
    match json with
    | `Assoc fields ->
      check
        "reused runner hash blocker"
        (List.mem "insufficient_distinct_runner_executables" (blockers fields));
      check
        "runner executable count is one"
        (match assoc_value "distinct_runner_executable_count" fields with
         | `Int count -> count = 1
         | _ -> false)
    | _ -> failwith "matrix output must be object")

let check_matrix_accepts_required_opcode_reports () =
  with_temp_dir (fun dir ->
    let a =
      write_report
        dir
        "a.cjson"
        (report ~runner_sha:(hex_root '1') "Darwin" "arm64")
    in
    let b =
      write_report
        dir
        "b.cjson"
        (report ~runner_sha:(hex_root '2') "Linux" "x86_64")
    in
    let code, json =
      run_matrix ~args:["--opcode"; "LINEAR_Q1_G128_FP"] [a; b]
    in
    check "required opcode matrix exits zero" (code = 0);
    match json with
    | `Assoc fields ->
      check "required opcode matrix accepted"
        (String.equal (string_value "status" fields) "accepted");
      check
        "required opcode coverage accepted"
        (String.equal
           (string_value "opcode_coverage_status" fields)
           "accepted");
      check
        "required opcode recorded"
        (string_list_value "required_opcodes" fields = ["LINEAR_Q1_G128_FP"])
    | _ -> failwith "matrix output must be object")

let check_matrix_rejects_missing_required_opcode () =
  with_temp_dir (fun dir ->
    let a =
      write_report
        dir
        "a.cjson"
        (report
           ~runner_sha:(hex_root '1')
           ~opcode:"RMSNORM_FP_EPS"
           ~required_failure_contract_status:"not_applicable"
           "Darwin"
           "arm64")
    in
    let b =
      write_report
        dir
        "b.cjson"
        (report
           ~runner_sha:(hex_root '2')
           ~opcode:"RMSNORM_FP_EPS"
           ~required_failure_contract_status:"not_applicable"
           "Linux"
           "x86_64")
    in
    let code, json =
      run_matrix ~args:["--opcode"; "LINEAR_Q1_G128_FP"] [a; b]
    in
    check "missing required opcode matrix exits nonzero" (code = 1);
    match json with
    | `Assoc fields ->
      check
        "missing required opcode coverage rejected"
        (String.equal
           (string_value "opcode_coverage_status" fields)
           "rejected");
      check
        "missing required opcode blocker"
        (List.mem "required_opcode_missing" (blockers fields));
      check
        "missing required opcode next blocker"
        (String.equal
           (string_value "next_validator_readiness_blocker" fields)
           "required_opcode_missing")
    | _ -> failwith "matrix output must be object")

let check_matrix_rejects_missing_runner_hash () =
  with_temp_dir (fun dir ->
    let a = write_report dir "a.cjson" (report "Darwin" "arm64") in
    let b =
      write_report
        dir
        "b.cjson"
        (report ~runner_sha:(hex_root '2') "Linux" "x86_64")
    in
    let code, json = run_matrix [a; b] in
    check "missing hash matrix exits nonzero" (code = 1);
    match json with
    | `Assoc fields ->
      check
        "missing hash blocker"
        (List.mem "runner_report_rejected" (blockers fields));
      (match assoc_value "reports" fields with
       | `List (`Assoc report_fields :: _) ->
         check
           "source row blocker"
           (List.mem
              "missing_runner_executable_sha256"
              (blockers report_fields))
       | _ -> failwith "missing report rows")
    | _ -> failwith "matrix output must be object")

let check_matrix_rejects_corpus_mismatch () =
  with_temp_dir (fun dir ->
    let a =
      write_report
        dir
        "a.cjson"
        (report ~runner_sha:(hex_root '1') "Darwin" "arm64")
    in
    let b =
      write_report
        dir
        "b.cjson"
        (report
           ~runner_sha:(hex_root '2')
           ~corpus:(hex_root 'e')
           "Linux"
           "x86_64")
    in
    let code, json = run_matrix [a; b] in
    check "corpus mismatch matrix exits nonzero" (code = 1);
    match json with
    | `Assoc fields ->
      check
        "corpus mismatch blocker"
        (List.mem "template_corpus_mismatch" (blockers fields))
    | _ -> failwith "matrix output must be object")

let check_matrix_rejects_failure_case_mismatch () =
  with_temp_dir (fun dir ->
    let a =
      write_report
        dir
        "a.cjson"
        (report ~runner_sha:(hex_root '1') "Darwin" "arm64")
    in
    let b =
      write_report
        dir
        "b.cjson"
        (report
           ~runner_sha:(hex_root '2')
           ~failure:(failure_case ~observed:"vm_accepted" ~observed_effort:201 ())
           "Linux"
           "x86_64")
    in
    let code, json = run_matrix [a; b] in
    check "failure mismatch matrix exits nonzero" (code = 1);
    match json with
    | `Assoc fields ->
      check
        "failure mismatch blocker"
        (List.mem "result_mismatch_across_platforms" (blockers fields))
    | _ -> failwith "matrix output must be object")

let check_matrix_rejects_failure_mutation_payload_mismatch () =
  with_temp_dir (fun dir ->
    let a =
      write_report
        dir
        "a.cjson"
        (report ~runner_sha:(hex_root '1') "Darwin" "arm64")
    in
    let b =
      write_report
        dir
        "b.cjson"
        (report
           ~runner_sha:(hex_root '2')
           ~failure:
             (failure_case
                ~executable_mutations:
                  [
                    executable_mutation
                      "replace_first_f64_input_cell"
                      "lhs"
                      ["value_bits", `Intlit "9218868437227405312"];
                  ]
                ())
           "Linux"
           "x86_64")
    in
    let code, json = run_matrix [a; b] in
    check "failure mutation payload mismatch exits nonzero" (code = 1);
    match json with
    | `Assoc fields ->
      check
        "failure mutation payload mismatch blocker"
        (List.mem "result_mismatch_across_platforms" (blockers fields))
    | _ -> failwith "matrix output must be object")

let check_matrix_rejects_forged_failure_mutation_payload_shape () =
  with_temp_dir (fun dir ->
    let forged =
      failure_case
        ~case:"nonfinite_input_nan"
        ~executable_mutations:
          [
            executable_mutation
              "replace_first_f64_input_cell"
              "lhs"
              ["value_bits", `Intlit "9218868437227405312"];
          ]
        ()
    in
    let a =
      write_report
        dir
        "a.cjson"
        (report ~runner_sha:(hex_root '1') ~failure:forged "Darwin" "arm64")
    in
    let b =
      write_report
        dir
        "b.cjson"
        (report ~runner_sha:(hex_root '2') ~failure:forged "Linux" "x86_64")
    in
    let code, json = run_matrix [a; b] in
    check "forged mutation payload shape exits nonzero" (code = 1);
    match json with
    | `Assoc fields ->
      check
        "forged mutation payload shape top-level blocker"
        (List.mem "runner_report_rejected" (blockers fields));
      check
        "forged mutation payload shape validator blocker"
        (List.mem
           "required_q1_failure_cases_rejected"
           (string_list_value "validator_readiness_blockers" fields))
    | _ -> failwith "matrix output must be object")

let check_matrix_accepts_nonfinite_fp16_nan_scale_payload () =
  with_temp_dir (fun dir ->
    let nan_scale =
      failure_case
        ~case:"nonfinite_fp16_scale"
        ~executable_mutations:
          [
            executable_mutation
              "replace_q1_scale_bits"
              "q1_owner[0..2]"
              ["value_hex_le", `String "017c"];
          ]
        ()
    in
    let a =
      write_report
        dir
        "a.cjson"
        (report ~runner_sha:(hex_root '1') ~failure:nan_scale "Darwin" "arm64")
    in
    let b =
      write_report
        dir
        "b.cjson"
        (report ~runner_sha:(hex_root '2') ~failure:nan_scale "Linux" "x86_64")
    in
    let code, json = run_matrix [a; b] in
    check "nonfinite fp16 nan scale matrix exits zero" (code = 0);
    match json with
    | `Assoc fields ->
      check "nonfinite fp16 nan scale accepted" (String.equal (string_value "status" fields) "accepted")
    | _ -> failwith "matrix output must be object")

let check_matrix_rejects_partial_alias_payload_outside_lhs () =
  with_temp_dir (fun dir ->
    let forged =
      failure_case
        ~case:"partial_output_input_aliasing"
        ~expected:"accept_from_snapshot_partial"
        ~executable_mutations:
          [
            executable_mutation
              "set_output_base_to_first_input_base_plus"
              "output.base_address"
              ["offset_cells", `Int 128];
          ]
        ()
    in
    let a =
      write_report
        dir
        "a.cjson"
        (report ~runner_sha:(hex_root '1') ~failure:forged "Darwin" "arm64")
    in
    let b =
      write_report
        dir
        "b.cjson"
        (report ~runner_sha:(hex_root '2') ~failure:forged "Linux" "x86_64")
    in
    let code, json = run_matrix [a; b] in
    check "partial alias outside lhs matrix exits nonzero" (code = 1);
    match json with
    | `Assoc fields ->
      check
        "partial alias outside lhs top-level blocker"
        (List.mem "runner_report_rejected" (blockers fields));
      check
        "partial alias outside lhs validator blocker"
        (List.mem
           "required_q1_failure_cases_rejected"
           (string_list_value "validator_readiness_blockers" fields))
    | _ -> failwith "matrix output must be object")

let check_matrix_rejects_missing_failure_mutation_payload () =
  with_temp_dir (fun dir ->
    let a =
      write_report
        dir
        "a.cjson"
        (report ~runner_sha:(hex_root '1') "Darwin" "arm64")
    in
    let b =
      write_report
        dir
        "b.cjson"
        (report ~runner_sha:(hex_root '2') "Linux" "x86_64"
         |> remove_failure_field "executable_mutations")
    in
    let code, json = run_matrix [a; b] in
    check "missing mutation payload matrix exits nonzero" (code = 1);
    match json with
    | `Assoc fields ->
      check
        "missing mutation payload row blocker"
        (List.mem
           "runner_report_rejected"
           (blockers fields));
      check
        "missing mutation payload validator blocker"
        (List.mem
           "missing_failure_case_executable_mutations"
           (string_list_value "validator_readiness_blockers" fields))
    | _ -> failwith "matrix output must be object")

let check_matrix_rejects_missing_required_q1_failure_row () =
  with_temp_dir (fun dir ->
    let a =
      write_report
        dir
        "a.cjson"
        (report ~runner_sha:(hex_root '1') "Darwin" "arm64")
    in
    let b =
      write_report
        dir
        "b.cjson"
        (report ~runner_sha:(hex_root '2') "Linux" "x86_64"
         |> remove_failure_case "lower_effort_limit")
    in
    let code, json = run_matrix [a; b] in
    check "missing required q1 row matrix exits nonzero" (code = 1);
    match json with
    | `Assoc fields ->
      check
        "missing required q1 row top-level blocker"
        (List.mem "runner_report_rejected" (blockers fields));
      check
        "missing required q1 row validator blocker"
        (List.mem
           "required_q1_failure_cases_rejected"
           (string_list_value "validator_readiness_blockers" fields))
    | _ -> failwith "matrix output must be object")

let check_matrix_rejects_failure_snapshot_mismatch () =
  with_temp_dir (fun dir ->
    let a =
      write_report
        dir
        "a.cjson"
        (report
           ~runner_sha:(hex_root '1')
           ~failure:
             (failure_case
                ~case:"output_input_aliasing"
                ~expected:"accept_from_snapshot_exact"
                ~observed:"vm_accepted"
                ~observed_effort:201
                ~snapshot_sha:(hex_root 'a')
                ())
           "Darwin"
           "arm64")
    in
    let b =
      write_report
        dir
        "b.cjson"
        (report
           ~runner_sha:(hex_root '2')
           ~failure:
             (failure_case
                ~case:"output_input_aliasing"
                ~expected:"accept_from_snapshot_exact"
                ~observed:"vm_accepted"
                ~observed_effort:201
                ~snapshot_sha:(hex_root 'b')
                ())
           "Linux"
           "x86_64")
    in
    let code, json = run_matrix [a; b] in
    check "snapshot mismatch matrix exits nonzero" (code = 1);
    match json with
    | `Assoc fields ->
      check
        "snapshot mismatch blocker"
        (List.mem "result_mismatch_across_platforms" (blockers fields))
    | _ -> failwith "matrix output must be object")

let check_matrix_rejects_failure_finite_span_mismatch () =
  with_temp_dir (fun dir ->
    let a =
      write_report
        dir
        "a.cjson"
        (report
           ~runner_sha:(hex_root '1')
           ~failure:
             (failure_case
                ~finite_spans:(`List [finite_span "output" 10000 6])
                ~active_finite_spans:(`List [finite_span "active_output" 10000 6])
                ())
           "Darwin"
           "arm64")
    in
    let b =
      write_report
        dir
        "b.cjson"
        (report
           ~runner_sha:(hex_root '2')
           ~failure:
             (failure_case
                ~finite_spans:(`List [finite_span ~finite:false "output" 10000 6])
                ~active_finite_spans:
                  (`List [finite_span ~finite:false "active_output" 10000 6])
                ())
           "Linux"
           "x86_64")
    in
    let code, json = run_matrix [a; b] in
    check "finite span mismatch matrix exits nonzero" (code = 1);
    match json with
    | `Assoc fields ->
      check
        "finite span mismatch blocker"
        (List.mem "result_mismatch_across_platforms" (blockers fields))
    | _ -> failwith "matrix output must be object")

let check_matrix_rejects_opcode_effort_mismatch () =
  with_temp_dir (fun dir ->
    let a =
      write_report
        dir
        "a.cjson"
        (report ~runner_sha:(hex_root '1') "Darwin" "arm64")
    in
    let b =
      write_report
        dir
        "b.cjson"
        (report
           ~runner_sha:(hex_root '2')
           ~observed_opcode_effort:201
           ~opcode_effort_match:false
           "Linux"
           "x86_64")
    in
    let code, json = run_matrix [a; b] in
    check "opcode effort mismatch matrix exits nonzero" (code = 1);
    match json with
    | `Assoc fields ->
      check
        "opcode effort mismatch top-level blocker"
        (List.mem "runner_report_rejected" (blockers fields));
      check
        "opcode effort mismatch validator blocker"
        (List.mem
           "opcode_effort_mismatch"
           (string_list_value "validator_readiness_blockers" fields))
    | _ -> failwith "matrix output must be object")

let check_matrix_rejects_abi_declaration_binding_mismatch () =
  with_temp_dir (fun dir ->
    let a =
      write_report
        dir
        "a.cjson"
        (report ~runner_sha:(hex_root '1') "Darwin" "arm64")
    in
    let b =
      write_report
        dir
        "b.cjson"
        (report
           ~runner_sha:(hex_root '2')
           ~abi_declaration_status:"unbound"
           ~abi_gate_status:"rejected"
           "Linux"
           "x86_64")
    in
    let code, json = run_matrix [a; b] in
    check "abi mismatch matrix exits nonzero" (code = 1);
    match json with
    | `Assoc fields ->
      check
        "abi mismatch top-level blocker"
        (List.mem "runner_report_rejected" (blockers fields));
      check
        "abi mismatch validator blocker"
        (List.mem
           "abi_declaration_binding_rejected"
           (string_list_value "validator_readiness_blockers" fields))
    | _ -> failwith "matrix output must be object")

let check_matrix_rejects_executable_abi_binding_mismatch () =
  with_temp_dir (fun dir ->
    let a =
      write_report
        dir
        "a.cjson"
        (report ~runner_sha:(hex_root '1') "Darwin" "arm64")
    in
    let b =
      write_report
        dir
        "b.cjson"
        (report
           ~runner_sha:(hex_root '2')
           ~executable_abi_status:"mismatch"
           "Linux"
           "x86_64")
    in
    let code, json = run_matrix [a; b] in
    check "executable ABI mismatch matrix exits nonzero" (code = 1);
    match json with
    | `Assoc fields ->
      check
        "executable ABI mismatch top-level blocker"
        (List.mem "runner_report_rejected" (blockers fields));
      check
        "executable ABI mismatch validator blocker"
        (List.mem
           "executable_abi_binding_mismatch"
           (string_list_value "validator_readiness_blockers" fields))
    | _ -> failwith "matrix output must be object")

let check_matrix_prioritizes_executable_abi_not_run () =
  with_temp_dir (fun dir ->
    let a =
      write_report
        dir
        "a.cjson"
        (report ~runner_sha:(hex_root '1') "Darwin" "arm64")
    in
    let b =
      write_report
        dir
        "b.cjson"
        (report
           ~runner_sha:(hex_root '2')
           ~validator_readiness_blockers:["executable_abi_binding_not_run"]
           "Linux"
           "x86_64")
    in
    let code, json =
      run_matrix ~args:["--require-validator-readiness"] [a; b]
    in
    check "executable ABI not-run readiness exits nonzero" (code = 1);
    match json with
    | `Assoc fields ->
      let readiness_blockers =
        string_list_value "validator_readiness_blockers" fields
      in
      check
        "executable ABI not-run validator blocker"
        (List.mem "executable_abi_binding_not_run" readiness_blockers);
      check
        "executable ABI not-run next blocker"
        (String.equal
           (string_value "next_validator_readiness_blocker" fields)
           "executable_abi_binding_not_run")
    | _ -> failwith "matrix output must be object")

let check_matrix_rejects_profile_root_binding_mismatch () =
  with_temp_dir (fun dir ->
    let a =
      write_report
        dir
        "a.cjson"
        (report ~runner_sha:(hex_root '1') "Darwin" "arm64")
    in
    let b =
      write_report
        dir
        "b.cjson"
        (report
           ~runner_sha:(hex_root '2')
           ~profile_root_status:"unbound"
           ~profile_gate_status:"rejected"
           "Linux"
           "x86_64")
    in
    let code, json = run_matrix [a; b] in
    check "profile root mismatch matrix exits nonzero" (code = 1);
    match json with
    | `Assoc fields ->
      check
        "profile root mismatch top-level blocker"
        (List.mem "runner_report_rejected" (blockers fields));
      check
        "profile root mismatch validator blocker"
        (List.mem
           "profile_root_binding_rejected"
           (string_list_value "validator_readiness_blockers" fields));
      check
        "profile root mismatch row blocker"
        (List.mem
           "profile_root_binding_mismatch"
           (string_list_value "validator_readiness_blockers" fields))
    | _ -> failwith "matrix output must be object")

let check_matrix_rejects_profile_root_result_mismatch_with_gate_accepted () =
  with_temp_dir (fun dir ->
    let a =
      write_report
        dir
        "a.cjson"
        (report ~runner_sha:(hex_root '1') "Darwin" "arm64")
    in
    let b =
      write_report
        dir
        "b.cjson"
        (report
           ~runner_sha:(hex_root '2')
           ~profile_root_status:"unbound"
           "Linux"
           "x86_64")
    in
    let code, json = run_matrix [a; b] in
    check "profile root result mismatch exits nonzero" (code = 1);
    match json with
    | `Assoc fields ->
      let readiness_blockers =
        string_list_value "validator_readiness_blockers" fields
      in
      check
        "profile root result mismatch is isolated"
        (not (List.mem "profile_root_binding_rejected" readiness_blockers));
      check
        "profile root result mismatch blocker"
        (List.mem "profile_root_binding_mismatch" readiness_blockers)
    | _ -> failwith "matrix output must be object")

let check_matrix_rejects_failure_contract_result_mismatch_with_gate_accepted () =
  with_temp_dir (fun dir ->
    let a =
      write_report
        dir
        "a.cjson"
        (report ~runner_sha:(hex_root '1') "Darwin" "arm64")
    in
    let b =
      write_report
        dir
        "b.cjson"
        (report
           ~runner_sha:(hex_root '2')
           ~required_failure_contract_status:"rejected"
           "Linux"
           "x86_64")
    in
    let code, json = run_matrix [a; b] in
    check "failure contract result mismatch exits nonzero" (code = 1);
    match json with
    | `Assoc fields ->
      let readiness_blockers =
        string_list_value "validator_readiness_blockers" fields
      in
      check
        "failure contract result mismatch blocker"
        (List.mem
           "required_failure_case_contract_rejected"
           readiness_blockers);
      check
        "failure contract result mismatch next blocker"
        (String.equal
           (string_value "next_validator_readiness_blocker" fields)
           "required_failure_case_contract_rejected")
    | _ -> failwith "matrix output must be object")

let check_matrix_rejects_missing_failure_contract_with_gate_accepted () =
  with_temp_dir (fun dir ->
    let a =
      write_report
        dir
        "a.cjson"
        (report ~runner_sha:(hex_root '1') "Darwin" "arm64")
    in
    let b =
      write_report
        dir
        "b.cjson"
        (report ~runner_sha:(hex_root '2') "Linux" "x86_64"
         |> remove_result_field "required_failure_case_contract")
    in
    let code, json = run_matrix [a; b] in
    check "missing failure contract exits nonzero" (code = 1);
    match json with
    | `Assoc fields ->
      let readiness_blockers =
        string_list_value "validator_readiness_blockers" fields
      in
      check
        "missing failure contract blocker"
        (List.mem
           "required_failure_case_contract_rejected"
           readiness_blockers)
    | _ -> failwith "matrix output must be object")

let check_matrix_rejects_q1_failure_contract_missing_payload () =
  with_temp_dir (fun dir ->
    let a =
      write_report
        dir
        "a.cjson"
        (report ~runner_sha:(hex_root '1') "Darwin" "arm64")
    in
    let b =
      write_report
        dir
        "b.cjson"
        (report
           ~runner_sha:(hex_root '2')
           ~include_required_contract_payload:false
           "Linux"
           "x86_64")
    in
    let code, json = run_matrix [a; b] in
    check "missing Q1 failure contract payload exits nonzero" (code = 1);
    match json with
    | `Assoc fields ->
      check
        "missing Q1 failure contract payload blocker"
        (List.mem
           "required_failure_case_contract_rejected"
           (string_list_value "validator_readiness_blockers" fields))
    | _ -> failwith "matrix output must be object")

let check_matrix_rejects_q1_failure_contract_weakened_payload () =
  with_temp_dir (fun dir ->
    let weak_contract =
      q1_failure_contract_json ["nonfinite_fp16_scale", "reject_before_write"]
    in
    let a =
      write_report
        dir
        "a.cjson"
        (report ~runner_sha:(hex_root '1') "Darwin" "arm64")
    in
    let b =
      write_report
        dir
        "b.cjson"
        (report
           ~runner_sha:(hex_root '2')
           ~required_failure_contract_payload:weak_contract
           "Linux"
           "x86_64")
    in
    let code, json = run_matrix [a; b] in
    check "weak Q1 failure contract payload exits nonzero" (code = 1);
    match json with
    | `Assoc fields ->
      check
        "weak Q1 failure contract payload blocker"
        (List.mem
           "required_failure_case_contract_rejected"
           (string_list_value "validator_readiness_blockers" fields))
    | _ -> failwith "matrix output must be object")

let check_matrix_rejects_q1_failure_contract_accepted_with_blockers () =
  with_temp_dir (fun dir ->
    let a =
      write_report
        dir
        "a.cjson"
        (report ~runner_sha:(hex_root '1') "Darwin" "arm64")
    in
    let b =
      write_report
        dir
        "b.cjson"
        (report
           ~runner_sha:(hex_root '2')
           ~required_failure_contract_status:"accepted"
           ~required_failure_contract_blockers:
             ["q1_failure_case_missing_partial_output_input_aliasing"]
           "Linux"
           "x86_64")
    in
    let code, json = run_matrix [a; b] in
    check "accepted failure contract with blockers exits nonzero" (code = 1);
    match json with
    | `Assoc fields ->
      check
        "accepted failure contract with blockers is rejected"
        (List.mem
           "required_failure_case_contract_rejected"
           (string_list_value "validator_readiness_blockers" fields))
    | _ -> failwith "matrix output must be object")

let check_matrix_rejects_q1_failure_mutation_shape_rejected () =
  with_temp_dir (fun dir ->
    let a =
      write_report
        dir
        "a.cjson"
        (report ~runner_sha:(hex_root '1') "Darwin" "arm64")
    in
    let b =
      write_report
        dir
        "b.cjson"
        (report
           ~runner_sha:(hex_root '2')
           ~failure:
             (failure_case
                ~case:"negative_byte_offset"
                ~mutation_shape_status:"rejected"
                ~mutation_shape_blockers:
                  ["q1_failure_case_mutation_mismatch_negative_byte_offset"]
                ())
           "Linux"
           "x86_64")
    in
    let code, json = run_matrix [a; b] in
    check "rejected Q1 mutation shape exits nonzero" (code = 1);
    match json with
    | `Assoc fields ->
      check
        "rejected Q1 mutation shape top-level blocker"
        (List.mem "runner_report_rejected" (blockers fields));
      check
        "rejected Q1 mutation shape validator blocker"
        (List.mem
           "q1_failure_case_mutation_shape_rejected"
           (string_list_value "validator_readiness_blockers" fields))
    | _ -> failwith "matrix output must be object")

let check_matrix_rejects_forged_q1_contract_shape () =
  with_temp_dir (fun dir ->
    let forged runner_sha system machine =
      report ~runner_sha system machine
      |> replace_q1_contract_shape_field "lhs_cells" (`Int 129)
    in
    let a =
      write_report dir "a.cjson" (forged (hex_root '1') "Darwin" "arm64")
    in
    let b =
      write_report
        dir
        "b.cjson"
        (forged (hex_root '2') "Linux" "x86_64")
    in
    let code, json = run_matrix [a; b] in
    check "forged Q1 contract shape exits nonzero" (code = 1);
    match json with
    | `Assoc fields ->
      check
        "forged Q1 contract shape top-level blocker"
        (List.mem "runner_report_rejected" (blockers fields));
      check
        "forged Q1 contract shape validator blocker"
        (List.mem
           "q1_contract_shape_rejected"
           (string_list_value "validator_readiness_blockers" fields))
    | _ -> failwith "matrix output must be object")

let check_matrix_rejects_q1_contract_shape_effort_mismatch () =
  with_temp_dir (fun dir ->
    let forged runner_sha system machine =
      report ~runner_sha system machine
      |> replace_q1_contract_shape_field "expected_effort" (`Int 203)
    in
    let a =
      write_report dir "a.cjson" (forged (hex_root '1') "Darwin" "arm64")
    in
    let b =
      write_report
        dir
        "b.cjson"
        (forged (hex_root '2') "Linux" "x86_64")
    in
    let code, json = run_matrix [a; b] in
    check "Q1 contract shape effort mismatch exits nonzero" (code = 1);
    match json with
    | `Assoc fields ->
      check
        "Q1 contract shape effort mismatch blocker"
        (List.mem
           "q1_contract_shape_rejected"
           (string_list_value "validator_readiness_blockers" fields))
    | _ -> failwith "matrix output must be object")

let check_matrix_rejects_q1_contract_shape_opcode_effort_mismatch () =
  with_temp_dir (fun dir ->
    let forged runner_sha system machine =
      report ~runner_sha system machine
      |> replace_result_field "expected_opcode_effort" (`Int 202)
      |> replace_result_field "observed_opcode_effort" (`Int 202)
    in
    let a =
      write_report dir "a.cjson" (forged (hex_root '1') "Darwin" "arm64")
    in
    let b =
      write_report
        dir
        "b.cjson"
        (forged (hex_root '2') "Linux" "x86_64")
    in
    let code, json = run_matrix [a; b] in
    check "Q1 contract shape opcode effort mismatch exits nonzero" (code = 1);
    match json with
    | `Assoc fields ->
      check
        "Q1 contract shape opcode effort mismatch blocker"
        (List.mem
           "q1_contract_shape_rejected"
           (string_list_value "validator_readiness_blockers" fields))
    | _ -> failwith "matrix output must be object")

let check_matrix_rejects_q1_contract_shape_byte_offset_span () =
  with_temp_dir (fun dir ->
    let forged runner_sha system machine =
      report ~runner_sha system machine
      |> replace_q1_contract_shape_field "byte_offset" (`Int 1)
    in
    let a =
      write_report dir "a.cjson" (forged (hex_root '1') "Darwin" "arm64")
    in
    let b =
      write_report
        dir
        "b.cjson"
        (forged (hex_root '2') "Linux" "x86_64")
    in
    let code, json = run_matrix [a; b] in
    check "Q1 contract shape byte offset span exits nonzero" (code = 1);
    match json with
    | `Assoc fields ->
      check
        "Q1 contract shape byte offset span blocker"
        (List.mem
           "q1_contract_shape_rejected"
           (string_list_value "validator_readiness_blockers" fields))
    | _ -> failwith "matrix output must be object")

let check_matrix_rejects_q1_contract_shape_observed_effort_forgery () =
  with_temp_dir (fun dir ->
    let forged runner_sha system machine =
      report ~runner_sha system machine
      |> replace_result_field "observed_effort" (`Int 203)
      |> replace_result_field "observed_program_effort" (`Int 203)
      |> replace_result_field "observed_opcode_effort" (`Int 202)
    in
    let a =
      write_report dir "a.cjson" (forged (hex_root '1') "Darwin" "arm64")
    in
    let b =
      write_report
        dir
        "b.cjson"
        (forged (hex_root '2') "Linux" "x86_64")
    in
    let code, json = run_matrix [a; b] in
    check "Q1 observed effort forgery exits nonzero" (code = 1);
    match json with
    | `Assoc fields ->
      check
        "Q1 observed effort forgery blocker"
        (List.mem
           "q1_contract_shape_rejected"
           (string_list_value "validator_readiness_blockers" fields))
    | _ -> failwith "matrix output must be object")

let check_matrix_rejects_q1_contract_shape_output_cells_mismatch () =
  with_temp_dir (fun dir ->
    let forged runner_sha system machine =
      report ~runner_sha system machine
      |> replace_q1_contract_shape_field "output_cells" (`Int 7)
    in
    let a =
      write_report dir "a.cjson" (forged (hex_root '1') "Darwin" "arm64")
    in
    let b =
      write_report
        dir
        "b.cjson"
        (forged (hex_root '2') "Linux" "x86_64")
    in
    let code, json = run_matrix [a; b] in
    check "Q1 output cells mismatch exits nonzero" (code = 1);
    match json with
    | `Assoc fields ->
      check
        "Q1 output cells mismatch blocker"
        (List.mem
           "q1_contract_shape_rejected"
           (string_list_value "validator_readiness_blockers" fields))
    | _ -> failwith "matrix output must be object")

let check_matrix_rejects_q1_contract_shape_output_span_base_mismatch () =
  with_temp_dir (fun dir ->
    let forged runner_sha system machine =
      report ~runner_sha system machine
      |> replace_result_subspan_field "base_address" (`Int 10001)
    in
    let a =
      write_report dir "a.cjson" (forged (hex_root '1') "Darwin" "arm64")
    in
    let b =
      write_report
        dir
        "b.cjson"
        (forged (hex_root '2') "Linux" "x86_64")
    in
    let code, json = run_matrix [a; b] in
    check "Q1 output span base mismatch exits nonzero" (code = 1);
    match json with
    | `Assoc fields ->
      check
        "Q1 output span base mismatch blocker"
        (List.mem
           "q1_contract_shape_rejected"
           (string_list_value "validator_readiness_blockers" fields))
    | _ -> failwith "matrix output must be object")

let check_matrix_rejects_q1_contract_shape_oversized_intlit () =
  with_temp_dir (fun dir ->
    let forged =
      report ~runner_sha:(hex_root '1') "Darwin" "arm64"
      |> replace_q1_contract_shape_field "m" (`Intlit "999999999999999999999")
    in
    let a = write_report dir "a.cjson" forged in
    let b =
      write_report
        dir
        "b.cjson"
        (report ~runner_sha:(hex_root '2') "Linux" "x86_64")
    in
    let code, json = run_matrix [a; b] in
    check "Q1 oversized intlit exits nonzero without crashing" (code = 1);
    match json with
    | `Assoc fields ->
      check
        "Q1 oversized intlit blocker"
        (List.mem
           "q1_contract_shape_rejected"
           (string_list_value "validator_readiness_blockers" fields))
    | _ -> failwith "matrix output must be object")

let check_matrix_rejects_q1_failure_contract_not_applicable () =
  with_temp_dir (fun dir ->
    let a =
      write_report
        dir
        "a.cjson"
        (report ~runner_sha:(hex_root '1') "Darwin" "arm64")
    in
    let b =
      write_report
        dir
        "b.cjson"
        (report
           ~runner_sha:(hex_root '2')
           ~required_failure_contract_status:"not_applicable"
           "Linux"
           "x86_64")
    in
    let code, json = run_matrix [a; b] in
    check "q1 not-applicable failure contract exits nonzero" (code = 1);
    match json with
    | `Assoc fields ->
      check
        "q1 not-applicable failure contract blocker"
        (List.mem
           "required_failure_case_contract_rejected"
           (string_list_value "validator_readiness_blockers" fields))
    | _ -> failwith "matrix output must be object")

let check_matrix_accepts_non_q1_failure_contract_not_applicable () =
  with_temp_dir (fun dir ->
    let a =
      write_report
        dir
        "a.cjson"
        (report
           ~runner_sha:(hex_root '1')
           ~opcode:"RMSNORM_FP_EPS"
           ~required_failure_contract_status:"not_applicable"
           "Darwin"
           "arm64")
    in
    let b =
      write_report
        dir
        "b.cjson"
        (report
           ~runner_sha:(hex_root '2')
           ~opcode:"RMSNORM_FP_EPS"
           ~required_failure_contract_status:"not_applicable"
           "Linux"
           "x86_64")
    in
    let code, json = run_matrix [a; b] in
    check "non-q1 not-applicable matrix exits zero" (code = 0);
    match json with
    | `Assoc fields ->
      check "non-q1 not-applicable matrix accepted"
        (String.equal (string_value "status" fields) "accepted")
    | _ -> failwith "matrix output must be object")

let check_matrix_rejects_empty_results () =
  with_temp_dir (fun dir ->
    let a =
      write_report
        dir
        "a.cjson"
        (report ~runner_sha:(hex_root '1') "Darwin" "arm64")
    in
    let b =
      write_report
        dir
        "b.cjson"
        (report ~runner_sha:(hex_root '2') "Linux" "x86_64"
         |> replace_assoc_field "results" (`List []))
    in
    let code, json = run_matrix [a; b] in
    check "empty results matrix exits nonzero" (code = 1);
    match json with
    | `Assoc fields ->
      check
        "empty results top-level blocker"
        (List.mem "runner_report_rejected" (blockers fields));
      check
        "empty results validator blocker"
        (List.mem
           "missing_results"
           (string_list_value "validator_readiness_blockers" fields))
    | _ -> failwith "matrix output must be object")

let check_matrix_rejects_vm_semantics_binding_mismatch () =
  with_temp_dir (fun dir ->
    let a =
      write_report
        dir
        "a.cjson"
        (report ~runner_sha:(hex_root '1') "Darwin" "arm64")
    in
    let b =
      write_report
        dir
        "b.cjson"
        (report
           ~runner_sha:(hex_root '2')
           ~vm_semantics_status:"unbound"
           "Linux"
           "x86_64")
    in
    let code, json = run_matrix [a; b] in
    check "vm semantics mismatch matrix exits nonzero" (code = 1);
    match json with
    | `Assoc fields ->
      check
        "vm semantics mismatch matrix blocker"
        (List.mem "result_mismatch_across_platforms" (blockers fields));
      check
        "vm semantics mismatch validator blocker"
        (List.mem
           "vm_semantics_binding_mismatch"
           (string_list_value "validator_readiness_blockers" fields));
      (match assoc_value "reports" fields with
       | `List reports ->
         check
           "vm semantics mismatch source row blocker"
           (List.exists
              (function
                | `Assoc report_fields ->
                  List.mem "vm_semantics_binding_mismatch" (blockers report_fields)
                | _ -> false)
              reports)
       | _ -> failwith "missing report rows")
    | _ -> failwith "matrix output must be object")

let check_matrix_rejects_same_platform () =
  with_temp_dir (fun dir ->
    let a =
      write_report
        dir
        "a.cjson"
        (report ~runner_sha:(hex_root '1') "Darwin" "arm64")
    in
    let b =
      write_report
        dir
        "b.cjson"
        (report ~runner_sha:(hex_root '2') "Darwin" "arm64")
    in
    let code, json = run_matrix [a; b] in
    check "same platform matrix exits nonzero" (code = 1);
    match json with
    | `Assoc fields ->
      check
        "same platform blocker"
        (List.mem "insufficient_distinct_platforms" (blockers fields))
    | _ -> failwith "matrix output must be object")

let () =
  check_matrix_accepts_bound_reports ();
  check_matrix_rejects_reused_runner_hash ();
  check_matrix_accepts_required_opcode_reports ();
  check_matrix_rejects_missing_required_opcode ();
  check_matrix_rejects_missing_runner_hash ();
  check_matrix_rejects_corpus_mismatch ();
  check_matrix_rejects_failure_case_mismatch ();
  check_matrix_rejects_failure_mutation_payload_mismatch ();
  check_matrix_rejects_forged_failure_mutation_payload_shape ();
  check_matrix_accepts_nonfinite_fp16_nan_scale_payload ();
  check_matrix_rejects_partial_alias_payload_outside_lhs ();
  check_matrix_rejects_missing_failure_mutation_payload ();
  check_matrix_rejects_missing_required_q1_failure_row ();
  check_matrix_rejects_failure_snapshot_mismatch ();
  check_matrix_rejects_failure_finite_span_mismatch ();
  check_matrix_rejects_opcode_effort_mismatch ();
  check_matrix_rejects_profile_root_binding_mismatch ();
  check_matrix_rejects_profile_root_result_mismatch_with_gate_accepted ();
  check_matrix_rejects_failure_contract_result_mismatch_with_gate_accepted ();
  check_matrix_rejects_missing_failure_contract_with_gate_accepted ();
  check_matrix_rejects_q1_failure_contract_missing_payload ();
  check_matrix_rejects_q1_failure_contract_weakened_payload ();
  check_matrix_rejects_q1_failure_contract_accepted_with_blockers ();
  check_matrix_rejects_q1_failure_mutation_shape_rejected ();
  check_matrix_rejects_forged_q1_contract_shape ();
  check_matrix_rejects_q1_contract_shape_effort_mismatch ();
  check_matrix_rejects_q1_contract_shape_opcode_effort_mismatch ();
  check_matrix_rejects_q1_contract_shape_byte_offset_span ();
  check_matrix_rejects_q1_contract_shape_observed_effort_forgery ();
  check_matrix_rejects_q1_contract_shape_output_cells_mismatch ();
  check_matrix_rejects_q1_contract_shape_output_span_base_mismatch ();
  check_matrix_rejects_q1_contract_shape_oversized_intlit ();
  check_matrix_rejects_q1_failure_contract_not_applicable ();
  check_matrix_accepts_non_q1_failure_contract_not_applicable ();
  check_matrix_rejects_empty_results ();
  check_matrix_rejects_abi_declaration_binding_mismatch ();
  check_matrix_rejects_executable_abi_binding_mismatch ();
  check_matrix_prioritizes_executable_abi_not_run ();
  check_matrix_rejects_vm_semantics_binding_mismatch ();
  check_matrix_rejects_same_platform ()
