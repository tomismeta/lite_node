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

let run_matrix reports =
  let command =
    String.concat
      " "
      (Filename.quote (tool_path ())
       :: List.concat
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

let failure_case ?(observed = "vm_rejected") ?(observed_effort = 200) () =
  `Assoc [
    "opcode", `String "LINEAR_Q1_G128_FP";
    "case", `String "nonfinite_input_nan";
    "expected", `String "reject_before_write";
    "status", `String "accepted";
    "counted", `Bool true;
    "observed", `String observed;
    "observed_effort", `Int observed_effort;
    "unchanged_status", `String "matched";
    "changed_status", `String "not_changed";
    "finite_status", `String "finite";
    "active_changed_status", `String "not_changed";
    "active_finite_status", `String "finite";
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
    "finite_spans", `List [];
    "active_changed_spans", `List [];
    "active_finite_spans", `List [];
  ]

let result
    ?(opcode = "LINEAR_Q1_G128_FP")
    ?(observed_opcode_effort = 200)
    ?(opcode_effort_match = true)
    ?(profile_root_status = "matched")
    ?(vm_semantics_status = "matched")
    ?(abi_declaration_status = "matched")
    ?(required_failure_contract_status = "accepted")
    ?required_failure_contract_blockers
    ?(failure = failure_case ())
    () =
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
    "status", `String "accepted";
    "vm_run", `String "accepted";
    "output_status", `String "matched";
    "expected_effort", `Int 201;
    "observed_effort", `Int 201;
    "expected_opcode_effort", `Int 200;
    "observed_opcode_effort", `Int observed_opcode_effort;
    "opcode_effort_match", `Bool opcode_effort_match;
    "effort_match", `Bool true;
    "strict_effort", `Bool true;
    "required_failure_case_contract",
    `Assoc [
      "status", `String required_failure_contract_status;
      "blockers",
      `List
        (List.map
           (fun blocker -> `String blocker)
           required_failure_contract_blockers);
    ];
    "subspans",
    `List [
      `Assoc [
        "name", `String "output";
        "length_f64_cells", `Int 6;
        "expected_sha256", `String (hex_root 'a');
        "observed_sha256", `String (hex_root 'a');
        "expected_root", `String (hex_root 'b');
        "matched", `Bool true;
      ];
    ];
    "failure_cases", `List [failure];
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
    ?(required_failure_contract_status = "accepted")
    ?required_failure_contract_blockers
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
      "blockers", `List [`String "cross_platform_conformance_missing"];
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
        ~required_failure_contract_status
        ?required_failure_contract_blockers
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
        "matrix carries runner hashes"
        (List.length (string_list_value "runner_executable_sha256s" fields) = 2)
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
           readiness_blockers)
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
  check_matrix_rejects_missing_runner_hash ();
  check_matrix_rejects_corpus_mismatch ();
  check_matrix_rejects_failure_case_mismatch ();
  check_matrix_rejects_opcode_effort_mismatch ();
  check_matrix_rejects_profile_root_binding_mismatch ();
  check_matrix_rejects_profile_root_result_mismatch_with_gate_accepted ();
  check_matrix_rejects_failure_contract_result_mismatch_with_gate_accepted ();
  check_matrix_rejects_missing_failure_contract_with_gate_accepted ();
  check_matrix_rejects_q1_failure_contract_accepted_with_blockers ();
  check_matrix_rejects_q1_failure_contract_not_applicable ();
  check_matrix_accepts_non_q1_failure_contract_not_applicable ();
  check_matrix_rejects_empty_results ();
  check_matrix_rejects_abi_declaration_binding_mismatch ();
  check_matrix_rejects_vm_semantics_binding_mismatch ();
  check_matrix_rejects_same_platform ()
