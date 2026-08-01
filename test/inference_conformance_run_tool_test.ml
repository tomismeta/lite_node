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

module Abi = Octra_vm.Inference_session_abi
module Profile = Octra_vm.Inference_numerical_profile
module Template = Octra_vm.Inference_conformance_template

let opcode = "LINEAR_Q1_G128_FP"
let primitive = "q1_g128_projection"

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

let int_value name fields =
  match assoc_value name fields with
  | `Int value -> value
  | `Intlit value -> int_of_string value
  | _ -> failwith ("json field must be an int: " ^ name)

let bool_value name fields =
  match assoc_value name fields with
  | `Bool value -> value
  | _ -> failwith ("json field must be a bool: " ^ name)

let list_value name fields =
  match assoc_value name fields with
  | `List values -> values
  | _ -> failwith ("json field must be a list: " ^ name)

let assoc_json name fields =
  match assoc_value name fields with
  | `Assoc values -> values
  | _ -> failwith ("json field must be an object: " ^ name)

let write_file path raw =
  let output = open_out_bin path in
  Fun.protect
    ~finally:(fun () -> close_out_noerr output)
    (fun () -> output_string output raw)

let write_json path json =
  write_file path (Yojson.Safe.to_string json)

let read_all input =
  let buffer = Buffer.create 4096 in
  (try
     while true do
       Buffer.add_string buffer (input_line input);
       Buffer.add_char buffer '\n'
     done
   with End_of_file -> ());
  Buffer.contents buffer

let tool_path () =
  let candidates =
    [
      "_build/default/tools/inference_conformance_run.exe";
      "../tools/inference_conformance_run.exe";
    ]
  in
  match List.find_opt Sys.file_exists candidates with
  | Some path -> path
  | None -> failwith "missing inference_conformance_run.exe"

let sha256 raw =
  Digestif.SHA256.(digest_string raw |> to_hex)

let current_profile () =
  match Profile.current_runtime_profile ~opcode with
  | Some profile -> profile
  | None -> failwith "missing Q1 runtime profile"

let current_profile_root () =
  let profile = current_profile () in
  match Profile.of_name profile with
  | Ok profile -> Profile.root_for_opcode ~opcode profile
  | Error error -> failwith (Profile.error_message error)

let current_vm_semantics_root () =
  match Template.vm_semantics_root_for_opcode ~opcode with
  | Some root -> root
  | None -> failwith "missing Q1 VM semantics root"

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

let f64_bytes values =
  let raw = Bytes.create (List.length values * 8) in
  List.iteri
    (fun index value -> put_int64_le raw index (Int64.bits_of_float value))
    values;
  Bytes.to_string raw

let q1_owner =
  "\000\060" ^ String.make 16 '\255'

let q1_owner_for ~k ~n =
  let blocks = n * (k / 128) in
  String.concat "" (List.init blocks (fun _ -> q1_owner))

let input_for ~m ~k =
  f64_bytes (List.init (m * k) (fun _ -> 1.0))

let expected_output_for ~m ~k ~n =
  f64_bytes (List.init (m * n) (fun _ -> float_of_int k))

let expected_output = expected_output_for ~m:1 ~k:128 ~n:1

let manifest name path raw =
  `Assoc [
    "name", `String name;
    "path", `String path;
    "bytes", `Int (String.length raw);
    "sha256", `String (sha256 raw);
  ]

let span name base cells =
  `Assoc [
    "name", `String name;
    "base_address", `Int base;
    "length_f64_cells", `Int cells;
  ]

let source path raw =
  `Assoc [
    "path", `String path;
    "bytes", `Int (String.length raw);
    "sha256", `String (sha256 raw);
  ]

let mutation name target fields =
  `Assoc (["mutation", `String name; "target", `String target] @ fields)

let reject_case ?(output_cells = 1) name mutations =
  `Assoc [
    "case", `String name;
    "expected", `String "reject_before_write";
    "executable_mutations", `List mutations;
    "unchanged_spans", `List [span "expected" 10000 output_cells];
  ]

let accept_snapshot_case ?(output_cells = 1) name expected offset =
  `Assoc [
    "case", `String name;
    "expected", `String expected;
    "executable_mutations",
    `List [
      mutation
        "set_output_base_to_first_input_base_plus"
        "output.base_address"
        ["offset_cells", `Int offset];
    ];
    "unchanged_spans", `List [span name (2000 + offset) output_cells];
  ]

let q1_failure_cases ?(output_cells = 1) ?(lower_effort = 199) () =
  [
    reject_case
      ~output_cells
      "nonfinite_input_nan"
      [
        mutation
          "replace_first_f64_input_cell"
          "lhs"
          ["value_bits", `Intlit "9221120237041090560"];
      ];
    reject_case
      ~output_cells
      "nonfinite_input_infinity"
      [
        mutation
          "replace_first_f64_input_cell"
          "lhs"
          ["value_bits", `Intlit "9218868437227405312"];
      ];
    accept_snapshot_case
      ~output_cells
      "output_input_aliasing"
      "accept_from_snapshot_exact"
      0;
    accept_snapshot_case
      ~output_cells
      "partial_output_input_aliasing"
      "accept_from_snapshot_partial"
      1;
    reject_case
      ~output_cells
      "k_not_multiple_of_128"
      [
        mutation
          "set_scalar_param"
          "parameter_addresses_and_scalar_params.values.k"
          ["value", `Int 127];
      ];
    reject_case
      ~output_cells
      "bad_q1_owner_length"
      [
        mutation
          "truncate_input_manifest"
          "q1_owner"
          ["truncate_bytes", `Int 1];
      ];
    reject_case
      ~output_cells
      "negative_byte_offset"
      [
        mutation
          "set_scalar_param"
          "parameter_addresses_and_scalar_params.values.byte_offset"
          ["value", `Int (-1)];
      ];
    reject_case
      ~output_cells
      "byte_offset_out_of_bounds"
      [
        mutation
          "set_scalar_param"
          "parameter_addresses_and_scalar_params.values.byte_offset"
          ["value", `Int 1_000_000];
      ];
    reject_case
      ~output_cells
      "byte_offset_truncated_span"
      [
        mutation
          "set_scalar_param"
          "parameter_addresses_and_scalar_params.values.byte_offset"
          ["value", `Int 1];
      ];
    reject_case
      ~output_cells
      "nonfinite_fp16_scale"
      [
        mutation
          "replace_q1_scale_bits"
          "q1_owner[0..2]"
          ["value_hex_le", `String "007c"];
      ];
    reject_case
      ~output_cells
      "lower_effort_limit"
      [
        mutation
          "lower_effort_limit"
          "effort"
          ["value", `Int lower_effort];
      ];
  ]

let q1_template ?(session_abi_root = Abi.v1_root)
    ?(m = 1) ?(k = 128) ?(n = 1) ?expected_effort
    ?failure_cases ?(output_count_unit = "cells") ?(r1 = 1) () =
  let input = input_for ~m ~k in
  let q1_owner = q1_owner_for ~k ~n in
  let expected_output = expected_output_for ~m ~k ~n in
  let expected_effort =
    match expected_effort with
    | Some value -> value
    | None -> 200 + ((m * n * k) / 512) + 1
  in
  let failure_cases =
    match failure_cases with
    | Some cases -> cases
    | None -> q1_failure_cases ~output_cells:(m * n) ()
  in
  `Assoc [
    "type", `String "p0_litenode_vm_execution_template";
    "schema", `String "octra.inference.p0.vm-template.v1";
    "opcode", `String opcode;
    "primitive", `String primitive;
    "profile", `String (current_profile ());
    "vm_semantics_root", `String (current_vm_semantics_root ());
    "numerical_profile_root", `String (current_profile_root ());
    "expected_effort", `Int expected_effort;
    "program_effect_requirements",
    `Assoc [
      "program_effects",
      `List [
        `String "memory_read";
        `String "memory_write";
        `String "storage_read";
      ];
    ];
    "input_memory_ranges",
    `List [
      `Assoc [
        "name", `String "lhs";
        "source", source "fixtures/lhs.f64le.bin" input;
        "range_binding", `Assoc ["encoding", `String "f64le"];
        "vm_memory",
        `Assoc [
          "base_address", `Int 2000;
          "length_f64_cells", `Int (m * k);
        ];
      ];
      `Assoc [
        "name", `String "q1_owner";
        "source", source "fixtures/q1-owner.bin" q1_owner;
        "range_binding", `Assoc ["encoding", `String "tensor.q1-g128"];
        "vm_memory",
        `Assoc [
          "base_address", `Int 0;
          "raw_register", `String "r3";
        ];
      ];
    ];
    "expected_output_byte_manifests",
    `List [manifest "expected" "fixtures/expected.f64le.bin" expected_output];
    "parameter_addresses_and_scalar_params",
    `Assoc [
      "registers",
      `Assoc [
        "dst", `String "r0";
        "lhs", `String "r2";
        "q1_owner", `String "r3";
        "byte_offset", `String "r4";
        "m", `String "r5";
        "k", `String "r6";
        "n", `String "r7";
      ];
      "values",
      `Assoc [
        "dst", `Int 10000;
        "lhs", `Int 2000;
        "byte_offset", `Int 0;
        "m", `Int m;
        "k", `Int k;
        "n", `Int n;
      ];
    ];
    "abi",
    `Assoc [
      "session_abi_root", `String session_abi_root;
      "entrypoint", `String Abi.advance_entrypoint;
      "label", `Int Abi.advance_label;
      "output_base_register", `String "r0";
      "output_count_register", `String "r1";
      "output_count_unit", `String output_count_unit;
      "request_input_root_cell", `Int Abi.input_root_cell;
    ];
    "output",
    `Assoc [
      "base_address", `Int 10000;
      "length_f64_cells", `Int (m * n);
      "abi_registers", `Assoc ["r0", `Int 10000; "r1", `Int r1];
      "subspans",
      `List [
        `Assoc [
          "name", `String "expected";
          "base_address", `Int 10000;
          "length_f64_cells", `Int (m * n);
          "sha256", `String (sha256 expected_output);
          "root", `String (sha256 expected_output);
        ];
      ];
    ];
    "expected_failure_atomicity_behavior", `List failure_cases;
  ]

let replace_failure_expected case expected = function
  | `Assoc fields ->
    `Assoc
      (List.map
         (fun (key, value) ->
            if String.equal key "expected"
               && String.equal (string_value "case" fields) case then
              key, `String expected
            else
              key, value)
         fields)
  | value -> value

let replace_failure_mutations case mutations = function
  | `Assoc fields ->
    `Assoc
      (List.map
         (fun (key, value) ->
            if String.equal key "executable_mutations"
               && String.equal (string_value "case" fields) case then
              key, `List mutations
            else
              key, value)
         fields)
  | value -> value

let index_json =
  `Assoc [
    "type", `String "p0_litenode_vm_execution_template_index";
    "templates",
    `List [
      `Assoc [
        "opcode", `String opcode;
        "primitive", `String primitive;
        "vm_execution_template", `String "q1.cjson";
      ];
    ];
  ]

let remove_tree root =
  if Sys.file_exists root then begin
    let rec remove path =
      if Sys.is_directory path then begin
        Sys.readdir path
        |> Array.iter (fun name -> remove (Filename.concat path name));
        Unix.rmdir path
      end
      else Sys.remove path
    in
    remove root
  end

let with_temp_dir f =
  let dir = Filename.temp_file "octra-run-tool-test" "" in
  Sys.remove dir;
  Unix.mkdir dir 0o700;
  Fun.protect ~finally:(fun () -> remove_tree dir) (fun () -> f dir)

let write_fixture dir template =
  let fixtures = Filename.concat dir "fixtures" in
  if not (Sys.file_exists fixtures) then Unix.mkdir fixtures 0o700;
  let fields =
    match template with
    | `Assoc fields -> fields
    | _ -> failwith "template must be object"
  in
  let params = assoc_json "parameter_addresses_and_scalar_params" fields in
  let values = assoc_json "values" params in
  let m = int_value "m" values in
  let k = int_value "k" values in
  let n = int_value "n" values in
  write_file (Filename.concat fixtures "lhs.f64le.bin") (input_for ~m ~k);
  write_file (Filename.concat fixtures "q1-owner.bin") (q1_owner_for ~k ~n);
  write_file
    (Filename.concat fixtures "expected.f64le.bin")
    (expected_output_for ~m ~k ~n);
  write_json (Filename.concat dir "q1.cjson") template;
  write_json (Filename.concat dir "index.cjson") index_json

let run_conformance dir template args =
  write_fixture dir template;
  let command =
    String.concat
      " "
      ([
         Filename.quote (tool_path ());
         "--template-index";
         Filename.quote (Filename.concat dir "index.cjson");
         "--opcode";
         opcode;
       ]
       @ args)
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

let first_result report =
  match report with
  | `Assoc fields ->
    (match list_value "results" fields with
     | `Assoc result :: _ -> result
     | _ -> failwith "missing runner result")
  | _ -> failwith "report must be object"

let gate_status name report =
  match report with
  | `Assoc fields -> string_value "status" (assoc_json name fields)
  | _ -> failwith "report must be object"

let string_list name fields =
  list_value name fields
  |> List.map (function
    | `String value -> value
    | _ -> failwith ("json field must be a string list: " ^ name))

let check_q1_contract_visible gate =
  match list_value "required_failure_case_contracts" gate with
  | [`Assoc contract] ->
    check
      "runner q1 contract opcode"
      (String.equal
         (string_value "opcode" contract)
         "LINEAR_Q1_G128_FP");
    check
      "runner q1 contract expectation count"
      (List.length (list_value "expectations" contract) = 11)
  | _ -> failwith "expected one runner required failure-case contract"

let bound_matrix_report_row
    ~profile_catalog_root
    ~template_corpus_root
    ~result_signature_sha256
    platform_key runner_sha =
  `Assoc [
    "accepted", `Bool true;
    "runner_report_sha256", `String (sha256 platform_key);
    "result_signature_sha256", `String result_signature_sha256;
    "result_signature_schema",
    `String "octra.inference.conformance.result-signature.v6";
    "result_opcodes", `List [`String opcode];
    "platform_key", `String platform_key;
    "runner_executable_sha256", `String runner_sha;
    "profile_catalog_root", `String profile_catalog_root;
    "template_corpus_root", `String template_corpus_root;
  ]

let accepted_matrix
    ~profile_catalog_root
    ~template_corpus_root
    ~result_signature_sha256 =
  `Assoc [
    "status", `String "accepted";
    "schema", `String "octra.inference.conformance.matrix.v1";
    "cross_platform_status", `String "accepted";
    "blockers", `List [];
    "result_opcodes", `List [`String opcode];
    "result_signature_schema",
    `String "octra.inference.conformance.result-signature.v6";
    "distinct_platform_count", `Int 2;
    "distinct_runner_executable_count", `Int 2;
    "distinct_platform_runner_observation_count", `Int 2;
    "result_signature_count", `Int 1;
    "profile_catalog_roots", `List [`String profile_catalog_root];
    "template_corpus_roots", `List [`String template_corpus_root];
    "reports",
    `List [
      bound_matrix_report_row
        ~profile_catalog_root
        ~template_corpus_root
        ~result_signature_sha256
        "4.14.2|Unix|Darwin|1.0|arm64|64|false|native"
        (hex_root '1');
      bound_matrix_report_row
        ~profile_catalog_root
        ~template_corpus_root
        ~result_signature_sha256
        "4.14.2|Unix|Linux|1.0|x86_64|64|false|native"
        (hex_root '2');
    ];
  ]

let write_matrix dir matrix =
  let path = Filename.concat dir "matrix.cjson" in
  let raw = Yojson.Safe.to_string matrix in
  write_file path raw;
  path, sha256 raw

let replace_assoc_field name value = function
  | `Assoc fields ->
    `Assoc
      ((name, value)
       :: List.filter (fun (key, _) -> not (String.equal key name)) fields)
  | _ -> failwith "json value must be an object"

let replace_first_report_field name value = function
  | `Assoc fields as matrix ->
    let reports =
      match list_value "reports" fields with
      | [] -> []
      | first :: rest -> replace_assoc_field name value first :: rest
    in
    replace_assoc_field "reports" (`List reports) matrix
  | _ -> failwith "matrix must be an object"

let replace_report_field index name value = function
  | `Assoc fields as matrix ->
    let reports =
      match list_value "reports" fields with
      | reports ->
        List.mapi
          (fun report_index -> function
             | `Assoc report_fields when report_index = index ->
               `Assoc
                 ((name, value)
                  :: List.filter
                       (fun (key, _) -> not (String.equal key name))
                       report_fields)
             | report -> report)
          reports
    in
    replace_assoc_field "reports" (`List reports) matrix
  | _ -> failwith "matrix must be an object"

let different_sha sha =
  match String.length sha with
  | 64 ->
    let replacement = if Char.equal sha.[0] '0' then '1' else '0' in
    String.make 1 replacement ^ String.sub sha 1 63
  | _ -> failwith "expected sha256 hex"

let failure_case_fields result case =
  list_value "failure_cases" result
  |> List.find_map (function
    | `Assoc fields when String.equal (string_value "case" fields) case ->
      Some fields
    | _ -> None)
  |> function
  | Some fields -> fields
  | None -> failwith ("missing failure case: " ^ case)

let replace_output_subspan_sha sha = function
  | `Assoc fields ->
    let replace_subspan = function
      | `Assoc subspan ->
        `Assoc
          (List.map
             (fun (key, value) ->
                if String.equal key "sha256" then key, `String sha
                else key, value)
             subspan)
      | value -> value
    in
    let replace_output = function
      | `Assoc output ->
        `Assoc
          (List.map
             (fun (key, value) ->
                if String.equal key "subspans" then
                  key, `List (List.map replace_subspan (list_value key output))
                else key, value)
             output)
      | value -> value
    in
    `Assoc
      (List.map
         (fun (key, value) ->
            if String.equal key "output" then key, replace_output value
            else key, value)
         fields)
  | value -> value

let replace_output_subspan_base base = function
  | `Assoc fields ->
    let replace_subspan = function
      | `Assoc subspan ->
        `Assoc
          (List.map
             (fun (key, value) ->
                if String.equal key "base_address" then key, `Int base
                else key, value)
             subspan)
      | value -> value
    in
    let replace_output = function
      | `Assoc output ->
        `Assoc
          (List.map
             (fun (key, value) ->
                if String.equal key "subspans" then
                  key, `List (List.map replace_subspan (list_value key output))
                else key, value)
             output)
      | value -> value
    in
    `Assoc
      (List.map
         (fun (key, value) ->
            if String.equal key "output" then key, replace_output value
            else key, value)
         fields)
  | value -> value

let check_good_template_reports_bound_abi () =
  with_temp_dir (fun dir ->
    let code, report =
      run_conformance
        dir
        (q1_template ())
        [
          "--strict-effort";
          "--include-failures";
          "--require-failure-cases";
          "--require-profile-roots-bound";
        ]
    in
    check "good runner exits zero" (code = 0);
    let result = first_result report in
    check "good runner status" (String.equal (string_value "status" result) "accepted");
    check "good runner output" (String.equal (string_value "output_status" result) "matched");
    check "good program effort" (bool_value "program_effort_match" result);
    check "good opcode effort" (bool_value "opcode_effort_match" result);
    check "good failure cases included" (bool_value "failure_cases_included" result);
    check "good failure case count" (int_value "failure_case_count" result = 11);
    check
      "good counted failure case count"
      (int_value "counted_failure_case_count" result = 11);
    check
      "good accepted counted failure case count"
      (int_value "accepted_counted_failure_case_count" result = 11);
    let nonfinite_nan = failure_case_fields result "nonfinite_input_nan" in
    check
      "failure case carries executable mutation payload"
      (List.length (list_value "executable_mutations" nonfinite_nan) = 1);
    let exact_alias = failure_case_fields result "output_input_aliasing" in
    check
      "exact alias snapshot matched"
      (String.equal
         (string_value "snapshot_output_status" exact_alias)
         "matched");
    let partial_alias =
      failure_case_fields result "partial_output_input_aliasing"
    in
    check
      "partial alias snapshot matched"
      (String.equal
         (string_value "snapshot_output_status" partial_alias)
         "matched");
    let bad_q1_owner =
      failure_case_fields result "bad_q1_owner_length"
    in
    check
      "bad q1 owner length reaches VM"
      (String.equal
         (string_value "observed" bad_q1_owner)
         "vm_rejected");
    (match report with
     | `Assoc fields ->
       check_q1_contract_visible (assoc_json "failure_case_gate" fields)
     | _ -> failwith "report must be object");
    let result_contract = assoc_json "required_failure_case_contract" result in
    (match assoc_value "contract" result_contract with
     | `Assoc contract_fields ->
       check
         "good result Q1 contract opcode"
         (String.equal
            (string_value "opcode" contract_fields)
            "LINEAR_Q1_G128_FP");
       check
         "good result Q1 contract expectation count"
         (List.length (list_value "expectations" contract_fields) = 11)
     | _ -> failwith "expected result Q1 failure-case contract");
    check
      "good failure gate accepted"
      (String.equal (gate_status "failure_case_gate" report) "accepted");
    let semantics = assoc_json "vm_semantics_binding" result in
    check
      "good VM semantics matched"
      (String.equal (string_value "status" semantics) "matched");
    check
      "good VM semantics gate accepted"
      (String.equal (gate_status "vm_semantics_binding_gate" report) "accepted");
    let abi = assoc_json "abi_declaration_binding" result in
    check "good ABI declaration matched" (String.equal (string_value "status" abi) "matched");
    let executable_abi = assoc_json "executable_abi_binding" result in
    check
      "good executable ABI matched"
      (String.equal (string_value "status" executable_abi) "matched");
    check
      "good executable ABI r0"
      (int_value "observed_r0" executable_abi = 10000);
    check
      "good executable ABI r1"
      (int_value "observed_r1" executable_abi = 1);
    check
      "good executable ABI payload"
      (String.equal
         (string_value "output_payload_status" executable_abi)
         "accepted");
    let readiness =
      match report with
      | `Assoc fields -> assoc_json "validator_readiness_gate" fields
      | _ -> failwith "report must be object"
    in
    check
      "good readiness executable ABI accepted"
      (String.equal
         (string_value "executable_abi_status" readiness)
         "accepted");
    check
      "good executable ABI gate accepted"
      (String.equal (gate_status "executable_abi_binding_gate" report) "accepted");
    check
      "good ABI gate accepted"
      (String.equal (gate_status "abi_declaration_binding_gate" report) "accepted"))

let check_dynamic_q1_effort_vector () =
  with_temp_dir (fun dir ->
    let failure_cases = q1_failure_cases ~lower_effort:200 () in
    let code, report =
      run_conformance
        dir
        (q1_template ~k:512 ~failure_cases ())
        [
          "--strict-effort";
          "--include-failures";
          "--require-failure-cases";
          "--require-profile-roots-bound";
        ]
    in
    check "dynamic Q1 effort runner exits zero" (code = 0);
    let result = first_result report in
    check
      "dynamic Q1 expected program effort"
      (int_value "expected_effort" result = 202);
    check
      "dynamic Q1 observed program effort"
      (int_value "observed_effort" result = 202);
    check
      "dynamic Q1 expected opcode effort"
      (int_value "expected_opcode_effort" result = 201);
    check
      "dynamic Q1 observed opcode effort"
      (int_value "observed_opcode_effort" result = 201);
    let lower_effort = failure_case_fields result "lower_effort_limit" in
    check
      "dynamic Q1 lower effort reaches VM"
      (String.equal (string_value "observed" lower_effort) "vm_rejected"))

let check_q1_reject_failure_requires_full_output_span () =
  with_temp_dir (fun dir ->
    let code, report =
      run_conformance
        dir
        (q1_template
           ~m:2
           ~n:3
           ~r1:6
           ~failure_cases:(q1_failure_cases ~output_cells:1 ())
           ())
        [
          "--strict-effort";
          "--include-failures";
          "--require-failure-cases";
          "--require-profile-roots-bound";
        ]
    in
    check "partial output span runner exits nonzero" (code = 1);
    let result = first_result report in
    let rejected =
      failure_case_fields result "nonfinite_input_nan"
    in
    check
      "partial output span failure rejected"
      (String.equal (string_value "status" rejected) "rejected");
    check
      "partial output span marked uncovered"
      (String.equal
         (string_value "output_span_status" rejected)
         "not_covered");
    check
      "partial output span blocker"
      (List.mem
         "q1_failure_case_output_span_not_covered"
         (string_list "output_span_blockers" rejected)))

let check_truncated_decoded_input_manifest_reports_ingress_rejected () =
  with_temp_dir (fun dir ->
    let failure_cases =
      q1_failure_cases ()
      @ [
        reject_case
          "insufficient_input_bytes"
          [
            mutation
              "truncate_input_manifest"
              "lhs"
              ["truncate_bytes", `Int 1];
          ];
      ]
    in
    let code, report =
      run_conformance
        dir
        (q1_template ~failure_cases ())
        [
          "--strict-effort";
          "--include-failures";
          "--require-failure-cases";
          "--require-profile-roots-bound";
        ]
    in
    check "decoded input truncation runner exits zero" (code = 0);
    let result = first_result report in
    let truncated =
      failure_case_fields result "insufficient_input_bytes"
    in
    check
      "decoded input truncation accepted"
      (String.equal (string_value "status" truncated) "accepted");
    check
      "decoded input truncation is counted"
      (bool_value "counted" truncated);
    check
      "decoded input truncation reports ingress rejection"
      (String.equal (string_value "observed" truncated) "ingress_rejected");
    check
      "decoded input truncation authority is modeled"
      (String.equal
         (string_value "ingress_rejection_authority" truncated)
         "modeled_direct_runner_pre_ingress");
    (match list_value "mutation_results" truncated with
     | [`Assoc mutation_result] ->
       check
         "decoded input truncation mutation status"
         (String.equal
            (string_value "status" mutation_result)
            "ingress_rejected");
       check
         "decoded input truncation mutation authority"
         (String.equal
            (string_value "authority" mutation_result)
            "modeled_direct_runner_pre_ingress")
     | _ -> failwith "expected one mutation result"))

let check_zero_decoded_input_truncation_is_not_ingress_rejected () =
  with_temp_dir (fun dir ->
    let failure_cases =
      q1_failure_cases ()
      @ [
        reject_case
          "zero_decoded_input_truncation"
          [
            mutation
              "truncate_input_manifest"
              "lhs"
              ["truncate_bytes", `Int 0];
          ];
      ]
    in
    let code, report =
      run_conformance
        dir
        (q1_template ~failure_cases ())
        [
          "--strict-effort";
          "--include-failures";
          "--require-failure-cases";
          "--require-profile-roots-bound";
        ]
    in
    check "zero decoded truncation runner exits nonzero" (code = 1);
    let result = first_result report in
    let truncated =
      failure_case_fields result "zero_decoded_input_truncation"
    in
    check
      "zero decoded truncation rejected"
      (String.equal (string_value "status" truncated) "rejected");
    check
      "zero decoded truncation reaches VM"
      (String.equal (string_value "observed" truncated) "vm_accepted");
    check
      "zero decoded truncation is not ingress evidence"
      (String.equal
         (string_value "ingress_rejection_authority" truncated)
         "not_applicable"))

let check_require_failure_cases_rejects_missing_q1_case () =
  with_temp_dir (fun dir ->
    let failure_cases =
      List.filter
        (function
          | `Assoc fields ->
            not
              (String.equal
                 (string_value "case" fields)
                 "nonfinite_fp16_scale")
          | _ -> true)
        (q1_failure_cases ())
    in
    let code, report =
      run_conformance
        dir
        (q1_template ~failure_cases ())
        [
          "--strict-effort";
          "--include-failures";
          "--require-failure-cases";
          "--require-profile-roots-bound";
        ]
    in
    check "missing Q1 failure case runner exits nonzero" (code = 1);
    (match report with
     | `Assoc fields ->
       let gate = assoc_json "failure_case_gate" fields in
       check
         "missing Q1 failure gate rejected"
         (String.equal (string_value "status" gate) "rejected");
       check
         "missing Q1 failure gate blocker"
         (List.mem
            "q1_failure_case_missing_nonfinite_fp16_scale"
            (string_list "blockers" gate));
       let readiness = assoc_json "validator_readiness_gate" fields in
       let readiness_blockers = string_list "blockers" readiness in
       check
         "missing Q1 failure readiness generic blocker"
         (List.mem "required_failure_case_contract_rejected" readiness_blockers);
       check
         "missing Q1 failure next blocker"
         (String.equal
            (string_value "next_blocker" readiness)
            "q1_failure_case_missing_nonfinite_fp16_scale")
     | _ -> failwith "report must be object"))

let check_require_failure_cases_rejects_wrong_q1_expectation () =
  with_temp_dir (fun dir ->
    let failure_cases =
      List.map
        (replace_failure_expected
           "nonfinite_fp16_scale"
           "accept_from_snapshot_wrong")
        (q1_failure_cases ())
    in
    let code, report =
      run_conformance
        dir
        (q1_template ~failure_cases ())
        [
          "--strict-effort";
          "--include-failures";
          "--require-failure-cases";
          "--require-profile-roots-bound";
        ]
    in
    check "wrong Q1 expectation runner exits nonzero" (code = 1);
    (match report with
     | `Assoc fields ->
       let gate = assoc_json "failure_case_gate" fields in
       check
         "wrong Q1 expectation gate rejected"
         (String.equal (string_value "status" gate) "rejected");
       check
         "wrong Q1 expectation blocker"
         (List.mem
         "q1_failure_case_expected_mismatch_nonfinite_fp16_scale"
            (string_list "blockers" gate))
     | _ -> failwith "report must be object"))

let check_require_failure_cases_rejects_wrong_q1_mutation_shape () =
  with_temp_dir (fun dir ->
    let failure_cases =
      List.map
        (replace_failure_mutations
           "negative_byte_offset"
           [
             mutation
               "set_scalar_param"
               "parameter_addresses_and_scalar_params.values.k"
               ["value", `Int 127];
           ])
        (q1_failure_cases ())
    in
    let code, report =
      run_conformance
        dir
        (q1_template ~failure_cases ())
        [
          "--strict-effort";
          "--include-failures";
          "--require-failure-cases";
          "--require-profile-roots-bound";
        ]
    in
    check "wrong Q1 mutation shape runner exits nonzero" (code = 1);
    let result = first_result report in
    let negative_offset = failure_case_fields result "negative_byte_offset" in
    check
      "wrong Q1 mutation shape row rejected"
      (String.equal
         (string_value "mutation_shape_status" negative_offset)
         "rejected");
    check
      "wrong Q1 mutation shape row blocker"
      (List.mem
         "q1_failure_case_mutation_mismatch_negative_byte_offset"
         (string_list "mutation_shape_blockers" negative_offset));
    (match report with
     | `Assoc fields ->
       let gate = assoc_json "failure_case_gate" fields in
       check
         "wrong Q1 mutation shape gate rejected"
         (String.equal (string_value "status" gate) "rejected");
       check
         "wrong Q1 mutation shape gate blocker"
         (List.mem
            "q1_failure_case_mutation_mismatch_negative_byte_offset"
            (string_list "blockers" gate))
     | _ -> failwith "report must be object"))

let check_require_failure_cases_rejects_composite_q1_mutation_shape () =
  with_temp_dir (fun dir ->
    let failure_cases =
      List.map
        (replace_failure_mutations
           "negative_byte_offset"
           [
             mutation
               "set_scalar_param"
               "parameter_addresses_and_scalar_params.values.byte_offset"
               ["value", `Int (-1)];
             mutation
               "set_scalar_param"
               "parameter_addresses_and_scalar_params.values.k"
               ["value", `Int 127];
           ])
        (q1_failure_cases ())
    in
    let code, report =
      run_conformance
        dir
        (q1_template ~failure_cases ())
        [
          "--strict-effort";
          "--include-failures";
          "--require-failure-cases";
          "--require-profile-roots-bound";
        ]
    in
    check "composite Q1 mutation shape runner exits nonzero" (code = 1);
    let result = first_result report in
    let negative_offset = failure_case_fields result "negative_byte_offset" in
    check
      "composite Q1 mutation shape row rejected"
      (String.equal
         (string_value "mutation_shape_status" negative_offset)
         "rejected");
    check
      "composite Q1 mutation shape row blocker"
      (List.mem
         "q1_failure_case_mutation_mismatch_negative_byte_offset"
         (string_list "mutation_shape_blockers" negative_offset)))

let check_require_failure_cases_rejects_exact_partial_alias_shape () =
  with_temp_dir (fun dir ->
    let failure_cases =
      List.map
        (replace_failure_mutations
           "partial_output_input_aliasing"
           [
             mutation
               "set_output_base_to_first_input_base_plus"
               "output.base_address"
               ["offset_cells", `Int 0];
           ])
        (q1_failure_cases ())
    in
    let code, report =
      run_conformance
        dir
        (q1_template ~failure_cases ())
        [
          "--strict-effort";
          "--include-failures";
          "--require-failure-cases";
          "--require-profile-roots-bound";
        ]
    in
    check "exact partial alias runner exits nonzero" (code = 1);
    let result = first_result report in
    let partial_alias =
      failure_case_fields result "partial_output_input_aliasing"
    in
    check
      "exact partial alias row rejected"
      (String.equal
         (string_value "mutation_shape_status" partial_alias)
         "rejected");
    check
      "exact partial alias row blocker"
      (List.mem
         "q1_failure_case_mutation_mismatch_partial_output_input_aliasing"
         (string_list "mutation_shape_blockers" partial_alias)))

let check_accept_snapshot_requires_exact_output () =
  with_temp_dir (fun dir ->
    let bad_sha = different_sha (sha256 expected_output) in
    let code, report =
      run_conformance
        dir
        (q1_template () |> replace_output_subspan_sha bad_sha)
        [
          "--strict-effort";
          "--include-failures";
          "--require-failure-cases";
          "--require-profile-roots-bound";
        ]
    in
    check "bad snapshot output runner exits nonzero" (code = 1);
    let result = first_result report in
    let alias = failure_case_fields result "output_input_aliasing" in
    check
      "bad exact alias snapshot rejected"
      (String.equal (string_value "status" alias) "rejected");
    check
      "bad exact alias snapshot mismatch"
      (String.equal
         (string_value "snapshot_output_status" alias)
         "mismatch");
    (match report with
     | `Assoc fields ->
       let gate = assoc_json "failure_case_gate" fields in
       check
         "bad snapshot failure gate rejected"
         (String.equal (string_value "status" gate) "rejected");
       check
         "bad snapshot failure gate blocker"
         (List.mem
            "q1_failure_case_rejected_output_input_aliasing"
            (string_list "blockers" gate))
     | _ -> failwith "report must be object"))

let check_stale_abi_is_visible_in_executable_report () =
  with_temp_dir (fun dir ->
    let code, report =
      run_conformance
        dir
        (q1_template ~session_abi_root:(hex_root '3') ())
        ["--strict-effort"; "--require-profile-roots-bound"]
    in
    check "stale ABI runner still exits zero without readiness gate" (code = 0);
    let result = first_result report in
    check "stale ABI execution accepted" (String.equal (string_value "status" result) "accepted");
    let abi = assoc_json "abi_declaration_binding" result in
    check "stale ABI declaration unbound" (String.equal (string_value "status" abi) "unbound");
    let blockers =
      list_value "blockers" abi
      |> List.map (function
        | `String value -> value
        | _ -> failwith "blocker must be a string")
    in
    check
      "stale ABI declaration reason"
      (List.mem "session_abi_root_mismatch" blockers);
    check
      "stale ABI gate rejected"
      (String.equal (gate_status "abi_declaration_binding_gate" report) "rejected"))

let check_readiness_gate_rejects_stale_abi () =
  with_temp_dir (fun dir ->
    let code, report =
      run_conformance
        dir
        (q1_template ~output_count_unit:"f64_cells" ())
        [
          "--strict-effort";
          "--require-profile-roots-bound";
          "--require-validator-readiness";
        ]
    in
    check "stale ABI readiness exits nonzero" (code = 1);
    check
      "stale ABI readiness gate rejected"
      (String.equal (gate_status "validator_readiness_gate" report) "rejected");
    match report with
    | `Assoc fields ->
      let readiness = assoc_json "validator_readiness_gate" fields in
      let blockers =
        list_value "blockers" readiness
        |> List.map (function
          | `String value -> value
          | _ -> failwith "blocker must be a string")
      in
      check
        "stale ABI readiness blocker"
        (List.mem "unbound_abi_declaration_binding" blockers)
    | _ -> failwith "report must be object")

let check_readiness_gate_reports_executable_abi_mismatch () =
  with_temp_dir (fun dir ->
    let code, report =
      run_conformance
        dir
        (q1_template ~r1:2 ())
        [
          "--strict-effort";
          "--include-failures";
          "--require-failure-cases";
          "--require-profile-roots-bound";
          "--require-validator-readiness";
        ]
    in
    check "bad executable ABI runner exits nonzero" (code = 1);
    (match report with
     | `Assoc fields ->
       let readiness = assoc_json "validator_readiness_gate" fields in
       check
         "bad executable ABI readiness status"
         (String.equal
            (string_value "executable_abi_status" readiness)
            "rejected");
       let blockers = string_list "blockers" readiness in
       check
         "bad executable ABI readiness blocker"
         (List.mem "executable_abi_binding_mismatch" blockers);
       let gate = assoc_json "executable_abi_binding_gate" fields in
       check
         "bad executable ABI top-level gate"
         (String.equal (string_value "status" gate) "rejected")
     | _ -> failwith "report must be object"))

let check_missing_output_subspan_reports_rejected () =
  with_temp_dir (fun dir ->
    let code, report =
      run_conformance
        dir
        (q1_template () |> replace_output_subspan_base 10001)
        [
          "--strict-effort";
          "--include-failures";
          "--require-failure-cases";
          "--require-profile-roots-bound";
        ]
    in
    check "missing output subspan runner exits nonzero" (code = 1);
    let result = first_result report in
    check
      "missing output subspan result rejected"
      (String.equal (string_value "status" result) "rejected");
    check
      "missing output subspan output mismatch"
      (String.equal (string_value "output_status" result) "mismatch");
    match assoc_value "subspans" result with
    | `List [`Assoc subspan] ->
      check
        "missing output subspan root marked false"
        (not (bool_value "root_matched" subspan));
      check
        "missing output subspan marked false"
        (not (bool_value "matched" subspan));
      check
        "missing output subspan records error"
        (String.equal
           (string_value "error" subspan)
           "missing output cell: base 10001 index 0")
    | _ -> failwith "expected one subspan")

let check_pinned_cross_platform_matrix_is_consumed () =
  with_temp_dir (fun dir ->
    let code, seed_report =
      run_conformance
        dir
        (q1_template ())
        [
          "--strict-effort";
          "--include-failures";
          "--require-failure-cases";
          "--require-profile-roots-bound";
        ]
    in
    check "matrix seed run exits zero" (code = 0);
    match seed_report with
    | `Assoc seed_fields ->
      let matrix =
        accepted_matrix
          ~profile_catalog_root:(string_value "profile_catalog_root" seed_fields)
          ~template_corpus_root:(string_value "template_corpus_root" seed_fields)
          ~result_signature_sha256:(string_value "result_signature_sha256" seed_fields)
      in
      let matrix_path, matrix_sha = write_matrix dir matrix in
      let code, report =
        run_conformance
          dir
          (q1_template ())
          [
            "--strict-effort";
            "--include-failures";
            "--require-failure-cases";
            "--require-profile-roots-bound";
            "--cross-platform-matrix";
            Filename.quote matrix_path;
            "--expected-cross-platform-matrix-sha256";
            matrix_sha;
            "--require-validator-readiness";
          ]
      in
      check "matrix-backed readiness exits nonzero" (code = 1);
      (match report with
       | `Assoc fields ->
         let readiness = assoc_json "validator_readiness_gate" fields in
         let cross_platform = assoc_json "cross_platform_evidence" readiness in
         check
           "matrix-backed cross-platform accepted"
           (String.equal (string_value "status" cross_platform) "accepted");
         check
           "matrix sha is pinned"
           (String.equal (string_value "matrix_sha256_status" cross_platform) "accepted");
         check
           "matrix-backed declared platform count"
           (int_value "matrix_distinct_platform_count" cross_platform = 2);
         check
           "matrix-backed row platform count"
           (int_value "row_distinct_platform_count" cross_platform = 2);
         check
           "matrix-backed declared runner count"
           (int_value "matrix_distinct_runner_executable_count" cross_platform = 2);
         check
           "matrix-backed row runner count"
           (int_value "row_distinct_runner_executable_count" cross_platform = 2);
         check
           "matrix-backed declared platform runner observation count"
           (int_value "matrix_distinct_platform_runner_observation_count" cross_platform = 2);
         check
           "matrix-backed row platform runner observation count"
           (int_value "row_distinct_platform_runner_observation_count" cross_platform = 2);
         check
           "matrix-backed declared signature count"
           (int_value "matrix_result_signature_count" cross_platform = 1);
         check
           "matrix-backed row signature count"
           (int_value "row_result_signature_count" cross_platform = 1);
         let blockers = string_list "blockers" readiness in
         check
           "matrix-backed readiness no longer blocked by missing matrix"
           (not (List.mem "cross_platform_conformance_missing" blockers));
         check
           "matrix-backed readiness still needs consensus promotion"
           (List.mem "consensus_candidate_profile_gates" blockers);
         check
           "matrix-backed readiness next blocker"
           (String.equal
              (string_value "next_blocker" readiness)
              "consensus_candidate_profile_gates")
       | _ -> failwith "report must be object")
    | _ -> failwith "seed report must be object")

let check_cross_platform_matrix_without_pin_rejects () =
  with_temp_dir (fun dir ->
    let code, seed_report =
      run_conformance
        dir
        (q1_template ())
        [
          "--strict-effort";
          "--include-failures";
          "--require-failure-cases";
          "--require-profile-roots-bound";
        ]
    in
    check "matrix unpinned seed run exits zero" (code = 0);
    match seed_report with
    | `Assoc seed_fields ->
      let matrix =
        accepted_matrix
          ~profile_catalog_root:(string_value "profile_catalog_root" seed_fields)
          ~template_corpus_root:(string_value "template_corpus_root" seed_fields)
          ~result_signature_sha256:(string_value "result_signature_sha256" seed_fields)
      in
      let matrix_path, _ = write_matrix dir matrix in
      let code, report =
        run_conformance
          dir
          (q1_template ())
          [
            "--strict-effort";
            "--include-failures";
            "--require-failure-cases";
            "--require-profile-roots-bound";
            "--cross-platform-matrix";
            Filename.quote matrix_path;
            "--require-validator-readiness";
          ]
      in
      check "matrix unpinned exits nonzero" (code = 1);
      (match report with
       | `Assoc fields ->
         let readiness = assoc_json "validator_readiness_gate" fields in
         let cross_platform = assoc_json "cross_platform_evidence" readiness in
         check
           "matrix unpinned rejected"
           (String.equal (string_value "status" cross_platform) "rejected");
         check
           "matrix unpinned status"
           (String.equal (string_value "matrix_sha256_status" cross_platform) "rejected");
         let blockers = string_list "blockers" cross_platform in
         check
           "matrix unpinned blocker"
           (List.mem "matrix_sha256_unpinned_or_mismatch" blockers)
       | _ -> failwith "report must be object")
    | _ -> failwith "seed report must be object")

let check_cross_platform_matrix_sha_mismatch_rejects () =
  with_temp_dir (fun dir ->
    let code, seed_report =
      run_conformance
        dir
        (q1_template ())
        [
          "--strict-effort";
          "--include-failures";
          "--require-failure-cases";
          "--require-profile-roots-bound";
        ]
    in
    check "matrix mismatch seed run exits zero" (code = 0);
    match seed_report with
    | `Assoc seed_fields ->
      let matrix =
        accepted_matrix
          ~profile_catalog_root:(string_value "profile_catalog_root" seed_fields)
          ~template_corpus_root:(string_value "template_corpus_root" seed_fields)
          ~result_signature_sha256:(string_value "result_signature_sha256" seed_fields)
      in
      let matrix_path, matrix_sha = write_matrix dir matrix in
      let code, report =
        run_conformance
          dir
          (q1_template ())
          [
            "--strict-effort";
            "--include-failures";
            "--require-failure-cases";
            "--require-profile-roots-bound";
            "--cross-platform-matrix";
            Filename.quote matrix_path;
            "--expected-cross-platform-matrix-sha256";
            different_sha matrix_sha;
            "--require-validator-readiness";
          ]
      in
      check "matrix sha mismatch exits nonzero" (code = 1);
      (match report with
       | `Assoc fields ->
         let readiness = assoc_json "validator_readiness_gate" fields in
         let cross_platform = assoc_json "cross_platform_evidence" readiness in
         check
           "matrix sha mismatch rejected"
           (String.equal (string_value "status" cross_platform) "rejected");
         check
           "matrix sha mismatch status"
           (String.equal (string_value "matrix_sha256_status" cross_platform) "rejected");
         let blockers = string_list "blockers" cross_platform in
         check
           "matrix sha mismatch blocker"
           (List.mem "matrix_sha256_unpinned_or_mismatch" blockers)
       | _ -> failwith "report must be object")
    | _ -> failwith "seed report must be object")

let check_cross_platform_matrix_signature_schema_mismatch_rejects () =
  with_temp_dir (fun dir ->
    let code, seed_report =
      run_conformance
        dir
        (q1_template ())
        [
          "--strict-effort";
          "--include-failures";
          "--require-failure-cases";
          "--require-profile-roots-bound";
        ]
    in
    check "matrix schema mismatch seed run exits zero" (code = 0);
    match seed_report with
    | `Assoc seed_fields ->
      let matrix =
        accepted_matrix
          ~profile_catalog_root:(string_value "profile_catalog_root" seed_fields)
          ~template_corpus_root:(string_value "template_corpus_root" seed_fields)
          ~result_signature_sha256:(string_value "result_signature_sha256" seed_fields)
        |> replace_assoc_field
             "result_signature_schema"
             (`String "octra.inference.conformance.result-signature.v1")
      in
      let matrix_path, matrix_sha = write_matrix dir matrix in
      let code, report =
        run_conformance
          dir
          (q1_template ())
          [
            "--strict-effort";
            "--include-failures";
            "--require-failure-cases";
            "--require-profile-roots-bound";
            "--cross-platform-matrix";
            Filename.quote matrix_path;
            "--expected-cross-platform-matrix-sha256";
            matrix_sha;
            "--require-validator-readiness";
          ]
      in
      check "matrix schema mismatch exits nonzero" (code = 1);
      (match report with
       | `Assoc fields ->
         let readiness = assoc_json "validator_readiness_gate" fields in
         let cross_platform = assoc_json "cross_platform_evidence" readiness in
         check
           "matrix signature schema rejected"
           (String.equal
              (string_value "result_signature_schema_status" cross_platform)
              "rejected");
         check
           "matrix signature schema blocker"
           (List.mem
              "matrix_result_signature_schema_mismatch"
              (string_list "blockers" cross_platform))
       | _ -> failwith "report must be object")
    | _ -> failwith "seed report must be object")

let check_cross_platform_matrix_row_envelope_mismatch_rejects () =
  with_temp_dir (fun dir ->
    let code, seed_report =
      run_conformance
        dir
        (q1_template ())
        [
          "--strict-effort";
          "--include-failures";
          "--require-failure-cases";
          "--require-profile-roots-bound";
        ]
    in
    check "matrix row envelope mismatch seed run exits zero" (code = 0);
    match seed_report with
    | `Assoc seed_fields ->
      let matrix =
        accepted_matrix
          ~profile_catalog_root:(string_value "profile_catalog_root" seed_fields)
          ~template_corpus_root:(string_value "template_corpus_root" seed_fields)
          ~result_signature_sha256:(string_value "result_signature_sha256" seed_fields)
        |> replace_report_field
             0
             "result_signature_schema"
             (`String "octra.inference.conformance.result-signature.v1")
        |> replace_report_field
             1
             "result_opcodes"
             (`List [`String "RMSNORM_FP_EPS"])
      in
      let matrix_path, matrix_sha = write_matrix dir matrix in
      let code, report =
        run_conformance
          dir
          (q1_template ())
          [
            "--strict-effort";
            "--include-failures";
            "--require-failure-cases";
            "--require-profile-roots-bound";
            "--cross-platform-matrix";
            Filename.quote matrix_path;
            "--expected-cross-platform-matrix-sha256";
            matrix_sha;
            "--require-validator-readiness";
          ]
      in
      check "matrix row envelope mismatch exits nonzero" (code = 1);
      (match report with
       | `Assoc fields ->
         let readiness = assoc_json "validator_readiness_gate" fields in
         let cross_platform = assoc_json "cross_platform_evidence" readiness in
         check
           "matrix row signature schema rejected"
           (String.equal
              (string_value "row_result_signature_schema_status" cross_platform)
              "rejected");
         check
           "matrix row opcode rejected"
           (String.equal
              (string_value "row_opcode_coverage_status" cross_platform)
              "rejected");
         let blockers = string_list "blockers" cross_platform in
         check
           "matrix row signature schema blocker"
           (List.mem "matrix_row_result_signature_schema_mismatch" blockers);
         check
           "matrix row opcode blocker"
           (List.mem "matrix_row_opcode_scope_mismatch" blockers)
       | _ -> failwith "report must be object")
    | _ -> failwith "seed report must be object")

let check_cross_platform_matrix_local_signature_mismatch_rejects () =
  with_temp_dir (fun dir ->
    let code, seed_report =
      run_conformance
        dir
        (q1_template ())
        [
          "--strict-effort";
          "--include-failures";
          "--require-failure-cases";
          "--require-profile-roots-bound";
        ]
    in
    check "matrix local signature mismatch seed run exits zero" (code = 0);
    match seed_report with
    | `Assoc seed_fields ->
      let matrix =
        accepted_matrix
          ~profile_catalog_root:(string_value "profile_catalog_root" seed_fields)
          ~template_corpus_root:(string_value "template_corpus_root" seed_fields)
          ~result_signature_sha256:
            (different_sha (string_value "result_signature_sha256" seed_fields))
      in
      let matrix_path, matrix_sha = write_matrix dir matrix in
      let code, report =
        run_conformance
          dir
          (q1_template ())
          [
            "--strict-effort";
            "--include-failures";
            "--require-failure-cases";
            "--require-profile-roots-bound";
            "--cross-platform-matrix";
            Filename.quote matrix_path;
            "--expected-cross-platform-matrix-sha256";
            matrix_sha;
            "--require-validator-readiness";
          ]
      in
      check "matrix local signature mismatch exits nonzero" (code = 1);
      (match report with
       | `Assoc fields ->
         let readiness = assoc_json "validator_readiness_gate" fields in
         let cross_platform = assoc_json "cross_platform_evidence" readiness in
         check
           "matrix local signature rejected"
           (String.equal
              (string_value "local_result_signature_status" cross_platform)
              "rejected");
         check
           "matrix local signature blocker"
           (List.mem
              "matrix_local_result_signature_mismatch"
              (string_list "blockers" cross_platform))
       | _ -> failwith "report must be object")
    | _ -> failwith "seed report must be object")

let check_cross_platform_matrix_forged_runner_count_rejects () =
  with_temp_dir (fun dir ->
    let code, seed_report =
      run_conformance
        dir
        (q1_template ())
        [
          "--strict-effort";
          "--include-failures";
          "--require-failure-cases";
          "--require-profile-roots-bound";
        ]
    in
    check "matrix forged count seed run exits zero" (code = 0);
    match seed_report with
    | `Assoc seed_fields ->
      let matrix =
        accepted_matrix
          ~profile_catalog_root:(string_value "profile_catalog_root" seed_fields)
          ~template_corpus_root:(string_value "template_corpus_root" seed_fields)
          ~result_signature_sha256:(string_value "result_signature_sha256" seed_fields)
        |> replace_assoc_field "distinct_runner_executable_count" (`Int 3)
      in
      let matrix_path, matrix_sha = write_matrix dir matrix in
      let code, report =
        run_conformance
          dir
          (q1_template ())
          [
            "--strict-effort";
            "--include-failures";
            "--require-failure-cases";
            "--require-profile-roots-bound";
            "--cross-platform-matrix";
            Filename.quote matrix_path;
            "--expected-cross-platform-matrix-sha256";
            matrix_sha;
            "--require-validator-readiness";
          ]
      in
      check "matrix forged count exits nonzero" (code = 1);
      (match report with
       | `Assoc fields ->
         let readiness = assoc_json "validator_readiness_gate" fields in
         let cross_platform = assoc_json "cross_platform_evidence" readiness in
         check
           "matrix forged count rejected"
           (String.equal (string_value "status" cross_platform) "rejected");
         check
           "matrix forged count row aggregate rejected"
           (String.equal (string_value "row_aggregate_status" cross_platform) "rejected");
         check
           "matrix forged count reports declared runner count"
           (int_value "matrix_distinct_runner_executable_count" cross_platform = 3);
         check
           "matrix forged count reports row runner count"
           (int_value "row_distinct_runner_executable_count" cross_platform = 2);
         let blockers = string_list "blockers" cross_platform in
         check
           "matrix forged count blocker"
           (List.mem "matrix_runner_executable_count_mismatch" blockers)
       | _ -> failwith "report must be object")
    | _ -> failwith "seed report must be object")

let check_cross_platform_matrix_forged_observation_count_rejects () =
  with_temp_dir (fun dir ->
    let code, seed_report =
      run_conformance
        dir
        (q1_template ())
        [
          "--strict-effort";
          "--include-failures";
          "--require-failure-cases";
          "--require-profile-roots-bound";
        ]
    in
    check "matrix forged observation count seed run exits zero" (code = 0);
    match seed_report with
    | `Assoc seed_fields ->
      let matrix =
        accepted_matrix
          ~profile_catalog_root:(string_value "profile_catalog_root" seed_fields)
          ~template_corpus_root:(string_value "template_corpus_root" seed_fields)
          ~result_signature_sha256:(string_value "result_signature_sha256" seed_fields)
        |> replace_assoc_field
             "distinct_platform_runner_observation_count"
             (`Int 3)
      in
      let matrix_path, matrix_sha = write_matrix dir matrix in
      let code, report =
        run_conformance
          dir
          (q1_template ())
          [
            "--strict-effort";
            "--include-failures";
            "--require-failure-cases";
            "--require-profile-roots-bound";
            "--cross-platform-matrix";
            Filename.quote matrix_path;
            "--expected-cross-platform-matrix-sha256";
            matrix_sha;
            "--require-validator-readiness";
          ]
      in
      check "matrix forged observation count exits nonzero" (code = 1);
      (match report with
       | `Assoc fields ->
         let readiness = assoc_json "validator_readiness_gate" fields in
         let cross_platform = assoc_json "cross_platform_evidence" readiness in
         check
           "matrix forged observation count rejected"
           (String.equal (string_value "status" cross_platform) "rejected");
         check
           "matrix forged observation count row aggregate rejected"
           (String.equal (string_value "row_aggregate_status" cross_platform) "rejected");
         check
           "matrix forged observation count declared"
           (int_value "matrix_distinct_platform_runner_observation_count" cross_platform = 3);
         check
           "matrix forged observation count observed"
           (int_value "row_distinct_platform_runner_observation_count" cross_platform = 2);
         let blockers = string_list "blockers" cross_platform in
         check
           "matrix forged observation count blocker"
           (List.mem "matrix_platform_runner_observation_count_mismatch" blockers)
       | _ -> failwith "report must be object")
    | _ -> failwith "seed report must be object")

let check_cross_platform_matrix_rejects_insufficient_row_diversity () =
  with_temp_dir (fun dir ->
    let code, seed_report =
      run_conformance
        dir
        (q1_template ())
        [
          "--strict-effort";
          "--include-failures";
          "--require-failure-cases";
          "--require-profile-roots-bound";
        ]
    in
    check "matrix row diversity seed run exits zero" (code = 0);
    match seed_report with
    | `Assoc seed_fields ->
      let matrix =
        accepted_matrix
          ~profile_catalog_root:(string_value "profile_catalog_root" seed_fields)
          ~template_corpus_root:(string_value "template_corpus_root" seed_fields)
          ~result_signature_sha256:(string_value "result_signature_sha256" seed_fields)
        |> replace_assoc_field "distinct_platform_count" (`Int 1)
        |> replace_assoc_field "distinct_runner_executable_count" (`Int 1)
        |> replace_report_field
             1
             "platform_key"
             (`String "4.14.2|Unix|Darwin|1.0|arm64|64|false|native")
        |> replace_report_field 1 "runner_executable_sha256" (`String (hex_root '1'))
      in
      let matrix_path, matrix_sha = write_matrix dir matrix in
      let code, report =
        run_conformance
          dir
          (q1_template ())
          [
            "--strict-effort";
            "--include-failures";
            "--require-failure-cases";
            "--require-profile-roots-bound";
            "--cross-platform-matrix";
            Filename.quote matrix_path;
            "--expected-cross-platform-matrix-sha256";
            matrix_sha;
            "--require-validator-readiness";
          ]
      in
      check "matrix row diversity exits nonzero" (code = 1);
      (match report with
       | `Assoc fields ->
         let readiness = assoc_json "validator_readiness_gate" fields in
         let cross_platform = assoc_json "cross_platform_evidence" readiness in
         check
           "matrix row platform minimum rejected"
           (String.equal
              (string_value "row_distinct_platform_status" cross_platform)
              "rejected");
         check
           "matrix row runner minimum rejected"
           (String.equal
              (string_value "row_distinct_runner_executable_status" cross_platform)
              "rejected");
         check
           "matrix row platform runner observation minimum rejected"
           (String.equal
              (string_value "row_distinct_platform_runner_observation_status" cross_platform)
              "rejected");
         let blockers = string_list "blockers" cross_platform in
         check
           "matrix row platform minimum blocker"
           (List.mem "matrix_insufficient_distinct_platforms" blockers);
         check
           "matrix row runner minimum blocker"
           (List.mem "matrix_insufficient_distinct_runner_executables" blockers);
         check
           "matrix row platform runner observation minimum blocker"
           (List.mem
              "matrix_insufficient_distinct_platform_runner_observations"
              blockers)
       | _ -> failwith "report must be object")
    | _ -> failwith "seed report must be object")

let check_cross_platform_matrix_forged_row_root_rejects () =
  with_temp_dir (fun dir ->
    let code, seed_report =
      run_conformance
        dir
        (q1_template ())
        [
          "--strict-effort";
          "--include-failures";
          "--require-failure-cases";
          "--require-profile-roots-bound";
        ]
    in
    check "matrix forged row root seed run exits zero" (code = 0);
    match seed_report with
    | `Assoc seed_fields ->
      let matrix =
        accepted_matrix
          ~profile_catalog_root:(string_value "profile_catalog_root" seed_fields)
          ~template_corpus_root:(string_value "template_corpus_root" seed_fields)
          ~result_signature_sha256:(string_value "result_signature_sha256" seed_fields)
        |> replace_first_report_field "profile_catalog_root" (`String (hex_root '9'))
      in
      let matrix_path, matrix_sha = write_matrix dir matrix in
      let code, report =
        run_conformance
          dir
          (q1_template ())
          [
            "--strict-effort";
            "--include-failures";
            "--require-failure-cases";
            "--require-profile-roots-bound";
            "--cross-platform-matrix";
            Filename.quote matrix_path;
            "--expected-cross-platform-matrix-sha256";
            matrix_sha;
            "--require-validator-readiness";
          ]
      in
      check "matrix forged row root exits nonzero" (code = 1);
      (match report with
       | `Assoc fields ->
         let readiness = assoc_json "validator_readiness_gate" fields in
         let cross_platform = assoc_json "cross_platform_evidence" readiness in
         check
           "matrix forged row root rejected"
           (String.equal (string_value "status" cross_platform) "rejected");
         check
           "matrix forged row root aggregate rejected"
           (String.equal (string_value "row_aggregate_status" cross_platform) "rejected");
         let blockers = string_list "blockers" cross_platform in
         check
           "matrix forged row root blocker"
           (List.mem "matrix_row_profile_catalog_mismatch" blockers)
       | _ -> failwith "report must be object")
    | _ -> failwith "seed report must be object")

let () =
  check_good_template_reports_bound_abi ();
  check_dynamic_q1_effort_vector ();
  check_q1_reject_failure_requires_full_output_span ();
  check_truncated_decoded_input_manifest_reports_ingress_rejected ();
  check_zero_decoded_input_truncation_is_not_ingress_rejected ();
  check_require_failure_cases_rejects_missing_q1_case ();
  check_require_failure_cases_rejects_wrong_q1_expectation ();
  check_require_failure_cases_rejects_wrong_q1_mutation_shape ();
  check_require_failure_cases_rejects_composite_q1_mutation_shape ();
  check_require_failure_cases_rejects_exact_partial_alias_shape ();
  check_accept_snapshot_requires_exact_output ();
  check_stale_abi_is_visible_in_executable_report ();
  check_readiness_gate_rejects_stale_abi ();
  check_readiness_gate_reports_executable_abi_mismatch ();
  check_missing_output_subspan_reports_rejected ();
  check_pinned_cross_platform_matrix_is_consumed ();
  check_cross_platform_matrix_without_pin_rejects ();
  check_cross_platform_matrix_sha_mismatch_rejects ();
  check_cross_platform_matrix_signature_schema_mismatch_rejects ();
  check_cross_platform_matrix_row_envelope_mismatch_rejects ();
  check_cross_platform_matrix_local_signature_mismatch_rejects ();
  check_cross_platform_matrix_forged_runner_count_rejects ();
  check_cross_platform_matrix_forged_observation_count_rejects ();
  check_cross_platform_matrix_rejects_insufficient_row_diversity ();
  check_cross_platform_matrix_forged_row_root_rejects ()
