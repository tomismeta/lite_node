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

let mkdir_if_missing path =
  if not (Sys.file_exists path) then Unix.mkdir path 0o700

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

let matrix_tool_path () =
  let candidates =
    [
      "_build/default/tools/inference_conformance_matrix.exe";
      "../tools/inference_conformance_matrix.exe";
    ]
  in
  match List.find_opt Sys.file_exists candidates with
  | Some path -> path
  | None -> failwith "missing inference_conformance_matrix.exe"

let check_tool_path () =
  let candidates =
    [
      "_build/default/tools/inference_conformance_check.exe";
      "../tools/inference_conformance_check.exe";
    ]
  in
  match List.find_opt Sys.file_exists candidates with
  | Some path -> path
  | None -> failwith "missing inference_conformance_check.exe"

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
    "root", `String (fixture_value_root ~name raw);
  ]

let expected_manifest ?layout name path raw =
  let fields =
    [
      "name", `String name;
      "path", `String path;
      "bytes", `Int (String.length raw);
      "sha256", `String (sha256 raw);
      "root", `String (fixture_value_root ~name raw);
    ]
  in
  match layout with
  | Some layout -> `Assoc (fields @ ["layout", `String layout])
  | None -> `Assoc fields

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
    ?(entrypoint = Abi.advance_entrypoint)
    ?(label = Abi.advance_label)
    ?(output_base_register = "r0")
    ?(output_count_register = "r1")
    ?(request_input_root_cell = Abi.input_root_cell)
    ?(m = 1) ?(k = 128) ?(n = 1) ?expected_effort
    ?failure_cases ?(output_count_unit = "cells") ?(r0 = 10000) ?(r1 = 1)
    ?(input_name = "lhs") () =
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
        "name", `String input_name;
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
      "entrypoint", `String entrypoint;
      "label", `Int label;
      "output_base_register", `String output_base_register;
      "output_count_register", `String output_count_register;
      "output_count_unit", `String output_count_unit;
      "request_input_root_cell", `Int request_input_root_cell;
    ];
    "output",
    `Assoc [
      "base_address", `Int 10000;
      "length_f64_cells", `Int (m * n);
      "abi_registers", `Assoc ["r0", `Int r0; "r1", `Int r1];
      "subspans",
      `List [
        `Assoc [
          "name", `String "expected";
          "base_address", `Int 10000;
          "length_f64_cells", `Int (m * n);
          "sha256", `String (sha256 expected_output);
          "root", `String (fixture_value_root ~name:"expected" expected_output);
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

let ends_with ~suffix value =
  let suffix_len = String.length suffix in
  let value_len = String.length value in
  value_len >= suffix_len
  && String.equal
       (String.sub value (value_len - suffix_len) suffix_len)
       suffix

let rec has_ref_field = function
  | `Assoc fields ->
    List.exists
      (fun (name, value) -> ends_with ~suffix:"_ref" name || has_ref_field value)
      fields
  | `List values -> List.exists has_ref_field values
  | _ -> false

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

let replace_assoc_field name value = function
  | `Assoc fields ->
    `Assoc
      ((name, value)
       :: List.filter (fun (key, _) -> not (String.equal key name)) fields)
  | _ -> failwith "json value must be an object"

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

let run_check_index dir template args =
  write_fixture dir template;
  let command =
    String.concat
      " "
      ([
         Filename.quote (check_tool_path ());
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

let run_matrix runner_reports args =
  let command =
    String.concat
      " "
      (List.map
         Filename.quote
         ((matrix_tool_path () :: [])
          @ (runner_reports
             |> List.map (fun path -> ["--runner-report"; path])
             |> List.concat)
          @ args))
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

let replace_platform_observation report ~system_name ~machine ~runner_sha =
  match report with
  | `Assoc fields ->
    let platform =
      assoc_value "platform" fields
      |> replace_assoc_field "system_name" (`String system_name)
      |> replace_assoc_field "machine" (`String machine)
      |> replace_assoc_field "runner_executable_sha256" (`String runner_sha)
    in
    replace_assoc_field "platform" platform report
  | _ -> failwith "report must be object"

let write_p0_plus_softmax_fixture
    ?scores
    ?expected_values
    dir
    case_name
    expected =
  let root = Filename.concat dir "p0-plus-fixtures" in
  let primitive_dir = Filename.concat root "softmax-fp" in
  let case_dir = Filename.concat primitive_dir case_name in
  mkdir_if_missing root;
  mkdir_if_missing primitive_dir;
  mkdir_if_missing case_dir;
  let scores_values =
    match scores with
    | Some values -> values
    | None -> [0.0]
  in
  let expected_values =
    match expected_values with
    | Some values -> values
    | None -> [expected]
  in
  let scores = f64_bytes scores_values in
  let expected = f64_bytes expected_values in
  let scores_path =
    Filename.concat "p0-plus-fixtures" ("softmax-fp/" ^ case_name ^ "/scores.f64le.bin")
  in
  let expected_path =
    Filename.concat
      "p0-plus-fixtures"
      ("softmax-fp/" ^ case_name ^ "/expected.f64le.bin")
  in
  write_file (Filename.concat dir scores_path) scores;
  write_file (Filename.concat dir expected_path) expected;
  let fixture_path =
    Filename.concat
      "p0-plus-fixtures"
      ("softmax-fp/" ^ case_name ^ "/fixture.cjson")
  in
  write_json
    (Filename.concat dir fixture_path)
    (`Assoc [
      "case", `String case_name;
      "opcode", `String "SOFTMAX_FP";
      "primitive", `String "softmax_fp";
      "parameters", `Assoc ["count", `Int (List.length scores_values)];
      "input_byte_manifests", `List [manifest "scores" scores_path scores];
      "expected_output_byte_manifests",
      `List [
        expected_manifest
          ~layout:
            (Printf.sprintf
               "f64le[%d]"
               (List.length expected_values))
          "expected_probabilities"
          expected_path
          expected;
      ];
    ]);
  fixture_path

let run_p0_plus dir fixtures =
  let pack_path = Filename.concat dir "p0-plus-pack.cjson" in
  write_json
    pack_path
    (`Assoc [
      "fixtures",
      `List
        (List.map
           (fun manifest -> `Assoc ["manifest", `String manifest])
           fixtures);
    ]);
  let command =
    String.concat
      " "
      [
        Filename.quote (tool_path ());
        "--p0-plus-pack";
        Filename.quote pack_path;
      ]
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

let producer_repair_hints report =
  match report with
  | `Assoc fields -> list_value "producer_repair_hints" fields
  | _ -> failwith "report must be object"

let first_repair_hint report =
  match producer_repair_hints report with
  | [`Assoc fields] -> fields
  | _ -> failwith "expected one producer repair hint"

let repair_fields hint =
  list_value "repairs" hint
  |> List.map (function
    | `Assoc fields -> fields
    | _ -> failwith "repair must be object")

let repair_for_field field repairs =
  match
    List.find_opt
      (fun repair -> String.equal (string_value "field" repair) field)
      repairs
  with
  | Some repair -> repair
  | None -> failwith ("missing repair field: " ^ field)

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
      (String.equal (gate_status "abi_declaration_binding_gate" report) "accepted");
    check
      "good report has no producer repair hints"
      (producer_repair_hints report = []))

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
        (q1_template ~failure_cases ~input_name:"input" ())
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
        (q1_template ~failure_cases ~input_name:"input" ())
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
            "q1_failure_case_missing_nonfinite_fp16_scale");
       let repairs = first_repair_hint report |> repair_fields in
       let missing_case =
         repair_for_field
           "expected_failure_atomicity_behavior[nonfinite_fp16_scale]"
           repairs
       in
       check
         "missing Q1 repair is case-specific"
         (String.equal
            (string_value "action" missing_case)
            "add_required_failure_case");
       check
         "missing Q1 repair case"
         (String.equal
            (string_value "case" missing_case)
            "nonfinite_fp16_scale");
      check
        "missing Q1 repair expected prefix"
        (String.equal
           (string_value "expected_prefix" missing_case)
           "reject_before_write");
      let required_case = assoc_json "required_case" missing_case in
      check
        "missing Q1 repair required case"
        (String.equal
           (string_value "case" required_case)
           "nonfinite_fp16_scale");
      check
        "missing Q1 repair required expected"
        (String.equal
           (string_value "expected" required_case)
           "reject_before_write");
      (match list_value "executable_mutations" required_case with
       | [`Assoc mutation_fields] ->
         check
           "missing Q1 repair mutation"
           (String.equal
              (string_value "mutation" mutation_fields)
              "replace_q1_scale_bits");
         check
           "missing Q1 repair mutation target"
           (String.equal
              (string_value "target" mutation_fields)
              "q1_owner[0..2]");
         check
           "missing Q1 repair mutation payload"
           (String.equal
              (string_value "value_hex_le" mutation_fields)
              "007c")
       | _ -> failwith "expected one required mutation");
      let repaired_code, repaired_report =
        run_check_index
          dir
          (q1_template ~failure_cases:(failure_cases @ [`Assoc required_case]) ())
          []
      in
      check "missing Q1 repair required case checks" (repaired_code = 0);
      (match repaired_report with
       | `Assoc repaired_fields ->
         check
           "missing Q1 repair required case accepted"
           (String.equal (string_value "status" repaired_fields) "accepted");
         check
           "missing Q1 repair required case has no issues"
           (int_value "issue_count" repaired_fields = 0)
       | _ -> failwith "checker report must be object")
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
            (string_list "blockers" gate));
       let repairs = first_repair_hint report |> repair_fields in
       let expectation =
         repair_for_field
           "expected_failure_atomicity_behavior[nonfinite_fp16_scale].expected"
           repairs
       in
       check
         "wrong Q1 expectation repair action"
         (String.equal
            (string_value "action" expectation)
            "set_expected_prefix");
       check
         "wrong Q1 expectation repair case"
         (String.equal
            (string_value "case" expectation)
            "nonfinite_fp16_scale");
       check
         "wrong Q1 expectation repair prefix"
         (String.equal
            (string_value "expected_prefix" expectation)
           "reject_before_write")
     | _ -> failwith "report must be object"))

let check_q1_repair_payloads_are_checker_valid () =
  List.iter
    (fun (case, expected_prefix) ->
       with_temp_dir (fun dir ->
         let failure_cases =
           List.filter
             (function
               | `Assoc fields ->
                 not (String.equal (string_value "case" fields) case)
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
         check ("Q1 repair runner exits nonzero: " ^ case) (code = 1);
         (match report with
          | `Assoc fields ->
            let gate = assoc_json "failure_case_gate" fields in
            check
              ("Q1 repair gate rejected: " ^ case)
              (String.equal (string_value "status" gate) "rejected");
            let repairs = first_repair_hint report |> repair_fields in
            let repair =
              repair_for_field
                ("expected_failure_atomicity_behavior[" ^ case ^ "]")
                repairs
            in
            check
              ("Q1 repair action: " ^ case)
              (String.equal
                 (string_value "action" repair)
                 "add_required_failure_case");
            check
              ("Q1 repair expected prefix: " ^ case)
              (String.equal
                 (string_value "expected_prefix" repair)
                 expected_prefix);
            let required_fields = assoc_json "required_case" repair in
            let required_case = `Assoc required_fields in
            check
              ("Q1 repair required case: " ^ case)
              (String.equal
                 (string_value "case" required_fields)
                 case);
            check
              ("Q1 repair required concrete: " ^ case)
              (not (has_ref_field required_case));
            let repaired_cases =
              failure_cases @ [required_case]
            in
            let repaired_code, repaired_report =
              run_check_index
                dir
                (q1_template ~failure_cases:repaired_cases ())
                []
            in
            check ("Q1 repair checks: " ^ case) (repaired_code = 0);
            (match repaired_report with
             | `Assoc repaired_fields ->
               check
                 ("Q1 repair checker accepted: " ^ case)
                 (String.equal
                    (string_value "status" repaired_fields)
                    "accepted");
               check
                 ("Q1 repair checker has no issues: " ^ case)
                 (int_value "issue_count" repaired_fields = 0)
             | _ -> failwith "checker report must be object")
          | _ -> failwith "report must be object")))
    Template.q1_required_failure_expectations

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
            (string_list "blockers" gate));
       let repairs = first_repair_hint report |> repair_fields in
       let mutation =
         repair_for_field
           "expected_failure_atomicity_behavior[negative_byte_offset].executable_mutations"
           repairs
       in
       check
         "wrong Q1 mutation repair action"
         (String.equal
            (string_value "action" mutation)
            "set_required_mutation_shape");
       check
         "wrong Q1 mutation repair case"
         (String.equal
            (string_value "case" mutation)
            "negative_byte_offset")
     | _ -> failwith "report must be object"))

let check_q1_failure_shape_accepts_input_lhs_alias () =
  with_temp_dir (fun dir ->
    let failure_cases =
      q1_failure_cases ()
      |> List.map
           (replace_failure_mutations
              "nonfinite_input_nan"
              [
                mutation
                  "replace_first_f64_input_cell"
                  "input"
                  ["value_bits", `Intlit "9221120237041090560"];
              ])
      |> List.map
           (replace_failure_mutations
              "nonfinite_input_infinity"
              [
                mutation
                  "replace_first_f64_input_cell"
                  "input"
                  ["value_bits", `Intlit "9218868437227405312"];
              ])
    in
    let code, report =
      run_conformance
        dir
        (q1_template ~failure_cases ~input_name:"input" ())
        [
          "--strict-effort";
          "--include-failures";
          "--require-failure-cases";
          "--require-profile-roots-bound";
        ]
    in
    check "Q1 lhs alias runner exits zero" (code = 0);
    let result = first_result report in
    let nan = failure_case_fields result "nonfinite_input_nan" in
    let infinity = failure_case_fields result "nonfinite_input_infinity" in
    check
      "Q1 lhs alias nan mutation accepted"
      (String.equal (string_value "mutation_shape_status" nan) "accepted");
    check
      "Q1 lhs alias infinity mutation accepted"
      (String.equal
         (string_value "mutation_shape_status" infinity)
         "accepted"))

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
    let repairs = first_repair_hint report |> repair_fields in
    let session_abi_repair =
      repair_for_field "abi.session_abi_root" repairs
    in
    check
      "stale ABI repair action"
      (String.equal
         (string_value "action" session_abi_repair)
         "set_session_abi_root");
    check
      "stale ABI repair expected root"
      (String.equal
         (string_value "expected" session_abi_repair)
         Abi.v1_root);
    check
      "stale ABI gate rejected"
      (String.equal (gate_status "abi_declaration_binding_gate" report) "rejected"))

let check_v2_abi_is_visible_in_executable_report () =
  with_temp_dir (fun dir ->
    let code, report =
      run_conformance
        dir
        (q1_template ~session_abi_root:Abi.v2_root ())
        ["--strict-effort"; "--require-profile-roots-bound"]
    in
    check "v2 ABI runner exits zero" (code = 0);
    let result = first_result report in
    check "v2 ABI execution accepted" (String.equal (string_value "status" result) "accepted");
    let abi = assoc_json "abi_declaration_binding" result in
    check "v2 ABI declaration matched" (String.equal (string_value "status" abi) "matched");
    check
      "v2 ABI matched root"
      (String.equal
         (string_value "litenode_matched_session_abi_root" abi)
         Abi.v2_root);
    check
      "v2 ABI gate accepted"
      (String.equal (gate_status "abi_declaration_binding_gate" report) "accepted"))

let check_stale_entry_label_repair_hint_is_precise () =
  with_temp_dir (fun dir ->
    let code, report =
      run_conformance
        dir
        (q1_template ~label:(Abi.advance_label + 1) ())
        ["--strict-effort"; "--require-profile-roots-bound"]
    in
    check "stale label runner still exits zero without readiness gate" (code = 0);
    let result = first_result report in
    let abi = assoc_json "abi_declaration_binding" result in
    check "stale label declaration unbound" (String.equal (string_value "status" abi) "unbound");
    check
      "stale label blocker"
      (List.mem "entry_label_mismatch" (string_list "blockers" abi));
    let repairs = first_repair_hint report |> repair_fields in
    let label_repair = repair_for_field "abi.label" repairs in
    check
      "stale label repair action"
      (String.equal
         (string_value "action" label_repair)
         "set_entrypoint_label");
    check
      "stale label repair observed"
      (String.equal
         (string_value "observed" label_repair)
         (string_of_int (Abi.advance_label + 1)));
    check
      "stale label repair expected"
      (String.equal
         (string_value "expected" label_repair)
         (string_of_int Abi.advance_label)))

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
        (List.mem "unbound_abi_declaration_binding" blockers);
      let repairs = first_repair_hint report |> repair_fields in
      let unit_repair =
        repair_for_field "abi.output_count_unit" repairs
      in
      check
        "stale ABI unit repair"
        (String.equal
           (string_value "expected" unit_repair)
           "cells")
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
         (String.equal (string_value "status" gate) "rejected");
       let repairs = first_repair_hint report |> repair_fields in
       let output_count_repair =
         repair_for_field "output.abi_registers.r1" repairs
       in
       check
         "bad executable ABI repair action"
         (String.equal
            (string_value "action" output_count_repair)
            "match_output_count");
       check
         "bad executable ABI repair observed"
         (String.equal (string_value "observed" output_count_repair) "2");
       check
         "bad executable ABI repair expected"
         (String.equal (string_value "expected" output_count_repair) "1")
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

let check_missing_cross_platform_matrix_request_is_actionable () =
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
          "--require-validator-readiness";
        ]
    in
    check "missing matrix readiness exits nonzero" (code = 1);
    match report with
    | `Assoc fields ->
      let readiness = assoc_json "validator_readiness_gate" fields in
      let cross_platform = assoc_json "cross_platform_evidence" readiness in
      check
        "missing matrix status"
        (String.equal (string_value "status" cross_platform) "missing");
      check
        "missing matrix blocker"
        (List.mem
           "missing_cross_platform_matrix"
           (string_list "blockers" cross_platform));
      let request = assoc_json "matrix_request" cross_platform in
      check
        "matrix request schema"
        (String.equal
           (string_value "schema" request)
           "octra.inference.conformance.matrix.request.v1");
      check
        "matrix request no authority"
        (String.equal (string_value "authority" request) "none");
      check
        "matrix request evidence scope"
        (String.equal
           (string_value "evidence_scope" request)
           "diagnostic_collection_request");
      check
        "matrix request platform identity"
        (String.equal
           (string_value "platform_identity" request)
           "unsigned_self_reported_observation");
      check
        "matrix request min platform observations"
        (int_value "minimum_distinct_platform_observation_count" request = 2);
      check
        "matrix request opcode scope"
        (string_list "required_opcodes" request = [opcode]);
      check
        "matrix request profile root"
        (String.equal
           (string_value "required_profile_catalog_root" request)
           (string_value "profile_catalog_root" fields));
      check
        "matrix request template corpus root"
        (String.equal
           (string_value "required_template_corpus_root" request)
           (string_value "template_corpus_root" fields));
      check
        "matrix request local signature"
        (String.equal
           (string_value "local_result_signature_sha256" request)
           (string_value "result_signature_sha256" fields));
      let platform = assoc_json "local_platform_observation" request in
      check
        "matrix request omits runner executable path"
        (not (List.mem_assoc "runner_executable" platform));
      let expected_per_report_requirements =
        `Assoc [
          "execution_mode", `String "positive_template_vm_execution";
          "top_level_status", `String "accepted";
          "execution_status", `String "accepted";
          "result_signature_schema",
          `String "octra.inference.conformance.result-signature.v6";
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
          "template_corpus_root", `String "valid_sha256";
          "runner_executable_sha256", `String "valid_sha256";
        ]
      in
      check
        "matrix request per-report requirements"
        (assoc_value "per_report_requirements" request =
         expected_per_report_requirements);
      let command_templates = assoc_json "command_templates" request in
      let expected_runner_argv =
        [
          "inference_conformance_run";
          "--template-index";
          "P0_TEMPLATE_INDEX";
          "--strict-effort";
          "--include-failures";
          "--require-failure-cases";
          "--require-profile-roots-bound";
          "--opcode";
          opcode;
        ]
      in
      let expected_matrix_argv =
        [
          "inference_conformance_matrix";
          "--runner-report";
          "LOCAL_RUNNER_REPORT";
          "--runner-report";
          "REMOTE_RUNNER_REPORT";
          "--min-platforms";
          "2";
          "--opcode";
          opcode;
        ]
      in
      let expected_final_argv =
        expected_runner_argv
        @ [
          "--require-validator-readiness";
          "--cross-platform-matrix";
          "CROSS_PLATFORM_MATRIX";
          "--expected-cross-platform-matrix-sha256";
          "MATRIX_SHA256";
        ]
      in
      check
        "matrix request local collection argv"
        (string_list "local_runner_report" command_templates = expected_runner_argv);
      check
        "matrix request remote collection argv"
        (string_list "remote_runner_report" command_templates = expected_runner_argv);
      check
        "matrix request matrix argv"
        (string_list "matrix" command_templates = expected_matrix_argv);
      check
        "matrix request final rerun argv"
        (string_list "final_readiness_rerun" command_templates = expected_final_argv)
    | _ -> failwith "report must be object")

let check_matrix_request_seed_reports_are_matrix_inputs () =
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
    check "matrix seed collection exits zero" (code = 0);
    let local_report = Filename.concat dir "local-runner-report.cjson" in
    let remote_report = Filename.concat dir "remote-runner-report.cjson" in
    write_json local_report seed_report;
    write_json
      remote_report
      (replace_platform_observation
         seed_report
         ~system_name:"Linux"
         ~machine:"x86_64"
         ~runner_sha:(hex_root '9'));
    let code, matrix =
      run_matrix
        [local_report; remote_report]
        ["--min-platforms"; "2"; "--opcode"; opcode]
    in
    check "seed reports are accepted by matrix" (code = 0);
    match matrix with
    | `Assoc fields ->
      check
        "seed matrix accepted"
        (String.equal (string_value "status" fields) "accepted");
      check
        "seed matrix has two platform observations"
        (int_value "distinct_platform_count" fields = 2);
      check
        "seed matrix has two runner observations"
        (int_value "distinct_runner_executable_count" fields = 2)
    | _ -> failwith "matrix must be object")

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

let check_p0_plus_rejected_results_empty_when_accepted () =
  with_temp_dir (fun dir ->
    let fixture =
      write_p0_plus_softmax_fixture dir "unit-softmax-match" 1.0
    in
    let code, report = run_p0_plus dir [fixture] in
    check "P0-plus accepted exits zero" (code = 0);
    match report with
    | `Assoc fields ->
      check
        "P0-plus accepted status"
        (String.equal (string_value "status" fields) "accepted");
      check
        "P0-plus accepted count"
        (int_value "accepted_count" fields = 1);
      check
        "P0-plus rejected count"
        (int_value "rejected_count" fields = 0);
      check
        "P0-plus rejected summary empty"
        (list_value "rejected_results" fields = []);
      check
        "P0-plus accepted replacement plans empty"
        (list_value "deterministic_replacement_plans" fields = []);
      check
        "P0-plus accepted repair hints empty"
        (list_value "producer_repair_hints" fields = [])
    | _ -> failwith "report must be object")

let check_p0_plus_rejected_results_report_outputs () =
  with_temp_dir (fun dir ->
    let accepted =
      write_p0_plus_softmax_fixture dir "unit-softmax-match" 1.0
    in
    let expected_mismatch =
      Int64.float_of_bits 0x3ff0000000000001L
    in
    let rejected =
      write_p0_plus_softmax_fixture dir "unit-softmax-mismatch" expected_mismatch
    in
    let code, report = run_p0_plus dir [accepted; rejected] in
    check "P0-plus rejected exits nonzero" (code = 1);
    match report with
    | `Assoc fields ->
      check
        "P0-plus rejected status"
        (String.equal (string_value "status" fields) "rejected");
      check
        "P0-plus mixed accepted count"
        (int_value "accepted_count" fields = 1);
      check
        "P0-plus mixed rejected count"
        (int_value "rejected_count" fields = 1);
      check
        "P0-plus mixed repair hints empty"
        (list_value "producer_repair_hints" fields = []);
      check
        "P0-plus generic mismatch replacement plans empty"
        (list_value "deterministic_replacement_plans" fields = []);
      (match list_value "rejected_results" fields with
       | [`Assoc rejected_fields] ->
         check
           "P0-plus rejected case"
           (String.equal
              (string_value "case" rejected_fields)
              "unit-softmax-mismatch");
         check
           "P0-plus rejected opcode"
           (String.equal
              (string_value "opcode" rejected_fields)
              "SOFTMAX_FP");
         check
           "P0-plus rejected output status"
           (String.equal
              (string_value "output_status" rejected_fields)
              "mismatch");
         check
           "P0-plus rejected classification"
           (String.equal
              (string_value "determinism_classification" rejected_fields)
              "deterministic_output_mismatch");
         check
           "P0-plus rejected next action"
           (String.equal
              (string_value "next_action" rejected_fields)
              "inspect_vm_semantics_or_fixture_authority");
         check
           "P0-plus rejected replacement plan absent"
           (assoc_value "deterministic_replacement_plan" rejected_fields = `Null);
         (match list_value "outputs" rejected_fields with
          | [`Assoc output_fields] ->
            check
              "P0-plus rejected output"
              (String.equal
                 (string_value "name" output_fields)
                 "expected_probabilities");
            check
              "P0-plus rejected output root"
              (not (bool_value "root_matched" output_fields));
            let detail = assoc_json "mismatch_detail" output_fields in
            check
              "P0-plus rejected output detail"
              (String.equal
                 (string_value "status" detail)
                 "mismatch");
            check
              "P0-plus rejected f64 cell"
              (int_value "first_mismatch_f64_cell" detail = 0)
          | _ -> failwith "expected one rejected output")
       | _ -> failwith "expected one rejected result")
    | _ -> failwith "report must be object")

let check_p0_plus_softmax_gap_reports_diagnostic_replacement_plan () =
  let outputs =
    [
      `Assoc [
        "name", `String "expected_probabilities";
        "status", `String "mismatch";
      ];
    ]
  in
  let plan =
    Octra_vm.Inference_conformance_diagnostics.p0_plus_replacement_plan
      ~opcode:"SOFTMAX_FP"
      ~case_name:"wide-1024-stable-tail"
      ~classification:"host_transcendental_portability_gap"
      ~outputs
  in
  match plan with
  | Some (`Assoc plan_fields) ->
    check
      "Softmax gap plan schema"
      (String.equal
         (string_value "schema" plan_fields)
         "octra.inference.deterministic-replacement-plan.v1");
    check
      "Softmax gap plan diagnostic"
      (bool_value "diagnostic_only" plan_fields);
    check
      "Softmax gap plan authority"
      (String.equal (string_value "authority" plan_fields) "none");
    check
      "Softmax gap plan opcode"
      (String.equal (string_value "opcode" plan_fields) "SOFTMAX_FP");
    check
      "Softmax gap plan case"
      (String.equal
         (string_value "case" plan_fields)
         "wide-1024-stable-tail");
    check
      "Softmax gap plan classification"
      (String.equal
         (string_value "current_classification" plan_fields)
         "host_transcendental_portability_gap");
    check
      "Softmax gap plan status"
      (String.equal
         (string_value "status" plan_fields)
         "required_before_consensus");
    check
      "Softmax gap plan replacement"
      (String.equal
         (string_value "preferred_replacement" plan_fields)
         "protocol-owned software exp over finite nonpositive binary64 inputs");
    check
      "Softmax gap plan gates"
      (List.mem
         (`String "cross_platform_matrix_matches")
         (list_value "acceptance_gates" plan_fields));
    check
      "Softmax gap plan carries observed outputs"
      (list_value "observed_outputs" plan_fields = outputs);
    check
      "Generic mismatch produces no plan"
      (Octra_vm.Inference_conformance_diagnostics.p0_plus_replacement_plan
         ~opcode:"SOFTMAX_FP"
         ~case_name:"wide-1024-stable-tail"
         ~classification:"deterministic_output_mismatch"
         ~outputs
       = None);
    check
      "Other opcode produces no plan"
      (Octra_vm.Inference_conformance_diagnostics.p0_plus_replacement_plan
         ~opcode:"ARGMAX_FP"
         ~case_name:"wide-1024-stable-tail"
         ~classification:"host_transcendental_portability_gap"
         ~outputs
       = None)
  | _ -> failwith "expected Softmax replacement plan"

let check_p0_plus_softmax_gap_requires_known_fixture_identity () =
  with_temp_dir (fun dir ->
    let count = 1024 in
    let expected = 1.0 /. float_of_int count in
    let expected_values =
      List.init
        count
        (fun index ->
           if index = 613 then
             Int64.float_of_bits
               (Int64.succ (Int64.bits_of_float expected))
           else
             expected)
    in
    let fixture =
      write_p0_plus_softmax_fixture
        ~scores:(List.init count (fun _ -> 0.0))
        ~expected_values
        dir
        "wide-1024-stable-tail"
        expected
    in
    let code, report = run_p0_plus dir [fixture] in
    check "Softmax lookalike gap exits nonzero" (code = 1);
    match report with
    | `Assoc fields ->
      check
        "Softmax lookalike replacement plans empty"
        (list_value "deterministic_replacement_plans" fields = []);
      (match list_value "rejected_results" fields with
       | [`Assoc rejected_fields] ->
         check
           "Softmax lookalike gap classification"
           (String.equal
              (string_value "determinism_classification" rejected_fields)
              "deterministic_output_mismatch");
         check
           "Softmax lookalike gap next action"
           (String.equal
              (string_value "next_action" rejected_fields)
              "inspect_vm_semantics_or_fixture_authority");
         check
           "Softmax lookalike replacement plan absent"
           (assoc_value "deterministic_replacement_plan" rejected_fields = `Null);
         (match list_value "outputs" rejected_fields with
          | [`Assoc output_fields] ->
            let detail = assoc_json "mismatch_detail" output_fields in
            check
              "Softmax lookalike gap cell"
              (int_value "first_mismatch_f64_cell" detail = 613);
            check
              "Softmax lookalike gap bit delta"
              (int_value "raw_u64_bit_delta" detail = 1)
          | _ -> failwith "expected one rejected output")
       | _ -> failwith "expected one rejected result")
    | _ -> failwith "report must be object")

let () =
  check_good_template_reports_bound_abi ();
  check_dynamic_q1_effort_vector ();
  check_q1_reject_failure_requires_full_output_span ();
  check_truncated_decoded_input_manifest_reports_ingress_rejected ();
  check_zero_decoded_input_truncation_is_not_ingress_rejected ();
  check_require_failure_cases_rejects_missing_q1_case ();
  check_require_failure_cases_rejects_wrong_q1_expectation ();
  check_q1_repair_payloads_are_checker_valid ();
  check_require_failure_cases_rejects_wrong_q1_mutation_shape ();
  check_q1_failure_shape_accepts_input_lhs_alias ();
  check_require_failure_cases_rejects_composite_q1_mutation_shape ();
  check_require_failure_cases_rejects_exact_partial_alias_shape ();
  check_accept_snapshot_requires_exact_output ();
  check_stale_abi_is_visible_in_executable_report ();
  check_v2_abi_is_visible_in_executable_report ();
  check_stale_entry_label_repair_hint_is_precise ();
  check_readiness_gate_rejects_stale_abi ();
  check_readiness_gate_reports_executable_abi_mismatch ();
  check_missing_output_subspan_reports_rejected ();
  check_missing_cross_platform_matrix_request_is_actionable ();
  check_matrix_request_seed_reports_are_matrix_inputs ();
  check_pinned_cross_platform_matrix_is_consumed ();
  check_cross_platform_matrix_without_pin_rejects ();
  check_cross_platform_matrix_sha_mismatch_rejects ();
  check_cross_platform_matrix_signature_schema_mismatch_rejects ();
  check_cross_platform_matrix_row_envelope_mismatch_rejects ();
  check_cross_platform_matrix_local_signature_mismatch_rejects ();
  check_cross_platform_matrix_forged_runner_count_rejects ();
  check_cross_platform_matrix_forged_observation_count_rejects ();
  check_cross_platform_matrix_rejects_insufficient_row_diversity ();
  check_cross_platform_matrix_forged_row_root_rejects ();
  check_p0_plus_rejected_results_empty_when_accepted ();
  check_p0_plus_rejected_results_report_outputs ();
  check_p0_plus_softmax_gap_reports_diagnostic_replacement_plan ();
  check_p0_plus_softmax_gap_requires_known_fixture_identity ()
