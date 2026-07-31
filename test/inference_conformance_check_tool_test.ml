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

let list_value name fields =
  match assoc_value name fields with
  | `List values -> values
  | _ -> failwith ("json field must be a list: " ^ name)

let write_json path json =
  let output = open_out_bin path in
  Fun.protect
    ~finally:(fun () -> close_out_noerr output)
    (fun () -> output_string output (Yojson.Safe.to_string json))

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
      "_build/default/tools/inference_conformance_check.exe";
      "../tools/inference_conformance_check.exe";
    ]
  in
  match List.find_opt Sys.file_exists candidates with
  | Some path -> path
  | None -> failwith "missing inference_conformance_check.exe"

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

let mutation name target fields =
  `Assoc (["mutation", `String name; "target", `String target] @ fields)

let span name base cells =
  `Assoc [
    "name", `String name;
    "base_address", `Int base;
    "length_f64_cells", `Int cells;
  ]

let failure_case case expected mutation unchanged =
  `Assoc [
    "case", `String case;
    "expected", `String expected;
    "executable_mutations", `List [mutation];
    "unchanged_spans", `List [unchanged];
  ]

let q1_failure_cases =
  [
    failure_case
      "nonfinite_input_nan"
      "reject_before_write"
      (mutation
         "replace_first_f64_input_cell"
         "lhs"
         ["value_bits", `Intlit "9221120237041090560"])
      (span "output" 10000 2);
    failure_case
      "nonfinite_input_infinity"
      "reject_before_write"
      (mutation
         "replace_first_f64_input_cell"
         "lhs"
         ["value_bits", `Intlit "9218868437227405312"])
      (span "output" 10000 2);
    failure_case
      "output_input_aliasing"
      "accept_from_snapshot_exact"
      (mutation
         "set_output_base_to_first_input_base"
         "output.base_address"
         [])
      (span "lhs" 2000 2);
    failure_case
      "partial_output_input_aliasing"
      "accept_from_snapshot_partial"
      (mutation
         "set_output_base_to_first_input_base_plus"
         "output.base_address"
         ["offset_cells", `Int 1])
      (span "lhs_prefix" 2001 2);
    failure_case
      "k_not_multiple_of_128"
      "reject_before_write"
      (mutation
         "set_scalar_param"
         "parameter_addresses_and_scalar_params.values.k"
         ["value", `Int 127])
      (span "output" 10000 2);
    failure_case
      "bad_q1_owner_length"
      "reject_before_write"
      (mutation
         "truncate_input_manifest"
         "q1_owner"
         ["truncate_bytes", `Int 1])
      (span "output" 10000 2);
    failure_case
      "nonfinite_fp16_scale"
      "reject_before_write"
      (mutation
         "replace_q1_scale_bits"
         "q1_owner[0..2]"
         ["value_hex_le", `String "007c"])
      (span "output" 10000 2);
    failure_case
      "lower_effort_limit"
      "reject_before_write"
      (mutation
         "lower_effort_limit"
         "effort"
         ["value", `Int 199])
      (span "output" 10000 2);
  ]

let lhs_input_range ?(encoding = "f64le") ?(source_bytes = 1024)
    ?(length_f64_cells = 128) () =
  `Assoc [
    "name", `String "lhs";
    "source",
    `Assoc [
      "path", `String "fixtures/input.bin";
      "bytes", `Int source_bytes;
      "sha256", `String (hex_root '1');
    ];
    "range_binding", `Assoc ["encoding", `String encoding];
    "vm_memory",
    `Assoc [
      "base_address", `Int 2000;
      "length_f64_cells", `Int length_f64_cells;
    ];
  ]

let q1_owner_input_range ?(encoding = "tensor.q1-g128") ?(source_bytes = 36)
    ?(include_f64_length = false) () =
  let vm_memory =
    if include_f64_length then
      `Assoc ["base_address", `Int 5000; "length_f64_cells", `Int 3]
    else
      `Assoc ["base_address", `Int 5000]
  in
  `Assoc [
    "name", `String "q1_owner";
    "source",
    `Assoc [
      "path", `String "fixtures/q1-owner.bin";
      "bytes", `Int source_bytes;
      "sha256", `String (hex_root '3');
    ];
    "range_binding", `Assoc ["encoding", `String encoding];
    "vm_memory", vm_memory;
  ]

let q1_input_ranges ?(q1_owner_encoding = "tensor.q1-g128") () =
  [
    lhs_input_range ();
    q1_owner_input_range ~encoding:q1_owner_encoding ();
  ]

let q1_parameter_block ?(include_registers = true) ?(lhs_register = "r2")
    ?(q1_owner_register = "r3") ?(dst_register = "r0") ?(k = 128)
    ?(n = 2) ?(byte_offset = 0) () =
  let registers =
    if include_registers then
      [
        "registers",
        `Assoc [
          "dst", `String dst_register;
          "lhs", `String lhs_register;
          "q1_owner", `String q1_owner_register;
          "byte_offset", `String "r4";
          "m", `String "r5";
          "k", `String "r6";
          "n", `String "r7";
        ];
      ]
    else
      []
  in
  `Assoc
    (registers
     @ [
       "values",
       `Assoc [
         "dst", `Int 10000;
         "lhs", `Int 2000;
         "byte_offset", `Int byte_offset;
         "m", `Int 1;
         "k", `Int k;
         "n", `Int n;
       ];
     ])

let q1_template ?(session_abi_root = Abi.v1_root) ?(expected_effort = 201)
    ?(output_count_unit = "cells") ?(r1 = 2) () =
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
    `List (q1_input_ranges ());
    "parameter_addresses_and_scalar_params", q1_parameter_block ();
    "expected_output_byte_manifests",
    `List [
      `Assoc [
        "name", `String "expected";
        "path", `String "fixtures/expected.bin";
        "bytes", `Int 16;
        "sha256", `String (hex_root '2');
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
      "length_f64_cells", `Int 2;
      "abi_registers", `Assoc ["r0", `Int 10000; "r1", `Int r1];
      "subspans", `List [];
    ];
    "expected_failure_atomicity_behavior",
    `List q1_failure_cases;
  ]

let replace_assoc_field name value = function
  | `Assoc fields ->
    `Assoc
      ((name, value)
       :: List.filter (fun (key, _) -> not (String.equal key name)) fields)
  | _ -> failwith "template must be object"

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

let replace_failure_unchanged_spans case spans = function
  | `Assoc fields ->
    `Assoc
      (List.map
         (fun (key, value) ->
            if String.equal key "unchanged_spans"
               && String.equal (string_value "case" fields) case then
              key, `List spans
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

let with_temp_dir f =
  let dir = Filename.temp_file "octra-check-tool-test" "" in
  Sys.remove dir;
  Unix.mkdir dir 0o700;
  Fun.protect
    ~finally:(fun () ->
      Sys.readdir dir
      |> Array.iter (fun name -> Sys.remove (Filename.concat dir name));
      Unix.rmdir dir)
    (fun () -> f dir)

let run_check dir template =
  let template_path = Filename.concat dir "q1.cjson" in
  let index_path = Filename.concat dir "index.cjson" in
  write_json template_path template;
  write_json index_path index_json;
  let command =
    String.concat
      " "
      [
        Filename.quote (tool_path ());
        "--template-index";
        Filename.quote index_path;
        "--opcode";
        opcode;
        "--require-profile-roots-bound";
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

let report_issues = function
  | `Assoc fields ->
    list_value "issues" fields
    |> List.map (function
      | `Assoc fields -> string_value "message" fields
      | _ -> failwith "issue must be object")
  | _ -> failwith "report must be object"

let report_gate name = function
  | `Assoc fields ->
    (match assoc_value name fields with
     | `Assoc gate -> gate
     | _ -> failwith ("gate must be object: " ^ name))
  | _ -> failwith "report must be object"

let check_q1_contract_visible report =
  match report with
  | `Assoc fields ->
    (match list_value "required_failure_case_contracts" fields with
     | [`Assoc contract] ->
       check
         "q1 contract opcode"
         (String.equal
            (string_value "opcode" contract)
            "LINEAR_Q1_G128_FP");
       check
         "q1 contract expectation count"
         (List.length (list_value "expectations" contract) = 8)
     | _ -> failwith "expected one required failure-case contract")
  | _ -> failwith "report must be object"

let check_accepts_bound_abi_declaration () =
  with_temp_dir (fun dir ->
    let code, report = run_check dir (q1_template ()) in
    check "accepted ABI declaration exits zero" (code = 0);
    check_q1_contract_visible report;
    match report_gate "abi_declaration_binding_gate" report with
    | fields ->
      check
        "accepted ABI declaration gate"
        (String.equal (string_value "status" fields) "accepted"))

let check_rejects_stale_session_abi_root () =
  with_temp_dir (fun dir ->
    let code, report =
      run_check dir (q1_template ~session_abi_root:(hex_root '3') ())
    in
    check "stale ABI root exits nonzero" (code = 1);
    check
      "stale ABI root issue"
      (List.mem
         "ABI declaration binding rejected: session_abi_root_mismatch"
         (report_issues report));
    let abi_gate = report_gate "abi_declaration_binding_gate" report in
    check
      "stale ABI root gate"
      (String.equal (string_value "status" abi_gate) "rejected");
    let readiness = report_gate "validator_readiness_gate" report in
    check
      "stale ABI root next blocker"
      (String.equal (string_value "next_blocker" readiness) "schema_rejected"))

let check_rejects_narrow_output_unit () =
  with_temp_dir (fun dir ->
    let code, report =
      run_check dir (q1_template ~output_count_unit:"f64_cells" ())
    in
    check "narrow output unit exits nonzero" (code = 1);
    check
      "narrow output unit issue"
      (List.mem
         "ABI declaration binding rejected: output_count_unit_mismatch"
         (report_issues report)))

let check_rejects_r1_output_count_drift () =
  with_temp_dir (fun dir ->
    let code, report = run_check dir (q1_template ~r1:3 ()) in
    check "r1 output count drift exits nonzero" (code = 1);
    check
      "r1 output count drift issue"
      (List.mem
         "ABI declaration binding rejected: r1_output_count_mismatch"
         (report_issues report)))

let check_rejects_q1_expected_effort_drift () =
  with_temp_dir (fun dir ->
    let code, report = run_check dir (q1_template ~expected_effort:204 ()) in
    check "q1 expected effort drift exits nonzero" (code = 1);
    check
      "q1 expected effort drift issue"
      (List.mem
         "expected_effort mismatch for LINEAR_Q1_G128_FP: expected 201 actual 204"
         (report_issues report)))

let check_rejects_missing_q1_owner_input_range () =
  with_temp_dir (fun dir ->
    let code, report =
      run_check
        dir
        (q1_template ()
         |> replace_assoc_field
              "input_memory_ranges"
              (`List [lhs_input_range ()]))
    in
    check "missing q1 owner range exits nonzero" (code = 1);
    check
      "missing q1 owner range issue"
      (List.mem
         "LINEAR_Q1_G128_FP requires q1_owner input range"
         (report_issues report)))

let check_rejects_q1_owner_f64le_range () =
  with_temp_dir (fun dir ->
    let code, report =
      run_check
        dir
        (q1_template ()
         |> replace_assoc_field
              "input_memory_ranges"
              (`List (q1_input_ranges ~q1_owner_encoding:"f64le" ())))
    in
    check "q1 owner f64le range exits nonzero" (code = 1);
    check
      "q1 owner f64le range issue"
      (List.mem
         "LINEAR_Q1_G128_FP q1_owner input must be tensor.q1-g128"
         (report_issues report)))

let check_rejects_q1_owner_f64_cell_length () =
  with_temp_dir (fun dir ->
    let code, report =
      run_check
        dir
        (q1_template ()
         |> replace_assoc_field
              "input_memory_ranges"
              (`List [
                lhs_input_range ();
                q1_owner_input_range ~include_f64_length:true ();
              ]))
    in
    check "q1 owner f64 length exits nonzero" (code = 1);
    check
      "q1 owner f64 length issue"
      (List.mem
         "LINEAR_Q1_G128_FP q1_owner input must be raw bytes"
         (report_issues report)))

let check_rejects_q1_lhs_layout_mismatch () =
  with_temp_dir (fun dir ->
    let code, report =
      run_check
        dir
        (q1_template ()
         |> replace_assoc_field
              "input_memory_ranges"
              (`List [
                lhs_input_range ~source_bytes:4096 ~length_f64_cells:512 ();
                q1_owner_input_range ();
              ]))
    in
    check "q1 lhs layout mismatch exits nonzero" (code = 1);
    let issues = report_issues report in
    check
      "q1 lhs bytes mismatch issue"
      (List.mem
         "LINEAR_Q1_G128_FP lhs source bytes mismatch: expected 1024 actual 4096"
         issues);
    check
      "q1 lhs cells mismatch issue"
      (List.mem
         "LINEAR_Q1_G128_FP lhs length_f64_cells mismatch: expected 128 actual 512"
         issues))

let check_rejects_q1_owner_byte_span_too_short () =
  with_temp_dir (fun dir ->
    let code, report =
      run_check
        dir
        (q1_template ()
         |> replace_assoc_field
              "input_memory_ranges"
              (`List [
                lhs_input_range ();
                q1_owner_input_range ~source_bytes:18 ();
              ])
         |> replace_assoc_field
              "parameter_addresses_and_scalar_params"
              (q1_parameter_block ~n:2 ()))
    in
    check "short q1 owner span exits nonzero" (code = 1);
    check
      "short q1 owner span issue"
      (List.mem
         "LINEAR_Q1_G128_FP q1_owner source bytes too short: required 36 actual 18"
         (report_issues report)))

let check_rejects_q1_expected_output_manifest_size () =
  with_temp_dir (fun dir ->
    let code, report =
      run_check
        dir
        (q1_template ()
         |> replace_assoc_field
              "expected_output_byte_manifests"
              (`List [
                `Assoc [
                  "name", `String "expected";
                  "path", `String "fixtures/expected.bin";
                  "bytes", `Int 8;
                  "sha256", `String (hex_root '2');
                ];
              ]))
    in
    check "q1 expected manifest size exits nonzero" (code = 1);
    check
      "q1 expected manifest size issue"
      (List.mem
         "LINEAR_Q1_G128_FP expected output manifest bytes mismatch: expected 16 actual 8"
         (report_issues report)))

let check_rejects_q1_missing_register_metadata () =
  with_temp_dir (fun dir ->
    let code, report =
      run_check
        dir
        (q1_template ()
         |> replace_assoc_field
              "parameter_addresses_and_scalar_params"
              (q1_parameter_block ~include_registers:false ()))
    in
    check "missing q1 register metadata exits nonzero" (code = 1);
    check
      "missing q1 register metadata issue"
      (List.mem
         "missing LINEAR_Q1_G128_FP register metadata"
         (report_issues report)))

let check_rejects_q1_lhs_output_count_register_collision () =
  with_temp_dir (fun dir ->
    let code, report =
      run_check
        dir
        (q1_template ()
         |> replace_assoc_field
              "parameter_addresses_and_scalar_params"
              (q1_parameter_block ~lhs_register:"r1" ()))
    in
    check "q1 lhs output count collision exits nonzero" (code = 1);
    check
      "q1 lhs output count collision issue"
      (List.mem
         "LINEAR_Q1_G128_FP input/scalar registers must not use ABI output_count_register: lhs"
         (report_issues report)))

let check_rejects_q1_dst_not_output_base_register () =
  with_temp_dir (fun dir ->
    let code, report =
      run_check
        dir
        (q1_template ()
         |> replace_assoc_field
              "parameter_addresses_and_scalar_params"
              (q1_parameter_block ~dst_register:"r8" ()))
    in
    check "q1 dst output base mismatch exits nonzero" (code = 1);
    check
      "q1 dst output base mismatch issue"
      (List.mem
         "LINEAR_Q1_G128_FP dst register must match ABI output_base_register"
         (report_issues report)))

let check_rejects_q1_bad_k_shape () =
  with_temp_dir (fun dir ->
    let code, report =
      run_check
        dir
        (q1_template ()
         |> replace_assoc_field
              "parameter_addresses_and_scalar_params"
              (q1_parameter_block ~k:127 ()))
    in
    check "bad q1 k shape exits nonzero" (code = 1);
    check
      "bad q1 k shape issue"
      (List.mem
         "LINEAR_Q1_G128_FP k must be a multiple of 128"
         (report_issues report)))

let check_rejects_missing_q1_failure_case () =
  with_temp_dir (fun dir ->
    let failures =
      List.filter
        (function
          | `Assoc fields ->
            not
              (String.equal
                 (string_value "case" fields)
                 "nonfinite_fp16_scale")
          | _ -> true)
        q1_failure_cases
    in
    let code, report =
      run_check
        dir
        (q1_template ()
         |> replace_assoc_field
              "expected_failure_atomicity_behavior"
              (`List failures))
    in
    check "missing Q1 failure case exits nonzero" (code = 1);
    check
      "missing Q1 failure case issue"
      (List.mem
         "nonfinite_fp16_scale failure case is required for Q1 validator readiness"
         (report_issues report)))

let check_rejects_wrong_q1_failure_expectation () =
  with_temp_dir (fun dir ->
    let failures =
      List.map
        (replace_failure_expected
           "nonfinite_fp16_scale"
           "accept_from_snapshot_wrong")
        q1_failure_cases
    in
    let code, report =
      run_check
        dir
        (q1_template ()
         |> replace_assoc_field
              "expected_failure_atomicity_behavior"
              (`List failures))
    in
    check "wrong Q1 failure expectation exits nonzero" (code = 1);
    check
      "wrong Q1 failure expectation issue"
      (List.mem
         "nonfinite_fp16_scale must declare reject_before_write"
         (report_issues report)))

let check_rejects_q1_exact_alias_without_alias_mutation () =
  with_temp_dir (fun dir ->
    let failures =
      List.map
        (replace_failure_mutations
           "output_input_aliasing"
           [
             mutation
               "replace_first_f64_input_cell"
               "lhs"
               ["value_bits", `Intlit "9221120237041090560"];
           ])
        q1_failure_cases
    in
    let code, report =
      run_check
        dir
        (q1_template ()
         |> replace_assoc_field
              "expected_failure_atomicity_behavior"
              (`List failures))
    in
    check "wrong exact alias mutation exits nonzero" (code = 1);
    check
      "wrong exact alias mutation issue"
      (List.mem
         "output_input_aliasing must declare output/lhs alias mutation"
         (report_issues report)))

let check_rejects_q1_partial_alias_without_partial_mutation () =
  with_temp_dir (fun dir ->
    let failures =
      List.map
        (replace_failure_mutations
           "partial_output_input_aliasing"
           [
             mutation
               "set_output_base_to_first_input_base"
               "output.base_address"
               [];
           ])
        q1_failure_cases
    in
    let code, report =
      run_check
        dir
        (q1_template ()
         |> replace_assoc_field
              "expected_failure_atomicity_behavior"
              (`List failures))
    in
    check "wrong partial alias mutation exits nonzero" (code = 1);
    check
      "wrong partial alias mutation issue"
      (List.mem
         "partial_output_input_aliasing must declare partial output/lhs alias mutation"
         (report_issues report)))

let check_rejects_string_executable_mutation () =
  with_temp_dir (fun dir ->
    let failures =
      List.map
        (replace_failure_mutations "nonfinite_input_nan" [`String "lhs[0]=nan"])
        q1_failure_cases
    in
    let code, report =
      run_check
        dir
        (q1_template ()
         |> replace_assoc_field
              "expected_failure_atomicity_behavior"
              (`List failures))
    in
    check "string mutation exits nonzero" (code = 1);
    check
      "string mutation issue"
      (List.mem
         "failure case executable_mutations entries must be objects: nonfinite_input_nan"
         (report_issues report)))

let check_rejects_string_unchanged_span () =
  with_temp_dir (fun dir ->
    let failures =
      List.map
        (replace_failure_unchanged_spans "nonfinite_input_nan" [`String "output"])
        q1_failure_cases
    in
    let code, report =
      run_check
        dir
        (q1_template ()
         |> replace_assoc_field
              "expected_failure_atomicity_behavior"
              (`List failures))
    in
    check "string unchanged span exits nonzero" (code = 1);
    check
      "string unchanged span issue"
      (List.mem
         "failure case unchanged_spans entries must be objects: nonfinite_input_nan"
         (report_issues report)))

let check_rejects_incomplete_unchanged_span () =
  with_temp_dir (fun dir ->
    let failures =
      List.map
        (replace_failure_unchanged_spans
           "nonfinite_input_nan"
           [`Assoc ["name", `String "output"; "base_address", `Int 10000]])
        q1_failure_cases
    in
    let code, report =
      run_check
        dir
        (q1_template ()
         |> replace_assoc_field
              "expected_failure_atomicity_behavior"
              (`List failures))
    in
    check "incomplete unchanged span exits nonzero" (code = 1);
    check
      "incomplete unchanged span issue"
      (List.mem
         "failure case unchanged span missing length_f64_cells: nonfinite_input_nan"
         (report_issues report)))

let check_rejects_q1_reject_case_without_full_output_span () =
  with_temp_dir (fun dir ->
    let failures =
      List.map
        (replace_failure_unchanged_spans
           "nonfinite_input_nan"
           [span "output_prefix" 10000 1])
        q1_failure_cases
    in
    let code, report =
      run_check
        dir
        (q1_template ()
         |> replace_assoc_field
              "expected_failure_atomicity_behavior"
              (`List failures))
    in
    check "partial reject span exits nonzero" (code = 1);
    check
      "partial reject span issue"
      (List.mem
         "nonfinite_input_nan must preserve full LINEAR_Q1_G128_FP output span"
         (report_issues report)))

let () =
  check_accepts_bound_abi_declaration ();
  check_rejects_stale_session_abi_root ();
  check_rejects_narrow_output_unit ();
  check_rejects_r1_output_count_drift ();
  check_rejects_q1_expected_effort_drift ();
  check_rejects_missing_q1_owner_input_range ();
  check_rejects_q1_owner_f64le_range ();
  check_rejects_q1_owner_f64_cell_length ();
  check_rejects_q1_lhs_layout_mismatch ();
  check_rejects_q1_owner_byte_span_too_short ();
  check_rejects_q1_expected_output_manifest_size ();
  check_rejects_q1_missing_register_metadata ();
  check_rejects_q1_lhs_output_count_register_collision ();
  check_rejects_q1_dst_not_output_base_register ();
  check_rejects_q1_bad_k_shape ();
  check_rejects_missing_q1_failure_case ();
  check_rejects_wrong_q1_failure_expectation ();
  check_rejects_q1_exact_alias_without_alias_mutation ();
  check_rejects_q1_partial_alias_without_partial_mutation ();
  check_rejects_string_executable_mutation ();
  check_rejects_string_unchanged_span ();
  check_rejects_incomplete_unchanged_span ();
  check_rejects_q1_reject_case_without_full_output_span ()
