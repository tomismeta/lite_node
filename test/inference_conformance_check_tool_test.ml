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

let q1_template ?(session_abi_root = Abi.v1_root)
    ?(output_count_unit = "cells") ?(r1 = 2) () =
  `Assoc [
    "type", `String "p0_litenode_vm_execution_template";
    "schema", `String "octra.inference.p0.vm-template.v1";
    "opcode", `String opcode;
    "primitive", `String primitive;
    "profile", `String (current_profile ());
    "vm_semantics_root", `String (current_vm_semantics_root ());
    "numerical_profile_root", `String (current_profile_root ());
    "expected_effort", `Int 201;
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
        "source",
        `Assoc [
          "path", `String "fixtures/input.bin";
          "bytes", `Int 4096;
          "sha256", `String (hex_root '1');
        ];
        "range_binding", `Assoc ["encoding", `String "f64le"];
        "vm_memory",
        `Assoc [
          "base_address", `Int 2000;
          "length_f64_cells", `Int 512;
        ];
      ];
    ];
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
    `List [
      `Assoc [
        "case", `String "output_input_aliasing";
        "expected", `String "accept_from_snapshot_exact";
        "executable_mutations", `List [`String "dst=lhs"];
        "unchanged_spans", `List [`String "lhs"];
      ];
      `Assoc [
        "case", `String "partial_output_input_aliasing";
        "expected", `String "accept_from_snapshot_partial";
        "executable_mutations", `List [`String "dst=lhs+1"];
        "unchanged_spans", `List [`String "lhs_prefix"];
      ];
    ];
  ]

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

let check_accepts_bound_abi_declaration () =
  with_temp_dir (fun dir ->
    let code, report = run_check dir (q1_template ()) in
    check "accepted ABI declaration exits zero" (code = 0);
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
    match report_gate "abi_declaration_binding_gate" report with
    | fields ->
      check
        "stale ABI root gate"
        (String.equal (string_value "status" fields) "rejected"))

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

let () =
  check_accepts_bound_abi_declaration ();
  check_rejects_stale_session_abi_root ();
  check_rejects_narrow_output_unit ();
  check_rejects_r1_output_count_drift ()
