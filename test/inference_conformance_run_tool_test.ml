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

let input =
  f64_bytes (List.init 128 (fun _ -> 1.0))

let expected_output =
  f64_bytes [128.0]

let manifest name path raw =
  `Assoc [
    "name", `String name;
    "path", `String path;
    "bytes", `Int (String.length raw);
    "sha256", `String (sha256 raw);
  ]

let source path raw =
  `Assoc [
    "path", `String path;
    "bytes", `Int (String.length raw);
    "sha256", `String (sha256 raw);
  ]

let q1_template ?(session_abi_root = Abi.v1_root)
    ?(output_count_unit = "cells") ?(r1 = 1) () =
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
        "source", source "fixtures/lhs.f64le.bin" input;
        "range_binding", `Assoc ["encoding", `String "f64le"];
        "vm_memory",
        `Assoc [
          "base_address", `Int 2000;
          "length_f64_cells", `Int 128;
        ];
      ];
      `Assoc [
        "name", `String "q1_owner";
        "source", source "fixtures/q1-owner.bin" q1_owner;
        "range_binding", `Assoc ["encoding", `String "tensor.q1-g128"];
        "vm_memory",
        `Assoc [
          "base_address", `Int 0;
          "raw_register", `String "r2";
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
        "lhs", `String "r1";
        "q1_owner", `String "r2";
        "byte_offset", `String "r3";
        "m", `String "r4";
        "k", `String "r5";
        "n", `String "r6";
      ];
      "values",
      `Assoc [
        "dst", `Int 10000;
        "lhs", `Int 2000;
        "byte_offset", `Int 0;
        "m", `Int 1;
        "k", `Int 128;
        "n", `Int 1;
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
      "length_f64_cells", `Int 1;
      "abi_registers", `Assoc ["r0", `Int 10000; "r1", `Int r1];
      "subspans",
      `List [
        `Assoc [
          "name", `String "expected";
          "base_address", `Int 10000;
          "length_f64_cells", `Int 1;
          "sha256", `String (sha256 expected_output);
          "root", `String (sha256 expected_output);
        ];
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
  Unix.mkdir fixtures 0o700;
  write_file (Filename.concat fixtures "lhs.f64le.bin") input;
  write_file (Filename.concat fixtures "q1-owner.bin") q1_owner;
  write_file (Filename.concat fixtures "expected.f64le.bin") expected_output;
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

let check_good_template_reports_bound_abi () =
  with_temp_dir (fun dir ->
    let code, report =
      run_conformance
        dir
        (q1_template ())
        ["--strict-effort"; "--require-profile-roots-bound"]
    in
    check "good runner exits zero" (code = 0);
    let result = first_result report in
    check "good runner status" (String.equal (string_value "status" result) "accepted");
    check "good runner output" (String.equal (string_value "output_status" result) "matched");
    check "good program effort" (bool_value "program_effort_match" result);
    check "good opcode effort" (bool_value "opcode_effort_match" result);
    let abi = assoc_json "abi_declaration_binding" result in
    check "good ABI declaration matched" (String.equal (string_value "status" abi) "matched");
    check
      "good ABI gate accepted"
      (String.equal (gate_status "abi_declaration_binding_gate" report) "accepted"))

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

let () =
  check_good_template_reports_bound_abi ();
  check_stale_abi_is_visible_in_executable_report ();
  check_readiness_gate_rejects_stale_abi ()
