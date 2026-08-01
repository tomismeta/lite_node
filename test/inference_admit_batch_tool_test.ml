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
module Model = Octra_vm.Inference_model
module Req = Octra_vm.Execution_requirement
module Request = Octra_vm.Inference_request
module Target = Octra_vm.Inference_target
module VM = Octra_vm.Contract_vm

let check label condition =
  if not condition then failwith label

let hex_root char =
  String.make 64 char

let sha256 raw =
  Digestif.SHA256.(digest_string raw |> to_hex)

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

let rec remove_tree path =
  if Sys.file_exists path then
    if Sys.is_directory path then begin
      Sys.readdir path
      |> Array.iter (fun name -> remove_tree (Filename.concat path name));
      Unix.rmdir path
    end
    else Sys.remove path

let with_temp_dir name f =
  let dir =
    Filename.concat
      (Filename.get_temp_dir_name ())
      (Printf.sprintf "%s-%d" name (Unix.getpid ()))
  in
  remove_tree dir;
  Unix.mkdir dir 0o700;
  Fun.protect ~finally:(fun () -> remove_tree dir) (fun () -> f dir)

let tool_path () =
  let candidates =
    [
      "_build/default/tools/inference_admit.exe";
      "../tools/inference_admit.exe";
    ]
  in
  match List.find_opt Sys.file_exists candidates with
  | Some path -> path
  | None -> failwith "missing inference_admit.exe"

let assoc_value name fields =
  match List.assoc_opt name fields with
  | Some value -> value
  | None -> failwith ("missing json field: " ^ name)

let assoc_json name fields =
  match assoc_value name fields with
  | `Assoc value -> value
  | _ -> failwith ("json field must be an object: " ^ name)

let list_json name fields =
  match assoc_value name fields with
  | `List value -> value
  | _ -> failwith ("json field must be a list: " ^ name)

let string_json name fields =
  match assoc_value name fields with
  | `String value -> value
  | _ -> failwith ("json field must be a string: " ^ name)

let bool_json name fields =
  match assoc_value name fields with
  | `Bool value -> value
  | _ -> failwith ("json field must be a bool: " ^ name)

let int_json name fields =
  match assoc_value name fields with
  | `Int value -> value
  | `Intlit value -> int_of_string value
  | _ -> failwith ("json field must be an int: " ^ name)

let capability_json Req.{ name; root } =
  `Assoc ["name", `String name; "root", `String root]

let limits_json Req.{
    max_model_bytes;
    max_view_bytes;
    max_session_bytes;
    max_scratch_bytes;
    max_output_bytes;
    max_advance_effort;
  } =
  `Assoc [
    "max_model_bytes", `Int max_model_bytes;
    "max_view_bytes", `Int max_view_bytes;
    "max_session_bytes", `Int max_session_bytes;
    "max_scratch_bytes", `Int max_scratch_bytes;
    "max_output_bytes", `Int max_output_bytes;
    "max_advance_effort", `Int max_advance_effort;
  ]

let requirement_json requirement =
  `Assoc [
    "vm_semantics_root", `String requirement.Req.vm_semantics_root;
    "numerical_root", `String requirement.numerical_root;
    "effort_root", `String requirement.effort_root;
    "capabilities",
    `List (List.map capability_json requirement.capabilities);
    "limits", limits_json requirement.limits;
    "requirement_root", `String (Req.root requirement);
  ]

let support_json support =
  `Assoc [
    "vm_semantics_root", `String support.Req.support_vm_semantics_root;
    "numerical_roots",
    `List (List.map (fun root -> `String root) support.support_numerical_roots);
    "effort_roots",
    `List (List.map (fun root -> `String root) support.support_effort_roots);
    "capabilities",
    `List (List.map capability_json support.support_capabilities);
    "limits", limits_json support.support_limits;
  ]

let target_json target =
  let entrypoint Target.{ entry_name; entry_label } =
    `Assoc ["name", `String entry_name; "label", `Int entry_label]
  in
  `Assoc [
    "program_root", `String target.Target.program_root;
    "requirement_root", `String target.requirement_root;
    "model_root", `String target.model_root;
    "execution_descriptor_root", `String target.execution_descriptor_root;
    "store_root", `String target.store_root;
    "session_abi_root", `String target.session_abi_root;
    "entrypoints", `List (List.map entrypoint target.entrypoints);
    "target_root", `String (Target.root target);
  ]

let range_json Model.{ owner_root; offset; length; encoding; shape_root } =
  `Assoc [
    "owner_root", `String owner_root;
    "offset", `Int offset;
    "length", `Int length;
    "encoding", `String encoding;
    "shape_root",
    (match shape_root with
     | None -> `Null
     | Some root -> `String root);
  ]

let model_json model =
  `Assoc [
    "model_root", `String model.Model.model_root;
    "store_root", `String model.store_root;
    "ranges", `List (List.map range_json model.ranges);
    "model_ranges_root", `String (Model.root model);
  ]

let request_json request =
  `Assoc [
    "schema", `Int request.Request.schema;
    "target_root", `String request.target_root;
    "entrypoint", `String request.entrypoint;
    "input_root", `String request.input_root;
    "request_nonce", `String request.request_nonce;
    "max_output_bytes", `Int request.max_output_bytes;
    "max_advance_effort", `Int request.max_advance_effort;
    "request_root", `String (Request.root request);
  ]

let drop_assoc_fields names = function
  | `Assoc fields ->
    `Assoc
      (List.filter
         (fun (field, _) -> not (List.exists (String.equal field) names))
         fields)
  | value -> value

let write_batch_fixture dir =
  let stage_dir = Filename.concat dir "stage" in
  Unix.mkdir stage_dir 0o700;
  let code =
    [|
      VM.JDEST Abi.advance_label;
      VM.LDI (2, VM.VInt (Z.of_int 7));
      VM.MSTORE (10, 2);
      VM.LDI (0, VM.VInt (Z.of_int 10));
      VM.LDI (1, VM.VInt Z.one);
      VM.STOP;
    |]
  in
  let capability = Req.{ name = "storage.authenticated-range"; root = hex_root 'd' } in
  let limits =
    Req.{
      max_model_bytes = 64;
      max_view_bytes = 64;
      max_session_bytes = 1024;
      max_scratch_bytes = 128;
      max_output_bytes = 64;
      max_advance_effort = 4096;
    }
  in
  let requirement =
    Req.{
      vm_semantics_root = hex_root 'a';
      numerical_root = hex_root 'b';
      effort_root = hex_root 'c';
      capabilities = [capability];
      limits;
    }
  in
  let support =
    Req.{
      support_vm_semantics_root = requirement.vm_semantics_root;
      support_numerical_roots = [requirement.numerical_root];
      support_effort_roots = [requirement.effort_root];
      support_capabilities = requirement.capabilities;
      support_limits = limits;
    }
  in
  let program = Inference_cert.source code in
  let admitted = Inference_cert.admit ~support ~requirement code in
  let target =
    Target.{
      program_root = Target.program_root admitted;
      requirement_root = Req.root requirement;
      model_root = hex_root 'e';
      execution_descriptor_root = hex_root '1';
      store_root = hex_root '2';
      session_abi_root = Abi.v1_root;
      entrypoints = [{
        entry_name = Abi.advance_entrypoint;
        entry_label = Abi.advance_label;
      }];
    }
  in
  let owner = "batch fixture owner bytes" in
  let owner_root = sha256 owner in
  let model =
    Model.{
      model_root = target.model_root;
      store_root = target.store_root;
      ranges = [{
        owner_root;
        offset = 0;
        length = String.length owner;
        encoding = "octets";
        shape_root = None;
      }];
    }
  in
  let request_input = "" in
  let request =
    Request.{
      schema = Abi.request_schema;
      target_root = Target.root target;
      entrypoint = Abi.advance_entrypoint;
      input_root = sha256 request_input;
      request_nonce = hex_root '5';
      max_output_bytes = 64;
      max_advance_effort = 4096;
    }
  in
  write_file (Filename.concat stage_dir "program.ocpg") program;
  write_json (Filename.concat stage_dir "requirement.json") (requirement_json requirement);
  write_json (Filename.concat stage_dir "support.json") (support_json support);
  write_json (Filename.concat stage_dir "target.json") (target_json target);
  write_json (Filename.concat stage_dir "model-ranges.json") (model_json model);
  write_json (Filename.concat stage_dir "request.json") (request_json request);
  write_file (Filename.concat stage_dir "request-input.json") request_input;
  write_file (Filename.concat stage_dir "owner.bin") owner;
  let batch =
    `Assoc [
      "schema", `String "octra.inference.run_batch";
      "stages",
      `List [
        `Assoc [
          "stage_id", `String "unit-stage";
          "program", `String "stage/program.ocpg";
          "requirement", `String "stage/requirement.json";
          "target", `String "stage/target.json";
          "support", `String "stage/support.json";
          "request", `String "stage/request.json";
          "input", `String "stage/request-input.json";
          "model_ranges", `String "stage/model-ranges.json";
          "expected_output_root", `Null;
          "range_sources",
          `List [
            `Assoc [
              "owner_root", `String owner_root;
              "path", `String "stage/owner.bin";
            ];
          ];
        ];
      ];
    ]
  in
  let batch_path = Filename.concat dir "batch.cjson" in
  write_json batch_path batch;
  batch_path

let run_batch batch_path =
  let command =
    String.concat
      " "
      [
        Filename.quote (tool_path ());
        "--run-batch";
        Filename.quote batch_path;
        "--timing-mode";
        "opcode";
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

let check_runtime_semantics report =
  match report with
  | `Assoc fields ->
    let semantics = assoc_json "runtime_semantics" fields in
    check
      "session mode"
      (String.equal
         (string_json "session_mode" semantics)
         "independent_session_per_stage");
    check
      "continuation unsupported"
      (not (bool_json "continuation_supported" semantics));
    check
      "stage lifecycle"
      (list_json "stage_lifecycle" semantics
       = [
         `String "open_session";
         `String "advance_session";
         `String "finalize_session";
       ]);
    check
      "batch cache scope"
      (list_json "batch_cache_scope" semantics
       = [`String "owner_bytes"; `String "model_range_pins"]);
    check
      "no resident cache claim"
      (List.assoc_opt "resident_cache_scope" semantics = None);
    check
      "no state carry field"
      (List.assoc_opt "state_carry" semantics = None);
    check
      "no next blocker field"
      (List.assoc_opt "next_runtime_blocker" semantics = None)
  | _ -> failwith "report must be an object"

let check_batch_hash report =
  match report with
  | `Assoc fields ->
    let expected_hash = string_json "batch_report_sha256" fields in
    let stages = list_json "stages" fields in
    let sanitized =
      `List
        (List.map
           (drop_assoc_fields ["timing"; "execution_timing"; "opcode_timing"])
           stages)
    in
    let payload =
      "octra.inference.run_batch.report.v1:"
      ^ Yojson.Safe.to_string sanitized
    in
    check
      "batch report hash"
      (String.equal expected_hash (sha256 payload));
    check
      "runtime semantics outside deterministic hash"
      (List.assoc_opt "runtime_semantics" fields <> None)
  | _ -> failwith "report must be an object"

let check_legacy_report_fields report =
  match report with
  | `Assoc fields ->
    check "status accepted" (String.equal (string_json "status" fields) "accepted");
    check "stage count" (int_json "stage_count" fields = 1);
    check "old batch hash field" (String.length (string_json "batch_report_sha256" fields) = 64);
    check "old last root field" (String.length (string_json "last_stage_output_root" fields) = 64);
    (match list_json "stages" fields with
     | [`Assoc stage] ->
       check
         "stage status"
         (String.equal (string_json "status" stage) "accepted");
       check
         "stage session"
         (String.equal (string_json "session_status" stage) "accepted");
       check
         "stage reference"
         (String.equal (string_json "reference_status" stage) "unchecked");
       ignore (assoc_value "execution_timing" stage);
       ignore (assoc_value "opcode_timing" stage)
     | _ -> failwith "expected one stage")
  | _ -> failwith "report must be an object"

let check_batch_runtime_semantics () =
  with_temp_dir "octra-inference-admit-batch-test" @@ fun dir ->
  let batch_path = write_batch_fixture dir in
  let code, report = run_batch batch_path in
  check "run-batch exit code" (code = 0);
  check_runtime_semantics report;
  check_batch_hash report;
  check_legacy_report_fields report

let () =
  check_batch_runtime_semantics ()
