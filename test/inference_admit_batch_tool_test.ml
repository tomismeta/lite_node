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

let check_payload_sha fields ~payload_field ~sha_field =
  let payload = string_json payload_field fields in
  let digest = string_json sha_field fields in
  check
    (sha_field ^ " binds " ^ payload_field)
    (String.equal digest (sha256 payload));
  payload

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

let replace_assoc_field name replacement = function
  | `Assoc fields ->
    `Assoc
      (List.map
         (fun (field, value) ->
            if String.equal field name then field, replacement
            else field, value)
         fields)
  | _ -> failwith "expected object"

let request_from_json = function
  | `Assoc fields ->
    Request.{
      schema = int_json "schema" fields;
      target_root = string_json "target_root" fields;
      entrypoint = string_json "entrypoint" fields;
      input_root = string_json "input_root" fields;
      request_nonce = string_json "request_nonce" fields;
      max_output_bytes = int_json "max_output_bytes" fields;
      max_advance_effort = int_json "max_advance_effort" fields;
    }
  | _ -> failwith "request must be an object"

let continuation_code =
  [|
    VM.JDEST Abi.advance_label;
    VM.MLOAD (2, Abi.sequence_cell);
    VM.MSTORE (10, 2);
    VM.MLOAD (2, Abi.logical_position_cell);
    VM.MSTORE (11, 2);
    VM.MLOAD (2, Abi.output_root_cell);
    VM.MSTORE (12, 2);
    VM.MLOAD (2, Abi.output_prefix_root_cell);
    VM.MSTORE (13, 2);
    VM.MLOAD (2, Abi.committed_target_state_root_cell);
    VM.MSTORE (14, 2);
    VM.LDI (0, VM.VInt (Z.of_int 10));
    VM.LDI (1, VM.VInt (Z.of_int 5));
    VM.STOP;
  |]

let continuation_token_code =
  [|
    VM.JDEST Abi.advance_label;
    VM.MLOAD (2, Abi.sequence_cell);
    VM.MSTORE (10, 2);
    VM.LDI (0, VM.VInt (Z.of_int 10));
    VM.LDI (1, VM.VInt Z.one);
    VM.STOP;
  |]

let committed_state_payload = "state:0"
let committed_state_payload_root = sha256 committed_state_payload

let selected_index_u64le_payload index =
  Bytes.init 8 (fun offset ->
    Char.chr ((index lsr (offset * 8)) land 0xff))
  |> Bytes.to_string

let feedback_prefill_payload = "prefill"
let feedback_selected_index = 258
let feedback_selected_index_offset = 2
let feedback_selected_index_payload =
  "ix" ^ selected_index_u64le_payload feedback_selected_index
let feedback_selected_index_payload_root = sha256 feedback_selected_index_payload

let committed_state_token_code =
  [|
    VM.JDEST Abi.advance_label;
    VM.MLOAD (20, Abi.sequence_cell);
    VM.LDI (21, VM.VInt Z.zero);
    VM.EQ (22, 20, 21);
    VM.JIF (22, 101);
    VM.MLOAD (5, Abi.committed_target_state_root_cell);
    VM.FLOAD (6, 5);
    VM.MSTORE (30, 6);
    VM.LDI (2, VM.VInt Z.one);
    VM.MSTORE (10, 2);
    VM.LDI (0, VM.VInt (Z.of_int 10));
    VM.LDI (1, VM.VInt Z.one);
    VM.STOP;
    VM.JDEST 101;
    VM.LDI (5, VM.VString committed_state_payload);
    VM.FSTORE (6, 5);
    VM.MSTORE (Abi.committed_target_state_root_cell, 6);
    VM.LDI (2, VM.VInt Z.one);
    VM.MSTORE (10, 2);
    VM.LDI (0, VM.VInt (Z.of_int 10));
    VM.LDI (1, VM.VInt Z.one);
    VM.STOP;
  |]

let committed_state_feedback_code =
  [|
    VM.JDEST Abi.advance_label;
    VM.MLOAD (20, Abi.sequence_cell);
    VM.LDI (21, VM.VInt Z.zero);
    VM.EQ (22, 20, 21);
    VM.JIF (22, 101);
    VM.LDI (21, VM.VInt Z.one);
    VM.EQ (22, 20, 21);
    VM.JIF (22, 102);
    VM.MLOAD (5, Abi.committed_target_state_root_cell);
    VM.FLOAD (6, 5);
    VM.MSTORE (30, 6);
    VM.LDI (2, VM.VInt (Z.of_int feedback_selected_index));
    VM.MSTORE (10, 2);
    VM.LDI (0, VM.VInt (Z.of_int 10));
    VM.LDI (1, VM.VInt Z.one);
    VM.STOP;
    VM.JDEST 101;
    VM.LDI (5, VM.VString feedback_prefill_payload);
    VM.FSTORE (6, 5);
    VM.MSTORE (Abi.committed_target_state_root_cell, 6);
    VM.LDI (2, VM.VInt Z.zero);
    VM.MSTORE (10, 2);
    VM.LDI (0, VM.VInt (Z.of_int 10));
    VM.LDI (1, VM.VInt Z.one);
    VM.STOP;
    VM.JDEST 102;
    VM.LDI (5, VM.VString feedback_selected_index_payload);
    VM.FSTORE (6, 5);
    VM.MSTORE (Abi.committed_target_state_root_cell, 6);
    VM.LDI (2, VM.VInt (Z.of_int feedback_selected_index));
    VM.MSTORE (10, 2);
    VM.LDI (0, VM.VInt (Z.of_int 10));
    VM.LDI (1, VM.VInt Z.one);
    VM.STOP;
  |]

let graph_real_code =
  [|
    VM.JDEST Abi.advance_label;
    VM.LDI (0, VM.VInt (Z.of_int 20));
    VM.LDI (1, VM.VString "\000\000\000\000\000\000\240\063");
    VM.LDI (2, VM.VInt Z.zero);
    VM.LDI (3, VM.VInt Z.one);
    VM.LOAD_F64_LE_FP (0, 1, 2, 3);
    VM.SILU_FP (0, 3);
    VM.LDI (0, VM.VInt (Z.of_int 20));
    VM.LDI (1, VM.VInt Z.one);
    VM.STOP;
  |]

let dead_branch_graph_real_code =
  [|
    VM.JDEST Abi.advance_label;
    VM.LDI (22, VM.VInt Z.one);
    VM.LDI (23, VM.VInt Z.one);
    VM.EQ (24, 22, 23);
    VM.JIF (24, 101);
    VM.LDI (0, VM.VInt (Z.of_int 20));
    VM.LDI (1, VM.VString "\000\000\000\000\000\000\240\063");
    VM.LDI (2, VM.VInt Z.zero);
    VM.LDI (3, VM.VInt Z.one);
    VM.LOAD_F64_LE_FP (0, 1, 2, 3);
    VM.SILU_FP (0, 3);
    VM.JDEST 101;
    VM.LDI (2, VM.VInt (Z.of_int 7));
    VM.MSTORE (10, 2);
    VM.LDI (0, VM.VInt (Z.of_int 10));
    VM.LDI (1, VM.VInt Z.one);
    VM.STOP;
  |]

let graph_real_feedback_code =
  [|
    VM.JDEST Abi.advance_label;
    VM.LDI (0, VM.VInt (Z.of_int 40));
    VM.LDI (1, VM.VString "\000\000\000\000\000\000\240\063");
    VM.LDI (2, VM.VInt Z.zero);
    VM.LDI (3, VM.VInt Z.one);
    VM.LOAD_F64_LE_FP (0, 1, 2, 3);
    VM.SILU_FP (0, 3);
    VM.MLOAD (20, Abi.sequence_cell);
    VM.LDI (21, VM.VInt Z.zero);
    VM.EQ (22, 20, 21);
    VM.JIF (22, 101);
    VM.LDI (21, VM.VInt Z.one);
    VM.EQ (22, 20, 21);
    VM.JIF (22, 102);
    VM.MLOAD (5, Abi.committed_target_state_root_cell);
    VM.FLOAD (6, 5);
    VM.MSTORE (30, 6);
    VM.LDI (2, VM.VInt (Z.of_int feedback_selected_index));
    VM.MSTORE (10, 2);
    VM.LDI (0, VM.VInt (Z.of_int 10));
    VM.LDI (1, VM.VInt Z.one);
    VM.STOP;
    VM.JDEST 101;
    VM.LDI (5, VM.VString feedback_prefill_payload);
    VM.FSTORE (6, 5);
    VM.MSTORE (Abi.committed_target_state_root_cell, 6);
    VM.LDI (2, VM.VInt Z.zero);
    VM.MSTORE (10, 2);
    VM.LDI (0, VM.VInt (Z.of_int 10));
    VM.LDI (1, VM.VInt Z.one);
    VM.STOP;
    VM.JDEST 102;
    VM.LDI (5, VM.VString feedback_selected_index_payload);
    VM.FSTORE (6, 5);
    VM.MSTORE (Abi.committed_target_state_root_cell, 6);
    VM.LDI (2, VM.VInt (Z.of_int feedback_selected_index));
    VM.MSTORE (10, 2);
    VM.LDI (0, VM.VInt (Z.of_int 10));
    VM.LDI (1, VM.VInt Z.one);
    VM.STOP;
  |]

let selected_index_contract ?(output_base = 10) () =
  `Assoc [
    "kind", `String "selected_index";
    "output_base", `Int output_base;
    "output_count", `Int 1;
  ]

let selected_index_feedback_contract ?(offset = 0) source_transition_id =
  `Assoc [
    "kind", `String "previous_selected_index_u64le";
    "source_transition_id", `String source_transition_id;
    "offset", `Int offset;
  ]

let graph_real_contract ?min_program_instructions () =
  let min_fields =
    match min_program_instructions with
    | None -> []
    | Some value -> ["min_program_instructions", `Int value]
  in
  `Assoc (["kind", `String "graph_real"] @ min_fields)

let lifecycle_only_contract =
  `Assoc ["kind", `String "lifecycle_only"]

let write_batch_fixture
    ?(session_abi_root = Abi.v1_root)
    ?code
    dir =
  let stage_dir = Filename.concat dir "stage" in
  Unix.mkdir stage_dir 0o700;
  let code =
    match code with
    | Some code -> code
    | None ->
      [|
        VM.JDEST Abi.advance_label;
        VM.LDI (2, VM.VInt (Z.of_int 7));
        VM.MSTORE (10, 2);
        VM.LDI (0, VM.VInt (Z.of_int 10));
        VM.LDI (1, VM.VInt Z.one);
        VM.STOP;
      |]
  in
  let continuation = Abi.continuation_supported session_abi_root in
  let committed_state = Abi.committed_state_supported session_abi_root in
  let max_output_bytes = if continuation then 512 else 64 in
  let max_scratch_bytes = if continuation then 2048 else 128 in
  let capability = Req.{ name = "storage.authenticated-range"; root = hex_root 'd' } in
  let strict_fp_capability =
    Req.{ name = "tensor.strict-fp"; root = hex_root '7' }
  in
  let capabilities =
    [capability; strict_fp_capability]
    @
    if committed_state then
      [Req.{ name = "session.committed-state"; root = hex_root '6' }]
    else []
  in
  let limits =
    Req.{
      max_model_bytes = 64;
      max_view_bytes = 64;
      max_session_bytes = 1024;
      max_scratch_bytes;
      max_output_bytes;
      max_advance_effort = 4096;
    }
  in
  let requirement =
    Req.{
      vm_semantics_root = hex_root 'a';
      numerical_root = hex_root 'b';
      effort_root = hex_root 'c';
      capabilities;
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
      session_abi_root;
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
      max_output_bytes;
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

let stage_json_from_batch batch_path =
  match Yojson.Safe.from_file batch_path with
  | `Assoc fields ->
    (match list_json "stages" fields with
     | [stage] -> stage
     | _ -> failwith "expected one batch stage")
  | _ -> failwith "batch must be an object"

let write_session_bundle_fixture
    ?(schema = Some "octra.inference.session.bundle")
    ?(decode_steps = Some 1)
    ?session_abi_root
    ?code
    ?phase_for_index
    ?execution_contract_for_index
    ?output_contract_for_index
    ?prior_state_contract_for_index
    ?request_root
    ?model_deployment_root
    ?second_request_nonce
    ?(transition_count = 1)
    dir =
  let batch_path =
    write_batch_fixture ?session_abi_root ?code dir
  in
  let stage = stage_json_from_batch batch_path in
  let second_stage =
    match second_request_nonce with
    | None -> stage
    | Some request_nonce ->
      let request_path = Filename.concat dir "stage/request.json" in
      let request =
        request_from_json (Yojson.Safe.from_file request_path)
      in
      let second_request =
        Request.{ request with request_nonce }
      in
      write_json
        (Filename.concat dir "stage/request-second.json")
        (request_json second_request);
      replace_assoc_field
        "request"
        (`String "stage/request-second.json")
        stage
  in
  let transitions =
    List.init
      transition_count
      (fun index ->
         let output_contract_fields =
           match output_contract_for_index with
           | None -> []
           | Some output_contract ->
             (match output_contract index with
              | None -> []
              | Some value -> ["output_contract", value])
         in
         let execution_contract_fields =
           match execution_contract_for_index with
           | None -> []
           | Some execution_contract ->
             (match execution_contract index with
              | None -> []
              | Some value -> ["execution_contract", value])
         in
         let prior_state_contract_fields =
           match prior_state_contract_for_index with
           | None -> []
           | Some prior_state_contract ->
             (match prior_state_contract index with
              | None -> []
              | Some value -> ["prior_state_contract", value])
         in
         `Assoc
           ([
             "transition_id", `String (Printf.sprintf "token-%03d" index);
             "phase",
             `String
               (match phase_for_index with
                | Some phase -> phase index
                | None ->
                  if transition_count = 1 then "decode"
                  else if index = 0 then "prefill"
                  else "decode");
           ]
            @ execution_contract_fields
            @ output_contract_fields
            @ prior_state_contract_fields
            @ [
              "stage", (if index = 1 then second_stage else stage);
            ]))
  in
  let bundle_path = Filename.concat dir "session-bundle.cjson" in
  let schema_fields =
    match schema with
    | None -> []
    | Some value -> ["schema", `String value]
  in
  let decode_fields =
    match decode_steps with
    | None -> []
    | Some value -> ["decode_steps", `Int value]
  in
  let declared_root_fields =
    (match request_root with
     | None -> []
     | Some value -> ["request_root", `String value])
    @
    (match model_deployment_root with
     | None -> []
     | Some value -> ["model_deployment_root", `String value])
  in
  write_json
    bundle_path
    (`Assoc
       (schema_fields
        @ decode_fields
        @ declared_root_fields
        @ [
          "transitions", `List transitions;
        ]));
  bundle_path

let run_session_bundle_raw ?(timing_mode = Some "opcode") bundle_path =
  let timing_fields =
    match timing_mode with
    | None -> []
    | Some mode -> ["--timing-mode"; mode]
  in
  let command =
    String.concat
      " "
      ([
        Filename.quote (tool_path ());
        "--run-inference-session";
        Filename.quote bundle_path;
      ]
       @ timing_fields)
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
  code, raw

let run_session_bundle bundle_path =
  let code, raw = run_session_bundle_raw bundle_path in
  code, Yojson.Safe.from_string raw

let check_runtime_semantics report =
  match report with
  | `Assoc fields ->
    let semantics = assoc_json "runtime_semantics" fields in
    check
      "runtime semantics diagnostic"
      (bool_json "diagnostic_only" semantics);
    check
      "batch lifecycle root"
      (String.equal
         (string_json "resident_session_lifecycle_root" semantics)
         Abi.resident_lifecycle_root);
    check
      "batch lifecycle status"
      (String.equal
         (string_json "resident_session_lifecycle_status" semantics)
         "not_supported_by_independent_batch");
    check
      "session mode"
      (String.equal
         (string_json "session_mode" semantics)
         "independent_session_per_stage");
    check
      "product lifecycle"
      (list_json "product_lifecycle" semantics
       = [
         `String "open_session";
         `String "prefill";
         `String "decode";
         `String "finalize";
       ]);
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
      "resident cache empty"
      (list_json "resident_cache_scope" semantics = []);
    check
      "state carry unsupported"
      (String.equal
         (string_json "state_carry" semantics)
         "not_supported");
    check
      "runtime readiness rejected"
      (String.equal
         (string_json "runtime_readiness_status" semantics)
         "rejected");
    check
      "next runtime blocker"
      (String.equal
         (string_json "next_runtime_blocker" semantics)
         "session_continuation_state_carry_not_supported");
    check
      "missing runtime capabilities"
      (list_json "missing_runtime_capabilities" semantics
       = [
         `String "resident_session";
         `String "multi_advance_state_carry";
         `String "prefill_decode_phase_contract";
         `String "decode_loop_token_contract";
       ])
  | _ -> failwith "report must be an object"

let check_session_runtime_semantics
    ?(next_runtime_blocker = "session_continuation_state_carry_not_supported")
    ?(decode_token_required = 1)
    ?(decode_token_bound = 0)
    ?(decode_token_mismatched = 0)
    ?(decode_prior_required = 0)
    ?(decode_prior_bound = 0)
    ?(decode_prior_mismatched = 0)
    semantics =
  check
    "session runtime diagnostic"
    (bool_json "diagnostic_only" semantics);
  check
    "session lifecycle root"
    (String.equal
       (string_json "resident_session_lifecycle_root" semantics)
       Abi.resident_lifecycle_root);
  check
    "session product lifecycle"
    (list_json "product_lifecycle" semantics
     = [
       `String "open_session";
       `String "prefill";
       `String "decode";
       `String "finalize";
     ]);
  check
    "session continuation unsupported"
    (not (bool_json "continuation_supported" semantics));
  check
    "session state carry unsupported"
    (String.equal
       (string_json "state_carry" semantics)
       "not_supported");
  check
    "session next blocker"
    (String.equal
       (string_json "next_runtime_blocker" semantics)
       next_runtime_blocker);
  let token_summary = assoc_json "decode_token_contract_summary" semantics in
  check
    "session decode token contracts required"
    (int_json "required" token_summary = decode_token_required);
  check
    "session decode token contracts bound"
    (int_json "bound" token_summary = decode_token_bound);
  check
    "session decode token contracts mismatched"
    (int_json "mismatched" token_summary = decode_token_mismatched);
  let prior_summary =
    assoc_json "decode_prior_state_contract_summary" semantics
  in
  check
    "session decode prior contracts required"
    (int_json "required" prior_summary = decode_prior_required);
  check
    "session decode prior contracts bound"
    (int_json "bound" prior_summary = decode_prior_bound);
  check
    "session decode prior contracts mismatched"
    (int_json "mismatched" prior_summary = decode_prior_mismatched);
  let graph_summary = assoc_json "graph_execution_contract_summary" semantics in
  check
    "session graph contracts not required"
    (int_json "required" graph_summary = 0);
  check
    "session graph contracts not bound"
    (int_json "bound" graph_summary = 0);
  check
    "session graph contracts not mismatched"
    (int_json "mismatched" graph_summary = 0)

let check_contract_summary
    field
    label
    semantics
    ~required
    ~bound
    ~mismatched =
  let summary = assoc_json field semantics in
  let check_int suffix actual expected =
    check
      (Printf.sprintf
         "%s %s: expected %d actual %d"
         label
         suffix
         expected
         actual)
      (actual = expected)
  in
  check_int "required count" (int_json "required" summary) required;
  check_int "bound count" (int_json "bound" summary) bound;
  check_int "mismatched count" (int_json "mismatched" summary) mismatched

let check_decode_token_summary =
  check_contract_summary "decode_token_contract_summary" "decode token"

let check_decode_prior_state_summary =
  check_contract_summary
    "decode_prior_state_contract_summary"
    "decode prior-state"

let check_graph_execution_summary =
  check_contract_summary
    "graph_execution_contract_summary"
    "graph execution"

let check_transition_root_chain_summary
    semantics
    ~transition_count
    ~complete_transitions
    ~advanced_session_roots
    ~advance_receipt_roots
    ~output_prefix_roots
    ~incomplete_transition_ids =
  let summary =
    assoc_json "transition_root_chain_summary" semantics
  in
  check
    "transition root chain count"
    (int_json "transition_count" summary = transition_count);
  check
    "transition root chain complete count"
    (int_json "complete_transitions" summary = complete_transitions);
  check
    "transition root chain session roots"
    (int_json "advanced_session_roots" summary = advanced_session_roots);
  check
    "transition root chain receipt roots"
    (int_json "advance_receipt_roots" summary = advance_receipt_roots);
  check
    "transition root chain output prefix roots"
    (int_json "output_prefix_roots" summary = output_prefix_roots);
  check
    "transition root chain incomplete ids"
    (list_json "incomplete_transition_ids" summary
     = List.map (fun value -> `String value) incomplete_transition_ids)

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

let check_session_hash report =
  match report with
  | `Assoc fields ->
    let expected_hash = string_json "session_report_sha256" fields in
    let transitions = list_json "transitions" fields in
    let sanitized =
      `List
        (List.map
           (drop_assoc_fields ["timing"; "execution_timing"; "opcode_timing"])
           transitions)
    in
    let envelope_fields = [
        "schema", `String (string_json "schema" fields);
        "status", `String (string_json "status" fields);
        "resident_session_lifecycle_root",
        assoc_value "resident_session_lifecycle_root" fields;
        "transition_count", `Int (int_json "transition_count" fields);
        "decode_steps", `Int (int_json "decode_steps" fields);
        "runtime_semantics", assoc_value "runtime_semantics" fields;
        "next_runtime_blocker", assoc_value "next_runtime_blocker" fields;
        "opened_session_root", assoc_value "opened_session_root" fields;
        "final_session_root", assoc_value "final_session_root" fields;
        "final_receipt_root", assoc_value "final_receipt_root" fields;
        "output_prefix_root", assoc_value "output_prefix_root" fields;
        "last_transition_output_payload",
        assoc_value "last_transition_output_payload" fields;
        "last_transition_output_payload_sha256",
        assoc_value "last_transition_output_payload_sha256" fields;
        "last_transition_output_root",
        assoc_value "last_transition_output_root" fields;
        "unsupported_opcodes", assoc_value "unsupported_opcodes" fields;
        "missing_capabilities", assoc_value "missing_capabilities" fields;
        "policy_violations", assoc_value "policy_violations" fields;
        "transitions", sanitized;
      ]
    in
    let envelope_fields =
      match List.assoc_opt "continuation_preflight" fields with
      | None -> envelope_fields
      | Some value -> envelope_fields @ ["continuation_preflight", value]
    in
    let sanitized_envelope = `Assoc envelope_fields
    in
    let payload =
      "octra.inference.run_inference_session.report.v1:"
      ^ Yojson.Safe.to_string sanitized_envelope
    in
    check
      "session report hash"
      (String.equal expected_hash (sha256 payload))
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
       ignore
         (check_payload_sha
            stage
            ~payload_field:"output_payload"
            ~sha_field:"output_payload_sha256");
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

let check_session_bundle_single_transition () =
  with_temp_dir "octra-inference-session-bundle-test" @@ fun dir ->
  let bundle_path = write_session_bundle_fixture dir in
  let code, report = run_session_bundle bundle_path in
  check "session bundle exit code" (code = 0);
  match report with
  | `Assoc fields ->
    check
      "session report status"
      (String.equal (string_json "status" fields) "accepted");
    check
      "session report schema"
      (String.equal
         (string_json "schema" fields)
         "octra.inference.session.report");
    check "session transition count" (int_json "transition_count" fields = 1);
    check "session decode steps" (int_json "decode_steps" fields = 1);
    check
      "session last output root"
      (String.length (string_json "last_transition_output_root" fields) = 64);
    ignore
      (check_payload_sha
         fields
         ~payload_field:"last_transition_output_payload"
         ~sha_field:"last_transition_output_payload_sha256");
    check "single opened root null" (assoc_value "opened_session_root" fields = `Null);
    check "single final root null" (assoc_value "final_session_root" fields = `Null);
    check
      "single final receipt root null"
      (assoc_value "final_receipt_root" fields = `Null);
    check
      "single output prefix root null"
      (assoc_value "output_prefix_root" fields = `Null);
    let semantics = assoc_json "runtime_semantics" fields in
    check_session_runtime_semantics semantics;
    check
      "single transition mode"
      (String.equal
         (string_json "session_mode" semantics)
         "single_transition_session");
    check
      "single transition readiness"
      (String.equal
         (string_json "runtime_readiness_status" semantics)
         "partial_single_transition");
    check_session_hash report;
    (match list_json "transitions" fields with
     | [`Assoc transition] ->
       check
         "transition id"
         (String.equal (string_json "transition_id" transition) "token-000");
       check
         "transition phase"
         (String.equal (string_json "phase" transition) "decode");
       check
         "transition status"
         (String.equal (string_json "status" transition) "accepted");
       check
         "transition session"
         (String.equal (string_json "session_status" transition) "accepted")
     | _ -> failwith "expected one transition")
  | _ -> failwith "report must be an object"

let check_session_bundle_hash_binds_decode_steps () =
  with_temp_dir "octra-inference-session-bundle-test" @@ fun dir ->
  let first_dir = Filename.concat dir "first" in
  let second_dir = Filename.concat dir "second" in
  Unix.mkdir first_dir 0o700;
  Unix.mkdir second_dir 0o700;
  let first = write_session_bundle_fixture ~decode_steps:(Some 0) first_dir in
  let second = write_session_bundle_fixture ~decode_steps:(Some 1) second_dir in
  let first_code, first_report = run_session_bundle first in
  let second_code, second_report = run_session_bundle second in
  check "first decode-step run accepted" (first_code = 0);
  check "second decode-step run accepted" (second_code = 0);
  match first_report, second_report with
  | `Assoc first_fields, `Assoc second_fields ->
    check_session_hash first_report;
    check_session_hash second_report;
    check
      "decode step changes session report hash"
      (not
         (String.equal
            (string_json "session_report_sha256" first_fields)
            (string_json "session_report_sha256" second_fields)))
  | _ -> failwith "reports must be objects"

let check_session_bundle_rejects_multi_transition () =
  with_temp_dir "octra-inference-session-bundle-test" @@ fun dir ->
  let bundle_path = write_session_bundle_fixture ~transition_count:2 dir in
  let code, report = run_session_bundle bundle_path in
  check "multi-transition session exits nonzero" (code = 1);
  match report with
  | `Assoc fields ->
    check
      "multi-transition status"
      (String.equal (string_json "status" fields) "rejected");
    check "multi-transition count" (int_json "transition_count" fields = 2);
    let semantics = assoc_json "runtime_semantics" fields in
    check_session_runtime_semantics
      ~next_runtime_blocker:"session_abi_v2_required"
      semantics;
    check
      "multi-transition mode"
      (String.equal
         (string_json "session_mode" semantics)
         "multi_transition_session_requested");
    check
      "multi-transition readiness"
      (String.equal
         (string_json "runtime_readiness_status" semantics)
         "rejected");
    check
      "multi-transition blocker"
      (String.equal
         (string_json "next_runtime_blocker" fields)
         "session_abi_v2_required");
    check "no executed transitions" (list_json "transitions" fields = []);
    check
      "no rejected output payload"
      (assoc_value "last_transition_output_payload" fields = `Null);
    check
      "no rejected output payload sha"
      (assoc_value "last_transition_output_payload_sha256" fields = `Null);
    check_session_hash report;
    let preflight = assoc_json "continuation_preflight" fields in
    check
      "preflight blocked"
      (String.equal (string_json "status" preflight) "blocked");
    check
      "preflight readiness rejected"
      (String.equal
         (string_json "runtime_readiness_status" preflight)
         "rejected");
    check
      "preflight next blocker"
      (String.equal
         (string_json "next_runtime_blocker" preflight)
         "session_abi_v2_required");
    check
      "preflight did not execute"
      (not (bool_json "execution_attempted" preflight));
    check
      "preflight lifecycle contract"
      (String.equal
         (string_json "phase_contract" preflight)
         Abi.resident_lifecycle_schema);
    check
      "preflight continuation undefined"
      (String.equal
         (string_json "continuation_basis" preflight)
         "undefined");
    check
      "preflight no state payload"
      (not (bool_json "state_payload_transport_supported" preflight));
    check
      "preflight no repeated advance"
      (not (bool_json "repeated_advance_supported" preflight));
    check
      "preflight has output-prefix hash primitive"
      (bool_json "output_prefix_hash_primitive_available" preflight);
    check
      "preflight no continuation output-prefix support"
      (not (bool_json "continuation_output_prefix_supported" preflight));
    check
      "preflight phase sequence"
      (list_json "declared_phase_sequence" preflight
       = [`String "prefill"; `String "decode"]);
    check
      "preflight decode transition count"
      (int_json "declared_decode_transitions" preflight = 1);
    check
      "preflight decode steps match"
      (bool_json "decode_steps_match" preflight);
    let identities = assoc_json "identity_checks" preflight in
    check
      "preflight target root uniform"
      (bool_json "target_root_uniform" identities);
    check
      "preflight request root uniform"
      (bool_json "request_root_uniform" identities);
    check
      "preflight model ranges root uniform"
      (bool_json "model_ranges_root_uniform" identities);
    check
      "preflight model deployment root uniform"
      (bool_json "model_deployment_root_uniform" identities);
    check
      "preflight session abi root uniform"
      (bool_json "session_abi_root_uniform" identities);
    let claims = assoc_json "top_level_claims" preflight in
    check
      "request root not declared"
      (String.equal
         (string_json "request_root_status" claims)
         "not_declared");
    check
      "model deployment root not declared"
      (String.equal
         (string_json "model_deployment_root_status" claims)
         "not_declared");
    check
      "no identity blockers"
      (list_json "identity_blockers" preflight = []);
    check
      "no declaration blockers"
      (list_json "declaration_blockers" preflight = []);
    let blockers = list_json "blockers" preflight in
    List.iter
      (fun expected ->
         check
           ("preflight blocker " ^ expected)
           (List.exists (( = ) (`String expected)) blockers))
      [
        "session_abi_v2_required";
      ];
    (match list_json "transition_plan" preflight with
     | [`Assoc first; `Assoc second] ->
       let check_plan_entry expected_index expected_id expected_phase entry =
         check
           ("plan index " ^ expected_id)
           (int_json "index" entry = expected_index);
         check
           ("plan transition id " ^ expected_id)
           (String.equal
              (string_json "transition_id" entry)
              expected_id);
         check
           ("plan phase " ^ expected_id)
           (String.equal (string_json "phase" entry) expected_phase);
         check
           ("plan stage id " ^ expected_id)
           (String.equal (string_json "stage_id" entry) "unit-stage");
         check
           ("plan not run " ^ expected_id)
           (String.equal
              (string_json "execution_status" entry)
              "not_run");
         check
           ("plan created " ^ expected_id)
           (bool_json "plan_created" entry);
         check
           ("plan input checked " ^ expected_id)
           (bool_json "input_payload_checked" entry);
         check
           ("plan ranges checked " ^ expected_id)
           (bool_json "range_payloads_checked" entry);
         check
           ("plan preflight status " ^ expected_id)
           (String.equal
              (string_json "preflight_status" entry)
              "plan_created");
         List.iter
           (fun field ->
              check
                (Printf.sprintf "plan %s root %s" expected_id field)
                (String.length (string_json field entry) = 64))
           [
             "program_root";
             "requirement_root";
             "target_root";
             "request_root";
             "model_ranges_root";
             "session_abi_root";
           ];
         List.iter
           (fun field ->
              check
                (Printf.sprintf "plan %s has no %s" expected_id field)
                (List.assoc_opt field entry = None))
           [
             "opened_session_root";
             "advanced_session_root";
             "final_session_root";
             "advance_receipt_root";
             "candidate_root";
             "output_prefix_root";
             "output_root";
           ]
       in
       check_plan_entry 0 "token-000" "prefill" first;
       check_plan_entry 1 "token-001" "decode" second
     | _ -> failwith "expected two preflight transitions")
  | _ -> failwith "report must be an object"

let check_session_bundle_accepts_v2_multi_transition () =
  with_temp_dir "octra-inference-session-bundle-test" @@ fun dir ->
  let bundle_path =
    write_session_bundle_fixture
      ~session_abi_root:Abi.v2_root
      ~code:continuation_code
      ~transition_count:2
      dir
  in
  let code, report = run_session_bundle bundle_path in
  check "v2 multi-transition exits incomplete" (code = 1);
  match report with
  | `Assoc fields ->
    check
      "v2 multi-transition incomplete"
      (String.equal (string_json "status" fields) "runtime_incomplete");
    check "v2 transition count" (int_json "transition_count" fields = 2);
    check "v2 decode steps" (int_json "decode_steps" fields = 1);
    check
      "v2 no continuation preflight in accepted report"
      (List.assoc_opt "continuation_preflight" fields = None);
    let opened_session_root = string_json "opened_session_root" fields in
    let final_session_root = string_json "final_session_root" fields in
    let final_receipt_root = string_json "final_receipt_root" fields in
    let output_prefix_root = string_json "output_prefix_root" fields in
    let last_transition_output_root =
      string_json "last_transition_output_root" fields
    in
    let last_transition_output_payload =
      check_payload_sha
        fields
        ~payload_field:"last_transition_output_payload"
        ~sha_field:"last_transition_output_payload_sha256"
    in
    List.iter
      (fun (label, root) ->
         check
           ("v2 root " ^ label)
           (String.length root = 64))
      [
        "opened_session_root", opened_session_root;
        "final_session_root", final_session_root;
        "final_receipt_root", final_receipt_root;
        "output_prefix_root", output_prefix_root;
        "last_transition_output_root", last_transition_output_root;
      ];
    check
      "v2 session root advanced"
      (not (String.equal opened_session_root final_session_root));
    let semantics = assoc_json "runtime_semantics" fields in
    check
      "v2 resident mode"
      (String.equal
         (string_json "session_mode" semantics)
         "resident_multi_transition_session");
    check
      "v2 lifecycle status"
      (String.equal
         (string_json "resident_session_lifecycle_status" semantics)
         "bound");
    check
      "v2 continuation supported"
      (bool_json "continuation_supported" semantics);
    check
      "v2 state carry"
      (String.equal
         (string_json "state_carry" semantics)
         "abi_v2_progress_cells");
    check
      "v2 resident cache scope"
      (list_json "resident_cache_scope" semantics
       = [`String "session"]);
    check
      "v2 resident readiness"
      (String.equal
         (string_json "runtime_readiness_status" semantics)
         "resident_session_candidate");
    check
      "v2 remaining runtime blocker"
      (String.equal
         (string_json "next_runtime_blocker" semantics)
         "committed_target_state_payload_transport_not_bound");
    check
      "v2 remaining missing capabilities"
      (list_json "missing_runtime_capabilities" semantics
       = [
         `String "committed_target_state_payload_transport";
         `String "decode_loop_token_contract";
       ]);
    check
      "v2 decode token contract not bound"
      (String.equal
         (string_json "decode_token_contract_status" semantics)
         "not_bound");
    check_session_hash report;
    (match list_json "transitions" fields with
     | [`Assoc first; `Assoc second] ->
       let check_transition expected_id expected_phase transition =
         check
           ("v2 transition id " ^ expected_id)
           (String.equal
              (string_json "transition_id" transition)
              expected_id);
         check
           ("v2 transition phase " ^ expected_id)
           (String.equal (string_json "phase" transition) expected_phase);
         check
           ("v2 transition stage " ^ expected_id)
           (String.equal (string_json "stage_id" transition) "unit-stage");
         check
           ("v2 transition accepted " ^ expected_id)
           (String.equal (string_json "status" transition) "accepted");
         check
           ("v2 transition session accepted " ^ expected_id)
           (String.equal
              (string_json "session_status" transition)
              "accepted");
         check
           ("v2 transition unchecked reference " ^ expected_id)
           (String.equal
              (string_json "reference_status" transition)
              "unchecked");
         List.iter
           (fun field ->
              check
                (Printf.sprintf "v2 transition %s root %s" expected_id field)
                (String.length (string_json field transition) = 64))
         [
           "program_root";
           "target_root";
           "request_root";
           "model_ranges_root";
           "prior_session_root";
           "advanced_session_root";
           "advance_receipt_root";
           "output_root";
           "output_prefix_root";
           "candidate_root";
          ];
         ignore
           (check_payload_sha
              transition
              ~payload_field:"output_payload"
              ~sha_field:"output_payload_sha256")
       in
       check_transition "token-000" "prefill" first;
       check_transition "token-001" "decode" second;
       check
         "first prior is opened session"
         (String.equal
            (string_json "prior_session_root" first)
            opened_session_root);
       check
         "second prior is first advanced session"
         (String.equal
            (string_json "prior_session_root" second)
            (string_json "advanced_session_root" first));
       check
         "last output is second output"
         (String.equal
            last_transition_output_root
            (string_json "output_root" second));
       check
         "last payload is second payload"
         (String.equal
            last_transition_output_payload
            (string_json "output_payload" second));
         check
           "outputs differ across progress"
           (not
              (String.equal
                 (string_json "output_root" first)
                 (string_json "output_root" second)));
         check
           "payloads differ across progress"
           (not
              (String.equal
                 (string_json "output_payload" first)
                 (string_json "output_payload" second)))
       | _ -> failwith "expected two resident transitions")
    | _ -> failwith "report must be an object"

let check_session_bundle_binds_graph_execution_contract () =
  with_temp_dir "octra-inference-session-bundle-test" @@ fun dir ->
  let bundle_path =
    write_session_bundle_fixture
      ~session_abi_root:Abi.v2_root
      ~code:graph_real_code
      ~execution_contract_for_index:(fun index ->
        if index = 0 then
          Some (graph_real_contract ~min_program_instructions:4 ())
        else Some lifecycle_only_contract)
      ~transition_count:2
      dir
  in
  let code, report = run_session_bundle bundle_path in
  check "graph-contract bundle exits incomplete" (code = 1);
  match report with
  | `Assoc fields ->
    check
      "graph-contract bundle incomplete"
      (String.equal (string_json "status" fields) "runtime_incomplete");
    let semantics = assoc_json "runtime_semantics" fields in
    check
      "graph contract bound"
      (String.equal
         (string_json "graph_execution_contract_status" semantics)
         "bound");
    check_graph_execution_summary
      semantics
      ~required:1
      ~bound:1
      ~mismatched:0;
    check_session_hash report;
    (match list_json "transitions" fields with
     | [`Assoc first; `Assoc second] ->
       let first_contract = assoc_json "execution_contract" first in
       check
         "first graph contract matched"
         (String.equal (string_json "status" first_contract) "matched");
       check "first graph real" (bool_json "graph_real" first_contract);
       check
         "first graph opcode"
         (List.exists
            (( = ) (`String "SILU_FP"))
            (list_json "inference_opcodes" first_contract));
       check
         "first executed graph opcode"
         (List.exists
            (( = ) (`String "SILU_FP"))
            (list_json "executed_inference_opcodes" first_contract));
       let second_contract = assoc_json "execution_contract" second in
       check
         "second lifecycle contract matched"
         (String.equal (string_json "status" second_contract) "matched");
       check
         "second lifecycle-only"
         (not (bool_json "graph_real" second_contract))
     | _ -> failwith "expected two resident transitions")
  | _ -> failwith "report must be an object"

let check_session_bundle_rejects_graph_execution_overclaim () =
  with_temp_dir "octra-inference-session-bundle-test" @@ fun dir ->
  let bundle_path =
    write_session_bundle_fixture
      ~session_abi_root:Abi.v2_root
      ~code:continuation_code
      ~execution_contract_for_index:(fun _ ->
        Some (graph_real_contract ~min_program_instructions:4 ()))
      ~transition_count:2
      dir
  in
  let code, report = run_session_bundle bundle_path in
  check "graph overclaim exits nonzero" (code = 1);
  match report with
  | `Assoc fields ->
    check
      "graph overclaim rejected"
      (String.equal (string_json "status" fields) "rejected");
    check
      "graph overclaim blocker"
      (String.equal
         (string_json "next_runtime_blocker" fields)
         "graph_execution_contract_mismatch");
    let semantics = assoc_json "runtime_semantics" fields in
    check_graph_execution_summary
      semantics
      ~required:2
      ~bound:0
      ~mismatched:2;
    check "graph overclaim no executed transitions" (list_json "transitions" fields = []);
    check_session_hash report;
    let preflight = assoc_json "continuation_preflight" fields in
    check
      "graph overclaim declaration rejected"
      (String.equal
         (string_json "runtime_readiness_status" preflight)
         "declaration_rejected");
    check
      "graph overclaim preflight blocker"
      (String.equal
         (string_json "next_runtime_blocker" preflight)
         "graph_execution_contract_mismatch");
    (match list_json "transition_plan" preflight with
     | `Assoc first :: _ ->
       let contract = assoc_json "execution_contract" first in
       check
         "graph overclaim contract mismatch"
         (String.equal (string_json "status" contract) "mismatch");
       check
         "graph overclaim reason"
         (String.equal
            (string_json "reason" contract)
            "missing_inference_opcode")
     | _ -> failwith "expected preflight transitions")
  | _ -> failwith "report must be an object"

let check_single_transition_rejects_graph_execution_contract () =
  with_temp_dir "octra-inference-session-bundle-test" @@ fun dir ->
  let bundle_path =
    write_session_bundle_fixture
      ~session_abi_root:Abi.v2_root
      ~code:graph_real_code
      ~execution_contract_for_index:(fun _ ->
        Some (graph_real_contract ~min_program_instructions:4 ()))
      dir
  in
  let code, report = run_session_bundle bundle_path in
  check "single graph contract exits nonzero" (code = 1);
  match report with
  | `Assoc fields ->
    check
      "single graph contract rejected"
      (String.equal (string_json "status" fields) "rejected");
    check
      "single graph contract blocker"
      (String.equal
         (string_json "next_runtime_blocker" fields)
         "graph_execution_contract_requires_resident_session");
    let semantics = assoc_json "runtime_semantics" fields in
    check
      "single graph contract semantics mismatch"
      (String.equal
         (string_json "graph_execution_contract_status" semantics)
         "mismatch");
    check_graph_execution_summary
      semantics
      ~required:1
      ~bound:0
      ~mismatched:1;
    check
      "single graph contract no execution"
      (list_json "transitions" fields = []);
    check_session_hash report
  | _ -> failwith "report must be an object"

let check_graph_execution_contract_requires_opcode_timing () =
  with_temp_dir "octra-inference-session-bundle-test" @@ fun dir ->
  let bundle_path =
    write_session_bundle_fixture
      ~session_abi_root:Abi.v2_root
      ~code:graph_real_code
      ~execution_contract_for_index:(fun index ->
        if index = 0 then
          Some (graph_real_contract ~min_program_instructions:4 ())
        else Some lifecycle_only_contract)
      ~transition_count:2
      dir
  in
  let code, raw = run_session_bundle_raw ~timing_mode:None bundle_path in
  let report = Yojson.Safe.from_string raw in
  check "graph contract without timing exits nonzero" (code = 1);
  match report with
  | `Assoc fields ->
    check
      "graph contract without timing mismatches"
      (String.equal
         (string_json "status" fields)
         "execution_contract_mismatch");
    check
      "graph contract timing blocker"
      (String.equal
         (string_json "next_runtime_blocker" fields)
         "graph_execution_contract_mismatch");
    let semantics = assoc_json "runtime_semantics" fields in
    check
      "graph contract timing semantics mismatch"
      (String.equal
         (string_json "graph_execution_contract_status" semantics)
         "mismatch");
    check_graph_execution_summary
      semantics
      ~required:1
      ~bound:0
      ~mismatched:1;
    (match list_json "transitions" fields with
     | `Assoc first :: _ ->
       let contract = assoc_json "execution_contract" first in
       check
         "graph contract timing status"
         (String.equal (string_json "status" contract) "mismatch");
       check
         "graph contract timing reason"
         (String.equal
            (string_json "reason" contract)
            "opcode_timing_required")
     | _ -> failwith "expected resident transitions");
    check_session_hash report
  | _ -> failwith "report must be an object"

let check_graph_execution_contract_rejects_dead_branch_opcode () =
  with_temp_dir "octra-inference-session-bundle-test" @@ fun dir ->
  let bundle_path =
    write_session_bundle_fixture
      ~session_abi_root:Abi.v2_root
      ~code:dead_branch_graph_real_code
      ~execution_contract_for_index:(fun index ->
        if index = 0 then
          Some (graph_real_contract ~min_program_instructions:4 ())
        else Some lifecycle_only_contract)
      ~transition_count:2
      dir
  in
  let code, report = run_session_bundle bundle_path in
  check "dead graph branch exits nonzero" (code = 1);
  match report with
  | `Assoc fields ->
    check
      "dead graph branch status"
      (String.equal
         (string_json "status" fields)
         "execution_contract_mismatch");
    check
      "dead graph branch blocker"
      (String.equal
         (string_json "next_runtime_blocker" fields)
         "graph_execution_contract_mismatch");
    let semantics = assoc_json "runtime_semantics" fields in
    check
      "dead graph branch graph status"
      (String.equal
         (string_json "graph_execution_contract_status" semantics)
         "mismatch");
    check_graph_execution_summary
      semantics
      ~required:1
      ~bound:0
      ~mismatched:1;
    check_session_hash report;
    (match list_json "transitions" fields with
     | `Assoc first :: _ ->
       check
         "dead graph branch transition mismatch"
         (String.equal
            (string_json "status" first)
            "execution_contract_mismatch");
       let contract = assoc_json "execution_contract" first in
       check
         "dead graph branch static opcode present"
         (List.exists
            (( = ) (`String "SILU_FP"))
            (list_json "inference_opcodes" contract));
       check
         "dead graph branch executed opcode absent"
         (list_json "executed_inference_opcodes" contract = []);
       check
         "dead graph branch reason"
         (String.equal
            (string_json "reason" contract)
            "no_inference_opcode_executed")
     | _ -> failwith "expected resident transition")
  | _ -> failwith "report must be an object"

let check_session_bundle_accepts_graph_real_feedback_loop () =
  with_temp_dir "octra-inference-session-bundle-test" @@ fun dir ->
  let bundle_path =
    write_session_bundle_fixture
      ~decode_steps:(Some 2)
      ~session_abi_root:Abi.committed_state_root
      ~code:graph_real_feedback_code
      ~execution_contract_for_index:(fun _ ->
        Some (graph_real_contract ~min_program_instructions:8 ()))
      ~output_contract_for_index:(fun index ->
        if index > 0 then Some (selected_index_contract ())
        else None)
      ~prior_state_contract_for_index:(fun index ->
        if index = 2 then
          Some
            (selected_index_feedback_contract
               ~offset:feedback_selected_index_offset
               "token-001")
        else None)
      ~transition_count:3
      dir
  in
  let code, report = run_session_bundle bundle_path in
  check "graph feedback bundle exits cleanly" (code = 0);
  match report with
  | `Assoc fields ->
    check
      "graph feedback bundle accepted"
      (String.equal (string_json "status" fields) "accepted");
    check
      "graph feedback blocker cleared"
      (String.equal (string_json "next_runtime_blocker" fields) "none");
    ignore (string_json "opened_session_root" fields);
    ignore (string_json "final_session_root" fields);
    ignore (string_json "final_receipt_root" fields);
    ignore (string_json "output_prefix_root" fields);
    let semantics = assoc_json "runtime_semantics" fields in
    check
      "graph feedback graph contract bound"
      (String.equal
         (string_json "graph_execution_contract_status" semantics)
         "bound");
    check_graph_execution_summary
      semantics
      ~required:3
      ~bound:3
      ~mismatched:0;
    check
      "graph feedback token contract bound"
      (String.equal
         (string_json "decode_token_contract_status" semantics)
         "bound");
    check_decode_token_summary
      semantics
      ~required:2
      ~bound:2
      ~mismatched:0;
    check
      "graph feedback prior contract bound"
      (String.equal
         (string_json "decode_prior_state_contract_status" semantics)
         "bound");
    check_decode_prior_state_summary
      semantics
      ~required:1
      ~bound:1
      ~mismatched:0;
    check_transition_root_chain_summary
      semantics
      ~transition_count:3
      ~complete_transitions:3
      ~advanced_session_roots:3
      ~advance_receipt_roots:3
      ~output_prefix_roots:3
      ~incomplete_transition_ids:[];
    check
      "graph feedback missing capabilities clear"
      (list_json "missing_runtime_capabilities" semantics = []);
    check_session_hash report;
    (match list_json "transitions" fields with
     | [
       `Assoc prefill;
       `Assoc first_decode;
       `Assoc second_decode;
     ] ->
       List.iter
         (fun transition ->
            ignore (string_json "advanced_session_root" transition);
            ignore (string_json "advance_receipt_root" transition);
            ignore (string_json "output_prefix_root" transition);
            let contract = assoc_json "execution_contract" transition in
            check
              "graph feedback contract matched"
              (String.equal
                 (string_json "status" contract)
                 "matched");
            check
              "graph feedback contract graph-real"
              (bool_json "graph_real" contract);
            check
              "graph feedback executed silu"
              (List.exists
                 (( = ) (`String "SILU_FP"))
                 (list_json "executed_inference_opcodes" contract)))
         [prefill; first_decode; second_decode];
       check
         "graph feedback first decode output contract"
         (String.equal
            (string_json
               "status"
               (assoc_json "output_contract" first_decode))
            "matched");
       check
         "graph feedback second decode output contract"
         (String.equal
            (string_json
               "status"
               (assoc_json "output_contract" second_decode))
            "matched");
       check
         "graph feedback second decode prior contract"
         (String.equal
            (string_json
               "status"
               (assoc_json "prior_state_contract" second_decode))
            "matched");
       check
         "graph feedback last payload"
         (String.equal
            (string_json "last_transition_output_payload" fields)
            ("base=10|length=1|values=int:"
             ^ string_of_int feedback_selected_index))
     | _ -> failwith "expected three graph feedback transitions")
  | _ -> failwith "report must be an object"

let check_session_bundle_binds_decode_token_contract () =
  with_temp_dir "octra-inference-session-bundle-test" @@ fun dir ->
  let bundle_path =
    write_session_bundle_fixture
      ~session_abi_root:Abi.v2_root
      ~code:continuation_token_code
      ~output_contract_for_index:(fun index ->
        if index = 1 then Some (selected_index_contract ())
        else None)
      ~transition_count:2
      dir
  in
  let code, report = run_session_bundle bundle_path in
  check "token-contract bundle exits incomplete" (code = 1);
  match report with
  | `Assoc fields ->
    check
      "token-contract bundle incomplete"
      (String.equal (string_json "status" fields) "runtime_incomplete");
    let semantics = assoc_json "runtime_semantics" fields in
    check
      "decode token contract bound"
      (String.equal
         (string_json "decode_token_contract_status" semantics)
         "bound");
    check_decode_token_summary
      semantics
      ~required:1
      ~bound:1
      ~mismatched:0;
    check_decode_prior_state_summary
      semantics
      ~required:0
      ~bound:0
      ~mismatched:0;
    check
      "token contract clears decode blocker"
      (list_json "missing_runtime_capabilities" semantics
       = [`String "committed_target_state_payload_transport"]);
    check
      "token contract next blocker"
      (String.equal
         (string_json "next_runtime_blocker" fields)
         "committed_target_state_payload_transport_not_bound");
    check_session_hash report;
    (match list_json "transitions" fields with
     | [`Assoc first; `Assoc second] ->
       check
         "prefill contract absent"
         (String.equal
            (string_json
               "status"
               (assoc_json "output_contract" first))
            "not_declared");
       let contract = assoc_json "output_contract" second in
       check
         "decode contract matched"
         (String.equal (string_json "status" contract) "matched");
       check
         "decode contract kind"
         (String.equal
            (string_json "kind" contract)
            "selected_index");
       check
         "decode selected index"
         (String.equal (string_json "selected_index" contract) "1");
       check
         "decode payload"
         (String.equal
            (string_json "output_payload" second)
            "base=10|length=1|values=int:1")
     | _ -> failwith "expected two resident transitions")
  | _ -> failwith "report must be an object"

let check_session_bundle_binds_committed_state_transport () =
  with_temp_dir "octra-inference-session-bundle-test" @@ fun dir ->
  let bundle_path =
    write_session_bundle_fixture
      ~session_abi_root:Abi.committed_state_root
      ~code:committed_state_token_code
      ~output_contract_for_index:(fun index ->
        if index = 1 then Some (selected_index_contract ())
        else None)
      ~transition_count:2
      dir
  in
  let code, report = run_session_bundle bundle_path in
  check "committed-state bundle exits cleanly" (code = 0);
  match report with
  | `Assoc fields ->
    check
      "committed-state bundle accepted"
      (String.equal (string_json "status" fields) "accepted");
    check
      "committed-state blocker cleared"
      (String.equal (string_json "next_runtime_blocker" fields) "none");
    let semantics = assoc_json "runtime_semantics" fields in
    check
      "committed-state carry named"
      (String.equal
         (string_json "state_carry" semantics)
         "abi_committed_state_payload");
    check
      "committed-state lifecycle status"
      (String.equal
         (string_json "resident_session_lifecycle_status" semantics)
         "bound");
    check
      "committed-state support reported"
      (bool_json "committed_state_transport_supported" semantics);
    check
      "committed-state binding reported"
      (bool_json "committed_state_transport_bound" semantics);
    check
      "committed-state cache scope"
      (list_json "resident_cache_scope" semantics
       = [
         `String "session";
         `String "committed_target_state_payload";
       ]);
    check
      "committed-state missing capabilities clear"
      (list_json "missing_runtime_capabilities" semantics = []);
    check
      "committed-state decode token contract bound"
      (String.equal
         (string_json "decode_token_contract_status" semantics)
         "bound");
    check_decode_token_summary
      semantics
      ~required:1
      ~bound:1
      ~mismatched:0;
    check_decode_prior_state_summary
      semantics
      ~required:0
      ~bound:0
      ~mismatched:0;
    check_session_hash report;
    (match list_json "transitions" fields with
     | [`Assoc first; `Assoc second] ->
       check
         "first transition does not start bound"
         (not (bool_json "committed_state_transport_bound" first));
       check
         "second transition starts bound"
         (bool_json "committed_state_transport_bound" second);
       check
         "first transition has no prior root"
         (match List.assoc_opt "prior_committed_target_state_root" first with
          | Some `Null -> true
          | _ -> false);
       check
         "second transition prior root"
         (String.equal
            (string_json
               "prior_committed_target_state_root"
               second)
            committed_state_payload_root);
       check
         "second transition prior payload sha"
         (String.equal
            (string_json
               "prior_committed_target_state_payload_sha256"
               second)
            committed_state_payload_root);
       check
         "second transition prior payload bytes"
         (int_json
            "prior_committed_target_state_payload_bytes"
            second
          = String.length committed_state_payload);
       List.iter
         (fun transition ->
            check
              "committed-state transition root"
              (String.equal
                 (string_json
                    "committed_target_state_root"
                    transition)
                 committed_state_payload_root);
            check
              "committed-state transition payload sha"
              (String.equal
                 (string_json
                    "committed_target_state_payload_sha256"
                    transition)
                 committed_state_payload_root);
            check
              "committed-state transition payload bytes"
              (int_json
                 "committed_target_state_payload_bytes"
                 transition
               = String.length committed_state_payload))
         [first; second];
       let contract = assoc_json "output_contract" second in
       check
         "committed-state decode contract matched"
         (String.equal (string_json "status" contract) "matched")
     | _ -> failwith "expected two committed-state transitions")
  | _ -> failwith "report must be an object"

let check_session_bundle_binds_decode_prior_state_contract () =
  with_temp_dir "octra-inference-session-bundle-test" @@ fun dir ->
  let bundle_path =
    write_session_bundle_fixture
      ~decode_steps:(Some 2)
      ~session_abi_root:Abi.committed_state_root
      ~code:committed_state_feedback_code
      ~output_contract_for_index:(fun index ->
        if index > 0 then Some (selected_index_contract ())
        else None)
      ~prior_state_contract_for_index:(fun index ->
        if index = 2 then
          Some
            (selected_index_feedback_contract
               ~offset:feedback_selected_index_offset
               "token-001")
        else None)
      ~transition_count:3
      dir
  in
  let code, report = run_session_bundle bundle_path in
  check "feedback-contract bundle exits cleanly" (code = 0);
  match report with
  | `Assoc fields ->
    check
      "feedback-contract bundle accepted"
      (String.equal (string_json "status" fields) "accepted");
    check
      "feedback blocker cleared"
      (String.equal (string_json "next_runtime_blocker" fields) "none");
    let semantics = assoc_json "runtime_semantics" fields in
    check
      "feedback decode token contract bound"
      (String.equal
         (string_json "decode_token_contract_status" semantics)
         "bound");
    check_decode_token_summary
      semantics
      ~required:2
      ~bound:2
      ~mismatched:0;
    check
      "feedback contract bound"
      (String.equal
         (string_json "decode_prior_state_contract_status" semantics)
         "bound");
    check_decode_prior_state_summary
      semantics
      ~required:1
      ~bound:1
      ~mismatched:0;
    check_transition_root_chain_summary
      semantics
      ~transition_count:3
      ~complete_transitions:3
      ~advanced_session_roots:3
      ~advance_receipt_roots:3
      ~output_prefix_roots:3
      ~incomplete_transition_ids:[];
    check
      "feedback missing capabilities clear"
      (list_json "missing_runtime_capabilities" semantics = []);
    check_session_hash report;
    (match list_json "transitions" fields with
     | [`Assoc prefill; `Assoc first_decode; `Assoc second_decode] ->
       check
         "prefill prior feedback absent"
         (String.equal
            (string_json
               "status"
               (assoc_json "prior_state_contract" prefill))
            "not_declared");
       check
         "first decode feedback absent"
         (String.equal
            (string_json
               "status"
               (assoc_json "prior_state_contract" first_decode))
            "not_declared");
       let feedback =
         assoc_json "prior_state_contract" second_decode
       in
       check
         "second decode feedback matched"
         (String.equal (string_json "status" feedback) "matched");
       check
         "second decode feedback kind"
         (String.equal
            (string_json "kind" feedback)
            "previous_selected_index_u64le");
       check
         "second decode feedback source"
         (String.equal
            (string_json "source_transition_id" feedback)
            "token-001");
       check
         "second decode feedback index"
         (String.equal
            (string_json "selected_index" feedback)
            (string_of_int feedback_selected_index));
       check
         "second decode prior state root"
         (String.equal
            (string_json
               "prior_committed_target_state_root"
               second_decode)
            feedback_selected_index_payload_root);
       check
         "last transition payload"
         (String.equal
            (string_json "last_transition_output_payload" fields)
            ("base=10|length=1|values=int:"
             ^ string_of_int feedback_selected_index))
     | _ -> failwith "expected three feedback transitions")
  | _ -> failwith "report must be an object"

let check_session_bundle_rejects_decode_prior_state_contract_mismatch () =
  with_temp_dir "octra-inference-session-bundle-test" @@ fun dir ->
  let bundle_path =
    write_session_bundle_fixture
      ~decode_steps:(Some 2)
      ~session_abi_root:Abi.committed_state_root
      ~code:committed_state_feedback_code
      ~output_contract_for_index:(fun index ->
        if index > 0 then Some (selected_index_contract ())
        else None)
      ~prior_state_contract_for_index:(fun index ->
        if index = 2 then
          Some
            (selected_index_feedback_contract
               ~offset:(feedback_selected_index_offset + 1)
               "token-001")
        else None)
      ~transition_count:3
      dir
  in
  let code, report = run_session_bundle bundle_path in
  check "feedback-contract mismatch exits nonzero" (code = 1);
  match report with
  | `Assoc fields ->
    check
      "feedback-contract mismatch status"
      (String.equal
         (string_json "status" fields)
         "prior_state_contract_mismatch");
    check
      "feedback-contract mismatch blocker"
      (String.equal
         (string_json "next_runtime_blocker" fields)
         "decode_loop_prior_state_contract_mismatch");
    let semantics = assoc_json "runtime_semantics" fields in
    check
      "feedback contract mismatch status"
      (String.equal
         (string_json "decode_prior_state_contract_status" semantics)
         "mismatch");
    check_decode_token_summary
      semantics
      ~required:2
      ~bound:1
      ~mismatched:0;
    check_decode_prior_state_summary
      semantics
      ~required:1
      ~bound:0
      ~mismatched:1;
    check_transition_root_chain_summary
      semantics
      ~transition_count:3
      ~complete_transitions:2
      ~advanced_session_roots:2
      ~advance_receipt_roots:2
      ~output_prefix_roots:2
      ~incomplete_transition_ids:["token-002"];
    check_session_hash report;
    (match list_json "transitions" fields with
     | [_; _; `Assoc second_decode] ->
       check
         "second decode transition mismatch"
         (String.equal
            (string_json "status" second_decode)
            "prior_state_contract_mismatch");
       let feedback =
         assoc_json "prior_state_contract" second_decode
       in
       check
         "second decode feedback mismatch"
         (String.equal (string_json "status" feedback) "mismatch");
       check
         "second decode feedback mismatch reason"
         (String.equal
            (string_json "reason" feedback)
            "prior_committed_target_state_payload_too_short")
     | _ -> failwith "expected three feedback transitions")
  | _ -> failwith "report must be an object"

let check_session_bundle_rejects_nonadjacent_prior_state_contract () =
  with_temp_dir "octra-inference-session-bundle-test" @@ fun dir ->
  let bundle_path =
    write_session_bundle_fixture
      ~decode_steps:(Some 2)
      ~session_abi_root:Abi.committed_state_root
      ~code:committed_state_feedback_code
      ~output_contract_for_index:(fun index ->
        if index > 0 then Some (selected_index_contract ())
        else None)
      ~prior_state_contract_for_index:(fun index ->
        if index = 2 then
          Some
            (selected_index_feedback_contract
               ~offset:feedback_selected_index_offset
               "token-000")
        else None)
      ~transition_count:3
      dir
  in
  let code, report = run_session_bundle bundle_path in
  check "nonadjacent prior-state exits nonzero" (code = 1);
  match report with
  | `Assoc fields ->
    check
      "nonadjacent prior-state status"
      (String.equal
         (string_json "status" fields)
         "prior_state_contract_mismatch");
    check
      "nonadjacent prior-state blocker"
      (String.equal
         (string_json "next_runtime_blocker" fields)
         "decode_loop_prior_state_contract_mismatch");
    check_session_hash report;
    (match list_json "transitions" fields with
     | [_; _; `Assoc second_decode] ->
       let contract = assoc_json "prior_state_contract" second_decode in
       check
         "nonadjacent prior-state mismatch reason"
         (String.equal
            (string_json "reason" contract)
            "source_transition_not_previous_decode");
       check
         "nonadjacent prior-state previous decode"
         (String.equal
            (string_json "previous_decode_transition_id" contract)
            "token-001")
     | _ -> failwith "expected three prior-state transitions")
  | _ -> failwith "report must be an object"

let check_session_bundle_rejects_multiple_prefills () =
  with_temp_dir "octra-inference-session-bundle-test" @@ fun dir ->
  let bundle_path =
    write_session_bundle_fixture
      ~decode_steps:(Some 0)
      ~session_abi_root:Abi.v2_root
      ~code:continuation_token_code
      ~phase_for_index:(fun _ -> "prefill")
      ~transition_count:2
      dir
  in
  let code, report = run_session_bundle bundle_path in
  check "multiple-prefill bundle exits nonzero" (code = 1);
  match report with
  | `Assoc fields ->
    check_session_hash report;
    check
      "multiple-prefill top-level blocker"
      (String.equal
         (string_json "next_runtime_blocker" fields)
         "prefill_decode_phase_sequence_mismatch");
    let semantics = assoc_json "runtime_semantics" fields in
    check
      "multiple-prefill readiness"
      (String.equal
         (string_json "runtime_readiness_status" semantics)
         "declaration_rejected");
    let preflight = assoc_json "continuation_preflight" fields in
    check
      "multiple-prefill phase sequence invalid"
      (not (bool_json "prefill_decode_phase_sequence_valid" preflight));
    check
      "multiple-prefill declaration blocker"
      (list_json "declaration_blockers" preflight
       = [`String "prefill_decode_phase_sequence_mismatch"])
  | _ -> failwith "report must be an object"

let check_session_bundle_rejects_decode_token_contract_mismatch () =
  with_temp_dir "octra-inference-session-bundle-test" @@ fun dir ->
  let bundle_path =
    write_session_bundle_fixture
      ~session_abi_root:Abi.v2_root
      ~code:continuation_token_code
      ~output_contract_for_index:(fun index ->
        if index = 1 then
          Some (selected_index_contract ~output_base:11 ())
        else None)
      ~transition_count:2
      dir
  in
  let code, report = run_session_bundle bundle_path in
  check "token-contract mismatch exits nonzero" (code = 1);
  match report with
  | `Assoc fields ->
    check
      "token-contract mismatch status"
      (String.equal
         (string_json "status" fields)
         "output_contract_mismatch");
    check
      "token-contract mismatch blocker"
      (String.equal
         (string_json "next_runtime_blocker" fields)
         "decode_loop_token_contract_mismatch");
    let semantics = assoc_json "runtime_semantics" fields in
    check
      "decode token contract mismatch status"
      (String.equal
         (string_json "decode_token_contract_status" semantics)
         "mismatch");
    check_decode_token_summary
      semantics
      ~required:1
      ~bound:0
      ~mismatched:1;
    check_decode_prior_state_summary
      semantics
      ~required:0
      ~bound:0
      ~mismatched:0;
    check_session_hash report;
    (match list_json "transitions" fields with
     | [_; `Assoc second] ->
       check
         "decode transition contract mismatch status"
         (String.equal
            (string_json "status" second)
            "output_contract_mismatch");
       let contract = assoc_json "output_contract" second in
       check
         "decode contract mismatch"
         (String.equal (string_json "status" contract) "mismatch");
       check
         "decode contract mismatch reason"
         (String.equal
            (string_json "reason" contract)
            "output_base_mismatch")
     | _ -> failwith "expected two resident transitions")
  | _ -> failwith "report must be an object"

let check_session_bundle_rejects_invalid_phase_order () =
  with_temp_dir "octra-inference-session-bundle-test" @@ fun dir ->
  let bundle_path =
    write_session_bundle_fixture
      ~session_abi_root:Abi.v2_root
      ~code:continuation_code
      ~phase_for_index:(fun index ->
        if index = 0 then "decode" else "prefill")
      ~transition_count:2
      dir
  in
  let code, report = run_session_bundle bundle_path in
  check "phase-order bundle exits nonzero" (code = 1);
  match report with
  | `Assoc fields ->
    check_session_hash report;
    check
      "phase-order top-level blocker"
      (String.equal
         (string_json "next_runtime_blocker" fields)
         "prefill_decode_phase_sequence_mismatch");
    let semantics = assoc_json "runtime_semantics" fields in
    check
      "phase-order readiness"
      (String.equal
         (string_json "runtime_readiness_status" semantics)
         "declaration_rejected");
    let preflight = assoc_json "continuation_preflight" fields in
    check
      "phase-order preflight readiness"
      (String.equal
         (string_json "runtime_readiness_status" preflight)
         "declaration_rejected");
    check
      "phase sequence invalid"
      (not (bool_json "prefill_decode_phase_sequence_valid" preflight));
    check
      "decode steps still match"
      (bool_json "decode_steps_match" preflight);
    check
      "phase-order declaration blocker"
      (list_json "declaration_blockers" preflight
       = [`String "prefill_decode_phase_sequence_mismatch"]);
    check
      "phase-order blocker list"
      (list_json "blockers" preflight
       = [`String "prefill_decode_phase_sequence_mismatch"])
  | _ -> failwith "report must be an object"

let check_session_bundle_reports_top_level_claim_mismatch () =
  with_temp_dir "octra-inference-session-bundle-test" @@ fun dir ->
  let bundle_path =
    write_session_bundle_fixture
      ~request_root:(hex_root '9')
      ~transition_count:2
      dir
  in
  let code, report = run_session_bundle bundle_path in
  check "claim mismatch bundle exits nonzero" (code = 1);
  match report with
  | `Assoc fields ->
    check_session_hash report;
    let preflight = assoc_json "continuation_preflight" fields in
    let claims = assoc_json "top_level_claims" preflight in
    check
      "request root mismatch"
      (String.equal
         (string_json "request_root_status" claims)
         "mismatch")
  | _ -> failwith "report must be an object"

let check_session_bundle_reports_identity_mismatch () =
  with_temp_dir "octra-inference-session-bundle-test" @@ fun dir ->
  let bundle_path =
    write_session_bundle_fixture
      ~second_request_nonce:(hex_root '6')
      ~transition_count:2
      dir
  in
  let code, report = run_session_bundle bundle_path in
  check "identity mismatch bundle exits nonzero" (code = 1);
  match report with
  | `Assoc fields ->
    check_session_hash report;
    check
      "identity mismatch top-level blocker"
      (String.equal
         (string_json "next_runtime_blocker" fields)
         "request_root_mismatch");
    let semantics = assoc_json "runtime_semantics" fields in
    check
      "identity mismatch readiness"
      (String.equal
         (string_json "runtime_readiness_status" semantics)
         "identity_rejected");
    check
      "identity mismatch runtime blocker"
      (String.equal
         (string_json "next_runtime_blocker" semantics)
         "request_root_mismatch");
    let preflight = assoc_json "continuation_preflight" fields in
    check
      "identity mismatch preflight readiness"
      (String.equal
         (string_json "runtime_readiness_status" preflight)
         "identity_rejected");
    check
      "identity mismatch preflight blocker"
      (String.equal
         (string_json "next_runtime_blocker" preflight)
         "request_root_mismatch");
    let identities = assoc_json "identity_checks" preflight in
    check
      "request root not uniform"
      (not (bool_json "request_root_uniform" identities));
    check
      "other roots still uniform"
      (bool_json "target_root_uniform" identities
       && bool_json "model_ranges_root_uniform" identities
       && bool_json "model_deployment_root_uniform" identities
       && bool_json "session_abi_root_uniform" identities);
    let blockers = list_json "identity_blockers" preflight in
    check
      "request identity blocker"
      (blockers = [`String "request_root_mismatch"]);
    check
      "no declaration blocker for identity mismatch"
      (list_json "declaration_blockers" preflight = [])
  | _ -> failwith "report must be an object"

let check_session_bundle_reports_declaration_mismatch () =
  with_temp_dir "octra-inference-session-bundle-test" @@ fun dir ->
  let bundle_path =
    write_session_bundle_fixture
      ~decode_steps:(Some 2)
      ~transition_count:2
      dir
  in
  let code, report = run_session_bundle bundle_path in
  check "declaration mismatch bundle exits nonzero" (code = 1);
  match report with
  | `Assoc fields ->
    check_session_hash report;
    check
      "declaration mismatch top-level blocker"
      (String.equal
         (string_json "next_runtime_blocker" fields)
         "decode_steps_mismatch");
    let semantics = assoc_json "runtime_semantics" fields in
    check
      "declaration mismatch readiness"
      (String.equal
         (string_json "runtime_readiness_status" semantics)
         "declaration_rejected");
    let preflight = assoc_json "continuation_preflight" fields in
    check
      "declaration mismatch preflight readiness"
      (String.equal
         (string_json "runtime_readiness_status" preflight)
         "declaration_rejected");
    check
      "declaration mismatch blocker"
      (list_json "declaration_blockers" preflight
       = [`String "decode_steps_mismatch"]);
    check
      "declaration mismatch has no identity blockers"
      (list_json "identity_blockers" preflight = [])
  | _ -> failwith "report must be an object"

let check_session_bundle_preflight_checks_input_payload () =
  with_temp_dir "octra-inference-session-bundle-test" @@ fun dir ->
  let bundle_path = write_session_bundle_fixture ~transition_count:2 dir in
  write_file (Filename.concat dir "stage/request-input.json") "corrupt";
  let code, raw = run_session_bundle_raw bundle_path in
  check "corrupt input bundle exits nonzero" (code = 1);
  check "corrupt input emits no checked preflight report" (raw = "")

let check_session_bundle_requires_schema_and_decode_steps () =
  with_temp_dir "octra-inference-session-bundle-test" @@ fun dir ->
  let missing_schema =
    write_session_bundle_fixture ~schema:None dir
  in
  let missing_schema_code, _ = run_session_bundle_raw missing_schema in
  check "missing schema rejected" (missing_schema_code = 1);
  remove_tree dir;
  Unix.mkdir dir 0o700;
  let wrong_schema =
    write_session_bundle_fixture ~schema:(Some "octra.inference.bad") dir
  in
  let wrong_schema_code, _ = run_session_bundle_raw wrong_schema in
  check "wrong schema rejected" (wrong_schema_code = 1);
  remove_tree dir;
  Unix.mkdir dir 0o700;
  let missing_decode =
    write_session_bundle_fixture ~decode_steps:None dir
  in
  let missing_decode_code, _ = run_session_bundle_raw missing_decode in
  check "missing decode steps rejected" (missing_decode_code = 1);
  remove_tree dir;
  Unix.mkdir dir 0o700;
  let lifecycle_with_graph_field =
    write_session_bundle_fixture
      ~execution_contract_for_index:(fun _ ->
        Some
          (`Assoc [
            "kind", `String "lifecycle_only";
            "min_program_instructions", `Int 1;
          ]))
      dir
  in
  let lifecycle_with_graph_field_code, _ =
    run_session_bundle_raw lifecycle_with_graph_field
  in
  check
    "lifecycle contract rejects graph field"
    (lifecycle_with_graph_field_code = 1)

let () =
  check_batch_runtime_semantics ();
  check_session_bundle_single_transition ();
  check_session_bundle_hash_binds_decode_steps ();
  check_session_bundle_rejects_multi_transition ();
  check_session_bundle_accepts_v2_multi_transition ();
  check_session_bundle_binds_graph_execution_contract ();
  check_session_bundle_rejects_graph_execution_overclaim ();
  check_single_transition_rejects_graph_execution_contract ();
  check_graph_execution_contract_requires_opcode_timing ();
  check_graph_execution_contract_rejects_dead_branch_opcode ();
  check_session_bundle_accepts_graph_real_feedback_loop ();
  check_session_bundle_binds_decode_token_contract ();
  check_session_bundle_binds_committed_state_transport ();
  check_session_bundle_binds_decode_prior_state_contract ();
  check_session_bundle_rejects_decode_prior_state_contract_mismatch ();
  check_session_bundle_rejects_nonadjacent_prior_state_contract ();
  check_session_bundle_rejects_multiple_prefills ();
  check_session_bundle_rejects_decode_token_contract_mismatch ();
  check_session_bundle_rejects_invalid_phase_order ();
  check_session_bundle_reports_top_level_claim_mismatch ();
  check_session_bundle_reports_identity_mismatch ();
  check_session_bundle_reports_declaration_mismatch ();
  check_session_bundle_preflight_checks_input_payload ();
  check_session_bundle_requires_schema_and_decode_steps ()
