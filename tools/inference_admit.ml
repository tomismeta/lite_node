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


module Admission = Octra_vm.Admission
module Contract_vm = Octra_vm.Contract_vm
module Program_envelope = Octra_vm.Program_envelope
module Program_type_flow = Octra_vm.Program_type_flow
module Model = Octra_vm.Inference_model
module Bytecode = Octra_vm.Bytecode
module Inference_opcode_policy = Octra_vm.Inference_opcode_policy
module Opcode_policy = Octra_vm.Opcode_policy
module Plan = Octra_vm.Inference_plan
module Receipt = Octra_vm.Inference_receipt
module Req = Octra_vm.Execution_requirement
module Deployment = Octra_vm.Inference_model_deployment
module Inference_execution = Octra_vm.Inference_execution
module Request = Octra_vm.Inference_request
module Session = Octra_vm.Inference_session
module Store = Octra_vm.Inference_store
module Target = Octra_vm.Inference_target

type timing_mode =
  | Timing_none
  | Timing_stage
  | Timing_opcode

type timer = {
  mode : timing_mode;
  mutable last : float;
  mutable phases : (string * int) list;
}

type paths = {
  mutable program : string option;
  mutable requirement : string option;
  mutable target : string option;
  mutable support : string option;
  mutable request : string option;
  mutable input : string option;
  mutable model_ranges : string option;
  mutable model_deployment : string option;
  mutable range_sources : (string * string) list;
  mutable run_session : bool;
  mutable scan_policy : bool;
  mutable run_batch : string option;
  mutable timing_mode : timing_mode;
}

type target_packet = {
  target : Target.t;
  declared_root : string option;
}

type requirement_packet = {
  requirement : Req.t;
  declared_root : string option;
}

type request_packet = {
  request : Request.t;
  declared_root : string option;
}

type model_packet = {
  model : Model.t;
  declared_root : string option;
}

type deployment_packet = {
  deployment : Deployment.t;
  declared_root : string option;
}

type batch_stage = {
  stage_id : string;
  program_path : string;
  requirement_path : string;
  target_path : string;
  support_path : string;
  request_path : string;
  input_path : string;
  model_ranges_path : string;
  model_deployment_path : string option;
  range_sources : (string * string) list;
  expected_output_root : string option;
}

type batch = {
  batch_path : string;
  stages : batch_stage list;
}

type batch_cache = {
  owner_bytes : (string, string) Hashtbl.t;
  pins_by_ranges_root_and_limit : (string, Store.pin_set) Hashtbl.t;
}

type batch_stage_result = {
  stage_json : Yojson.Safe.t;
  stage_output_root : string;
  stage_reference_mismatch : bool;
  stage_unsupported_opcodes : string list;
  stage_missing_capabilities : string list;
  stage_policy_violations : Yojson.Safe.t list;
}

let max_batch_stages = 256

let max_batch_owner_cache_entries = 4096

let max_batch_pin_cache_entries = 512

let fail message =
  prerr_endline message;
  exit 1

let start_timer mode =
  { mode; last = Unix.gettimeofday (); phases = [] }

let timing_mark timer phase =
  match timer.mode with
  | Timing_none -> ()
  | Timing_stage
  | Timing_opcode ->
    let now = Unix.gettimeofday () in
    let elapsed_ms =
      int_of_float ((now -. timer.last) *. 1000.0)
    in
    timer.phases <- (phase, elapsed_ms) :: timer.phases;
    timer.last <- now

let decimal_seconds ms =
  Printf.sprintf "%.3f" (float_of_int ms /. 1000.0)

let timing_json timer =
  let phases = List.rev timer.phases in
  let total =
    List.fold_left (fun acc (_, ms) -> acc + ms) 0 phases
  in
  let mode =
    match timer.mode with
    | Timing_none -> "none"
    | Timing_stage -> "stage"
    | Timing_opcode -> "opcode"
  in
  `Assoc [
    "mode", `String mode;
    "phases",
    `List
      (List.map
         (fun (phase, milliseconds) ->
           `Assoc [
             "phase", `String phase;
             "milliseconds", `Int milliseconds;
             "seconds_decimal", `String (decimal_seconds milliseconds);
           ])
         phases);
    "total_milliseconds", `Int total;
    "total_seconds_decimal", `String (decimal_seconds total);
  ]

let timing_fields timer =
  match timer.mode with
  | Timing_none -> []
  | Timing_stage
  | Timing_opcode -> ["timing", timing_json timer]

let diagnostic_profile =
  {
    Inference_execution.clock = Unix.gettimeofday;
    opcode_name = Opcode_policy.opcode_name;
  }

let decimal_milliseconds microseconds =
  Printf.sprintf "%.3f" (float_of_int microseconds /. 1000.0)

let opcode_profile_json profile =
  `List
    (List.map
       (fun (entry : Contract_vm.opcode_profile) ->
         `Assoc [
           "opcode", `String entry.opcode;
           "count", `Int entry.count;
           "effort_delta", `Int entry.effort_used;
           "microseconds", `Int entry.microseconds;
           "milliseconds_decimal",
           `String (decimal_milliseconds entry.microseconds);
         ])
       profile)

let execution_profile_json profile =
  `List
    (List.map
       (fun (entry : Inference_execution.execution_profile) ->
         `Assoc [
           "phase", `String entry.phase;
           "microseconds", `Int entry.microseconds;
           "milliseconds_decimal",
           `String (decimal_milliseconds entry.microseconds);
         ])
       profile)

let opcode_profile_fields = function
  | Timing_opcode, execution_profile, opcode_profile ->
    [
      "execution_timing", execution_profile_json execution_profile;
      "opcode_timing", opcode_profile_json opcode_profile;
    ]
  | Timing_none, _, _
  | Timing_stage, _, _ -> []

let read_file path =
  let input = open_in_bin path in
  let len = in_channel_length input in
  let data = really_input_string input len in
  close_in input;
  data

let require_path name = function
  | Some path -> path
  | None -> fail ("missing " ^ name)

let split_pair value message =
  match String.index_opt value '=' with
  | None -> fail message
  | Some index ->
    let left = String.sub value 0 index in
    let right =
      String.sub value (index + 1) (String.length value - index - 1)
    in
    if left = "" || right = "" then fail message else left, right

let field name fields =
  match List.filter (fun (key, _) -> String.equal key name) fields with
  | [(_, value)] -> Ok value
  | [] -> Error ("missing field: " ^ name)
  | _ -> Error ("duplicate field: " ^ name)

let optional_field name fields =
  match List.filter (fun (key, _) -> String.equal key name) fields with
  | [] -> Ok None
  | [(_, value)] -> Ok (Some value)
  | _ -> Error ("duplicate field: " ^ name)

let check_known fields names =
  match
    List.find_opt
      (fun (name, _) -> not (List.exists (String.equal name) names))
      fields
  with
  | None -> Ok ()
  | Some (name, _) -> Error ("unknown field: " ^ name)

let string_field name fields =
  match field name fields with
  | Ok (`String value) -> Ok value
  | Ok _ -> Error ("field must be a string: " ^ name)
  | Error error -> Error error

let optional_string_field name fields =
  match optional_field name fields with
  | Ok None -> Ok None
  | Ok (Some (`String value)) -> Ok (Some value)
  | Ok (Some _) -> Error ("field must be a string: " ^ name)
  | Error error -> Error error

let optional_nullable_string_field name fields =
  match optional_field name fields with
  | Ok None -> Ok None
  | Ok (Some `Null) -> Ok None
  | Ok (Some (`String value)) -> Ok (Some value)
  | Ok (Some _) -> Error ("field must be a string or null: " ^ name)
  | Error error -> Error error

let int_field name fields =
  match field name fields with
  | Ok (`Int value) -> Ok value
  | Ok _ -> Error ("field must be an int: " ^ name)
  | Error error -> Error error

let assoc_field name fields =
  match field name fields with
  | Ok (`Assoc values) -> Ok values
  | Ok _ -> Error ("field must be an object: " ^ name)
  | Error error -> Error error

let list_field name fields =
  match field name fields with
  | Ok (`List values) -> Ok values
  | Ok _ -> Error ("field must be a list: " ^ name)
  | Error error -> Error error

let parse_capability = function
  | `Assoc fields ->
    (match check_known fields ["name"; "root"] with
     | Error error -> Error error
     | Ok () ->
       (match string_field "name" fields, string_field "root" fields with
        | Ok name, Ok capability_root ->
          Ok Req.{ name; root = capability_root }
        | Error error, _
        | _, Error error -> Error error))
  | _ -> Error "capability must be an object"

let rec parse_capabilities acc = function
  | [] -> Ok (List.rev acc)
  | value :: rest ->
    (match parse_capability value with
     | Error error -> Error error
     | Ok capability -> parse_capabilities (capability :: acc) rest)

let parse_limits fields =
  match
    check_known
      fields
      [
        "max_model_bytes";
        "max_view_bytes";
        "max_session_bytes";
        "max_scratch_bytes";
        "max_output_bytes";
        "max_advance_effort";
      ]
  with
  | Error error -> Error error
  | Ok () ->
    (match
       int_field "max_model_bytes" fields,
       int_field "max_view_bytes" fields,
       int_field "max_session_bytes" fields,
       int_field "max_scratch_bytes" fields,
       int_field "max_output_bytes" fields,
       int_field "max_advance_effort" fields
     with
     | Ok max_model_bytes,
       Ok max_view_bytes,
       Ok max_session_bytes,
       Ok max_scratch_bytes,
       Ok max_output_bytes,
       Ok max_advance_effort ->
       Ok Req.{
         max_model_bytes;
         max_view_bytes;
         max_session_bytes;
         max_scratch_bytes;
         max_output_bytes;
         max_advance_effort;
       }
     | Error error, _, _, _, _, _
     | _, Error error, _, _, _, _
     | _, _, Error error, _, _, _
     | _, _, _, Error error, _, _
     | _, _, _, _, Error error, _
     | _, _, _, _, _, Error error -> Error error)

let parse_requirement_json json =
  match json with
  | `Assoc fields ->
    (match
       check_known fields [
         "vm_semantics_root";
         "numerical_root";
         "effort_root";
         "capabilities";
         "limits";
         "requirement_root";
       ]
     with
     | Error error -> Error error
     | Ok () ->
       (match
          string_field "vm_semantics_root" fields,
          string_field "numerical_root" fields,
          string_field "effort_root" fields,
          list_field "capabilities" fields,
          assoc_field "limits" fields,
          optional_string_field "requirement_root" fields
        with
        | Ok vm_semantics_root,
          Ok numerical_root,
          Ok effort_root,
          Ok capabilities,
          Ok limits,
          Ok declared_root ->
          (match parse_capabilities [] capabilities, parse_limits limits with
           | Ok capabilities, Ok limits ->
             Ok {
               requirement = Req.{
                 vm_semantics_root;
                 numerical_root;
                 effort_root;
                 capabilities;
                 limits;
               };
               declared_root;
             }
           | Error error, _
           | _, Error error -> Error error)
        | Error error, _, _, _, _, _
        | _, Error error, _, _, _, _
        | _, _, Error error, _, _, _
        | _, _, _, Error error, _, _
        | _, _, _, _, Error error, _
        | _, _, _, _, _, Error error -> Error error))
  | _ -> Error "requirement must be an object"

let string_list_field name fields =
  match list_field name fields with
  | Error error -> Error error
  | Ok values ->
    let rec loop acc = function
      | [] -> Ok (List.rev acc)
      | `String value :: rest -> loop (value :: acc) rest
      | _ :: _ -> Error ("field must be a string list: " ^ name)
    in
    loop [] values

let parse_support_json json =
  match json with
  | `Assoc fields ->
    (match
       check_known fields [
         "vm_semantics_root";
         "numerical_roots";
         "effort_roots";
         "capabilities";
         "limits";
       ]
     with
     | Error error -> Error error
     | Ok () ->
       (match
          string_field "vm_semantics_root" fields,
          string_list_field "numerical_roots" fields,
          string_list_field "effort_roots" fields,
          list_field "capabilities" fields,
          assoc_field "limits" fields
        with
        | Ok support_vm_semantics_root,
          Ok support_numerical_roots,
          Ok support_effort_roots,
          Ok capabilities,
          Ok limits ->
          (match parse_capabilities [] capabilities, parse_limits limits with
           | Ok support_capabilities, Ok support_limits ->
             Ok Req.{
               support_vm_semantics_root;
               support_numerical_roots;
               support_effort_roots;
               support_capabilities;
               support_limits;
             }
           | Error error, _
           | _, Error error -> Error error)
        | Error error, _, _, _, _
        | _, Error error, _, _, _
        | _, _, Error error, _, _
        | _, _, _, Error error, _
        | _, _, _, _, Error error -> Error error))
  | _ -> Error "support must be an object"

let parse_entrypoint = function
  | `Assoc fields ->
    (match check_known fields ["name"; "label"] with
     | Error error -> Error error
     | Ok () ->
       (match string_field "name" fields, int_field "label" fields with
        | Ok entry_name, Ok label ->
          Ok Target.{ entry_name; entry_label = label }
        | Error error, _
        | _, Error error -> Error error))
  | _ -> Error "entrypoint must be an object"

let rec parse_entrypoints acc = function
  | [] -> Ok (List.rev acc)
  | value :: rest ->
    (match parse_entrypoint value with
     | Error error -> Error error
     | Ok entrypoint -> parse_entrypoints (entrypoint :: acc) rest)

let parse_target_json json =
  match json with
  | `Assoc fields ->
    (match
       check_known fields [
         "program_root";
         "requirement_root";
         "model_root";
         "execution_descriptor_root";
         "store_root";
         "session_abi_root";
         "entrypoints";
         "target_root";
       ]
     with
     | Error error -> Error error
     | Ok () ->
       (match
          string_field "program_root" fields,
          string_field "requirement_root" fields,
          string_field "model_root" fields,
          string_field "execution_descriptor_root" fields,
          string_field "store_root" fields,
          string_field "session_abi_root" fields,
          list_field "entrypoints" fields,
          optional_string_field "target_root" fields
        with
        | Ok program_root_value,
          Ok requirement_root_value,
          Ok model_root_value,
          Ok execution_descriptor_root_value,
          Ok store_root_value,
          Ok session_abi_root_value,
          Ok entrypoints,
          Ok declared_root ->
          (match parse_entrypoints [] entrypoints with
           | Error error -> Error error
           | Ok entrypoints ->
             Ok {
               target = Target.{
                 program_root = program_root_value;
                 requirement_root = requirement_root_value;
                 model_root = model_root_value;
                 execution_descriptor_root = execution_descriptor_root_value;
                 store_root = store_root_value;
                 session_abi_root = session_abi_root_value;
                 entrypoints;
               };
               declared_root;
             })
        | Error error, _, _, _, _, _, _, _
        | _, Error error, _, _, _, _, _, _
        | _, _, Error error, _, _, _, _, _
        | _, _, _, Error error, _, _, _, _
        | _, _, _, _, Error error, _, _, _
        | _, _, _, _, _, Error error, _, _
        | _, _, _, _, _, _, Error error, _
        | _, _, _, _, _, _, _, Error error -> Error error))
  | _ -> Error "target must be an object"

let parse_range = function
  | `Assoc fields ->
    (match
       check_known fields [
         "owner_root";
         "offset";
         "length";
         "encoding";
         "shape_root";
       ]
     with
     | Error error -> Error error
     | Ok () ->
       (match
          string_field "owner_root" fields,
          int_field "offset" fields,
          int_field "length" fields,
          string_field "encoding" fields,
          optional_nullable_string_field "shape_root" fields
        with
        | Ok owner_root,
          Ok offset,
          Ok length,
          Ok encoding,
          Ok shape_root ->
          Ok Model.{ owner_root; offset; length; encoding; shape_root }
        | Error error, _, _, _, _
        | _, Error error, _, _, _
        | _, _, Error error, _, _
        | _, _, _, Error error, _
        | _, _, _, _, Error error -> Error error))
  | _ -> Error "range must be an object"

let rec parse_ranges acc = function
  | [] -> Ok (List.rev acc)
  | value :: rest ->
    (match parse_range value with
     | Error error -> Error error
     | Ok range -> parse_ranges (range :: acc) rest)

let parse_model_ranges_json json =
  match json with
  | `Assoc fields ->
    (match
       check_known fields [
         "model_root";
         "store_root";
         "ranges";
         "model_ranges_root";
       ]
     with
     | Error error -> Error error
     | Ok () ->
       (match
          string_field "model_root" fields,
          string_field "store_root" fields,
          list_field "ranges" fields,
          optional_string_field "model_ranges_root" fields
        with
        | Ok model_root, Ok store_root, Ok ranges, Ok declared_root ->
          (match parse_ranges [] ranges with
           | Error error -> Error error
           | Ok ranges ->
             Ok {
               model = Model.{ model_root; store_root; ranges };
               declared_root;
             })
        | Error error, _, _, _
        | _, Error error, _, _
        | _, _, Error error, _
        | _, _, _, Error error -> Error error))
  | _ -> Error "model ranges must be an object"

let parse_model_deployment_json json =
  match json with
  | `Assoc fields ->
    (match
       check_known fields [
         "model_root";
         "store_root";
         "tensor_index_root";
         "tokenizer_root";
         "numerical_profile_root";
         "capability_set_root";
         "default_program_root";
         "model_deployment_root";
       ]
     with
     | Error error -> Error error
     | Ok () ->
       (match
          string_field "model_root" fields,
          string_field "store_root" fields,
          string_field "tensor_index_root" fields,
          optional_nullable_string_field "tokenizer_root" fields,
          string_field "numerical_profile_root" fields,
          string_field "capability_set_root" fields,
          optional_nullable_string_field "default_program_root" fields,
          optional_string_field "model_deployment_root" fields
        with
        | Ok model_root,
          Ok store_root,
          Ok tensor_index_root,
          Ok tokenizer_root,
          Ok numerical_profile_root,
          Ok capability_set_root_value,
          Ok default_program_root,
          Ok declared_root ->
          Ok {
            deployment = Deployment.{
              model_root;
              store_root;
              tensor_index_root;
              tokenizer_root;
              numerical_profile_root;
              capability_set_root = capability_set_root_value;
              default_program_root;
            };
            declared_root;
          }
        | Error error, _, _, _, _, _, _, _
        | _, Error error, _, _, _, _, _, _
        | _, _, Error error, _, _, _, _, _
        | _, _, _, Error error, _, _, _, _
        | _, _, _, _, Error error, _, _, _
        | _, _, _, _, _, Error error, _, _
        | _, _, _, _, _, _, Error error, _
        | _, _, _, _, _, _, _, Error error -> Error error))
  | _ -> Error "model deployment must be an object"

let parse_request_json json =
  match json with
  | `Assoc fields ->
    (match
       check_known fields [
         "schema";
         "target_root";
         "entrypoint";
         "input_root";
         "request_nonce";
         "max_output_bytes";
         "max_advance_effort";
         "request_root";
       ]
     with
     | Error error -> Error error
     | Ok () ->
       (match
          int_field "schema" fields,
          string_field "target_root" fields,
          string_field "entrypoint" fields,
          string_field "input_root" fields,
          string_field "request_nonce" fields,
          int_field "max_output_bytes" fields,
          int_field "max_advance_effort" fields,
          optional_string_field "request_root" fields
        with
        | Ok schema,
          Ok target_root,
          Ok entrypoint,
          Ok input_root,
          Ok request_nonce,
          Ok max_output_bytes,
          Ok max_advance_effort,
          Ok declared_root ->
          Ok {
            request = Request.{
              schema;
              target_root;
              entrypoint;
              input_root;
              request_nonce;
              max_output_bytes;
              max_advance_effort;
            };
            declared_root;
          }
        | Error error, _, _, _, _, _, _, _
        | _, Error error, _, _, _, _, _, _
        | _, _, Error error, _, _, _, _, _
        | _, _, _, Error error, _, _, _, _
        | _, _, _, _, Error error, _, _, _
        | _, _, _, _, _, Error error, _, _
        | _, _, _, _, _, _, Error error, _
        | _, _, _, _, _, _, _, Error error -> Error error))
  | _ -> Error "request must be an object"

let json_file path parse =
  try
    match parse (Yojson.Safe.from_file path) with
    | Ok value -> value
    | Error error -> fail (path ^ ": " ^ error)
  with
  | Yojson.Json_error error -> fail (path ^ ": " ^ error)
  | Sys_error error -> fail error

let admit_program ~support ~requirement raw =
  if Program_envelope.is_program raw then
    Admission.decode_inference_program_source ~support ~requirement raw
  else
    Error (Admission.Verify_error
             "inference program must be a certified program envelope")

let decode_program_code raw =
  if not (Program_envelope.is_program raw) then
    Error "inference program must be a certified program envelope"
  else
    match Program_envelope.decode raw with
    | Error error -> Error (Program_envelope.error_message error)
    | Ok envelope ->
      (match Bytecode.decode envelope.Program_envelope.code with
       | Error error -> Error error
       | Ok code -> Ok code)

let list_json values =
  `List (List.map (fun value -> `String value) values)

let nullable_string_json = function
  | None -> `Null
  | Some value -> `String value

let violation_json = function
  | Inference_opcode_policy.Missing_capability { detail; capability } ->
    `Assoc [
      "kind", `String "missing_capability";
      "pc", `Int detail.pc;
      "opcode", `String detail.opcode;
      "capability", `String capability;
    ]
  | Inference_opcode_policy.Forbidden_opcode detail ->
    `Assoc [
      "kind", `String "forbidden_opcode";
      "pc", `Int detail.pc;
      "opcode", `String detail.opcode;
    ]

let unique_strings values =
  List.sort_uniq String.compare values

let unsupported_opcode_names violations =
  violations
  |> List.filter_map (function
    | Inference_opcode_policy.Forbidden_opcode detail -> Some detail.opcode
    | _ -> None)
  |> unique_strings

let missing_capability_names violations =
  violations
  |> List.filter_map (function
    | Inference_opcode_policy.Missing_capability { capability; _ } ->
      Some capability
    | _ -> None)
  |> unique_strings

let print_policy_scan ~support ~requirement raw =
  match decode_program_code raw with
  | Error error ->
    print_endline
      (Yojson.Safe.pretty_to_string
         (`Assoc [
           "status", `String "decode_error";
           "decode_error", `String error;
         ]));
    1
  | Ok code ->
    let violations =
      Inference_opcode_policy.violations ~requirement code
    in
    let admission_status, admission_error =
      match admit_program ~support ~requirement raw with
      | Ok _ -> "accepted", []
      | Error error ->
        "rejected",
        ["admission_error", `String (Admission.error_message error)]
    in
    let report =
      `Assoc (
        [
          "status", `String admission_status;
          "program_instructions", `Int (Array.length code);
          "policy_violation_count", `Int (List.length violations);
          "unsupported_opcodes",
          list_json (unsupported_opcode_names violations);
          "missing_capabilities",
          list_json (missing_capability_names violations);
          "policy_violations",
          `List (List.map violation_json violations);
        ]
        @ admission_error)
    in
    print_endline (Yojson.Safe.pretty_to_string report);
    0

let entrypoints_json target =
  `List
    (List.map
       (fun Target.{ entry_name; entry_label = label } ->
         `Assoc ["name", `String entry_name; "label", `Int label])
       target.Target.entrypoints)

let check_declared_root name declared actual =
  match declared with
  | None -> Ok ()
  | Some expected when String.equal expected actual -> Ok ()
  | Some expected ->
    Error (Printf.sprintf "%s mismatch: expected %s actual %s" name expected actual)

let read_range_source sources owner_root =
  match List.assoc_opt owner_root sources with
  | None -> None
  | Some path ->
    try Some (read_file path) with Sys_error error -> fail error

let sha256 raw =
  Digestif.SHA256.(digest_string raw |> to_hex)

let dirname path =
  let dir = Filename.dirname path in
  if String.equal dir "" then "." else dir

let resolve_path ~base path =
  if Filename.is_relative path then Filename.concat base path else path

let optional_string_any names fields =
  let rec loop = function
    | [] -> Ok None
    | name :: rest ->
      (match optional_string_field name fields with
       | Ok (Some value) -> Ok (Some value)
       | Ok None -> loop rest
       | Error error -> Error error)
  in
  loop names

let string_any names fields =
  match optional_string_any names fields with
  | Ok (Some value) -> Ok value
  | Ok None -> Error ("missing field: " ^ String.concat " or " names)
  | Error error -> Error error

let parse_batch_range_source ~base (value : Yojson.Safe.t) =
  match value with
  | `Assoc fields ->
    (match
       check_known
         fields
         ["owner_root"; "path"; "bytes"; "original_path"; "sha256"]
     with
     | Error error -> Error error
     | Ok () ->
       (match
          string_field "owner_root" fields,
          string_field "path" fields
        with
        | Ok owner_root, Ok path -> Ok (owner_root, resolve_path ~base path)
        | Error error, _
        | _, Error error -> Error error))
  | _ -> Error "range source must be an object"

let rec parse_batch_range_sources ~base acc = function
  | [] -> Ok (List.rev acc)
  | value :: rest ->
    (match parse_batch_range_source ~base value with
     | Error error -> Error error
     | Ok range -> parse_batch_range_sources ~base (range :: acc) rest)

let path_field ~base name fields =
  match string_field name fields with
  | Ok path -> Ok (resolve_path ~base path)
  | Error error -> Error error

let optional_path_field ~base name fields =
  match optional_string_field name fields with
  | Ok None -> Ok None
  | Ok (Some path) -> Ok (Some (resolve_path ~base path))
  | Error error -> Error error

let parse_batch_stage ~batch_base ~top_support (value : Yojson.Safe.t) =
  match value with
  | `Assoc fields ->
    (match
       check_known
         fields
         [
           "artifact_dir";
           "evidence_files";
           "expected_output_root";
           "expected_output_sha256";
           "input";
           "layer";
           "model-deployment";
           "model-ranges";
           "model_deployment";
           "model_ranges";
           "negative_fixture_note";
           "output_count";
           "output_memory_base";
           "packet_files";
           "program";
           "range_source_count";
           "range_sources";
           "range_sources_tsv";
           "request";
           "requirement";
           "scan_command";
           "session_command_prefix";
           "session_command_suffix";
           "source_cutpoint";
           "stage";
           "stage_id";
           "standalone_rerun_command";
           "standalone_scan";
           "standalone_session";
           "support";
           "target";
         ]
     with
     | Error error -> Error error
     | Ok () ->
    let stage_id =
      match optional_string_any ["stage_id"; "stage"] fields with
      | Ok (Some value) -> Ok value
      | Ok None -> Error "missing field: stage_id or stage"
      | Error error -> Error error
    in
    let expected_output_root =
      match optional_nullable_string_field "expected_output_root" fields with
      | Ok value -> Ok value
      | Error error -> Error error
    in
    let artifact_dir =
      match optional_string_field "artifact_dir" fields with
      | Ok (Some path) -> Some (resolve_path ~base:batch_base path)
      | Ok None -> None
      | Error error -> fail error
    in
    (match stage_id, expected_output_root with
    | Error error, _
    | _, Error error -> Error error
    | Ok stage_id, Ok expected_output_root ->
      (match artifact_dir with
       | Some stage_base ->
         let support_path =
           Filename.concat stage_base "support.json"
         in
         let range_values =
           match optional_field "range_sources" fields with
           | Ok (Some (`List values)) -> Ok values
           | Ok (Some _) -> Error "field must be a list: range_sources"
           | Ok None -> Ok []
           | Error error -> Error error
         in
         (match range_values with
          | Error error -> Error error
          | Ok values ->
            (match parse_batch_range_sources ~base:stage_base [] values with
             | Error error -> Error error
             | Ok range_sources ->
               Ok {
                 stage_id;
                 program_path = Filename.concat stage_base "program.ocpg";
                 requirement_path = Filename.concat stage_base "requirement.json";
                 target_path = Filename.concat stage_base "target.json";
                 support_path;
                 request_path = Filename.concat stage_base "request.json";
                 input_path = Filename.concat stage_base "request-input.json";
                 model_ranges_path =
                   Filename.concat stage_base "model-ranges.json";
                 model_deployment_path =
                   Some (Filename.concat stage_base "model-deployment.json");
                 range_sources;
                 expected_output_root;
               }))
       | None ->
         let base = batch_base in
         let support_path =
           match optional_path_field ~base "support" fields, top_support with
           | Ok (Some path), _ -> Ok path
           | Ok None, Some path -> Ok path
           | Ok None, None -> Error "missing field: support"
           | Error error, _ -> Error error
         in
         let range_values =
           match optional_field "range_sources" fields with
           | Ok (Some (`List values)) -> Ok values
           | Ok (Some _) -> Error "field must be a list: range_sources"
           | Ok None -> Ok []
           | Error error -> Error error
         in
         (match
            path_field ~base "program" fields,
            path_field ~base "requirement" fields,
            path_field ~base "target" fields,
            support_path,
            path_field ~base "request" fields,
            path_field ~base "input" fields,
            string_any ["model_ranges"; "model-ranges"] fields,
            optional_string_any ["model_deployment"; "model-deployment"] fields,
            range_values
          with
          | Ok program_path,
            Ok requirement_path,
            Ok target_path,
            Ok support_path,
            Ok request_path,
            Ok input_path,
            Ok model_ranges_path,
            Ok model_deployment_path,
            Ok range_values ->
            (match parse_batch_range_sources ~base [] range_values with
             | Error error -> Error error
             | Ok range_sources ->
               Ok {
                 stage_id;
                 program_path;
                 requirement_path;
                 target_path;
                 support_path;
                 request_path;
                 input_path;
                 model_ranges_path = resolve_path ~base model_ranges_path;
                 model_deployment_path =
                   Option.map (resolve_path ~base) model_deployment_path;
                 range_sources;
                 expected_output_root;
               })
          | Error error, _, _, _, _, _, _, _, _
          | _, Error error, _, _, _, _, _, _, _
          | _, _, Error error, _, _, _, _, _, _
          | _, _, _, Error error, _, _, _, _, _
          | _, _, _, _, Error error, _, _, _, _
          | _, _, _, _, _, Error error, _, _, _
          | _, _, _, _, _, _, Error error, _, _
          | _, _, _, _, _, _, _, Error error, _
          | _, _, _, _, _, _, _, _, Error error -> Error error))))
  | _ -> Error "batch stage must be an object"

let parse_batch_json batch_path (json : Yojson.Safe.t) =
  let batch_base = dirname batch_path in
  match json with
  | `Assoc fields ->
    (match
       check_known
         fields
         [
           "artifact_root";
           "batch_semantics";
           "claim";
           "created_utc";
           "harness";
           "harness_sha256";
           "lite_node_commit";
           "model_deployment";
           "negative_fixture";
           "octra_inference_commit";
           "receipt_mode";
           "schema";
           "schema_version";
           "source_benchmark_artifact";
           "stages";
           "support";
           "type";
         ]
     with
     | Error error -> Error error
     | Ok () ->
    let top_support =
      match optional_string_field "support" fields with
      | Ok None -> Ok None
      | Ok (Some path) -> Ok (Some (resolve_path ~base:batch_base path))
      | Error error -> Error error
    in
    (match top_support, list_field "stages" fields with
     | Error error, _
     | _, Error error -> Error error
     | Ok top_support, Ok stages ->
       if List.length stages = 0 then Error "batch must contain at least one stage"
       else if List.length stages > max_batch_stages then
         Error
           (Printf.sprintf
              "batch has too many stages: %d > %d"
              (List.length stages)
              max_batch_stages)
       else
       let rec loop acc = function
         | [] -> Ok { batch_path; stages = List.rev acc }
         | value :: rest ->
           (match parse_batch_stage ~batch_base ~top_support value with
            | Error error -> Error error
            | Ok stage ->
              if List.exists
                   (fun existing ->
                     String.equal existing.stage_id stage.stage_id)
                   acc
              then Error ("duplicate stage_id: " ^ stage.stage_id)
              else loop (stage :: acc) rest)
       in
       loop [] stages))
  | _ -> Error "batch must be an object"

let batch_file path =
  try
    match parse_batch_json path (Yojson.Safe.from_file path) with
    | Ok batch -> batch
    | Error error -> fail (path ^ ": " ^ error)
  with
  | Yojson.Json_error error -> fail (path ^ ": " ^ error)
  | Sys_error error -> fail error

let batch_cache () =
  {
    owner_bytes = Hashtbl.create 256;
    pins_by_ranges_root_and_limit = Hashtbl.create 64;
  }

let cached_range_reader cache sources owner_root =
  match Hashtbl.find_opt cache.owner_bytes owner_root with
  | Some bytes -> Some bytes
  | None ->
    (match read_range_source sources owner_root with
     | None -> None
     | Some bytes ->
       if Hashtbl.length cache.owner_bytes >= max_batch_owner_cache_entries then
         fail
           (Printf.sprintf
              "batch owner-byte cache exceeded: %d"
              max_batch_owner_cache_entries);
       Hashtbl.replace cache.owner_bytes owner_root bytes;
       Some bytes)

let batch_pins cache ~limits ~read model =
  let model_ranges_root = Model.root model in
  let cache_key =
    Printf.sprintf
      "%s:%d"
      model_ranges_root
      limits.Req.max_model_bytes
  in
  match Hashtbl.find_opt cache.pins_by_ranges_root_and_limit cache_key with
  | Some pins -> Ok pins
  | None ->
    (match Store.pin ~limits ~read model with
     | Error error -> Error error
     | Ok pins ->
       if
         Hashtbl.length cache.pins_by_ranges_root_and_limit
         >= max_batch_pin_cache_entries
       then
         fail
           (Printf.sprintf
              "batch pin cache exceeded: %d"
              max_batch_pin_cache_entries);
       Hashtbl.replace cache.pins_by_ranges_root_and_limit cache_key pins;
       Ok pins)

let run_batch_stage ~cache ~timing_mode stage =
  let timer = start_timer timing_mode in
  let raw_program =
    try read_file stage.program_path with Sys_error error -> fail error
  in
  let requirement_packet =
    json_file stage.requirement_path parse_requirement_json
  in
  let requirement = requirement_packet.requirement in
  let support =
    json_file stage.support_path parse_support_json
  in
  let target_packet =
    json_file stage.target_path parse_target_json
  in
  let model_packet =
    json_file stage.model_ranges_path parse_model_ranges_json
  in
  let deployment_packet =
    Option.map
      (fun path -> json_file path parse_model_deployment_json)
      stage.model_deployment_path
  in
  let request_packet =
    json_file stage.request_path parse_request_json
  in
  let input =
    try read_file stage.input_path with Sys_error error -> fail error
  in
  timing_mark timer "read_parse";
  let code =
    match decode_program_code raw_program with
    | Error error -> fail error
    | Ok code -> code
  in
  let violations =
    Inference_opcode_policy.violations ~requirement code
  in
  let admitted =
    match admit_program ~support ~requirement raw_program with
    | Error error -> fail (Admission.error_message error)
    | Ok admitted -> admitted
  in
  (match Target.check ~admitted target_packet.target with
   | Error error -> fail (Target.error_message error)
   | Ok () -> ());
  let requirement_root = Req.root requirement in
  let target_root = Target.root target_packet.target in
  (match
     check_declared_root
       "declared requirement root"
       requirement_packet.declared_root
       requirement_root,
     check_declared_root
       "declared target root"
       target_packet.declared_root
       target_root
   with
   | Error error, _
   | _, Error error -> fail error
   | Ok (), Ok () -> ());
  timing_mark timer "admission";
  let model_ranges_root = Model.root model_packet.model in
  (match Model.check ~target:target_packet.target model_packet.model with
   | Error error -> fail (Model.error_message error)
   | Ok () -> ());
  (match
     check_declared_root
       "declared model ranges root"
       model_packet.declared_root
       model_ranges_root
   with
   | Error error -> fail error
   | Ok () -> ());
  let deployment =
    match deployment_packet with
    | None -> None
    | Some packet ->
      (match
         Deployment.check
           ~target:target_packet.target
           ~requirement
           packet.deployment
       with
       | Error error -> fail (Deployment.error_message error)
       | Ok () ->
         let model_deployment_root = Deployment.root packet.deployment in
         (match
            check_declared_root
              "declared model deployment root"
              packet.declared_root
              model_deployment_root
          with
          | Error error -> fail error
          | Ok () -> Some packet.deployment))
  in
  (match
     Request.check
       ~target:target_packet.target
       ~requirement
       request_packet.request
   with
   | Error error -> fail (Request.error_message error)
   | Ok () -> ());
  let request_root = Request.root request_packet.request in
  (match
     check_declared_root
       "declared request root"
       request_packet.declared_root
       request_root
   with
   | Error error -> fail error
   | Ok () -> ());
  timing_mark timer "root_checks";
  let read =
    cached_range_reader cache stage.range_sources
  in
  let pins =
    match batch_pins cache ~limits:requirement.Req.limits ~read model_packet.model with
    | Error error -> fail (Store.error_message error)
    | Ok pins -> pins
  in
  timing_mark timer "pin_model_ranges";
  let plan =
    match deployment with
    | None ->
      Plan.create
        ~admitted
        ~target:target_packet.target
        ~request:request_packet.request
        ~model:model_packet.model
        ~pins
        ~input
    | Some deployment ->
      Plan.create_with_deployment
        ~deployment
        ~admitted
        ~target:target_packet.target
        ~request:request_packet.request
        ~model:model_packet.model
        ~pins
        ~input
  in
  let plan =
    match plan with
    | Error error -> fail (Plan.error_message error)
    | Ok plan -> plan
  in
  timing_mark timer "plan_create";
  let opened =
    match Session.open_session ~plan with
    | Error error -> fail (Session.error_message error)
    | Ok opened -> opened
  in
  timing_mark timer "open_session";
  let advanced, advance_receipt, execution_profile, opcode_profile =
    match timer.mode with
    | Timing_opcode ->
      (match
         Session.advance_profiled
           ~profile:diagnostic_profile
           ~plan
           ~expected_sequence:0
           opened
       with
       | Error error -> fail (Session.error_message error)
       | Ok result -> result)
    | Timing_none
    | Timing_stage ->
    match Session.advance ~plan ~expected_sequence:0 opened with
    | Error error -> fail (Session.error_message error)
    | Ok (advanced, receipt) -> advanced, receipt, [], []
  in
  timing_mark timer "advance_session";
  let finalized, final_receipt =
    match
      Session.finalize
        ~expected_sequence:(Session.sequence advanced)
        advanced
    with
    | Error error -> fail (Session.error_message error)
    | Ok result -> result
  in
  timing_mark timer "finalize_session";
  let output_root = Session.output_root finalized in
  let reference_status, reference_mismatch, root_match =
    match stage.expected_output_root with
    | None -> "unchecked", false, []
    | Some expected ->
      let matched = String.equal expected output_root in
      [
        "expected_output_root", `String expected;
        "output_root_matches_expected", `Bool matched;
      ]
      |> fun fields ->
      (if matched then "matched" else "mismatch"), not matched, fields
  in
  let stage_status =
    if reference_mismatch then "reference_mismatch" else "accepted"
  in
  let unsupported = unsupported_opcode_names violations in
  let missing = missing_capability_names violations in
  let violation_values = List.map violation_json violations in
  {
    stage_json =
      `Assoc (
        [
          "stage_id", `String stage.stage_id;
          "status", `String stage_status;
          "session_status", `String "accepted";
          "reference_status", `String reference_status;
          "program_root", `String (Target.program_root admitted);
          "target_root", `String target_root;
          "request_root", `String request_root;
          "model_ranges_root", `String model_ranges_root;
          "program_instructions", `Int (Array.length (Admission.code admitted));
          "unsupported_opcodes", list_json unsupported;
          "missing_capabilities", list_json missing;
          "policy_violations", `List violation_values;
          "opened_session_root", `String (Session.root opened);
          "advanced_session_root", `String (Session.root advanced);
          "final_session_root", `String (Session.root finalized);
          "advance_receipt_root", `String (Receipt.root advance_receipt);
          "final_receipt_root", `String (Receipt.root final_receipt);
          "output_root", `String output_root;
          "output_prefix_root", `String (Session.output_prefix_root finalized);
          "candidate_root", `String (Session.candidate_root finalized);
          "effort_delta", `Int advance_receipt.Receipt.effort_delta;
          "final_effort_delta", `Int final_receipt.Receipt.effort_delta;
          "consensus_accepted", `Bool false;
        ]
        @ root_match
        @ opcode_profile_fields
            (timer.mode, execution_profile, opcode_profile)
        @ timing_fields timer);
    stage_output_root = output_root;
    stage_reference_mismatch = reference_mismatch;
    stage_unsupported_opcodes = unsupported;
    stage_missing_capabilities = missing;
    stage_policy_violations = violation_values;
  }

let unique_json_strings values =
  values |> unique_strings |> list_json

let drop_assoc_fields names = function
  | `Assoc fields ->
    `Assoc
      (List.filter
         (fun (field_name, _) ->
           not (List.exists (String.equal field_name) names))
         fields)
  | value -> value

let run_batch_file ~timing_mode path =
  let batch = batch_file path in
  let cache = batch_cache () in
  let results =
    List.map
      (run_batch_stage ~cache ~timing_mode)
      batch.stages
  in
  let stage_json = List.map (fun result -> result.stage_json) results in
  let unsupported =
    List.concat_map
      (fun result -> result.stage_unsupported_opcodes)
      results
  in
  let missing =
    List.concat_map
      (fun result -> result.stage_missing_capabilities)
      results
  in
  let violations =
    List.concat_map
      (fun result -> result.stage_policy_violations)
      results
  in
  let last_output_root =
    match List.rev results with
    | [] -> ""
    | result :: _ -> result.stage_output_root
  in
  let reference_mismatch =
    List.exists (fun result -> result.stage_reference_mismatch) results
  in
  let status =
    if reference_mismatch then "reference_mismatch" else "accepted"
  in
  let runtime_semantics =
    `Assoc [
      "session_mode", `String "independent_session_per_stage";
      "stage_lifecycle",
      `List [
        `String "open_session";
        `String "advance_session";
        `String "finalize_session";
      ];
      "batch_cache_scope",
      `List [
        `String "owner_bytes";
        `String "model_range_pins";
      ];
      "continuation_supported", `Bool false;
    ]
  in
  let batch_payload =
    "octra.inference.run_batch.report.v1:"
    ^ Yojson.Safe.to_string
        (`List
           (List.map
              (drop_assoc_fields
                 ["timing"; "execution_timing"; "opcode_timing"])
              stage_json))
  in
  let report =
    `Assoc [
      "status", `String status;
      "schema", `String "octra.inference.run_batch.report";
      "batch_path", `String batch.batch_path;
      "stage_count", `Int (List.length results);
      "batch_report_sha256", `String (sha256 batch_payload);
      "runtime_semantics", runtime_semantics;
      "last_stage_output_root", `String last_output_root;
      "unsupported_opcodes", unique_json_strings unsupported;
      "missing_capabilities", unique_json_strings missing;
      "policy_violations", `List violations;
      "owner_cache_entries", `Int (Hashtbl.length cache.owner_bytes);
      "pin_cache_entries",
      `Int (Hashtbl.length cache.pins_by_ranges_root_and_limit);
      "stages", `List stage_json;
    ]
  in
  print_endline (Yojson.Safe.pretty_to_string report);
  if reference_mismatch then 1 else 0

let usage =
  "usage: inference_admit --program FILE --requirement FILE --target FILE \
   --support FILE [--model-ranges FILE] [--model-deployment FILE] \
   [--range-source ROOT=FILE] [--request FILE] [--input FILE] \
   [--run-session] [--scan-policy] [--run-batch FILE] \
   [--timing-mode none|stage|opcode]"

let () =
  let paths =
    {
      program = None;
      requirement = None;
      target = None;
      support = None;
      request = None;
      input = None;
      model_ranges = None;
      model_deployment = None;
      range_sources = [];
      run_session = false;
      scan_policy = false;
      run_batch = None;
      timing_mode = Timing_none;
    }
  in
  let set_program value = paths.program <- Some value in
  let set_requirement value = paths.requirement <- Some value in
  let set_target value = paths.target <- Some value in
  let set_support value = paths.support <- Some value in
  let set_request value = paths.request <- Some value in
  let set_input value = paths.input <- Some value in
  let set_model_ranges value = paths.model_ranges <- Some value in
  let set_model_deployment value =
    paths.model_deployment <- Some value
  in
  let set_run_batch value = paths.run_batch <- Some value in
  let add_range_source value =
    let owner_root, path =
      split_pair value "--range-source requires <owner-root=file>"
    in
    paths.range_sources <- (owner_root, path) :: paths.range_sources
  in
  let set_run_session () = paths.run_session <- true in
  let set_scan_policy () = paths.scan_policy <- true in
  let set_timing_mode = function
    | "none" -> paths.timing_mode <- Timing_none
    | "stage" -> paths.timing_mode <- Timing_stage
    | "opcode" -> paths.timing_mode <- Timing_opcode
    | value ->
      fail ("unsupported --timing-mode: " ^ value)
  in
  Arg.parse
    [
      "--program", Arg.String set_program, "certified program envelope";
      "--requirement", Arg.String set_requirement, "execution requirement JSON";
      "--target", Arg.String set_target, "inference target JSON";
      "--support",
      Arg.String set_support,
      "declared node support JSON";
      "--model-ranges",
      Arg.String set_model_ranges,
      "optional immutable model ranges JSON";
      "--model-deployment",
      Arg.String set_model_deployment,
      "optional rooted model deployment JSON";
      "--range-source",
      Arg.String add_range_source,
      "authenticated local owner bytes, as owner-root=file";
      "--request", Arg.String set_request, "optional request fixture JSON";
      "--input", Arg.String set_input, "authenticated request input bytes";
      "--run-session",
      Arg.Unit set_run_session,
      "run a local open/advance/finalize session after admission";
      "--scan-policy",
      Arg.Unit set_scan_policy,
      "report all visible inference opcode policy violations";
      "--run-batch",
      Arg.String set_run_batch,
      "run independent one-shot inference stages from a batch fixture";
      "--timing-mode",
      Arg.String set_timing_mode,
      "optional local diagnostic timing: none, stage, or opcode";
    ]
    (fun value -> fail ("unexpected argument: " ^ value))
    usage;
  if paths.scan_policy && paths.run_session then
    fail "--scan-policy cannot be combined with --run-session";
  (match paths.run_batch with
   | Some path ->
     exit (run_batch_file ~timing_mode:paths.timing_mode path)
   | None -> ());
  let program_path = require_path "--program" paths.program in
  let requirement_path = require_path "--requirement" paths.requirement in
  let target_path = require_path "--target" paths.target in
  let requirement_packet =
    json_file requirement_path parse_requirement_json
  in
  let requirement = requirement_packet.requirement in
  let support =
    json_file
      (require_path "--support" paths.support)
      parse_support_json
  in
  let target_packet = json_file target_path parse_target_json in
  let model_packet =
    Option.map
      (fun path -> json_file path parse_model_ranges_json)
      paths.model_ranges
  in
  let deployment_packet =
    Option.map
      (fun path -> json_file path parse_model_deployment_json)
      paths.model_deployment
  in
  let request_packet =
    Option.map
      (fun path -> json_file path parse_request_json)
      paths.request
  in
  let input =
    if paths.run_session then
      Some (read_file (require_path "--input" paths.input))
    else None
  in
  let raw_program =
    try read_file program_path with Sys_error error -> fail error
  in
  if paths.scan_policy then (
    let status = print_policy_scan ~support ~requirement raw_program in
    exit status);
  match admit_program ~support ~requirement raw_program with
  | Error error -> fail (Admission.error_message error)
  | Ok admitted ->
    (match Target.check ~admitted target_packet.target with
     | Error error -> fail (Target.error_message error)
     | Ok () ->
       let requirement_root = Req.root requirement in
       let target_root = Target.root target_packet.target in
       (match
          check_declared_root
            "declared requirement root"
            requirement_packet.declared_root
            requirement_root,
          check_declared_root
            "declared target root"
            target_packet.declared_root
            target_root
        with
        | Error error, _
        | _, Error error -> fail error
        | Ok (), Ok () ->
          let deployment_fields =
            match deployment_packet with
            | None -> []
            | Some packet ->
              (match
                 Deployment.check
                   ~target:target_packet.target
                   ~requirement
                   packet.deployment
               with
               | Error error -> fail (Deployment.error_message error)
               | Ok () ->
                 let model_deployment_root =
                   Deployment.root packet.deployment
                 in
                 match
                   check_declared_root
                     "declared model deployment root"
                     packet.declared_root
                     model_deployment_root
                 with
                 | Error error -> fail error
                 | Ok () ->
                   [
                     "model_deployment_root",
                     `String model_deployment_root;
                     "tensor_index_root",
                     `String
                       packet.deployment.Deployment.tensor_index_root;
                     "capability_set_root",
                     `String
                       packet.deployment.Deployment.capability_set_root;
                     "tokenizer_root",
                     nullable_string_json
                       packet.deployment.Deployment.tokenizer_root;
                     "default_program_root",
                     nullable_string_json
                       packet.deployment.Deployment.default_program_root;
                   ])
          in
          let model_fields =
            match model_packet with
            | None -> []
            | Some packet ->
              (match Model.check ~target:target_packet.target packet.model with
               | Error error -> fail (Model.error_message error)
               | Ok () ->
                 let model_ranges_root = Model.root packet.model in
                 match
                   check_declared_root
                     "declared model ranges root"
                     packet.declared_root
                     model_ranges_root
                 with
                 | Error error -> fail error
                | Ok () ->
                   [
                     "model_ranges_root", `String model_ranges_root;
                     "model_range_count",
                     `Int (List.length packet.model.Model.ranges);
                   ])
          in
          let request_field =
            match request_packet with
            | None -> []
            | Some packet ->
              (match
                 Request.check
                   ~target:target_packet.target
                   ~requirement
                   packet.request
               with
               | Error error -> fail (Request.error_message error)
               | Ok () ->
                 let request_root = Request.root packet.request in
                 match
                   check_declared_root
                     "declared request root"
                     packet.declared_root
                     request_root
                 with
                 | Error error -> fail error
                 | Ok () ->
                   [
                     "request_root", `String request_root;
                     "request_entrypoint",
                     `String packet.request.Request.entrypoint;
                   ])
          in
          let session_fields =
            if not paths.run_session then []
            else
              let timer = start_timer paths.timing_mode in
              match model_packet, request_packet with
              | Some model_packet, Some request_packet ->
                (match
                   Model.check
                     ~target:target_packet.target
                     model_packet.model
                 with
                 | Error error -> fail (Model.error_message error)
                 | Ok () ->
                   timing_mark timer "model_check";
                   let read =
                     read_range_source paths.range_sources
                   in
                   match
                     Store.pin
                       ~limits:requirement.Req.limits
                       ~read
                       model_packet.model
                   with
                   | Error error -> fail (Store.error_message error)
                   | Ok pins ->
                     timing_mark timer "pin_model_ranges";
                     (match input with
                      | None -> fail "--run-session requires --input"
                      | Some input ->
                        let deployment =
                          Option.map
                            (fun packet -> packet.deployment)
                            deployment_packet
                        in
                        (match
                           match deployment with
                           | None ->
                             Plan.create
                               ~admitted
                               ~target:target_packet.target
                               ~request:request_packet.request
                               ~model:model_packet.model
                               ~pins
                               ~input
                           | Some deployment ->
                             Plan.create_with_deployment
                               ~deployment
                               ~admitted
                               ~target:target_packet.target
                               ~request:request_packet.request
                               ~model:model_packet.model
                               ~pins
                               ~input
                         with
                         | Error error -> fail (Plan.error_message error)
                         | Ok plan ->
                           timing_mark timer "plan_create";
                           let opened =
                             match Session.open_session ~plan with
                             | Error error -> fail (Session.error_message error)
                             | Ok opened -> opened
                           in
                           timing_mark timer "open_session";
                           let
                             advanced,
                             advance_receipt,
                             execution_profile,
                             opcode_profile
                           =
                             match paths.timing_mode with
                             | Timing_opcode ->
                               (match
                                  Session.advance_profiled
                                    ~profile:diagnostic_profile
                                    ~plan
                                    ~expected_sequence:0
                                    opened
                                with
                                | Error error ->
                                  fail (Session.error_message error)
                                | Ok result -> result)
                             | Timing_none
                             | Timing_stage ->
                               (match
                                  Session.advance
                                    ~plan
                                    ~expected_sequence:0
                                    opened
                                with
                                | Error error ->
                                  fail (Session.error_message error)
                                | Ok (advanced, receipt) ->
                                  advanced, receipt, [], [])
                           in
                           timing_mark timer "advance_session";
                           let finalized, final_receipt =
                             match
                               Session.finalize
                                 ~expected_sequence:(Session.sequence advanced)
                                 advanced
                             with
                             | Error error -> fail (Session.error_message error)
                             | Ok result -> result
                           in
                           timing_mark timer "finalize_session";
                           [
                             "opened_session_root",
                             `String (Session.root opened);
                             "advanced_session_root",
                             `String (Session.root advanced);
                             "final_session_root",
                             `String (Session.root finalized);
                             "advance_receipt_root",
                             `String (Receipt.root advance_receipt);
                             "final_receipt_root",
                             `String (Receipt.root final_receipt);
                             "output_root",
                             `String (Session.output_root finalized);
                             "output_prefix_root",
                             `String (Session.output_prefix_root finalized);
                             "candidate_root",
                             `String (Session.candidate_root finalized);
                             "effort_delta",
                             `Int advance_receipt.Receipt.effort_delta;
                             "consensus_accepted", `Bool false;
                           ]
                           @ opcode_profile_fields
                               ( paths.timing_mode,
                                 execution_profile,
                                 opcode_profile )
                           @ timing_fields timer)))
              | _ ->
                fail "--run-session requires --model-ranges and --request"
          in
          let report =
            `Assoc (
              [
                "status", `String "accepted";
                "program_root", `String (Target.program_root admitted);
                "program_provenance",
                `String
                  (Admission.provenance_name
                     (Admission.provenance admitted));
                "program_attested",
                `Bool (Admission.program_attested admitted);
                "requirement_root", `String requirement_root;
                "target_root", `String target_root;
                "program_instructions",
                `Int (Array.length (Admission.code admitted));
                "program_effects",
                list_json
                  (Admission.effects admitted
                   |> Octra_vm.Program_effects.names);
                "support_mode",
                `String "declared";
                "runtime_support_verified",
                `Bool false;
                "entrypoints", entrypoints_json target_packet.target;
              ]
              @ deployment_fields
              @ model_fields
              @ request_field
              @ session_fields)
          in
          print_endline (Yojson.Safe.pretty_to_string report)))
