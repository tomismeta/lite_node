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
module Program_envelope = Octra_vm.Program_envelope
module Program_type_flow = Octra_vm.Program_type_flow
module Model = Octra_vm.Inference_model
module Bytecode = Octra_vm.Bytecode
module Inference_opcode_policy = Octra_vm.Inference_opcode_policy
module Plan = Octra_vm.Inference_plan
module Receipt = Octra_vm.Inference_receipt
module Req = Octra_vm.Execution_requirement
module Deployment = Octra_vm.Inference_model_deployment
module Request = Octra_vm.Inference_request
module Session = Octra_vm.Inference_session
module Store = Octra_vm.Inference_store
module Target = Octra_vm.Inference_target

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

let fail message =
  prerr_endline message;
  exit 1

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

let usage =
  "usage: inference_admit --program FILE --requirement FILE --target FILE \
   --support FILE [--model-ranges FILE] [--model-deployment FILE] \
   [--range-source ROOT=FILE] [--request FILE] [--input FILE] \
   [--run-session] [--scan-policy]"

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
  let add_range_source value =
    let owner_root, path =
      split_pair value "--range-source requires <owner-root=file>"
    in
    paths.range_sources <- (owner_root, path) :: paths.range_sources
  in
  let set_run_session () = paths.run_session <- true in
  let set_scan_policy () = paths.scan_policy <- true in
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
    ]
    (fun value -> fail ("unexpected argument: " ^ value))
    usage;
  if paths.scan_policy && paths.run_session then
    fail "--scan-policy cannot be combined with --run-session";
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
              match model_packet, request_packet with
              | Some model_packet, Some request_packet ->
                (match
                   Model.check
                     ~target:target_packet.target
                     model_packet.model
                 with
                 | Error error -> fail (Model.error_message error)
                 | Ok () ->
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
                           (match Session.open_session ~plan with
                            | Error error -> fail (Session.error_message error)
                            | Ok opened ->
                              (match
                                 Session.advance
                                   ~plan
                                   ~expected_sequence:0
                                   opened
                               with
                               | Error error -> fail (Session.error_message error)
                               | Ok (advanced, advance_receipt) ->
                                 (match
                                    Session.finalize
                                      ~expected_sequence:(Session.sequence advanced)
                                      advanced
                                  with
                                  | Error error -> fail (Session.error_message error)
                                  | Ok (finalized, final_receipt) ->
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
                                      "candidate_root",
                                      `String (Session.candidate_root finalized);
                                      "effort_delta",
                                      `Int advance_receipt.Receipt.effort_delta;
                                      "consensus_accepted", `Bool false;
                                    ]))))))
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
