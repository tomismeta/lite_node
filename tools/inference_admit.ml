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
module Bytecode = Octra_vm.Bytecode
module Program_envelope = Octra_vm.Program_envelope
module Program_type_flow = Octra_vm.Program_type_flow
module Req = Octra_vm.Execution_requirement
module Request = Octra_vm.Inference_request
module Target = Octra_vm.Inference_target

type paths = {
  mutable program : string option;
  mutable requirement : string option;
  mutable target : string option;
  mutable support : string option;
  mutable request : string option;
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
  | _, _, _, _, _, Error error -> Error error

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

let support_from_requirement requirement =
  Req.{
    support_vm_semantics_root = requirement.vm_semantics_root;
    support_numerical_roots = [requirement.numerical_root];
    support_effort_roots = [requirement.effort_root];
    support_capabilities = requirement.capabilities;
    support_limits = requirement.limits;
  }

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
  let decoded =
    if Program_envelope.is_program raw then
      Admission.decode_program_source raw
    else
      match Bytecode.decode raw with
      | Error error -> Error (Admission.Decode_error error)
      | Ok code -> Admission.of_program code
  in
  match decoded with
  | Error error -> Error error
  | Ok admitted ->
    let facts =
      match Admission.profile admitted with
      | Admission.Program facts -> facts
      | Admission.Legacy -> Program_type_flow.empty_facts
    in
    Admission.of_program_with_requirement
      ~facts
      ~support
      ~requirement
      (Admission.code admitted)

let list_json values =
  `List (List.map (fun value -> `String value) values)

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

let usage =
  "usage: inference_admit --program FILE --requirement FILE --target FILE \
   [--support FILE] [--request FILE]"

let () =
  let paths =
    {
      program = None;
      requirement = None;
      target = None;
      support = None;
      request = None;
    }
  in
  let set_program value = paths.program <- Some value in
  let set_requirement value = paths.requirement <- Some value in
  let set_target value = paths.target <- Some value in
  let set_support value = paths.support <- Some value in
  let set_request value = paths.request <- Some value in
  Arg.parse
    [
      "--program", Arg.String set_program, "program envelope or raw bytecode";
      "--requirement", Arg.String set_requirement, "execution requirement JSON";
      "--target", Arg.String set_target, "inference target JSON";
      "--support", Arg.String set_support, "optional node support JSON";
      "--request", Arg.String set_request, "optional request fixture JSON";
    ]
    (fun value -> fail ("unexpected argument: " ^ value))
    usage;
  let program_path = require_path "--program" paths.program in
  let requirement_path = require_path "--requirement" paths.requirement in
  let target_path = require_path "--target" paths.target in
  let requirement_packet =
    json_file requirement_path parse_requirement_json
  in
  let requirement = requirement_packet.requirement in
  let support =
    match paths.support with
    | Some path -> json_file path parse_support_json
    | None -> support_from_requirement requirement
  in
  let target_packet = json_file target_path parse_target_json in
  let request_packet =
    Option.map
      (fun path -> json_file path parse_request_json)
      paths.request
  in
  let raw_program =
    try read_file program_path with Sys_error error -> fail error
  in
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
          let report =
            `Assoc (
              [
                "status", `String "accepted";
                "program_root", `String (Target.program_root admitted);
                "requirement_root", `String requirement_root;
                "target_root", `String target_root;
                "program_instructions",
                `Int (Array.length (Admission.code admitted));
                "program_effects",
                list_json
                  (Admission.effects admitted
                   |> Octra_vm.Program_effects.names);
                "support_mode",
                `String
                  (match paths.support with
                   | Some _ -> "file"
                   | None -> "derived");
                "entrypoints", entrypoints_json target_packet.target;
              ]
              @ request_field)
          in
          print_endline (Yojson.Safe.pretty_to_string report)))
