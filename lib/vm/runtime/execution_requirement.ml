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


type capability = {
  name : string;
  root : string;
}

type limits = {
  max_model_bytes : int;
  max_view_bytes : int;
  max_session_bytes : int;
  max_scratch_bytes : int;
  max_output_bytes : int;
  max_advance_effort : int;
}

type t = {
  vm_semantics_root : string;
  numerical_root : string;
  effort_root : string;
  capabilities : capability list;
  limits : limits;
}

type support = {
  support_vm_semantics_root : string;
  support_numerical_roots : string list;
  support_effort_roots : string list;
  support_capabilities : capability list;
  support_limits : limits;
}

type error =
  | Bad_root of string
  | Bad_name of string
  | Bad_limit of string * int
  | Duplicate_capability of string
  | Vm_semantics_mismatch
  | Numerical_root_unsupported of string
  | Effort_root_unsupported of string
  | Capability_unsupported of capability
  | Limit_exceeded of string * int * int

let hex = function
  | '0' .. '9'
  | 'a' .. 'f' -> true
  | _ -> false

let valid_root value =
  String.length value = 64 && String.for_all hex value

let name_char = function
  | 'a' .. 'z'
  | '0' .. '9'
  | '.'
  | '_'
  | '-' -> true
  | _ -> false

let valid_name value =
  value <> "" && String.for_all name_char value

let capability_order left right =
  match String.compare left.name right.name with
  | 0 -> String.compare left.root right.root
  | order -> order

let sort_capabilities capabilities =
  List.sort capability_order capabilities

let capability_json capability =
  `Assoc [
    "name", `String capability.name;
    "root", `String capability.root;
  ]

let capability_set_json capabilities =
  `List (List.map capability_json (sort_capabilities capabilities))

let capability_set_root capabilities =
  let payload = Yojson.Safe.to_string (capability_set_json capabilities) in
  Digestif.SHA256.(
    digest_string ("octra:inference:capability-set\000" ^ payload) |> to_hex)

let limits_json limits =
  `Assoc [
    "max_model_bytes", `Int limits.max_model_bytes;
    "max_view_bytes", `Int limits.max_view_bytes;
    "max_session_bytes", `Int limits.max_session_bytes;
    "max_scratch_bytes", `Int limits.max_scratch_bytes;
    "max_output_bytes", `Int limits.max_output_bytes;
    "max_advance_effort", `Int limits.max_advance_effort;
  ]

let to_json requirement =
  `Assoc [
    "vm_semantics_root", `String requirement.vm_semantics_root;
    "numerical_root", `String requirement.numerical_root;
    "effort_root", `String requirement.effort_root;
    "capabilities", capability_set_json requirement.capabilities;
    "limits", limits_json requirement.limits;
  ]

let root requirement =
  let payload = Yojson.Safe.to_string (to_json requirement) in
  Digestif.SHA256.(
    digest_string ("octra:inference:requirement\000" ^ payload) |> to_hex)

let check_root value =
  if valid_root value then Ok () else Error (Bad_root value)

let check_name value =
  if valid_name value then Ok () else Error (Bad_name value)

let check_limit name value =
  if value >= 0 then Ok () else Error (Bad_limit (name, value))

let check_limits limits =
  match check_limit "max_model_bytes" limits.max_model_bytes with
  | Error error -> Error error
  | Ok () ->
    (match check_limit "max_view_bytes" limits.max_view_bytes with
     | Error error -> Error error
     | Ok () ->
       (match check_limit "max_session_bytes" limits.max_session_bytes with
        | Error error -> Error error
        | Ok () ->
          (match check_limit "max_scratch_bytes" limits.max_scratch_bytes with
           | Error error -> Error error
           | Ok () ->
             (match check_limit "max_output_bytes" limits.max_output_bytes with
              | Error error -> Error error
              | Ok () ->
                check_limit
                  "max_advance_effort"
                  limits.max_advance_effort))))

let rec check_capabilities seen = function
  | [] -> Ok ()
  | capability :: rest ->
    if List.mem capability.name seen then
      Error (Duplicate_capability capability.name)
    else
      match check_name capability.name, check_root capability.root with
      | Error error, _
      | _, Error error -> Error error
      | Ok (), Ok () -> check_capabilities (capability.name :: seen) rest

let capability_key capability =
  capability.name ^ "\000" ^ capability.root

let rec check_support_capabilities seen = function
  | [] -> Ok ()
  | capability :: rest ->
    let key = capability_key capability in
    if List.mem key seen then
      Error (Duplicate_capability capability.name)
    else
      match check_name capability.name, check_root capability.root with
      | Error error, _
      | _, Error error -> Error error
      | Ok (), Ok () -> check_support_capabilities (key :: seen) rest

let check_roots roots =
  let rec loop = function
    | [] -> Ok ()
    | root :: rest ->
      (match check_root root with
       | Error error -> Error error
       | Ok () -> loop rest)
  in
  loop roots

let validate requirement =
  match check_root requirement.vm_semantics_root with
  | Error error -> Error error
  | Ok () ->
    (match check_root requirement.numerical_root with
     | Error error -> Error error
     | Ok () ->
       (match check_root requirement.effort_root with
        | Error error -> Error error
        | Ok () ->
          (match check_capabilities [] requirement.capabilities with
           | Error error -> Error error
           | Ok () -> check_limits requirement.limits)))

let validate_support support =
  match check_root support.support_vm_semantics_root with
  | Error error -> Error error
  | Ok () ->
    (match check_roots support.support_numerical_roots with
     | Error error -> Error error
     | Ok () ->
       (match check_roots support.support_effort_roots with
        | Error error -> Error error
        | Ok () ->
          (match check_support_capabilities [] support.support_capabilities with
           | Error error -> Error error
           | Ok () -> check_limits support.support_limits)))

let has_capability capabilities capability =
  List.exists
    (fun candidate ->
      String.equal candidate.name capability.name
      && String.equal candidate.root capability.root)
    capabilities

let check_limit_supported name required available =
  if required <= available then Ok ()
  else Error (Limit_exceeded (name, required, available))

let check_limits_supported required available =
  match
    check_limit_supported
      "max_model_bytes"
      required.max_model_bytes
      available.max_model_bytes
  with
  | Error error -> Error error
  | Ok () ->
    (match
       check_limit_supported
         "max_view_bytes"
         required.max_view_bytes
         available.max_view_bytes
     with
     | Error error -> Error error
     | Ok () ->
       (match
          check_limit_supported
            "max_session_bytes"
            required.max_session_bytes
            available.max_session_bytes
        with
        | Error error -> Error error
        | Ok () ->
          (match
             check_limit_supported
               "max_scratch_bytes"
               required.max_scratch_bytes
               available.max_scratch_bytes
           with
           | Error error -> Error error
           | Ok () ->
             (match
                check_limit_supported
                  "max_output_bytes"
                  required.max_output_bytes
                  available.max_output_bytes
              with
              | Error error -> Error error
              | Ok () ->
                check_limit_supported
                  "max_advance_effort"
                  required.max_advance_effort
                  available.max_advance_effort))))

let check support requirement =
  match validate requirement with
  | Error error -> Error error
  | Ok () ->
    (match validate_support support with
     | Error error -> Error error
     | Ok () ->
       if not (String.equal
                 support.support_vm_semantics_root
                 requirement.vm_semantics_root) then
         Error Vm_semantics_mismatch
       else if not (List.mem
                      requirement.numerical_root
                      support.support_numerical_roots) then
         Error (Numerical_root_unsupported requirement.numerical_root)
       else if not (List.mem
                      requirement.effort_root
                      support.support_effort_roots) then
         Error (Effort_root_unsupported requirement.effort_root)
       else
         let missing =
           List.find_opt
             (fun capability ->
               not (has_capability support.support_capabilities capability))
             requirement.capabilities
         in
         match missing with
         | Some capability -> Error (Capability_unsupported capability)
         | None ->
           check_limits_supported requirement.limits support.support_limits)

let error_message = function
  | Bad_root root -> Printf.sprintf "invalid root: %s" root
  | Bad_name name -> Printf.sprintf "invalid capability name: %s" name
  | Bad_limit (name, value) ->
    Printf.sprintf "invalid limit %s: %d" name value
  | Duplicate_capability name ->
    Printf.sprintf "duplicate capability: %s" name
  | Vm_semantics_mismatch ->
    "VM semantics root mismatch"
  | Numerical_root_unsupported root ->
    Printf.sprintf "unsupported numerical root: %s" root
  | Effort_root_unsupported root ->
    Printf.sprintf "unsupported effort root: %s" root
  | Capability_unsupported capability ->
    Printf.sprintf
      "unsupported capability: %s %s"
      capability.name capability.root
  | Limit_exceeded (name, required, available) ->
    Printf.sprintf
      "limit exceeded %s: required %d available %d"
      name required available
