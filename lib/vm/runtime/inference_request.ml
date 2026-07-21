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


type t = {
  schema : int;
  target_root : string;
  entrypoint : string;
  input_root : string;
  request_nonce : string;
  max_output_bytes : int;
  max_advance_effort : int;
}

type error =
  | Bad_schema of int
  | Bad_root of string
  | Bad_name of string
  | Bad_limit of string * int
  | Target_root_mismatch of string * string
  | Requirement_root_mismatch of string * string
  | Entrypoint_unsupported of string
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

let to_json request =
  `Assoc [
    "entrypoint", `String request.entrypoint;
    "input_root", `String request.input_root;
    "max_advance_effort", `Int request.max_advance_effort;
    "max_output_bytes", `Int request.max_output_bytes;
    "request_nonce", `String request.request_nonce;
    "schema", `Int request.schema;
    "target_root", `String request.target_root;
  ]

let root request =
  let payload = Yojson.Safe.to_string (to_json request) in
  Digestif.SHA256.(
    digest_string ("octra:inference:request\000" ^ payload) |> to_hex)

let check_root value =
  if valid_root value then Ok () else Error (Bad_root value)

let check_name value =
  if valid_name value then Ok () else Error (Bad_name value)

let check_limit name value =
  if value >= 0 then Ok () else Error (Bad_limit (name, value))

let validate request =
  if request.schema <> 1 then Error (Bad_schema request.schema)
  else
    match check_root request.target_root with
    | Error error -> Error error
    | Ok () ->
      (match check_name request.entrypoint with
       | Error error -> Error error
       | Ok () ->
         (match check_root request.input_root with
          | Error error -> Error error
          | Ok () ->
            (match check_root request.request_nonce with
             | Error error -> Error error
             | Ok () ->
               (match check_limit
                        "max_output_bytes"
                        request.max_output_bytes with
                | Error error -> Error error
                | Ok () ->
                  check_limit
                    "max_advance_effort"
                    request.max_advance_effort))))

let check_limit_supported name required available =
  if required <= available then Ok ()
  else Error (Limit_exceeded (name, required, available))

let check_limits request (limits : Execution_requirement.limits) =
  match
    check_limit_supported
      "max_output_bytes"
      request.max_output_bytes
      limits.max_output_bytes
  with
  | Error error -> Error error
  | Ok () ->
    check_limit_supported
      "max_advance_effort"
      request.max_advance_effort
      limits.max_advance_effort

let check ~target ~requirement request =
  match validate request with
  | Error error -> Error error
  | Ok () ->
    let actual_target_root = Inference_target.root target in
    if not (String.equal request.target_root actual_target_root) then
      Error (Target_root_mismatch (request.target_root, actual_target_root))
    else
      let actual_requirement_root = Execution_requirement.root requirement in
      if not (String.equal target.requirement_root actual_requirement_root) then
        Error (Requirement_root_mismatch
                 (target.requirement_root, actual_requirement_root))
      else
        match Inference_target.entry_label target request.entrypoint with
        | None -> Error (Entrypoint_unsupported request.entrypoint)
        | Some _ -> check_limits request requirement.limits

let error_message = function
  | Bad_schema schema ->
    Printf.sprintf "unsupported request schema: %d" schema
  | Bad_root root -> Printf.sprintf "invalid root: %s" root
  | Bad_name name -> Printf.sprintf "invalid entrypoint name: %s" name
  | Bad_limit (name, value) ->
    Printf.sprintf "invalid limit %s: %d" name value
  | Target_root_mismatch (expected, actual) ->
    Printf.sprintf
      "request target root mismatch: expected %s actual %s"
      expected actual
  | Requirement_root_mismatch (expected, actual) ->
    Printf.sprintf
      "request requirement root mismatch: expected %s actual %s"
      expected actual
  | Entrypoint_unsupported name ->
    Printf.sprintf "unsupported request entrypoint: %s" name
  | Limit_exceeded (name, required, available) ->
    Printf.sprintf
      "limit exceeded %s: required %d available %d"
      name required available
