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
  model_root : string;
  store_root : string;
  tensor_index_root : string;
  tokenizer_root : string option;
  numerical_profile_root : string;
  capability_set_root : string;
  default_program_root : string option;
}

type error =
  | Bad_root of string
  | Model_root_mismatch of string * string
  | Store_root_mismatch of string * string
  | Numerical_root_mismatch of string * string
  | Capability_set_root_mismatch of string * string

let hex = function
  | '0' .. '9'
  | 'a' .. 'f' -> true
  | _ -> false

let valid_root value =
  String.length value = 64 && String.for_all hex value

let check_root value =
  if valid_root value then Ok () else Error (Bad_root value)

let check_optional_root = function
  | None -> Ok ()
  | Some root -> check_root root

let capability_set_root =
  Execution_requirement.capability_set_root

let to_json deployment =
  `Assoc [
    "model_root", `String deployment.model_root;
    "store_root", `String deployment.store_root;
    "tensor_index_root", `String deployment.tensor_index_root;
    "tokenizer_root",
    (match deployment.tokenizer_root with
     | None -> `Null
     | Some root -> `String root);
    "numerical_profile_root", `String deployment.numerical_profile_root;
    "capability_set_root", `String deployment.capability_set_root;
    "default_program_root",
    (match deployment.default_program_root with
     | None -> `Null
     | Some root -> `String root);
  ]

let root deployment =
  let payload = Yojson.Safe.to_string (to_json deployment) in
  Digestif.SHA256.(
    digest_string ("octra:inference:model-deployment\000" ^ payload) |> to_hex)

let validate deployment =
  match check_root deployment.model_root with
  | Error error -> Error error
  | Ok () ->
    (match check_root deployment.store_root with
     | Error error -> Error error
     | Ok () ->
       (match check_root deployment.tensor_index_root with
        | Error error -> Error error
        | Ok () ->
          (match check_optional_root deployment.tokenizer_root with
           | Error error -> Error error
           | Ok () ->
             (match check_root deployment.numerical_profile_root with
              | Error error -> Error error
              | Ok () ->
                (match check_root deployment.capability_set_root with
                 | Error error -> Error error
                 | Ok () ->
                   check_optional_root deployment.default_program_root)))))

let check ~target ~requirement deployment =
  match validate deployment with
  | Error error -> Error error
  | Ok () ->
    if not
        (String.equal
           deployment.model_root
           target.Inference_target.model_root)
    then
      Error
        (Model_root_mismatch
           (deployment.model_root, target.Inference_target.model_root))
    else if not
        (String.equal
           deployment.store_root
           target.Inference_target.store_root)
    then
      Error
        (Store_root_mismatch
           (deployment.store_root, target.Inference_target.store_root))
    else if not
        (String.equal
           deployment.numerical_profile_root
           requirement.Execution_requirement.numerical_root)
    then
      Error
        (Numerical_root_mismatch
           (deployment.numerical_profile_root,
            requirement.Execution_requirement.numerical_root))
    else
      let expected =
        capability_set_root requirement.Execution_requirement.capabilities
      in
      if not (String.equal deployment.capability_set_root expected) then
        Error
          (Capability_set_root_mismatch
             (deployment.capability_set_root, expected))
      else
        Ok ()

let error_message = function
  | Bad_root root -> Printf.sprintf "invalid root: %s" root
  | Model_root_mismatch (actual, expected) ->
    Printf.sprintf
      "model deployment root mismatch: actual %s expected %s"
      actual expected
  | Store_root_mismatch (actual, expected) ->
    Printf.sprintf
      "model deployment store root mismatch: actual %s expected %s"
      actual expected
  | Numerical_root_mismatch (actual, expected) ->
    Printf.sprintf
      "model deployment numerical root mismatch: actual %s expected %s"
      actual expected
  | Capability_set_root_mismatch (actual, expected) ->
    Printf.sprintf
      "model deployment capability set root mismatch: actual %s expected %s"
      actual expected
