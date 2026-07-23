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
  admitted : Admission.t;
  target : Inference_target.t;
  request : Inference_request.t;
  model : Inference_model.t;
  deployment : Inference_model_deployment.t option;
  pins : Inference_store.pin_set;
  input : string;
}

type error =
  | Target_invalid of string
  | Request_invalid of string
  | Model_invalid of string
  | Deployment_invalid of string
  | Pin_model_root_mismatch of string * string
  | Pin_store_root_mismatch of string * string
  | Pin_ranges_root_mismatch of string * string
  | Input_root_mismatch of string * string
  | Input_limit_exceeded of int * int
  | Missing_requirement

let sha256 raw =
  Digestif.SHA256.(digest_string raw |> to_hex)

let check_deployment ~target ~requirement = function
  | None -> Ok ()
  | Some deployment ->
    (match
       Inference_model_deployment.check
         ~target
         ~requirement
         deployment
     with
     | Ok () -> Ok ()
     | Error error ->
       Error
         (Deployment_invalid
            (Inference_model_deployment.error_message error)))

let create_internal ~deployment ~admitted ~target ~request ~model ~pins ~input =
  match Admission.requirement admitted with
  | None -> Error Missing_requirement
  | Some requirement ->
    (match Inference_target.check ~admitted target with
     | Error error -> Error (Target_invalid (Inference_target.error_message error))
     | Ok () ->
       (match Inference_request.check ~target ~requirement request with
        | Error error -> Error (Request_invalid (Inference_request.error_message error))
        | Ok () ->
          (match Inference_model.check ~target model with
           | Error error -> Error (Model_invalid (Inference_model.error_message error))
           | Ok () ->
             (match check_deployment ~target ~requirement deployment with
              | Error error -> Error error
              | Ok () ->
                if not
                    (String.equal
                       (Inference_store.model_root pins)
                       target.model_root)
                then
                  Error
                    (Pin_model_root_mismatch
                       (Inference_store.model_root pins, target.model_root))
                else if not
                    (String.equal
                       (Inference_store.store_root pins)
                       target.store_root)
                then
                  Error
                    (Pin_store_root_mismatch
                       (Inference_store.store_root pins, target.store_root))
                else
                  let expected_ranges_root = Inference_model.root model in
                  if not
                      (String.equal
                         (Inference_store.model_ranges_root pins)
                         expected_ranges_root)
                  then
                    Error
                      (Pin_ranges_root_mismatch
                         (Inference_store.model_ranges_root pins,
                          expected_ranges_root))
                  else
                    let actual_input_root = sha256 input in
                    if not (String.equal actual_input_root request.input_root) then
                      Error
                        (Input_root_mismatch
                           (request.input_root, actual_input_root))
                    else if String.length input > requirement.limits.max_view_bytes then
                      Error
                        (Input_limit_exceeded
                           (String.length input,
                            requirement.limits.max_view_bytes))
                    else
                      Ok {
                        admitted;
                        target;
                        request;
                        model;
                        deployment;
                        pins;
                        input;
                      }))))

let admitted plan = plan.admitted
let target plan = plan.target
let request plan = plan.request
let model plan = plan.model
let deployment plan = plan.deployment
let pins plan = plan.pins
let input plan = plan.input
let model_ranges_root plan = Inference_store.model_ranges_root plan.pins
let model_deployment_root plan =
  Option.map Inference_model_deployment.root plan.deployment

let create =
  create_internal ~deployment:None

let create_with_deployment ~deployment =
  create_internal ~deployment:(Some deployment)

let requirement plan =
  match Admission.requirement plan.admitted with
  | Some requirement -> requirement
  | None -> invalid_arg "inference plan missing requirement"

let limits plan =
  (requirement plan).Execution_requirement.limits

let error_message = function
  | Target_invalid error -> "invalid inference target: " ^ error
  | Request_invalid error -> "invalid inference request: " ^ error
  | Model_invalid error -> "invalid inference model: " ^ error
  | Deployment_invalid error -> "invalid inference model deployment: " ^ error
  | Pin_model_root_mismatch (actual, expected) ->
    Printf.sprintf
      "inference plan model root mismatch: actual %s expected %s"
      actual expected
  | Pin_store_root_mismatch (actual, expected) ->
    Printf.sprintf
      "inference plan store root mismatch: actual %s expected %s"
      actual expected
  | Pin_ranges_root_mismatch (actual, expected) ->
    Printf.sprintf
      "inference plan model ranges root mismatch: actual %s expected %s"
      actual expected
  | Input_root_mismatch (expected, actual) ->
    Printf.sprintf
      "inference plan input root mismatch: expected %s actual %s"
      expected actual
  | Input_limit_exceeded (required, available) ->
    Printf.sprintf
      "inference plan input limit exceeded: required %d available %d"
      required available
  | Missing_requirement -> "admitted program has no execution requirement"
