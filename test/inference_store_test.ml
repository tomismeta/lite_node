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


module Model = Octra_vm.Inference_model
module Req = Octra_vm.Execution_requirement
module Store = Octra_vm.Inference_store

let check label condition =
  if not condition then failwith label

let hex_root char =
  String.make 64 char

let sha256 raw =
  Digestif.SHA256.(digest_string raw |> to_hex)

let limits max_model_bytes =
  Req.{
    max_model_bytes;
    max_view_bytes = 0;
    max_session_bytes = 0;
    max_scratch_bytes = 0;
    max_output_bytes = 0;
    max_advance_effort = 0;
  }

let owner = "authenticated owner bytes"
let owner_root = sha256 owner

let model =
  Model.{
    model_root = hex_root 'a';
    store_root = hex_root 'b';
    ranges = [
      {
        owner_root;
        offset = 0;
        length = 13;
        encoding = "octets";
        shape_root = None;
      };
    ];
  }

let read root =
  if String.equal root owner_root then Some owner else None

let check_pin () =
  match Store.pin ~limits:(limits 32) ~read model with
  | Error error -> failwith (Store.error_message error)
  | Ok pins ->
    check "one range pinned" (List.length pins.ranges = 1);
    match pins.ranges with
    | [range] -> check "range bytes" (String.equal range.bytes "authenticated")
    | _ -> failwith "unexpected ranges"

let check_missing_owner () =
  match Store.pin ~limits:(limits 32) ~read:(fun _ -> None) model with
  | Error (Store.Missing_owner _) -> ()
  | _ -> failwith "expected missing owner"

let check_owner_mismatch () =
  let read _ = Some "wrong" in
  match Store.pin ~limits:(limits 32) ~read model with
  | Error (Store.Owner_root_mismatch (_, _)) -> ()
  | _ -> failwith "expected owner root mismatch"

let check_bounds () =
  let bad_range =
    Model.{ (List.hd model.ranges) with offset = String.length owner; length = 1 }
  in
  let model = Model.{ model with ranges = [bad_range] } in
  match Store.pin ~limits:(limits 32) ~read model with
  | Error (Store.Range_out_of_bounds (_, _, _, _)) -> ()
  | _ -> failwith "expected out-of-bounds range"

let check_limit () =
  match Store.pin ~limits:(limits 4) ~read model with
  | Error (Store.Model_limit_exceeded (_, _)) -> ()
  | _ -> failwith "expected model limit exceeded"

let () =
  check_pin ();
  check_missing_owner ();
  check_owner_mismatch ();
  check_bounds ();
  check_limit ()
