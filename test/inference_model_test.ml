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
module Abi = Octra_vm.Inference_session_abi
module Target = Octra_vm.Inference_target

let check label condition =
  if not condition then failwith label

let hex_root char =
  String.make 64 char

let target =
  Target.{
    program_root = hex_root 'a';
    requirement_root = hex_root 'b';
    model_root = hex_root 'c';
    execution_descriptor_root = hex_root 'd';
    store_root = hex_root 'e';
    session_abi_root = Abi.v1_root;
    entrypoints = [];
  }

let range owner_root encoding =
  Model.{
    owner_root;
    offset = 0;
    length = 32;
    encoding;
    shape_root = Some (hex_root '1');
  }

let model =
  Model.{
    model_root = target.model_root;
    store_root = target.store_root;
    ranges = [
      range (hex_root '2') "octets";
      range (hex_root '3') "tensor.fixed";
    ];
  }

let check_root_order () =
  let left = Model.root model in
  let right =
    Model.root Model.{ model with ranges = List.rev model.ranges }
  in
  check "model ranges root sorts ranges" (String.equal left right)

let check_supported () =
  match Model.check ~target model with
  | Ok () -> ()
  | Error error -> failwith (Model.error_message error)

let check_model_root_mismatch () =
  let model = Model.{ model with model_root = hex_root '4' } in
  match Model.check ~target model with
  | Error (Model.Model_root_mismatch (_, _)) -> ()
  | _ -> failwith "expected model root mismatch"

let check_store_root_mismatch () =
  let model = Model.{ model with store_root = hex_root '5' } in
  match Model.check ~target model with
  | Error (Model.Store_root_mismatch (_, _)) -> ()
  | _ -> failwith "expected store root mismatch"

let check_empty_ranges () =
  let model = Model.{ model with ranges = [] } in
  match Model.check ~target model with
  | Error Model.Empty_ranges -> ()
  | _ -> failwith "expected empty ranges"

let check_duplicate_range () =
  let range = range (hex_root '2') "octets" in
  let model = Model.{ model with ranges = [range; range] } in
  match Model.check ~target model with
  | Error (Model.Duplicate_range _) -> ()
  | _ -> failwith "expected duplicate range"

let check_bad_owner_root () =
  let range = Model.{ (range (hex_root '2') "octets") with owner_root = "bad" } in
  let model = Model.{ model with ranges = [range] } in
  match Model.check ~target model with
  | Error (Model.Bad_root "bad") -> ()
  | _ -> failwith "expected bad owner root"

let check_bad_encoding () =
  let range = range (hex_root '2') "Octets" in
  let model = Model.{ model with ranges = [range] } in
  match Model.check ~target model with
  | Error (Model.Bad_name "Octets") -> ()
  | _ -> failwith "expected bad encoding"

let check_bad_length () =
  let range = Model.{ (range (hex_root '2') "octets") with length = 0 } in
  let model = Model.{ model with ranges = [range] } in
  match Model.check ~target model with
  | Error (Model.Bad_range ("length", 0)) -> ()
  | _ -> failwith "expected bad length"

let check_range_overflow () =
  let range =
    Model.{ (range (hex_root '2') "octets") with offset = max_int; length = 1 }
  in
  let model = Model.{ model with ranges = [range] } in
  match Model.check ~target model with
  | Error (Model.Range_overflow (_, _)) -> ()
  | _ -> failwith "expected range overflow"

let () =
  check_root_order ();
  check_supported ();
  check_model_root_mismatch ();
  check_store_root_mismatch ();
  check_empty_ranges ();
  check_duplicate_range ();
  check_bad_owner_root ();
  check_bad_encoding ();
  check_bad_length ();
  check_range_overflow ()
