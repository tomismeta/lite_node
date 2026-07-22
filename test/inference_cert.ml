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
module Program_effects = Octra_vm.Program_effects
module Program_envelope = Octra_vm.Program_envelope
module Program_type_flow = Octra_vm.Program_type_flow

let sha256 raw =
  Digestif.SHA256.(digest_string raw |> to_hex)

let facts_json =
  `Assoc [
    "root", `List [];
    "entries", `List [];
    "calls", `List [];
    "xcalls", `List [];
  ]

let source code =
  let bytecode = Bytecode.encode code in
  let effects =
    Program_effects.scan code
    |> Program_effects.names
    |> List.map (fun name -> `String name)
  in
  let cert =
    Yojson.Safe.to_string (`Assoc [
      "schema", `String "aml_bytecode_certificate_v2";
      "compiler", `String "octra_aml";
      "compiler_version", `String "test";
      "declaration", `String "program";
      "source_mode", `String "single";
      "source_hash", `String (sha256 "test source");
      "bytecode_hash", `String (sha256 bytecode);
      "verification_hash", `String (sha256 "test verification");
      "effects", `List effects;
      "facts_hash", `String (Program_type_flow.facts_hash Program_type_flow.empty_facts);
      "facts", facts_json;
    ])
  in
  match Program_envelope.encode ~code:bytecode ~cert with
  | Ok raw -> raw
  | Error error ->
    failwith (Program_envelope.error_message error)

let admit ~support ~requirement code =
  match
    Admission.decode_inference_program_source
      ~support
      ~requirement
      (source code)
  with
  | Ok admitted -> admitted
  | Error error -> failwith (Admission.error_message error)
