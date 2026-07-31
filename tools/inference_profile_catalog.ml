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

module Profile = Octra_vm.Inference_numerical_profile
module Template = Octra_vm.Inference_conformance_template

let requested_opcodes = ref []
let use_p0 = ref false
let use_all = ref false

let fail message =
  prerr_endline message;
  exit 1

let args = [
  "--opcode",
  Arg.String (fun value -> requested_opcodes := value :: !requested_opcodes),
  "include one opcode; may be repeated";
  "--p0",
  Arg.Set use_p0,
  "emit only the P0 determinism opcode profile roots";
  "--all",
  Arg.Set use_all,
  "emit the current inference runtime opcode profile roots";
]

let usage =
  "inference_profile_catalog [--all] [--p0] [--opcode <opcode> ...]"

let unique values =
  List.sort_uniq String.compare values

let opcodes () =
  let values =
    match List.rev !requested_opcodes, !use_p0, !use_all with
    | [], true, _ -> Template.p0_opcodes
    | [], _, _ -> Profile.current_runtime_opcodes
    | values, _, _ -> values
  in
  unique values

let validate_opcodes opcodes =
  List.iter
    (fun opcode ->
       match Profile.current_runtime_profile ~opcode with
       | Some _ -> ()
       | None -> fail ("opcode has no current inference runtime profile: " ^ opcode))
    opcodes

let () =
  Arg.parse args (fun value -> fail ("unexpected argument: " ^ value)) usage;
  let opcodes = opcodes () in
  validate_opcodes opcodes;
  print_endline
    (Yojson.Safe.pretty_to_string
       (Profile.current_runtime_profile_catalog_json ~opcodes))
