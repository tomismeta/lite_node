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

let gate_for_opcode opcode =
  match Profile.current_runtime_profile ~opcode with
  | None -> fail ("opcode has no current inference runtime profile: " ^ opcode)
  | Some name ->
    (match Profile.of_name name with
     | Error error -> fail (Profile.error_message error)
     | Ok profile -> Profile.to_json_for_opcode ~opcode profile)

let catalog_root json =
  let payload = Yojson.Safe.to_string json in
  Digestif.SHA256.(
    digest_string ("octra:inference:profile-catalog\000" ^ payload) |> to_hex)

let opcodes () =
  let values =
    match List.rev !requested_opcodes, !use_p0, !use_all with
    | [], true, _ -> Template.p0_opcodes
    | [], _, _ -> Profile.current_runtime_opcodes
    | values, _, _ -> values
  in
  unique values

let report opcodes =
  let gates = List.map gate_for_opcode opcodes in
  let status_counts = Profile.status_counts_of_json_gates gates in
  let root_catalog = Profile.profile_root_catalog_json gates in
  let blocker_catalog = Profile.consensus_blocker_catalog_json gates in
  let report =
    `Assoc [
      "schema", `String "octra.inference.profile-catalog.v1";
      "diagnostic_only", `Bool true;
      "profile_source", `String "current_runtime_profile";
      "opcode_count", `Int (List.length opcodes);
      "opcodes", `List (List.map (fun opcode -> `String opcode) opcodes);
      "profile_consensus_status_counts",
      Profile.status_counts_json status_counts;
      "profile_root_catalog", root_catalog;
      "consensus_blocker_catalog", blocker_catalog;
    ]
  in
  match report with
  | `Assoc fields ->
    `Assoc (fields @ ["profile_catalog_root", `String (catalog_root report)])
  | _ -> report

let () =
  Arg.parse args (fun value -> fail ("unexpected argument: " ^ value)) usage;
  print_endline (Yojson.Safe.pretty_to_string (report (opcodes ())))
