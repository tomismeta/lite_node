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

module Abi = Octra_vm.Inference_session_abi
module Profile = Octra_vm.Inference_numerical_profile

let check label condition =
  if not condition then failwith label

let read_all input =
  let buffer = Buffer.create 4096 in
  (try
     while true do
       Buffer.add_string buffer (input_line input);
       Buffer.add_char buffer '\n'
     done
   with End_of_file -> ());
  Buffer.contents buffer

let tool_path () =
  let candidates =
    [
      "_build/default/tools/inference_profile_catalog.exe";
      "../tools/inference_profile_catalog.exe";
    ]
  in
  match List.find_opt Sys.file_exists candidates with
  | Some path -> path
  | None -> failwith "missing inference_profile_catalog.exe"

let field name fields =
  match List.assoc_opt name fields with
  | Some value -> value
  | None -> failwith ("missing json field: " ^ name)

let string_value name fields =
  match field name fields with
  | `String value -> value
  | _ -> failwith ("json field must be a string: " ^ name)

let list_value name fields =
  match field name fields with
  | `List values -> values
  | _ -> failwith ("json field must be a list: " ^ name)

let assoc_value name fields =
  match field name fields with
  | `Assoc fields -> fields
  | _ -> failwith ("json field must be an object: " ^ name)

let catalog_entry_root = function
  | `Assoc fields ->
    Some (string_value "name" fields, string_value "session_abi_root" fields)
  | _ -> None

let check_p0_catalog_exports_session_abi_authority () =
  let command = tool_path () ^ " --p0" in
  let input = Unix.open_process_in command in
  let raw = read_all input in
  let status = Unix.close_process_in input in
  check "catalog command exits" (status = Unix.WEXITED 0);
  match Yojson.Safe.from_string raw with
  | `Assoc fields ->
    let entries =
      list_value "session_abi_root_catalog" fields
      |> List.filter_map catalog_entry_root
    in
    let root name =
      match List.assoc_opt name entries with
      | Some value -> value
      | None -> failwith ("missing session ABI entry: " ^ name)
    in
    check "v1 root" (String.equal (root "v1") Abi.v1_root);
    check "v2 root" (String.equal (root "v2") Abi.v2_root);
    check
      "committed-state root"
      (String.equal (root "committed-state") Abi.committed_state_root);
    ignore (string_value "session_abi_catalog_root" fields);
    check
      "resident lifecycle root"
      (String.equal
         (string_value "resident_session_lifecycle_root" fields)
         Abi.resident_lifecycle_root);
    let lifecycle =
      assoc_value "resident_session_lifecycle" fields
    in
    check
      "resident lifecycle schema"
      (String.equal
         (string_value "schema" lifecycle)
         Abi.resident_lifecycle_schema);
    check
      "resident lifecycle binds product phases"
      (list_value "product_lifecycle" lifecycle
       = [
           `String "open_session";
           `String "prefill";
           `String "decode";
           `String "finalize";
         ]);
    let abi_roots =
      assoc_value "referenced_session_abi_roots" lifecycle
    in
    check
      "resident lifecycle references v2"
      (String.equal
         (string_value "progress_cells" abi_roots)
         Abi.v2_root);
    check
      "resident lifecycle references committed state"
      (String.equal
         (string_value "committed_state_transport" abi_roots)
         Abi.committed_state_root)
  | _ -> failwith "catalog output must be an object"

let dependency_entry opcode = function
  | `Assoc fields ->
    (match List.assoc_opt "opcode" fields with
     | Some (`String candidate) when String.equal candidate opcode ->
       Some fields
     | _ -> None)
  | _ -> None

let check_p0_catalog_exports_transcendental_dependencies () =
  let command = tool_path () ^ " --p0" in
  let input = Unix.open_process_in command in
  let raw = read_all input in
  let status = Unix.close_process_in input in
  check "catalog command exits" (status = Unix.WEXITED 0);
  match Yojson.Safe.from_string raw with
  | `Assoc fields ->
    let catalog = assoc_value "transcendental_dependency_catalog" fields in
    check
      "dependency catalog schema"
      (String.equal
         (string_value "schema" catalog)
         "octra.inference.transcendental-dependency-catalog.v1");
    check
      "dependency catalog source"
      (String.equal
         (string_value "source" catalog)
         "litenode_runtime_profile");
    check
      "dependency catalog has no validator authority"
      (String.equal (string_value "authority" catalog) "none");
    check
      "dependency catalog root"
      (String.equal
         (string_value "transcendental_dependency_catalog_root" fields)
         (Profile.transcendental_dependency_catalog_root (`Assoc catalog)));
    let entries = list_value "entries" catalog in
    let host_gate = assoc_value "host_native_math_gate" catalog in
    check "p0 dependency entries retired" (entries = []);
    check
      "host native math gate accepted"
      (String.equal (string_value "status" host_gate) "accepted");
    check
      "host native math gate does not block consensus"
      (String.equal
         (string_value "consensus_admission_status" host_gate)
         "not_blocked");
    check
      "host native math gate next action"
      (String.equal
         (string_value "next_action" host_gate)
         "continue_validator_readiness");
    check
      "host native math gate has no blocked opcodes"
      (list_value "blocked_opcodes" host_gate = []);
    check
      "host native math gate has no blockers"
      (list_value "validator_admission_blockers" host_gate = []);
    check
      "softmax retired from host dependency catalog"
      (List.find_map (dependency_entry "SOFTMAX_FP") entries = None);
    check
      "gated delta retired from host dependency catalog"
      (List.find_map (dependency_entry "GATED_DELTA_RULE_FP") entries = None)
  | _ -> failwith "catalog output must be an object"

let int_value name fields =
  match field name fields with
  | `Int value -> value
  | _ -> failwith ("json field must be an int: " ^ name)

let sorted_dependency_opcodes entries =
  entries
  |> List.filter_map (function
    | `Assoc fields ->
      (match List.assoc_opt "opcode" fields with
       | Some (`String opcode) -> Some opcode
       | _ -> None)
    | _ -> None)
  |> List.sort_uniq String.compare

let check_all_catalog_exports_complete_transcendental_inventory () =
  let command = tool_path () ^ " --all" in
  let input = Unix.open_process_in command in
  let raw = read_all input in
  let status = Unix.close_process_in input in
  check "catalog command exits" (status = Unix.WEXITED 0);
  match Yojson.Safe.from_string raw with
  | `Assoc fields ->
    let catalog = assoc_value "transcendental_dependency_catalog" fields in
    let entries = list_value "entries" catalog in
    check "entry count" (int_value "entry_count" catalog = 0);
    check "dependency count" (int_value "dependency_count" catalog = 0);
    check
      "dependency catalog root"
      (String.equal
         (string_value "transcendental_dependency_catalog_root" fields)
         (Profile.transcendental_dependency_catalog_root (`Assoc catalog)));
    check
      "dependency opcodes retired"
      (sorted_dependency_opcodes entries = []);
    List.iter
      (function
        | `Assoc entry_fields ->
          check
            "no metadata-missing dependency entries"
            (not
               (String.equal
                  (string_value "status" entry_fields)
                  "metadata_missing"))
        | _ -> failwith "dependency entry must be an object")
      entries
  | _ -> failwith "catalog output must be an object"

let () =
  check_p0_catalog_exports_session_abi_authority ();
  check_p0_catalog_exports_transcendental_dependencies ();
  check_all_catalog_exports_complete_transcendental_inventory ()
