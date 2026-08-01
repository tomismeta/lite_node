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
    ignore (string_value "session_abi_catalog_root" fields)
  | _ -> failwith "catalog output must be an object"

let () =
  check_p0_catalog_exports_session_abi_authority ()
