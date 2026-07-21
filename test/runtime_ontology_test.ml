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


let forbidden =
  [
    "Q" ^ "wen";
    "q" ^ "wen";
    "B" ^ "onsai";
    "b" ^ "onsai";
  ]

let code_suffix name =
  String.equal (Filename.basename name) "dune"
  || List.exists
       (fun suffix -> Filename.check_suffix name suffix)
       [".ml"; ".mli"; ".c"; ".h"; ".cpp"; ".hpp"; ".dune"]

let parent dir =
  let next = Filename.dirname dir in
  if String.equal next dir then None else Some next

let rec find_root dir =
  if Sys.file_exists (Filename.concat dir "dune-project") then dir
  else
    match parent dir with
    | Some next -> find_root next
    | None -> failwith "could not find workspace root"

let read_file path =
  let input = open_in_bin path in
  let len = in_channel_length input in
  let data = really_input_string input len in
  close_in input;
  data

let contains haystack needle =
  let haystack_len = String.length haystack in
  let needle_len = String.length needle in
  let rec loop offset =
    offset + needle_len <= haystack_len
    && (String.equal (String.sub haystack offset needle_len) needle
        || loop (offset + 1))
  in
  needle_len = 0 || loop 0

let scan_file path =
  if code_suffix path then
    let data = read_file path in
    List.iter
      (fun needle ->
        if contains data needle then
          failwith ("model-family identifier leaked into runtime code: " ^ path))
      forbidden

let rec scan_tree path =
  if Sys.is_directory path then
    Array.iter
      (fun name ->
        if not (String.equal name "_build") then
          scan_tree (Filename.concat path name))
      (Sys.readdir path)
  else
    scan_file path

let () =
  let root = find_root (Sys.getcwd ()) in
  List.iter
    (fun dir ->
      let path = Filename.concat root dir in
      if Sys.file_exists path then scan_tree path)
    ["lib"; "node_runtime"; "circle_runtime"; "pvac_ffi"]
