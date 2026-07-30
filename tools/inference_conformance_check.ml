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

module Template = Octra_vm.Inference_conformance_template

let template_path = ref None
let template_dir = ref None

let fail message =
  prerr_endline message;
  exit 1

let args = [
  "--template",
  Arg.String (fun value -> template_path := Some value),
  "single conformance template json";
  "--template-dir",
  Arg.String (fun value -> template_dir := Some value),
  "directory containing conformance template json files";
]

let usage =
  "inference_conformance_check --template <path>\n\
   or inference_conformance_check --template-dir <dir>"

let read_json path =
  try Yojson.Safe.from_file path with
  | Sys_error message -> fail message
  | Yojson.Json_error message ->
    fail ("invalid json " ^ path ^ ": " ^ message)

let read_file path =
  let input = open_in_bin path in
  Fun.protect
    ~finally:(fun () -> close_in_noerr input)
    (fun () ->
       let len = in_channel_length input in
       really_input_string input len)

let sha256 raw =
  Digestif.SHA256.(digest_string raw |> to_hex)

type checked_template = {
  path : string;
  template : Template.t;
}

let verify_memory_files template_path template =
  let dir = Filename.dirname template_path in
  List.iter
    (fun (memory : Template.memory_binding) ->
       match memory.path, memory.byte_length, memory.sha256 with
       | Some relative, Some byte_length, Some expected_sha ->
         let path = Filename.concat dir relative in
         let raw =
           try read_file path with
           | Sys_error message -> fail (template_path ^ ": " ^ message)
         in
         if String.length raw <> byte_length then
           fail
             (Printf.sprintf
                "%s: memory %s byte_length expected %d actual %d"
                template_path
                memory.name
                byte_length
                (String.length raw));
         let actual_sha = sha256 raw in
         if not (String.equal actual_sha expected_sha) then
           fail
             (Printf.sprintf
                "%s: memory %s sha256 expected %s actual %s"
                template_path
                memory.name
                expected_sha
                actual_sha)
       | _ -> ())
    template.Template.memory

let check_template path =
  match Template.of_json (read_json path) with
  | Error error -> fail (path ^ ": " ^ Template.error_message error)
  | Ok template ->
    verify_memory_files path template;
    { path; template }

let checked_template_json checked =
    `Assoc [
      "path", `String checked.path;
      "template", Template.to_json checked.template;
    ]

let template_files dir =
  if not (Sys.file_exists dir) then fail ("missing template dir: " ^ dir);
  if not (Sys.is_directory dir) then fail ("not a directory: " ^ dir);
  Sys.readdir dir
  |> Array.to_list
  |> List.filter (fun name ->
    Filename.check_suffix name ".cjson"
    || Filename.check_suffix name ".json")
  |> List.sort String.compare
  |> List.map (Filename.concat dir)

let missing_p0 templates =
  List.filter
    (fun opcode ->
       not
         (List.exists
            (fun checked -> String.equal checked.template.Template.opcode opcode)
            templates))
    Template.p0_opcodes

let duplicate_opcodes templates =
  let seen = Hashtbl.create 8 in
  templates
  |> List.filter_map (fun checked ->
    let opcode = checked.template.Template.opcode in
    if Hashtbl.mem seen opcode then Some opcode
    else begin
      Hashtbl.add seen opcode ();
      None
    end)

let () =
  Arg.parse args (fun arg -> fail ("unexpected argument: " ^ arg)) usage;
  match !template_path, !template_dir with
  | Some _, Some _ -> fail "use --template or --template-dir, not both"
  | None, None -> fail "missing --template or --template-dir"
  | Some path, None ->
    let checked = check_template path in
    print_endline
      (Yojson.Safe.pretty_to_string
         (`Assoc [
           "status", `String "accepted";
           "diagnostic_only", `Bool true;
           "templates", `List [checked_template_json checked];
         ]))
  | None, Some dir ->
    let templates = List.map check_template (template_files dir) in
    if templates = [] then fail ("no templates in " ^ dir);
    let missing = missing_p0 templates in
    if missing <> [] then
      fail ("missing P0 templates: " ^ String.concat "," missing);
    let duplicates = duplicate_opcodes templates in
    if duplicates <> [] then
      fail ("duplicate P0 templates: " ^ String.concat "," duplicates);
    print_endline
      (Yojson.Safe.pretty_to_string
         (`Assoc [
           "status", `String "accepted";
           "diagnostic_only", `Bool true;
           "template_count", `Int (List.length templates);
           "p0_opcodes",
           `List (List.map (fun opcode -> `String opcode) Template.p0_opcodes);
           "templates", `List (List.map checked_template_json templates);
         ]))
