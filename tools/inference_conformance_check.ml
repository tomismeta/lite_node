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
module Profile = Octra_vm.Inference_numerical_profile

let template_path = ref None
let template_dir = ref None
let template_index = ref None
let require_consensus_ready = ref false

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
  "--template-index",
  Arg.String (fun value -> template_index := Some value),
  "producer template index json";
  "--require-consensus-ready",
  Arg.Set require_consensus_ready,
  "reject reports whose profile gates are not all consensus_ready";
]

let usage =
  "inference_conformance_check --template <path> [--require-consensus-ready]\n\
   or inference_conformance_check --template-dir <dir> [--require-consensus-ready]\n\
   or inference_conformance_check --template-index <path> [--require-consensus-ready]"

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

let field name fields =
  match List.filter (fun (key, _) -> String.equal key name) fields with
  | [(_, value)] -> Some value
  | _ -> None

let string_field name fields =
  match field name fields with
  | Some (`String value) -> Some value
  | _ -> None

let int_field name fields =
  match field name fields with
  | Some (`Int value) -> Some value
  | _ -> None

let list_field name fields =
  match field name fields with
  | Some (`List values) -> Some values
  | _ -> None

let assoc_field name fields =
  match field name fields with
  | Some (`Assoc values) -> Some values
  | _ -> None

let relative_path_ok path =
  String.length path > 0
  && path.[0] <> '/'
  && not (String.contains path '\\')
  && List.for_all
       (fun part -> part <> "" && part <> "." && part <> "..")
       (String.split_on_char '/' path)

let hex_char = function
  | '0' .. '9'
  | 'a' .. 'f'
  | 'A' .. 'F' -> true
  | _ -> false

let root_ok value =
  String.length value = 64
  && String.for_all hex_char value

type issue = {
  path : string;
  opcode : string option;
  message : string;
}

let issue ?opcode path message = { path; opcode; message }

let issue_json issue =
  `Assoc [
    "path", `String issue.path;
    "opcode",
    (match issue.opcode with None -> `Null | Some opcode -> `String opcode);
    "message", `String issue.message;
  ]

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

let profile_gate_present = function
  | `Assoc fields ->
    (match List.assoc_opt "profile_gate" fields with
     | Some `Null
     | None -> false
     | Some _ -> true)
  | _ -> false

let profile_gate_value = function
  | `Assoc fields ->
    (match List.assoc_opt "profile_gate" fields with
     | Some (`Assoc _ as gate) -> Some gate
     | _ -> None)
  | _ -> None

let profile_status_counts values =
  values
  |> List.filter_map profile_gate_value
  |> Profile.status_counts_of_json_gates

let profile_root_binding_value value =
  match value with
  | `Assoc fields ->
    if profile_gate_present value then
      match List.assoc_opt "profile_root_binding" fields with
      | Some (`Assoc _ as binding) -> Some binding
      | _ -> Some Profile.unavailable_root_binding_json
    else
      None
  | _ -> None

let profile_root_binding_status_counts values =
  values
  |> List.filter_map profile_root_binding_value
  |> Profile.root_binding_counts_of_json

let profile_root_binding_classification_counts values =
  values
  |> List.filter_map profile_root_binding_value
  |> Profile.root_binding_classification_counts_of_json

let consensus_ready_gate
    ~profile_gate_count
    ~unprofiled_count
    ~root_binding_counts
    status_counts =
  let ready =
    Profile.consensus_ready
      ~profile_gate_count
      ~unprofiled_count
      status_counts
    && Profile.root_bindings_are_consensus_ready root_binding_counts
  in
  `Assoc [
    "required", `Bool !require_consensus_ready;
    "status",
    `String
      (if not !require_consensus_ready then "not_required"
       else if ready then "accepted"
       else "rejected");
    "blockers",
    `List
      (List.map
         (fun blocker -> `String blocker)
         (Profile.consensus_ready_blockers
            ~profile_gate_count
            ~unprofiled_count
            status_counts
          @ Profile.root_binding_blockers root_binding_counts));
  ]

let consensus_ready_required_passes
    ~profile_gate_count
    ~unprofiled_count
    ~root_binding_counts
    status_counts =
  (not !require_consensus_ready)
  ||
  (Profile.consensus_ready
     ~profile_gate_count
     ~unprofiled_count
     status_counts
   && Profile.root_bindings_are_consensus_ready root_binding_counts)

let template_profile_gate_present (checked : checked_template) =
  profile_gate_present (Template.to_json checked.template)

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

let resolve_template_path index_path value =
  if relative_path_ok value then
    Filename.concat (Filename.dirname index_path) value
  else value

let read_template_for_diagnostics path =
  try Some (read_json path) with
  | _ -> None

let issues_for_program_effects path opcode fields =
  match assoc_field "program_effect_requirements" fields with
  | None -> [issue ~opcode path "missing program_effect_requirements"]
  | Some effect_fields ->
    let effects =
      match list_field "program_effects" effect_fields with
      | Some values ->
        List.filter_map
          (function `String value -> Some value | _ -> None)
          values
      | None -> []
    in
    ["memory_read"; "memory_write"; "storage_read"]
    |> List.filter
         (fun effect -> not (List.exists (String.equal effect) effects))
    |> List.map (fun effect ->
      issue ~opcode path ("missing program effect: " ^ effect))

let source_path_issues path opcode fields =
  match list_field "input_memory_ranges" fields with
  | None -> [issue ~opcode path "missing input_memory_ranges"]
  | Some ranges ->
    List.concat
      (List.map
         (function
           | `Assoc range_fields ->
             let name =
               match string_field "name" range_fields with
               | Some value -> value
               | None -> "<unknown>"
             in
             let range_issues =
               match assoc_field "source" range_fields with
               | None -> [issue ~opcode path ("missing source for input " ^ name)]
               | Some source_fields ->
                 let base =
                   match string_field "path" source_fields with
                   | Some source when not (relative_path_ok source) ->
                     [issue ~opcode path
                        ("source path must be relative for input " ^ name)]
                   | Some _ -> []
                   | None ->
                     [issue ~opcode path ("missing source path for input " ^ name)]
                 in
                 let bytes_issue =
                   match int_field "bytes" source_fields with
                   | Some bytes when bytes > 0 -> []
                   | _ -> [issue ~opcode path
                             ("source bytes must be positive for input " ^ name)]
                 in
                 let sha_issue =
                   match string_field "sha256" source_fields with
                   | Some _ -> []
                   | None -> [issue ~opcode path
                                ("source sha256 is required for input " ^ name)]
                 in
                 base @ bytes_issue @ sha_issue
             in
             let vm_issues =
               match assoc_field "vm_memory" range_fields with
               | None -> [issue ~opcode path ("missing vm_memory for input " ^ name)]
               | Some vm_fields ->
                 let f64_input =
                   match assoc_field "range_binding" range_fields with
                   | Some binding_fields ->
                     (match string_field "encoding" binding_fields with
                      | Some "f64le" -> true
                      | _ -> false)
                   | None -> false
                 in
                 let base_issue =
                   match int_field "base_address" vm_fields with
                   | Some base when base >= 0 -> []
                   | _ -> [issue ~opcode path
                             ("vm_memory base_address required for input " ^ name)]
                 in
                 let count_issue =
                   match int_field "length_f64_cells" vm_fields with
                   | Some cells when cells > 0 -> []
                   | _ when not f64_input -> []
                   | _ -> [issue ~opcode path
                             ("vm_memory length_f64_cells required for input " ^ name)]
                 in
                 base_issue @ count_issue
             in
             range_issues @ vm_issues
           | _ -> [issue ~opcode path "input_memory_ranges entry must be object"])
         ranges)

let expected_issues path opcode fields =
  let manifest_issues =
    match list_field "expected_output_byte_manifests" fields with
    | None -> [issue ~opcode path "missing expected_output_byte_manifests"]
    | Some manifests ->
      if manifests = [] then
        [issue ~opcode path "expected_output_byte_manifests must be non-empty"]
      else
        List.concat
          (List.map
             (function
               | `Assoc manifest_fields ->
                 let name =
                   match string_field "name" manifest_fields with
                   | Some value -> value
                   | None -> "<unknown>"
                 in
                 let path_issue =
                   match string_field "path" manifest_fields with
                   | Some value when not (relative_path_ok value) ->
                     [issue ~opcode path
                        ("expected output path must be relative for " ^ name)]
                   | Some _ -> []
                   | None ->
                     [issue ~opcode path
                        ("expected output path required for " ^ name)]
                 in
                 let bytes_issue =
                   match int_field "bytes" manifest_fields with
                   | Some bytes when bytes > 0 -> []
                   | _ -> [issue ~opcode path
                             ("expected output bytes must be positive for " ^ name)]
                 in
                 let sha_issue =
                   match string_field "sha256" manifest_fields with
                   | Some _ -> []
                   | None -> [issue ~opcode path
                                ("expected output sha256 required for " ^ name)]
                 in
                 path_issue @ bytes_issue @ sha_issue
               | _ -> [issue ~opcode path
                         "expected_output_byte_manifests entry must be object"])
             manifests)
  in
  let span_issues =
    match assoc_field "output" fields with
    | None -> [issue ~opcode path "missing output"]
    | Some output_fields ->
      let subspans =
        match list_field "subspans" output_fields with
        | Some values -> values
        | None -> []
      in
      if String.equal opcode "GATED_DELTA_RULE_FP" && List.length subspans < 2 then
        [issue ~opcode path
           "GATED_DELTA_RULE_FP requires recurrent output and next-state subspans"]
      else []
  in
  manifest_issues @ span_issues

let failure_issues path opcode fields =
  match list_field "expected_failure_atomicity_behavior" fields with
  | None -> [issue ~opcode path "missing expected_failure_atomicity_behavior"]
  | Some failures ->
    if failures = [] then
      [issue ~opcode path "expected_failure_atomicity_behavior must be non-empty"]
    else
      let parsed =
        List.map
          (function
            | `Assoc failure_fields ->
              let case =
                match string_field "case" failure_fields with
                | Some value -> value
                | None -> "<unknown>"
              in
              `Case
                (case,
                 field "mutations" failure_fields <> None,
                 field "unchanged_spans" failure_fields <> None)
            | _ -> `Bad)
          failures
      in
      let bad =
        if List.exists (( = ) `Bad) parsed then
          [issue ~opcode path
             "expected_failure_atomicity_behavior entries must be objects"]
        else []
      in
      let missing_mutations =
        List.filter_map
          (function
            | `Case (case, false, _) -> Some case
            | _ -> None)
          parsed
      in
      let missing_unchanged =
        List.filter_map
          (function
            | `Case (case, _, false) -> Some case
            | _ -> None)
          parsed
      in
      let mutation_issue =
        if missing_mutations = [] then []
        else
          [issue ~opcode path
             ("failure cases lack executable mutations: "
              ^ String.concat "," missing_mutations)]
      in
      let unchanged_issue =
        if missing_unchanged = [] then []
        else
          [issue ~opcode path
             ("failure cases lack unchanged_spans: "
              ^ String.concat "," missing_unchanged)]
      in
      bad @ mutation_issue @ unchanged_issue

let gated_delta_semantic_issues path opcode fields =
  if not (String.equal opcode "GATED_DELTA_RULE_FP") then []
  else
    match assoc_field "parameter_addresses_and_scalar_params" fields with
    | None -> [issue ~opcode path "missing Gated Delta parameter metadata"]
    | Some params ->
      (match assoc_field "source_parameters" params with
       | None -> [issue ~opcode path "missing Gated Delta source_parameters"]
       | Some source ->
         match string_field "operation_order" source with
         | Some order
           when String.contains order 'v'
                && String.contains order '*'
                && not (String.contains order '-') ->
           [issue ~opcode path
              "Gated Delta operation_order still describes beta*v*k update, not LiteNode memory-correction recurrence"]
         | Some _ -> []
         | None -> [issue ~opcode path
                     "missing Gated Delta operation_order"])

let profile_gate_result opcode fields =
  let add_source source = function
    | `Assoc gate -> `Assoc (gate @ ["profile_source", `String source])
    | value -> value
  in
  match string_field "profile" fields with
  | Some profile ->
    (match Profile.validate_for_opcode ~opcode ~profile with
     | Ok gate ->
       Ok
         (Profile.to_json_for_opcode ~opcode gate
          |> add_source "template")
     | Error error ->
       Error (Profile.error_message error))
  | None ->
    (match Profile.current_runtime_profile ~opcode with
     | None -> Ok `Null
     | Some profile ->
       (match Profile.validate_for_opcode ~opcode ~profile with
        | Ok gate ->
          Ok
            (Profile.to_json_for_opcode ~opcode gate
             |> add_source "current_runtime_profile")
        | Error error ->
          Error (Profile.error_message error)))

let profile_issues path opcode fields =
  match profile_gate_result opcode fields with
  | Ok _ -> []
  | Error message -> [issue ~opcode path message]

let profile_gate_entry path opcode fields =
  match profile_gate_result opcode fields with
  | Ok profile_gate ->
    let profile_root_binding =
      match string_field "numerical_profile_root" fields with
      | Some numerical_profile_root ->
        Profile.root_binding_json ~numerical_profile_root profile_gate
      | None -> `Null
    in
    Some
      (`Assoc [
        "path", `String path;
        "opcode", `String opcode;
        "profile_gate", profile_gate;
        "profile_root_binding", profile_root_binding;
      ])
  | Error _ -> None

let template_identity_issues path opcode primitive fields =
  let opcode_issues =
    match string_field "opcode" fields with
    | Some value when not (String.equal value opcode) ->
      [issue ~opcode path ("template opcode mismatch: " ^ value)]
    | _ -> []
  in
  let primitive_issues =
    match primitive, string_field "primitive" fields with
    | Some expected, Some value when not (String.equal value expected) ->
      [issue ~opcode path ("template primitive mismatch: " ^ value)]
    | _ -> []
  in
  opcode_issues @ primitive_issues

let producer_template_issues path opcode primitive json =
  match json with
  | `Assoc fields ->
    let type_issue =
      match string_field "type" fields with
      | Some "p0_litenode_vm_execution_template" -> []
      | Some value -> [issue ~opcode path ("unexpected template type: " ^ value)]
      | None -> [issue ~opcode path "missing template type"]
    in
    let root_issues =
      let vm =
        match string_field "vm_semantics_root" fields with
        | Some value when root_ok value -> []
        | Some _ -> [issue ~opcode path
                       "vm_semantics_root must be a 32-byte hex root"]
        | None -> [issue ~opcode path "missing vm_semantics_root"]
      in
      let numerical =
        match string_field "numerical_profile_root" fields with
        | Some value when root_ok value -> []
        | Some _ -> [issue ~opcode path
                       "numerical_profile_root must be a 32-byte hex root"]
        | None -> [issue ~opcode path "missing numerical_profile_root"]
      in
      let effort =
        match int_field "expected_effort" fields with
        | Some value when value >= 0 -> []
        | _ -> [issue ~opcode path "missing expected_effort"]
      in
      vm @ numerical @ effort
    in
    type_issue
    @ template_identity_issues path opcode primitive fields
    @ root_issues
    @ profile_issues path opcode fields
    @ issues_for_program_effects path opcode fields
    @ source_path_issues path opcode fields
    @ expected_issues path opcode fields
    @ failure_issues path opcode fields
    @ gated_delta_semantic_issues path opcode fields
  | _ -> [issue ~opcode path "template must be an object"]

let producer_index_report index_path =
  let index = read_json index_path in
  match index with
  | `Assoc fields ->
    let templates =
      match list_field "templates" fields with
      | Some values -> values
      | None -> fail (index_path ^ ": missing templates")
    in
    let entries =
      List.map
        (function
          | `Assoc entry_fields ->
            let opcode = string_field "opcode" entry_fields in
            let primitive = string_field "primitive" entry_fields in
            let template_path = string_field "vm_execution_template" entry_fields in
            let path_issues =
              match template_path with
              | Some path when not (relative_path_ok path) ->
                [issue ?opcode index_path "vm_execution_template path must be relative"]
              | Some _ -> []
              | None -> [issue ?opcode index_path "missing vm_execution_template"]
            in
            let template_issues, profile_gate =
              match template_path with
              | None -> [], None
              | Some raw_path ->
                let resolved = resolve_template_path index_path raw_path in
                (match read_template_for_diagnostics resolved with
                 | None ->
                   [issue ?opcode resolved "template file is unreadable"], None
                 | Some json ->
                   let opcode_value =
                     match opcode with Some value -> value | None -> "<unknown>"
                   in
                   let issues =
                     producer_template_issues resolved opcode_value primitive json
                   in
                   let profile_gate =
                     match opcode, json with
                     | Some opcode, `Assoc fields ->
                       profile_gate_entry resolved opcode fields
                     | _ -> None
                   in
                   issues, profile_gate)
            in
            opcode, path_issues @ template_issues, profile_gate
          | _ ->
            None, [issue index_path "template index entry must be object"], None)
        templates
    in
    let opcodes = List.filter_map (fun (opcode, _, _) -> opcode) entries in
    let profile_gates =
      List.filter_map (fun (_, _, profile_gate) -> profile_gate) entries
    in
    let profile_gate_count =
      List.length (List.filter profile_gate_present profile_gates)
    in
    let unprofiled_template_count =
      List.length templates - profile_gate_count
    in
    let profile_status_counts = profile_status_counts profile_gates in
    let profile_root_binding_counts =
      profile_root_binding_status_counts profile_gates
    in
    let profile_root_binding_classification_counts =
      profile_root_binding_classification_counts profile_gates
    in
    let classified_profile_gate_count =
      Profile.classified_gate_count profile_status_counts
    in
    let missing =
      Template.p0_opcodes
      |> List.filter
           (fun opcode -> not (List.exists (String.equal opcode) opcodes))
      |> List.map (fun opcode -> issue ~opcode index_path "missing P0 template")
    in
    let seen = Hashtbl.create 8 in
    let duplicates =
      opcodes
      |> List.filter_map (fun opcode ->
        if Hashtbl.mem seen opcode then Some (issue ~opcode index_path "duplicate P0 template")
        else begin Hashtbl.add seen opcode (); None end)
    in
    let issues =
      List.concat (List.map (fun (_, issues, _) -> issues) entries)
      @ missing
      @ duplicates
    in
    let schema_status = if issues = [] then "accepted" else "rejected" in
    let status =
      if
        String.equal schema_status "accepted"
        && consensus_ready_required_passes
             ~profile_gate_count
             ~unprofiled_count:unprofiled_template_count
             ~root_binding_counts:profile_root_binding_counts
             profile_status_counts
      then "accepted"
      else "rejected"
    in
    `Assoc [
      "status", `String status;
      "schema_status", `String schema_status;
      "diagnostic_only", `Bool true;
      "index_path", `String index_path;
      "template_count", `Int (List.length templates);
      "p0_opcodes",
      `List (List.map (fun opcode -> `String opcode) Template.p0_opcodes);
      "profile_gates", `List profile_gates;
      "profile_gate_count", `Int profile_gate_count;
      "classified_profile_gate_count", `Int classified_profile_gate_count;
      "unprofiled_template_count", `Int unprofiled_template_count;
      "profile_consensus_status_counts",
      Profile.status_counts_json profile_status_counts;
      "profile_root_catalog",
      Profile.profile_root_catalog_json profile_gates;
      "consensus_blocker_catalog",
      Profile.consensus_blocker_catalog_json profile_gates;
      "consensus_blocker_class_counts",
      Profile.consensus_blocker_class_counts_json profile_gates;
      "profile_root_binding_status_counts",
      Profile.root_binding_counts_json profile_root_binding_counts;
      "profile_root_binding_classification_counts",
      Profile.root_binding_classification_counts_json
        profile_root_binding_classification_counts;
      "consensus_ready_gate",
      consensus_ready_gate
        ~profile_gate_count
        ~unprofiled_count:unprofiled_template_count
        ~root_binding_counts:profile_root_binding_counts
        profile_status_counts;
      "issue_count", `Int (List.length issues);
      "issues", `List (List.map issue_json issues);
    ]
  | _ -> fail (index_path ^ ": template index must be an object")

let print_report_and_exit report =
  print_endline (Yojson.Safe.pretty_to_string report);
  match report with
  | `Assoc fields ->
    (match string_field "status" fields with
     | Some "accepted" -> ()
     | _ -> exit 1)
  | _ -> exit 1

let () =
  Arg.parse args (fun arg -> fail ("unexpected argument: " ^ arg)) usage;
  let modes =
    List.filter_map
      Fun.id
      [
        Option.map (fun value -> `Template value) !template_path;
        Option.map (fun value -> `Dir value) !template_dir;
        Option.map (fun value -> `Index value) !template_index;
      ]
  in
  match modes with
  | [] -> fail "missing --template, --template-dir, or --template-index"
  | _ :: _ :: _ -> fail "use only one template input mode"
  | [`Index path] -> print_report_and_exit (producer_index_report path)
  | [`Template path] ->
    let json = read_json path in
    (match json with
     | `Assoc fields ->
       (match string_field "type" fields with
        | Some "p0_litenode_vm_execution_template_index" ->
          print_report_and_exit (producer_index_report path)
        | _ ->
          let checked = check_template path in
          let template_json = Template.to_json checked.template in
          let profile_gate_count =
            if profile_gate_present template_json then 1 else 0
          in
          let profile_status_counts = profile_status_counts [template_json] in
          let profile_root_binding_counts =
            profile_root_binding_status_counts [template_json]
          in
          let profile_root_binding_classification_counts =
            profile_root_binding_classification_counts [template_json]
          in
          let classified_profile_gate_count =
            Profile.classified_gate_count profile_status_counts
          in
          let schema_status = "accepted" in
          let status =
            if
              consensus_ready_required_passes
                ~profile_gate_count
                ~unprofiled_count:(1 - profile_gate_count)
                ~root_binding_counts:profile_root_binding_counts
                profile_status_counts
            then "accepted"
            else "rejected"
          in
          print_report_and_exit
            (`Assoc [
              "status", `String status;
              "schema_status", `String schema_status;
              "diagnostic_only", `Bool true;
              "template_count", `Int 1;
              "profile_gate_count", `Int profile_gate_count;
              "classified_profile_gate_count",
              `Int classified_profile_gate_count;
              "unprofiled_template_count", `Int (1 - profile_gate_count);
              "profile_consensus_status_counts",
              Profile.status_counts_json profile_status_counts;
              "profile_root_catalog",
              Profile.profile_root_catalog_json [template_json];
              "consensus_blocker_catalog",
              Profile.consensus_blocker_catalog_json [template_json];
              "consensus_blocker_class_counts",
              Profile.consensus_blocker_class_counts_json [template_json];
              "profile_root_binding_status_counts",
              Profile.root_binding_counts_json profile_root_binding_counts;
              "profile_root_binding_classification_counts",
              Profile.root_binding_classification_counts_json
                profile_root_binding_classification_counts;
              "consensus_ready_gate",
              consensus_ready_gate
                ~profile_gate_count
                ~unprofiled_count:(1 - profile_gate_count)
                ~root_binding_counts:profile_root_binding_counts
                profile_status_counts;
              "templates", `List [checked_template_json checked];
            ]))
     | _ -> fail (path ^ ": template must be an object"))
  | [`Dir dir] ->
    let templates = List.map check_template (template_files dir) in
    if templates = [] then fail ("no templates in " ^ dir);
    let missing = missing_p0 templates in
    if missing <> [] then
      fail ("missing P0 templates: " ^ String.concat "," missing);
    let duplicates = duplicate_opcodes templates in
    if duplicates <> [] then
      fail ("duplicate P0 templates: " ^ String.concat "," duplicates);
    let profile_gate_count =
      List.length (List.filter template_profile_gate_present templates)
    in
    let template_jsons =
      List.map (fun checked -> Template.to_json checked.template) templates
    in
    let status_counts = profile_status_counts template_jsons in
    let root_binding_counts =
      profile_root_binding_status_counts template_jsons
    in
    let root_binding_classification_counts =
      profile_root_binding_classification_counts template_jsons
    in
    let classified_profile_gate_count =
      Profile.classified_gate_count status_counts
    in
    let unprofiled_count = List.length templates - profile_gate_count in
    let schema_status = "accepted" in
    let status =
      if
        consensus_ready_required_passes
          ~profile_gate_count
          ~unprofiled_count
          ~root_binding_counts
          status_counts
      then "accepted"
      else "rejected"
    in
    print_report_and_exit
      (`Assoc [
        "status", `String status;
        "schema_status", `String schema_status;
        "diagnostic_only", `Bool true;
        "template_count", `Int (List.length templates);
        "profile_gate_count", `Int profile_gate_count;
        "classified_profile_gate_count", `Int classified_profile_gate_count;
        "unprofiled_template_count", `Int unprofiled_count;
        "profile_consensus_status_counts",
        Profile.status_counts_json status_counts;
        "profile_root_catalog",
        Profile.profile_root_catalog_json template_jsons;
        "consensus_blocker_catalog",
        Profile.consensus_blocker_catalog_json template_jsons;
        "consensus_blocker_class_counts",
        Profile.consensus_blocker_class_counts_json template_jsons;
        "profile_root_binding_status_counts",
        Profile.root_binding_counts_json root_binding_counts;
        "profile_root_binding_classification_counts",
        Profile.root_binding_classification_counts_json
          root_binding_classification_counts;
        "consensus_ready_gate",
        consensus_ready_gate
          ~profile_gate_count
          ~unprofiled_count
          ~root_binding_counts
          status_counts;
        "p0_opcodes",
        `List (List.map (fun opcode -> `String opcode) Template.p0_opcodes);
        "templates", `List (List.map checked_template_json templates);
      ])
