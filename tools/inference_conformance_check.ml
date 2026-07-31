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
let requested_opcodes = ref []
let require_profile_roots_bound = ref false
let require_consensus_candidate = ref false
let require_consensus_ready = ref false
let require_validator_readiness = ref false

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
  "--opcode",
  Arg.String (fun value -> requested_opcodes := value :: !requested_opcodes),
  "limit template-index checks to one P0 opcode; may be repeated";
  "--require-profile-roots-bound",
  Arg.Set require_profile_roots_bound,
  "reject reports whose numerical_profile_root values do not match LiteNode profile roots";
  "--require-consensus-candidate",
  Arg.Set require_consensus_candidate,
  "reject reports whose profile gates are still local-only or unbound";
  "--require-consensus-ready",
  Arg.Set require_consensus_ready,
  "reject reports whose profile gates are not all consensus_ready";
  "--require-validator-readiness",
  Arg.Set require_validator_readiness,
  "exit nonzero unless the report also proves validator readiness";
]

let usage =
  "inference_conformance_check --template <path> \
   [--require-profile-roots-bound] [--require-consensus-candidate] \
   [--require-consensus-ready] [--require-validator-readiness]\n\
   or inference_conformance_check --template-dir <dir> \
   [--require-profile-roots-bound] [--require-consensus-candidate] \
   [--require-consensus-ready] [--require-validator-readiness]\n\
   or inference_conformance_check --template-index <path> \
   [--opcode <opcode> ...] \
   [--require-profile-roots-bound] [--require-consensus-candidate] \
   [--require-consensus-ready] [--require-validator-readiness]"

let selected_opcodes () =
  List.rev !requested_opcodes |> List.sort_uniq String.compare

let opcode_selected opcode =
  match selected_opcodes () with
  | [] -> true
  | opcodes -> List.exists (String.equal opcode) opcodes

let p0_scope_opcodes () =
  match selected_opcodes () with
  | [] -> Template.p0_opcodes
  | opcodes -> opcodes

let validate_selected_opcodes () =
  List.iter
    (fun opcode ->
       if not (List.exists (String.equal opcode) Template.p0_opcodes) then
         fail ("unsupported P0 opcode filter: " ^ opcode))
    (selected_opcodes ())

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

let starts_with prefix value =
  let prefix_len = String.length prefix in
  String.length value >= prefix_len
  && String.equal (String.sub value 0 prefix_len) prefix

let backend_type_string = function
  | Sys.Native -> "native"
  | Sys.Bytecode -> "bytecode"
  | Sys.Other value -> "other:" ^ value

let platform_json () =
  `Assoc [
    "ocaml_version", `String Sys.ocaml_version;
    "os_type", `String Sys.os_type;
    "word_size", `Int Sys.word_size;
    "big_endian", `Bool Sys.big_endian;
    "backend_type", `String (backend_type_string Sys.backend_type);
  ]

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

let mutation_name = function
  | `Assoc fields -> string_field "mutation" fields
  | _ -> None

let intlike_field name fields =
  match field name fields with
  | Some (`Int _)
  | Some (`Intlit _) -> true
  | _ -> false

let nonempty_string_list_field name fields =
  match field name fields with
  | Some (`List (_ :: _ as values)) ->
    List.for_all (function `String _ -> true | _ -> false) values
  | _ -> false

let supported_executable_mutation = function
  | "replace_first_f64_input_cell"
  | "replace_all_score_cells"
  | "replace_scores_with_large_finite_values"
  | "set_count_to_zero"
  | "set_epsilon_bits"
  | "set_scalar_param"
  | "set_state_dst_to_output_base"
  | "set_output_base_to_first_input_base"
  | "set_output_base_to_first_input_base_plus"
  | "replace_q1_scale_bits"
  | "truncate_input_manifest"
  | "lower_effort_limit" -> true
  | _ -> false

let mutation_required_param_missing name fields =
  match name with
  | "replace_first_f64_input_cell"
  | "replace_all_score_cells"
  | "set_epsilon_bits" ->
    if intlike_field "value_bits" fields then None else Some "value_bits"
  | "replace_scores_with_large_finite_values" ->
    if nonempty_string_list_field "values_decimal" fields then None
    else Some "values_decimal"
  | "set_scalar_param"
  | "lower_effort_limit" ->
    if int_field "value" fields <> None then None else Some "value"
  | "set_output_base_to_first_input_base_plus" ->
    if int_field "offset_cells" fields <> None then None else Some "offset_cells"
  | "replace_q1_scale_bits" ->
    if string_field "value_hex_le" fields <> None then None
    else Some "value_hex_le"
  | "truncate_input_manifest" ->
    if int_field "truncate_bytes" fields <> None then None
    else Some "truncate_bytes"
  | "set_count_to_zero"
  | "set_state_dst_to_output_base"
  | "set_output_base_to_first_input_base" -> None
  | _ -> None

let mutation_targets_output = function
  | `Assoc fields ->
    (match string_field "target" fields with
     | Some "output.base_address"
     | Some "dst" -> true
     | Some _ -> false
     | None -> true)
  | _ -> true

let mutation_offset_cells = function
  | `Assoc fields -> int_field "offset_cells" fields
  | _ -> None

let exact_q1_alias_mutation mutation =
  match mutation_name mutation with
  | Some "set_output_base_to_first_input_base" ->
    mutation_targets_output mutation
  | Some "set_output_base_to_first_input_base_plus" ->
    mutation_targets_output mutation
    && Option.value ~default:(-1) (mutation_offset_cells mutation) = 0
  | _ -> false

let partial_q1_alias_mutation mutation =
  match mutation_name mutation with
  | Some "set_output_base_to_first_input_base_plus" ->
    mutation_targets_output mutation
    && Option.value ~default:0 (mutation_offset_cells mutation) > 0
  | _ -> false

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

let failure_span_issues path opcode case = function
  | `Assoc fields ->
    let name_issue =
      match string_field "name" fields with
      | Some _ -> []
      | None ->
        [issue ~opcode path
           ("failure case unchanged span missing name: " ^ case)]
    in
    let base_issue =
      match int_field "base_address" fields with
      | Some value when value >= 0 -> []
      | Some _ ->
        [issue ~opcode path
           ("failure case unchanged span base_address must be nonnegative: "
            ^ case)]
      | None ->
        [issue ~opcode path
           ("failure case unchanged span missing base_address: " ^ case)]
    in
    let length_issue =
      match int_field "length_f64_cells" fields with
      | Some value when value > 0 -> []
      | Some _ ->
        [issue ~opcode path
           ("failure case unchanged span length_f64_cells must be positive: "
            ^ case)]
      | None ->
        [issue ~opcode path
           ("failure case unchanged span missing length_f64_cells: " ^ case)]
    in
    name_issue @ base_issue @ length_issue
  | _ ->
    [issue ~opcode path
       ("failure case unchanged_spans entries must be objects: " ^ case)]

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

let vm_semantics_binding_value = function
  | `Assoc fields ->
    (match List.assoc_opt "vm_semantics_binding" fields with
     | Some (`Assoc _ as binding) -> Some binding
     | _ -> Some Profile.unavailable_root_binding_json)
  | _ -> None

let vm_semantics_binding_status_counts values =
  values
  |> List.filter_map vm_semantics_binding_value
  |> Profile.root_binding_counts_of_json

let vm_semantics_root_blockers counts =
  let add_if condition value values =
    if condition then value :: values else values
  in
  let total = counts.Profile.matched + counts.unbound + counts.unavailable in
  []
  |> add_if (total = 0) "no_vm_semantics_roots"
  |> add_if (counts.unavailable > 0) "unavailable_vm_semantics_roots"
  |> add_if (counts.unbound > 0) "unbound_vm_semantics_roots"

let vm_semantics_binding_gate_json counts =
  let ready = Profile.root_bindings_are_consensus_ready counts in
  `Assoc [
    "status", `String (if ready then "accepted" else "rejected");
    "blockers",
    `List
      (List.map
         (fun blocker -> `String blocker)
         (vm_semantics_root_blockers counts));
  ]

let abi_declaration_binding_value = function
  | `Assoc fields ->
    (match List.assoc_opt "abi_declaration_binding" fields with
     | Some (`Assoc _ as binding) -> Some binding
     | _ -> Some Profile.unavailable_root_binding_json)
  | _ -> None

let abi_declaration_binding_status_counts values =
  values
  |> List.filter_map abi_declaration_binding_value
  |> Profile.root_binding_counts_of_json

let abi_declaration_binding_blockers counts =
  let add_if condition value values =
    if condition then value :: values else values
  in
  let total = counts.Profile.matched + counts.unbound + counts.unavailable in
  []
  |> add_if (total = 0) "no_abi_declaration_binding"
  |> add_if (counts.unavailable > 0) "unavailable_abi_declaration_binding"
  |> add_if (counts.unbound > 0) "unbound_abi_declaration_binding"

let abi_declaration_binding_gate_json counts =
  let ready = Profile.root_bindings_are_consensus_ready counts in
  `Assoc [
    "status", `String (if ready then "accepted" else "rejected");
    "blockers",
    `List
      (List.map
         (fun blocker -> `String blocker)
         (abi_declaration_binding_blockers counts));
  ]

let consensus_candidate_gate
    ~profile_gate_count
    ~unprofiled_count
    ~root_binding_counts
    status_counts =
  let ready =
    Profile.consensus_candidate
      ~profile_gate_count
      ~unprofiled_count
      status_counts
    && Profile.root_bindings_are_consensus_ready root_binding_counts
  in
  `Assoc [
    "required", `Bool !require_consensus_candidate;
    "status",
    `String
      (if not !require_consensus_candidate then "not_required"
       else if ready then "accepted"
       else "rejected");
    "blockers",
    `List
      (List.map
         (fun blocker -> `String blocker)
         (Profile.consensus_candidate_blockers
            ~profile_gate_count
            ~unprofiled_count
            status_counts
          @ Profile.root_binding_blockers root_binding_counts));
  ]

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

let consensus_candidate_required_passes
    ~profile_gate_count
    ~unprofiled_count
    ~root_binding_counts
    status_counts =
  (not !require_consensus_candidate)
  ||
  (Profile.consensus_candidate
     ~profile_gate_count
     ~unprofiled_count
     status_counts
   && Profile.root_bindings_are_consensus_ready root_binding_counts)

let profile_roots_required_passes ~root_binding_counts =
  Profile.root_bindings_required_pass
    ~required:!require_profile_roots_bound
    root_binding_counts

let add_blocker condition blocker blockers =
  if condition then blocker :: blockers else blockers

let blocker_matches pattern blocker =
  if String.equal pattern "q1_failure_case_" then
    starts_with pattern blocker
  else
    String.equal pattern blocker

let next_readiness_blocker blockers =
  let priority =
    [
      "schema_rejected";
      "execution_rejected";
      "execution_not_run";
      "strict_effort_not_enabled";
      "effort_mismatch";
      "effort_not_run";
      "q1_failure_case_";
      "uncounted_failure_cases";
      "punitive_failure_cases_not_accepted";
      "punitive_failure_cases_not_run";
      "vm_semantics_root_binding_not_proven";
      "unbound_vm_semantics_roots";
      "vm_semantics_binding_mismatch";
      "vm_semantics_binding_rejected";
      "abi_declaration_binding_not_proven";
      "unbound_abi_declaration_binding";
      "abi_declaration_binding_mismatch";
      "abi_declaration_binding_rejected";
      "consensus_candidate_profile_gates";
      "unbound_profile_roots";
      "profile_root_binding_mismatch";
      "profile_root_binding_rejected";
      "cross_platform_conformance_missing";
      "missing_cross_platform_matrix";
    ]
  in
  match
    List.find_map
      (fun pattern ->
         List.find_opt (blocker_matches pattern) blockers)
      priority
  with
  | Some blocker -> Some blocker
  | None ->
    (match blockers with
     | [] -> None
     | blocker :: _ -> Some blocker)

let next_blocker_json blockers =
  match next_readiness_blocker blockers with
  | Some blocker -> `String blocker
  | None -> `Null

let gate_status_accepted = function
  | `Assoc fields ->
    (match string_field "status" fields with
     | Some "accepted" -> true
     | _ -> false)
  | _ -> false

let required_gate_passes ~required gate =
  (not required) || gate_status_accepted gate

let static_validator_readiness_gate
    ~required
    ~schema_status
    ~profile_gate_count
    ~unprofiled_count
    ~root_binding_counts
    ~vm_semantics_binding_counts
    ~abi_declaration_binding_counts
    status_counts =
  let schema_accepted = String.equal schema_status "accepted" in
  let profile_ready =
    Profile.consensus_ready
      ~profile_gate_count
      ~unprofiled_count
      status_counts
  in
  let roots_ready =
    Profile.root_bindings_are_consensus_ready root_binding_counts
  in
  let vm_semantics_ready =
    Profile.root_bindings_are_consensus_ready vm_semantics_binding_counts
  in
  let abi_ready =
    Profile.root_bindings_are_consensus_ready abi_declaration_binding_counts
  in
  let cross_platform_ready = false in
  let ready =
    Profile.validator_readiness_accepted
      ~execution_ready:false
      ~failure_cases_ready:false
      ~effort_ready:false
      ~profile_ready
      ~roots_ready
      ~cross_platform_ready
    && vm_semantics_ready
    && abi_ready
  in
  let blockers =
    []
    |> add_blocker (not schema_accepted) "schema_rejected"
    |> add_blocker true "execution_not_run"
    |> add_blocker true "punitive_failure_cases_not_run"
    |> add_blocker true "effort_not_run"
    |> add_blocker
         (not vm_semantics_ready)
         "vm_semantics_root_binding_not_proven"
    |> add_blocker
         (not abi_ready)
         "abi_declaration_binding_not_proven"
    |> add_blocker
         (not cross_platform_ready)
         "cross_platform_conformance_missing"
  in
  let blockers =
    blockers
    @ Profile.consensus_ready_blockers
        ~profile_gate_count
        ~unprofiled_count
        status_counts
    @ Profile.root_binding_blockers root_binding_counts
    @ vm_semantics_root_blockers vm_semantics_binding_counts
    @ abi_declaration_binding_blockers abi_declaration_binding_counts
  in
  `Assoc [
    "diagnostic_only", `Bool true;
    "required", `Bool required;
    "status", `String (if ready then "accepted" else "rejected");
    "schema_status", `String schema_status;
    "execution_status", `String "not_run";
    "failure_case_status", `String "not_run";
    "strict_effort_status", `String "not_run";
    "effort_status", `String "not_run";
    "profile_status",
    `String (if profile_ready then "accepted" else "rejected");
    "profile_root_status",
    `String (if roots_ready then "accepted" else "rejected");
    "vm_semantics_root_status",
    `String (if vm_semantics_ready then "accepted" else "rejected");
    "vm_semantics_binding_gate",
    vm_semantics_binding_gate_json vm_semantics_binding_counts;
    "abi_declaration_status",
    `String (if abi_ready then "accepted" else "rejected");
    "abi_declaration_binding_gate", abi_declaration_binding_gate_json abi_declaration_binding_counts;
    "cross_platform_status",
    `String (if cross_platform_ready then "accepted" else "rejected");
    "next_blocker", next_blocker_json blockers;
    "blockers",
    `List (List.map (fun blocker -> `String blocker) blockers);
  ]

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
    List.filter
      (fun effect_name ->
         not (List.exists (String.equal effect_name) effects))
      ["memory_read"; "memory_write"; "storage_read"]
    |> List.map (fun effect_name ->
      issue ~opcode path ("missing program effect: " ^ effect_name))

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
              let expected = string_field "expected" failure_fields in
              let mutations =
                match list_field "executable_mutations" failure_fields with
                | Some mutations -> mutations
                | None -> []
              in
              let unchanged_spans =
                match list_field "unchanged_spans" failure_fields with
                | Some spans -> spans
                | None -> []
              in
              `Case
                (case,
                 expected,
                 mutations,
                 (match mutations with _ :: _ -> true | [] -> false),
                 unchanged_spans,
                 (match unchanged_spans with _ :: _ -> true | [] -> false))
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
            | `Case (case, _, _, false, _, _) -> Some case
            | _ -> None)
          parsed
      in
      let missing_unchanged =
        List.filter_map
          (function
            | `Case (case, _, _, _, _, false) -> Some case
            | _ -> None)
          parsed
      in
      let missing_expected =
        List.filter_map
          (function
            | `Case (case, None, _, _, _, _) -> Some case
            | _ -> None)
          parsed
      in
      let unchanged_span_issues =
        parsed
        |> List.concat_map (function
          | `Case (case, _, _, _, spans, _) ->
            List.concat_map (failure_span_issues path opcode case) spans
          | _ -> [])
      in
      let executable_mutation_issues =
        parsed
        |> List.concat_map (function
          | `Case (case, _, mutations, _, _, _) ->
            mutations
            |> List.concat_map (function
              | `Assoc mutation_fields ->
                let name_issues =
                  match string_field "mutation" mutation_fields with
                  | Some name when supported_executable_mutation name ->
                    (match mutation_required_param_missing name mutation_fields with
                     | Some param ->
                       [issue ~opcode path
                          ("failure case mutation "
                           ^ name
                           ^ " missing "
                           ^ param
                           ^ ": "
                           ^ case)]
                     | None -> [])
                  | Some name ->
                    [issue ~opcode path
                       ("unsupported executable mutation "
                        ^ name
                        ^ ": "
                        ^ case)]
                  | None ->
                    [issue ~opcode path
                       ("failure case mutation missing mutation: " ^ case)]
                in
                let target_issues =
                  match string_field "target" mutation_fields with
                  | Some _ -> []
                  | None ->
                    [issue ~opcode path
                       ("failure case mutation missing target: " ^ case)]
                in
                name_issues @ target_issues
              | _ ->
                [issue ~opcode path
                   ("failure case executable_mutations entries must be objects: "
                    ^ case)])
          | _ -> [])
      in
      let q1_required_case_issues =
        if not (String.equal opcode "LINEAR_Q1_G128_FP") then []
        else
          let expected_for case =
            List.find_map
              (function
                | `Case (actual, expected, _, _, _, _)
                  when String.equal actual case ->
                  expected
                | _ -> None)
              parsed
          in
          Template.q1_required_failure_expectations
          |> List.filter_map (fun (case, expected_prefix) ->
            match expected_for case with
            | Some expected when starts_with expected_prefix expected -> None
            | Some _ ->
              Some
                (issue ~opcode path
                   (case ^ " must declare " ^ expected_prefix))
            | None ->
              Some
                (issue ~opcode path
                   (case
                   ^ " failure case is required for Q1 validator readiness")))
      in
      let q1_alias_shape_issues =
        if not (String.equal opcode "LINEAR_Q1_G128_FP") then []
        else
          let mutations_for case =
            List.find_map
              (function
                | `Case (actual, _, mutations, _, _, _)
                  when String.equal actual case ->
                  Some mutations
                | _ -> None)
              parsed
          in
          let exact_issue =
            match mutations_for "output_input_aliasing" with
            | Some mutations
              when List.exists exact_q1_alias_mutation mutations -> []
            | Some _ ->
              [issue ~opcode path
                 "output_input_aliasing must declare output/lhs alias mutation"]
            | None -> []
          in
          let partial_issue =
            match mutations_for "partial_output_input_aliasing" with
            | Some mutations
              when List.exists partial_q1_alias_mutation mutations -> []
            | Some _ ->
              [issue ~opcode path
                 "partial_output_input_aliasing must declare partial output/lhs alias mutation"]
            | None -> []
          in
          exact_issue @ partial_issue
      in
      let expected_issue =
        if missing_expected = [] then []
        else
          [issue ~opcode path
             ("failure cases lack expected outcome: "
              ^ String.concat "," missing_expected)]
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
      bad @ expected_issue @ mutation_issue @ unchanged_issue
      @ unchanged_span_issues @ executable_mutation_issues
      @ q1_required_case_issues @ q1_alias_shape_issues

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

let abi_declaration_binding_json fields =
  Template.abi_declaration_binding_json (`Assoc fields)

let abi_issues path opcode fields =
  match abi_declaration_binding_json fields with
  | `Assoc binding_fields ->
    (match string_field "status" binding_fields with
     | Some "matched" -> []
     | _ ->
       (match list_field "blockers" binding_fields with
        | Some blockers ->
          List.map
            (function
              | `String blocker ->
                issue ~opcode path ("ABI declaration binding rejected: " ^ blocker)
              | _ -> issue ~opcode path "ABI declaration binding has invalid blocker")
            blockers
        | None -> [issue ~opcode path "ABI declaration binding rejected"]))
  | _ -> [issue ~opcode path "ABI declaration binding rejected"]

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
    let vm_semantics_binding =
      match string_field "vm_semantics_root" fields with
      | Some vm_semantics_root ->
        Template.vm_semantics_binding_json ~opcode ~vm_semantics_root
      | None -> `Null
    in
    let abi_declaration_binding = abi_declaration_binding_json fields in
    Some
      (`Assoc [
        "path", `String path;
        "opcode", `String opcode;
        "profile_gate", profile_gate;
        "profile_root_binding", profile_root_binding;
        "vm_semantics_binding", vm_semantics_binding;
        "abi_declaration_binding", abi_declaration_binding;
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

let checked_mul left right =
  if left < 0 || right < 0 then None
  else if left <> 0 && right > max_int / left then None
  else Some (left * right)

let q1_expected_effort_issues path opcode fields =
  if not (String.equal opcode "LINEAR_Q1_G128_FP") then []
  else
    match int_field "expected_effort" fields with
    | None -> []
    | Some expected_effort ->
      (match assoc_field "parameter_addresses_and_scalar_params" fields with
       | None -> [issue ~opcode path "missing Q1 effort parameter metadata"]
       | Some params ->
         match assoc_field "values" params with
         | None -> [issue ~opcode path "missing Q1 effort parameter values"]
         | Some values ->
           match int_field "m" values, int_field "k" values, int_field "n" values with
           | Some m, Some k, Some n when m >= 0 && k >= 0 && n >= 0 ->
             (match Option.bind (checked_mul m n) (fun mn -> checked_mul mn k) with
              | Some product ->
                let expected = 200 + (product / 512) + 1 in
                if expected_effort = expected then []
                else
                  [issue ~opcode path
                     (Printf.sprintf
                        "expected_effort mismatch for LINEAR_Q1_G128_FP: expected %d actual %d"
                        expected
                        expected_effort)]
              | None ->
                [issue ~opcode path
                   "LINEAR_Q1_G128_FP effort factors overflow"])
           | _ ->
             [issue ~opcode path
                "missing LINEAR_Q1_G128_FP effort parameters m/k/n"])

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
    @ q1_expected_effort_issues path opcode fields
    @ profile_issues path opcode fields
    @ issues_for_program_effects path opcode fields
    @ source_path_issues path opcode fields
    @ expected_issues path opcode fields
    @ failure_issues path opcode fields
    @ gated_delta_semantic_issues path opcode fields
    @ abi_issues path opcode fields
  | _ -> [issue ~opcode path "template must be an object"]

let producer_index_report index_path =
  let index = read_json index_path in
  match index with
  | `Assoc fields ->
    let all_templates =
      match list_field "templates" fields with
      | Some values -> values
      | None -> fail (index_path ^ ": missing templates")
    in
    let templates =
      List.filter
        (function
          | `Assoc entry_fields ->
            (match string_field "opcode" entry_fields with
             | Some opcode -> opcode_selected opcode
             | None -> selected_opcodes () = [])
          | _ -> selected_opcodes () = [])
        all_templates
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
    let unprofiled_template_count = List.length templates - profile_gate_count in
    let profile_status_counts = profile_status_counts profile_gates in
    let profile_root_binding_counts =
      profile_root_binding_status_counts profile_gates
    in
    let vm_semantics_binding_counts =
      vm_semantics_binding_status_counts profile_gates
    in
    let abi_declaration_binding_counts =
      abi_declaration_binding_status_counts profile_gates
    in
    let profile_root_binding_classification_counts =
      profile_root_binding_classification_counts profile_gates
    in
    let classified_profile_gate_count =
      Profile.classified_gate_count profile_status_counts
    in
    let missing =
      List.filter
        (fun opcode -> not (List.exists (String.equal opcode) opcodes))
        (p0_scope_opcodes ())
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
    let validator_readiness_gate_json =
      static_validator_readiness_gate
        ~required:!require_validator_readiness
        ~schema_status
        ~profile_gate_count
        ~unprofiled_count:unprofiled_template_count
        ~root_binding_counts:profile_root_binding_counts
        ~vm_semantics_binding_counts
        ~abi_declaration_binding_counts
        profile_status_counts
    in
    let status =
      if
        String.equal schema_status "accepted"
        && profile_roots_required_passes
             ~root_binding_counts:profile_root_binding_counts
        && consensus_candidate_required_passes
             ~profile_gate_count
             ~unprofiled_count:unprofiled_template_count
             ~root_binding_counts:profile_root_binding_counts
             profile_status_counts
        && consensus_ready_required_passes
             ~profile_gate_count
             ~unprofiled_count:unprofiled_template_count
             ~root_binding_counts:profile_root_binding_counts
             profile_status_counts
        && required_gate_passes
             ~required:!require_validator_readiness
             validator_readiness_gate_json
      then "accepted"
      else "rejected"
    in
    `Assoc [
      "status", `String status;
      "schema_status", `String schema_status;
      "diagnostic_only", `Bool true;
      "validator_readiness_required", `Bool !require_validator_readiness;
      "platform", platform_json ();
      "index_path", `String index_path;
      "selected_opcodes",
      `List (List.map (fun opcode -> `String opcode) (selected_opcodes ()));
      "source_template_count", `Int (List.length all_templates);
      "template_count", `Int (List.length templates);
      "p0_opcodes",
      `List (List.map (fun opcode -> `String opcode) (p0_scope_opcodes ()));
      "required_failure_case_contracts",
      `List [Template.q1_required_failure_expectations_json];
      "profile_gates", `List profile_gates;
      "profile_gate_count", `Int profile_gate_count;
      "classified_profile_gate_count", `Int classified_profile_gate_count;
      "unprofiled_template_count", `Int unprofiled_template_count;
      "profile_consensus_status_counts",
      Profile.status_counts_json profile_status_counts;
      "profile_root_catalog",
      Profile.profile_root_catalog_json profile_gates;
      "profile_catalog_root",
      Profile.profile_catalog_root_json profile_gates;
      "profile_root_binding_catalog",
      Profile.profile_root_binding_catalog_json profile_gates;
      "consensus_blocker_catalog",
      Profile.consensus_blocker_catalog_json profile_gates;
      "consensus_blocker_class_counts",
      Profile.consensus_blocker_class_counts_json profile_gates;
      "profile_root_binding_status_counts",
      Profile.root_binding_counts_json profile_root_binding_counts;
      "profile_root_binding_classification_counts",
      Profile.root_binding_classification_counts_json
        profile_root_binding_classification_counts;
      "profile_root_binding_gate",
      Profile.root_binding_gate_json
        ~required:!require_profile_roots_bound
        profile_root_binding_counts;
      "vm_semantics_binding_status_counts",
      Profile.root_binding_counts_json vm_semantics_binding_counts;
      "vm_semantics_binding_gate",
      vm_semantics_binding_gate_json vm_semantics_binding_counts;
      "abi_declaration_binding_status_counts",
      Profile.root_binding_counts_json abi_declaration_binding_counts;
      "abi_declaration_binding_gate", abi_declaration_binding_gate_json abi_declaration_binding_counts;
      "validator_readiness_gate", validator_readiness_gate_json;
      "consensus_candidate_gate",
      consensus_candidate_gate
        ~profile_gate_count
        ~unprofiled_count:unprofiled_template_count
        ~root_binding_counts:profile_root_binding_counts
        profile_status_counts;
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
  let accepted =
    match report with
    | `Assoc fields ->
      let report_accepted =
        match string_field "status" fields with
        | Some "accepted" -> true
        | _ -> false
      in
      let validator_readiness_accepted =
        (not !require_validator_readiness)
        ||
        match field "validator_readiness_gate" fields with
        | Some (`Assoc gate_fields) ->
          (match string_field "status" gate_fields with
           | Some "accepted" -> true
           | _ -> false)
        | _ -> false
      in
      report_accepted && validator_readiness_accepted
    | _ -> false
  in
  if not accepted then exit 1

let () =
  Arg.parse args (fun arg -> fail ("unexpected argument: " ^ arg)) usage;
  validate_selected_opcodes ();
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
          if selected_opcodes () <> [] then
            fail "--opcode is supported only with --template-index";
          let checked = check_template path in
          let template_json = Template.to_json checked.template in
          let profile_gate_count =
            if profile_gate_present template_json then 1 else 0
          in
          let profile_status_counts = profile_status_counts [template_json] in
          let profile_root_binding_counts =
            profile_root_binding_status_counts [template_json]
          in
          let vm_semantics_binding_counts =
            vm_semantics_binding_status_counts [template_json]
          in
          let abi_declaration_binding_counts =
            abi_declaration_binding_status_counts [template_json]
          in
          let profile_root_binding_classification_counts =
            profile_root_binding_classification_counts [template_json]
          in
          let classified_profile_gate_count =
            Profile.classified_gate_count profile_status_counts
          in
          let schema_status = "accepted" in
          let validator_readiness_gate_json =
            static_validator_readiness_gate
              ~required:!require_validator_readiness
              ~schema_status
              ~profile_gate_count
              ~unprofiled_count:(1 - profile_gate_count)
              ~root_binding_counts:profile_root_binding_counts
              ~vm_semantics_binding_counts
              ~abi_declaration_binding_counts
              profile_status_counts
          in
          let status =
            if
              profile_roots_required_passes
                ~root_binding_counts:profile_root_binding_counts
              &&
              consensus_candidate_required_passes
                ~profile_gate_count
                ~unprofiled_count:(1 - profile_gate_count)
                ~root_binding_counts:profile_root_binding_counts
                profile_status_counts
              &&
              consensus_ready_required_passes
                ~profile_gate_count
                ~unprofiled_count:(1 - profile_gate_count)
                ~root_binding_counts:profile_root_binding_counts
                profile_status_counts
              && required_gate_passes
                   ~required:!require_validator_readiness
                   validator_readiness_gate_json
            then "accepted"
            else "rejected"
          in
          print_report_and_exit
            (`Assoc [
              "status", `String status;
              "schema_status", `String schema_status;
              "diagnostic_only", `Bool true;
              "validator_readiness_required", `Bool !require_validator_readiness;
              "platform", platform_json ();
              "template_count", `Int 1;
              "profile_gate_count", `Int profile_gate_count;
              "classified_profile_gate_count",
              `Int classified_profile_gate_count;
              "unprofiled_template_count", `Int (1 - profile_gate_count);
              "profile_consensus_status_counts",
              Profile.status_counts_json profile_status_counts;
              "profile_root_catalog",
              Profile.profile_root_catalog_json [template_json];
              "profile_catalog_root",
              Profile.profile_catalog_root_json [template_json];
              "profile_root_binding_catalog",
              Profile.profile_root_binding_catalog_json [template_json];
              "consensus_blocker_catalog",
              Profile.consensus_blocker_catalog_json [template_json];
              "consensus_blocker_class_counts",
              Profile.consensus_blocker_class_counts_json [template_json];
              "profile_root_binding_status_counts",
              Profile.root_binding_counts_json profile_root_binding_counts;
              "profile_root_binding_classification_counts",
              Profile.root_binding_classification_counts_json
                profile_root_binding_classification_counts;
              "profile_root_binding_gate",
              Profile.root_binding_gate_json
                ~required:!require_profile_roots_bound
                profile_root_binding_counts;
              "vm_semantics_binding_status_counts",
              Profile.root_binding_counts_json vm_semantics_binding_counts;
              "vm_semantics_binding_gate",
              vm_semantics_binding_gate_json vm_semantics_binding_counts;
              "abi_declaration_binding_status_counts",
              Profile.root_binding_counts_json abi_declaration_binding_counts;
              "abi_declaration_binding_gate", abi_declaration_binding_gate_json abi_declaration_binding_counts;
              "validator_readiness_gate", validator_readiness_gate_json;
              "consensus_candidate_gate",
              consensus_candidate_gate
                ~profile_gate_count
                ~unprofiled_count:(1 - profile_gate_count)
                ~root_binding_counts:profile_root_binding_counts
                profile_status_counts;
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
    if selected_opcodes () <> [] then
      fail "--opcode is supported only with --template-index";
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
    let vm_semantics_binding_counts =
      vm_semantics_binding_status_counts template_jsons
    in
    let abi_declaration_binding_counts =
      abi_declaration_binding_status_counts template_jsons
    in
    let root_binding_classification_counts =
      profile_root_binding_classification_counts template_jsons
    in
    let classified_profile_gate_count =
      Profile.classified_gate_count status_counts
    in
    let unprofiled_count = List.length templates - profile_gate_count in
    let schema_status = "accepted" in
    let validator_readiness_gate_json =
      static_validator_readiness_gate
        ~required:!require_validator_readiness
        ~schema_status
        ~profile_gate_count
        ~unprofiled_count
        ~root_binding_counts
        ~vm_semantics_binding_counts
        ~abi_declaration_binding_counts
        status_counts
    in
    let status =
      if
        profile_roots_required_passes
          ~root_binding_counts
        &&
        consensus_candidate_required_passes
          ~profile_gate_count
          ~unprofiled_count
          ~root_binding_counts
          status_counts
        &&
        consensus_ready_required_passes
          ~profile_gate_count
          ~unprofiled_count
          ~root_binding_counts
          status_counts
        && required_gate_passes
             ~required:!require_validator_readiness
             validator_readiness_gate_json
      then "accepted"
      else "rejected"
    in
    print_report_and_exit
      (`Assoc [
        "status", `String status;
        "schema_status", `String schema_status;
        "diagnostic_only", `Bool true;
        "validator_readiness_required", `Bool !require_validator_readiness;
        "platform", platform_json ();
        "template_count", `Int (List.length templates);
        "profile_gate_count", `Int profile_gate_count;
        "classified_profile_gate_count", `Int classified_profile_gate_count;
        "unprofiled_template_count", `Int unprofiled_count;
        "profile_consensus_status_counts",
        Profile.status_counts_json status_counts;
        "profile_root_catalog",
        Profile.profile_root_catalog_json template_jsons;
        "profile_catalog_root",
        Profile.profile_catalog_root_json template_jsons;
        "profile_root_binding_catalog",
        Profile.profile_root_binding_catalog_json template_jsons;
        "consensus_blocker_catalog",
        Profile.consensus_blocker_catalog_json template_jsons;
        "consensus_blocker_class_counts",
        Profile.consensus_blocker_class_counts_json template_jsons;
        "profile_root_binding_status_counts",
        Profile.root_binding_counts_json root_binding_counts;
        "profile_root_binding_classification_counts",
        Profile.root_binding_classification_counts_json
          root_binding_classification_counts;
        "profile_root_binding_gate",
        Profile.root_binding_gate_json
          ~required:!require_profile_roots_bound
          root_binding_counts;
        "vm_semantics_binding_status_counts",
        Profile.root_binding_counts_json vm_semantics_binding_counts;
        "vm_semantics_binding_gate",
        vm_semantics_binding_gate_json vm_semantics_binding_counts;
        "abi_declaration_binding_status_counts",
        Profile.root_binding_counts_json abi_declaration_binding_counts;
        "abi_declaration_binding_gate", abi_declaration_binding_gate_json abi_declaration_binding_counts;
        "validator_readiness_gate", validator_readiness_gate_json;
        "consensus_candidate_gate",
        consensus_candidate_gate
          ~profile_gate_count
          ~unprofiled_count
          ~root_binding_counts
          status_counts;
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
