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

type consensus_status =
  | Local_only
  | Consensus_candidate
  | Consensus_ready

type t = {
  name : string;
  consensus_status : consensus_status;
  summary : string;
  required_actions : string list;
}

type status_counts = {
  local_only : int;
  consensus_candidate : int;
  consensus_ready : int;
  unknown : int;
}

type root_binding_counts = {
  matched : int;
  unbound : int;
  unavailable : int;
}

type root_binding_classification_counts = {
  none : int;
  profile_root_mismatch : int;
  profile_root_unavailable : int;
  root_binding_unknown : int;
}

type error =
  | Unknown_profile of string
  | Unsupported_opcode_profile of {
      opcode : string;
      profile : string;
      expected : string;
    }

let status_string = function
  | Local_only -> "local_only"
  | Consensus_candidate -> "consensus_candidate"
  | Consensus_ready -> "consensus_ready"

let empty_status_counts = {
  local_only = 0;
  consensus_candidate = 0;
  consensus_ready = 0;
  unknown = 0;
}

let empty_root_binding_counts = {
  matched = 0;
  unbound = 0;
  unavailable = 0;
}

let empty_root_binding_classification_counts = {
  none = 0;
  profile_root_mismatch = 0;
  profile_root_unavailable = 0;
  root_binding_unknown = 0;
}

let gate_status = function
  | `Assoc fields ->
    (match List.assoc_opt "consensus_status" fields with
     | Some (`String value) -> Some value
     | _ -> Some "unknown")
  | _ -> Some "unknown"

let add_gate_status counts gate =
  match gate_status gate with
  | Some "local_only" ->
    { counts with local_only = counts.local_only + 1 }
  | Some "consensus_candidate" ->
    { counts with consensus_candidate = counts.consensus_candidate + 1 }
  | Some "consensus_ready" ->
    { counts with consensus_ready = counts.consensus_ready + 1 }
  | Some _ ->
    { counts with unknown = counts.unknown + 1 }
  | None -> counts

let status_counts_of_json_gates gates =
  List.fold_left add_gate_status empty_status_counts gates

let classified_gate_count counts =
  counts.local_only
  + counts.consensus_candidate
  + counts.consensus_ready
  + counts.unknown

let status_counts_are_consensus_candidate counts =
  counts.consensus_candidate + counts.consensus_ready > 0
  && counts.local_only = 0
  && counts.unknown = 0

let status_counts_are_consensus_ready counts =
  counts.consensus_ready > 0
  && counts.local_only = 0
  && counts.consensus_candidate = 0
  && counts.unknown = 0

let consensus_candidate ~profile_gate_count ~unprofiled_count counts =
  profile_gate_count > 0
  && classified_gate_count counts = profile_gate_count
  && unprofiled_count = 0
  && status_counts_are_consensus_candidate counts

let consensus_ready ~profile_gate_count ~unprofiled_count counts =
  profile_gate_count > 0
  && classified_gate_count counts = profile_gate_count
  && unprofiled_count = 0
  && status_counts_are_consensus_ready counts

let add_if condition value values =
  if condition then value :: values else values

let consensus_candidate_blockers ~profile_gate_count ~unprofiled_count counts =
  let classified_count = classified_gate_count counts in
  []
  |> add_if
       (classified_count <> profile_gate_count)
       "unclassified_profile_gates"
  |> add_if (counts.unknown > 0) "unknown_profile_gates"
  |> add_if (counts.local_only > 0) "local_only_profile_gates"
  |> add_if (unprofiled_count > 0) "unprofiled_profile_gates"
  |> add_if (profile_gate_count = 0) "no_profile_gates"

let consensus_ready_blockers ~profile_gate_count ~unprofiled_count counts =
  let classified_count = classified_gate_count counts in
  []
  |> add_if
       (classified_count <> profile_gate_count)
       "unclassified_profile_gates"
  |> add_if (counts.unknown > 0) "unknown_profile_gates"
  |> add_if (counts.consensus_candidate > 0) "consensus_candidate_profile_gates"
  |> add_if (counts.local_only > 0) "local_only_profile_gates"
  |> add_if (unprofiled_count > 0) "unprofiled_profile_gates"
  |> add_if (profile_gate_count = 0) "no_profile_gates"

let root_binding_status = function
  | `Assoc fields ->
    (match List.assoc_opt "status" fields with
     | Some (`String "matched") -> "matched"
     | Some (`String "unbound") -> "unbound"
     | Some (`String "unavailable") -> "unavailable"
     | _ -> "unavailable")
  | _ -> "unavailable"

let add_root_binding_status counts binding =
  match root_binding_status binding with
  | "matched" -> { counts with matched = counts.matched + 1 }
  | "unbound" -> { counts with unbound = counts.unbound + 1 }
  | _ -> { counts with unavailable = counts.unavailable + 1 }

let root_binding_counts_of_json bindings =
  List.fold_left add_root_binding_status empty_root_binding_counts bindings

let root_binding_classification = function
  | `Assoc fields ->
    (match List.assoc_opt "classification" fields with
     | Some (`String "none") -> "none"
     | Some (`String "profile_root_mismatch") -> "profile_root_mismatch"
     | Some (`String "profile_root_unavailable") -> "profile_root_unavailable"
     | Some (`String _) -> "root_binding_unknown"
     | _ ->
       (match root_binding_status (`Assoc fields) with
        | "matched" -> "none"
        | "unbound" -> "profile_root_mismatch"
        | "unavailable" -> "profile_root_unavailable"
        | _ -> "root_binding_unknown"))
  | _ -> "profile_root_unavailable"

let add_root_binding_classification counts binding =
  match root_binding_classification binding with
  | "none" -> { counts with none = counts.none + 1 }
  | "profile_root_mismatch" ->
    { counts with profile_root_mismatch = counts.profile_root_mismatch + 1 }
  | "profile_root_unavailable" ->
    {
      counts with
      profile_root_unavailable = counts.profile_root_unavailable + 1;
    }
  | _ ->
    {
      counts with
      root_binding_unknown = counts.root_binding_unknown + 1;
    }

let root_binding_classification_counts_of_json bindings =
  List.fold_left
    add_root_binding_classification
    empty_root_binding_classification_counts
    bindings

let root_bindings_are_consensus_ready counts =
  counts.matched > 0 && counts.unbound = 0 && counts.unavailable = 0

let root_binding_blockers counts =
  let total = counts.matched + counts.unbound + counts.unavailable in
  []
  |> add_if (total = 0) "no_profile_roots"
  |> add_if (counts.unavailable > 0) "unavailable_profile_roots"
  |> add_if (counts.unbound > 0) "unbound_profile_roots"

let root_bindings_required_pass ~required counts =
  (not required) || root_bindings_are_consensus_ready counts

let validator_readiness_accepted
    ~execution_ready
    ~failure_cases_ready
    ~effort_ready
    ~profile_ready
    ~roots_ready
    ~cross_platform_ready =
  execution_ready
  && failure_cases_ready
  && effort_ready
  && profile_ready
  && roots_ready
  && cross_platform_ready

let root_binding_gate_json ~required counts =
  let ready = root_bindings_are_consensus_ready counts in
  `Assoc [
    "required", `Bool required;
    "status",
    `String
      (if not required then "not_required"
       else if ready then "accepted"
       else "rejected");
    "blockers",
    `List
      (List.map
         (fun blocker -> `String blocker)
         (root_binding_blockers counts));
  ]

let root_binding_counts_json counts =
  `Assoc [
    "matched", `Int counts.matched;
    "unbound", `Int counts.unbound;
    "unavailable", `Int counts.unavailable;
  ]

let root_binding_classification_counts_json counts =
  `Assoc [
    "none", `Int counts.none;
    "profile_root_mismatch", `Int counts.profile_root_mismatch;
    "profile_root_unavailable", `Int counts.profile_root_unavailable;
    "root_binding_unknown", `Int counts.root_binding_unknown;
  ]

let status_counts_json counts =
  `Assoc [
    "local_only", `Int counts.local_only;
    "consensus_candidate", `Int counts.consensus_candidate;
    "consensus_ready", `Int counts.consensus_ready;
    "unknown", `Int counts.unknown;
  ]

let string_field name fields =
  match List.assoc_opt name fields with
  | Some (`String value) -> Some value
  | _ -> None

let string_list_field name fields =
  match List.assoc_opt name fields with
  | Some (`List values) ->
    values
    |> List.filter_map (function `String value -> Some value | _ -> None)
  | _ -> []

let json_list_field name fields =
  match List.assoc_opt name fields with
  | Some (`List values) -> values
  | _ -> []

let json_assoc_field name fields =
  match List.assoc_opt name fields with
  | Some (`Assoc _ as value) -> Some value
  | _ -> None

let catalog_entry_json (opcode, name, consensus_status, profile_root, source) =
  let fields = [
    "opcode", `String opcode;
    "name", `String name;
    "consensus_status", `String consensus_status;
    "profile_root", `String profile_root;
  ] in
  let fields =
    match source with
    | None -> fields
    | Some value -> fields @ ["profile_source", `String value]
  in
  `Assoc fields

let rec profile_root_catalog_entries = function
  | `Assoc fields ->
    let direct =
      match
        string_field "opcode" fields,
        string_field "name" fields,
        string_field "consensus_status" fields,
        string_field "profile_root" fields
      with
      | Some opcode, Some name, Some consensus_status, Some profile_root ->
        [
          ( opcode,
            name,
            consensus_status,
            profile_root,
            string_field "profile_source" fields );
        ]
      | _ -> []
    in
    let nested_gate =
      match List.assoc_opt "profile_gate" fields with
      | Some value -> profile_root_catalog_entries value
      | None -> []
    in
    let nested_gates =
      match List.assoc_opt "profile_gates" fields with
      | Some (`List values) ->
        List.fold_left
          (fun entries value -> profile_root_catalog_entries value @ entries)
          []
          values
      | _ -> []
    in
    direct @ nested_gate @ nested_gates
  | `List values ->
    List.fold_left
      (fun entries value -> profile_root_catalog_entries value @ entries)
      []
      values
  | _ -> []

let profile_root_catalog_json values =
  values
  |> List.fold_left
       (fun entries value -> profile_root_catalog_entries value @ entries)
       []
  |> List.sort_uniq compare
  |> List.map catalog_entry_json
  |> fun entries -> `List entries

let optional_root_json = function
  | Some value -> `String value
  | None -> `Null

let validator_readiness_blockers gate_fields binding_fields =
  let root_blockers =
    match string_field "classification" binding_fields with
    | Some "none" -> []
    | Some value -> [value]
    | None -> ["profile_root_unavailable"]
  in
  let profile_blockers =
    match string_field "consensus_status" gate_fields with
    | Some "consensus_ready" -> []
    | Some "consensus_candidate" -> ["consensus_candidate_profile_gate"]
    | Some "local_only" -> ["local_only_profile_gate"]
    | Some _ -> ["unknown_profile_gate"]
    | None -> ["unknown_profile_gate"]
  in
  root_blockers
  @ profile_blockers
  @ string_list_field "consensus_blocker_codes" gate_fields

let binding_catalog_entry ?path ?case ?manifest profile_gate binding =
  match profile_gate, binding with
  | `Assoc gate_fields, `Assoc binding_fields ->
    let readiness_blockers =
      validator_readiness_blockers gate_fields binding_fields
    in
    let fields = [
      "opcode",
      `String
        (match string_field "opcode" gate_fields with
         | Some value -> value
         | None -> "unknown");
      "name",
      `String
        (match string_field "name" gate_fields with
         | Some value -> value
         | None -> "unknown");
      "consensus_status",
      `String
        (match string_field "consensus_status" gate_fields with
         | Some value -> value
         | None -> "unknown");
      "status",
      `String
        (match string_field "status" binding_fields with
         | Some value -> value
         | None -> "unavailable");
      "classification",
      `String
        (match string_field "classification" binding_fields with
         | Some value -> value
         | None -> "profile_root_unavailable");
      "numerical_profile_root",
      optional_root_json (string_field "numerical_profile_root" binding_fields);
      "profile_root",
      optional_root_json (string_field "profile_root" binding_fields);
      "consensus_blocker_codes",
      `List
        (List.map
           (fun blocker -> `String blocker)
           (string_list_field "consensus_blocker_codes" gate_fields));
      "validator_readiness_status",
      `String (if readiness_blockers = [] then "accepted" else "rejected");
      "validator_readiness_blockers",
      `List (List.map (fun blocker -> `String blocker) readiness_blockers);
    ] in
    let fields =
      match string_field "profile_source" gate_fields with
      | Some value -> fields @ ["profile_source", `String value]
      | None -> fields
    in
    let fields =
      match path with
      | Some value -> fields @ ["path", `String value]
      | None -> fields
    in
    let fields =
      match case with
      | Some value -> fields @ ["case", `String value]
      | None -> fields
    in
    let fields =
      match manifest with
      | Some value -> fields @ ["manifest", `String value]
      | None -> fields
    in
    [`Assoc fields]
  | _ -> []

let rec paired_binding_entries ?path ?case ?manifest acc gates bindings =
  match gates, bindings with
  | gate :: gates, binding :: bindings ->
    paired_binding_entries
      ?path
      ?case
      ?manifest
      (binding_catalog_entry ?path ?case ?manifest gate binding @ acc)
      gates
      bindings
  | _ -> List.rev acc

let rec profile_root_binding_catalog_entries = function
  | `Assoc fields ->
    let path =
      match string_field "path" fields with
      | Some value -> Some value
      | None -> string_field "template_path" fields
    in
    let case = string_field "case" fields in
    let manifest = string_field "manifest" fields in
    let direct =
      match
        json_assoc_field "profile_gate" fields,
        json_assoc_field "profile_root_binding" fields
      with
      | Some gate, Some binding ->
        binding_catalog_entry ?path ?case ?manifest gate binding
      | _ -> []
    in
    let paired =
      paired_binding_entries
        ?path
        ?case
        ?manifest
        []
        (json_list_field "profile_gates" fields)
        (json_list_field "profile_root_bindings" fields)
    in
    direct @ paired
  | `List values ->
    List.fold_left
      (fun entries value -> profile_root_binding_catalog_entries value @ entries)
      []
      values
  | _ -> []

let profile_root_binding_catalog_json values =
  values
  |> List.fold_left
       (fun entries value -> profile_root_binding_catalog_entries value @ entries)
       []
  |> List.sort_uniq compare
  |> fun entries -> `List entries

let rec consensus_blocker_pairs = function
  | `Assoc fields ->
    let direct =
      match string_field "opcode" fields with
      | Some opcode ->
        List.map
          (fun blocker -> blocker, opcode)
          (string_list_field "consensus_blocker_codes" fields)
      | None -> []
    in
    let nested_gate =
      match List.assoc_opt "profile_gate" fields with
      | Some value -> consensus_blocker_pairs value
      | None -> []
    in
    let nested_gates =
      match List.assoc_opt "profile_gates" fields with
      | Some (`List values) ->
        List.fold_left
          (fun pairs value -> consensus_blocker_pairs value @ pairs)
          []
          values
      | _ -> []
    in
    direct @ nested_gate @ nested_gates
  | `List values ->
    List.fold_left
      (fun pairs value -> consensus_blocker_pairs value @ pairs)
      []
      values
  | _ -> []

let add_blocker_pair groups (blocker, opcode) =
  let rec loop acc = function
    | [] -> List.rev ((blocker, [opcode]) :: acc)
    | (candidate, opcodes) :: rest when String.equal candidate blocker ->
      let opcodes =
        if List.exists (String.equal opcode) opcodes then opcodes
        else opcode :: opcodes
      in
      List.rev_append acc ((candidate, opcodes) :: rest)
    | entry :: rest -> loop (entry :: acc) rest
  in
  loop [] groups

let consensus_blocker_groups values =
  values
  |> List.fold_left
       (fun pairs value -> consensus_blocker_pairs value @ pairs)
       []
  |> List.sort_uniq compare
  |> List.fold_left add_blocker_pair []
  |> List.sort (fun (left, _) (right, _) -> String.compare left right)

let blocker_class = function
  | "host_fp_exp"
  | "host_fp_arithmetic"
  | "host_fp_exponentiation"
  | "host_fp_log1p"
  | "host_fp_trig" ->
    "host_native_math"
  | "fp64_add_conformance"
  | "fp64_add_sub_conformance"
  | "fp64_comparison_conformance"
  | "fp64_divide_conformance"
  | "fp64_epsilon_add_conformance"
  | "fp64_mul_add_conformance"
  | "fp64_multiply_conformance"
  | "fp64_output_multiply_conformance"
  | "fp64_recurrence_add_mul_conformance"
  | "fp64_reduction_conformance"
  | "fp64_sqrt_conformance"
  | "fp64_subtract_conformance" ->
    "software_fp64_conformance"
  | "accumulation_order"
  | "kernel_accumulation_order"
  | "probability_ordering"
  | "score_accumulation_order"
  | "state_transition_order"
  | "weighted_sum_accumulation_order" ->
    "execution_order"
  | "alias_rejection"
  | "atomic_writeback"
  | "edge_value_policy"
  | "finite_overflow_policy"
  | "finite_rejection"
  | "overlap_policy"
  | "snapshot_alias_policy"
  | "signed_zero_subnormal_policy" ->
    "safety_policy"
  | "authenticated_range_binding" ->
    "storage_binding"
  | "binary16_scale_decode"
  | "epsilon_bit_interpretation"
  | "first_max_tie_policy"
  | "float_byte_decode"
  | "causal_indexing"
  | "position_base_policy"
  | "q1_sign_mapping"
  | "selected_index_encoding"
  | "tail_preservation" ->
    "encoding_or_layout"
  | "activation_branch_policy"
  | "cross_platform_conformance" ->
    "external_qualification"
  | _ -> "unknown"

let blocker_entry_json (blocker, opcodes) =
  let opcodes = List.sort_uniq String.compare opcodes in
  `Assoc [
    "blocker_code", `String blocker;
    "blocker_class", `String (blocker_class blocker);
    "opcode_count", `Int (List.length opcodes);
    "opcodes", `List (List.map (fun opcode -> `String opcode) opcodes);
  ]

let consensus_blocker_catalog_json values =
  values
  |> consensus_blocker_groups
  |> List.map blocker_entry_json
  |> fun entries -> `List entries

let add_blocker_class groups (blocker, opcodes) =
  let class_name = blocker_class blocker in
  let rec loop acc = function
    | [] -> List.rev ((class_name, [blocker], opcodes) :: acc)
    | (candidate, blockers, class_opcodes) :: rest
      when String.equal candidate class_name ->
      let blockers =
        if List.exists (String.equal blocker) blockers then blockers
        else blocker :: blockers
      in
      let class_opcodes =
        List.fold_left
          (fun acc opcode ->
             if List.exists (String.equal opcode) acc then acc
             else opcode :: acc)
          class_opcodes
          opcodes
      in
      List.rev_append acc ((candidate, blockers, class_opcodes) :: rest)
    | entry :: rest -> loop (entry :: acc) rest
  in
  loop [] groups

let blocker_class_count_json (class_name, blockers, opcodes) =
  let blockers = List.sort_uniq String.compare blockers in
  let opcodes = List.sort_uniq String.compare opcodes in
  `Assoc [
    "blocker_class", `String class_name;
    "blocker_count", `Int (List.length blockers);
    "opcode_count", `Int (List.length opcodes);
    "blocker_codes", `List (List.map (fun value -> `String value) blockers);
    "opcodes", `List (List.map (fun value -> `String value) opcodes);
  ]

let consensus_blocker_class_counts_json values =
  values
  |> consensus_blocker_groups
  |> List.fold_left add_blocker_class []
  |> List.sort (fun (left, _, _) (right, _, _) -> String.compare left right)
  |> List.map blocker_class_count_json
  |> fun entries -> `List entries

let static_evidence_blockers = [
  "execution_not_proven";
  "failure_cases_not_proven";
  "effort_not_proven";
  "profile_root_binding_not_proven";
  "cross_platform_not_proven";
]

let profile_gate_readiness_blockers fields =
  let profile_blockers =
    match string_field "consensus_status" fields with
    | Some "consensus_ready" -> []
    | Some "consensus_candidate" -> ["consensus_candidate_profile_gate"]
    | Some "local_only" -> ["local_only_profile_gate"]
    | Some _ -> ["unknown_profile_gate"]
    | None -> ["unknown_profile_gate"]
  in
  profile_blockers
  @ string_list_field "consensus_blocker_codes" fields
  @ static_evidence_blockers
  |> List.sort_uniq String.compare

let consensus_blocker_classes_json fields =
  string_list_field "consensus_blocker_codes" fields
  |> List.map (fun blocker ->
    `Assoc [
      "blocker_code", `String blocker;
      "blocker_class", `String (blocker_class blocker);
    ])
  |> fun entries -> `List entries

let profile_readiness_worklist_entry_json = function
  | `Assoc fields ->
    let blockers = profile_gate_readiness_blockers fields in
    Some
      (`Assoc [
        "opcode",
        `String
          (match string_field "opcode" fields with
           | Some value -> value
           | None -> "unknown");
        "name",
        `String
          (match string_field "name" fields with
           | Some value -> value
           | None -> "unknown");
        "profile_source",
        `String
          (match string_field "profile_source" fields with
           | Some value -> value
           | None -> "current_runtime_profile");
        "consensus_status",
        `String
          (match string_field "consensus_status" fields with
           | Some value -> value
           | None -> "unknown");
        "profile_root",
        optional_root_json (string_field "profile_root" fields);
        "validator_readiness_scope",
        `String "profile_catalog_static";
        "validator_readiness_status",
        `String (if blockers = [] then "accepted" else "rejected");
        "validator_readiness_blockers",
        `List (List.map (fun blocker -> `String blocker) blockers);
        "consensus_blocker_codes",
        `List
          (List.map
             (fun blocker -> `String blocker)
             (string_list_field "consensus_blocker_codes" fields));
        "consensus_blocker_classes",
        consensus_blocker_classes_json fields;
        "consensus_obligations",
        `List
          (List.map
             (fun obligation -> `String obligation)
             (string_list_field "consensus_obligations" fields));
        "evidence_required",
        `List
          (List.map
             (fun blocker -> `String blocker)
             static_evidence_blockers);
      ])
  | _ -> None

let profile_readiness_worklist_json gates =
  gates
  |> List.filter_map profile_readiness_worklist_entry_json
  |> List.sort
       (fun left right ->
          match left, right with
          | `Assoc left_fields, `Assoc right_fields ->
            String.compare
              (match string_field "opcode" left_fields with
               | Some value -> value
               | None -> "")
              (match string_field "opcode" right_fields with
               | Some value -> value
               | None -> "")
          | _ -> 0)
  |> fun entries -> `List entries

let error_message = function
  | Unknown_profile profile -> "unknown numerical profile: " ^ profile
  | Unsupported_opcode_profile { opcode; profile; expected } ->
    Printf.sprintf
      "profile %s is not implemented for opcode %s; expected %s"
      profile
      opcode
      expected

let host_fp_actions = [
  "bind exact reduction order, rounding, non-finite, signed-zero, subnormal, overflow, and aliasing semantics";
  "replace host math with deterministic software arithmetic or qualify this profile as local-only";
  "pass cross-platform conformance before validator admission";
]

let of_name = function
  | "host-fp-local-candidate" as name ->
    Ok {
      name;
      consensus_status = Local_only;
      summary =
        "native host floating point accepted only for local inference proof execution";
      required_actions = host_fp_actions;
    }
  | "host-fp-exp-local-candidate" as name ->
    Ok {
      name;
      consensus_status = Local_only;
      summary =
        "native exponential math accepted only for local inference proof execution";
      required_actions = [
        "replace or qualify native exp/log-style math before validator admission";
        "bind deterministic exponential oracle vectors and profile roots";
        "preserve finite-domain gates, rejection policy, effort, and atomic writeback";
      ];
    }
  | "host-fp-trig-local-candidate" as name ->
    Ok {
      name;
      consensus_status = Local_only;
      summary =
        "native trigonometric and exponentiation math accepted only for local inference proof execution";
      required_actions = [
        "replace or qualify native pow/cos/sin math before validator admission";
        "bind deterministic rotary oracle vectors and profile roots";
        "preserve position/base interpretation, finite rejection, effort, and atomic writeback";
      ];
    }
  | "byte-ingress-exact" as name ->
    Ok {
      name;
      consensus_status = Consensus_candidate;
      summary = "little-endian finite floating-point byte-ingress profile";
      required_actions = [
        "pin little-endian f32/f64 bit interpretation and finite rejection";
        "bind source bytes through authenticated ranges and storage_read effects";
        "preserve decode atomicity before exposing loaded cells to arithmetic kernels";
      ];
    }
  | "deterministic-fp64-comparison" as name ->
    Ok {
      name;
      consensus_status = Consensus_candidate;
      summary = "deterministic finite binary64 comparison profile";
      required_actions = [
        "bind the numerical profile root in the model or request authority";
        "pin cross-platform finite-value ordering, signed-zero tie, and selected-index encoding";
        "preserve non-finite rejection and destination atomicity before validator admission";
      ];
    }
  | "deterministic-fp64-elementwise" as name ->
    Ok {
      name;
      consensus_status = Consensus_candidate;
      summary = "deterministic finite binary64 elementwise arithmetic profile";
      required_actions = [
        "bind the numerical profile root in the model or request authority";
        "qualify software-defined binary64 add and multiply edge vectors across validators";
        "pin signed-zero, subnormal, overflow, aliasing, effort, and atomic writeback policy";
      ];
    }
  | "deterministic-fp64-accumulation" as name ->
    Ok {
      name;
      consensus_status = Consensus_candidate;
      summary = "deterministic finite binary64 accumulation profile";
      required_actions = [
        "bind the numerical profile root in the model or request authority";
        "qualify software-defined binary64 multiply/add reductions and scale transforms across validators";
        "pin loop order, signed-zero, subnormal, overflow, aliasing, effort, and atomic writeback policy";
      ];
    }
  | "deterministic-q1-g128-fp64-linear" as name ->
    Ok {
      name;
      consensus_status = Consensus_candidate;
      summary = "deterministic Q1-G128 binary16-scale linear profile";
      required_actions = [
        "bind the numerical profile root in the model or request authority";
        "qualify binary16 scale decode, Q1 sign mapping, and binary64 accumulator edge vectors across validators";
        "pin loop order, finite rejection, aliasing, effort, and atomic writeback policy";
      ];
    }
  | "deterministic-fp64-normalization" as name ->
    Ok {
      name;
      consensus_status = Consensus_candidate;
      summary = "deterministic finite binary64 normalization profile";
      required_actions = [
        "bind the numerical profile root in the model or request authority";
        "qualify software-defined binary64 reduction, division, sqrt, and output multiply edge vectors across validators";
        "pin epsilon bits, signed-zero, subnormal, overflow, aliasing, effort, and atomic writeback policy";
      ];
    }
  | "q16-exact" as name ->
    Ok {
      name;
      consensus_status = Consensus_candidate;
      summary = "integer Q16 fixed-point profile candidate";
      required_actions = [
        "prove model quality and token-order preservation for each admitted primitive";
        "reject use where explicit epsilon or precision requirements cannot be represented";
      ];
    }
  | "q32-exact" as name ->
    Ok {
      name;
      consensus_status = Consensus_candidate;
      summary = "integer Q32 fixed-point profile candidate";
      required_actions = [
        "define exact scaling, rounding, overflow, and saturation behavior";
        "prove model quality and token-order preservation";
      ];
    }
  | "soft-fp-exact" as name ->
    Ok {
      name;
      consensus_status = Consensus_candidate;
      summary = "software-defined floating-point profile candidate";
      required_actions = [
        "implement software sqrt, exp, log, sin, cos, and arithmetic where used";
        "pin bit-exact scalar oracle and cross-platform conformance";
      ];
    }
  | profile -> Error (Unknown_profile profile)

let current_runtime_profile_entries = [
  "LOAD_F32_LE_FP", "byte-ingress-exact";
  "LOAD_F64_LE_FP", "byte-ingress-exact";
  "LINEAR_Q1_G128_FP", "deterministic-q1-g128-fp64-linear";
  "SIGMOID_FP", "host-fp-exp-local-candidate";
  "SOFTPLUS_FP", "host-fp-exp-local-candidate";
  "SILU_FP", "host-fp-exp-local-candidate";
  "CAUSAL_DEPTHWISE_CONV1D_FP", "deterministic-fp64-accumulation";
  "GATED_DELTA_RULE_FP", "host-fp-exp-local-candidate";
  "RMSNORM_FP_EPS", "deterministic-fp64-normalization";
  "L2NORM_FP", "deterministic-fp64-normalization";
  "ELEMWISE_MUL_FP", "deterministic-fp64-elementwise";
  "RESIDUAL_ADD_FP", "deterministic-fp64-elementwise";
  "ROPE_APPLY_INDEXED_FP", "host-fp-trig-local-candidate";
  "ATTENTION_SCORES_FP", "deterministic-fp64-accumulation";
  "SOFTMAX_FP", "host-fp-exp-local-candidate";
  "ATTENTION_WEIGHTED_SUM_FP", "deterministic-fp64-accumulation";
  "ARGMAX_FP", "deterministic-fp64-comparison";
]

let current_runtime_opcodes =
  List.map fst current_runtime_profile_entries

let current_runtime_profile ~opcode =
  List.assoc_opt opcode current_runtime_profile_entries

let validate_for_opcode ~opcode ~profile =
  match of_name profile, current_runtime_profile ~opcode with
  | Error error, _ -> Error error
  | Ok parsed, None -> Ok parsed
  | Ok parsed, Some expected when String.equal parsed.name expected -> Ok parsed
  | Ok parsed, Some expected ->
    Error (Unsupported_opcode_profile { opcode; profile = parsed.name; expected })

let local_semantics ~opcode =
  match opcode with
  | "LOAD_F32_LE_FP" ->
    [
      "source bytes are read from a string/bytes register";
      "offset is byte-based, non-negative, and must cover count little-endian f32 cells";
      "finite IEEE-754 binary32 values are decoded into binary64 memory cells";
      "NaN and infinity are rejected before writeback";
      "outputs are written only after the complete finite decode buffer is computed";
    ]
  | "LOAD_F64_LE_FP" ->
    [
      "source bytes are read from a string/bytes register";
      "offset is byte-based, non-negative, and must cover count little-endian f64 cells";
      "finite IEEE-754 binary64 bit patterns are copied into binary64 memory cells";
      "NaN and infinity are rejected before writeback";
      "outputs are written only after the complete finite decode buffer is computed";
    ]
  | "LINEAR_Q1_G128_FP" ->
    [
      "Q1-G128 blocks are 18 bytes: little-endian binary16 scale followed by 128 sign bits";
      "binary16 scale decode uses integer bit fields and binary exponent shifts";
      "finite binary16 scales include zero, signed zero, subnormal, normal, and max-finite values";
      "sign bit 1 maps to +1.0 and sign bit 0 maps to -1.0";
      "loop order is row, column, block, item with a deterministic finite binary64 accumulator";
      "input cells, decoded scales, and final outputs must be finite before writeback";
      "destination cells are written only after the full output buffer is computed";
      "the Q1 linear compute path uses no native host floating-point math";
    ]
  | "RMSNORM_FP_EPS" ->
    [
      "epsilon is read from an integer register as binary64 bits and must be finite and positive";
      "minimum positive subnormal epsilon is accepted by the current normalization profile";
      "input and gamma cells are finite binary64 values and their ranges must not overlap";
      "sum of squares is accumulated left-to-right with deterministic finite binary64 multiply/add";
      "mean-square division, epsilon addition, reciprocal division, and output multiply use deterministic finite binary64";
      "sqrt in the inverse RMS path uses deterministic finite binary64";
      "output multiply uses deterministic finite binary64 multiply";
      "outputs are written only after the complete finite output vector is computed";
      "the normalization compute path uses no native host floating-point math";
    ]
  | "L2NORM_FP" ->
    [
      "epsilon is read from an integer register as binary64 bits and must be finite and positive";
      "input cells are finite binary64 values";
      "sum of squares is accumulated left-to-right with deterministic finite binary64 multiply/add";
      "epsilon addition, reciprocal division, and output multiply use deterministic finite binary64";
      "sqrt in the inverse norm path uses deterministic finite binary64";
      "output multiply uses deterministic finite binary64 multiply";
      "outputs are written only after the complete finite output vector is computed";
      "the normalization compute path uses no native host floating-point math";
    ]
  | "SOFTMAX_FP" ->
    [
      "score cells are finite binary64 values";
      "maximum score is selected left-to-right with deterministic finite binary64 comparison before exponentiation";
      "score-minus-maximum shifts use deterministic finite binary64 subtraction";
      "each shifted score must deterministically compare less than or equal to +0.0 before native exp";
      "exp is applied to each shifted score using native binary64";
      "exponentials are summed left-to-right with deterministic finite binary64 addition";
      "probabilities are divided by the binary64 sum of exponentials using deterministic finite binary64 division";
      "destination may equal scores exactly, but partial overlap is rejected";
    ]
  | "SIGMOID_FP" ->
    [
      "input cells are finite binary64 values and are updated in place";
      "nonnegative inputs use exp(-x) and negative inputs use exp(x), so native exp only sees finite nonpositive inputs";
      "1.0 + exp and the final ratio use deterministic finite binary64 add/divide";
      "outputs are written only after the complete finite output vector is computed";
    ]
  | "SOFTPLUS_FP" ->
    [
      "input cells are finite binary64 values and are updated in place";
      "positive inputs use exp(-x) and other inputs use exp(x), so native exp only sees finite nonpositive inputs";
      "log1p receives finite nonnegative inputs produced by the exp branch";
      "positive-branch x + log1p(exp(-x)) uses deterministic finite binary64 addition";
      "outputs are written only after the complete finite output vector is computed";
    ]
  | "SILU_FP" ->
    [
      "input cells are finite binary64 values and are updated in place";
      "SiLU reuses the SIGMOID_FP sign branch and nonpositive native exp-domain gate";
      "x * sigmoid(x) uses deterministic finite binary64 multiplication";
      "outputs are written only after the complete finite output vector is computed";
    ]
  | "ELEMWISE_MUL_FP" ->
    [
      "destination and source cells are finite binary64 values";
      "destination may equal source exactly, but partial overlap is rejected";
      "each output cell is computed with deterministic finite binary64 multiplication";
      "outputs are written only after the complete finite output vector is computed";
      "no native host math is used";
    ]
  | "RESIDUAL_ADD_FP" ->
    [
      "destination and source cells are finite binary64 values";
      "destination may equal source exactly, but partial overlap is rejected";
      "each output cell is computed with deterministic finite binary64 addition";
      "outputs are written only after the complete finite output vector is computed";
      "no native host math is used";
    ]
  | "GATED_DELTA_RULE_FP" ->
    [
      "query, key, value, decay, beta, and recurrent-state cells are finite binary64 values";
      "value heads map to query and key heads by modulo";
      "finite log_decay inputs must deterministically compare less than or equal to +0.0 before native exp";
      "state decay uses native exp(log_decay) over the nonpositive domain and query-scale sqrt uses deterministic finite binary64";
      "integer key-dimension conversion, query-scale reciprocal division, recurrence add/mul, and output scaling multiply use deterministic finite binary64";
      "loop order is timestep, value head, value row, key column";
      "output and next-state cells are written only after both buffers are finite";
    ]
  | "CAUSAL_DEPTHWISE_CONV1D_FP" ->
    [
      "input and kernel cells are finite binary64 values";
      "timesteps, channels, and width must be positive";
      "each output cell uses causal depthwise indexing over matching channels";
      "accumulation is left-to-right by kernel index using deterministic finite binary64 multiplication and addition";
      "input and kernel ranges are snapshotted before output writeback";
      "outputs are written only after the complete finite output tensor is computed";
    ]
  | "ARGMAX_FP" ->
    [
      "input cells are finite binary64 values";
      "comparison uses deterministic finite binary64 greater-than";
      "ties keep the lowest zero-based index, including signed-zero ties";
      "the selected index is written only after the full input span is read";
      "no native host math or floating-point arithmetic is used";
    ]
  | "ROPE_APPLY_INDEXED_FP" ->
    [
      "input cells are finite binary64 values and are updated in place";
      "base is read as binary64, must be finite, and must be greater than 1.0";
      "position cells must be exact signed integers within the binary64-safe integer range";
      "theta uses native binary64 exponentiation before native cos and sin";
      "outputs are written only after the complete finite output vector is computed";
    ]
  | "ATTENTION_SCORES_FP" ->
    [
      "query and key cells are finite binary64 values";
      "each score is a dot product accumulated left-to-right by head dimension";
      "scale is computed as 1.0 / sqrt(head_dim) using deterministic finite binary64";
      "dot products and scaled scores use deterministic finite binary64 multiplication and addition";
      "outputs are written only after the complete finite score vector is computed";
    ]
  | "ATTENTION_WEIGHTED_SUM_FP" ->
    [
      "probability and value cells are finite binary64 values";
      "each output dimension is accumulated left-to-right by key index";
      "probabilities are consumed as provided and are not renormalized";
      "weighted values and reductions use deterministic finite binary64 multiplication and addition";
      "outputs are written only after the complete finite output vector is computed";
    ]
  | _ -> []

let consensus_obligations ~opcode =
  match opcode with
  | "LOAD_F32_LE_FP" ->
    [
      "pin IEEE-754 binary32 to binary64 widening for zero, signed zero, subnormal, normal, and max-finite values";
      "define non-finite rejection, offset/count bounds, writeback atomicity, and effort";
      "bind source bytes to authenticated range roots when loaded through FLOAD";
      "pass independent cross-platform conformance for f32 little-endian ingress edge vectors";
    ]
  | "LOAD_F64_LE_FP" ->
    [
      "pin IEEE-754 binary64 bit preservation for zero, signed zero, subnormal, normal, and max-finite values";
      "define non-finite rejection, offset/count bounds, writeback atomicity, and effort";
      "bind source bytes to authenticated range roots when loaded through FLOAD";
      "pass independent cross-platform conformance for f64 little-endian ingress edge vectors";
    ]
  | "LINEAR_Q1_G128_FP" ->
    [
      "bind the deterministic Q1-G128 linear profile root before consensus admission";
      "pin binary16 scale decode for zero, signed zero, subnormal, normal, NaN, and infinity";
      "qualify deterministic binary64 multiply/add rounding and accumulator behavior";
      "define exact output encoding, overflow policy, and writeback atomicity";
      "pass independent cross-platform conformance for scale, sign, and accumulation edge vectors";
    ]
  | "RMSNORM_FP_EPS" ->
    [
      "bind the deterministic normalization profile root before consensus admission";
      "qualify deterministic binary64 reduction, division, epsilon addition, sqrt, and output multiplication";
      "pin signed-zero, subnormal, overflow, underflow, and non-finite behavior";
      "define exact epsilon-bit interpretation and range-overlap rejection";
      "pass independent cross-platform conformance for reduction and sqrt edge vectors";
    ]
  | "L2NORM_FP" ->
    [
      "bind the deterministic normalization profile root before consensus admission";
      "qualify deterministic binary64 reduction, division, epsilon addition, sqrt, and output multiplication";
      "pin epsilon-bit interpretation, signed-zero, subnormal, overflow, and non-finite behavior";
      "define exact output encoding and in-place writeback atomicity";
      "pass independent cross-platform conformance for inverse-norm edge vectors";
    ]
  | "SOFTMAX_FP" ->
    [
      "replace or qualify native binary64 exp behavior";
      "pin deterministic nonpositive exp-domain gate before native exp";
      "qualify deterministic binary64 score shifting, exponential summation, and probability division";
      "qualify deterministic binary64 comparison for maximum-score selection";
      "pin max-subtract semantics, ties, underflow, overflow, and non-finite rejection";
      "define exact output encoding and overlap/writeback atomicity";
      "pass independent cross-platform conformance for probability and ordering edge vectors";
    ]
  | "SIGMOID_FP" ->
    [
      "replace or qualify native binary64 exp";
      "pin deterministic sign branch and nonpositive exp-domain gate";
      "qualify deterministic binary64 addition and division";
      "pin saturation, signed-zero, subnormal, overflow, and non-finite behavior";
      "define in-place writeback atomicity and effort";
      "pass independent cross-platform conformance for sigmoid edge vectors";
    ]
  | "SOFTPLUS_FP" ->
    [
      "replace or qualify native binary64 exp and log1p";
      "pin deterministic positive/nonpositive branch boundary and nonpositive exp-domain gate";
      "qualify deterministic binary64 positive-branch addition";
      "pin signed-zero, subnormal, overflow, and non-finite behavior";
      "define in-place writeback atomicity and effort";
      "pass independent cross-platform conformance for softplus edge vectors";
    ]
  | "SILU_FP" ->
    [
      "replace or qualify native binary64 exp";
      "pin deterministic sigmoid reuse and nonpositive exp-domain gate";
      "qualify deterministic binary64 addition, division, and multiplication";
      "pin signed-zero, subnormal, overflow, and non-finite behavior";
      "define in-place writeback atomicity and effort";
      "pass independent cross-platform conformance for SiLU edge vectors";
    ]
  | "ELEMWISE_MUL_FP" ->
    [
      "bind the deterministic elementwise profile root before consensus admission";
      "qualify deterministic binary64 multiplication";
      "pin signed-zero, subnormal, overflow, and non-finite behavior";
      "define same-range aliasing, partial-overlap rejection, writeback atomicity, and effort";
      "pass independent cross-platform conformance for elementwise multiply edge vectors";
    ]
  | "RESIDUAL_ADD_FP" ->
    [
      "bind the deterministic elementwise profile root before consensus admission";
      "qualify deterministic binary64 addition";
      "pin signed-zero, subnormal, overflow, and non-finite behavior";
      "define same-range aliasing, partial-overlap rejection, writeback atomicity, and effort";
      "pass independent cross-platform conformance for residual add edge vectors";
    ]
  | "GATED_DELTA_RULE_FP" ->
    [
      "replace or qualify native binary64 exp for state decay";
      "pin deterministic finite nonpositive log_decay gate before native exp";
      "qualify deterministic binary64 recurrence add/mul, dot-product, and reciprocal division behavior";
      "qualify deterministic binary64 query-scale sqrt behavior";
      "pin head mapping, decay order, beta application, state update order, and scaling";
      "define exact output plus next-state encoding, alias rejection, software-fp effort, and atomicity";
      "pass independent cross-platform conformance for recurrent state-transition edge vectors";
    ]
  | "CAUSAL_DEPTHWISE_CONV1D_FP" ->
    [
      "qualify deterministic binary64 multiplication and addition";
      "pin causal depthwise indexing, kernel-order accumulation, finite rejection, and effort";
      "define input/kernel snapshot behavior, output encoding, alias handling, and writeback atomicity";
      "pass independent cross-platform conformance for causal-convolution edge vectors";
    ]
  | "ARGMAX_FP" ->
    [
      "bind the deterministic comparison profile root before consensus admission";
      "qualify the deterministic binary64 comparison relation for signed zero and all finite values";
      "reject non-finite logits before selection and preserve destination on rejection";
      "pin first-maximum tie behavior, selected-index encoding, and effort";
      "prove ordering preservation before mapping fixed-point logits to token authority";
    ]
  | "ROPE_APPLY_INDEXED_FP" ->
    [
      "replace or qualify native binary64 exponentiation, cos, sin, multiply, and add/subtract";
      "pin exact position-cell interpretation, base handling, rotary dimension validation, and zero-position behavior";
      "define in-place writeback atomicity, tail preservation, finite rejection, and effort";
      "pass independent cross-platform conformance for indexed rotary edge vectors";
    ]
  | "ATTENTION_SCORES_FP" ->
    [
      "qualify deterministic binary64 multiplication and addition";
      "qualify deterministic binary64 sqrt and division for score scaling";
      "pin scale semantics, finite rejection, score accumulation order, output atomicity, and effort";
      "define exact output encoding for cancellation, signed-zero, subnormal, and overflow cases";
      "pass independent cross-platform conformance for attention-score edge vectors";
    ]
  | "ATTENTION_WEIGHTED_SUM_FP" ->
    [
      "qualify deterministic binary64 multiplication and addition";
      "pin weighted-sum accumulation order, finite rejection, overlap rejection, writeback atomicity, and effort";
      "define exact output encoding for cancellation, signed-zero, subnormal, and overflow cases";
      "pass independent cross-platform conformance for weighted-sum edge vectors";
    ]
  | _ -> [
      "write primitive-specific deterministic math obligations before promotion";
    ]

let consensus_blocker_codes ~opcode =
  match opcode with
  | "LOAD_F32_LE_FP"
  | "LOAD_F64_LE_FP" ->
    [
      "float_byte_decode";
      "finite_rejection";
      "authenticated_range_binding";
      "atomic_writeback";
      "cross_platform_conformance";
    ]
  | "LINEAR_Q1_G128_FP" ->
    [
      "binary16_scale_decode";
      "q1_sign_mapping";
      "fp64_mul_add_conformance";
      "accumulation_order";
      "finite_overflow_policy";
      "atomic_writeback";
      "cross_platform_conformance";
    ]
  | "RMSNORM_FP_EPS" ->
    [
      "epsilon_bit_interpretation";
      "fp64_reduction_conformance";
      "fp64_epsilon_add_conformance";
      "fp64_sqrt_conformance";
      "fp64_divide_conformance";
      "fp64_output_multiply_conformance";
      "signed_zero_subnormal_policy";
      "alias_rejection";
      "atomic_writeback";
      "cross_platform_conformance";
    ]
  | "L2NORM_FP" ->
    [
      "epsilon_bit_interpretation";
      "fp64_reduction_conformance";
      "fp64_epsilon_add_conformance";
      "fp64_sqrt_conformance";
      "fp64_divide_conformance";
      "fp64_output_multiply_conformance";
      "signed_zero_subnormal_policy";
      "atomic_writeback";
      "cross_platform_conformance";
    ]
  | "SOFTMAX_FP" ->
    [
      "fp64_comparison_conformance";
      "fp64_subtract_conformance";
      "host_fp_exp";
      "fp64_reduction_conformance";
      "fp64_divide_conformance";
      "probability_ordering";
      "overlap_policy";
      "cross_platform_conformance";
    ]
  | "SIGMOID_FP" ->
    [
      "fp64_comparison_conformance";
      "host_fp_exp";
      "fp64_add_conformance";
      "fp64_divide_conformance";
      "activation_branch_policy";
      "signed_zero_subnormal_policy";
      "finite_overflow_policy";
      "atomic_writeback";
      "cross_platform_conformance";
    ]
  | "SOFTPLUS_FP" ->
    [
      "fp64_comparison_conformance";
      "host_fp_exp";
      "host_fp_log1p";
      "fp64_add_conformance";
      "activation_branch_policy";
      "signed_zero_subnormal_policy";
      "finite_overflow_policy";
      "atomic_writeback";
      "cross_platform_conformance";
    ]
  | "SILU_FP" ->
    [
      "fp64_comparison_conformance";
      "host_fp_exp";
      "fp64_add_conformance";
      "fp64_divide_conformance";
      "fp64_multiply_conformance";
      "activation_branch_policy";
      "signed_zero_subnormal_policy";
      "finite_overflow_policy";
      "atomic_writeback";
      "cross_platform_conformance";
    ]
  | "GATED_DELTA_RULE_FP" ->
    [
      "fp64_recurrence_add_mul_conformance";
      "state_transition_order";
      "host_fp_exp";
      "fp64_sqrt_conformance";
      "fp64_divide_conformance";
      "alias_rejection";
      "atomic_writeback";
      "cross_platform_conformance";
    ]
  | "ROPE_APPLY_INDEXED_FP" ->
    [
      "host_fp_exponentiation";
      "host_fp_trig";
      "fp64_multiply_conformance";
      "fp64_add_sub_conformance";
      "position_base_policy";
      "tail_preservation";
      "atomic_writeback";
      "cross_platform_conformance";
    ]
  | "ELEMWISE_MUL_FP" ->
    [
      "fp64_multiply_conformance";
      "signed_zero_subnormal_policy";
      "finite_overflow_policy";
      "overlap_policy";
      "atomic_writeback";
      "cross_platform_conformance";
    ]
  | "RESIDUAL_ADD_FP" ->
    [
      "fp64_add_conformance";
      "signed_zero_subnormal_policy";
      "finite_overflow_policy";
      "overlap_policy";
      "atomic_writeback";
      "cross_platform_conformance";
    ]
  | "ARGMAX_FP" ->
    [
      "fp64_comparison_conformance";
      "first_max_tie_policy";
      "selected_index_encoding";
      "finite_rejection";
      "atomic_writeback";
      "cross_platform_conformance";
    ]
  | "ATTENTION_SCORES_FP" ->
    [
      "fp64_multiply_conformance";
      "fp64_add_conformance";
      "fp64_sqrt_conformance";
      "fp64_divide_conformance";
      "score_accumulation_order";
      "finite_overflow_policy";
      "atomic_writeback";
      "cross_platform_conformance";
    ]
  | "ATTENTION_WEIGHTED_SUM_FP" ->
    [
      "fp64_multiply_conformance";
      "fp64_add_conformance";
      "weighted_sum_accumulation_order";
      "finite_overflow_policy";
      "overlap_policy";
      "atomic_writeback";
      "cross_platform_conformance";
    ]
  | "CAUSAL_DEPTHWISE_CONV1D_FP" ->
    [
      "fp64_multiply_conformance";
      "fp64_add_conformance";
      "causal_indexing";
      "kernel_accumulation_order";
      "finite_overflow_policy";
      "snapshot_alias_policy";
      "atomic_writeback";
      "cross_platform_conformance";
    ]
  | opcode ->
    match current_runtime_profile ~opcode with
    | Some "host-fp-local-candidate" ->
      [
        "host_fp_arithmetic";
        "edge_value_policy";
        "atomic_writeback";
        "cross_platform_conformance";
      ]
    | Some "byte-ingress-exact" ->
      [
        "float_byte_decode";
        "finite_rejection";
        "authenticated_range_binding";
        "atomic_writeback";
        "cross_platform_conformance";
      ]
    | _ -> []

let arithmetic_domain ~profile ~opcode =
  match profile.name, opcode with
  | "byte-ingress-exact", _ -> "ieee754-little-endian-byte-ingress"
  | "q16-exact", _ -> "integer-q16"
  | "q32-exact", _ -> "integer-q32"
  | "soft-fp-exact", _ -> "software-defined-floating-point"
  | "deterministic-q1-g128-fp64-linear", "LINEAR_Q1_G128_FP" ->
    "q1-g128-binary16-scale-deterministic-binary64-accumulator"
  | "deterministic-fp64-normalization", "RMSNORM_FP_EPS" ->
    "deterministic-binary64-reduction-divide-epsilon-sqrt-output-mul"
  | "deterministic-fp64-normalization", "L2NORM_FP" ->
    "deterministic-binary64-reduction-epsilon-sqrt-divide-output-mul"
  | "deterministic-fp64-elementwise", "ELEMWISE_MUL_FP" ->
    "deterministic-binary64-elementwise-multiply"
  | "deterministic-fp64-elementwise", "RESIDUAL_ADD_FP" ->
    "deterministic-binary64-elementwise-add"
  | "host-fp-exp-local-candidate", "SOFTMAX_FP" ->
    "deterministic-binary64-compare-shift-nonpositive-exp-gate-sum-divide-host-exp"
  | "host-fp-exp-local-candidate", "GATED_DELTA_RULE_FP" ->
    "deterministic-binary64-nonpositive-exp-gate-recurrence-sqrt-divide-host-exp"
  | "host-fp-exp-local-candidate", "SIGMOID_FP" ->
    "deterministic-binary64-sigmoid-nonpositive-exp-gate-host-exp"
  | "host-fp-exp-local-candidate", "SOFTPLUS_FP" ->
    "deterministic-binary64-softplus-nonpositive-exp-gate-host-exp-log1p"
  | "host-fp-exp-local-candidate", "SILU_FP" ->
    "deterministic-binary64-silu-nonpositive-exp-gate-host-exp"
  | "host-fp-local-candidate", "ARGMAX_FP" ->
    "deterministic-binary64-comparison"
  | "deterministic-fp64-comparison", "ARGMAX_FP" ->
    "deterministic-binary64-comparison"
  | "deterministic-fp64-accumulation", "ATTENTION_SCORES_FP" ->
    "deterministic-binary64-attention-score-dot-scale"
  | "deterministic-fp64-accumulation", "ATTENTION_WEIGHTED_SUM_FP" ->
    "deterministic-binary64-attention-weighted-sum"
  | "deterministic-fp64-accumulation", "CAUSAL_DEPTHWISE_CONV1D_FP" ->
    "deterministic-binary64-causal-depthwise-convolution"
  | "host-fp-trig-local-candidate", "ROPE_APPLY_INDEXED_FP" ->
    "deterministic-binary64-indexed-rotary-host-pow-cos-sin"
  | "host-fp-local-candidate", _ -> "native-binary64-host-floating-point"
  | name, _ -> name

let rounding_mode ~profile ~opcode =
  match profile.name, opcode with
  | "deterministic-q1-g128-fp64-linear", "LINEAR_Q1_G128_FP" ->
    "deterministic-binary64-roundTiesToEven"
  | ( "deterministic-fp64-normalization",
      ( "RMSNORM_FP_EPS" | "L2NORM_FP" ) ) ->
    "deterministic-binary64-roundTiesToEven"
  | ( "host-fp-exp-local-candidate",
      ( "SOFTMAX_FP" | "GATED_DELTA_RULE_FP" ) ) ->
    "deterministic-binary64-roundTiesToEven-with-host-exp"
  | ( "host-fp-exp-local-candidate",
      ( "SIGMOID_FP"
      | "SOFTPLUS_FP"
      | "SILU_FP" ) ) ->
    "deterministic-binary64-roundTiesToEven-with-host-math"
  | ( "deterministic-fp64-elementwise",
      ( "ELEMWISE_MUL_FP" | "RESIDUAL_ADD_FP" ) ) ->
    "deterministic-binary64-roundTiesToEven"
  | "host-fp-local-candidate", "ARGMAX_FP" ->
    "not-applicable-deterministic-comparison"
  | "deterministic-fp64-comparison", "ARGMAX_FP" ->
    "not-applicable-deterministic-comparison"
  | ( "deterministic-fp64-accumulation",
      ( "ATTENTION_SCORES_FP" | "ATTENTION_WEIGHTED_SUM_FP" ) ) ->
    "deterministic-binary64-roundTiesToEven"
  | "deterministic-fp64-accumulation", "CAUSAL_DEPTHWISE_CONV1D_FP" ->
    "deterministic-binary64-roundTiesToEven"
  | "host-fp-trig-local-candidate", "ROPE_APPLY_INDEXED_FP" ->
    "host-runtime-native-pow-cos-sin"
  | ("q16-exact" | "q32-exact"), _ -> "integer-profile-defined"
  | "soft-fp-exact", _ -> "software-profile-defined"
  | "byte-ingress-exact", _ -> "exact-byte-decode"
  | "host-fp-local-candidate", _ -> "host-runtime-native"
  | _ -> "profile-defined"

let operation_sequence ~opcode =
  match opcode with
  | "LINEAR_Q1_G128_FP" ->
    [
      "snapshot_lhs_and_q1_before_output";
      "read_q1_g128_blocks_18_bytes_each";
      "decode_q1_g128_scale_fp16_le_exact";
      "decode_q1_g128_sign_bits_lsb0_one_is_positive";
      "iterate_row_col_block_item";
      "multiply_lhs_sign_scale";
      "multiply_round_nearest_ties_to_even";
      "accumulate_left_to_right";
      "add_round_nearest_ties_to_even";
      "finite_output_check";
      "atomic_output_writeback";
    ]
  | "RMSNORM_FP_EPS" ->
    [
      "read_epsilon_binary64_bits";
      "snapshot_input_and_gamma";
      "sum_squares_left_to_right";
      "divide_by_count_deterministic";
      "add_epsilon_deterministic";
      "sqrt_deterministic";
      "reciprocal_divide_deterministic";
      "multiply_input_inverse_gamma_deterministic";
      "finite_output_check";
      "atomic_output_writeback";
    ]
  | "L2NORM_FP" ->
    [
      "read_epsilon_binary64_bits";
      "snapshot_input";
      "sum_squares_left_to_right";
      "add_epsilon_deterministic";
      "sqrt_deterministic";
      "reciprocal_divide_deterministic";
      "multiply_input_inverse_deterministic";
      "finite_output_check";
      "atomic_output_writeback";
    ]
  | "SOFTMAX_FP" ->
    [
      "snapshot_scores";
      "select_max_left_to_right_deterministic";
      "subtract_max_deterministic";
      "check_shifted_score_nonpositive_deterministic";
      "exp_each_score_host";
      "sum_exponentials_left_to_right_deterministic";
      "divide_each_exponential_by_sum_deterministic";
      "finite_output_check";
      "atomic_output_writeback";
    ]
  | "SIGMOID_FP" ->
    [
      "snapshot_input";
      "select_exp_branch_by_deterministic_sign_compare";
      "check_exp_input_nonpositive_deterministic";
      "compute_exp_host";
      "add_one_plus_exp_deterministic";
      "divide_ratio_deterministic";
      "finite_output_check";
      "atomic_output_writeback";
    ]
  | "SOFTPLUS_FP" ->
    [
      "snapshot_input";
      "select_positive_or_nonpositive_branch_deterministic";
      "check_exp_input_nonpositive_deterministic";
      "compute_exp_host";
      "check_log1p_input_nonnegative_deterministic";
      "compute_log1p_host";
      "add_positive_branch_tail_deterministic";
      "finite_output_check";
      "atomic_output_writeback";
    ]
  | "SILU_FP" ->
    [
      "snapshot_input";
      "compute_sigmoid_with_deterministic_sign_branch";
      "multiply_input_sigmoid_deterministic";
      "finite_output_check";
      "atomic_output_writeback";
    ]
  | "GATED_DELTA_RULE_FP" ->
    [
      "snapshot_operands_and_state";
      "iterate_timestep_value_head_row_column";
      "check_log_decay_nonpositive_deterministic";
      "compute_decay_host";
      "compute_query_scale_sqrt_deterministic";
      "compute_query_scale_reciprocal_deterministic";
      "apply_state_decay_deterministic";
      "compute_memory_dot_deterministic";
      "apply_beta_delta_deterministic";
      "update_state_deterministic";
      "compute_output_dot_deterministic";
      "apply_query_scale_multiply_deterministic";
      "finite_output_and_state_check";
      "atomic_output_and_state_writeback";
    ]
  | "ARGMAX_FP" ->
    [
      "snapshot_logits";
      "select_first_max_left_to_right_deterministic";
      "write_selected_index";
    ]
  | "ATTENTION_SCORES_FP" ->
    [
      "snapshot_query_and_keys";
      "compute_query_scale_sqrt_deterministic";
      "compute_query_scale_reciprocal_deterministic";
      "accumulate_dot_product_left_to_right_deterministic";
      "multiply_score_by_scale_deterministic";
      "finite_output_check";
      "atomic_output_writeback";
    ]
  | "ATTENTION_WEIGHTED_SUM_FP" ->
    [
      "snapshot_probabilities_and_values";
      "accumulate_weighted_values_left_to_right_deterministic";
      "finite_output_check";
      "atomic_output_writeback";
    ]
  | "CAUSAL_DEPTHWISE_CONV1D_FP" ->
    [
      "snapshot_input_and_kernel";
      "iterate_timestep_channel_kernel_left_to_right";
      "skip_future_kernel_positions";
      "multiply_input_kernel_deterministic";
      "accumulate_kernel_left_to_right_deterministic";
      "finite_output_check";
      "atomic_output_writeback";
    ]
  | _ ->
    [
      "primitive_defined_snapshot";
      "primitive_defined_compute";
      "finite_output_check";
      "atomic_writeback";
    ]

let edge_value_policy ~opcode =
  match opcode with
  | "LINEAR_Q1_G128_FP" ->
    [
      "q1_block_is_18_bytes_little_endian_binary16_scale_then_128_lsb0_sign_bits";
      "reject_nonfinite_binary16_scale";
      "accept_finite_binary16_zero_signed_zero_subnormal_normal_max";
      "sign_bit_1_maps_to_positive_scale_and_0_maps_to_negative_scale";
      "preserve_gradual_underflow";
      "multiplication_signed_zero_uses_xor_sign";
      "addition_exact_nonzero_cancellation_returns_positive_zero";
      "reject_missing_or_nonfinite_lhs";
      "reject_nonfinite_output";
      "snapshot_lhs_and_q1_before_output_writeback";
      "preserve_destination_on_reject";
    ]
  | "RMSNORM_FP_EPS" | "L2NORM_FP" ->
    [
      "epsilon_must_decode_to_finite_positive_binary64";
      "reject_missing_or_nonfinite_operands";
      "reject_nonfinite_reduction_inverse_root_input_or_output";
      "preserve_destination_on_reject";
    ]
  | "ARGMAX_FP" ->
    [
      "reject_missing_or_nonfinite_operands";
      "signed_zero_values_compare_equal";
      "preserve_first_index_on_equal_max";
      "preserve_destination_on_reject";
    ]
  | "SOFTMAX_FP" ->
    [
      "reject_missing_or_nonfinite_operands";
      "reject_positive_shifted_exp_input";
      "reject_nonpositive_or_nonfinite_exp_sum";
      "preserve_destination_on_reject";
    ]
  | "SIGMOID_FP" ->
    [
      "reject_missing_or_nonfinite_operands";
      "native_exp_input_must_be_finite_and_nonpositive";
      "reject_nonfinite_output";
      "preserve_destination_on_reject";
    ]
  | "SOFTPLUS_FP" ->
    [
      "reject_missing_or_nonfinite_operands";
      "native_exp_input_must_be_finite_and_nonpositive";
      "native_log1p_input_must_be_finite_and_nonnegative";
      "reject_nonfinite_output";
      "preserve_destination_on_reject";
    ]
  | "SILU_FP" ->
    [
      "reject_missing_or_nonfinite_operands";
      "reuse_sigmoid_nonpositive_exp_gate";
      "reject_nonfinite_output";
      "preserve_destination_on_reject";
    ]
  | "GATED_DELTA_RULE_FP" ->
    [
      "reject_missing_or_nonfinite_operands";
      "reject_positive_log_decay_before_state_mutation";
      "accept_negative_zero_log_decay";
      "reject_nonfinite_output_or_next_state";
      "preserve_destination_and_next_state_on_reject";
    ]
  | "ATTENTION_SCORES_FP" ->
    [
      "reject_missing_or_nonfinite_operands";
      "reject_nonfinite_or_overflowed_score";
      "reject_output_input_overlap";
      "preserve_destination_on_reject";
    ]
  | "ATTENTION_WEIGHTED_SUM_FP" ->
    [
      "reject_missing_or_nonfinite_operands";
      "reject_nonfinite_or_overflowed_output";
      "reject_output_input_overlap";
      "preserve_destination_on_reject";
    ]
  | "CAUSAL_DEPTHWISE_CONV1D_FP" ->
    [
      "reject_missing_or_nonfinite_operands";
      "reject_invalid_shape_or_effort";
      "reject_nonfinite_or_overflowed_output";
      "input_and_kernel_are_snapshotted_before_writeback";
      "preserve_destination_on_reject";
    ]
  | _ ->
    [
      "reject_missing_or_nonfinite_operands";
      "reject_nonfinite_output";
      "preserve_destination_on_reject";
    ]

let oracle_vector_root ~opcode =
  let vectors =
    match opcode with
    | "LINEAR_Q1_G128_FP" ->
      [
        "golden_q1_g128";
        "sign_and_scale_edges";
        "accumulation_order_stress";
        "overflow_revert";
        "invalid_lhs_revert";
        "invalid_scale_revert";
        "shape_effort_revert";
        "offset_snapshot_overlap";
      ]
    | "RMSNORM_FP_EPS" ->
      [
        "model_epsilon";
        "epsilon_1e_5";
        "signed_zero_subnormal";
        "minimum_subnormal_epsilon";
        "reduction_order_stress";
        "row_composition";
        "invalid_epsilon_revert";
        "alias_effort_revert";
        "overflow_revert";
        "inverse_root_overflow_revert";
      ]
    | "L2NORM_FP" ->
      [
        "model_epsilon";
        "minimum_subnormal_epsilon";
        "row_composition";
        "invalid_epsilon_revert";
        "shape_effort_revert";
        "overflow_revert";
        "inverse_root_overflow_revert";
      ]
    | "ELEMWISE_MUL_FP" ->
      [
        "golden";
        "same_range_alias";
        "missing_operand_revert";
        "nonfinite_operand_revert";
        "partial_overlap_revert";
        "overflow_revert";
        "effort_revert";
      ]
    | "RESIDUAL_ADD_FP" ->
      [
        "golden";
        "same_range_alias";
        "missing_operand_revert";
        "nonfinite_operand_revert";
        "partial_overlap_revert";
        "overflow_revert";
        "effort_revert";
      ]
    | "SOFTMAX_FP" ->
      [
        "equal_scores";
        "exact_inplace";
        "equal_max_scores";
        "ordinary_near_tie";
        "dominated_score_underflow";
        "missing_score_revert";
        "nonfinite_score_revert";
        "partial_overlap_revert";
        "bad_count_revert";
        "effort_revert";
      ]
    | "SIGMOID_FP" ->
      [
        "golden";
        "negative_branch_nonpositive_exp";
        "positive_branch_nonpositive_exp";
        "signed_zero";
        "large_magnitude_saturation";
        "missing_operand_revert";
        "nonfinite_operand_revert";
        "effort_revert";
      ]
    | "SOFTPLUS_FP" ->
      [
        "golden";
        "positive_branch";
        "nonpositive_branch";
        "signed_zero";
        "large_magnitude";
        "missing_operand_revert";
        "nonfinite_operand_revert";
        "effort_revert";
      ]
    | "SILU_FP" ->
      [
        "golden";
        "sigmoid_branch_reuse";
        "signed_zero";
        "large_magnitude_saturation";
        "missing_operand_revert";
        "nonfinite_operand_revert";
        "effort_revert";
      ]
    | "ARGMAX_FP" ->
      [
        "finite_argmax";
        "equal_max_tie";
        "signed_zero_tie";
        "negative_and_subnormal_ordering";
        "nonfinite_revert";
        "empty_revert";
        "effort_revert";
      ]
    | "GATED_DELTA_RULE_FP" ->
      [
        "zero_state_one_timestep";
        "nonzero_state_one_timestep";
        "zero_state_multiple_timesteps";
        "nonzero_state_multiple_timesteps";
        "irregular_dimensions";
        "signed_zero_subnormal_one_timestep";
        "negative_zero_decay_one_timestep";
        "state_in_place_alias";
        "alias_rejections";
        "missing_nonfinite_reverts";
        "positive_log_decay_revert";
        "invalid_shape_effort_revert";
        "late_add_mul_overflow_revert";
      ]
    | "ATTENTION_SCORES_FP" ->
      [
        "golden";
        "query_key_aliasing";
        "left_to_right_accumulation";
        "missing_operand_revert";
        "nonfinite_operand_revert";
        "overflow_revert";
        "output_overlap_revert";
        "shape_effort_revert";
      ]
    | "ATTENTION_WEIGHTED_SUM_FP" ->
      [
        "golden";
        "left_to_right_accumulation";
        "missing_operand_revert";
        "nonfinite_operand_revert";
        "overflow_revert";
        "output_overlap_revert";
        "shape_effort_revert";
      ]
    | "CAUSAL_DEPTHWISE_CONV1D_FP" ->
      [
        "golden";
        "causal_indexing";
        "channel_isolation";
        "left_to_right_accumulation";
        "input_alias_snapshot";
        "kernel_alias_snapshot";
        "missing_operand_revert";
        "nonfinite_operand_revert";
        "overflow_revert";
        "shape_effort_revert";
      ]
    | _ -> ["profile_gate_only"]
  in
  let payload =
    `Assoc [
      "schema", `String "octra.inference.oracle-vectors.v1";
      "opcode", `String opcode;
      "vectors", `List (List.map (fun value -> `String value) vectors);
    ]
    |> Yojson.Safe.to_string
  in
  Digestif.SHA256.(
    digest_string ("octra:inference:oracle-vectors\000" ^ payload) |> to_hex)

let contract_json_for_opcode ~opcode profile =
  let q1_effort_policy =
    match opcode with
    | "LINEAR_Q1_G128_FP" ->
      [
        "effort_policy",
        `Assoc [
          "static_cost", `Int 200;
          "dynamic_cost", `String "floor(m * n * k / 512)";
          "total_cost", `String "200 + floor(m * n * k / 512)";
          "dimension_names", `List [`String "m"; `String "k"; `String "n"];
          "overflow_policy", `String "reject_before_writeback";
        ];
      ]
    | _ -> []
  in
  `Assoc ([
    "schema", `String "octra.inference.numerical-contract.v1";
    "profile_name", `String profile.name;
    "opcode", `String opcode;
    "arithmetic_domain", `String (arithmetic_domain ~profile ~opcode);
    "rounding_mode", `String (rounding_mode ~profile ~opcode);
    "operation_sequence",
    `List (List.map (fun value -> `String value) (operation_sequence ~opcode));
    "edge_value_policy",
    `List (List.map (fun value -> `String value) (edge_value_policy ~opcode));
    "overflow_policy", `String "reject_nonfinite_before_writeback";
    "writeback_policy", `String "atomic_after_successful_full_output";
    "oracle_vector_root", `String (oracle_vector_root ~opcode);
  ] @ q1_effort_policy)

let root_for_opcode ~opcode profile =
  let payload =
    Yojson.Safe.to_string (contract_json_for_opcode ~opcode profile)
  in
  Digestif.SHA256.(
    digest_string ("octra:inference:numerical-contract\000" ^ payload) |> to_hex)

let profile_set_root_for_opcodes ~opcodes profile =
  let entries =
    opcodes
    |> List.sort_uniq String.compare
    |> List.map (fun opcode ->
      `Assoc [
        "opcode", `String opcode;
        "profile_root", `String (root_for_opcode ~opcode profile);
      ])
  in
  let payload =
    `Assoc [
      "schema", `String "octra.inference.numerical-profile-set.v1";
      "entries", `List entries;
    ]
    |> Yojson.Safe.to_string
  in
  Digestif.SHA256.(
    digest_string ("octra:inference:numerical-profile-set\000" ^ payload)
    |> to_hex)

let profile_gate_core_json ~opcode profile =
  `Assoc [
    "name", `String profile.name;
    "consensus_status", `String (status_string profile.consensus_status);
    "summary", `String profile.summary;
    "required_actions",
    `List (List.map (fun action -> `String action) profile.required_actions);
    "opcode", `String opcode;
    "local_semantics",
    `List
      (List.map
         (fun value -> `String value)
         (local_semantics ~opcode));
    "consensus_obligations",
    `List
      (List.map
         (fun value -> `String value)
         (consensus_obligations ~opcode));
    "consensus_blocker_codes",
    `List
      (List.map
         (fun value -> `String value)
         (consensus_blocker_codes ~opcode));
  ]

let profile_root_of_json = function
  | `Assoc fields ->
    (match List.assoc_opt "profile_root" fields with
     | Some (`String root) -> Some root
     | _ -> None)
  | _ -> None

let root_binding_json ~numerical_profile_root profile_gate =
  let profile_root = profile_root_of_json profile_gate in
  let status =
    match profile_root with
    | Some root when String.equal root numerical_profile_root -> "matched"
    | Some _ -> "unbound"
    | None -> "unavailable"
  in
  let classification =
    match profile_root with
    | Some root when String.equal root numerical_profile_root -> "none"
    | Some _ -> "profile_root_mismatch"
    | None -> "profile_root_unavailable"
  in
  `Assoc [
    "status", `String status;
    "classification", `String classification;
    "numerical_profile_root", `String numerical_profile_root;
    "profile_root",
    (match profile_root with
     | None -> `Null
     | Some root -> `String root);
  ]

let unavailable_root_binding_json =
  `Assoc [
    "status", `String "unavailable";
    "classification", `String "profile_root_unavailable";
    "numerical_profile_root", `Null;
    "profile_root", `Null;
  ]

let to_json profile =
  `Assoc [
    "name", `String profile.name;
    "consensus_status", `String (status_string profile.consensus_status);
    "summary", `String profile.summary;
    "required_actions",
    `List (List.map (fun action -> `String action) profile.required_actions);
  ]

let to_json_for_opcode ~opcode profile =
  match profile_gate_core_json ~opcode profile with
  | `Assoc fields ->
    `Assoc
      (fields
       @ [
         "profile_contract", contract_json_for_opcode ~opcode profile;
         "profile_root", `String (root_for_opcode ~opcode profile);
       ])
  | value -> value

let current_runtime_profile_gate ~opcode =
  match current_runtime_profile ~opcode with
  | None -> None
  | Some name ->
    (match of_name name with
     | Ok profile -> Some (to_json_for_opcode ~opcode profile)
     | Error _ -> None)

let profile_catalog_root json =
  let payload = Yojson.Safe.to_string json in
  Digestif.SHA256.(
    digest_string ("octra:inference:profile-catalog\000" ^ payload) |> to_hex)

let current_runtime_profile_catalog_json ~opcodes =
  let opcodes = List.sort_uniq String.compare opcodes in
  let gates =
    opcodes
    |> List.filter_map (fun opcode -> current_runtime_profile_gate ~opcode)
  in
  let status_counts = status_counts_of_json_gates gates in
  let root_catalog = profile_root_catalog_json gates in
  let blocker_catalog = consensus_blocker_catalog_json gates in
  let report =
    `Assoc [
      "schema", `String "octra.inference.profile-catalog.v1";
      "diagnostic_only", `Bool true;
      "profile_source", `String "current_runtime_profile";
      "opcode_count", `Int (List.length opcodes);
      "opcodes", `List (List.map (fun opcode -> `String opcode) opcodes);
      "profile_consensus_status_counts", status_counts_json status_counts;
      "profile_root_catalog", root_catalog;
      "consensus_blocker_catalog", blocker_catalog;
      "consensus_blocker_class_counts",
      consensus_blocker_class_counts_json gates;
      "profile_readiness_worklist", profile_readiness_worklist_json gates;
    ]
  in
  match report with
  | `Assoc fields ->
    `Assoc (fields @ ["profile_catalog_root", `String (profile_catalog_root report)])
  | _ -> report

let profile_catalog_opcodes values =
  values
  |> List.fold_left
       (fun opcodes value ->
          List.map
            (fun (opcode, _, _, _, _) -> opcode)
            (profile_root_catalog_entries value)
          @ opcodes)
       []
  |> List.sort_uniq String.compare

let profile_catalog_root_json values =
  match profile_catalog_opcodes values with
  | [] -> `Null
  | opcodes ->
    (match current_runtime_profile_catalog_json ~opcodes with
     | `Assoc fields ->
       (match List.assoc_opt "profile_catalog_root" fields with
        | Some (`String _ as root) -> root
        | _ -> `Null)
     | _ -> `Null)
