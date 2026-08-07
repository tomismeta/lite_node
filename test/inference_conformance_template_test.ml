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
module Abi = Octra_vm.Inference_session_abi

let check label condition =
  if not condition then failwith label

let hex_root char =
  String.make 64 char

let hex_char = function
  | '0'..'9'
  | 'a'..'f' -> true
  | _ -> false

let root_ok value =
  String.length value = 64 && String.for_all hex_char value

let register register name kind value =
  `Assoc [
    "register", `Int register;
    "name", `String name;
    "kind", `String kind;
    "value", `String value;
  ]

let memory name access =
  `Assoc [
    "name", `String name;
    "path", `String ("fixtures/" ^ name ^ ".bin");
    "root", `String (hex_root 'a');
    "byte_length", `Int 32;
    "sha256", `String (hex_root 'c');
    "encoding", `String "f64le";
    "base", `Int 100;
    "cells", `Int 4;
    "access", `String access;
  ]

let failure case =
  `Assoc [
    "case", `String case;
    "expected", `String "reject_before_write";
    "mutations", `List [`String "input[0]=nan"];
    "unchanged_spans", `List [`String "output"];
  ]

let assoc_value name fields =
  match List.assoc_opt name fields with
  | Some value -> value
  | None -> failwith ("missing json field: " ^ name)

let string_value name fields =
  match assoc_value name fields with
  | `String value -> value
  | _ -> failwith ("json field must be a string: " ^ name)

let int_value name fields =
  match assoc_value name fields with
  | `Int value -> value
  | _ -> failwith ("json field must be an int: " ^ name)

let string_list_value name fields =
  match assoc_value name fields with
  | `List values ->
    List.map
      (function
        | `String value -> value
        | _ -> failwith ("json field must be a string list: " ^ name))
      values
  | _ -> failwith ("json field must be a list: " ^ name)

let list_equal left right =
  List.length left = List.length right
  && List.for_all2 String.equal left right

let contains_substring needle value =
  let needle_len = String.length needle in
  let value_len = String.length value in
  let rec loop index =
    if index + needle_len > value_len then false
    else if String.sub value index needle_len = needle then true
    else loop (index + 1)
  in
  needle_len = 0 || loop 0

let list_contains_substring needle values =
  List.exists (contains_substring needle) values

let check_protocol_owned_transcendental_obligation opcode gate =
  let obligations = string_list_value "consensus_obligations" gate in
  check
    (opcode ^ " requires protocol-owned math")
    (list_contains_substring "protocol-owned deterministic" obligations);
  check
    (opcode ^ " has no native qualification escape hatch")
    (not (list_contains_substring "replace or qualify" obligations));
  check
    (opcode ^ " has no native consensus evaluation")
    (not (list_contains_substring "before native" obligations))

let string_list_field name fields =
  match assoc_value name fields with
  | `List values ->
    List.map
      (function
        | `String value -> value
        | _ -> failwith ("json field must be a string list: " ^ name))
      values
  | _ -> failwith ("json field must be a list: " ^ name)

let abi_template
    ?(session_abi_root = Abi.v1_root)
    ?(entrypoint = Abi.advance_entrypoint)
    ?(label = Abi.advance_label)
    ?(output_base_register = "r0")
    ?(output_count_register = "r1")
    ?(output_count_unit = "cells")
    ?(request_input_root_cell = Abi.input_root_cell)
    ?(r0 = 10000)
    ?(r1 = 6)
    () =
  `Assoc [
    "abi",
    `Assoc [
      "session_abi_root", `String session_abi_root;
      "entrypoint", `String entrypoint;
      "label", `Int label;
      "output_base_register", `String output_base_register;
      "output_count_register", `String output_count_register;
      "output_count_unit", `String output_count_unit;
      "request_input_root_cell", `Int request_input_root_cell;
    ];
    "output",
    `Assoc [
      "base_address", `Int 10000;
      "length_f64_cells", `Int 6;
      "abi_registers", `Assoc ["r0", `Int r0; "r1", `Int r1];
    ];
  ]

let template ?(opcode = "RMSNORM_FP_EPS") ?(primitive = "rmsnorm_fp_eps")
    ?(effects = ["memory_read"; "memory_write"]) ?(memory_access = "read_write")
    ?profile ?(vm_semantics_root = hex_root 'd')
    ?(numerical_profile_root = hex_root 'e')
    ?(failure_cases = [failure "nonfinite_input_nan"]) () =
  let profile =
    match profile, Profile.current_runtime_profile ~opcode with
    | Some profile, _ -> profile
    | None, Some profile -> profile
    | None, None -> "host-fp-local-candidate"
  in
  `Assoc [
    "type", `String "litenode_vm_conformance_template";
    "schema", `Int 1;
    "opcode", `String opcode;
    "primitive", `String primitive;
    "profile", `String profile;
    "vm_semantics_root", `String vm_semantics_root;
    "numerical_profile_root", `String numerical_profile_root;
    "expected_effort", `Int 16;
    "effects", `List (List.map (fun effect -> `String effect) effects);
    "registers",
    `List [
      register 0 "addr" "address" "100";
      register 1 "count" "usize" "4";
      register 2 "gamma" "address" "200";
      register 3 "epsilon_bits" "u64" "4517329193108106637";
    ];
    "memory", `List [memory "input" memory_access];
    "expected",
    `Assoc [
      "spans",
      `List [
        `Assoc [
          "name", `String "output";
          "base", `Int 100;
          "cells", `Int 4;
          "output_root", `String (hex_root 'b');
          "output_sha256", `Null;
        ];
      ];
    ];
    "failure_cases", `List failure_cases;
  ]

let check_accepts_template () =
  match Template.of_json (template ()) with
  | Error error -> failwith (Template.error_message error)
  | Ok result ->
    check "p0 opcode count" (List.length Template.p0_opcodes = 13);
    check "opcode" (String.equal result.Template.opcode "RMSNORM_FP_EPS");
    check "register count" (List.length result.registers = 4);
    check "memory count" (List.length result.memory = 1);
    check "expected span count" (List.length result.expected = 1);
    check "failure count" (List.length result.failure_cases = 1);
    match Template.to_json result with
    | `Assoc fields ->
      check
        "diagnostic only"
        (List.mem_assoc "diagnostic_only" fields);
      let consensus_status =
        match List.assoc_opt "consensus_status" fields with
        | Some (`String status) -> status
        | _ -> "missing"
      in
      check
        ("consensus status " ^ consensus_status)
        (String.equal consensus_status "consensus_ready");
      check
        "profile root binding"
        (match List.assoc_opt "profile_root_binding" fields with
         | Some (`Assoc binding) ->
           String.equal (string_value "status" binding) "unbound"
           && String.equal
                (string_value "numerical_profile_root" binding)
                (hex_root 'e')
           && root_ok (string_value "profile_root" binding)
         | _ -> false);
      check
        "profile gate"
        (match List.assoc_opt "profile_gate" fields with
         | Some (`Assoc gate) ->
           String.equal (string_value "opcode" gate) "RMSNORM_FP_EPS"
           && root_ok (string_value "profile_root" gate)
           && list_contains_substring
                "sqrt"
                (string_list_value "consensus_obligations" gate)
           && list_contains_substring
                "epsilon"
                (string_list_value "local_semantics" gate)
           && list_contains_substring
                "minimum positive subnormal epsilon"
                (string_list_value "local_semantics" gate)
           && List.mem
                "fp64_sqrt_conformance"
                (string_list_value "consensus_blocker_codes" gate)
           && List.mem
                "fp64_reduction_conformance"
                (string_list_value "consensus_blocker_codes" gate)
         | _ -> false)
    | _ -> failwith "template json must be object"

let check_q1_profile_obligations () =
  match
    Template.of_json
      (template
         ~opcode:"LINEAR_Q1_G128_FP"
         ~primitive:"linear_q1_0_g128_fp"
         ())
  with
  | Error error -> failwith (Template.error_message error)
  | Ok result ->
    (match Template.to_json result with
     | `Assoc fields ->
       (match List.assoc_opt "profile_gate" fields with
        | Some (`Assoc gate) ->
          let contract =
            match List.assoc_opt "profile_contract" gate with
            | Some (`Assoc contract) -> contract
            | _ -> failwith "missing q1 profile contract"
          in
          check
            "q1 profile opcode"
            (String.equal (string_value "opcode" gate) "LINEAR_Q1_G128_FP");
          check
            "q1 local sign semantics"
            (list_contains_substring
               "sign bit"
               (string_list_value "local_semantics" gate));
          check
            "q1 local scale semantics"
            (list_contains_substring
               "signed zero"
               (string_list_value "local_semantics" gate));
          check
            "q1 consensus binary16 obligation"
            (list_contains_substring
               "binary16 scale"
               (string_list_value "consensus_obligations" gate));
          check
            "q1 consensus ready"
            (String.equal
               (string_value "consensus_status" gate)
               "consensus_ready");
          check
            "q1 deterministic profile"
            (String.equal
               (string_value "name" gate)
               "deterministic-q1-g128-fp64-linear");
          check
            "q1 records no native fp math"
            (list_contains_substring
               "no native host floating-point math"
               (string_list_value "local_semantics" gate));
          check
            "q1 blocker code"
            (List.mem
               "binary16_scale_decode"
               (string_list_value "consensus_blocker_codes" gate));
          check
            "q1 fp64 conformance blocker code"
            (List.mem
               "fp64_mul_add_conformance"
               (string_list_value "consensus_blocker_codes" gate));
          check
            "q1 contract rounding mode"
            (String.equal
               (string_value "rounding_mode" contract)
               "deterministic-binary64-roundTiesToEven");
          check
            "q1 contract block layout"
            (list_contains_substring
               "18_bytes"
               (string_list_value "edge_value_policy" contract));
          check
            "q1 contract sign polarity"
            (list_contains_substring
               "1_maps_to_positive"
               (string_list_value "edge_value_policy" contract));
          check
            "q1 contract snapshots inputs"
            (list_contains_substring
               "snapshot_lhs_and_q1"
               (string_list_value "operation_sequence" contract));
          check
            "q1 contract pins product rounding"
            (List.mem
               "multiply_round_nearest_ties_to_even"
               (string_list_value "operation_sequence" contract));
          check
            "q1 contract pins add rounding"
            (List.mem
               "add_round_nearest_ties_to_even"
               (string_list_value "operation_sequence" contract));
          check
            "q1 contract pins gradual underflow"
            (List.mem
               "preserve_gradual_underflow"
               (string_list_value "edge_value_policy" contract));
          check
            "q1 contract pins signed-zero product"
            (List.mem
               "multiplication_signed_zero_uses_xor_sign"
               (string_list_value "edge_value_policy" contract));
          check
            "q1 contract pins exact cancellation zero"
            (List.mem
               "addition_exact_nonzero_cancellation_returns_positive_zero"
               (string_list_value "edge_value_policy" contract));
          let effort_policy =
            match List.assoc_opt "effort_policy" contract with
            | Some (`Assoc effort_policy) -> effort_policy
            | _ -> failwith "missing q1 effort policy"
          in
          check
            "q1 effort static cost"
            (int_value "static_cost" effort_policy = 200);
          check
            "q1 effort dynamic formula"
            (String.equal
               (string_value "dynamic_cost" effort_policy)
               "floor(m * n * k / 512)");
          check
            "q1 effort total formula"
            (String.equal
               (string_value "total_cost" effort_policy)
               "200 + floor(m * n * k / 512)")
        | _ -> failwith "missing q1 profile gate");
       check
         "q1 vm semantics root"
         (match
            Template.vm_semantics_root_for_opcode
              ~opcode:"LINEAR_Q1_G128_FP"
          with
          | Some root ->
            let expected =
              "db31cbfb8754b8a4b53867f338497e57baee17a64d8fa957108fefe1e93e5da7"
            in
            if String.equal root expected then true
            else
              failwith
                ("q1 vm semantics root observed " ^ root
                 ^ " expected " ^ expected)
          | None -> false);
       (match Template.vm_semantics_contract_json ~opcode:"LINEAR_Q1_G128_FP" with
        | Some (`Assoc semantics) ->
          check
            "q1 vm semantics pins block bytes"
            (List.mem
               "each block is exactly 18 bytes"
               (string_list_value "q1_block_layout" semantics));
          check
            "q1 vm semantics pins bit order"
            (list_contains_substring
               "item mod 8"
               (string_list_value "q1_block_layout" semantics));
          check
            "q1 vm semantics pins ties-to-even"
            (list_contains_substring
               "round-to-nearest-ties-to-even"
               (string_list_value "arithmetic_policy" semantics));
          check
            "q1 vm semantics pins gradual underflow"
            (list_contains_substring
               "gradual underflow"
               (string_list_value "arithmetic_policy" semantics));
          check
            "q1 vm semantics pins signed zero"
            (list_contains_substring
               "signed zero"
               (string_list_value "arithmetic_policy" semantics));
          check
            "q1 vm semantics pins cancellation zero"
            (list_contains_substring
               "positive zero"
               (string_list_value "arithmetic_policy" semantics));
          check
            "q1 vm semantics keeps opcode separate from ABI"
            (list_contains_substring
               "does not mutate session ABI registers"
               (string_list_value "output_policy" semantics))
        | _ -> failwith "missing q1 vm semantics contract");
       (match
          Template.vm_semantics_binding_json
            ~opcode:"LINEAR_Q1_G128_FP"
            ~vm_semantics_root:
              "db31cbfb8754b8a4b53867f338497e57baee17a64d8fa957108fefe1e93e5da7"
        with
        | `Assoc binding ->
          check
            "q1 vm semantics binding accepted"
            (String.equal (string_value "status" binding) "matched")
        | _ -> failwith "q1 vm semantics binding must be object");
       (match
          Template.vm_semantics_binding_json
            ~opcode:"LINEAR_Q1_G128_FP"
            ~vm_semantics_root:(hex_root 'a')
        with
        | `Assoc binding ->
          check
            "q1 vm semantics binding rejected"
            (String.equal (string_value "status" binding) "unbound")
        | _ -> failwith "q1 stale vm semantics binding must be object");
       (match
          Profile.validate_for_opcode
            ~opcode:"LINEAR_Q1_G128_FP"
            ~profile:"host-fp-local-candidate"
        with
        | Error (Profile.Unsupported_opcode_profile { opcode; profile; expected }) ->
          check
            "q1 old host profile opcode"
            (String.equal opcode "LINEAR_Q1_G128_FP");
          check
            "q1 old host profile rejected"
            (String.equal profile "host-fp-local-candidate");
          check
            "q1 old host profile expected"
            (String.equal expected "deterministic-q1-g128-fp64-linear")
        | Error error -> failwith (Profile.error_message error)
        | Ok _ -> failwith "q1 should reject old host profile")
     | _ -> failwith "template json must be object")

let profile_gate opcode =
  match Profile.current_runtime_profile ~opcode with
  | None -> failwith ("missing runtime profile for " ^ opcode)
  | Some profile_name ->
    match Profile.of_name profile_name with
    | Error error -> failwith (Profile.error_message error)
    | Ok profile ->
      (match Profile.to_json_for_opcode ~opcode profile with
       | `Assoc gate -> gate
       | _ -> failwith "profile gate must be object")

let check_profile_root () =
  match Profile.of_name "deterministic-fp64-rmsnorm" with
  | Error error -> failwith (Profile.error_message error)
  | Ok profile ->
    let root = Profile.root_for_opcode ~opcode:"RMSNORM_FP_EPS" profile in
    let prose_changed_profile =
      {
        profile with
        Profile.consensus_status = Profile.Consensus_ready;
        summary = "changed summary";
        required_actions = ["changed action"];
      }
    in
    let gate = profile_gate "RMSNORM_FP_EPS" in
    check "profile root is hex" (root_ok root);
    check
      "profile root is reported"
      (String.equal root (string_value "profile_root" gate));
    check
      "profile root ignores readiness prose"
      (String.equal
         root
         (Profile.root_for_opcode
            ~opcode:"RMSNORM_FP_EPS"
            prose_changed_profile));
    (match List.assoc_opt "profile_contract" gate with
     | Some (`Assoc contract) ->
       check
         "profile contract schema"
         (String.equal
            (string_value "schema" contract)
            "octra.inference.numerical-contract.v1");
       check
         "profile contract opcode"
         (String.equal (string_value "opcode" contract) "RMSNORM_FP_EPS")
     | _ -> failwith "missing profile contract");
    check
      "profile set root is order-independent"
      (String.equal
         (Profile.profile_set_root_for_opcodes
            ~opcodes:["RMSNORM_FP_EPS"; "L2NORM_FP"]
            profile)
         (Profile.profile_set_root_for_opcodes
            ~opcodes:["L2NORM_FP"; "RMSNORM_FP_EPS"]
            profile))

let check_profile_root_binding_counts () =
  match Profile.of_name "deterministic-fp64-normalization" with
  | Error error -> failwith (Profile.error_message error)
  | Ok profile ->
    let gate = Profile.to_json_for_opcode ~opcode:"RMSNORM_FP_EPS" profile in
    let root = Profile.root_for_opcode ~opcode:"RMSNORM_FP_EPS" profile in
    let matched =
      Profile.root_binding_json ~numerical_profile_root:root gate
    in
    let unbound =
      Profile.root_binding_json ~numerical_profile_root:(hex_root 'e') gate
    in
    let unavailable = Profile.unavailable_root_binding_json in
    let counts =
      Profile.root_binding_counts_of_json
        [matched; unbound; unavailable]
    in
    let classification_counts =
      Profile.root_binding_classification_counts_of_json
        [matched; unbound; unavailable]
    in
    let classification = function
      | `Assoc fields -> string_value "classification" fields
      | _ -> failwith "root binding json must be object"
    in
    check
      "matched root classification"
      (String.equal (classification matched) "none");
    check
      "unbound root classification"
      (String.equal (classification unbound) "profile_root_mismatch");
    check
      "unavailable root classification"
      (String.equal
         (classification unavailable)
         "profile_root_unavailable");
    (match Profile.root_binding_counts_json counts with
     | `Assoc fields ->
       check "matched root count" (int_value "matched" fields = 1);
       check "unbound root count" (int_value "unbound" fields = 1);
       check "unavailable root count" (int_value "unavailable" fields = 1);
       check
         "mixed root bindings are not consensus-ready"
         (not (Profile.root_bindings_are_consensus_ready counts));
       check
         "mixed root binding blockers"
         (list_equal
            (Profile.root_binding_blockers counts)
            ["unbound_profile_roots"; "unavailable_profile_roots"])
     | _ -> failwith "root binding counts json must be object");
    check
      "mixed root bindings pass when not required"
      (Profile.root_bindings_required_pass ~required:false counts);
    check
      "mixed root bindings reject when required"
      (not (Profile.root_bindings_required_pass ~required:true counts));
    let empty_counts = Profile.root_binding_counts_of_json [] in
    check
      "empty root bindings are not consensus-ready"
      (not (Profile.root_bindings_are_consensus_ready empty_counts));
    check
      "empty root bindings reject when required"
      (not (Profile.root_bindings_required_pass ~required:true empty_counts));
    check
      "empty root binding blockers"
      (Profile.root_binding_blockers empty_counts = ["no_profile_roots"]);
    (match Profile.root_binding_gate_json ~required:false counts with
     | `Assoc fields ->
       check
         "optional root binding gate"
         (String.equal (string_value "status" fields) "not_required")
     | _ -> failwith "optional root binding gate must be object");
    (match Profile.root_binding_gate_json ~required:true counts with
     | `Assoc fields ->
       check
         "required root binding gate rejects"
         (String.equal (string_value "status" fields) "rejected");
       check
         "required root binding gate blockers"
         (list_equal
            (string_list_value "blockers" fields)
            ["unbound_profile_roots"; "unavailable_profile_roots"])
     | _ -> failwith "required root binding gate must be object");
    let ready_counts =
      Profile.root_binding_counts_of_json [matched]
    in
    check
      "matched root bindings pass when required"
      (Profile.root_bindings_required_pass ~required:true ready_counts);
    (match Profile.root_binding_gate_json ~required:true ready_counts with
     | `Assoc fields ->
       check
         "matched root binding gate accepts"
         (String.equal (string_value "status" fields) "accepted");
       check
         "matched root binding gate blockers empty"
         (string_list_value "blockers" fields = [])
     | _ -> failwith "matched root binding gate must be object");
    (match Profile.root_binding_gate_json ~required:true empty_counts with
     | `Assoc fields ->
       check
         "empty root binding gate rejects"
         (String.equal (string_value "status" fields) "rejected");
       check
         "empty root binding gate blocker"
         (string_list_value "blockers" fields = ["no_profile_roots"])
     | _ -> failwith "empty root binding gate must be object");
    (match Profile.root_binding_classification_counts_json classification_counts with
     | `Assoc fields ->
       check "none root binding count" (int_value "none" fields = 1);
       check
         "profile root mismatch count"
         (int_value "profile_root_mismatch" fields = 1);
       check
         "profile root unavailable count"
         (int_value "profile_root_unavailable" fields = 1);
       check
         "root binding unknown count"
         (int_value "root_binding_unknown" fields = 0)
     | _ -> failwith "root binding classification counts json must be object")

let check_profile_root_binding_catalog () =
  let gate = `Assoc (profile_gate "RMSNORM_FP_EPS") in
  let expected_root =
    match gate with
    | `Assoc fields -> string_value "profile_root" fields
    | _ -> failwith "profile gate must be object"
  in
  let binding =
    Profile.root_binding_json ~numerical_profile_root:(hex_root 'e') gate
  in
  let report_row =
    `Assoc [
      "path", `String "templates/rmsnorm.json";
      "opcode", `String "RMSNORM_FP_EPS";
      "profile_gate", gate;
      "profile_root_binding", binding;
    ]
  in
  (match Profile.profile_root_binding_catalog_json [report_row] with
   | `List [`Assoc fields] ->
     check
       "binding catalog opcode"
       (String.equal (string_value "opcode" fields) "RMSNORM_FP_EPS");
     check
       "binding catalog profile"
       (String.equal
          (string_value "name" fields)
          "deterministic-fp64-rmsnorm");
     check
       "binding catalog status"
       (String.equal (string_value "status" fields) "unbound");
     check
       "binding catalog classification"
       (String.equal
          (string_value "classification" fields)
          "profile_root_mismatch");
     check
       "binding catalog expected root"
       (String.equal (string_value "profile_root" fields) expected_root);
     check
       "binding catalog producer root"
       (String.equal
          (string_value "numerical_profile_root" fields)
          (hex_root 'e'));
     check
       "binding catalog readiness status"
       (String.equal
          (string_value "validator_readiness_status" fields)
          "rejected");
     check
       "binding catalog readiness blockers"
       (let blockers = string_list_value "validator_readiness_blockers" fields in
        (* Spine-ready profiles no longer emit consensus_candidate_profile_gate. *)
        List.mem "profile_root_mismatch" blockers
        && List.mem "fp64_sqrt_conformance" blockers);
     check
       "binding catalog blocker codes"
       (List.mem
          "fp64_sqrt_conformance"
          (string_list_value "consensus_blocker_codes" fields));
     check
       "binding catalog path"
       (String.equal (string_value "path" fields) "templates/rmsnorm.json")
   | _ -> failwith "profile root binding catalog must contain one entry");
  let q1_gate = `Assoc (profile_gate "LINEAR_Q1_G128_FP") in
  let q1_root =
    match q1_gate with
    | `Assoc fields -> string_value "profile_root" fields
    | _ -> failwith "q1 profile gate must be object"
  in
  let paired =
    `Assoc [
      "case", `String "logits-tail";
      "manifest", `String "fixtures/logits-tail.json";
      "profile_gates", `List [gate; q1_gate];
      "profile_root_bindings",
      `List [
        binding;
        Profile.root_binding_json ~numerical_profile_root:q1_root q1_gate;
      ];
    ]
  in
  (match Profile.profile_root_binding_catalog_json [paired] with
   | `List entries ->
     check "paired binding catalog count" (List.length entries = 2);
     check
       "paired binding catalog includes matched q1"
       (List.exists
          (function
            | `Assoc fields ->
              String.equal (string_value "opcode" fields) "LINEAR_Q1_G128_FP"
              && String.equal (string_value "status" fields) "matched"
              && String.equal (string_value "case" fields) "logits-tail"
              && String.equal
                   (string_value "manifest" fields)
                   "fixtures/logits-tail.json"
              &&
              (* Spine-ready Q1 may still be rejected for residual math blockers. *)
              (String.equal
                 (string_value "validator_readiness_status" fields)
                 "rejected"
               || String.equal
                    (string_value "validator_readiness_status" fields)
                    "accepted")
            | _ -> false)
          entries)
   | _ -> failwith "paired profile root binding catalog must be list")

let check_profile_status_counts () =
  let runtime_gate opcode = `Assoc (profile_gate opcode) in
  let candidate_gate =
    match Profile.of_name "byte-ingress-exact" with
    | Error error -> failwith (Profile.error_message error)
    | Ok profile -> Profile.to_json_for_opcode ~opcode:"LOAD_F64_LE_FP" profile
  in
  let counts =
    Profile.status_counts_of_json_gates
      [
        runtime_gate "LINEAR_Q1_G128_FP";
        runtime_gate "RMSNORM_FP_EPS";
        candidate_gate;
        `Assoc ["consensus_status", `String "consensus_ready"];
        `Assoc ["consensus_status", `String "future_profile"];
        `Assoc ["name", `String "profileless"];
      ]
  in
  (match Profile.status_counts_json counts with
   | `Assoc fields ->
     check "classified count" (Profile.classified_gate_count counts = 6);
     check "local-only count" (int_value "local_only" fields = 0);
     (* LINEAR + RMSNORM ready; byte-ingress-exact candidate; one explicit ready. *)
     check "candidate count" (int_value "consensus_candidate" fields = 1);
     check "ready count" (int_value "consensus_ready" fields = 3);
     check "unknown count" (int_value "unknown" fields = 2);
     check
       "mixed counts are not consensus-ready"
       (not (Profile.status_counts_are_consensus_ready counts));
     check
       "mixed counts are not consensus-candidate"
       (not (Profile.status_counts_are_consensus_candidate counts));
     check
       "mixed consensus-candidate blockers"
       (list_equal
          (Profile.consensus_candidate_blockers
             ~profile_gate_count:6
             ~unprofiled_count:2
             counts)
          [
            "unprofiled_profile_gates";
            "unknown_profile_gates";
          ]);
     check
       "mixed consensus-ready blockers"
       (list_equal
          (Profile.consensus_ready_blockers
             ~profile_gate_count:6
             ~unprofiled_count:2
             counts)
          [
            "unprofiled_profile_gates";
            "consensus_candidate_profile_gates";
            "unknown_profile_gates";
          ]);
     let ready_counts =
       Profile.status_counts_of_json_gates
         [`Assoc ["consensus_status", `String "consensus_ready"]]
     in
     check
       "ready-only counts are consensus-ready"
       (Profile.status_counts_are_consensus_ready ready_counts);
     check
       "ready-only counts are consensus-candidate"
       (Profile.status_counts_are_consensus_candidate ready_counts);
     check
       "ready-only gate is consensus-candidate"
       (Profile.consensus_candidate
          ~profile_gate_count:1
          ~unprofiled_count:0
          ready_counts);
     check
       "ready-only candidate blockers empty"
       (Profile.consensus_candidate_blockers
          ~profile_gate_count:1
          ~unprofiled_count:0
          ready_counts
        = []);
     check
       "ready-only gate is consensus-ready"
       (Profile.consensus_ready
          ~profile_gate_count:1
          ~unprofiled_count:0
          ready_counts);
     check
       "ready-only blockers empty"
       (Profile.consensus_ready_blockers
          ~profile_gate_count:1
          ~unprofiled_count:0
          ready_counts
        = []);
     check
       "unclassified gate blocker"
       (Profile.consensus_ready_blockers
          ~profile_gate_count:2
          ~unprofiled_count:0
          ready_counts
        = ["unclassified_profile_gates"]);
     check
       "unclassified candidate gate blocker"
       (Profile.consensus_candidate_blockers
          ~profile_gate_count:2
          ~unprofiled_count:0
          ready_counts
        = ["unclassified_profile_gates"]);
     check
       "unclassified gate is not consensus-candidate"
       (not
          (Profile.consensus_candidate
             ~profile_gate_count:2
             ~unprofiled_count:0
             ready_counts));
     check
       "unclassified gate is not consensus-ready"
       (not
          (Profile.consensus_ready
             ~profile_gate_count:2
             ~unprofiled_count:0
             ready_counts));
     let local_counts =
       Profile.status_counts_of_json_gates
         [`Assoc ["consensus_status", `String "local_only"]]
     in
     check
       "local-only counts are not consensus-candidate"
       (not (Profile.status_counts_are_consensus_candidate local_counts));
     check
       "local-only candidate blocker"
       (Profile.consensus_candidate_blockers
          ~profile_gate_count:1
          ~unprofiled_count:0
          local_counts
        = ["local_only_profile_gates"]);
     check
       "empty counts are not consensus-ready"
       (not
          (Profile.status_counts_are_consensus_ready
             (Profile.status_counts_of_json_gates [])))
   | _ -> failwith "status counts json must be object")

let check_validator_readiness_predicate () =
  check
    "validator readiness accepts full evidence"
    (Profile.validator_readiness_accepted
       ~execution_ready:true
       ~failure_cases_ready:true
       ~effort_ready:true
       ~profile_ready:true
       ~roots_ready:true
       ~cross_platform_ready:true);
  check
    "validator readiness rejects missing cross-platform"
    (not
       (Profile.validator_readiness_accepted
          ~execution_ready:true
          ~failure_cases_ready:true
          ~effort_ready:true
          ~profile_ready:true
          ~roots_ready:true
          ~cross_platform_ready:false));
  check
    "validator readiness rejects missing effort"
    (not
       (Profile.validator_readiness_accepted
          ~execution_ready:true
          ~failure_cases_ready:true
          ~effort_ready:false
          ~profile_ready:true
          ~roots_ready:true
          ~cross_platform_ready:true));
  check
    "validator readiness rejects local-only execution evidence"
    (not
       (Profile.validator_readiness_accepted
          ~execution_ready:true
          ~failure_cases_ready:true
          ~effort_ready:true
          ~profile_ready:false
          ~roots_ready:true
          ~cross_platform_ready:true))

let check_abi_declaration_binding () =
  let matched =
    Template.abi_declaration_binding_json (abi_template ())
  in
  (match matched with
   | `Assoc fields ->
     check
       "abi declaration matched"
       (String.equal (string_value "status" fields) "matched");
     check
       "abi declaration scope"
       (String.equal
          (string_value "evidence_scope" fields)
          "template_declaration");
     check
       "abi root bound"
       (String.equal (string_value "session_abi_root" fields) Abi.v1_root);
     check
       "abi root authority"
       (String.equal
          (string_value "litenode_session_abi_root" fields)
          Abi.v1_root);
     check
       "abi count unit is generic"
       (String.equal (string_value "output_count_unit" fields) "cells");
     check "abi output base register" (int_value "r0" fields = 10000);
     check "abi output count register" (int_value "r1" fields = 6)
   | _ -> failwith "abi declaration binding must be object");
  let matched_v2 =
    Template.abi_declaration_binding_json
      (abi_template ~session_abi_root:Abi.v2_root ())
  in
  (match matched_v2 with
   | `Assoc fields ->
     check
       "v2 abi declaration matched"
       (String.equal (string_value "status" fields) "matched");
     check
       "v2 abi root bound"
       (String.equal (string_value "session_abi_root" fields) Abi.v2_root);
     check
       "v2 abi matched root"
       (String.equal
          (string_value "litenode_matched_session_abi_root" fields)
          Abi.v2_root);
     check
       "v2 abi supported roots"
       (List.mem
          Abi.v2_root
          (string_list_value "litenode_supported_session_abi_roots" fields))
   | _ -> failwith "v2 abi declaration binding must be object");
  let stale_root =
    Template.abi_declaration_binding_json
      (abi_template ~session_abi_root:(hex_root '1') ())
  in
  (match stale_root with
   | `Assoc fields ->
     check
       "stale ABI root rejects"
       (String.equal (string_value "status" fields) "unbound");
     check
       "stale ABI root blocker"
       (List.mem
          "session_abi_root_mismatch"
          (string_list_value "blockers" fields))
   | _ -> failwith "stale ABI root binding must be object");
  let narrow_unit =
    Template.abi_declaration_binding_json
      (abi_template ~output_count_unit:"f64_cells" ())
  in
  (match narrow_unit with
   | `Assoc fields ->
     check
       "f64_cells session ABI rejects"
       (String.equal (string_value "status" fields) "unbound");
     check
       "f64_cells session ABI blocker"
       (List.mem
          "output_count_unit_mismatch"
          (string_list_value "blockers" fields))
   | _ -> failwith "narrow ABI unit binding must be object");
  let stale_register =
    Template.abi_declaration_binding_json (abi_template ~r1:7 ())
  in
  match stale_register with
  | `Assoc fields ->
    check
      "stale r1 rejects"
      (String.equal (string_value "status" fields) "unbound");
    check
      "stale r1 blocker"
      (List.mem "r1_output_count_mismatch" (string_list_value "blockers" fields))
  | _ -> failwith "stale register ABI binding must be object"

let check_p0_profile_gate_coverage () =
  List.iter
    (fun opcode ->
      check
        (opcode ^ " runtime profile")
        (match Profile.current_runtime_profile ~opcode with
         | Some _ -> true
         | _ -> false);
      let gate = profile_gate opcode in
      let local_semantics = string_list_value "local_semantics" gate in
      let consensus_obligations =
        string_list_value "consensus_obligations" gate
      in
      check (opcode ^ " local semantics present") (local_semantics <> []);
      check
        (opcode ^ " consensus obligations present")
        (consensus_obligations <> []);
      check
        (opcode ^ " consensus blocker codes present")
        (string_list_value "consensus_blocker_codes" gate <> []);
      check
        (opcode ^ " consensus obligations are specific")
        (not
           (list_contains_substring
              "write primitive-specific"
              consensus_obligations)))
    Template.p0_opcodes

let check_inference_profile_surface_coverage () =
  let surface =
    [
      "LOAD_F32_LE_FP", "byte-ingress-f32-bits", "consensus_ready",
      "3e90bd2ce93a98d639e7d78274beebb596b1fbc5ee112c91e8581da87434f1c4";
      "LOAD_F64_LE_FP", "byte-ingress-f64-bits", "consensus_ready",
      "4109c53ff1d1e56c15a023f701da288c8c78a0962d7a74491a5924f6342d41f0";
      "LINEAR_Q1_G128_FP", "deterministic-q1-g128-fp64-linear",
      "consensus_ready",
      "1247c6e4e8364a774c2585a76a05fadfdf631e9e7562d4949ee5d1b358589367";
      "SIGMOID_FP", "deterministic-fp64-sigmoid", "consensus_ready",
      "f49ff17137a92773d4173130b982f52d44c84c252dc973d11539697ceacba774";
      "SOFTPLUS_FP", "deterministic-fp64-softplus", "consensus_ready",
      "b53f54ea31047681a17559cddd8cd1f7a2a623f7562cf9fba16cfeafb8edfeb5";
      "SILU_FP", "deterministic-fp64-silu", "consensus_ready",
      "1e9da402fbea14ecd54a5ec775a76200bf1ff323315b94384a8e827148b82b98";
      "CAUSAL_DEPTHWISE_CONV1D_FP", "deterministic-fp64-accumulation",
      "consensus_candidate",
      "df645d83fe5fa0e32a7d5349b9b230de9002c0a980e32eb3d6582852fa05d545";
      "GATED_DELTA_RULE_FP", "deterministic-fp64-gated-delta",
      "consensus_ready",
      "f79dba18fda942ef0bc862c017b643688d45d15be1b46b0acf7494936cfe2bf0";
      "RMSNORM_FP_EPS", "deterministic-fp64-rmsnorm",
      "consensus_ready",
      "b8f1b4710cb0799b8c59f72314e40be23548f70cfb0a3ba4fb531a24803798f1";
      "L2NORM_FP", "deterministic-fp64-l2norm",
      "consensus_ready",
      "1b8026e38d749fb7aebb33a75ecf236f295934dcebb3cc937734e4ee2c2f8c26";
      "ELEMWISE_MUL_FP", "deterministic-fp64-elementwise",
      "consensus_candidate",
      "e2d242453af8bc36fb42d0083c97d3cffff38d9ad1c4c364385e52b84cb898d3";
      "RESIDUAL_ADD_FP", "deterministic-fp64-elementwise",
      "consensus_candidate",
      "a1d1bc7242bcca11b31395d8313b552eec80e5174651637df0690adec41cd32f";
      "ROPE_APPLY_INDEXED_FP", "host-fp-trig-local-candidate", "local_only",
      "a36d7dad881a763dab58fdd62b12a3ba7adddcc021c979d86372583559f0a0f4";
      "ATTENTION_SCORES_FP", "deterministic-fp64-accumulation",
      "consensus_candidate",
      "20bfb100d037cb05d3208ed6c35bb36deae747ff2aa9fc1ac78f1078f19b56e5";
      "SOFTMAX_FP", "deterministic-fp64-softmax", "consensus_ready",
      "407122b6630ad06842386561a931d05325e2c206d6fe4cc4429360fd755de549";
      "ATTENTION_WEIGHTED_SUM_FP", "deterministic-fp64-accumulation",
      "consensus_candidate",
      "31ccd36c648f33a0d1adabed83db830100090a49ad956a625cc53be21d2bc3fd";
      "ARGMAX_FP", "deterministic-fp64-comparison", "consensus_ready",
      "0b48c255fab38cd3a3ffe3fc57ab626532bafa3b54ffd690d4582625c58f1b42";
    ]
  in
  check "inference profile surface count" (List.length surface = 17);
  let surface_opcodes =
    List.map (fun (opcode, _, _, _) -> opcode) surface
  in
  check
    "runtime opcode list matches surface"
    (list_equal
       Profile.current_runtime_opcodes
       surface_opcodes);
  (match
     Profile.current_runtime_profile_catalog_json
       ~opcodes:Template.p0_opcodes
   with
   | `Assoc fields ->
     check
       "p0 profile catalog schema"
       (String.equal
          (string_value "schema" fields)
          "octra.inference.profile-catalog.v1");
     check "p0 profile catalog count" (int_value "opcode_count" fields = 13);
     check
       "p0 profile catalog root"
	       (String.equal
	          (string_value "profile_catalog_root" fields)
	          "19376a3823e400021d3a343e8ba5034ba8c7c452b44cd10a7c94b99bd209ffc3");
     (match assoc_value "profile_readiness_worklist" fields with
      | `List rows ->
        check "p0 readiness worklist count" (List.length rows = 13);
        let row opcode =
          List.find_opt
            (function
              | `Assoc row_fields ->
                String.equal (string_value "opcode" row_fields) opcode
              | _ -> false)
            rows
        in
        (match row "SOFTMAX_FP" with
         | Some (`Assoc row_fields) ->
           check
             "softmax readiness scope"
             (String.equal
                (string_value "validator_readiness_scope" row_fields)
                "profile_catalog_static");
           check
             "softmax readiness rejected"
             (String.equal
                (string_value "validator_readiness_status" row_fields)
                "rejected");
           check
             "softmax readiness includes protocol exp conformance"
             (List.mem
                "protocol_owned_exp_conformance"
                (string_list_value "validator_readiness_blockers" row_fields));
           check
             "softmax readiness includes missing execution"
             (List.mem
                "execution_not_proven"
                (string_list_value "validator_readiness_blockers" row_fields));
           (match assoc_value "consensus_blocker_classes" row_fields with
            | `List blocker_classes ->
              check
                "softmax blocker classes present"
                (List.exists
                   (function
                     | `Assoc class_fields ->
                       String.equal
                         (string_value "blocker_code" class_fields)
                         "protocol_owned_exp_conformance"
                       && String.equal
                            (string_value "blocker_class" class_fields)
                            "software_fp64_conformance"
                     | _ -> false)
                   blocker_classes)
            | _ -> failwith "softmax blocker classes must be a list")
         | _ -> failwith "missing SOFTMAX_FP readiness row")
      | _ -> failwith "p0 profile readiness worklist must be a list")
   | _ -> failwith "p0 profile catalog must be object");
  (match Profile.current_runtime_profile_catalog_json ~opcodes:surface_opcodes with
   | `Assoc fields ->
      check "runtime profile catalog count" (int_value "opcode_count" fields = 17);
      check
        "runtime profile catalog root"
	       (String.equal
	          (string_value "profile_catalog_root" fields)
	          "d7155fbae27675b630fa0e9ee7998d0daf6f078ae97b917317240bffaffb3fd0");
     (match assoc_value "profile_readiness_worklist" fields with
      | `List rows ->
        check "runtime readiness worklist count" (List.length rows = 17)
      | _ -> failwith "runtime profile readiness worklist must be a list")
   | _ -> failwith "runtime profile catalog must be object");
  let gates =
    List.map
      (fun (opcode, expected_profile, expected_status, expected_root) ->
         (match Profile.current_runtime_profile ~opcode with
          | Some actual ->
            check
              (opcode ^ " explicit runtime profile")
              (String.equal actual expected_profile)
          | None -> failwith (opcode ^ " missing runtime profile"));
         check
           (opcode ^ " does not use generic host profile")
           (not (String.equal expected_profile "host-fp-local-candidate"));
         let gate = profile_gate opcode in
         check
           (opcode ^ " gate profile")
           (String.equal (string_value "name" gate) expected_profile);
         check
           (opcode ^ " gate status")
           (String.equal (string_value "consensus_status" gate) expected_status);
         check
           (opcode ^ " gate profile root")
           (String.equal (string_value "profile_root" gate) expected_root);
         check
           (opcode ^ " local semantics present")
           (string_list_value "local_semantics" gate <> []);
         check
           (opcode ^ " obligations present")
           (string_list_value "consensus_obligations" gate <> []);
         check
           (opcode ^ " blockers present")
           (string_list_value "consensus_blocker_codes" gate <> []);
         `Assoc gate)
      surface
  in
  let counts = Profile.status_counts_of_json_gates gates in
  (match Profile.status_counts_json counts with
   | `Assoc fields ->
     check "surface local-only count" (int_value "local_only" fields = 1);
     check
       "surface consensus-candidate count"
       (int_value "consensus_candidate" fields = 5);
     check "surface consensus-ready count" (int_value "consensus_ready" fields = 11);
     check "surface unknown count" (int_value "unknown" fields = 0)
   | _ -> failwith "surface status counts json must be object");
  (match Profile.profile_root_catalog_json gates with
   | `List catalog ->
     check "surface profile root catalog count" (List.length catalog = 17);
     List.iter
       (fun (opcode, expected_profile, expected_status, expected_root) ->
          check
            (opcode ^ " catalog entry")
            (List.exists
               (function
                 | `Assoc fields ->
                   String.equal (string_value "opcode" fields) opcode
                   && String.equal (string_value "name" fields) expected_profile
                   && String.equal
                        (string_value "consensus_status" fields)
                        expected_status
                   && String.equal
                        (string_value "profile_root" fields)
                        expected_root
                 | _ -> false)
               catalog))
       surface
   | _ -> failwith "surface profile root catalog must be list");
  check
    "surface profile catalog root"
    (match Profile.profile_catalog_root_json gates with
     | `String root ->
	       String.equal
	         root
		         "d7155fbae27675b630fa0e9ee7998d0daf6f078ae97b917317240bffaffb3fd0"
     | _ -> false);
  check
    "empty profile catalog root"
    (match Profile.profile_catalog_root_json [] with
     | `Null -> true
     | _ -> false);
  (match Profile.consensus_blocker_catalog_json gates with
   | `List catalog ->
     List.iter
       (function
         | `Assoc fields ->
           let code = string_value "blocker_code" fields in
           let blocker_class = string_value "blocker_class" fields in
           check
             (code ^ " blocker class is known")
             (not (String.equal blocker_class "unknown"))
         | _ -> failwith "blocker catalog entry must be object")
       catalog;
     let blocker_entry code =
       List.find_opt
         (function
           | `Assoc fields ->
             String.equal (string_value "blocker_code" fields) code
           | _ -> false)
         catalog
     in
     let check_blocker code expected =
       match blocker_entry code with
       | Some (`Assoc fields) ->
         let opcodes = string_list_field "opcodes" fields in
         List.iter
           (fun opcode ->
              check
                (code ^ " catalog contains " ^ opcode)
                (List.mem opcode opcodes))
           expected
       | _ -> failwith ("missing blocker catalog entry: " ^ code)
     in
     let check_blocker_class code expected =
       match blocker_entry code with
       | Some (`Assoc fields) ->
         check
           (code ^ " blocker class")
           (String.equal (string_value "blocker_class" fields) expected)
       | _ -> failwith ("missing blocker catalog entry: " ^ code)
     in
     check_blocker
       "binary16_scale_decode"
       ["LINEAR_Q1_G128_FP"];
     check_blocker_class
       "binary16_scale_decode"
       "encoding_or_layout";
	     check_blocker
      "protocol_owned_exp_conformance"
      ["SIGMOID_FP"; "SOFTPLUS_FP"; "SILU_FP"];
     check_blocker_class
       "protocol_owned_exp_conformance"
       "software_fp64_conformance";
     check_blocker
       "protocol_owned_log1p_conformance"
       ["SOFTPLUS_FP"];
     check_blocker_class
       "protocol_owned_log1p_conformance"
       "software_fp64_conformance";
     check_blocker
       "fp64_sqrt_conformance"
       ["RMSNORM_FP_EPS"; "L2NORM_FP"; "GATED_DELTA_RULE_FP";
        "ATTENTION_SCORES_FP"];
     check_blocker_class
       "fp64_sqrt_conformance"
       "software_fp64_conformance";
     check_blocker_class
       "atomic_writeback"
       "safety_policy"
   | _ -> failwith "surface consensus blocker catalog must be list")
  ;
  (match Profile.consensus_blocker_class_counts_json gates with
   | `List counts ->
     let class_entry name =
       List.find_opt
         (function
           | `Assoc fields ->
             String.equal (string_value "blocker_class" fields) name
           | _ -> false)
         counts
     in
     let check_class name blocker opcode =
       match class_entry name with
       | Some (`Assoc fields) ->
         check
           (name ^ " blocker count is positive")
           (int_value "blocker_count" fields > 0);
         check
           (name ^ " opcode count is positive")
           (int_value "opcode_count" fields > 0);
         check
           (name ^ " includes blocker")
           (List.mem blocker (string_list_field "blocker_codes" fields));
         check
           (name ^ " includes opcode")
           (List.mem opcode (string_list_field "opcodes" fields))
       | _ -> failwith ("missing blocker class entry: " ^ name)
     in
     check_class
       "software_fp64_conformance"
       "fp64_sqrt_conformance"
       "RMSNORM_FP_EPS";
	     check_class
	       "software_fp64_conformance"
	       "protocol_owned_exp_conformance"
	       "SILU_FP";
     check_class
       "safety_policy"
       "atomic_writeback"
       "LINEAR_Q1_G128_FP";
     check
       "class count entries are objects"
       (List.for_all (function `Assoc _ -> true | _ -> false) counts)
   | _ -> failwith "surface consensus blocker class counts must be list")

let check_remaining_p0_profile_obligations () =
  List.iter
    (fun (opcode, local_needle, obligation_needle) ->
      let gate = profile_gate opcode in
      check
        (opcode ^ " local semantics")
        (list_contains_substring
           local_needle
           (string_list_value "local_semantics" gate));
      check
        (opcode ^ " consensus obligations")
        (list_contains_substring
           obligation_needle
           (string_list_value "consensus_obligations" gate)))
    [
      "L2NORM_FP", "inverse norm", "inverse-norm edge vectors";
      "SOFTMAX_FP", "maximum score", "probability and ordering";
      "GATED_DELTA_RULE_FP", "next-state cells", "state-transition";
    ];
  List.iter
    (fun (opcode, expected_status, expected_profile) ->
      let gate = profile_gate opcode in
      check
        (opcode ^ " consensus status")
        (String.equal
           (string_value "consensus_status" gate)
           expected_status);
      check
        (opcode ^ " records no native fp math")
        (list_contains_substring
           "no native host floating-point math"
           (string_list_value "local_semantics" gate));
      check
        (opcode ^ " profile root obligation")
        (list_contains_substring
           "profile root"
           (string_list_value "consensus_obligations" gate));
      (match List.assoc_opt "profile_contract" gate with
       | Some (`Assoc contract) ->
         check
           (opcode ^ " profile name")
           (String.equal
              (string_value "profile_name" contract)
              expected_profile);
         check
           (opcode ^ " rounding mode")
           (String.equal
              (string_value "rounding_mode" contract)
              "deterministic-binary64-roundTiesToEven")
       | _ -> failwith ("missing " ^ opcode ^ " profile contract"));
      (match
         Profile.validate_for_opcode
           ~opcode
           ~profile:"host-fp-local-candidate"
       with
       | Error (Profile.Unsupported_opcode_profile { opcode = actual; profile; expected }) ->
         check (opcode ^ " old host profile opcode") (String.equal actual opcode);
         check
           (opcode ^ " old host profile rejected")
           (String.equal profile "host-fp-local-candidate");
         check
           (opcode ^ " old host profile expected")
           (String.equal expected expected_profile)
       | Error error -> failwith (Profile.error_message error)
       | Ok _ -> failwith (opcode ^ " should reject old host profile"));
      match Profile.validate_for_opcode ~opcode ~profile:"q16-exact" with
      | Error (Profile.Unsupported_opcode_profile { opcode = actual; profile; expected }) ->
        check (opcode ^ " overclaim opcode") (String.equal actual opcode);
        check (opcode ^ " overclaim profile") (String.equal profile "q16-exact");
        check
          (opcode ^ " overclaim expected")
          (String.equal expected expected_profile)
      | Error error -> failwith (Profile.error_message error)
      | Ok _ -> failwith (opcode ^ " should reject profile overclaim"))
    [
      "RMSNORM_FP_EPS", "consensus_ready", "deterministic-fp64-rmsnorm";
      "L2NORM_FP", "consensus_ready", "deterministic-fp64-l2norm";
    ];
  let l2_gate = profile_gate "L2NORM_FP" in
  (match List.assoc_opt "profile_contract" l2_gate with
   | Some (`Assoc contract) ->
     check
       "l2 mixed rounding mode"
       (String.equal
          (string_value "rounding_mode" contract)
          "deterministic-binary64-roundTiesToEven")
   | _ -> failwith "missing l2 profile contract");
  let l2_blockers = string_list_value "consensus_blocker_codes" l2_gate in
  check
    "l2 reduction blocker"
    (List.mem "fp64_reduction_conformance" l2_blockers);
  check
    "l2 inverse-root blocker"
    (List.mem "fp64_sqrt_conformance" l2_blockers);
  check
    "l2 divide conformance blocker"
    (List.mem "fp64_divide_conformance" l2_blockers);
  let softmax_gate = profile_gate "SOFTMAX_FP" in
  check
    "softmax deterministic profile"
    (String.equal
       (string_value "name" softmax_gate)
       "deterministic-fp64-softmax");
  check
    "softmax is consensus ready"
    (String.equal (string_value "consensus_status" softmax_gate) "consensus_ready");
  check
    "softmax binds protocol-owned exp qualification"
    (list_contains_substring
       "protocol-owned nonpositive exp"
       (string_list_value "required_actions" softmax_gate));
  let softmax_blockers =
    string_list_value "consensus_blocker_codes" softmax_gate
  in
  check
    "softmax subtract blocker"
    (List.mem "fp64_subtract_conformance" softmax_blockers);
  check
    "softmax comparison blocker"
    (List.mem "fp64_comparison_conformance" softmax_blockers);
  check
    "softmax host comparison retired"
    (not (List.mem "host_fp_comparison" softmax_blockers));
  check
    "softmax reduction blocker"
    (List.mem "fp64_reduction_conformance" softmax_blockers);
  check
    "softmax protocol exp blocker"
    (List.mem "protocol_owned_exp_conformance" softmax_blockers);
  check
    "softmax host exp retired"
    (not (List.mem "host_fp_exp" softmax_blockers));
  check
    "softmax divide conformance blocker"
    (List.mem "fp64_divide_conformance" softmax_blockers);
  check
    "softmax effort reauthorization blocker"
    (List.mem "software_exp_effort_reauthorization" softmax_blockers);
  check
    "softmax nonpositive exp gate"
    (list_contains_substring
       "less than or equal to +0.0"
       (string_list_value "local_semantics" softmax_gate));
  (match List.assoc_opt "profile_contract" softmax_gate with
   | Some (`Assoc contract) ->
     check
       "softmax contract profile"
       (String.equal
         (string_value "profile_name" contract)
          "deterministic-fp64-softmax");
     check
       "softmax profile records exp gate"
       (list_contains_substring
         "check_shifted_score_nonpositive"
         (string_list_value "operation_sequence" contract));
     check
       "softmax profile records protocol exp"
       (List.mem
          "exp_each_score_protocol_q256"
          (string_list_value "operation_sequence" contract));
     check
       "softmax edge policy records positive shift rejection"
       (List.mem
          "reject_positive_shifted_exp_input"
          (string_list_value "edge_value_policy" contract))
   | _ -> failwith "missing softmax profile contract");
  let delta_gate = profile_gate "GATED_DELTA_RULE_FP" in
  check
    "delta deterministic profile"
    (String.equal
       (string_value "name" delta_gate)
       "deterministic-fp64-gated-delta");
  check
    "delta is consensus ready"
    (String.equal (string_value "consensus_status" delta_gate) "consensus_ready");
  check_protocol_owned_transcendental_obligation "GATED_DELTA_RULE_FP" delta_gate;
  let delta_blockers =
    string_list_value "consensus_blocker_codes" delta_gate
  in
  check
    "delta recurrence blocker"
    (List.mem "fp64_recurrence_add_mul_conformance" delta_blockers);
  check
    "delta protocol exp blocker"
    (List.mem "protocol_owned_exp_conformance" delta_blockers);
  check
    "delta has no host exp blocker"
    (not (List.mem "host_fp_exp" delta_blockers));
  check
    "delta software exp effort blocker"
    (List.mem "software_exp_effort_reauthorization" delta_blockers);
  check
    "delta sqrt blocker"
    (List.mem "fp64_sqrt_conformance" delta_blockers);
  check
    "delta divide conformance blocker"
    (List.mem "fp64_divide_conformance" delta_blockers);
  check
    "delta nonpositive exp gate"
    (list_contains_substring
       "protocol-owned"
       (string_list_value "local_semantics" delta_gate));
  (match List.assoc_opt "profile_contract" delta_gate with
   | Some (`Assoc contract) ->
     check
       "delta contract profile"
       (String.equal
          (string_value "profile_name" contract)
          "deterministic-fp64-gated-delta");
     check
       "delta profile records exp gate"
       (list_contains_substring
	          "compute_decay_protocol_q256"
	          (string_list_value "operation_sequence" contract));
     check
       "delta edge policy records positive decay rejection"
       (List.mem
          "reject_positive_log_decay_before_state_mutation"
          (string_list_value "edge_value_policy" contract))
   | _ -> failwith "missing delta profile contract");
	  List.iter
	    (fun (opcode, expected_profile) ->
	      (match
	         Profile.validate_for_opcode
	           ~opcode
	           ~profile:"host-fp-local-candidate"
	       with
	       | Error (Profile.Unsupported_opcode_profile { opcode = actual; profile; expected }) ->
	         check (opcode ^ " old host profile opcode") (String.equal actual opcode);
	         check
	           (opcode ^ " old host profile rejected")
	           (String.equal profile "host-fp-local-candidate");
	         check
	           (opcode ^ " old host profile expected")
	           (String.equal expected expected_profile)
	       | Error error -> failwith (Profile.error_message error)
	       | Ok _ -> failwith (opcode ^ " should reject old host profile"));
	      match Profile.validate_for_opcode ~opcode ~profile:"q16-exact" with
	      | Error (Profile.Unsupported_opcode_profile { opcode = actual; profile; expected }) ->
	        check (opcode ^ " q16 overclaim opcode") (String.equal actual opcode);
	        check (opcode ^ " q16 overclaim profile") (String.equal profile "q16-exact");
	        check
	          (opcode ^ " q16 overclaim expected")
	          (String.equal expected expected_profile)
	      | Error error -> failwith (Profile.error_message error)
	      | Ok _ -> failwith (opcode ^ " should reject q16 overclaim"))
	    [
	      "SOFTMAX_FP", "deterministic-fp64-softmax";
	      "GATED_DELTA_RULE_FP", "deterministic-fp64-gated-delta";
	    ];
  let oracle_root gate =
    match List.assoc_opt "profile_contract" gate with
    | Some (`Assoc contract) -> string_value "oracle_vector_root" contract
    | _ -> failwith "missing profile contract"
  in
  check
    "l2 oracle vectors are bound"
    (not
       (String.equal
          (oracle_root l2_gate)
          (oracle_root (profile_gate "ARGMAX_FP"))))

let check_argmax_profile_gate () =
  check
    "argmax runtime profile"
    (match Profile.current_runtime_profile ~opcode:"ARGMAX_FP" with
     | Some "deterministic-fp64-comparison" -> true
     | _ -> false);
  let gate = profile_gate "ARGMAX_FP" in
  check
    "argmax consensus ready"
    (String.equal
       (string_value "consensus_status" gate)
       "consensus_ready");
  check
    "argmax local tie semantics"
    (list_contains_substring
       "lowest zero-based index"
       (string_list_value "local_semantics" gate));
  check
    "argmax deterministic comparison semantics"
    (list_contains_substring
       "deterministic finite binary64"
       (string_list_value "local_semantics" gate));
  check
    "argmax records no host math"
    (list_contains_substring
       "no native host math"
       (string_list_value "local_semantics" gate));
  let blockers = string_list_value "consensus_blocker_codes" gate in
  check
    "argmax comparison blocker"
    (List.mem "fp64_comparison_conformance" blockers);
  check
    "argmax no host exp blocker"
    (not (List.mem "host_fp_exp" blockers));
  check
    "argmax no host arithmetic blocker"
    (not (List.mem "host_fp_arithmetic" blockers));
  check
    "argmax ordering obligation"
    (list_contains_substring
       "ordering preservation"
       (string_list_value "consensus_obligations" gate));
  (match List.assoc_opt "profile_contract" gate with
   | Some (`Assoc contract) ->
     check
       "argmax comparison rounding mode"
       (String.equal
          (string_value "rounding_mode" contract)
          "not-applicable-deterministic-comparison");
     check
       "argmax profile name"
       (String.equal
          (string_value "profile_name" contract)
          "deterministic-fp64-comparison")
   | _ -> failwith "missing argmax profile contract");
  (match
     Profile.validate_for_opcode
       ~opcode:"ARGMAX_FP"
       ~profile:"deterministic-fp64-comparison"
   with
   | Ok profile ->
     check
       "argmax deterministic profile accepted"
       (String.equal profile.Profile.name "deterministic-fp64-comparison")
   | Error error -> failwith (Profile.error_message error));
  match Profile.validate_for_opcode ~opcode:"ARGMAX_FP" ~profile:"q16-exact" with
  | Error (Profile.Unsupported_opcode_profile { opcode; profile; expected }) ->
    check "argmax overclaim opcode" (String.equal opcode "ARGMAX_FP");
    check "argmax overclaim profile" (String.equal profile "q16-exact");
    check
      "argmax overclaim expected"
      (String.equal expected "deterministic-fp64-comparison")
  | Error error -> failwith (Profile.error_message error)
  | Ok _ -> failwith "expected argmax profile overclaim rejection"

let check_rope_indexed_profile_gate () =
  check
    "rope indexed runtime profile"
    (match Profile.current_runtime_profile ~opcode:"ROPE_APPLY_INDEXED_FP" with
     | Some "host-fp-trig-local-candidate" -> true
     | _ -> false);
  let gate = profile_gate "ROPE_APPLY_INDEXED_FP" in
  check
    "rope indexed trig-local profile"
    (String.equal
       (string_value "name" gate)
       "host-fp-trig-local-candidate");
  check
    "rope indexed remains local-only"
    (String.equal (string_value "consensus_status" gate) "local_only");
  check_protocol_owned_transcendental_obligation "ROPE_APPLY_INDEXED_FP" gate;
  check
    "rope indexed requires protocol-owned rotary replacement"
    (list_contains_substring
       "protocol-owned deterministic rotary math"
       (string_list_value "required_actions" gate));
  check
    "rope indexed local trig semantics"
    (list_contains_substring
       "native cos and sin"
       (string_list_value "local_semantics" gate));
  check
    "rope indexed position obligation"
    (list_contains_substring
       "position-cell interpretation"
       (string_list_value "consensus_obligations" gate));
  let blockers = string_list_value "consensus_blocker_codes" gate in
  check
    "rope indexed exponentiation blocker"
    (List.mem "host_fp_exponentiation" blockers);
  check
    "rope indexed trig blocker"
    (List.mem "host_fp_trig" blockers);
  check
    "rope indexed multiply blocker"
    (List.mem "fp64_multiply_conformance" blockers);
  check
    "rope indexed add/sub blocker"
    (List.mem "fp64_add_sub_conformance" blockers);
  (match List.assoc_opt "profile_contract" gate with
   | Some (`Assoc contract) ->
     check
       "rope indexed contract profile"
       (String.equal
          (string_value "profile_name" contract)
          "host-fp-trig-local-candidate");
     check
       "rope indexed native trig rounding"
       (String.equal
          (string_value "rounding_mode" contract)
          "host-runtime-native-pow-cos-sin")
   | _ -> failwith "missing rope indexed profile contract");
  (match
     Profile.validate_for_opcode
       ~opcode:"ROPE_APPLY_INDEXED_FP"
       ~profile:"host-fp-local-candidate"
   with
   | Error (Profile.Unsupported_opcode_profile { opcode; profile; expected }) ->
     check
       "rope indexed old host profile opcode"
       (String.equal opcode "ROPE_APPLY_INDEXED_FP");
     check
       "rope indexed old host profile rejected"
       (String.equal profile "host-fp-local-candidate");
     check
       "rope indexed old host profile expected"
       (String.equal expected "host-fp-trig-local-candidate")
   | Error error -> failwith (Profile.error_message error)
   | Ok _ -> failwith "expected rope indexed old host profile rejection");
  match
    Profile.validate_for_opcode
      ~opcode:"ROPE_APPLY_INDEXED_FP"
      ~profile:"q16-exact"
  with
  | Error (Profile.Unsupported_opcode_profile { opcode; profile; expected }) ->
    check
      "rope indexed overclaim opcode"
      (String.equal opcode "ROPE_APPLY_INDEXED_FP");
    check "rope indexed overclaim profile" (String.equal profile "q16-exact");
    check
      "rope indexed overclaim expected"
      (String.equal expected "host-fp-trig-local-candidate")
  | Error error -> failwith (Profile.error_message error)
  | Ok _ -> failwith "expected rope indexed profile overclaim rejection"

let check_activation_profile_gates () =
  let runtime_activation_profile = function
    | "SIGMOID_FP" -> "deterministic-fp64-sigmoid"
    | "SOFTPLUS_FP" -> "deterministic-fp64-softplus"
    | "SILU_FP" -> "deterministic-fp64-silu"
    | _ -> "unknown"
  in
  List.iter
    (fun (opcode, local_needle, obligation_needle) ->
      check
        (opcode ^ " runtime profile")
        (match Profile.current_runtime_profile ~opcode with
         | Some "deterministic-fp64-sigmoid"
         | Some "deterministic-fp64-softplus"
         | Some "deterministic-fp64-silu" -> true
         | _ -> false);
      let gate = profile_gate opcode in
      check
        (opcode ^ " deterministic profile")
        (let name = string_value "name" gate in
         String.equal name "deterministic-fp64-sigmoid"
         || String.equal name "deterministic-fp64-softplus"
         || String.equal name "deterministic-fp64-silu");
      check
        (opcode ^ " is consensus-ready")
        (String.equal (string_value "consensus_status" gate) "consensus_ready");
      check
        (opcode ^ " local semantics")
        (list_contains_substring
           local_needle
           (string_list_value "local_semantics" gate));
      check
        (opcode ^ " consensus obligations")
        (list_contains_substring
           obligation_needle
           (string_list_value "consensus_obligations" gate));
      check_protocol_owned_transcendental_obligation opcode gate;
      (match
         Profile.validate_for_opcode
           ~opcode
           ~profile:"host-fp-local-candidate"
       with
       | Error (Profile.Unsupported_opcode_profile { opcode = actual; profile; expected }) ->
         check (opcode ^ " old host profile opcode") (String.equal actual opcode);
         check
           (opcode ^ " old host profile rejected")
           (String.equal profile "host-fp-local-candidate");
         check
           (opcode ^ " old host profile expected")
           (String.equal expected (runtime_activation_profile opcode))
       | Error error -> failwith (Profile.error_message error)
       | Ok _ -> failwith (opcode ^ " should reject old host profile"));
      match Profile.validate_for_opcode ~opcode ~profile:"q16-exact" with
      | Error (Profile.Unsupported_opcode_profile { opcode = actual; profile; expected }) ->
        check (opcode ^ " overclaim opcode") (String.equal actual opcode);
        check (opcode ^ " overclaim profile") (String.equal profile "q16-exact");
        check
          (opcode ^ " overclaim expected")
          (String.equal expected (runtime_activation_profile opcode))
      | Error error -> failwith (Profile.error_message error)
      | Ok _ -> failwith (opcode ^ " should reject profile overclaim"))
    [
      "SIGMOID_FP", "protocol exp", "protocol-owned deterministic";
      "SOFTPLUS_FP", "protocol log1p uses the artanh series", "protocol-owned deterministic";
      "SILU_FP", "protocol exp gate", "sigmoid composition order";
    ];
  let sigmoid_gate = profile_gate "SIGMOID_FP" in
  let sigmoid_blockers =
    string_list_value "consensus_blocker_codes" sigmoid_gate
  in
  check
    "sigmoid protocol exp blocker"
    (List.mem "protocol_owned_exp_conformance" sigmoid_blockers);
  check
    "sigmoid comparison blocker"
    (List.mem "fp64_comparison_conformance" sigmoid_blockers);
  check
    "sigmoid add blocker"
    (List.mem "fp64_add_conformance" sigmoid_blockers);
  check
    "sigmoid divide blocker"
    (List.mem "fp64_divide_conformance" sigmoid_blockers);
  check
    "sigmoid generic host arithmetic retired"
    (not (List.mem "host_fp_arithmetic" sigmoid_blockers));
  (match List.assoc_opt "profile_contract" sigmoid_gate with
   | Some (`Assoc contract) ->
     check
       "sigmoid operation records branch"
       (list_contains_substring
          "select_exp_branch_by_deterministic_sign_compare"
          (string_list_value "operation_sequence" contract));
     check
       "sigmoid edge records exp gate"
       (List.mem
          "protocol_exp_input_must_be_finite_and_nonpositive"
          (string_list_value "edge_value_policy" contract))
   | _ -> failwith "missing sigmoid profile contract");
  let softplus_gate = profile_gate "SOFTPLUS_FP" in
  let softplus_blockers =
    string_list_value "consensus_blocker_codes" softplus_gate
  in
  check
    "softplus protocol exp blocker"
    (List.mem "protocol_owned_exp_conformance" softplus_blockers);
  check
    "softplus protocol log1p blocker"
    (List.mem "protocol_owned_log1p_conformance" softplus_blockers);
  check
    "softplus add blocker"
    (List.mem "fp64_add_conformance" softplus_blockers);
  check
    "softplus generic host add retired"
    (not (List.mem "host_fp_add" softplus_blockers));
  check
    "softplus generic host arithmetic retired"
    (not (List.mem "host_fp_arithmetic" softplus_blockers));
  (match List.assoc_opt "profile_contract" softplus_gate with
   | Some (`Assoc contract) ->
     check
       "softplus operation records exp gate"
       (list_contains_substring
          "check_exp_input_nonpositive_deterministic"
          (string_list_value "operation_sequence" contract));
     check
       "softplus operation records log1p gate"
       (list_contains_substring
          "check_log1p_input_nonnegative_deterministic"
          (string_list_value "operation_sequence" contract));
     check
       "softplus edge records log1p gate"
       (List.mem
          "protocol_log1p_input_must_be_finite_and_nonnegative"
          (string_list_value "edge_value_policy" contract))
   | _ -> failwith "missing softplus profile contract");
  let silu_gate = profile_gate "SILU_FP" in
  let silu_blockers =
    string_list_value "consensus_blocker_codes" silu_gate
  in
  check
    "silu protocol exp blocker"
    (List.mem "protocol_owned_exp_conformance" silu_blockers);
  check
    "silu multiply blocker"
    (List.mem "fp64_multiply_conformance" silu_blockers);
  check
    "silu generic host arithmetic retired"
    (not (List.mem "host_fp_arithmetic" silu_blockers));
  (match List.assoc_opt "profile_contract" silu_gate with
   | Some (`Assoc contract) ->
     check
       "silu operation records sigmoid reuse"
       (list_contains_substring
          "compute_sigmoid_with_deterministic_sign_branch"
          (string_list_value "operation_sequence" contract));
     check
       "silu edge records exp gate"
       (List.mem
          "reuse_sigmoid_protocol_exp_gate"
          (string_list_value "edge_value_policy" contract))
   | _ -> failwith "missing silu profile contract")

let check_vector_arithmetic_profile_gates () =
  List.iter
    (fun (opcode, local_needle, obligation_needle) ->
      check
        (opcode ^ " runtime profile")
        (match Profile.current_runtime_profile ~opcode with
         | Some "deterministic-fp64-elementwise" -> true
         | _ -> false);
      let gate = profile_gate opcode in
      check
        (opcode ^ " consensus candidate")
        (String.equal
           (string_value "consensus_status" gate)
           "consensus_candidate");
      check
        (opcode ^ " local semantics")
        (list_contains_substring
           local_needle
           (string_list_value "local_semantics" gate));
      check
        (opcode ^ " records no host math")
        (list_contains_substring
           "no native host math"
           (string_list_value "local_semantics" gate));
      check
        (opcode ^ " consensus obligations")
        (list_contains_substring
           obligation_needle
           (string_list_value "consensus_obligations" gate));
      check
        (opcode ^ " profile root obligation")
        (list_contains_substring
           "profile root"
           (string_list_value "consensus_obligations" gate));
      let blockers = string_list_value "consensus_blocker_codes" gate in
      check
        (opcode ^ " no host arithmetic blocker")
        (not (List.mem "host_fp_arithmetic" blockers));
      (match List.assoc_opt "profile_contract" gate with
       | Some (`Assoc contract) ->
         check
           (opcode ^ " profile name")
           (String.equal
              (string_value "profile_name" contract)
              "deterministic-fp64-elementwise");
         check
           (opcode ^ " rounding mode")
           (String.equal
              (string_value "rounding_mode" contract)
              "deterministic-binary64-roundTiesToEven")
       | _ -> failwith ("missing " ^ opcode ^ " profile contract"));
      (match
         Profile.validate_for_opcode
           ~opcode
           ~profile:"deterministic-fp64-elementwise"
       with
       | Ok profile ->
         check
           (opcode ^ " deterministic profile accepted")
           (String.equal profile.Profile.name "deterministic-fp64-elementwise")
       | Error error -> failwith (Profile.error_message error));
      (match
         Profile.validate_for_opcode
           ~opcode
           ~profile:"host-fp-local-candidate"
       with
       | Error (Profile.Unsupported_opcode_profile { opcode = actual; profile; expected }) ->
         check (opcode ^ " old host profile opcode") (String.equal actual opcode);
         check
           (opcode ^ " old host profile rejected")
           (String.equal profile "host-fp-local-candidate");
         check
           (opcode ^ " old host profile expected")
           (String.equal expected "deterministic-fp64-elementwise")
       | Error error -> failwith (Profile.error_message error)
       | Ok _ -> failwith (opcode ^ " should reject old host profile"));
      match Profile.validate_for_opcode ~opcode ~profile:"q16-exact" with
      | Error (Profile.Unsupported_opcode_profile { opcode = actual; profile; expected }) ->
        check (opcode ^ " overclaim opcode") (String.equal actual opcode);
        check (opcode ^ " overclaim profile") (String.equal profile "q16-exact");
        check
          (opcode ^ " overclaim expected")
          (String.equal expected "deterministic-fp64-elementwise")
      | Error error -> failwith (Profile.error_message error)
      | Ok _ -> failwith (opcode ^ " should reject profile overclaim"))
    [
      "ELEMWISE_MUL_FP", "deterministic finite binary64 multiplication", "elementwise multiply edge vectors";
      "RESIDUAL_ADD_FP", "deterministic finite binary64 addition", "residual add edge vectors";
    ];
  let check_blocker opcode blocker =
    let gate = profile_gate opcode in
    check
      (opcode ^ " blocker")
      (List.mem blocker (string_list_value "consensus_blocker_codes" gate))
  in
  check_blocker "ELEMWISE_MUL_FP" "fp64_multiply_conformance";
  check_blocker "RESIDUAL_ADD_FP" "fp64_add_conformance"

let check_attention_profile_gates () =
  List.iter
    (fun (opcode, local_needle, obligation_needle) ->
      check
        (opcode ^ " runtime profile")
        (match Profile.current_runtime_profile ~opcode with
         | Some "deterministic-fp64-accumulation" -> true
         | _ -> false);
      let gate = profile_gate opcode in
      check
        (opcode ^ " consensus candidate")
        (String.equal
           (string_value "consensus_status" gate)
           "consensus_candidate");
      check
        (opcode ^ " profile name")
        (String.equal
           (string_value "name" gate)
           "deterministic-fp64-accumulation");
      check
        (opcode ^ " local semantics")
        (list_contains_substring
           local_needle
           (string_list_value "local_semantics" gate));
      check
        (opcode ^ " consensus obligations")
        (list_contains_substring
           obligation_needle
           (string_list_value "consensus_obligations" gate));
      (match
         Profile.validate_for_opcode
           ~opcode
           ~profile:"host-fp-local-candidate"
       with
       | Error (Profile.Unsupported_opcode_profile { opcode = actual; profile; expected }) ->
         check (opcode ^ " old host profile opcode") (String.equal actual opcode);
         check
           (opcode ^ " old host profile rejected")
           (String.equal profile "host-fp-local-candidate");
         check
           (opcode ^ " old host profile expected")
           (String.equal expected "deterministic-fp64-accumulation")
       | Error error -> failwith (Profile.error_message error)
       | Ok _ -> failwith (opcode ^ " should reject old host profile"));
      match Profile.validate_for_opcode ~opcode ~profile:"q16-exact" with
      | Error (Profile.Unsupported_opcode_profile { opcode = actual; profile; expected }) ->
        check (opcode ^ " overclaim opcode") (String.equal actual opcode);
        check (opcode ^ " overclaim profile") (String.equal profile "q16-exact");
        check
          (opcode ^ " overclaim expected")
          (String.equal expected "deterministic-fp64-accumulation")
      | Error error -> failwith (Profile.error_message error)
      | Ok _ -> failwith (opcode ^ " should reject profile overclaim"))
    [
      ( "ATTENTION_SCORES_FP",
        "deterministic finite binary64 multiplication",
        "attention-score edge vectors" );
      ( "ATTENTION_WEIGHTED_SUM_FP",
        "deterministic finite binary64 multiplication",
        "weighted-sum edge vectors" );
    ];
  let weighted_sum_gate = profile_gate "ATTENTION_WEIGHTED_SUM_FP" in
  check
    "ATTENTION_WEIGHTED_SUM_FP does not renormalize probabilities"
    (list_contains_substring
       "not renormalized"
       (string_list_value "local_semantics" weighted_sum_gate));
  let check_attention_contract opcode sequence_needle =
    let gate = profile_gate opcode in
    let blockers = string_list_value "consensus_blocker_codes" gate in
    check
      (opcode ^ " no longer uses generic host arithmetic blocker")
      (not (List.mem "host_fp_arithmetic" blockers));
    check
      (opcode ^ " multiply blocker")
      (List.mem "fp64_multiply_conformance" blockers);
    check
      (opcode ^ " add blocker")
      (List.mem "fp64_add_conformance" blockers);
    match List.assoc_opt "profile_contract" gate with
    | Some (`Assoc contract) ->
      check
        (opcode ^ " deterministic rounding")
        (String.equal
           (string_value "rounding_mode" contract)
           "deterministic-binary64-roundTiesToEven");
      check
        (opcode ^ " contract profile")
        (String.equal
           (string_value "profile_name" contract)
           "deterministic-fp64-accumulation");
      check
        (opcode ^ " deterministic sequence")
        (list_contains_substring
           sequence_needle
           (string_list_value "operation_sequence" contract))
    | _ -> failwith ("missing " ^ opcode ^ " profile contract")
  in
  check_attention_contract
    "ATTENTION_SCORES_FP"
    "accumulate_dot_product_left_to_right";
  check_attention_contract
    "ATTENTION_WEIGHTED_SUM_FP"
    "accumulate_weighted_values_left_to_right"

let check_causal_conv_profile_gate () =
  check
    "causal conv runtime profile"
    (match Profile.current_runtime_profile ~opcode:"CAUSAL_DEPTHWISE_CONV1D_FP" with
     | Some "deterministic-fp64-accumulation" -> true
     | _ -> false);
  let gate = profile_gate "CAUSAL_DEPTHWISE_CONV1D_FP" in
  check
    "causal conv consensus candidate"
    (String.equal (string_value "consensus_status" gate) "consensus_candidate");
  check
    "causal conv profile name"
    (String.equal
       (string_value "name" gate)
       "deterministic-fp64-accumulation");
  check
    "causal conv local indexing semantics"
    (list_contains_substring
       "causal depthwise indexing"
       (string_list_value "local_semantics" gate));
  check
    "causal conv snapshot semantics"
    (list_contains_substring
       "snapshotted before output writeback"
       (string_list_value "local_semantics" gate));
  check
    "causal conv deterministic multiply/add semantics"
    (list_contains_substring
       "deterministic finite binary64 multiplication and addition"
       (string_list_value "local_semantics" gate));
  check
    "causal conv edge obligation"
    (list_contains_substring
       "causal-convolution edge vectors"
       (string_list_value "consensus_obligations" gate));
  let blockers = string_list_value "consensus_blocker_codes" gate in
  check
    "causal conv no generic host arithmetic blocker"
    (not (List.mem "host_fp_arithmetic" blockers));
  check
    "causal conv multiply blocker"
    (List.mem "fp64_multiply_conformance" blockers);
  check
    "causal conv add blocker"
    (List.mem "fp64_add_conformance" blockers);
  (match List.assoc_opt "profile_contract" gate with
   | Some (`Assoc contract) ->
     check
       "causal conv deterministic rounding"
       (String.equal
          (string_value "rounding_mode" contract)
          "deterministic-binary64-roundTiesToEven");
     check
       "causal conv contract profile"
       (String.equal
          (string_value "profile_name" contract)
          "deterministic-fp64-accumulation");
     check
       "causal conv deterministic accumulation sequence"
       (list_contains_substring
          "accumulate_kernel_left_to_right"
          (string_list_value "operation_sequence" contract))
   | _ -> failwith "missing causal conv profile contract");
  (match
     Profile.validate_for_opcode
       ~opcode:"CAUSAL_DEPTHWISE_CONV1D_FP"
       ~profile:"host-fp-local-candidate"
   with
   | Error (Profile.Unsupported_opcode_profile { opcode; profile; expected }) ->
     check
       "causal conv old host profile opcode"
       (String.equal opcode "CAUSAL_DEPTHWISE_CONV1D_FP");
     check
       "causal conv old host profile rejected"
       (String.equal profile "host-fp-local-candidate");
     check
       "causal conv old host profile expected"
       (String.equal expected "deterministic-fp64-accumulation")
   | Error error -> failwith (Profile.error_message error)
   | Ok _ -> failwith "expected causal conv old host profile rejection");
  match
    Profile.validate_for_opcode
      ~opcode:"CAUSAL_DEPTHWISE_CONV1D_FP"
      ~profile:"q16-exact"
  with
  | Error (Profile.Unsupported_opcode_profile { opcode; profile; expected }) ->
    check
      "causal conv overclaim opcode"
      (String.equal opcode "CAUSAL_DEPTHWISE_CONV1D_FP");
    check "causal conv overclaim profile" (String.equal profile "q16-exact");
    check
      "causal conv overclaim expected"
      (String.equal expected "deterministic-fp64-accumulation")
  | Error error -> failwith (Profile.error_message error)
  | Ok _ -> failwith "expected causal conv profile overclaim rejection"

let check_byte_ingress_profile_gates () =
  List.iter
    (fun (opcode, profile_name, expected_status, local_needle, obligation_needle) ->
      check
        (opcode ^ " runtime profile")
        (match Profile.current_runtime_profile ~opcode with
         | Some actual -> String.equal actual profile_name
         | _ -> false);
      let profile =
        match Profile.of_name profile_name with
        | Ok profile -> profile
        | Error error -> failwith (Profile.error_message error)
      in
      check
        (opcode ^ " consensus status")
        (String.equal
           (Profile.status_string profile.Profile.consensus_status)
           expected_status);
      let gate =
        match Profile.to_json_for_opcode ~opcode profile with
        | `Assoc gate -> gate
        | _ -> failwith "byte ingress profile gate must be object"
      in
      check
        (opcode ^ " local semantics")
        (list_contains_substring
           local_needle
           (string_list_value "local_semantics" gate));
      check
        (opcode ^ " consensus obligations")
        (list_contains_substring
           obligation_needle
           (string_list_value "consensus_obligations" gate));
      List.iter
        (fun rejected_profile ->
          match Profile.validate_for_opcode ~opcode ~profile:rejected_profile with
          | Error (Profile.Unsupported_opcode_profile { opcode = actual; profile = actual_profile; expected }) ->
            check (opcode ^ " overclaim opcode") (String.equal actual opcode);
            check (opcode ^ " overclaim profile") (String.equal actual_profile rejected_profile);
            check
              (opcode ^ " overclaim expected")
              (String.equal expected profile_name)
          | Error error -> failwith (Profile.error_message error)
          | Ok _ -> failwith (opcode ^ " should reject profile " ^ rejected_profile))
        ["host-fp-local-candidate"; "q16-exact"])
    [
      "LOAD_F32_LE_FP", "byte-ingress-f32-bits", "consensus_ready",
      "finite f32→f64 bit widen", "f32 little-endian ingress";
      "LOAD_F64_LE_FP", "byte-ingress-f64-bits", "consensus_ready",
      "bit patterns are copied", "f64 little-endian ingress";
    ]

let check_rejects_unknown_opcode () =
  match Template.of_json (template ~opcode:"MODEL_SPECIFIC_FASTPATH" ()) with
  | Error (Template.Template_error message) ->
    check
      "unknown opcode"
      (String.equal message "unsupported P0 opcode: MODEL_SPECIFIC_FASTPATH")
  | Error error -> failwith (Template.error_message error)
  | Ok _ -> failwith "expected unknown opcode rejection"

let check_rejects_effect_drift () =
  match Template.of_json (template ~effects:["memory_read"] ()) with
  | Error (Template.Template_error message) ->
    check
      "effect drift"
      (String.equal message "missing required effects: memory_write")
  | Error error -> failwith (Template.error_message error)
  | Ok _ -> failwith "expected effect rejection"

let check_rejects_missing_failure_cases () =
  match Template.of_json (template ~failure_cases:[] ()) with
  | Error (Template.Template_error message) ->
    check
      "missing failures"
      (String.equal message "failure cases are required")
  | Error error -> failwith (Template.error_message error)
  | Ok _ -> failwith "expected failure-case rejection"

let check_rejects_unknown_profile () =
  match Template.of_json (template ~profile:"vendor-fast-float" ()) with
  | Error (Template.Template_error message) ->
    check
      "unknown profile"
      (String.equal message "unknown numerical profile: vendor-fast-float")
  | Error error -> failwith (Template.error_message error)
  | Ok _ -> failwith "expected profile rejection"

let check_rejects_profile_overclaim () =
  match Template.of_json (template ~profile:"soft-fp-exact" ()) with
  | Error (Template.Template_error message) ->
    check
      "profile overclaim"
      (String.equal
         message
         "profile soft-fp-exact is not implemented for opcode \
          RMSNORM_FP_EPS; expected deterministic-fp64-rmsnorm")
  | Error error -> failwith (Template.error_message error)
  | Ok _ -> failwith "expected profile overclaim rejection"

let check_rejects_bad_roots () =
  match Template.of_json (template ~numerical_profile_root:"not-a-root" ()) with
  | Error (Template.Template_error message) ->
    check
      "bad numerical root"
      (String.equal
         message
         "numerical_profile_root must be a 32-byte hex root")
  | Error error -> failwith (Template.error_message error)
  | Ok _ -> failwith "expected numerical root rejection"

let check_rejects_non_writable_template () =
  match Template.of_json (template ~memory_access:"read" ()) with
  | Error (Template.Template_error message) ->
    check
      "missing writable memory"
      (String.equal message "at least one writable memory binding is required")
  | Error error -> failwith (Template.error_message error)
  | Ok _ -> failwith "expected writable-memory rejection"

let check_rejects_wide_register () =
  match
    Template.of_json
      (`Assoc [
        "type", `String "litenode_vm_conformance_template";
        "schema", `Int 1;
        "opcode", `String "RMSNORM_FP_EPS";
        "primitive", `String "rmsnorm_fp_eps";
        "profile", `String "deterministic-fp64-normalization";
        "vm_semantics_root", `String (hex_root 'd');
        "numerical_profile_root", `String (hex_root 'e');
        "expected_effort", `Int 16;
        "effects", `List [`String "memory_read"; `String "memory_write"];
        "registers",
        `List [register 64 "bad" "address" "100"];
        "memory", `List [memory "input" "read_write"];
        "expected",
        `Assoc [
          "spans",
          `List [
            `Assoc [
              "name", `String "output";
              "base", `Int 100;
              "cells", `Int 4;
              "output_root", `String (hex_root 'b');
              "output_sha256", `Null;
            ];
          ];
        ];
        "failure_cases", `List [failure "nonfinite_input_nan"];
      ])
  with
  | Error (Template.Template_error message) ->
    check
      "wide register"
      (String.equal message "register out of range: 64")
  | Error error -> failwith (Template.error_message error)
  | Ok _ -> failwith "expected register rejection"

let check_rejects_single_delta_expected_span () =
  match
    Template.of_json
      (template
         ~opcode:"GATED_DELTA_RULE_FP"
         ~primitive:"gated_delta_rule_fp"
         ())
  with
  | Error (Template.Template_error message) ->
    check
      "delta span count"
      (String.equal
         message
         "GATED_DELTA_RULE_FP requires output and next-state expected spans")
  | Error error -> failwith (Template.error_message error)
  | Ok _ -> failwith "expected delta span rejection"

let check_q1_required_failure_expectations () =
  check
    "q1 required failure expectations"
    (Template.q1_required_failure_expectations
     = [
       "nonfinite_input_nan", "reject_before_write";
       "nonfinite_input_infinity", "reject_before_write";
       "output_input_aliasing", "accept_from_snapshot";
       "partial_output_input_aliasing", "accept_from_snapshot";
       "k_not_multiple_of_128", "reject_before_write";
       "bad_q1_owner_length", "reject_before_write";
       "negative_byte_offset", "reject_before_write";
       "byte_offset_out_of_bounds", "reject_before_write";
       "byte_offset_truncated_span", "reject_before_write";
       "nonfinite_fp16_scale", "reject_before_write";
       "lower_effort_limit", "reject_before_write";
     ]);
  check
    "q1 required failure expectations json"
    (Template.q1_required_failure_expectations_json
     = `Assoc [
       "opcode", `String "LINEAR_Q1_G128_FP";
       "expectations",
       `List [
         `Assoc [
           "case", `String "nonfinite_input_nan";
           "expected_prefix", `String "reject_before_write";
         ];
         `Assoc [
           "case", `String "nonfinite_input_infinity";
           "expected_prefix", `String "reject_before_write";
         ];
         `Assoc [
           "case", `String "output_input_aliasing";
           "expected_prefix", `String "accept_from_snapshot";
         ];
         `Assoc [
           "case", `String "partial_output_input_aliasing";
           "expected_prefix", `String "accept_from_snapshot";
         ];
         `Assoc [
           "case", `String "k_not_multiple_of_128";
           "expected_prefix", `String "reject_before_write";
         ];
         `Assoc [
           "case", `String "bad_q1_owner_length";
           "expected_prefix", `String "reject_before_write";
         ];
         `Assoc [
           "case", `String "negative_byte_offset";
           "expected_prefix", `String "reject_before_write";
         ];
         `Assoc [
           "case", `String "byte_offset_out_of_bounds";
           "expected_prefix", `String "reject_before_write";
         ];
         `Assoc [
           "case", `String "byte_offset_truncated_span";
           "expected_prefix", `String "reject_before_write";
         ];
         `Assoc [
           "case", `String "nonfinite_fp16_scale";
           "expected_prefix", `String "reject_before_write";
         ];
         `Assoc [
           "case", `String "lower_effort_limit";
           "expected_prefix", `String "reject_before_write";
         ];
       ];
     ])

let check_p0_vm_semantics_contracts () =
  List.iter
    (fun opcode ->
       match Template.vm_semantics_contract_json ~opcode with
       | Some (`Assoc semantics) ->
         check
           (opcode ^ " vm semantics schema")
           (String.equal
              (string_value "schema" semantics)
              "octra.inference.vm-semantics.v1");
         check
           (opcode ^ " vm semantics opcode")
           (String.equal (string_value "opcode" semantics) opcode);
         check
           (opcode ^ " vm semantics has signature")
           (String.length (string_value "signature" semantics) > 0);
         (match Template.vm_semantics_root_for_opcode ~opcode with
          | Some root ->
            check (opcode ^ " vm semantics root is hex") (root_ok root);
            (match
               Template.vm_semantics_binding_json
                 ~opcode
                 ~vm_semantics_root:root
             with
             | `Assoc binding ->
               check
                 (opcode ^ " vm semantics binding accepted")
                 (String.equal (string_value "status" binding) "matched")
             | _ -> failwith (opcode ^ " vm semantics binding must be object"))
          | None -> failwith (opcode ^ " missing vm semantics root"))
       | _ -> failwith (opcode ^ " missing vm semantics contract"))
    Template.p0_opcodes;
	  (match Template.vm_semantics_contract_json ~opcode:"SOFTMAX_FP" with
	   | Some (`Assoc semantics) ->
	     check
	       "softmax semantics marks protocol exp"
	       (list_contains_substring
	          "protocol-owned deterministic nonpositive binary64 exp"
	          (string_list_value "arithmetic_policy" semantics));
	     check
	       "softmax semantics marks consensus ready"
	       (contains_substring
	          "consensus_ready"
	          (string_value "consensus_note" semantics));
	     check
	       "softmax semantics no longer claims candidate-only status"
	       (not
	          (contains_substring
	             "remains a consensus candidate"
	             (string_value "consensus_note" semantics)))
	   | _ -> failwith "missing softmax vm semantics contract");
  (match Template.vm_semantics_contract_json ~opcode:"GATED_DELTA_RULE_FP" with
   | Some (`Assoc semantics) ->
     check
       "gated delta semantics pins correction term"
       (list_contains_substring
          "delta[row] = (v[row] - memory[row]) * beta"
          (string_list_value "arithmetic_policy" semantics));
	    check
	      "gated delta semantics marks protocol exp"
	      (list_contains_substring
	         "protocol-owned Q256"
	         (string_list_value "arithmetic_policy" semantics));
	    check
	      "gated delta semantics marks consensus ready"
	      (contains_substring
	         "consensus_ready"
	         (string_value "consensus_note" semantics));
	    check
	      "gated delta semantics no longer claims candidate-only status"
	      (not
	         (contains_substring
	            "remains a consensus candidate"
	            (string_value "consensus_note" semantics)))
   | _ -> failwith "missing gated delta vm semantics contract");
  (match Template.vm_semantics_contract_json ~opcode:"RMSNORM_FP_EPS" with
   | Some (`Assoc semantics) ->
     check
       "rmsnorm semantics pins inverse sqrt"
       (list_contains_substring
          "inverse_sqrt"
          (string_list_value "arithmetic_policy" semantics))
   | _ -> failwith "missing rmsnorm vm semantics contract");
  (match Template.vm_semantics_contract_json ~opcode:"L2NORM_FP" with
   | Some (`Assoc semantics) ->
     check
       "l2norm semantics pins inverse sqrt"
       (list_contains_substring
          "inverse_sqrt"
          (string_list_value "arithmetic_policy" semantics));
     check
       "l2norm semantics marks consensus ready"
       (contains_substring
          "consensus_ready"
          (string_value "consensus_note" semantics));
     check
       "l2norm semantics no longer claims candidate-only status"
       (not
          (contains_substring
             "remains a consensus candidate"
             (string_value "consensus_note" semantics)))
   | _ -> failwith "missing l2norm vm semantics contract");
  (match Template.vm_semantics_contract_json ~opcode:"LOAD_F32_LE_FP" with
   | Some (`Assoc semantics) ->
     check
       "load f32 semantics pins bits-only writeback"
       (list_contains_substring
          "mem_set_fp64_bits"
          (string_list_value "decode_policy" semantics));
     check
       "load f32 semantics marks consensus ready"
       (contains_substring
          "consensus_ready"
          (string_value "consensus_note" semantics));
     check
       "load f32 semantics no longer claims candidate-only status"
       (not
          (contains_substring
             "remains a consensus candidate"
             (string_value "consensus_note" semantics)))
   | _ -> failwith "missing load f32 vm semantics contract")

let () =
  check_accepts_template ();
  check_q1_profile_obligations ();
  check_q1_required_failure_expectations ();
  check_p0_vm_semantics_contracts ();
  check_profile_root ();
  check_profile_root_binding_counts ();
  check_profile_root_binding_catalog ();
  check_profile_status_counts ();
  check_validator_readiness_predicate ();
  check_abi_declaration_binding ();
  check_p0_profile_gate_coverage ();
  check_inference_profile_surface_coverage ();
  check_remaining_p0_profile_obligations ();
  check_argmax_profile_gate ();
  check_rope_indexed_profile_gate ();
  check_activation_profile_gates ();
  check_vector_arithmetic_profile_gates ();
  check_attention_profile_gates ();
  check_causal_conv_profile_gate ();
  check_byte_ingress_profile_gates ();
  check_rejects_unknown_opcode ();
  check_rejects_effect_drift ();
  check_rejects_missing_failure_cases ();
  check_rejects_unknown_profile ();
  check_rejects_profile_overclaim ();
  check_rejects_bad_roots ();
  check_rejects_non_writable_template ();
  check_rejects_wide_register ();
  check_rejects_single_delta_expected_span ()
