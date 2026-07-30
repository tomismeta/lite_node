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

let check label condition =
  if not condition then failwith label

let hex_root char =
  String.make 64 char

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

let string_list_value name fields =
  match assoc_value name fields with
  | `List values ->
    List.map
      (function
        | `String value -> value
        | _ -> failwith ("json field must be a string list: " ^ name))
      values
  | _ -> failwith ("json field must be a list: " ^ name)

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

let template ?(opcode = "RMSNORM_FP_EPS") ?(primitive = "rmsnorm_fp_eps")
    ?(effects = ["memory_read"; "memory_write"]) ?(memory_access = "read_write")
    ?(profile = "host-fp-local-candidate") ?(vm_semantics_root = hex_root 'd')
    ?(numerical_profile_root = hex_root 'e')
    ?(failure_cases = [failure "nonfinite_input_nan"]) () =
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
    check "p0 opcode count" (List.length Template.p0_opcodes = 5);
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
      check
        "consensus status"
        (match List.assoc_opt "consensus_status" fields with
         | Some (`String "local_only") -> true
         | _ -> false);
      check
        "profile gate"
        (match List.assoc_opt "profile_gate" fields with
         | Some (`Assoc gate) ->
           String.equal (string_value "opcode" gate) "RMSNORM_FP_EPS"
           && list_contains_substring
                "sqrt"
                (string_list_value "consensus_obligations" gate)
           && list_contains_substring
                "epsilon"
                (string_list_value "local_semantics" gate)
           && list_contains_substring
                "minimum positive subnormal epsilon"
                (string_list_value "local_semantics" gate)
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
               (string_list_value "consensus_obligations" gate))
        | _ -> failwith "missing q1 profile gate")
     | _ -> failwith "template json must be object")

let profile_gate opcode =
  match Profile.of_name "host-fp-local-candidate" with
  | Error error -> failwith (Profile.error_message error)
  | Ok profile ->
    (match Profile.to_json_for_opcode ~opcode profile with
     | `Assoc gate -> gate
     | _ -> failwith "profile gate must be object")

let check_p0_profile_gate_coverage () =
  List.iter
    (fun opcode ->
      check
        (opcode ^ " runtime profile")
        (match Profile.current_runtime_profile ~opcode with
         | Some "host-fp-local-candidate" -> true
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
        (opcode ^ " consensus obligations are specific")
        (not
           (list_contains_substring
              "write primitive-specific"
              consensus_obligations)))
    Template.p0_opcodes

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
    ]

let check_argmax_profile_gate () =
  check
    "argmax runtime profile"
    (match Profile.current_runtime_profile ~opcode:"ARGMAX_FP" with
     | Some "host-fp-local-candidate" -> true
     | _ -> false);
  let gate = profile_gate "ARGMAX_FP" in
  check
    "argmax local tie semantics"
    (list_contains_substring
       "lowest zero-based index"
       (string_list_value "local_semantics" gate));
  check
    "argmax ordering obligation"
    (list_contains_substring
       "ordering preservation"
       (string_list_value "consensus_obligations" gate));
  match Profile.validate_for_opcode ~opcode:"ARGMAX_FP" ~profile:"q16-exact" with
  | Error (Profile.Unsupported_opcode_profile { opcode; profile; expected }) ->
    check "argmax overclaim opcode" (String.equal opcode "ARGMAX_FP");
    check "argmax overclaim profile" (String.equal profile "q16-exact");
    check
      "argmax overclaim expected"
      (String.equal expected "host-fp-local-candidate")
  | Error error -> failwith (Profile.error_message error)
  | Ok _ -> failwith "expected argmax profile overclaim rejection"

let check_rope_indexed_profile_gate () =
  check
    "rope indexed runtime profile"
    (match Profile.current_runtime_profile ~opcode:"ROPE_APPLY_INDEXED_FP" with
     | Some "host-fp-local-candidate" -> true
     | _ -> false);
  let gate = profile_gate "ROPE_APPLY_INDEXED_FP" in
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
      (String.equal expected "host-fp-local-candidate")
  | Error error -> failwith (Profile.error_message error)
  | Ok _ -> failwith "expected rope indexed profile overclaim rejection"

let check_activation_profile_gates () =
  List.iter
    (fun (opcode, local_needle, obligation_needle) ->
      check
        (opcode ^ " runtime profile")
        (match Profile.current_runtime_profile ~opcode with
         | Some "host-fp-local-candidate" -> true
         | _ -> false);
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
           (string_list_value "consensus_obligations" gate));
      match Profile.validate_for_opcode ~opcode ~profile:"q16-exact" with
      | Error (Profile.Unsupported_opcode_profile { opcode = actual; profile; expected }) ->
        check (opcode ^ " overclaim opcode") (String.equal actual opcode);
        check (opcode ^ " overclaim profile") (String.equal profile "q16-exact");
        check
          (opcode ^ " overclaim expected")
          (String.equal expected "host-fp-local-candidate")
      | Error error -> failwith (Profile.error_message error)
      | Ok _ -> failwith (opcode ^ " should reject profile overclaim"))
    [
      "SIGMOID_FP", "1.0 / (1.0 + exp(-x))", "sigmoid edge vectors";
      "SOFTPLUS_FP", "log1p", "softplus edge vectors";
      "SILU_FP", "x * (1.0 / (1.0 + exp(-x)))", "SiLU edge vectors";
    ]

let check_vector_arithmetic_profile_gates () =
  List.iter
    (fun (opcode, local_needle, obligation_needle) ->
      check
        (opcode ^ " runtime profile")
        (match Profile.current_runtime_profile ~opcode with
         | Some "host-fp-local-candidate" -> true
         | _ -> false);
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
           (string_list_value "consensus_obligations" gate));
      match Profile.validate_for_opcode ~opcode ~profile:"q16-exact" with
      | Error (Profile.Unsupported_opcode_profile { opcode = actual; profile; expected }) ->
        check (opcode ^ " overclaim opcode") (String.equal actual opcode);
        check (opcode ^ " overclaim profile") (String.equal profile "q16-exact");
        check
          (opcode ^ " overclaim expected")
          (String.equal expected "host-fp-local-candidate")
      | Error error -> failwith (Profile.error_message error)
      | Ok _ -> failwith (opcode ^ " should reject profile overclaim"))
    [
      "ELEMWISE_MUL_FP", "native binary64 multiplication", "elementwise multiply edge vectors";
      "RESIDUAL_ADD_FP", "native binary64 addition", "residual add edge vectors";
    ]

let check_attention_profile_gates () =
  List.iter
    (fun (opcode, local_needle, obligation_needle) ->
      check
        (opcode ^ " runtime profile")
        (match Profile.current_runtime_profile ~opcode with
         | Some "host-fp-local-candidate" -> true
         | _ -> false);
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
           (string_list_value "consensus_obligations" gate));
      match Profile.validate_for_opcode ~opcode ~profile:"q16-exact" with
      | Error (Profile.Unsupported_opcode_profile { opcode = actual; profile; expected }) ->
        check (opcode ^ " overclaim opcode") (String.equal actual opcode);
        check (opcode ^ " overclaim profile") (String.equal profile "q16-exact");
        check
          (opcode ^ " overclaim expected")
          (String.equal expected "host-fp-local-candidate")
      | Error error -> failwith (Profile.error_message error)
      | Ok _ -> failwith (opcode ^ " should reject profile overclaim"))
    [
      "ATTENTION_SCORES_FP", "scale is computed", "attention-score edge vectors";
      "ATTENTION_WEIGHTED_SUM_FP", "not renormalized", "weighted-sum edge vectors";
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
          RMSNORM_FP_EPS; expected host-fp-local-candidate")
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
        "profile", `String "host-fp-local-candidate";
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

let () =
  check_accepts_template ();
  check_q1_profile_obligations ();
  check_p0_profile_gate_coverage ();
  check_remaining_p0_profile_obligations ();
  check_argmax_profile_gate ();
  check_rope_indexed_profile_gate ();
  check_activation_profile_gates ();
  check_vector_arithmetic_profile_gates ();
  check_attention_profile_gates ();
  check_rejects_unknown_opcode ();
  check_rejects_effect_drift ();
  check_rejects_missing_failure_cases ();
  check_rejects_unknown_profile ();
  check_rejects_profile_overclaim ();
  check_rejects_bad_roots ();
  check_rejects_non_writable_template ();
  check_rejects_wide_register ();
  check_rejects_single_delta_expected_span ()
