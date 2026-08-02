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

type memory_access =
  | Read
  | Write
  | Read_write

type register_binding = {
  register : int;
  name : string;
  kind : string;
  value : string;
}

type memory_binding = {
  name : string;
  path : string option;
  root : string;
  byte_length : int option;
  sha256 : string option;
  encoding : string;
  base : int;
  cells : int;
  access : memory_access;
}

type expected_span = {
  span_name : string;
  base : int;
  cells : int;
  output_root : string option;
  output_sha256 : string option;
}

type failure_case = {
  case_name : string;
  expected : string;
  mutations : string list;
  unchanged_spans : string list;
}

type t = {
  opcode : string;
  primitive : string;
  profile : string;
  vm_semantics_root : string;
  numerical_profile_root : string;
  expected_effort : int;
  effects : string list;
  registers : register_binding list;
  memory : memory_binding list;
  expected : expected_span list;
  failure_cases : failure_case list;
}

type error =
  | Json_error of string
  | Template_error of string

type requirement = {
  opcode : string;
  primitives : string list;
  effects : string list;
}

let error_message = function
  | Json_error message -> "invalid conformance template json: " ^ message
  | Template_error message -> "invalid conformance template: " ^ message

let ( let* ) result f =
  match result with
  | Ok value -> f value
  | Error error -> Error error

let requirements = [
  {
    opcode = "LINEAR_Q1_G128_FP";
    primitives = ["linear_q1_0_g128_fp"; "q1_g128_projection"];
    effects = ["memory_read"; "memory_write"];
  };
  {
    opcode = "RMSNORM_FP_EPS";
    primitives = ["rmsnorm_fp_eps"];
    effects = ["memory_read"; "memory_write"];
  };
  {
    opcode = "L2NORM_FP";
    primitives = ["l2norm_fp"; "l2norm_fp_eps"];
    effects = ["memory_read"; "memory_write"];
  };
  {
    opcode = "SOFTMAX_FP";
    primitives = ["softmax_fp"];
    effects = ["memory_read"; "memory_write"];
  };
  {
    opcode = "GATED_DELTA_RULE_FP";
    primitives = ["gated_delta_rule_fp"];
    effects = ["memory_read"; "memory_write"];
  };
]

let p0_opcodes = List.map (fun requirement -> requirement.opcode) requirements

let q1_required_failure_expectations =
  [
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
  ]

let q1_required_failure_expectations_json =
  `Assoc [
    "opcode", `String "LINEAR_Q1_G128_FP";
    "expectations",
    `List
      (List.map
         (fun (case, expected_prefix) ->
            `Assoc [
              "case", `String case;
              "expected_prefix", `String expected_prefix;
            ])
         q1_required_failure_expectations);
  ]

let sha256 raw =
  Digestif.SHA256.(digest_string raw |> to_hex)

let vm_semantics_contract_json ~opcode =
  match opcode with
  | "LINEAR_Q1_G128_FP" ->
    Some
      (`Assoc [
        "schema", `String "octra.inference.vm-semantics.v1";
        "opcode", `String opcode;
        "bytecode", `String "0x89";
        "signature",
        `String "LINEAR_Q1_G128_FP(dst, lhs, q1_owner, byte_offset, m, k, n)";
        "register_roles",
        `List [
          `String "dst: output base cell";
          `String "lhs: f64 cell input base";
          `String "q1_owner: immutable Q1 owner bytes";
          `String "byte_offset: byte offset into q1_owner";
          `String "m: row count";
          `String "k: inner dimension, multiple of 128";
          `String "n: output column count";
        ];
        "memory_units",
        `List [
          `String "lhs and output use one binary64 bit pattern per VM cell";
          `String "q1_owner uses immutable octets supplied as VM bytes/string";
          `String "session ABI uses r0 base and r1 VM cell count";
        ];
        "shape_policy",
        `List [
          `String "m,k,n must be positive";
          `String "k must be a multiple of 128";
          `String "m,k,n each must be at most 32768";
          `String "lhs span is m*k cells";
          `String "output span is m*n cells";
          `String "q1 span is n*(k/128)*18 bytes from byte_offset";
        ];
        "q1_block_layout",
        `List [
          `String "group size is exactly 128 input lanes";
          `String "each block is exactly 18 bytes";
          `String "bytes 0..1 are one IEEE-754 binary16 scale in little-endian order";
          `String "bytes 2..17 are 128 one-bit signs";
          `String "sign bit index item uses byte 2 + floor(item/8), bit item mod 8";
          `String "sign bit 1 maps to +scale and sign bit 0 maps to -scale";
          `String "block index is column-major: col*(k/128)+block";
        ];
        "scale_decode_policy",
        `List [
          `String "binary16 zero, signed zero, subnormal, normal, and max-finite scales are accepted";
          `String "binary16 NaN and infinity scales are rejected before output writeback";
          `String "accepted scale encodings are widened to exact finite binary64 bit patterns";
          `String "negative weights are formed by sign-bit negation of the widened scale";
        ];
        "lhs_policy",
        `List [
          `String "lhs cells are read as binary64 bit patterns";
          `String "every lhs cell in the m*k span must exist and be finite";
          `String "lhs span is snapshotted before any output writeback";
          `String "exact and partial lhs/output overlap use the lhs snapshot";
        ];
        "arithmetic_policy",
        `List [
          `String "accumulator starts as positive zero binary64";
          `String "for each lane product = lhs[row,k_index] * signed_scale";
          `String "product uses deterministic finite binary64 multiplication with round-to-nearest-ties-to-even";
          `String "accumulator update uses deterministic finite binary64 addition with round-to-nearest-ties-to-even";
          `String "subnormal results are preserved by gradual underflow";
          `String "multiplication by signed zero preserves the XOR-derived result sign";
          `String "addition of exact nonzero cancellation returns positive zero";
          `String "any non-finite or unsupported product/add result rejects the whole opcode";
        ];
        "iteration_order",
        `List [
          `String "row ascending";
          `String "column ascending";
          `String "block ascending";
          `String "lane ascending";
        ];
        "read_write_policy",
        `List [
          `String "decode all Q1 scale bits before output writeback";
          `String "compute the complete m*n output buffer before writing any destination cell";
          `String "write output only after full output buffer succeeds";
          `String "preserve output on validation or arithmetic rejection";
        ];
        "output_policy",
        `List [
          `String "outputs are stored as binary64 bit patterns in row-major m*n order";
          `String "the opcode writes output memory only; it does not mutate session ABI registers";
          `String "enclosing session ABI r0 declares the output base cell";
          `String "enclosing session ABI r1 declares the output VM cell count";
        ];
        "effort_policy",
        `List [
          `String "opcode base effort is 200";
          `String "dynamic effort is floor(m*n*k/512)";
          `String "program effort also includes surrounding VM instructions such as STOP";
        ];
      ])
  | "RMSNORM_FP_EPS" ->
    Some
      (`Assoc [
        "schema", `String "octra.inference.vm-semantics.v1";
        "opcode", `String opcode;
        "bytecode", `String "0x8d";
        "signature", `String "RMSNORM_FP_EPS(addr, count, gamma, epsilon)";
        "register_roles",
        `List [
          `String "addr: in-place input/output base cell";
          `String "count: number of binary64 cells";
          `String "gamma: gamma scale base cell";
          `String "epsilon: finite positive binary64 bit pattern";
        ];
        "memory_units",
        `List [
          `String "input, output, and gamma use one binary64 bit pattern per VM cell";
          `String "the opcode mutates the addr span in place after complete validation";
        ];
        "shape_policy",
        `List [
          `String "count must be positive";
          `String "addr and gamma spans must be valid large VM memory spans";
          `String "addr and gamma spans must not overlap";
        ];
        "epsilon_policy",
        `List [
          `String "epsilon is read from its register as a binary64 bit pattern";
          `String "epsilon must be finite and greater than positive zero";
        ];
        "arithmetic_policy",
        `List [
          `String "read the full input and gamma spans before writeback";
          `String "sum_sq starts as positive zero binary64";
          `String "for index ascending: sum_sq += input[index] * input[index]";
          `String "mean_sq = sum_sq / count using deterministic finite binary64 division";
          `String "inverse_input = mean_sq + epsilon";
          `String "inv_rms = deterministic finite binary64 inverse_sqrt(inverse_input)";
          `String "output[index] = (input[index] * inv_rms) * gamma[index]";
          `String "all multiply, add, divide, sqrt, and output multiply steps use the deterministic finite binary64 core";
        ];
        "read_write_policy",
        `List [
          `String "compute every output cell before mutating addr";
          `String "reject before writeback on missing cells, non-finite cells, invalid epsilon, arithmetic failure, invalid span, or effort exhaustion";
        ];
        "output_policy",
        `List [
          `String "outputs are stored as binary64 bit patterns in addr order";
          `String "the opcode does not mutate session ABI registers";
        ];
        "effort_policy",
        `List [
          `String "opcode base effort is 50";
          `String "dynamic effort is count * 4";
          `String "program effort also includes surrounding VM instructions such as STOP";
        ];
      ])
  | "L2NORM_FP" ->
    Some
      (`Assoc [
        "schema", `String "octra.inference.vm-semantics.v1";
        "opcode", `String opcode;
        "bytecode", `String "0x8e";
        "signature", `String "L2NORM_FP(addr, count, epsilon)";
        "register_roles",
        `List [
          `String "addr: in-place input/output base cell";
          `String "count: number of binary64 cells";
          `String "epsilon: finite positive binary64 bit pattern";
        ];
        "memory_units",
        `List [
          `String "input and output use one binary64 bit pattern per VM cell";
          `String "the opcode mutates the addr span in place after complete validation";
        ];
        "shape_policy",
        `List [
          `String "count must be positive";
          `String "addr span must be a valid large VM memory span";
        ];
        "epsilon_policy",
        `List [
          `String "epsilon is read from its register as a binary64 bit pattern";
          `String "epsilon must be finite and greater than positive zero";
        ];
        "arithmetic_policy",
        `List [
          `String "read the full input span before writeback";
          `String "sum_sq starts as positive zero binary64";
          `String "for index ascending: sum_sq += input[index] * input[index]";
          `String "inverse_input = sum_sq + epsilon";
          `String "inv_norm = deterministic finite binary64 inverse_sqrt(inverse_input)";
          `String "output[index] = input[index] * inv_norm";
          `String "all multiply, add, sqrt, and output multiply steps use the deterministic finite binary64 core";
        ];
        "read_write_policy",
        `List [
          `String "compute every output cell before mutating addr";
          `String "reject before writeback on missing cells, non-finite cells, invalid epsilon, arithmetic failure, invalid span, or effort exhaustion";
        ];
        "output_policy",
        `List [
          `String "outputs are stored as binary64 bit patterns in addr order";
          `String "the opcode does not mutate session ABI registers";
        ];
        "effort_policy",
        `List [
          `String "opcode base effort is 40";
          `String "dynamic effort is count * 3";
          `String "program effort also includes surrounding VM instructions such as STOP";
        ];
      ])
  | "SOFTMAX_FP" ->
    Some
      (`Assoc [
        "schema", `String "octra.inference.vm-semantics.v1";
        "opcode", `String opcode;
        "bytecode", `String "0x94";
        "signature", `String "SOFTMAX_FP(dest, scores, count)";
        "register_roles",
        `List [
          `String "dest: output probability base cell";
          `String "scores: input score base cell";
          `String "count: number of binary64 score cells";
        ];
        "memory_units",
        `List [
          `String "scores and dest use one binary64 bit pattern per VM cell";
          `String "dest may equal scores for exact in-place execution";
        ];
        "shape_policy",
        `List [
          `String "count must be positive and at most 8192";
          `String "dest and scores spans must be valid large VM memory spans";
          `String "dest/scores overlap is rejected unless the spans are exactly the same range";
        ];
        "arithmetic_policy",
        `List [
          `String "read the full score span before writeback";
          `String "max_score starts at scores[0]";
          `String "for index ascending: replace max_score only when compare(scores[index], max_score) is greater than zero";
          `String "shifted[index] = scores[index] - max_score using deterministic finite binary64 subtraction";
          `String "every shifted score must compare less than or equal to positive zero";
          `String "exp(shifted[index]) uses protocol-owned deterministic nonpositive binary64 exp";
          `String "protocol exp uses Q256 range reduction, round-to-nearest ln(2), 80 Taylor terms, and deterministic ties-to-even binary64 composition";
          `String "sum_exp starts as positive zero binary64 and accumulates exps in index order";
          `String "output[index] = exp[index] / sum_exp using deterministic finite binary64 division";
        ];
        "read_write_policy",
        `List [
          `String "compute every output probability before mutating dest";
          `String "reject before writeback on missing scores, non-finite values, invalid span, zero/nonpositive sum, arithmetic failure, or effort exhaustion";
        ];
        "output_policy",
        `List [
          `String "outputs are stored as binary64 bit patterns in dest order";
          `String "the opcode does not mutate session ABI registers";
        ];
        "consensus_note",
        `String "protocol-owned nonpositive exp is deterministic but remains a consensus candidate until P0 vectors and cross-platform conformance are accepted";
        "effort_policy",
        `List [
          `String "opcode base effort is 100";
          `String "dynamic effort is count * 8";
          `String "program effort also includes surrounding VM instructions such as STOP";
        ];
      ])
  | "GATED_DELTA_RULE_FP" ->
    Some
      (`Assoc [
        "schema", `String "octra.inference.vm-semantics.v1";
        "opcode", `String opcode;
        "bytecode", `String "0x8e";
        "signature",
        `String
          "GATED_DELTA_RULE_FP(output, state_dst, q, k, v, log_decay, beta, state, t, q_heads, k_heads, v_heads, key_dim, value_dim)";
        "register_roles",
        `List [
          `String "output: recurrent output base cell";
          `String "state_dst: next recurrent state base cell";
          `String "q/k/v: query, key, and value input base cells";
          `String "log_decay: per timestep/value-head log decay base cell";
          `String "beta: per timestep/value-head beta base cell";
          `String "state: previous recurrent state base cell";
          `String "t: timestep count";
          `String "q_heads/k_heads/v_heads: head counts";
          `String "key_dim/value_dim: per-head dimensions";
        ];
        "memory_units",
        `List [
          `String "all tensor and state spans use one binary64 bit pattern per VM cell";
          `String "state layout is v_heads * value_dim * key_dim in head-major, row-major order";
          `String "output layout is timesteps * v_heads * value_dim in timestep-major order";
        ];
        "shape_policy",
        `List [
          `String "timesteps, q_heads, k_heads, v_heads, key_dim, and value_dim must all be positive";
          `String "q span cells = timesteps * q_heads * key_dim";
          `String "k span cells = timesteps * k_heads * key_dim";
          `String "v span cells = timesteps * v_heads * value_dim";
          `String "log_decay and beta span cells = timesteps * v_heads";
          `String "state cells = v_heads * value_dim * key_dim";
          `String "output cells = timesteps * v_heads * value_dim";
          `String "all spans must be valid large VM memory spans";
        ];
        "aliasing_policy",
        `List [
          `String "output and state_dst must not overlap";
          `String "input spans q, k, v, log_decay, and beta must not overlap output or state_dst";
          `String "state may equal state_dst exactly for in-place state update";
          `String "state must not partially overlap state_dst and must not overlap output";
        ];
        "arithmetic_policy",
        `List [
          `String "read all input and state spans before writeback";
          `String "state is copied before recurrence mutation";
          `String "scale = deterministic finite binary64 inverse_sqrt(key_dim)";
          `String "for timestep ascending and value-head ascending: q_head = head mod q_heads, k_head = head mod k_heads";
          `String "decay = exp_nonpositive(log_decay[timestep, head]) using protocol-owned Q256 range reduction, ln(2), 80 fixed Taylor terms, and deterministic binary64 composition";
          `String "state[head,:,:] *= decay in row-major state order";
          `String "memory[row] = sum_col state[row,col] * k[col] in col ascending order";
          `String "delta[row] = (v[row] - memory[row]) * beta[timestep, head]";
          `String "state[row,col] += k[col] * delta[row] in row-major order";
          `String "output[row] = sum_col state[row,col] * q[col] * scale in col ascending order";
          `String "all multiply, add, subtract, divide, sqrt, and output-scaling steps use the deterministic finite binary64 core";
        ];
        "read_write_policy",
        `List [
          `String "compute complete output and next-state buffers before mutating either destination";
          `String "reject before writeback on invalid spans, invalid overlap, missing cells, non-finite cells, positive/non-finite log_decay, arithmetic failure, product overflow, or effort exhaustion";
        ];
        "output_policy",
        `List [
          `String "output cells are written first in output layout order";
          `String "state_dst cells are written second in state layout order";
          `String "the opcode does not mutate session ABI registers";
        ];
        "consensus_note",
        `String "protocol-owned nonpositive exp is deterministic but remains a consensus candidate until P0 vectors and cross-platform conformance are accepted";
        "effort_policy",
        `List [
          `String "opcode base effort is 200";
          `String "dynamic effort is 4 * timesteps * v_heads * value_dim * key_dim + 2 * timesteps * v_heads * value_dim + timesteps * v_heads";
          `String "program effort also includes surrounding VM instructions such as STOP";
        ];
      ])
  | _ -> None

let vm_semantics_root_for_opcode ~opcode =
  match vm_semantics_contract_json ~opcode with
  | None -> None
  | Some json ->
    let payload = Yojson.Safe.to_string json in
    Some
      (sha256 ("octra:inference:vm-semantics\000" ^ payload))

let vm_semantics_binding_json ~opcode ~vm_semantics_root =
  match vm_semantics_root_for_opcode ~opcode with
  | Some root when String.equal root vm_semantics_root ->
    `Assoc [
      "status", `String "matched";
      "classification", `String "none";
      "vm_semantics_root", `String vm_semantics_root;
      "litenode_vm_semantics_root", `String root;
    ]
  | Some root ->
    `Assoc [
      "status", `String "unbound";
      "classification", `String "vm_semantics_root_mismatch";
      "vm_semantics_root", `String vm_semantics_root;
      "litenode_vm_semantics_root", `String root;
    ]
  | None ->
    `Assoc [
      "status", `String "unavailable";
      "classification", `String "vm_semantics_root_unavailable";
      "vm_semantics_root", `String vm_semantics_root;
      "litenode_vm_semantics_root", `Null;
    ]

let assoc name = function
  | `Assoc fields -> Ok fields
  | _ -> Error (Json_error ("expected object: " ^ name))

let field name fields =
  match List.filter (fun (key, _) -> String.equal key name) fields with
  | [(_, value)] -> Ok value
  | [] -> Error (Json_error ("missing field: " ^ name))
  | _ -> Error (Json_error ("duplicate field: " ^ name))

let optional_field name fields =
  match List.filter (fun (key, _) -> String.equal key name) fields with
  | [] -> Ok None
  | [(_, value)] -> Ok (Some value)
  | _ -> Error (Json_error ("duplicate field: " ^ name))

let check_known fields names =
  match
    List.find_opt
      (fun (name, _) -> not (List.exists (String.equal name) names))
      fields
  with
  | None -> Ok ()
  | Some (name, _) -> Error (Json_error ("unknown field: " ^ name))

let string_field name fields =
  match field name fields with
  | Ok (`String value) -> Ok value
  | Ok _ -> Error (Json_error ("field must be a string: " ^ name))
  | Error error -> Error error

let optional_string_field name fields =
  match optional_field name fields with
  | Ok None -> Ok None
  | Ok (Some `Null) -> Ok None
  | Ok (Some (`String value)) -> Ok (Some value)
  | Ok (Some _) -> Error (Json_error ("field must be a string: " ^ name))
  | Error error -> Error error

let int_field name fields =
  match field name fields with
  | Ok (`Int value) -> Ok value
  | Ok _ -> Error (Json_error ("field must be an int: " ^ name))
  | Error error -> Error error

let optional_int_field name fields =
  match optional_field name fields with
  | Ok None -> Ok None
  | Ok (Some `Null) -> Ok None
  | Ok (Some (`Int value)) -> Ok (Some value)
  | Ok (Some _) -> Error (Json_error ("field must be an int: " ^ name))
  | Error error -> Error error

let list_field name fields =
  match field name fields with
  | Ok (`List values) -> Ok values
  | Ok _ -> Error (Json_error ("field must be a list: " ^ name))
  | Error error -> Error error

let assoc_field name fields =
  match field name fields with
  | Ok (`Assoc fields) -> Ok fields
  | Ok _ -> Error (Json_error ("field must be an object: " ^ name))
  | Error error -> Error error

let opt_string_field name fields =
  match field name fields with
  | Ok (`String value) -> Some value
  | _ -> None

let opt_int_json_field name fields =
  match field name fields with
  | Ok (`Int value) -> Some value
  | Ok (`Intlit value) ->
    (try Some (int_of_string value) with Failure _ -> None)
  | _ -> None

let opt_assoc_field name fields =
  match field name fields with
  | Ok (`Assoc values) -> Some values
  | _ -> None

let string_list_field name fields =
  let* values = list_field name fields in
  let rec loop acc = function
    | [] -> Ok (List.rev acc)
    | `String value :: rest -> loop (value :: acc) rest
    | _ :: _ -> Error (Json_error ("field must be a string list: " ^ name))
  in
  loop [] values

let relative_path_ok path =
  String.length path > 0
  && path.[0] <> '/'
  && not (String.contains path '\\')
  && List.for_all
       (fun part -> part <> "" && part <> "." && part <> "..")
       (String.split_on_char '/' path)

let optional_exists predicate = function
  | None -> false
  | Some value -> predicate value

let hex_char = function
  | '0' .. '9'
  | 'a' .. 'f'
  | 'A' .. 'F' -> true
  | _ -> false

let root_ok value =
  String.length value = 64
  && String.for_all hex_char value

let json_string_opt = function
  | Some value -> `String value
  | None -> `Null

let json_int_opt = function
  | Some value -> `Int value
  | None -> `Null

let reg_name index = "r" ^ string_of_int index

let abi_declaration_binding_json = function
  | `Assoc fields ->
    (match opt_assoc_field "abi" fields, opt_assoc_field "output" fields with
     | Some abi, Some output ->
       let output_registers = opt_assoc_field "abi_registers" output in
       let entrypoint = opt_string_field "entrypoint" abi in
       let label = opt_int_json_field "label" abi in
       let output_base_register =
         opt_string_field "output_base_register" abi
       in
       let output_count_register =
         opt_string_field "output_count_register" abi
       in
       let output_count_unit = opt_string_field "output_count_unit" abi in
       let request_input_root_cell =
         opt_int_json_field "request_input_root_cell" abi
       in
       let session_abi_root = opt_string_field "session_abi_root" abi in
       let matched_session_abi_root =
         match session_abi_root with
         | Some root when Inference_session_abi.supported_root root -> Some root
         | _ -> None
       in
       let output_base = opt_int_json_field "base_address" output in
       let output_count = opt_int_json_field "length_f64_cells" output in
       let r0 =
         match output_registers with
         | Some registers -> opt_int_json_field "r0" registers
         | None -> None
       in
       let r1 =
         match output_registers with
         | Some registers -> opt_int_json_field "r1" registers
         | None -> None
       in
       let add_if condition blocker blockers =
         if condition then blocker :: blockers else blockers
       in
       let blockers =
         []
         |> add_if
              (match entrypoint with
               | Some value ->
                 not
                   (String.equal
                      value
                      Inference_session_abi.advance_entrypoint)
               | None -> true)
              "entrypoint_mismatch"
         |> add_if
              (match label with
               | Some value -> value <> Inference_session_abi.advance_label
               | None -> true)
              "entry_label_mismatch"
         |> add_if
              (match output_base_register with
               | Some value ->
                 not
                   (String.equal
                      value
                      (reg_name Inference_session_abi.output_base_register))
               | None -> true)
              "output_base_register_mismatch"
         |> add_if
              (match output_count_register with
               | Some value ->
                 not
                   (String.equal
                      value
                      (reg_name Inference_session_abi.output_count_register))
               | None -> true)
              "output_count_register_mismatch"
         |> add_if
              (match output_count_unit with
               | Some value -> not (String.equal value "cells")
               | None -> true)
              "output_count_unit_mismatch"
         |> add_if
              (match session_abi_root with
               | Some value ->
                 not (Inference_session_abi.supported_root value)
               | None -> true)
              "session_abi_root_mismatch"
         |> add_if
              (match request_input_root_cell with
               | Some value -> value <> Inference_session_abi.input_root_cell
               | None -> true)
              "request_input_root_cell_mismatch"
         |> add_if
              (match r0, output_base with
               | Some actual, Some expected -> actual <> expected
               | _ -> true)
              "r0_output_base_mismatch"
         |> add_if
              (match r1, output_count with
               | Some actual, Some expected -> actual <> expected
               | _ -> true)
              "r1_output_count_mismatch"
       in
       `Assoc [
         "status", `String (if blockers = [] then "matched" else "unbound");
         "classification",
         `String
           (if blockers = [] then "none" else "abi_declaration_mismatch");
         "evidence_scope", `String "template_declaration";
         "session_abi_root", json_string_opt session_abi_root;
         "litenode_session_abi_root", `String Inference_session_abi.v1_root;
         "litenode_matched_session_abi_root",
         json_string_opt matched_session_abi_root;
         "litenode_supported_session_abi_roots",
         `List
           (List.map
              (fun root -> `String root)
              Inference_session_abi.supported_roots);
         "entrypoint", json_string_opt entrypoint;
         "label", json_int_opt label;
         "output_base_register", json_string_opt output_base_register;
         "output_count_register", json_string_opt output_count_register;
         "output_count_unit", json_string_opt output_count_unit;
         "request_input_root_cell", json_int_opt request_input_root_cell;
         "output_base_address", json_int_opt output_base;
         "output_count", json_int_opt output_count;
         "r0", json_int_opt r0;
         "r1", json_int_opt r1;
         "blockers",
         `List (List.map (fun blocker -> `String blocker) blockers);
       ]
     | _ ->
       `Assoc [
         "status", `String "unavailable";
         "classification", `String "abi_declaration_unavailable";
         "evidence_scope", `String "template_declaration";
         "session_abi_root", `Null;
         "litenode_session_abi_root", `String Inference_session_abi.v1_root;
         "litenode_matched_session_abi_root", `Null;
         "litenode_supported_session_abi_roots",
         `List
           (List.map
              (fun root -> `String root)
              Inference_session_abi.supported_roots);
         "blockers", `List [`String "missing_abi_or_output"];
       ])
  | _ ->
    `Assoc [
      "status", `String "unavailable";
      "classification", `String "abi_declaration_unavailable";
      "evidence_scope", `String "template_declaration";
      "session_abi_root", `Null;
      "litenode_session_abi_root", `String Inference_session_abi.v1_root;
      "litenode_matched_session_abi_root", `Null;
      "litenode_supported_session_abi_roots",
      `List
        (List.map
           (fun root -> `String root)
           Inference_session_abi.supported_roots);
      "blockers", `List [`String "template_not_object"];
    ]

let require_string expected name fields =
  let* actual = string_field name fields in
  if String.equal actual expected then Ok ()
  else
    Error
      (Template_error
         (Printf.sprintf
            "field %s expected %s actual %s"
            name
            expected
            actual))

let rec parse_list parse acc = function
  | [] -> Ok (List.rev acc)
  | value :: rest ->
    let* parsed = parse value in
    parse_list parse (parsed :: acc) rest

let parse_access = function
  | "read" -> Ok Read
  | "write" -> Ok Write
  | "read_write" -> Ok Read_write
  | value -> Error (Template_error ("unknown memory access: " ^ value))

let access_string = function
  | Read -> "read"
  | Write -> "write"
  | Read_write -> "read_write"

let parse_register value =
  let* fields = assoc "register binding" value in
  let* () = check_known fields ["register"; "name"; "kind"; "value"] in
  let* register = int_field "register" fields in
  let* name = string_field "name" fields in
  let* kind = string_field "kind" fields in
  let* value = string_field "value" fields in
  if register < 0 || register > 63 then
    Error (Template_error ("register out of range: " ^ string_of_int register))
  else Ok { register; name; kind; value }

let parse_memory value =
  let* fields = assoc "memory binding" value in
  let* () =
    check_known
      fields
      [
        "name"; "path"; "root"; "byte_length"; "sha256"; "encoding";
        "base"; "cells"; "access";
      ]
  in
  let* name = string_field "name" fields in
  let* path = optional_string_field "path" fields in
  let* root = string_field "root" fields in
  let* byte_length = optional_int_field "byte_length" fields in
  let* sha256 = optional_string_field "sha256" fields in
  let* encoding = string_field "encoding" fields in
  let* base = int_field "base" fields in
  let* cells = int_field "cells" fields in
  let* access_name = string_field "access" fields in
  let* access = parse_access access_name in
  if optional_exists (fun path -> not (relative_path_ok path)) path then
    Error (Template_error ("memory path must be relative: " ^ name))
  else if
    (access = Read || access = Read_write)
    && (path = None || byte_length = None || sha256 = None)
  then
    Error
      (Template_error
         ("readable memory requires path, byte_length, and sha256: " ^ name))
  else if optional_exists (fun length -> length <= 0) byte_length then
    Error (Template_error ("byte_length must be positive: " ^ name))
  else if base < 0 then
    Error (Template_error ("memory base must be non-negative: " ^ name))
  else if cells <= 0 then
    Error (Template_error ("memory cells must be positive: " ^ name))
  else Ok { name; path; root; byte_length; sha256; encoding; base; cells; access }

let parse_expected_span value =
  let* fields = assoc "expected span" value in
  let* () =
    check_known
      fields
      ["name"; "base"; "cells"; "output_root"; "output_sha256"]
  in
  let* span_name = string_field "name" fields in
  let* base = int_field "base" fields in
  let* cells = int_field "cells" fields in
  let* output_root = optional_string_field "output_root" fields in
  let* output_sha256 = optional_string_field "output_sha256" fields in
  if base < 0 then
    Error (Template_error ("expected span base must be non-negative: " ^ span_name))
  else if cells <= 0 then
    Error (Template_error ("expected span cells must be positive: " ^ span_name))
  else if output_root = None && output_sha256 = None then
    Error
      (Template_error
         ("expected span output_root or output_sha256 is required: " ^ span_name))
  else Ok { span_name; base; cells; output_root; output_sha256 }

let parse_expected fields =
  let* fields = assoc_field "expected" fields in
  let* () = check_known fields ["spans"] in
  let* values = list_field "spans" fields in
  parse_list parse_expected_span [] values

let parse_failure_case value =
  let* fields = assoc "failure case" value in
  let* () =
    check_known fields ["case"; "expected"; "mutations"; "unchanged_spans"]
  in
  let* case_name = string_field "case" fields in
  let* expected = string_field "expected" fields in
  let* mutations = string_list_field "mutations" fields in
  let* unchanged_spans = string_list_field "unchanged_spans" fields in
  if mutations = [] then
    Error (Template_error ("failure case mutations are required: " ^ case_name))
  else if unchanged_spans = [] then
    Error
      (Template_error
         ("failure case unchanged_spans are required: " ^ case_name))
  else Ok { case_name; expected; mutations; unchanged_spans }

let requirement_for_opcode opcode =
  List.find_opt (fun item -> String.equal item.opcode opcode) requirements

let check_primitive requirement primitive =
  if List.exists (String.equal primitive) requirement.primitives then Ok ()
  else
    Error
      (Template_error
         (Printf.sprintf
            "primitive %s does not match opcode %s"
            primitive
            requirement.opcode))

let missing_effects required actual =
  List.filter
    (fun effect -> not (List.exists (String.equal effect) actual))
    required

let has_writable_memory memory =
  List.exists
    (fun item ->
       match item.access with
       | Write
       | Read_write -> true
       | Read -> false)
    memory

let duplicate_by key values =
  let seen = Hashtbl.create 16 in
  List.find_opt
    (fun value ->
       let key = key value in
       if Hashtbl.mem seen key then true
       else begin
         Hashtbl.add seen key ();
         false
       end)
    values

let validate (template : t) =
  match requirement_for_opcode template.opcode with
  | None -> Error (Template_error ("unsupported P0 opcode: " ^ template.opcode))
  | Some requirement ->
    let* () = check_primitive requirement template.primitive in
    let* _profile =
      match
        Inference_numerical_profile.validate_for_opcode
          ~opcode:template.opcode
          ~profile:template.profile
      with
      | Ok profile -> Ok profile
      | Error error ->
        Error
          (Template_error
             (Inference_numerical_profile.error_message error))
    in
    let missing = missing_effects requirement.effects template.effects in
    if missing <> [] then
      Error
        (Template_error
           ("missing required effects: " ^ String.concat "," missing))
    else if not (root_ok template.vm_semantics_root) then
      Error (Template_error "vm_semantics_root must be a 32-byte hex root")
    else if not (root_ok template.numerical_profile_root) then
      Error
        (Template_error "numerical_profile_root must be a 32-byte hex root")
    else if template.registers = [] then
      Error (Template_error "register bindings are required")
    else if template.memory = [] then
      Error (Template_error "memory bindings are required")
    else if not (has_writable_memory template.memory) then
      Error (Template_error "at least one writable memory binding is required")
    else if template.expected = [] then
      Error (Template_error "expected spans are required")
    else if
      String.equal template.opcode "GATED_DELTA_RULE_FP"
      && List.length template.expected < 2
    then
      Error
        (Template_error
           "GATED_DELTA_RULE_FP requires output and next-state expected spans")
    else if template.failure_cases = [] then
      Error (Template_error "failure cases are required")
    else
      match
        duplicate_by (fun (item : register_binding) -> item.register)
          template.registers
      with
      | Some item ->
        Error
          (Template_error
             ("duplicate register binding: " ^ string_of_int item.register))
      | None ->
        (match duplicate_by (fun (item : memory_binding) -> item.name) template.memory with
         | Some item ->
           Error (Template_error ("duplicate memory binding: " ^ item.name))
         | None ->
           (match duplicate_by (fun (item : expected_span) -> item.span_name)
                    template.expected with
            | Some item ->
              Error
                (Template_error
                   ("duplicate expected span: " ^ item.span_name))
            | None -> Ok template))

let of_json json =
  let* fields = assoc "conformance template" json in
  let* () =
    check_known
      fields
      [
        "type"; "schema"; "opcode"; "primitive"; "profile";
        "vm_semantics_root"; "numerical_profile_root"; "expected_effort";
        "effects"; "registers"; "memory"; "expected"; "failure_cases";
      ]
  in
  let* () =
    require_string "litenode_vm_conformance_template" "type" fields
  in
  let* schema = int_field "schema" fields in
  let* opcode = string_field "opcode" fields in
  let* primitive = string_field "primitive" fields in
  let* profile = string_field "profile" fields in
  let* vm_semantics_root = string_field "vm_semantics_root" fields in
  let* numerical_profile_root = string_field "numerical_profile_root" fields in
  let* expected_effort = int_field "expected_effort" fields in
  let* effects = string_list_field "effects" fields in
  let* register_values = list_field "registers" fields in
  let* memory_values = list_field "memory" fields in
  let* expected = parse_expected fields in
  let* failure_values = list_field "failure_cases" fields in
  if schema <> 1 then
    Error
      (Template_error ("unsupported schema: " ^ string_of_int schema))
  else if expected_effort < 0 then
    Error (Template_error "expected_effort must be non-negative")
  else
    let* registers = parse_list parse_register [] register_values in
    let* memory = parse_list parse_memory [] memory_values in
    let* failure_cases = parse_list parse_failure_case [] failure_values in
    validate {
      opcode;
      primitive;
      profile;
      vm_semantics_root;
      numerical_profile_root;
      expected_effort;
      effects;
      registers;
      memory;
      expected;
      failure_cases;
    }

let register_json (item : register_binding) =
  `Assoc [
    "register", `Int item.register;
    "name", `String item.name;
    "kind", `String item.kind;
    "value", `String item.value;
  ]

let optional_string_json = function
  | None -> `Null
  | Some value -> `String value

let memory_json (item : memory_binding) =
  `Assoc [
    "name", `String item.name;
    "path", optional_string_json item.path;
    "root", `String item.root;
    "byte_length",
    (match item.byte_length with None -> `Null | Some value -> `Int value);
    "sha256", optional_string_json item.sha256;
    "encoding", `String item.encoding;
    "base", `Int item.base;
    "cells", `Int item.cells;
    "access", `String (access_string item.access);
  ]

let expected_span_json (item : expected_span) =
  `Assoc [
    "name", `String item.span_name;
    "base", `Int item.base;
    "cells", `Int item.cells;
    "output_root", optional_string_json item.output_root;
    "output_sha256", optional_string_json item.output_sha256;
  ]

let failure_json (item : failure_case) =
  `Assoc [
    "case", `String item.case_name;
    "expected", `String item.expected;
    "mutations", `List (List.map (fun value -> `String value) item.mutations);
    "unchanged_spans",
    `List (List.map (fun value -> `String value) item.unchanged_spans);
  ]

let profile_json (template : t) =
  match
    Inference_numerical_profile.validate_for_opcode
      ~opcode:template.opcode
      ~profile:template.profile
  with
  | Ok profile ->
    Inference_numerical_profile.to_json_for_opcode
      ~opcode:template.opcode
      profile
  | Error error ->
    `Assoc [
      "name", `String template.profile;
      "consensus_status", `String "invalid";
      "error", `String (Inference_numerical_profile.error_message error);
    ]

let to_json (template : t) =
  let profile = profile_json template in
  let consensus_status =
    match profile with
    | `Assoc fields ->
      (match List.assoc_opt "consensus_status" fields with
       | Some (`String status) -> status
       | _ -> "unknown")
    | _ -> "unknown"
  in
  `Assoc [
    "status", `String "accepted";
    "diagnostic_only", `Bool true;
    "opcode", `String template.opcode;
    "primitive", `String template.primitive;
    "profile", `String template.profile;
    "profile_gate", profile;
    "consensus_status", `String consensus_status;
    "vm_semantics_root", `String template.vm_semantics_root;
    "vm_semantics_binding",
    vm_semantics_binding_json
      ~opcode:template.opcode
      ~vm_semantics_root:template.vm_semantics_root;
    "numerical_profile_root", `String template.numerical_profile_root;
    "profile_root_binding",
    Inference_numerical_profile.root_binding_json
      ~numerical_profile_root:template.numerical_profile_root
      profile;
    "expected_effort", `Int template.expected_effort;
    "effects", `List (List.map (fun value -> `String value) template.effects);
    "register_count", `Int (List.length template.registers);
    "memory_binding_count", `Int (List.length template.memory);
    "expected_span_count", `Int (List.length template.expected);
    "failure_case_count", `Int (List.length template.failure_cases);
    "registers", `List (List.map register_json template.registers);
    "memory", `List (List.map memory_json template.memory);
    "expected", `List (List.map expected_span_json template.expected);
    "failure_cases", `List (List.map failure_json template.failure_cases);
  ]
