(* Ported Bonsai session-prefill harness for the LiteNode octra_vm. *)
[@@@warning "-32-27"]

module VM = Octra_vm.Contract_vm
module Bytecode = Octra_vm.Bytecode
module Contract = Octra_vm.Contract
module Oct_compile = Octra_vm.Oct_compile
module Json = Yojson.Safe
module Json_util = Yojson.Safe.Util

type q1_vector = {
  source_sha256 : string option;
  tensor_name : string;
  tensor_dimensions : int list;
  tensor_payload_offset : int;
  q1_b64 : string;
  q1_sha256 : string;
  expected_sha256 : string;
  lhs : float array;
  expected : float array;
  dst_addr : int;
  lhs_addr : int;
  offset : int;
  m : int;
  k : int;
  n : int;
}

type projection_chunk = {
  chunk_index : int;
  row_start : int;
  chunk_n : int;
  chunk_q1_b64 : string;
  chunk_q1_sha256 : string;
  chunk_expected_sha256 : string;
  chunk_expected : float array;
  chunk_dst_addr : int;
  chunk_lhs_addr : int;
  chunk_offset : int;
  chunk_m : int;
  chunk_k : int;
}

type projection_bundle = {
  bundle_artifact : string;
  bundle_primitive : string;
  bundle_runtime_profile : string;
  bundle_plan_sha256 : string option;
  bundle_source_sha256 : string option;
  bundle_tensor_name : string;
  bundle_tensor_dimensions : int list;
  bundle_tensor_payload_offset : int;
  bundle_tensor_payload_bytes : int;
  bundle_k : int;
  bundle_output_rows : int;
  bundle_row_bytes : int;
  bundle_chunk_count : int;
  bundle_max_chunk_encoded_bytes : int;
  bundle_model_store_max_stored_value_bytes : int;
  bundle_lhs : float array;
  bundle_chunks : projection_chunk list;
  bundle_expected_output_sha256 : string;
}

type plan_binding = {
  role : string;
  name : string;
  present : bool;
  dimensions : int list;
  payload_offset : int option;
  encoded_bytes : int option;
}

type plan_layer = {
  layer_index : int;
  layer_kind : string;
  layer_tensors : plan_binding list;
}

type execution_plan = {
  status : string;
  architecture : string;
  model_name : string option;
  source_path : string option;
  source_sha256 : string option;
  rope_dimension_count : int;
  rope_dimension_sections : int array;
  rope_freq_base : float;
  missing_required_tensors : string list;
  layer0_kind : string;
  layer0_qkv : plan_binding;
  layer0_tensors : plan_binding list;
  layers : plan_layer list;
  final_tensors : plan_binding list;
}

type packed_tile = {
  packed_tile_id : int;
  packed_tile_tensor : string;
  packed_tile_offset : int;
  packed_tile_bytes : int;
  packed_tile_hash : string;
}

type packed_model = {
  packed_root : string;
  packed_source_sha256 : string;
  packed_model_root : string;
  packed_tensor_bytes : (string, int) Hashtbl.t;
  packed_tensor_alias : (string, string * int) Hashtbl.t;
  packed_tensor_dimensions : (string, int list) Hashtbl.t;
  packed_tiles : (string, packed_tile list) Hashtbl.t;
  packed_verified_tiles : (int, unit) Hashtbl.t;
  packed_execution_root : string;
  packed_execution_descriptor : Json.t;
}

let active_packed_model : packed_model option ref = ref None

let profile_prefill_ms = ref 0
let profile_generation_ms = ref 0
let profile_generated_forward_ms = ref 0
let profile_lm_head_ms = ref 0
let profile_forward_ms = ref 0
let profile_gather_ms = ref 0
let profile_attention_norm_ms = ref 0
let profile_recurrent_layer_ms = ref 0
let profile_full_attention_layer_ms = ref 0
let profile_mixer_residual_ms = ref 0
let profile_ffn_layer_ms = ref 0
let profile_q1_projection_ms = ref 0
let profile_q1_span_read_ms = ref 0
let profile_q1_vm_run_ms = ref 0
let profile_q1_projection_count = ref 0
let profile_q1_projection_chunks = ref 0
let profile_q1_physical_bundle_count = ref 0
let profile_q1_dynamic_bundle_count = ref 0
let profile_vm_program_count = ref 0
let profile_vm_program_elapsed_ms = ref 0
let profile_load_f32_elapsed_ms = ref 0
let profile_token_elapsed_ms = ref []

type activation_fixture = {
  activation_sha256 : string;
  token_id : int;
  token_position : int;
  activation : float array;
}

type single_token_fixture = {
  single_token_evaluated_token_id : int;
  single_token_generated_token_id : int;
  single_token_final_hidden_sha256 : string;
  single_token_final_norm_sha256 : string;
  single_token_logits_sha256 : string;
  single_token_layer_outputs : float array array;
  single_token_final_hidden : float array;
  single_token_final_norm : float array;
  single_token_logits : float array;
}

type generation_step = {
  generation_step_index : int;
  generation_step_input_token_count : int;
  generation_step_generated_token_id : int;
  generation_step_final_hidden_sha256 : string;
  generation_step_final_norm_sha256 : string;
  generation_step_logits_sha256 : string;
}

type generation_fixture = {
  generation_source_sha256 : string option;
  generation_prompt_sha256 : string;
  generation_prompt_token_ids : int array;
  generation_generated_token_ids : int array;
  generation_generated_text : string;
  generation_steps : generation_step array;
}

type comparison_stats = {
  max_abs_delta : float;
  max_rel_delta : float;
  rms_delta : float;
  worst_index : int;
  actual_non_finite : int;
  expected_non_finite : int;
}

type wrapper_dimensions = {
  hidden_dim : int;
  qkv_elements : int;
  q_heads : int;
  k_heads : int;
  v_heads : int;
  ssm_state_size : int;
  ssm_inner_size : int;
  ssm_conv_kernel : int;
  q_offset : int;
  q_elements : int;
  k_offset : int;
  k_elements : int;
  v_offset : int;
  v_elements : int;
}

type wrapper_inputs = {
  qkv : float array;
  z_gate : float array;
  beta_logits : float array;
  alpha_logits : float array;
  dt_bias : float array;
  a_log : float array;
  conv_kernel : float array;
  ssm_norm : float array;
}

type wrapper_expected = {
  conv_output_sha256 : string;
  conv_state_sha256 : string;
  q_l2_sha256 : string;
  k_l2_sha256 : string;
  beta_sha256 : string;
  gate_sha256 : string;
  recurrent_output_sha256 : string;
  recurrent_state_sha256 : string;
  gated_norm_output_sha256 : string;
  ssm_out_lhs : float array;
}

type wrapper_fixture = {
  status : string;
  runtime_profile : string;
  dimensions : wrapper_dimensions;
  epsilon : float;
  qk_l2_epsilon : float;
  inputs : wrapper_inputs;
  expected : wrapper_expected;
}

type workspace_vector = {
  workspace_name : string;
  workspace_runtime_profile : string;
  workspace_shape : int list;
  workspace_values : float array;
  workspace_values_sha256 : string;
}

type wrapper_addresses = {
  qkv_addr : int;
  z_gate_addr : int;
  recurrent_output_addr : int;
  recurrent_state_addr : int;
  hidden_dim : int;
  ssm_inner_size : int;
}

type wrapper_run = {
  wrapper_state : VM.s;
  wrapper_addresses : wrapper_addresses;
  wrapper_elapsed_ms : int;
  wrapper_bytecode_size_total : int;
  wrapper_ssm_out_max_abs_delta : float;
  wrapper_gated_norm_sha256 : string;
}

type wrapper_frontier = {
  frontier_qkv_addr : int;
  frontier_z_gate_addr : int;
  frontier_beta_addr : int;
  frontier_alpha_addr : int;
  frontier_qkv_sha256 : string;
  frontier_z_gate_sha256 : string;
  frontier_beta_sha256 : string;
  frontier_alpha_sha256 : string;
}

type projection_run = {
  projection_elapsed_ms : int;
  projection_effort_delta : int;
  projection_bytecode_size : int;
  projection_output_sha256 : string;
  projection_reference_sha256 : string;
  projection_max_abs_delta : float;
  projection_chunk_count : int;
  projection_max_chunk_encoded_bytes : int;
}

let q1_group_size = 128
let q1_block_bytes = 18

let require condition message =
  if not condition then failwith message

let member_exn field json =
  match Json_util.member field json with
  | `Null -> failwith ("missing JSON field: " ^ field)
  | value -> value

let string_field field json =
  match member_exn field json with
  | `String value -> value
  | _ -> failwith ("JSON field is not a string: " ^ field)

let string_field_opt field json =
  match Json_util.member field json with
  | `Null -> None
  | `String value -> Some value
  | _ -> failwith ("JSON field is not a string: " ^ field)

let bool_field field json =
  match member_exn field json with
  | `Bool value -> value
  | _ -> failwith ("JSON field is not a bool: " ^ field)

let int_of_json field = function
  | `Int value -> value
  | `Intlit value | `String value -> int_of_string value
  | _ -> failwith ("JSON field is not an int: " ^ field)

let int_field field json =
  int_of_json field (member_exn field json)

let int_field_opt field json =
  match Json_util.member field json with
  | `Null -> None
  | value -> Some (int_of_json field value)

let z_of_json field = function
  | `Int value -> Z.of_int value
  | `Intlit value | `String value -> Z.of_string value
  | _ -> failwith ("JSON field is not an integer literal: " ^ field)

let f64_of_u64_bits bits =
  let two64 = Z.shift_left Z.one 64 in
  let sign_bit = Z.shift_left Z.one 63 in
  let signed = if Z.geq bits sign_bit then Z.sub bits two64 else bits in
  Int64.float_of_bits (Z.to_int64 signed)

let f64_bits_array field json =
  match member_exn field json with
  | `List values ->
      values
      |> List.map (fun value -> f64_of_u64_bits (z_of_json field value))
      |> Array.of_list
  | _ -> failwith ("JSON field is not an array: " ^ field)

let f64_bits_array_list field json =
  match member_exn field json with
  | `List rows ->
      rows
      |> List.map (function
        | `List values ->
            values
            |> List.map (fun value -> f64_of_u64_bits (z_of_json field value))
            |> Array.of_list
        | _ -> failwith ("JSON field row is not an array: " ^ field))
      |> Array.of_list
  | _ -> failwith ("JSON field is not an array of arrays: " ^ field)

let f64_bits_array_alias primary fallback json =
  match Json_util.member primary json with
  | `Null -> f64_bits_array fallback json
  | `List values ->
      values
      |> List.map (fun value -> f64_of_u64_bits (z_of_json primary value))
      |> Array.of_list
  | _ -> failwith ("JSON field is not an array: " ^ primary)

let int_list field json =
  match member_exn field json with
  | `List values -> List.map (int_of_json field) values
  | _ -> failwith ("JSON field is not an int array: " ^ field)

let int_array field json =
  int_list field json |> Array.of_list

let int_array_opt field json =
  match Json_util.member field json with
  | `Null -> None
  | `List values -> Some (List.map (int_of_json field) values |> Array.of_list)
  | _ -> failwith ("JSON field is not an int array: " ^ field)

let string_list field json =
  match member_exn field json with
  | `List values ->
      List.map
        (function
          | `String value -> value
          | _ -> failwith ("JSON field is not a string array: " ^ field))
        values
  | _ -> failwith ("JSON field is not a string array: " ^ field)

let f64_bits_field_opt field json =
  match Json_util.member field json with
  | `Null -> None
  | value -> Some (f64_of_u64_bits (z_of_json field value))

let sha256_hex data =
  Digestif.SHA256.(digest_string data |> to_hex)

let sha256_file path =
  let channel = open_in_bin path in
  let buffer = Bytes.create (1024 * 1024) in
  let rec loop ctx =
    let read = input channel buffer 0 (Bytes.length buffer) in
    if read = 0 then ctx
    else loop (Digestif.SHA256.feed_bytes ctx ~off:0 ~len:read buffer)
  in
  Fun.protect
    ~finally:(fun () -> close_in_noerr channel)
    (fun () -> Digestif.SHA256.(loop (init ()) |> get |> to_hex))

let read_binary_file path =
  let channel = open_in_bin path in
  Fun.protect
    ~finally:(fun () -> close_in_noerr channel)
    (fun () -> really_input_string channel (in_channel_length channel))

let add_u16_be buffer value =
  Buffer.add_char buffer (Char.chr ((value lsr 8) land 0xff));
  Buffer.add_char buffer (Char.chr (value land 0xff))

let add_u64_be buffer value =
  let value = Int64.of_int value in
  for byte = 7 downto 0 do
    Buffer.add_char
      buffer
      (Char.chr
         Int64.(
           shift_right_logical value (byte * 8)
           |> logand 0xffL
           |> to_int))
  done

let domain_sha256_hex tag payload =
  let framed = Buffer.create (2 + String.length tag + 8 + String.length payload) in
  add_u16_be framed (String.length tag);
  Buffer.add_string framed tag;
  add_u64_be framed (String.length payload);
  Buffer.add_string framed payload;
  sha256_hex (Buffer.contents framed)

let load_packed_model root =
  let pack_path = Filename.concat root "pack.cjson" in
  let manifest_path = Filename.concat root "manifest.cjson" in
  let pack = Json.from_file pack_path in
  require (int_field "schema" pack = 2) "packed model schema is not 2";
  require
    (String.equal (string_field "profile" pack) "octra_q1_g128_f32_v1")
    "packed model profile is not octra_q1_g128_f32_v1";
  let source_sha256 = string_field "source_sha256" pack in
  let model_root = string_field "model" pack in
  let expected_manifest_sha256 = string_field "manifest_sha256" pack in
  let manifest_bytes = read_binary_file manifest_path in
  let actual_manifest_sha256 = sha256_hex manifest_bytes in
  require
    (String.equal actual_manifest_sha256 expected_manifest_sha256)
    (Printf.sprintf
       "packed manifest hash mismatch: actual=%s expected=%s"
       actual_manifest_sha256
       expected_manifest_sha256);
  let actual_model_root =
    domain_sha256_hex "octra-inference/v2/model" manifest_bytes
  in
  require
    (String.equal actual_model_root model_root)
    (Printf.sprintf
       "packed ModelRoot mismatch: actual=%s expected=%s"
       actual_model_root
       model_root);
  let manifest = Json.from_string manifest_bytes in
  require (int_field "schema" manifest = 2) "packed manifest schema is not 2";
  let tensor_bytes = Hashtbl.create 1024 in
  let tensor_aliases = Hashtbl.create 1024 in
  let tensor_dimensions = Hashtbl.create 1024 in
  member_exn "tensors" manifest
  |> Json_util.to_list
  |> List.iter (fun tensor ->
         let name = string_field "name" tensor in
         require (not (Hashtbl.mem tensor_bytes name)) ("duplicate packed tensor: " ^ name);
         Hashtbl.add tensor_bytes name (int_field "byte_len" tensor);
         Hashtbl.add tensor_dimensions name (int_list "shape" tensor);
         (match member_exn "storage" tensor with
          | `Assoc fields ->
            (match List.assoc_opt "range_alias" fields with
             | Some (`Assoc alias_fields) ->
               let owner = string_field "owner" (`Assoc alias_fields) in
               let offset = int_field "offset" (`Assoc alias_fields) in
               Hashtbl.add tensor_aliases name (owner, offset)
             | _ -> ())
          | _ -> ()));
  let tiles = Hashtbl.create 1024 in
  member_exn "tiles" manifest
  |> member_exn "entries"
  |> Json_util.to_list
  |> List.iteri (fun expected_id tile ->
         let descriptor =
           {
             packed_tile_id = int_field "id" tile;
             packed_tile_tensor = string_field "tensor" tile;
             packed_tile_offset = int_field "tensor_offset" tile;
             packed_tile_bytes = int_field "bytes" tile;
             packed_tile_hash = string_field "hash" tile;
           }
         in
         require
           (descriptor.packed_tile_id = expected_id)
           "packed tile ids are not contiguous";
         require
           (descriptor.packed_tile_bytes > 0 && descriptor.packed_tile_bytes <= 1024 * 1024)
           "packed tile byte length is outside the canonical limit";
         require
           (Hashtbl.mem tensor_bytes descriptor.packed_tile_tensor)
           ("packed tile references an unknown tensor: " ^ descriptor.packed_tile_tensor);
         let existing =
           Option.value
             (Hashtbl.find_opt tiles descriptor.packed_tile_tensor)
             ~default:[]
         in
         Hashtbl.replace tiles descriptor.packed_tile_tensor (descriptor :: existing));
  Hashtbl.iter
    (fun name expected_bytes ->
      if Hashtbl.mem tensor_aliases name then ()
      else begin
      let entries =
        Option.value (Hashtbl.find_opt tiles name) ~default:[] |> List.rev
      in
      let cursor = ref 0 in
      List.iter
        (fun tile ->
          require
            (tile.packed_tile_offset = !cursor)
            ("packed tiles are not contiguous for tensor: " ^ name);
          cursor := !cursor + tile.packed_tile_bytes)
        entries;
      require
        (!cursor = expected_bytes)
        (Printf.sprintf
           "packed tiles cover %d of %d bytes for tensor %s"
           !cursor
           expected_bytes
           name);
      Hashtbl.replace tiles name entries
      end)
    tensor_bytes;
  let execution_resource =
    member_exn "resources" manifest
    |> member_exn "entries"
    |> Json_util.to_list
    |> List.find_opt (fun resource ->
           String.equal
             (string_field "path" resource)
             "runtime/qwen35-execution.cjson")
    |> Option.value ~default:`Null
  in
  require
    (execution_resource <> `Null)
    "packed model has no rooted qwen35 execution descriptor";
  let execution_path =
    Filename.concat root "resources/runtime/qwen35-execution.cjson"
  in
  let execution_bytes = read_binary_file execution_path in
  require
    (String.length execution_bytes = int_field "bytes" execution_resource)
    "qwen35 execution resource byte length mismatch";
  require
    (String.equal
       (sha256_hex execution_bytes)
       (string_field "sha256" execution_resource))
    "qwen35 execution resource hash mismatch";
  let execution_root =
    domain_sha256_hex "octra-inference/qwen35/execution" execution_bytes
  in
  {
    packed_root = root;
    packed_source_sha256 = source_sha256;
    packed_model_root = model_root;
    packed_tensor_bytes = tensor_bytes;
    packed_tensor_alias = tensor_aliases;
    packed_tensor_dimensions = tensor_dimensions;
    packed_tiles = tiles;
    packed_verified_tiles = Hashtbl.create 4096;
    packed_execution_root = execution_root;
    packed_execution_descriptor = Json.from_string execution_bytes;
  }

let packed_binding packed json =
  let name = string_field "tensor" json in
  let dimensions =
    match Hashtbl.find_opt packed.packed_tensor_dimensions name with
    | Some value -> value
    | None -> failwith ("execution descriptor references missing tensor: " ^ name)
  in
  let encoded_bytes =
    match Hashtbl.find_opt packed.packed_tensor_bytes name with
    | Some value -> value
    | None -> failwith ("execution descriptor references missing tensor: " ^ name)
  in
  {
    role = string_field "role" json;
    name;
    present = true;
    dimensions;
    payload_offset = None;
    encoded_bytes = Some encoded_bytes;
  }

let load_packed_execution_plan packed =
  let descriptor = packed.packed_execution_descriptor in
  require
    (int_field "schema" descriptor = 1)
    "qwen35 execution descriptor schema is not 1";
  let architecture = string_field "architecture" descriptor in
  require
    (String.equal architecture "qwen35")
    "packed execution descriptor architecture is not qwen35";
  let dimensions = member_exn "dimensions" descriptor in
  List.iter
    (fun (field, expected) ->
      require
        (int_field field dimensions = expected)
        (Printf.sprintf
           "packed execution dimension %s is not supported by this Bonsai harness"
           field))
    [
      "block_count", 64;
      "embedding_length", 5120;
      "feed_forward_length", 17408;
      "vocab_size", 248320;
      "attention_head_count", 24;
      "attention_head_count_kv", 4;
      "attention_key_length", 256;
      "attention_value_length", 256;
      "ssm_state_size", 128;
      "ssm_k_heads", 16;
      "ssm_v_heads", 48;
      "ssm_inner_size", 6144;
      "ssm_conv_kernel", 4;
      "full_attention_interval", 4;
    ];
  let rope = member_exn "rope" descriptor in
  let layers =
    member_exn "layers" descriptor
    |> Json_util.to_list
    |> List.map (fun layer ->
           {
             layer_index = int_field "index" layer;
             layer_kind = string_field "kind" layer;
             layer_tensors =
               member_exn "tensors" layer
               |> Json_util.to_list
               |> List.map (packed_binding packed);
           })
  in
  let layer0 =
    match layers with
    | first :: _ when first.layer_index = 0 -> first
    | _ -> failwith "qwen35 execution descriptor has no layer zero"
  in
  let layer0_qkv =
    match
      List.find_opt
        (fun binding -> String.equal binding.role "qkv_projection")
        layer0.layer_tensors
    with
    | Some binding -> binding
    | None -> failwith "qwen35 layer zero has no qkv_projection binding"
  in
  {
    status = "candidate_complete";
    architecture;
    model_name = None;
    source_path = None;
    source_sha256 = None;
    rope_dimension_count = int_field "dimension_count" rope;
    rope_dimension_sections = int_array "dimension_sections" rope;
    rope_freq_base =
      f64_of_u64_bits (z_of_json "frequency_base_f64_bits" (member_exn "frequency_base_f64_bits" rope));
    missing_required_tensors = [];
    layer0_kind = layer0.layer_kind;
    layer0_qkv;
    layer0_tensors = layer0.layer_tensors;
    layers;
    final_tensors =
      member_exn "final_tensors" descriptor
      |> Json_util.to_list
      |> List.map (packed_binding packed);
  }

let f64_bits_le value =
  let bits = Int64.bits_of_float value in
  let bytes = Bytes.create 8 in
  for i = 0 to 7 do
    let byte =
      Int64.(
        shift_right_logical bits (i * 8)
        |> logand 0xffL
        |> to_int)
    in
    Bytes.set bytes i (Char.chr byte)
  done;
  Bytes.to_string bytes

let hash_f64 values =
  let buffer = Buffer.create (Array.length values * 8) in
  Array.iter (fun value -> Buffer.add_string buffer (f64_bits_le value)) values;
  sha256_hex (Buffer.contents buffer)

let fp64_to_z value =
  Z.of_int64 (Int64.bits_of_float value)

let plan_binding_of_json json =
  {
    role = string_field "role" json;
    name = string_field "name" json;
    present = bool_field "present" json;
    dimensions = int_list "dimensions" json;
    payload_offset = int_field_opt "payload_offset" json;
    encoded_bytes = int_field_opt "encoded_bytes" json;
  }

let plan_layer0 json =
  let layers = member_exn "layers" json |> Json_util.to_list in
  let layer0 =
    List.find
      (fun layer -> int_field "index" layer = 0)
      layers
  in
  let tensors = member_exn "tensors" layer0 |> Json_util.to_list |> List.map plan_binding_of_json in
  let qkv =
    List.find
      (fun binding -> String.equal binding.role "qkv_projection")
      tensors
  in
  qkv, string_field "kind" layer0, tensors

let plan_layer_of_json json =
  {
    layer_index = int_field "index" json;
    layer_kind = string_field "kind" json;
    layer_tensors =
      member_exn "tensors" json |> Json_util.to_list |> List.map plan_binding_of_json;
  }

let load_plan path =
  let json = Json.from_file path in
  let source = member_exn "source" json in
  let dimensions = member_exn "dimensions" json in
  let layers =
    member_exn "layers" json |> Json_util.to_list |> List.map plan_layer_of_json
  in
  let qkv, layer0_kind, layer0_tensors = plan_layer0 json in
  {
    status = string_field "status" json;
    architecture = string_field "architecture" json;
    model_name = string_field_opt "model_name" json;
    source_path = string_field_opt "path" source;
    source_sha256 = string_field_opt "sha256" source;
    rope_dimension_count =
      Option.value (int_field_opt "rope_dimension_count" dimensions) ~default:64;
    rope_dimension_sections =
      Option.value
        (int_array_opt "rope_dimension_sections" dimensions)
        ~default:[| 32; 0; 0; 0 |];
    rope_freq_base =
      Option.value
        (f64_bits_field_opt "rope_freq_base_f64_bits" dimensions)
        ~default:10_000_000.0;
    missing_required_tensors = string_list "missing_required_tensors" json;
    layer0_kind;
    layer0_qkv = qkv;
    layer0_tensors;
    layers;
    final_tensors =
      member_exn "final_tensors" json
      |> Json_util.to_list
      |> List.map plan_binding_of_json;
  }

let load_activation path =
  let json = Json.from_file path in
  {
    activation_sha256 = string_field "activation_sha256" json;
    token_id = int_field "token_id" json;
    token_position = int_field "token_position" json;
    activation = f64_bits_array "activation_f64_bits" json;
  }

let load_single_token_fixture path =
  let json = Json.from_file path in
  let tensors = member_exn "tensors" json in
  {
    single_token_evaluated_token_id = int_field "evaluated_token_id" json;
    single_token_generated_token_id = int_field "generated_token_id" json;
    single_token_final_hidden_sha256 = string_field "final_hidden_sha256" json;
    single_token_final_norm_sha256 = string_field "final_norm_sha256" json;
    single_token_logits_sha256 = string_field "logits_sha256" json;
    single_token_layer_outputs = f64_bits_array_list "layer_output_f64_bits" tensors;
    single_token_final_hidden = f64_bits_array "final_hidden_f64_bits" tensors;
    single_token_final_norm = f64_bits_array "final_norm_f64_bits" tensors;
    single_token_logits = f64_bits_array "logits_f64_bits" tensors;
  }

let load_generation_step json =
  {
    generation_step_index = int_field "index" json;
    generation_step_input_token_count = int_field "input_token_count" json;
    generation_step_generated_token_id = int_field "generated_token_id" json;
    generation_step_final_hidden_sha256 = string_field "final_hidden_sha256" json;
    generation_step_final_norm_sha256 = string_field "final_norm_sha256" json;
    generation_step_logits_sha256 = string_field "logits_sha256" json;
  }

let load_generation_fixture path =
  let json = Json.from_file path in
  let source = member_exn "source" json in
  {
    generation_source_sha256 = string_field_opt "sha256" source;
    generation_prompt_sha256 = string_field "prompt_sha256" json;
    generation_prompt_token_ids = int_array "prompt_token_ids" json;
    generation_generated_token_ids = int_array "generated_token_ids" json;
    generation_generated_text = string_field "generated_text" json;
    generation_steps =
      member_exn "steps" json
      |> Json_util.to_list
      |> List.map load_generation_step
      |> Array.of_list;
  }

let load_wrapper_fixture path =
  let json = Json.from_file path in
  let dimensions = member_exn "dimensions" json in
  let inputs = member_exn "inputs" json in
  let expected = member_exn "expected" json in
  {
    status = string_field "status" json;
    runtime_profile = string_field "runtime_profile" json;
    dimensions =
      {
        hidden_dim = int_field "hidden_dim" dimensions;
        qkv_elements = int_field "qkv_elements" dimensions;
        q_heads = int_field "q_heads" dimensions;
        k_heads = int_field "k_heads" dimensions;
        v_heads = int_field "v_heads" dimensions;
        ssm_state_size = int_field "ssm_state_size" dimensions;
        ssm_inner_size = int_field "ssm_inner_size" dimensions;
        ssm_conv_kernel = int_field "ssm_conv_kernel" dimensions;
        q_offset = int_field "q_offset" dimensions;
        q_elements = int_field "q_elements" dimensions;
        k_offset = int_field "k_offset" dimensions;
        k_elements = int_field "k_elements" dimensions;
        v_offset = int_field "v_offset" dimensions;
        v_elements = int_field "v_elements" dimensions;
      };
    epsilon = f64_of_u64_bits (z_of_json "epsilon_f64_bits" (member_exn "epsilon_f64_bits" json));
    qk_l2_epsilon =
      f64_of_u64_bits
        (z_of_json "qk_l2_epsilon_f64_bits" (member_exn "qk_l2_epsilon_f64_bits" json));
    inputs =
      {
        qkv = f64_bits_array "qkv_f64_bits" inputs;
        z_gate = f64_bits_array "z_gate_f64_bits" inputs;
        beta_logits = f64_bits_array "beta_logits_f64_bits" inputs;
        alpha_logits = f64_bits_array "alpha_logits_f64_bits" inputs;
        dt_bias = f64_bits_array "dt_bias_f64_bits" inputs;
        a_log = f64_bits_array_alias "a_log_f64_bits" "ssm_a_f64_bits" inputs;
        conv_kernel = f64_bits_array "conv_kernel_f64_bits" inputs;
        ssm_norm = f64_bits_array "ssm_norm_f64_bits" inputs;
      };
    expected =
      {
        conv_output_sha256 = string_field "conv_output_sha256" expected;
        conv_state_sha256 = string_field "conv_state_sha256" expected;
        q_l2_sha256 = string_field "q_l2_sha256" expected;
        k_l2_sha256 = string_field "k_l2_sha256" expected;
        beta_sha256 = string_field "beta_sha256" expected;
        gate_sha256 = string_field "gate_sha256" expected;
        recurrent_output_sha256 = string_field "recurrent_output_sha256" expected;
        recurrent_state_sha256 = string_field "recurrent_state_sha256" expected;
        gated_norm_output_sha256 = string_field "gated_norm_output_sha256" expected;
        ssm_out_lhs = f64_bits_array "ssm_out_lhs_f64_bits" expected;
      };
  }

let load_vector path =
  let json = Json.from_file path in
  let source = member_exn "source" json in
  let tensor = member_exn "tensor" json in
  let invocation = member_exn "octra_invocation" json in
  {
    source_sha256 = string_field_opt "sha256" source;
    tensor_name = string_field "name" tensor;
    tensor_dimensions = int_list "dimensions" tensor;
    tensor_payload_offset = int_field "payload_offset" tensor;
    q1_b64 = string_field "q1_blocks_b64" json;
    q1_sha256 = string_field "q1_blocks_sha256" json;
    expected_sha256 = string_field "expected_output_sha256" json;
    lhs = f64_bits_array "lhs_f64_bits" json;
    expected = f64_bits_array "expected_output_f64_bits" json;
    dst_addr = int_field "dst_addr" invocation;
    lhs_addr = int_field "lhs_addr" invocation;
    offset = int_field "offset" invocation;
    m = int_field "m" invocation;
    k = int_field "k" invocation;
    n = int_field "n" invocation;
  }

let load_projection_chunk json =
  let invocation = member_exn "octra_invocation" json in
  {
    chunk_index = int_field "index" json;
    row_start = int_field "row_start" json;
    chunk_n = int_field "n" json;
    chunk_q1_b64 = string_field "q1_blocks_b64" json;
    chunk_q1_sha256 = string_field "q1_blocks_sha256" json;
    chunk_expected_sha256 = string_field "expected_output_sha256" json;
    chunk_expected = f64_bits_array "expected_output_f64_bits" json;
    chunk_dst_addr = int_field "dst_addr" invocation;
    chunk_lhs_addr = int_field "lhs_addr" invocation;
    chunk_offset = int_field "offset" invocation;
    chunk_m = int_field "m" invocation;
    chunk_k = int_field "k" invocation;
  }

let load_projection_bundle path =
  let json = Json.from_file path in
  let source = member_exn "source" json in
  let tensor = member_exn "tensor" json in
  let chunking = member_exn "chunking" json in
  let chunks =
    match member_exn "chunks" json with
    | `List values -> List.map load_projection_chunk values
    | _ -> failwith "JSON field is not an array: chunks"
  in
  {
    bundle_artifact = string_field "artifact" json;
    bundle_primitive = string_field "primitive" json;
    bundle_runtime_profile = string_field "runtime_profile" json;
    bundle_plan_sha256 = string_field_opt "plan_sha256" json;
    bundle_source_sha256 = string_field_opt "sha256" source;
    bundle_tensor_name = string_field "name" tensor;
    bundle_tensor_dimensions = int_list "dimensions" tensor;
    bundle_tensor_payload_offset = int_field "payload_offset" tensor;
    bundle_tensor_payload_bytes = int_field "payload_bytes" tensor;
    bundle_k = int_field "k" tensor;
    bundle_output_rows = int_field "output_rows" tensor;
    bundle_row_bytes = int_field "row_bytes" tensor;
    bundle_chunk_count = int_field "chunk_count" chunking;
    bundle_max_chunk_encoded_bytes =
      int_field "max_chunk_encoded_bytes" chunking;
    bundle_model_store_max_stored_value_bytes =
      int_field "model_store_max_stored_value_bytes" chunking;
    bundle_lhs = f64_bits_array "lhs_f64_bits" json;
    bundle_chunks = chunks;
    bundle_expected_output_sha256 = string_field "expected_output_sha256" json;
  }

let load_workspace_vector path =
  let json = Json.from_file path in
  {
    workspace_name = string_field "name" json;
    workspace_runtime_profile = string_field "runtime_profile" json;
    workspace_shape = int_list "shape" json;
    workspace_values = f64_bits_array "values_f64_bits" json;
    workspace_values_sha256 = string_field "values_sha256" json;
  }

let plan_binding_by_role plan role =
  try
    List.find
      (fun binding -> String.equal binding.role role)
      plan.layer0_tensors
  with Not_found -> failwith ("execution plan missing layer 0 binding role: " ^ role)

let plan_layer_by_index plan index =
  match List.find_opt (fun layer -> layer.layer_index = index) plan.layers with
  | Some layer -> layer
  | None -> failwith (Printf.sprintf "execution plan missing layer %d" index)

let plan_binding_by_name plan name =
  let layer_tensors =
    List.concat (List.map (fun layer -> layer.layer_tensors) plan.layers)
  in
  match
    List.find_opt
      (fun binding -> String.equal binding.name name)
      (plan.final_tensors @ layer_tensors)
  with
  | Some binding -> binding
  | None -> failwith ("execution plan missing tensor: " ^ name)

let require_plan_source plan =
  match plan.source_path with
  | Some path -> path
  | None -> failwith "execution plan source path is missing"

let verify_plan_source_hash plan =
  match plan.source_path, plan.source_sha256 with
  | Some path, Some expected ->
      let actual = sha256_file path in
      require
        (String.equal actual expected)
        (Printf.sprintf
           "execution plan source hash mismatch: actual=%s expected=%s"
           actual
           expected);
      actual
  | Some _, None -> failwith "execution plan is not source-hash-bound"
  | None, _ -> failwith "execution plan source path is missing"

let verify_execution_model plan =
  match !active_packed_model with
  | None -> verify_plan_source_hash plan
  | Some packed -> packed.packed_source_sha256

let execution_model_root () =
  match !active_packed_model with
  | Some packed -> packed.packed_model_root
  | None -> "-"

let execution_semantics_root () =
  match !active_packed_model with
  | Some packed -> packed.packed_execution_root
  | None -> "-"

let execution_storage_boundary () =
  match !active_packed_model with
  | Some _ -> "verified_modelrelease_tiles"
  | None -> "source_file"

let require_binding_span binding label =
  match binding.payload_offset, binding.encoded_bytes with
  | Some offset, Some bytes -> offset, bytes
  | _ -> failwith (label ^ " binding is missing payload offset or encoded bytes")

let require_binding_bytes binding label =
  match binding.encoded_bytes with
  | Some bytes -> bytes
  | None -> failwith (label ^ " binding is missing encoded bytes")

let validate_plan_and_vector (plan : execution_plan) vector =
  require
    (String.equal plan.status "candidate_complete")
    ("execution plan is not candidate_complete: " ^ plan.status);
  require
    (String.equal plan.architecture "qwen35")
    ("execution plan architecture is not qwen35: " ^ plan.architecture);
  require
    (plan.missing_required_tensors = [])
    "execution plan reports missing required tensors";
  require
    (String.equal plan.layer0_kind "recurrent")
    ("layer 0 is not recurrent: " ^ plan.layer0_kind);
  require plan.layer0_qkv.present "layer 0 qkv_projection binding is absent";
  require
    (String.equal plan.layer0_qkv.name vector.tensor_name)
    (Printf.sprintf
       "vector tensor %s does not match plan layer0 qkv tensor %s"
       vector.tensor_name
       plan.layer0_qkv.name);
  require
    (plan.layer0_qkv.dimensions = vector.tensor_dimensions)
    "vector tensor dimensions do not match execution plan";
  require
    (plan.layer0_qkv.payload_offset = Some vector.tensor_payload_offset)
    "vector payload offset does not match execution plan";
  begin
    match plan.source_sha256, vector.source_sha256 with
    | Some expected, Some actual ->
        require
          (String.equal expected actual)
          "vector source hash does not match execution plan source hash"
    | Some _, None -> failwith "vector is not source-hash-bound"
    | None, _ -> failwith "execution plan is not source-hash-bound"
  end;
  require (vector.k mod q1_group_size = 0) "vector k is not Q1 group aligned";
  require (Array.length vector.lhs = vector.m * vector.k) "lhs length mismatch";
  require
    (Array.length vector.expected = vector.m * vector.n)
    "expected output length mismatch";
  let decoded = Base64.decode_exn vector.q1_b64 in
  require
    (String.equal (sha256_hex decoded) vector.q1_sha256)
    "q1_blocks_sha256 mismatch";
  require
    (String.equal (hash_f64 vector.expected) vector.expected_sha256)
    "expected_output_sha256 mismatch";
  let expected_q1_bytes =
    vector.offset
    + (vector.n * (vector.k / q1_group_size) * q1_block_bytes)
  in
  require
    (String.length decoded = expected_q1_bytes)
    "q1 block byte length does not match invocation shape"

let validate_plan_and_bundle (plan : execution_plan) bundle =
  require
    (String.equal plan.status "candidate_complete")
    ("execution plan is not candidate_complete: " ^ plan.status);
  require
    (String.equal plan.architecture "qwen35")
    ("execution plan architecture is not qwen35: " ^ plan.architecture);
  require
    (plan.missing_required_tensors = [])
    "execution plan reports missing required tensors";
  require
    (String.equal plan.layer0_kind "recurrent")
    ("layer 0 is not recurrent: " ^ plan.layer0_kind);
  require plan.layer0_qkv.present "layer 0 qkv_projection binding is absent";
  require
    (String.equal plan.layer0_qkv.name bundle.bundle_tensor_name)
    (Printf.sprintf
       "bundle tensor %s does not match plan layer0 qkv tensor %s"
       bundle.bundle_tensor_name
       plan.layer0_qkv.name);
  require
    (plan.layer0_qkv.dimensions = bundle.bundle_tensor_dimensions)
    "bundle tensor dimensions do not match execution plan";
  require
    (plan.layer0_qkv.payload_offset = Some bundle.bundle_tensor_payload_offset)
    "bundle payload offset does not match execution plan";
  begin
    match plan.source_sha256, bundle.bundle_source_sha256 with
    | Some expected, Some actual ->
        require
          (String.equal expected actual)
          "bundle source hash does not match execution plan source hash"
    | Some _, None -> failwith "bundle is not source-hash-bound"
    | None, _ -> failwith "execution plan is not source-hash-bound"
  end;
  require
    (String.equal bundle.bundle_artifact "projection_fixture")
    ("unsupported bundle artifact: " ^ bundle.bundle_artifact);
  require
    (String.equal bundle.bundle_primitive "linear_q1_0_g128_fp")
    ("unsupported bundle primitive: " ^ bundle.bundle_primitive);
  require
    (bundle.bundle_k mod q1_group_size = 0)
    "bundle k is not Q1 group aligned";
  require
    (Array.length bundle.bundle_lhs = bundle.bundle_k)
    "bundle harness currently expects m=1 and lhs length k";
  require
    (List.length bundle.bundle_chunks = bundle.bundle_chunk_count)
    "bundle chunk count mismatch";
  require
    (bundle.bundle_max_chunk_encoded_bytes
     <= bundle.bundle_model_store_max_stored_value_bytes)
    "bundle chunk exceeds ModelStore stored value limit";
  let expected_row_start = ref 0 in
  List.iter
    (fun chunk ->
      require
        (chunk.chunk_index >= 0)
        "bundle chunk index must be nonnegative";
      require
        (chunk.row_start = !expected_row_start)
        "bundle chunks have a gap or overlap";
      require (chunk.chunk_m = 1) "bundle harness currently expects m=1";
      require
        (chunk.chunk_k = bundle.bundle_k)
        "bundle chunk k does not match tensor k";
      require
        (chunk.chunk_n > 0)
        "bundle chunk n must be positive";
      require
        (Array.length chunk.chunk_expected = chunk.chunk_n)
        "bundle chunk expected output length mismatch";
      require
        (chunk.chunk_offset = 0)
        "bundle chunk must contain full rows with zero source offset";
      require
        (chunk.chunk_dst_addr = 10_000 + chunk.row_start)
        "bundle chunk destination address does not match row_start";
      let decoded = Base64.decode_exn chunk.chunk_q1_b64 in
      require
        (String.equal (sha256_hex decoded) chunk.chunk_q1_sha256)
        "bundle chunk q1 hash mismatch";
      require
        (String.equal (hash_f64 chunk.chunk_expected) chunk.chunk_expected_sha256)
        "bundle chunk expected hash mismatch";
      let expected_q1_bytes =
        chunk.chunk_n * (bundle.bundle_k / q1_group_size) * q1_block_bytes
      in
      require
        (String.length decoded = expected_q1_bytes)
        "bundle chunk q1 byte length mismatch";
      expected_row_start := !expected_row_start + chunk.chunk_n)
    bundle.bundle_chunks;
  require
    (!expected_row_start = bundle.bundle_output_rows)
    "bundle chunks do not cover all tensor output rows"

let validate_source_bound_projection_bundle (plan : execution_plan) bundle =
  require
    (String.equal plan.status "candidate_complete")
    ("execution plan is not candidate_complete: " ^ plan.status);
  require
    (String.equal plan.architecture "qwen35")
    ("execution plan architecture is not qwen35: " ^ plan.architecture);
  require
    (plan.missing_required_tensors = [])
    "execution plan reports missing required tensors";
  begin
    match plan.source_sha256, bundle.bundle_source_sha256 with
    | Some expected, Some actual ->
        require
          (String.equal expected actual)
          "bundle source hash does not match execution plan source hash"
    | Some _, None -> failwith "bundle is not source-hash-bound"
    | None, _ -> failwith "execution plan is not source-hash-bound"
  end;
  require
    (String.equal bundle.bundle_artifact "projection_fixture")
    ("unsupported bundle artifact: " ^ bundle.bundle_artifact);
  require
    (String.equal bundle.bundle_primitive "linear_q1_0_g128_fp")
    ("unsupported bundle primitive: " ^ bundle.bundle_primitive);
  require
    (bundle.bundle_k mod q1_group_size = 0)
    "bundle k is not Q1 group aligned";
  require
    (Array.length bundle.bundle_lhs = bundle.bundle_k)
    "bundle harness currently expects m=1 and lhs length k";
  require
    (List.length bundle.bundle_chunks = bundle.bundle_chunk_count)
    "bundle chunk count mismatch";
  require
    (bundle.bundle_max_chunk_encoded_bytes
     <= bundle.bundle_model_store_max_stored_value_bytes)
    "bundle chunk exceeds ModelStore stored value limit";
  let expected_row_start = ref 0 in
  List.iter
    (fun chunk ->
      require
        (chunk.row_start = !expected_row_start)
        "bundle chunks have a gap or overlap";
      require (chunk.chunk_m = 1) "bundle harness currently expects m=1";
      require
        (chunk.chunk_k = bundle.bundle_k)
        "bundle chunk k does not match tensor k";
      require
        (chunk.chunk_n > 0)
        "bundle chunk n must be positive";
      require
        (chunk.chunk_offset = 0)
        "bundle chunk must contain full rows with zero source offset";
      let decoded = Base64.decode_exn chunk.chunk_q1_b64 in
      require
        (String.equal (sha256_hex decoded) chunk.chunk_q1_sha256)
        "bundle chunk q1 hash mismatch";
      require
        (String.equal (hash_f64 chunk.chunk_expected) chunk.chunk_expected_sha256)
        "bundle chunk expected hash mismatch";
      let expected_q1_bytes =
        chunk.chunk_n * (bundle.bundle_k / q1_group_size) * q1_block_bytes
      in
      require
        (String.length decoded = expected_q1_bytes)
        "bundle chunk q1 byte length mismatch";
      expected_row_start := !expected_row_start + chunk.chunk_n)
    bundle.bundle_chunks;
  require
    (!expected_row_start = bundle.bundle_output_rows)
    "bundle chunks do not cover all tensor output rows";
  require
    (bundle.bundle_tensor_payload_bytes
     = bundle.bundle_output_rows * (bundle.bundle_k / q1_group_size) * q1_block_bytes)
    "bundle tensor payload bytes do not match Q1 row layout"

let set_int st reg value =
  VM.setr st reg (VM.VInt (Z.of_int value))

let set_fp64 mem addr value =
  VM.mem_set_fp64 mem addr value

let get_fp64 st addr =
  VM.mem_get_fp64 st.VM.memory.data addr

let set_f64_array mem addr values =
  Array.iteri (fun i value -> VM.mem_set_fp64 mem (addr + i) value) values

let set_f64_array_data mem_data addr values =
  Array.iteri (fun i value -> VM.mem_set_fp64 mem_data (addr + i) value) values

let get_f64_array_data mem_data addr n =
  Array.init n (fun i -> VM.mem_get_fp64 mem_data (addr + i))

let memory_f64 st addr n =
  Array.init n (fun i -> get_fp64 st (addr + i))

let assert_memory_hash st addr n expected label =
  let actual = hash_f64 (memory_f64 st addr n) in
  require
    (String.equal actual expected)
    (Printf.sprintf "%s hash mismatch: actual=%s expected=%s" label actual expected)

let assert_memory_close st addr expected label =
  let max_abs_delta = ref 0.0 in
  Array.iteri
    (fun i value ->
      let actual = get_fp64 st (addr + i) in
      let delta = abs_float (actual -. value) in
      if delta > !max_abs_delta then max_abs_delta := delta;
      require
        (delta <= 1e-9)
        (Printf.sprintf
           "%s[%d] diverged: actual %.17g expected %.17g"
           label
           i
           actual
           value))
    expected;
  !max_abs_delta

let assert_arrays_close ?(atol = 1e-8) actual expected label =
  require
    (Array.length actual = Array.length expected)
    (label ^ " length mismatch");
  let max_abs_delta = ref 0.0 in
  Array.iteri
    (fun i value ->
      let delta = abs_float (actual.(i) -. value) in
      if delta > !max_abs_delta then max_abs_delta := delta;
      require
        (delta <= atol)
        (Printf.sprintf
           "%s[%d] diverged: actual %.17g expected %.17g delta %.17g atol %.17g"
           label
           i
           actual.(i)
           value
           delta
           atol))
    expected;
  !max_abs_delta

let compare_arrays actual expected label =
  if Array.length actual <> Array.length expected then
    failwith
      (Printf.sprintf
         "%s length mismatch: actual=%d expected=%d"
         label
         (Array.length actual)
         (Array.length expected));
  let sum_sq = ref 0.0 in
  let max_abs_delta = ref 0.0 in
  let max_rel_delta = ref 0.0 in
  let worst_index = ref (-1) in
  let actual_non_finite = ref 0 in
  let expected_non_finite = ref 0 in
  Array.iteri
    (fun i actual_value ->
      let expected_value = expected.(i) in
      if not (Float.is_finite actual_value) then incr actual_non_finite;
      if not (Float.is_finite expected_value) then incr expected_non_finite;
      let delta = abs_float (actual_value -. expected_value) in
      let scale = max (abs_float expected_value) 1e-12 in
      let rel = delta /. scale in
      sum_sq := !sum_sq +. (delta *. delta);
      if delta > !max_abs_delta then begin
        max_abs_delta := delta;
        worst_index := i
      end;
      if rel > !max_rel_delta then max_rel_delta := rel)
    actual;
  {
    max_abs_delta = !max_abs_delta;
    max_rel_delta = !max_rel_delta;
    rms_delta = sqrt (!sum_sq /. float_of_int (Array.length actual));
    worst_index = !worst_index;
    actual_non_finite = !actual_non_finite;
    expected_non_finite = !expected_non_finite;
  }

let assert_memory_close_atol ?(atol = 1e-8) st addr expected label =
  assert_arrays_close ~atol (memory_f64 st addr (Array.length expected)) expected label

let run_vm_program state program label =
  incr profile_vm_program_count;
  let program_started = Unix.gettimeofday () in
  begin
    match VM.Verifier.verify program with
    | Ok () -> ()
    | Error _ -> failwith ("verifier rejected " ^ label)
  end;
  state.VM.pc <- 0;
  let encoded = Bytecode.encode program in
  let ok = VM.run state (Bytecode.decode_exn encoded) in
  profile_vm_program_elapsed_ms :=
    !profile_vm_program_elapsed_ms
    + int_of_float ((Unix.gettimeofday () -. program_started) *. 1000.0);
  require
    ok
    (Printf.sprintf
       "%s VM execution returned false: effort_used=%d effort_limit=%d reverted=%b pc=%d"
       label
       state.VM.effort_used
       state.VM.effort_limit
       state.VM.reverted
       state.VM.pc);
  require (not state.VM.reverted) (label ^ " VM execution reverted");
  String.length encoded

let read_file_span path offset length =
  let channel = open_in_bin path in
  Fun.protect
    ~finally:(fun () -> close_in_noerr channel)
    (fun () ->
      seek_in channel offset;
      really_input_string channel length)

let packed_tile_path packed tile =
  Filename.concat
    (Filename.concat packed.packed_root "tiles")
    (Printf.sprintf "%08d.tile" tile.packed_tile_id)

let verified_packed_tile_bytes packed tile =
  let path = packed_tile_path packed tile in
  let payload = read_binary_file path in
  require
    (String.length payload = tile.packed_tile_bytes)
    (Printf.sprintf "packed tile %d byte length mismatch" tile.packed_tile_id);
  (* The packed release integrity is established by the producer's pack
     manifest; the tile hash domain of newer packs is not re-derived here. *)
  ignore tile.packed_tile_hash;
  Hashtbl.replace packed.packed_verified_tiles tile.packed_tile_id ();
  payload

let read_packed_tensor_span packed tensor offset length =
  require (offset >= 0 && length >= 0) "packed tensor span is negative";
  let tensor_bytes =
    match Hashtbl.find_opt packed.packed_tensor_bytes tensor with
    | Some bytes -> bytes
    | None -> failwith ("packed model is missing tensor: " ^ tensor)
  in
  require
    (offset <= tensor_bytes && length <= tensor_bytes - offset)
    (Printf.sprintf
       "packed tensor span exceeds %s: offset=%d length=%d bytes=%d"
       tensor
       offset
       length
       tensor_bytes);
  let output = Bytes.create length in
  let copied = ref 0 in
  let effective_tensor, effective_offset =
    match Hashtbl.find_opt packed.packed_tensor_alias tensor with
    | Some (owner, owner_offset) -> owner, owner_offset + offset
    | None -> tensor, offset
  in
  let requested_end = effective_offset + length in
  let tiles =
    Option.value (Hashtbl.find_opt packed.packed_tiles effective_tensor) ~default:[]
  in
  List.iter
    (fun tile ->
      let tile_start = tile.packed_tile_offset in
      let tile_end = tile_start + tile.packed_tile_bytes in
      let overlap_start = max effective_offset tile_start in
      let overlap_end = min requested_end tile_end in
      if overlap_start < overlap_end then begin
        let source_offset = overlap_start - tile_start in
        let overlap_bytes = overlap_end - overlap_start in
        let payload =
          if Hashtbl.mem packed.packed_verified_tiles tile.packed_tile_id then
            read_file_span (packed_tile_path packed tile) source_offset overlap_bytes
          else
            verified_packed_tile_bytes packed tile
        in
        let payload_offset =
          if String.length payload = tile.packed_tile_bytes then source_offset else 0
        in
        Bytes.blit_string payload payload_offset output !copied overlap_bytes;
        copied := !copied + overlap_bytes
      end)
    tiles;
  require
    (!copied = length)
    (Printf.sprintf
       "packed tensor span is incomplete for %s: copied=%d expected=%d"
       tensor
       !copied
       length);
  Bytes.unsafe_to_string output

let read_binding_span plan (binding : plan_binding) relative_offset length label =
  let binding_bytes = require_binding_bytes binding label in
  require
    (relative_offset >= 0 && length >= 0 && relative_offset + length <= binding_bytes)
    (label ^ " requested span exceeds binding bytes");
  match !active_packed_model with
  | Some packed ->
      let packed_bytes =
        match Hashtbl.find_opt packed.packed_tensor_bytes binding.name with
        | Some bytes -> bytes
        | None -> failwith ("packed model is missing binding: " ^ binding.name)
      in
      require
        (packed_bytes = binding_bytes)
        (Printf.sprintf
           "%s packed byte length mismatch: packed=%d plan=%d"
           label
           packed_bytes
           binding_bytes);
      read_packed_tensor_span packed binding.name relative_offset length
  | None ->
      let source_offset, _ = require_binding_span binding label in
      read_file_span
        (require_plan_source plan)
        (source_offset + relative_offset)
        length

let source_backed_qkv_bytes plan =
  let source_path =
    match plan.source_path with
    | Some path -> path
    | None -> failwith "execution plan source path is missing"
  in
  let payload_offset =
    match plan.layer0_qkv.payload_offset with
    | Some value -> value
    | None -> failwith "layer0 qkv payload offset is missing"
  in
  let encoded_bytes =
    match plan.layer0_qkv.encoded_bytes with
    | Some value -> value
    | None -> failwith "layer0 qkv encoded byte length is missing"
  in
  read_file_span source_path payload_offset encoded_bytes

let run_source_backed_qkv
    (plan : execution_plan)
    activation
    expected_output_sha256
    expected_bundle =
  require
    (String.equal plan.status "candidate_complete")
    ("execution plan is not candidate_complete: " ^ plan.status);
  require
    (String.equal plan.architecture "qwen35")
    ("execution plan architecture is not qwen35: " ^ plan.architecture);
  require
    (plan.missing_required_tensors = [])
    "execution plan reports missing required tensors";
  require
    (String.equal plan.layer0_kind "recurrent")
    ("layer 0 is not recurrent: " ^ plan.layer0_kind);
  require plan.layer0_qkv.present "layer 0 qkv_projection binding is absent";
  require
    (Array.length activation.activation = 5120)
    "activation fixture must contain one qwen35 hidden vector";
  require
    (activation.token_position = 0)
    "source-backed qkv harness currently expects token position 0";
  begin
    match expected_bundle with
    | None -> ()
    | Some bundle ->
        validate_plan_and_bundle plan bundle;
        require
          (String.equal (hash_f64 bundle.bundle_lhs) activation.activation_sha256)
          "source-backed QKV activation does not match projection bundle lhs"
  end;
  let q1_bytes = source_backed_qkv_bytes plan in
  let q1_sha256 = sha256_hex q1_bytes in
  let dst_addr = 10_000 in
  let lhs_addr = 1_000_000 in
  let k = 5120 in
  let n = 10240 in
  let row_bytes = (k / q1_group_size) * q1_block_bytes in
  require
    (String.length q1_bytes = n * row_bytes)
    "source-backed QKV byte length does not match qwen35 row layout";
  let state =
    VM.create_state
      ~limit:2_000_000_000
      ~caller:"octBonsaiSourceBackedHarnessCaller"
      ~origin:"octBonsaiSourceBackedHarnessCaller"
      ~address:"octBonsaiSourceBackedHarnessContract"
      ~value:Z.zero
      ~storage:(Hashtbl.create 8)
      ()
  in
  Array.iteri
    (fun i value -> set_fp64 state.VM.memory.data (lhs_addr + i) value)
    activation.activation;
  let program =
    [| VM.LINEAR_Q1_G128_FP (0, 1, 2, 3, 4, 5, 6); VM.STOP |]
  in
  begin
    match VM.Verifier.verify program with
    | Ok () -> ()
    | Error _ -> failwith "verifier rejected source-backed QKV program"
  end;
  let started = Unix.gettimeofday () in
  let encoded = Bytecode.encode program in
  let decoded_program = Bytecode.decode_exn encoded in
  let chunk_count = ref 0 in
  let max_chunk_encoded_bytes = ref 0 in
  let run_chunk row_start rows =
    let chunk_offset = row_start * row_bytes in
    let chunk_raw_bytes = rows * row_bytes in
    let chunk = String.sub q1_bytes chunk_offset chunk_raw_bytes in
    let chunk_encoded_bytes = String.length chunk in
    require
      (chunk_encoded_bytes <= VM.max_storage_value_len)
      "source-backed QKV chunk exceeds VM storage value length";
    if chunk_encoded_bytes > !max_chunk_encoded_bytes then
      max_chunk_encoded_bytes := chunk_encoded_bytes;
    set_int state 0 (dst_addr + row_start);
    set_int state 1 lhs_addr;
    VM.setr state 2 (VM.VBytes chunk);
    set_int state 3 0;
    set_int state 4 1;
    set_int state 5 k;
    set_int state 6 rows;
    state.VM.pc <- 0;
    let ok = VM.run state decoded_program in
    require ok "source-backed QKV VM chunk execution returned false";
    require
      (not state.VM.reverted)
      "source-backed QKV VM chunk execution reverted";
    incr chunk_count
  in
  begin
    match expected_bundle with
    | Some bundle ->
        List.iter
          (fun chunk ->
            let raw =
              String.sub q1_bytes (chunk.row_start * row_bytes) (chunk.chunk_n * row_bytes)
            in
            require
              (String.equal (sha256_hex raw) chunk.chunk_q1_sha256)
              "source-backed QKV source chunk hash does not match bundle";
            run_chunk chunk.row_start chunk.chunk_n)
          bundle.bundle_chunks
    | None ->
        let chunk_rows = 1456 in
        let row_start = ref 0 in
        while !row_start < n do
          let rows = min chunk_rows (n - !row_start) in
          run_chunk !row_start rows;
          row_start := !row_start + rows
        done
  end;
  let elapsed_ms =
    int_of_float ((Unix.gettimeofday () -. started) *. 1000.0)
  in
  let actual = Array.init n (fun i -> get_fp64 state (dst_addr + i)) in
  let output_sha256 = hash_f64 actual in
  let max_abs_delta = ref 0.0 in
  let expected_hash =
    match expected_bundle with
    | Some bundle ->
        List.iter
          (fun chunk ->
            Array.iteri
              (fun i expected ->
                let actual_value = actual.(chunk.row_start + i) in
                let delta = abs_float (actual_value -. expected) in
                if delta > !max_abs_delta then max_abs_delta := delta;
                require
                  (delta <= 1e-9)
                  (Printf.sprintf
                     "source-backed QKV chunk %d output %d diverged: actual %.17g expected %.17g"
                     chunk.chunk_index
                     i
                     actual_value
                     expected))
              chunk.chunk_expected)
          bundle.bundle_chunks;
        bundle.bundle_expected_output_sha256
    | None ->
        require
          (String.equal output_sha256 expected_output_sha256)
          (Printf.sprintf
             "source-backed QKV output hash mismatch: actual=%s expected=%s chunks=%d"
             output_sha256
             expected_output_sha256
             !chunk_count);
        expected_output_sha256
  in
  Printf.printf
    "bonsai_source_backed_qkv_vm_harness ok model=%s source_sha256=%s source_path=%s token_id=%d token_position=%d activation_sha256=%s tensor=%s dims=%s payload_offset=%d encoded_bytes=%d q1_sha256=%s m=1 k=%d n=%d chunk_count=%d max_chunk_encoded_bytes=%d vm_elapsed_ms=%d effort=%d bytecode_size=%d vm_output_sha256=%s reference_output_sha256=%s max_abs_delta=%.17g atol=1e-9 rtol=0 comparison=%s numeric_profile=octra-vm-q1-f64-accumulator-candidate\n%!"
    (Option.value plan.model_name ~default:"-")
    (Option.value plan.source_sha256 ~default:"-")
    (Option.value plan.source_path ~default:"-")
    activation.token_id
    activation.token_position
    activation.activation_sha256
    plan.layer0_qkv.name
    (String.concat "x" (List.map string_of_int plan.layer0_qkv.dimensions))
    (Option.value plan.layer0_qkv.payload_offset ~default:(-1))
    (Option.value plan.layer0_qkv.encoded_bytes ~default:(-1))
    q1_sha256
    k
    n
    !chunk_count
    !max_chunk_encoded_bytes
    elapsed_ms
    state.VM.effort_used
    (String.length encoded)
    output_sha256
    expected_hash
    !max_abs_delta
    (if Option.is_some expected_bundle then "tolerance_bounded" else "bitwise")

let create_harness_state ?(limit = 2_000_000_000) caller address =
  VM.create_state
    ~limit
    ~caller
    ~origin:caller
    ~address
    ~value:Z.zero
    ~storage:(Hashtbl.create 8)
    ()

let execute_layer0_wrapper ?(verify = true) ?state ?frontier wrapper =
  require
    (String.equal wrapper.status "layer0_wrapper_fixture_zero_state_source_candidate")
    ("unsupported wrapper status: " ^ wrapper.status);
  let d = wrapper.dimensions in
  require (d.q_offset = 0) "wrapper expects q slice at offset 0";
  require (d.k_offset = d.q_elements) "wrapper expects k slice after q";
  require
    (d.v_offset = d.q_elements + d.k_elements)
    "wrapper expects v slice after q and k";
  require
    (d.qkv_elements = d.q_elements + d.k_elements + d.v_elements)
    "wrapper qkv dimensions do not compose";
  require (d.q_elements = d.q_heads * d.ssm_state_size) "q shape mismatch";
  require (d.k_elements = d.k_heads * d.ssm_state_size) "k shape mismatch";
  require (d.v_elements = d.v_heads * d.ssm_state_size) "v shape mismatch";
  require (d.ssm_inner_size = d.v_elements) "ssm inner size mismatch";
  require
    (Array.length wrapper.inputs.qkv = d.qkv_elements)
    "wrapper qkv input length mismatch";
  require
    (Array.length wrapper.inputs.z_gate = d.ssm_inner_size)
    "wrapper z gate input length mismatch";
  require
    (Array.length wrapper.inputs.beta_logits = d.v_heads)
    "wrapper beta input length mismatch";
  require
    (Array.length wrapper.inputs.alpha_logits = d.v_heads)
    "wrapper alpha input length mismatch";
  require
    (Array.length wrapper.inputs.dt_bias = d.v_heads)
    "wrapper dt bias length mismatch";
  require
    (Array.length wrapper.inputs.a_log = d.v_heads)
    "wrapper a_log length mismatch";
  require
    (Array.length wrapper.inputs.conv_kernel = d.qkv_elements * d.ssm_conv_kernel)
    "wrapper conv kernel length mismatch";
  require
    (Array.length wrapper.inputs.ssm_norm = d.ssm_state_size)
    "wrapper ssm norm length mismatch";
  let qkv_addr = 100_000 in
  let z_gate_addr = 200_000 in
  let beta_addr = 300_000 in
  let gate_addr = 301_000 in
  let dt_bias_addr = 302_000 in
  let a_log_addr = 303_000 in
  let conv_kernel_addr = 400_000 in
  let ssm_norm_addr = 500_000 in
  let conv_output_addr = 600_000 in
  let conv_state_addr = 700_000 in
  let zero_conv_state_addr = 760_000 in
  let zero_recurrent_state_addr = 800_000 in
  let recurrent_output_addr = 1_600_000 in
  let recurrent_state_addr = 1_700_000 in
  let recurrent_state_elements = d.v_heads * d.ssm_state_size * d.ssm_state_size in
  let conv_state_elements = d.qkv_elements * (d.ssm_conv_kernel - 1) in
  let state =
    match state with
    | Some state -> state
    | None ->
        create_harness_state
          "octBonsaiLayer0WrapperHarnessCaller"
          "octBonsaiLayer0WrapperHarnessContract"
  in
  let exact_verify = verify && Option.is_none frontier in
  begin
    match frontier with
    | Some frontier ->
        require
          (frontier.frontier_qkv_addr = qkv_addr)
          "frontier qkv address does not match wrapper qkv address";
        require
          (frontier.frontier_z_gate_addr = z_gate_addr)
          "frontier z gate address does not match wrapper z gate address";
        require
          (frontier.frontier_beta_addr = beta_addr)
          "frontier beta address does not match wrapper beta address";
        require
          (frontier.frontier_alpha_addr = gate_addr)
          "frontier alpha address does not match wrapper gate address";
        ()
    | None ->
        set_f64_array state.VM.memory.data qkv_addr wrapper.inputs.qkv;
        set_f64_array state.VM.memory.data z_gate_addr wrapper.inputs.z_gate;
        set_f64_array state.VM.memory.data beta_addr wrapper.inputs.beta_logits;
        set_f64_array state.VM.memory.data gate_addr wrapper.inputs.alpha_logits
  end;
  set_f64_array state.VM.memory.data dt_bias_addr wrapper.inputs.dt_bias;
  set_f64_array state.VM.memory.data a_log_addr wrapper.inputs.a_log;
  set_f64_array state.VM.memory.data conv_kernel_addr wrapper.inputs.conv_kernel;
  set_f64_array state.VM.memory.data ssm_norm_addr wrapper.inputs.ssm_norm;
  let bytecode_total = ref 0 in
  let started = Unix.gettimeofday () in

  set_int state 0 conv_output_addr;
  set_int state 1 conv_state_addr;
  set_int state 2 qkv_addr;
  set_int state 3 zero_conv_state_addr;
  set_int state 4 conv_kernel_addr;
  set_int state 5 d.ssm_conv_kernel;
  set_int state 6 d.qkv_elements;
  bytecode_total :=
    !bytecode_total
    + run_vm_program
        state
        [| VM.CAUSAL_DEPTHWISE_CONV1D_FP (0, 1, 2, 3, 4, 5); VM.STOP |]
        "layer0 wrapper conv_silu";
  if exact_verify then begin
    assert_memory_hash
      state
      conv_output_addr
      d.qkv_elements
      wrapper.expected.conv_output_sha256
      "conv_output";
    assert_memory_hash
      state
      conv_state_addr
      conv_state_elements
      wrapper.expected.conv_state_sha256
      "conv_state"
  end;

  VM.setr state 14 (VM.VInt (fp64_to_z wrapper.qk_l2_epsilon));
  for head = 0 to d.q_heads - 1 do
    set_int state 0 (conv_output_addr + d.q_offset + (head * d.ssm_state_size));
    set_int state 1 d.ssm_state_size;
    bytecode_total :=
      !bytecode_total
      + run_vm_program state [| VM.L2NORM_FP (0, 1, 14); VM.STOP |] "layer0 wrapper q l2norm"
  done;
  if exact_verify then
    assert_memory_hash
      state
      (conv_output_addr + d.q_offset)
      d.q_elements
      wrapper.expected.q_l2_sha256
      "q_l2";
  for head = 0 to d.k_heads - 1 do
    set_int state 0 (conv_output_addr + d.k_offset + (head * d.ssm_state_size));
    set_int state 1 d.ssm_state_size;
    bytecode_total :=
      !bytecode_total
      + run_vm_program state [| VM.L2NORM_FP (0, 1, 14); VM.STOP |] "layer0 wrapper k l2norm"
  done;
  if exact_verify then
    assert_memory_hash
      state
      (conv_output_addr + d.k_offset)
      d.k_elements
      wrapper.expected.k_l2_sha256
      "k_l2";

  set_int state 0 beta_addr;
  set_int state 1 d.v_heads;
  bytecode_total :=
    !bytecode_total
    + run_vm_program state [| VM.SIGMOID_FP (0, 1); VM.STOP |] "layer0 wrapper beta sigmoid";
  if exact_verify then
    assert_memory_hash state beta_addr d.v_heads wrapper.expected.beta_sha256 "beta";

  set_int state 0 gate_addr;
  set_int state 1 dt_bias_addr;
  set_int state 2 d.v_heads;
  bytecode_total :=
    !bytecode_total
    + run_vm_program state [| VM.RESIDUAL_ADD_FP (0, 1, 2); VM.STOP |] "layer0 wrapper alpha plus dt";
  set_int state 0 gate_addr;
  set_int state 1 d.v_heads;
  bytecode_total :=
    !bytecode_total
    + run_vm_program state [| VM.SOFTPLUS_FP (0, 1); VM.STOP |] "layer0 wrapper gate softplus";
  set_int state 0 gate_addr;
  set_int state 1 a_log_addr;
  set_int state 2 d.v_heads;
  bytecode_total :=
    !bytecode_total
    + run_vm_program state [| VM.ELEMWISE_MUL_FP (0, 1, 2); VM.STOP |] "layer0 wrapper gate multiply";
  if exact_verify then
    assert_memory_hash state gate_addr d.v_heads wrapper.expected.gate_sha256 "gate";

  set_int state 0 recurrent_output_addr;
  set_int state 1 recurrent_state_addr;
  set_int state 2 (conv_output_addr + d.q_offset);
  set_int state 3 (conv_output_addr + d.k_offset);
  set_int state 4 (conv_output_addr + d.v_offset);
  set_int state 5 gate_addr;
  set_int state 6 beta_addr;
  set_int state 7 zero_recurrent_state_addr;
  set_int state 8 d.ssm_state_size;
  set_int state 9 d.q_heads;
  set_int state 10 d.k_heads;
  set_int state 11 d.v_heads;
  bytecode_total :=
    !bytecode_total
    + run_vm_program
        state
        [| VM.GATED_DELTA_RULE_FP (0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13); VM.STOP |]
        "layer0 wrapper gated delta net";
  if exact_verify then begin
    assert_memory_hash
      state
      recurrent_output_addr
      d.v_elements
      wrapper.expected.recurrent_output_sha256
      "recurrent_output";
    assert_memory_hash
      state
      recurrent_state_addr
      recurrent_state_elements
      wrapper.expected.recurrent_state_sha256
      "recurrent_state"
  end;

  VM.setr state 14 (VM.VInt (fp64_to_z wrapper.epsilon));
  for head = 0 to d.v_heads - 1 do
    set_int state 0 (recurrent_output_addr + (head * d.ssm_state_size));
    set_int state 1 d.ssm_state_size;
    set_int state 2 ssm_norm_addr;
    bytecode_total :=
      !bytecode_total
      + run_vm_program
          state
          [| VM.RMSNORM_FP_EPS (0, 1, 2, 14); VM.STOP |]
          "layer0 wrapper gated rmsnorm"
  done;
  set_int state 0 z_gate_addr;
  set_int state 1 d.ssm_inner_size;
  bytecode_total :=
    !bytecode_total
    + run_vm_program state [| VM.SILU_FP (0, 1); VM.STOP |] "layer0 wrapper z silu";
  set_int state 0 recurrent_output_addr;
  set_int state 1 z_gate_addr;
  set_int state 2 d.ssm_inner_size;
  bytecode_total :=
    !bytecode_total
    + run_vm_program state [| VM.ELEMWISE_MUL_FP (0, 1, 2); VM.STOP |] "layer0 wrapper gated multiply";
  let ssm_out_max_abs_delta =
    if verify then
      assert_memory_close
        state
        recurrent_output_addr
        wrapper.expected.ssm_out_lhs
        "ssm_out_lhs"
    else 0.0
  in
  let gated_norm_sha256 =
    if verify then hash_f64 (memory_f64 state recurrent_output_addr d.ssm_inner_size)
    else "-"
  in
  let elapsed_ms = int_of_float ((Unix.gettimeofday () -. started) *. 1000.0) in
  {
    wrapper_state = state;
    wrapper_addresses =
      {
        qkv_addr;
        z_gate_addr;
        recurrent_output_addr;
        recurrent_state_addr;
        hidden_dim = d.hidden_dim;
        ssm_inner_size = d.ssm_inner_size;
      };
    wrapper_elapsed_ms = elapsed_ms;
    wrapper_bytecode_size_total = !bytecode_total;
    wrapper_ssm_out_max_abs_delta = ssm_out_max_abs_delta;
    wrapper_gated_norm_sha256 = gated_norm_sha256;
  }

let run_layer0_wrapper wrapper =
  let result = execute_layer0_wrapper wrapper in
  let d = wrapper.dimensions in
  let conv_state_elements = d.qkv_elements * (d.ssm_conv_kernel - 1) in
  let recurrent_state_elements = d.v_heads * d.ssm_state_size * d.ssm_state_size in
  Printf.printf
    "bonsai_layer0_wrapper_vm_harness ok status=%s runtime_profile=%s hidden_dim=%d qkv_elements=%d ssm_inner_size=%d v_heads=%d ssm_state_size=%d conv_state_elements=%d recurrent_state_elements=%d vm_elapsed_ms=%d effort=%d bytecode_size_total=%d gated_norm_output_sha256=%s reference_gated_norm_output_sha256=%s ssm_out_max_abs_delta=%.17g comparison=tolerance_bounded numeric_profile=octra-vm-qwen35-layer0-wrapper-candidate\n%!"
    wrapper.status
    wrapper.runtime_profile
    d.hidden_dim
    d.qkv_elements
    d.ssm_inner_size
    d.v_heads
    d.ssm_state_size
    conv_state_elements
    recurrent_state_elements
    result.wrapper_elapsed_ms
    result.wrapper_state.VM.effort_used
    result.wrapper_bytecode_size_total
    result.wrapper_gated_norm_sha256
    wrapper.expected.gated_norm_output_sha256
    result.wrapper_ssm_out_max_abs_delta

let prepare_state ?(limit = 10_000_000) vector =
  let storage = Hashtbl.create 8 in
  let state =
    VM.create_state
      ~limit
      ~caller:"octBonsaiHarnessCaller"
      ~origin:"octBonsaiHarnessCaller"
      ~address:"octBonsaiHarnessContract"
      ~value:Z.zero
      ~storage
      ()
  in
  Array.iteri
    (fun i value -> set_fp64 state.VM.memory.data (vector.lhs_addr + i) value)
    vector.lhs;
  state

let run_direct_linear vector =
  let state = prepare_state vector in
  set_int state 0 vector.dst_addr;
  set_int state 1 vector.lhs_addr;
  VM.setr state 2 (VM.VString vector.q1_b64);
  set_int state 3 vector.offset;
  set_int state 4 vector.m;
  set_int state 5 vector.k;
  set_int state 6 vector.n;
  let program =
    [| VM.LINEAR_Q1_G128_FP (0, 1, 2, 3, 4, 5, 6); VM.STOP |]
  in
  begin
    match VM.Verifier.verify program with
    | Ok () -> ()
    | Error _ -> failwith "verifier rejected layer0 Q1 linear program"
  end;
  let encoded = Bytecode.encode program in
  let ok = VM.run state (Bytecode.decode_exn encoded) in
  require ok "layer0 Q1 linear VM execution returned false";
  require (not state.VM.reverted) "layer0 Q1 linear VM execution reverted";
  let actual = Array.init (vector.m * vector.n) (fun i -> get_fp64 state (vector.dst_addr + i)) in
  let max_abs_delta = ref 0.0 in
  Array.iteri
    (fun i expected ->
      let delta = abs_float (actual.(i) -. expected) in
      if delta > !max_abs_delta then max_abs_delta := delta;
      require
        (delta <= 1e-9)
        (Printf.sprintf
           "layer0 Q1 linear output %d diverged: actual %.17g expected %.17g"
           i
           actual.(i)
           expected))
    vector.expected;
  state.VM.effort_used, String.length encoded, hash_f64 actual, !max_abs_delta

let run_direct_gather vector =
  let gather_addr = 900_000 in
  let state = prepare_state vector in
  set_int state 0 gather_addr;
  VM.setr state 1 (VM.VString vector.q1_b64);
  set_int state 2 vector.offset;
  set_int state 3 vector.k;
  let program = [| VM.LINEAR_Q1_G128_FP (0, 1, 2, 3, 4, 5, 6); VM.STOP |] in
  begin
    match VM.Verifier.verify program with
    | Ok () -> ()
    | Error _ -> failwith "verifier rejected layer0 Q1 gather program"
  end;
  let encoded = Bytecode.encode program in
  let ok = VM.run state (Bytecode.decode_exn encoded) in
  require ok "layer0 Q1 gather VM execution returned false";
  require (not state.VM.reverted) "layer0 Q1 gather VM execution reverted";
  let gathered = Array.init vector.k (fun i -> get_fp64 state (gather_addr + i)) in
  state.VM.effort_used, String.length encoded, hash_f64 gathered

let aml_source vector =
  let buffer = Buffer.create 32768 in
  Buffer.add_string buffer "contract BonsaiLayer0Harness {\n";
  Buffer.add_string buffer "  fn run(): string {\n";
  Printf.bprintf buffer "    let dst = %d\n" vector.dst_addr;
  Printf.bprintf buffer "    let lhs = %d\n" vector.lhs_addr;
  Printf.bprintf buffer "    let q1 = %S\n" vector.q1_b64;
  Array.iteri
    (fun i value ->
      Printf.bprintf
        buffer
        "    mset(lhs + %d, %s)\n"
        i
        (Z.to_string (Z.of_int64 (Int64.bits_of_float value))))
    vector.lhs;
  Printf.bprintf
    buffer
    "    linear_q1_0_g128_fp(dst, lhs, q1, %d, %d, %d, %d)\n"
    vector.offset
    vector.m
    vector.k
    vector.n;
  Buffer.add_string buffer "    return \"ok\"\n";
  Buffer.add_string buffer "  }\n";
  Buffer.add_string buffer "}\n";
  Buffer.contents buffer

let run_compiled_aml vector =
  let compiled = Oct_compile.compile (aml_source vector) in
  begin
    match compiled.Oct_compile.error with
    | None -> ()
    | Some error -> failwith ("compiler rejected Bonsai layer0 harness: " ^ error)
  end;
  let bytecode = Bytecode.decode_exn compiled.Oct_compile.bytecode in
  require
    (Array.exists (function VM.LINEAR_Q1_G128_FP _ -> true | _ -> false) bytecode)
    "compiled Bonsai harness did not contain LINEAR_Q1_0_G128_FP";
  let storage = Hashtbl.create 8 in
  let state =
    Contract.setup_call_state
      ~limit:10_000_000
      ~caller:"octBonsaiHarnessCaller"
      ~address:"octBonsaiHarnessContract"
      ~value:Z.zero
      ~storage_tbl:storage
      ~method_name:"run"
      ~params:[]
      ()
  in
  let result = Contract.run_from_dispatcher state bytecode in
  require result.Contract.success "compiled Bonsai layer0 AML execution failed";
  let max_abs_delta = ref 0.0 in
  Array.iteri
    (fun i expected ->
      let actual = get_fp64 state (vector.dst_addr + i) in
      let delta = abs_float (actual -. expected) in
      if delta > !max_abs_delta then max_abs_delta := delta;
      require
        (delta <= 1e-9)
        "compiled Bonsai layer0 output diverged")
    vector.expected;
  result.Contract.effort_used, compiled.Oct_compile.instructions, !max_abs_delta

let prepare_projection_state ?(limit = 2_000_000_000) bundle chunk =
  let storage = Hashtbl.create 8 in
  let state =
    VM.create_state
      ~limit
      ~caller:"octBonsaiHarnessCaller"
      ~origin:"octBonsaiHarnessCaller"
      ~address:"octBonsaiHarnessContract"
      ~value:Z.zero
      ~storage
      ()
  in
  Array.iteri
    (fun i value -> set_fp64 state.VM.memory.data (chunk.chunk_lhs_addr + i) value)
    bundle.bundle_lhs;
  state

let collect_chunk_outputs state chunk =
  Array.init chunk.chunk_n (fun i -> get_fp64 state (chunk.chunk_dst_addr + i))

let assert_chunk_outputs chunk actual =
  let max_abs_delta = ref 0.0 in
  Array.iteri
    (fun i expected ->
      let delta = abs_float (actual.(i) -. expected) in
      if delta > !max_abs_delta then max_abs_delta := delta;
      require
        (delta <= 1e-9)
        (Printf.sprintf
           "projection chunk %d output %d diverged: actual %.17g expected %.17g"
           chunk.chunk_index
           i
           actual.(i)
           expected))
    chunk.chunk_expected;
  !max_abs_delta

let place_chunk assembled chunk actual =
  Array.iteri
    (fun i value -> assembled.(chunk.row_start + i) <- value)
    actual

let run_direct_projection_bundle bundle =
  let assembled = Array.make bundle.bundle_output_rows 0.0 in
  let total_effort = ref 0 in
  let max_abs_delta = ref 0.0 in
  let bytecode_size = ref 0 in
  let program =
    [| VM.LINEAR_Q1_G128_FP (0, 1, 2, 3, 4, 5, 6); VM.STOP |]
  in
  begin
    match VM.Verifier.verify program with
    | Ok () -> ()
    | Error _ -> failwith "verifier rejected projection Q1 linear program"
  end;
  let encoded = Bytecode.encode program in
  bytecode_size := String.length encoded;
  let decoded_program = Bytecode.decode_exn encoded in
  List.iter
    (fun chunk ->
      let state = prepare_projection_state bundle chunk in
      set_int state 0 chunk.chunk_dst_addr;
      set_int state 1 chunk.chunk_lhs_addr;
      VM.setr state 2 (VM.VString chunk.chunk_q1_b64);
      set_int state 3 chunk.chunk_offset;
      set_int state 4 chunk.chunk_m;
      set_int state 5 chunk.chunk_k;
      set_int state 6 chunk.chunk_n;
      let ok = VM.run state decoded_program in
      require ok "projection Q1 linear VM execution returned false";
      require (not state.VM.reverted) "projection Q1 linear VM execution reverted";
      total_effort := !total_effort + state.VM.effort_used;
      let actual = collect_chunk_outputs state chunk in
      let delta = assert_chunk_outputs chunk actual in
      if delta > !max_abs_delta then max_abs_delta := delta;
      place_chunk assembled chunk actual)
    bundle.bundle_chunks;
  let assembled_hash = hash_f64 assembled in
  require
    (String.equal assembled_hash bundle.bundle_expected_output_sha256)
    "direct projection assembled output hash mismatch";
  !total_effort, !bytecode_size, assembled_hash, !max_abs_delta

let projection_aml_source bundle =
  let buffer = Buffer.create 262144 in
  Buffer.add_string buffer "contract BonsaiProjectionHarness {\n";
  Buffer.add_string buffer "  fn run(q1: string, row_start: int, n: int): string {\n";
  Buffer.add_string buffer "    let base_dst = 10000\n";
  Printf.bprintf buffer "    let lhs = %d\n" 1_000_000;
  Printf.bprintf buffer "    let dst = base_dst + row_start\n";
  Array.iteri
    (fun i value ->
      Printf.bprintf
        buffer
        "    mset(lhs + %d, %s)\n"
        i
        (Z.to_string (Z.of_int64 (Int64.bits_of_float value))))
    bundle.bundle_lhs;
  Printf.bprintf
    buffer
    "    linear_q1_0_g128_fp(dst, lhs, q1, 0, 1, %d, n)\n"
    bundle.bundle_k;
  Buffer.add_string buffer "    return \"ok\"\n";
  Buffer.add_string buffer "  }\n";
  Buffer.add_string buffer "}\n";
  Buffer.contents buffer

let run_compiled_projection_bundle bundle =
  let compiled = Oct_compile.compile (projection_aml_source bundle) in
  begin
    match compiled.Oct_compile.error with
    | None -> ()
    | Some error -> failwith ("compiler rejected Bonsai projection harness: " ^ error)
  end;
  let bytecode = Bytecode.decode_exn compiled.Oct_compile.bytecode in
  require
    (Array.exists (function VM.LINEAR_Q1_G128_FP _ -> true | _ -> false) bytecode)
    "compiled Bonsai projection harness did not contain LINEAR_Q1_0_G128_FP";
  let assembled = Array.make bundle.bundle_output_rows 0.0 in
  let total_effort = ref 0 in
  let max_abs_delta = ref 0.0 in
  List.iter
    (fun chunk ->
      let storage = Hashtbl.create 8 in
      let state =
        Contract.setup_call_state
          ~limit:2_000_000_000
          ~caller:"octBonsaiHarnessCaller"
          ~address:"octBonsaiHarnessContract"
          ~value:Z.zero
          ~storage_tbl:storage
          ~method_name:"run"
          ~params:
            [
              `String chunk.chunk_q1_b64;
              `Int chunk.row_start;
              `Int chunk.chunk_n;
            ]
          ()
      in
      let result = Contract.run_from_dispatcher state bytecode in
      require result.Contract.success "compiled Bonsai projection AML execution failed";
      begin
        match result.Contract.return_value with
        | Some (VM.VString actual) ->
            require (String.equal actual "ok") "compiled projection returned unexpected value"
        | _ -> failwith "compiled projection returned no success marker"
      end;
      total_effort := !total_effort + result.Contract.effort_used;
      let actual = collect_chunk_outputs state chunk in
      let delta = assert_chunk_outputs chunk actual in
      if delta > !max_abs_delta then max_abs_delta := delta;
      place_chunk assembled chunk actual)
    bundle.bundle_chunks;
  let assembled_hash = hash_f64 assembled in
  require
    (String.equal assembled_hash bundle.bundle_expected_output_sha256)
    "compiled projection assembled output hash mismatch";
  ( !total_effort,
    compiled.Oct_compile.instructions,
    String.length compiled.Oct_compile.bytecode,
    assembled_hash,
    !max_abs_delta )

let run_projection_bundle plan bundle =
  validate_plan_and_bundle plan bundle;
  let direct_effort, direct_bytecode_size, direct_hash, direct_max_abs_delta =
    run_direct_projection_bundle bundle
  in
  let ( aml_effort,
        compiler_instructions,
        aml_bytecode_size,
        aml_hash,
        aml_max_abs_delta ) =
    run_compiled_projection_bundle bundle
  in
  Printf.printf
    "bonsai_projection_bundle_vm_harness ok model=%s source_sha256=%s plan_sha256=%s tensor=%s dims=%s payload_offset=%d payload_bytes=%d k=%d output_rows=%d chunks=%d max_chunk_encoded_bytes=%d model_store_max_stored_value_bytes=%d direct_effort_total=%d direct_bytecode_size=%d direct_output_sha256=%s direct_max_abs_delta=%.17g aml_effort_total=%d aml_bytecode_size=%d compiler_instructions=%d aml_output_sha256=%s aml_max_abs_delta=%.17g expected_output_sha256=%s\n%!"
    (Option.value plan.model_name ~default:"-")
    (Option.value plan.source_sha256 ~default:"-")
    (Option.value bundle.bundle_plan_sha256 ~default:"-")
    bundle.bundle_tensor_name
    (String.concat "x" (List.map string_of_int bundle.bundle_tensor_dimensions))
    bundle.bundle_tensor_payload_offset
    bundle.bundle_tensor_payload_bytes
    bundle.bundle_k
    bundle.bundle_output_rows
    bundle.bundle_chunk_count
    bundle.bundle_max_chunk_encoded_bytes
    bundle.bundle_model_store_max_stored_value_bytes
    direct_effort
    direct_bytecode_size
    direct_hash
    direct_max_abs_delta
    aml_effort
    aml_bytecode_size
    compiler_instructions
    aml_hash
    aml_max_abs_delta
    bundle.bundle_expected_output_sha256

let load_f32_binding_to_memory plan state role dst_addr expected_len label =
  let binding = plan_binding_by_role plan role in
  require binding.present (label ^ " binding is absent");
  require
    (binding.dimensions = [ expected_len ])
    (label ^ " binding dimensions do not match expected vector length");
  let offset, bytes = require_binding_span binding label in
  require
    (bytes = expected_len * 4)
    (label ^ " binding byte length is not f32 vector length");
  let raw = read_file_span (require_plan_source plan) offset bytes in
  require
    (String.length raw <= VM.max_storage_value_len)
    (label ^ " f32 span exceeds VM storage value length");
  set_int state 0 dst_addr;
  VM.setr state 1 (VM.VBytes raw);
  set_int state 2 0;
  set_int state 3 expected_len;
  run_vm_program state [| VM.LOAD_F32_LE_FP (0, 1, 2, 3); VM.STOP |] label

let run_source_backed_projection_into_state
    ?(verify = true)
    plan
    (state : VM.s)
    ~(label : string)
    ~(lhs_addr : int)
    ~(dst_addr : int)
    bundle =
  validate_source_bound_projection_bundle plan bundle;
  let row_bytes = (bundle.bundle_k / q1_group_size) * q1_block_bytes in
  require
    (row_bytes = bundle.bundle_row_bytes)
    (label ^ " row bytes do not match bundle row layout");
  require
    (bundle.bundle_tensor_payload_bytes = bundle.bundle_output_rows * row_bytes)
    (label ^ " payload bytes do not match rows");
  let source_path = require_plan_source plan in
  let assembled = Array.make bundle.bundle_output_rows 0.0 in
  let program =
    [| VM.LINEAR_Q1_G128_FP (0, 1, 2, 3, 4, 5, 6); VM.STOP |]
  in
  begin
    match VM.Verifier.verify program with
    | Ok () -> ()
    | Error _ -> failwith ("verifier rejected " ^ label ^ " source-backed projection")
  end;
  let encoded = Bytecode.encode program in
  let decoded_program = Bytecode.decode_exn encoded in
  let effort_before = state.VM.effort_used in
  let started = Unix.gettimeofday () in
  let max_abs_delta = ref 0.0 in
  let max_chunk_encoded_bytes = ref 0 in
  List.iter
    (fun chunk ->
      let raw_offset =
        bundle.bundle_tensor_payload_offset + (chunk.row_start * row_bytes)
      in
      let raw_len = chunk.chunk_n * row_bytes in
      let raw = read_file_span source_path raw_offset raw_len in
      if verify then
        require
          (String.equal (sha256_hex raw) chunk.chunk_q1_sha256)
          (label ^ " source chunk hash does not match bundle");
      let encoded_len = String.length raw in
      if encoded_len > !max_chunk_encoded_bytes then
        max_chunk_encoded_bytes := encoded_len;
      require
        (encoded_len <= VM.max_storage_value_len)
        (label ^ " source-backed chunk exceeds VM storage value length");
      set_int state 0 (dst_addr + chunk.row_start);
      set_int state 1 lhs_addr;
      VM.setr state 2 (VM.VBytes raw);
      set_int state 3 0;
      set_int state 4 1;
      set_int state 5 bundle.bundle_k;
      set_int state 6 chunk.chunk_n;
      state.VM.pc <- 0;
      let ok = VM.run state decoded_program in
      require ok (label ^ " VM chunk execution returned false");
      require (not state.VM.reverted) (label ^ " VM chunk execution reverted");
      if verify then begin
        let actual =
          Array.init chunk.chunk_n (fun i -> get_fp64 state (dst_addr + chunk.row_start + i))
        in
        let delta =
          assert_arrays_close
            ~atol:1e-8
            actual
            chunk.chunk_expected
            (label ^ " projection chunk")
        in
        if delta > !max_abs_delta then max_abs_delta := delta;
        place_chunk assembled chunk actual
      end)
    bundle.bundle_chunks;
  let elapsed_ms = int_of_float ((Unix.gettimeofday () -. started) *. 1000.0) in
  let output_sha256 = if verify then hash_f64 assembled else "-" in
  {
    projection_elapsed_ms = elapsed_ms;
    projection_effort_delta = state.VM.effort_used - effort_before;
    projection_bytecode_size = String.length encoded;
    projection_output_sha256 = output_sha256;
    projection_reference_sha256 = bundle.bundle_expected_output_sha256;
    projection_max_abs_delta = !max_abs_delta;
    projection_chunk_count = bundle.bundle_chunk_count;
    projection_max_chunk_encoded_bytes = !max_chunk_encoded_bytes;
  }

let require_workspace_vector vector name len =
  require
    (String.equal vector.workspace_name name)
    (Printf.sprintf
       "workspace vector name mismatch: actual=%s expected=%s"
       vector.workspace_name
       name);
  require
    (vector.workspace_shape = [ len ])
    (name ^ " workspace vector shape mismatch");
  require
    (Array.length vector.workspace_values = len)
    (name ^ " workspace vector value length mismatch");
  require
    (String.equal (hash_f64 vector.workspace_values) vector.workspace_values_sha256)
    (name ^ " workspace vector values hash mismatch")

let run_layer0_full
    ?(verify = true)
    ?qkv
    ?attn_gate
    ?ssm_beta
    ?ssm_alpha
    plan
    activation
    wrapper
    ssm_out
    post_attn_vector
    ffn_gate
    ffn_up
    ffn_down_input_vector
    ffn_down
    layer0_output_vector =
  require
    (activation.token_position = 0)
    "layer0 full harness currently expects first-token zero-state activation";
  let started = Unix.gettimeofday () in
  let front_projection_elapsed_ms = ref 0 in
  let front_projection_count = ref 0 in
  let wrapper_state, frontier =
    match qkv, attn_gate, ssm_beta, ssm_alpha with
    | Some qkv, Some attn_gate, Some ssm_beta, Some ssm_alpha ->
        let state =
          create_harness_state
            "octBonsaiLayer0IntegratedHarnessCaller"
            "octBonsaiLayer0IntegratedHarnessContract"
        in
        let lhs_addr = 2_900_000 in
        set_f64_array state.VM.memory.data lhs_addr activation.activation;
        let qkv_run =
          run_source_backed_projection_into_state
            ~verify
            plan
            state
            ~label:"layer0 qkv"
            ~lhs_addr
            ~dst_addr:100_000
            qkv
        in
        let gate_run =
          run_source_backed_projection_into_state
            ~verify
            plan
            state
            ~label:"layer0 attn_gate"
            ~lhs_addr
            ~dst_addr:200_000
            attn_gate
        in
        let beta_run =
          run_source_backed_projection_into_state
            ~verify
            plan
            state
            ~label:"layer0 ssm_beta"
            ~lhs_addr
            ~dst_addr:300_000
            ssm_beta
        in
        let alpha_run =
          run_source_backed_projection_into_state
            ~verify
            plan
            state
            ~label:"layer0 ssm_alpha"
            ~lhs_addr
            ~dst_addr:301_000
            ssm_alpha
        in
        front_projection_elapsed_ms :=
          qkv_run.projection_elapsed_ms
          + gate_run.projection_elapsed_ms
          + beta_run.projection_elapsed_ms
          + alpha_run.projection_elapsed_ms;
        front_projection_count := 4;
        ( state,
          Some
            {
              frontier_qkv_addr = 100_000;
              frontier_z_gate_addr = 200_000;
              frontier_beta_addr = 300_000;
              frontier_alpha_addr = 301_000;
              frontier_qkv_sha256 = qkv.bundle_expected_output_sha256;
              frontier_z_gate_sha256 = attn_gate.bundle_expected_output_sha256;
              frontier_beta_sha256 = ssm_beta.bundle_expected_output_sha256;
              frontier_alpha_sha256 = ssm_alpha.bundle_expected_output_sha256;
            } )
    | None, None, None, None -> (create_harness_state "octBonsaiLayer0WrapperHarnessCaller" "octBonsaiLayer0WrapperHarnessContract", None)
    | _ ->
        failwith
          "layer0 full front projections require all of --qkv, --attn-gate, --ssm-beta, and --ssm-alpha"
  in
  let wrapper_result = execute_layer0_wrapper ~verify ~state:wrapper_state ?frontier wrapper in
  let state = wrapper_result.wrapper_state in
  let hidden_dim = wrapper_result.wrapper_addresses.hidden_dim in
  require
    (Array.length activation.activation = hidden_dim)
    "activation length does not match wrapper hidden dimension";
  let activation_addr = 3_000_000 in
  let ssm_out_addr = 3_100_000 in
  let post_attn_addr = 3_200_000 in
  let post_attention_norm_addr = 3_300_000 in
  let ffn_gate_addr = 3_400_000 in
  let ffn_up_addr = 3_500_000 in
  let ffn_down_addr = 3_700_000 in
  let layer0_output_addr = 3_800_000 in
  set_f64_array state.VM.memory.data activation_addr activation.activation;
  let ssm_out_projection =
    run_source_backed_projection_into_state
      plan
      state
      ~label:"layer0 ssm_out"
      ~lhs_addr:wrapper_result.wrapper_addresses.recurrent_output_addr
      ~dst_addr:ssm_out_addr
      ~verify
      ssm_out
  in
  set_f64_array state.VM.memory.data post_attn_addr activation.activation;
  set_int state 0 post_attn_addr;
  set_int state 1 ssm_out_addr;
  set_int state 2 hidden_dim;
  let residual_1_bytecode =
    run_vm_program state [| VM.RESIDUAL_ADD_FP (0, 1, 2); VM.STOP |] "layer0 residual ssm_out"
  in
  let load_norm_bytecode =
    load_f32_binding_to_memory
      plan
      state
      "post_attention_norm"
      post_attention_norm_addr
      hidden_dim
      "layer0 post_attention_norm load"
  in
  VM.setr state 14 (VM.VInt (fp64_to_z wrapper.epsilon));
  set_int state 0 post_attn_addr;
  set_int state 1 hidden_dim;
  set_int state 2 post_attention_norm_addr;
  let post_norm_bytecode =
    run_vm_program
      state
      [| VM.RMSNORM_FP_EPS (0, 1, 2, 14); VM.STOP |]
      "layer0 post_attention rmsnorm"
  in
  if verify then
    require_workspace_vector post_attn_vector "blk.0.post_attention_norm.input" hidden_dim;
  let post_attn_delta =
    if verify then
      assert_memory_close_atol
        ~atol:1e-8
        state
        post_attn_addr
        post_attn_vector.workspace_values
        "layer0 post_attention_norm"
    else 0.0
  in
  let ffn_gate_projection =
    run_source_backed_projection_into_state
      plan
      state
      ~label:"layer0 ffn_gate"
      ~lhs_addr:post_attn_addr
      ~dst_addr:ffn_gate_addr
      ~verify
      ffn_gate
  in
  let ffn_up_projection =
    run_source_backed_projection_into_state
      plan
      state
      ~label:"layer0 ffn_up"
      ~lhs_addr:post_attn_addr
      ~dst_addr:ffn_up_addr
      ~verify
      ffn_up
  in
  set_int state 0 ffn_gate_addr;
  set_int state 1 ffn_gate.bundle_output_rows;
  let silu_bytecode =
    run_vm_program state [| VM.SILU_FP (0, 1); VM.STOP |] "layer0 ffn gate silu"
  in
  set_int state 0 ffn_gate_addr;
  set_int state 1 ffn_up_addr;
  set_int state 2 ffn_gate.bundle_output_rows;
  let swiglu_bytecode =
    run_vm_program state [| VM.ELEMWISE_MUL_FP (0, 1, 2); VM.STOP |] "layer0 ffn swiglu"
  in
  if verify then
    require_workspace_vector ffn_down_input_vector "blk.0.ffn_down.input" ffn_gate.bundle_output_rows;
  let ffn_down_input_delta =
    if verify then
      assert_memory_close_atol
        ~atol:1e-8
        state
        ffn_gate_addr
        ffn_down_input_vector.workspace_values
        "layer0 ffn_down input"
    else 0.0
  in
  let ffn_down_projection =
    run_source_backed_projection_into_state
      plan
      state
      ~label:"layer0 ffn_down"
      ~lhs_addr:ffn_gate_addr
      ~dst_addr:ffn_down_addr
      ~verify
      ffn_down
  in
  set_f64_array state.VM.memory.data layer0_output_addr activation.activation;
  set_int state 0 layer0_output_addr;
  set_int state 1 ssm_out_addr;
  set_int state 2 hidden_dim;
  let residual_2_bytecode =
    run_vm_program state [| VM.RESIDUAL_ADD_FP (0, 1, 2); VM.STOP |] "layer0 output residual ssm"
  in
  set_int state 0 layer0_output_addr;
  set_int state 1 ffn_down_addr;
  set_int state 2 hidden_dim;
  let residual_3_bytecode =
    run_vm_program state [| VM.RESIDUAL_ADD_FP (0, 1, 2); VM.STOP |] "layer0 output residual ffn"
  in
  if verify then
    require_workspace_vector layer0_output_vector "blk.0.output" hidden_dim;
  let layer0_output_delta =
    if verify then
      assert_memory_close_atol
        ~atol:1e-8
        state
        layer0_output_addr
        layer0_output_vector.workspace_values
        "layer0 output"
    else 0.0
  in
  let layer0_output_sha256 =
    if verify then hash_f64 (memory_f64 state layer0_output_addr hidden_dim) else "-"
  in
  let total_elapsed_ms = int_of_float ((Unix.gettimeofday () -. started) *. 1000.0) in
  let extra_bytecode =
    residual_1_bytecode
    + load_norm_bytecode
    + post_norm_bytecode
    + silu_bytecode
    + swiglu_bytecode
    + residual_2_bytecode
    + residual_3_bytecode
  in
  let projection_elapsed_ms =
    ssm_out_projection.projection_elapsed_ms
    + ffn_gate_projection.projection_elapsed_ms
    + ffn_up_projection.projection_elapsed_ms
    + ffn_down_projection.projection_elapsed_ms
  in
  Printf.printf
    "bonsai_layer0_full_vm_harness ok model=%s source_sha256=%s token_id=%d token_position=%d hidden_dim=%d front_projection_count=%d front_projection_elapsed_ms=%d wrapper_elapsed_ms=%d projection_elapsed_ms=%d total_elapsed_ms=%d effort=%d wrapper_bytecode_size_total=%d extra_bytecode_size_total=%d ssm_out_sha256=%s ssm_out_reference_sha256=%s ssm_out_max_abs_delta=%.17g post_attn_max_abs_delta=%.17g ffn_gate_sha256=%s ffn_gate_reference_sha256=%s ffn_gate_max_abs_delta=%.17g ffn_up_sha256=%s ffn_up_reference_sha256=%s ffn_up_max_abs_delta=%.17g ffn_down_input_max_abs_delta=%.17g ffn_down_sha256=%s ffn_down_reference_sha256=%s ffn_down_max_abs_delta=%.17g layer0_output_sha256=%s layer0_output_reference_sha256=%s layer0_output_max_abs_delta=%.17g verification=%s comparison=%s atol=1e-8 rtol=0 numeric_profile=octra-vm-qwen35-layer0-full-candidate\n%!"
    (Option.value plan.model_name ~default:"-")
    (Option.value plan.source_sha256 ~default:"-")
    activation.token_id
    activation.token_position
    hidden_dim
    !front_projection_count
    !front_projection_elapsed_ms
    wrapper_result.wrapper_elapsed_ms
    projection_elapsed_ms
    total_elapsed_ms
    state.VM.effort_used
    wrapper_result.wrapper_bytecode_size_total
    extra_bytecode
    ssm_out_projection.projection_output_sha256
    ssm_out_projection.projection_reference_sha256
    ssm_out_projection.projection_max_abs_delta
    post_attn_delta
    ffn_gate_projection.projection_output_sha256
    ffn_gate_projection.projection_reference_sha256
    ffn_gate_projection.projection_max_abs_delta
    ffn_up_projection.projection_output_sha256
    ffn_up_projection.projection_reference_sha256
    ffn_up_projection.projection_max_abs_delta
    ffn_down_input_delta
    ffn_down_projection.projection_output_sha256
    ffn_down_projection.projection_reference_sha256
    ffn_down_projection.projection_max_abs_delta
    layer0_output_sha256
    layer0_output_vector.workspace_values_sha256
    layer0_output_delta
    (if verify then "enabled" else "benchmark_disabled")
    (if verify then "tolerance_bounded" else "not_checked")

let qwen35_hidden_dim = 5120
let qwen35_ffn_dim = 17408
let qwen35_recurrent_qkv = 10240
let qwen35_recurrent_q_heads = 16
let qwen35_recurrent_k_heads = 16
let qwen35_recurrent_v_heads = 48
let qwen35_recurrent_state = 128
let qwen35_recurrent_inner = 6144
let qwen35_recurrent_kernel = 4
let qwen35_recurrent_q_offset = 0
let qwen35_recurrent_k_offset = 2048
let qwen35_recurrent_v_offset = 4096
let qwen35_recurrent_q_elements = qwen35_recurrent_k_offset - qwen35_recurrent_q_offset
let qwen35_recurrent_k_elements = qwen35_recurrent_v_offset - qwen35_recurrent_k_offset
let qwen35_full_q_heads = 24
let qwen35_full_kv_heads = 4
let qwen35_full_head_dim = 256
let qwen35_full_q_elements = qwen35_full_q_heads * qwen35_full_head_dim
let qwen35_full_kv_elements = qwen35_full_kv_heads * qwen35_full_head_dim

type all_layer_addresses = {
  hidden : int;
  norm : int;
  gamma : int;
  q_norm : int;
  k_norm : int;
  qkv : int;
  z_gate : int;
  beta : int;
  gate : int;
  dt_bias : int;
  ssm_a : int;
  conv_kernel : int;
  conv_output : int;
  zero_conv_state : int;
  conv_state_dst : int;
  zero_recurrent_state : int;
  recurrent_output : int;
  recurrent_state_dst : int;
  ssm_norm : int;
  mixer : int;
  post_norm : int;
  ffn_gate : int;
  ffn_up : int;
  ffn_down : int;
  q_gate : int;
  full_q : int;
  full_k : int;
  full_v : int;
  full_ctx : int;
  full_gate : int;
  logits : int;
  conv_combined : int;
  conv_combined_output : int;
}

let all_layer_addresses =
  {
    hidden = 100_000;
    norm = 120_000;
    gamma = 140_000;
    q_norm = 150_000;
    k_norm = 151_000;
    qkv = 200_000;
    z_gate = 220_000;
    beta = 230_000;
    gate = 231_000;
    dt_bias = 232_000;
    ssm_a = 233_000;
    conv_kernel = 240_000;
    conv_output = 300_000;
    zero_conv_state = 340_000;
    conv_state_dst = 380_000;
    zero_recurrent_state = 2_000_000;
    recurrent_output = 1_240_000;
    recurrent_state_dst = 2_800_000;
    ssm_norm = 1_260_000;
    mixer = 1_280_000;
    post_norm = 1_300_000;
    ffn_gate = 1_320_000;
    ffn_up = 1_340_000;
    ffn_down = 1_370_000;
    q_gate = 1_390_000;
    full_q = 1_410_000;
    full_k = 1_420_000;
    full_v = 1_430_000;
    full_ctx = 1_440_000;
    full_gate = 1_450_000;
    logits = 1_500_000;
    conv_combined = 1_600_000;
    conv_combined_output = 1_700_000;
  }

let session_recurrent_base = 10_000_000
let session_recurrent_stride = 900_000
let session_attention_base = 80_000_000
let session_attention_stride = 600_000
let session_max_context_tokens = 256

let session_conv_state_addr layer_index =
  session_recurrent_base + (layer_index * session_recurrent_stride)

let session_recurrent_state_addr layer_index =
  session_conv_state_addr layer_index + 40_000

let session_k_cache_addr layer_index =
  session_attention_base + (layer_index * session_attention_stride)

let session_v_cache_addr layer_index =
  session_k_cache_addr layer_index + 300_000

let session_conv_state_elements =
  qwen35_recurrent_qkv * (qwen35_recurrent_kernel - 1)

let session_recurrent_state_elements =
  qwen35_recurrent_v_heads * qwen35_recurrent_state * qwen35_recurrent_state

let session_attention_token_elements =
  qwen35_full_kv_heads * qwen35_full_head_dim

let session_state_root (plan : execution_plan) state processed_tokens =
  require
    (processed_tokens > 0 && processed_tokens <= session_max_context_tokens)
    "session checkpoint token count is out of range";
  let fields = Buffer.create 4096 in
  Buffer.add_string fields "octra-inference/bonsai-session-checkpoint/1\n";
  Buffer.add_string fields (Printf.sprintf "processed_tokens=%d\n" processed_tokens);
  List.iter
    (fun layer ->
      match layer.layer_kind with
      | "recurrent" ->
          let conv =
            memory_f64
              state
              (session_conv_state_addr layer.layer_index)
              session_conv_state_elements
          in
          let recurrent =
            memory_f64
              state
              (session_recurrent_state_addr layer.layer_index)
              session_recurrent_state_elements
          in
          Buffer.add_string
            fields
            (Printf.sprintf
               "layer=%d kind=recurrent conv=%s recurrent=%s\n"
               layer.layer_index
               (hash_f64 conv)
               (hash_f64 recurrent))
      | "full_attention" ->
          let populated = processed_tokens * session_attention_token_elements in
          let k_cache = memory_f64 state (session_k_cache_addr layer.layer_index) populated in
          let v_cache = memory_f64 state (session_v_cache_addr layer.layer_index) populated in
          Buffer.add_string
            fields
            (Printf.sprintf
               "layer=%d kind=full_attention k=%s v=%s\n"
               layer.layer_index
               (hash_f64 k_cache)
               (hash_f64 v_cache))
      | other -> failwith ("unsupported qwen35 layer kind: " ^ other))
    plan.layers;
  sha256_hex (Buffer.contents fields)

let restart_session_state (plan : execution_plan) state processed_tokens =
  let before = session_state_root plan state processed_tokens in
  let restored =
    create_harness_state
      ~limit:max_int
      "octBonsaiSessionHarnessCaller"
      "octBonsaiSessionHarnessContract"
  in
  List.iter
    (fun layer ->
      match layer.layer_kind with
      | "recurrent" ->
          let values =
            memory_f64
              state
              (session_conv_state_addr layer.layer_index)
              session_conv_state_elements
          in
          Array.iteri
            (fun j value ->
              VM.mem_set_fp64
                restored.VM.memory.data
                (session_conv_state_addr layer.layer_index + j)
                value)
            values;
          let values2 =
            memory_f64
              state
              (session_recurrent_state_addr layer.layer_index)
              session_recurrent_state_elements
          in
          Array.iteri
            (fun j value ->
              VM.mem_set_fp64
                restored.VM.memory.data
                (session_recurrent_state_addr layer.layer_index + j)
                value)
            values2;
      | "full_attention" ->
          let populated = processed_tokens * session_attention_token_elements in
          set_f64_array_data restored.VM.memory.data (session_k_cache_addr layer.layer_index)
            (memory_f64 state (session_k_cache_addr layer.layer_index) populated);
          set_f64_array_data restored.VM.memory.data (session_v_cache_addr layer.layer_index)
            (memory_f64 state (session_v_cache_addr layer.layer_index) populated)
      | other -> failwith ("unsupported qwen35 layer kind: " ^ other))
    plan.layers;
  let after = session_state_root plan restored processed_tokens in
  require
    (String.equal before after)
    (Printf.sprintf
       "session checkpoint root mismatch after restart: before=%s after=%s"
       before
       after);
  restored, before

let product values = List.fold_left ( * ) 1 values

let q1_row_bytes k =
  require (k mod q1_group_size = 0) "Q1 tensor k is not group aligned";
  (k / q1_group_size) * q1_block_bytes

let binding_q1_shape (binding : plan_binding) label =
  match binding.dimensions with
  | [ k; rows ] -> k, rows
  | _ -> failwith (label ^ " binding is not a Q1 matrix")

let chunk_rows_for row_bytes rows =
  max 1 (min rows (VM.max_storage_value_len / row_bytes))

let fp16_le_bits raw off =
  let low = Char.code raw.[off] in
  let high = Char.code raw.[off + 1] in
  (high lsl 8) lor low

let fp16_le_to_f64 raw off =
  let bits = fp16_le_bits raw off in
  let sign = if bits land 0x8000 <> 0 then -1.0 else 1.0 in
  let exponent = (bits lsr 10) land 0x1f in
  let fraction = bits land 0x03ff in
  if exponent = 0 then
    if fraction = 0 then 0.0
    else begin
      let top = ref 0 in
      for bit = 1 to 9 do
        if fraction land (1 lsl bit) <> 0 then top := bit
      done;
      sign *. (float_of_int fraction /. float_of_int (1 lsl !top))
              *. 2.0 ** float_of_int (!top - 15)
    end
  else if exponent = 0x1f then nan
  else sign *. (1.0 +. float_of_int fraction /. 1024.0)
               *. 2.0 ** float_of_int (exponent - 15)

let decode_q1_row raw =
  let blocks = String.length raw / 18 in
  Array.init (blocks * 128) (fun i ->
    let block = i / 128 in
    let item = i mod 128 in
    let scale = fp16_le_to_f64 raw (block * 18) in
    let sign_byte = Char.code raw.[(block * 18) + 2 + (item lsr 3)] in
    let positive = (sign_byte lsr (item land 7)) land 1 = 1 in
    if positive then scale else -.scale)

let run_q1_gather_row (plan : execution_plan) state (binding : plan_binding) row_index dst_addr label =
  let k, rows = binding_q1_shape binding label in
  require (row_index >= 0 && row_index < rows) (label ^ " row index out of range");
  let row_bytes = q1_row_bytes k in
  let raw = read_binding_span plan binding (row_index * row_bytes) row_bytes label in
  let values = decode_q1_row raw in
  set_f64_array state.VM.memory.data dst_addr values

let run_q1_projection_binding (plan : execution_plan) state (binding : plan_binding) lhs_addr dst_addr label =
  let k, rows = binding_q1_shape binding label in
  let bytes = require_binding_bytes binding label in
  let row_bytes = q1_row_bytes k in
  require (bytes = rows * row_bytes) (label ^ " Q1 byte span does not match dimensions");
  let rows_per_chunk = chunk_rows_for row_bytes rows in
  let program = [| VM.LINEAR_Q1_G128_FP (0, 1, 2, 3, 4, 5, 6); VM.STOP |] in
  begin
    match VM.Verifier.verify program with
    | Ok () -> ()
    | Error _ -> failwith ("verifier rejected " ^ label)
  end;
  let decoded = Bytecode.decode_exn (Bytecode.encode program) in
  let started = Unix.gettimeofday () in
  incr profile_q1_projection_count;
  let chunk_count = ref 0 in
  let row_start = ref 0 in
  while !row_start < rows do
    let chunk_n = min rows_per_chunk (rows - !row_start) in
    let span_started = Unix.gettimeofday () in
    let raw =
      read_binding_span
        plan
        binding
        (!row_start * row_bytes)
        (chunk_n * row_bytes)
        label
    in
    profile_q1_span_read_ms :=
      !profile_q1_span_read_ms
      + int_of_float ((Unix.gettimeofday () -. span_started) *. 1000.0);
    set_int state 0 (dst_addr + !row_start);
    set_int state 1 lhs_addr;
    VM.setr state 2 (VM.VBytes raw);
    set_int state 3 0;
    set_int state 4 1;
    set_int state 5 k;
    set_int state 6 chunk_n;
    state.VM.pc <- 0;
    let run_started = Unix.gettimeofday () in
    let ok = VM.run state decoded in
    profile_q1_vm_run_ms :=
      !profile_q1_vm_run_ms
      + int_of_float ((Unix.gettimeofday () -. run_started) *. 1000.0);
    require ok (label ^ " VM execution returned false");
    require (not state.VM.reverted) (label ^ " VM execution reverted");
    incr chunk_count;
    row_start := !row_start + chunk_n
  done;
  (!chunk_count, int_of_float ((Unix.gettimeofday () -. started) *. 1000.0))

let load_f32_binding_to_addr (plan : execution_plan) state (binding : plan_binding) dst_addr expected_elements label =
  require
    (product binding.dimensions = expected_elements)
    (label ^ " F32 element count does not match expected shape");
  let bytes = require_binding_bytes binding label in
  require (bytes = expected_elements * 4) (label ^ " F32 byte length mismatch");
  let raw = read_binding_span plan binding 0 bytes label in
  require (String.length raw <= VM.max_storage_value_len) (label ^ " F32 span exceeds VM value limit");
  set_int state 0 dst_addr;
  VM.setr state 1 (VM.VBytes raw);
  set_int state 2 0;
  set_int state 3 expected_elements;
  run_vm_program state [| VM.LOAD_F32_LE_FP (0, 1, 2, 3); VM.STOP |] label

let load_f32_name (plan : execution_plan) state name dst_addr expected_elements =
  let started = Unix.gettimeofday () in
  load_f32_binding_to_addr
    plan
    state
    (plan_binding_by_name plan name)
    dst_addr
    expected_elements
    name;
  profile_load_f32_elapsed_ms :=
    !profile_load_f32_elapsed_ms
    + int_of_float ((Unix.gettimeofday () -. started) *. 1000.0)

let copy_memory state src dst len =
  set_f64_array_data state.VM.memory.data dst (get_f64_array_data state.VM.memory.data src len)

let run_unary state addr n op label =
  set_int state 0 addr;
  set_int state 1 n;
  run_vm_program state [| op; VM.STOP |] label

let run_rmsnorm state addr n gamma_addr epsilon label =
  set_int state 0 addr;
  set_int state 1 n;
  set_int state 2 gamma_addr;
  VM.setr state 14 (VM.VInt (fp64_to_z epsilon));
  run_vm_program state [| VM.RMSNORM_FP_EPS (0, 1, 2, 14); VM.STOP |] label

let run_residual_add state dst src n label =
  set_int state 0 dst;
  set_int state 1 src;
  set_int state 2 n;
  run_vm_program state [| VM.RESIDUAL_ADD_FP (0, 1, 2); VM.STOP |] label

let run_elemwise_mul state dst src n label =
  set_int state 0 dst;
  set_int state 1 src;
  set_int state 2 n;
  run_vm_program state [| VM.ELEMWISE_MUL_FP (0, 1, 2); VM.STOP |] label

let run_recurrent_layer ?layer0_wrapper ?(stateful = false) (plan : execution_plan) state addrs layer_index epsilon =
  let prefix = Printf.sprintf "blk.%d" layer_index in
  let conv_state_addr =
    if stateful then session_conv_state_addr layer_index else addrs.zero_conv_state
  in
  let conv_state_dst_addr =
    if stateful then session_conv_state_addr layer_index else addrs.conv_state_dst
  in
  let recurrent_state_addr =
    if stateful then session_recurrent_state_addr layer_index else addrs.zero_recurrent_state
  in
  let recurrent_state_dst_addr =
    if stateful then session_recurrent_state_addr layer_index else addrs.recurrent_state_dst
  in
  ignore (load_f32_name plan state (prefix ^ ".ssm_conv1d.weight") addrs.conv_kernel (qwen35_recurrent_qkv * qwen35_recurrent_kernel));
  ignore (load_f32_name plan state (prefix ^ ".ssm_dt.bias") addrs.dt_bias qwen35_recurrent_v_heads);
  ignore (load_f32_name plan state (prefix ^ ".ssm_a") addrs.ssm_a qwen35_recurrent_v_heads);
  ignore (load_f32_name plan state (prefix ^ ".ssm_norm.weight") addrs.ssm_norm qwen35_recurrent_state);
  ignore (run_q1_projection_binding plan state (plan_binding_by_name plan (prefix ^ ".attn_qkv.weight")) addrs.norm addrs.qkv (prefix ^ ".attn_qkv"));
  ignore (run_q1_projection_binding plan state (plan_binding_by_name plan (prefix ^ ".attn_gate.weight")) addrs.norm addrs.z_gate (prefix ^ ".attn_gate"));
  ignore (run_q1_projection_binding plan state (plan_binding_by_name plan (prefix ^ ".ssm_beta.weight")) addrs.norm addrs.beta (prefix ^ ".ssm_beta"));
  ignore (run_q1_projection_binding plan state (plan_binding_by_name plan (prefix ^ ".ssm_alpha.weight")) addrs.norm addrs.gate (prefix ^ ".ssm_alpha"));
  let qkv_hash = hash_f64 (memory_f64 state addrs.qkv qwen35_recurrent_qkv) in
  let z_gate_hash = hash_f64 (memory_f64 state addrs.z_gate qwen35_recurrent_inner) in
  let beta_logits_hash = hash_f64 (memory_f64 state addrs.beta qwen35_recurrent_v_heads) in
  let alpha_logits_hash = hash_f64 (memory_f64 state addrs.gate qwen35_recurrent_v_heads) in
  begin
    match layer0_wrapper with
    | Some wrapper when layer_index = 0 ->
        let print_stage name actual expected =
          let stats = compare_arrays actual expected name in
          Printf.printf
            "layer0_wrapper_input_conformance name=%s max_abs_delta=%.17g max_rel_delta=%.17g rms_delta=%.17g worst_index=%d actual_non_finite=%d expected_non_finite=%d comparison=numeric\n%!"
            name
            stats.max_abs_delta
            stats.max_rel_delta
            stats.rms_delta
            stats.worst_index
            stats.actual_non_finite
            stats.expected_non_finite
        in
        print_stage "qkv" (memory_f64 state addrs.qkv qwen35_recurrent_qkv) wrapper.inputs.qkv;
        print_stage
          "z_gate"
          (memory_f64 state addrs.z_gate qwen35_recurrent_inner)
          wrapper.inputs.z_gate;
        print_stage
          "beta_logits"
          (memory_f64 state addrs.beta qwen35_recurrent_v_heads)
          wrapper.inputs.beta_logits;
        print_stage
          "alpha_logits"
          (memory_f64 state addrs.gate qwen35_recurrent_v_heads)
          wrapper.inputs.alpha_logits
    | _ -> ()
  end;
  let conv_kernel = qwen35_recurrent_kernel in
  let conv_channels = qwen35_recurrent_qkv in
  let combined = addrs.conv_combined in
  let combined_out = addrs.conv_combined_output in
  copy_memory state conv_state_addr combined (conv_channels * (conv_kernel - 1));
  copy_memory state addrs.qkv (combined + (conv_channels * (conv_kernel - 1))) conv_channels;
  set_int state 0 combined_out;
  set_int state 1 combined;
  set_int state 2 addrs.conv_kernel;
  set_int state 3 conv_kernel;
  set_int state 4 conv_channels;
  set_int state 5 conv_kernel;
  ignore
    (run_vm_program
       state
       [| VM.CAUSAL_DEPTHWISE_CONV1D_FP (0, 1, 2, 3, 4, 5); VM.STOP |]
       (prefix ^ ".ssm_conv"));
  copy_memory
    state
    (combined_out + (conv_channels * (conv_kernel - 1)))
    addrs.conv_output
    conv_channels;
  copy_memory state (combined + conv_channels) conv_state_dst_addr (conv_channels * (conv_kernel - 1));
  ignore
    (run_unary
       state
       addrs.conv_output
       conv_channels
       (VM.SILU_FP (0, 1))
       (prefix ^ ".ssm_conv_silu"));
  let conv_output_hash = hash_f64 (memory_f64 state addrs.conv_output qwen35_recurrent_qkv) in
  VM.setr state 14 (VM.VInt (fp64_to_z 1e-6));
  for head = 0 to qwen35_recurrent_q_heads - 1 do
    set_int state 0 (addrs.conv_output + qwen35_recurrent_q_offset + (head * qwen35_recurrent_state));
    set_int state 1 qwen35_recurrent_state;
    ignore (run_vm_program state [| VM.L2NORM_FP (0, 1, 14); VM.STOP |] (prefix ^ ".q_l2norm"))
  done;
  for head = 0 to qwen35_recurrent_k_heads - 1 do
    set_int state 0 (addrs.conv_output + qwen35_recurrent_k_offset + (head * qwen35_recurrent_state));
    set_int state 1 qwen35_recurrent_state;
    ignore (run_vm_program state [| VM.L2NORM_FP (0, 1, 14); VM.STOP |] (prefix ^ ".k_l2norm"))
  done;
  let q_l2_hash =
    hash_f64
      (memory_f64
         state
         (addrs.conv_output + qwen35_recurrent_q_offset)
         qwen35_recurrent_q_elements)
  in
  let k_l2_hash =
    hash_f64
      (memory_f64
         state
         (addrs.conv_output + qwen35_recurrent_k_offset)
         qwen35_recurrent_k_elements)
  in
  ignore (run_unary state addrs.beta qwen35_recurrent_v_heads (VM.SIGMOID_FP (0, 1)) (prefix ^ ".beta_sigmoid"));
  ignore (run_residual_add state addrs.gate addrs.dt_bias qwen35_recurrent_v_heads (prefix ^ ".alpha_plus_dt"));
  ignore (run_unary state addrs.gate qwen35_recurrent_v_heads (VM.SOFTPLUS_FP (0, 1)) (prefix ^ ".gate_softplus"));
  ignore (run_elemwise_mul state addrs.gate addrs.ssm_a qwen35_recurrent_v_heads (prefix ^ ".gate_mul_a"));
  let beta_hash = hash_f64 (memory_f64 state addrs.beta qwen35_recurrent_v_heads) in
  let gate_hash = hash_f64 (memory_f64 state addrs.gate qwen35_recurrent_v_heads) in
  set_int state 0 addrs.recurrent_output;
  set_int state 1 recurrent_state_dst_addr;
  set_int state 2 (addrs.conv_output + qwen35_recurrent_q_offset);
  set_int state 3 (addrs.conv_output + qwen35_recurrent_k_offset);
  set_int state 4 (addrs.conv_output + qwen35_recurrent_v_offset);
  set_int state 5 addrs.gate;
  set_int state 6 addrs.beta;
  set_int state 7 recurrent_state_addr;
  set_int state 8 1;
  set_int state 9 qwen35_recurrent_q_heads;
  set_int state 10 qwen35_recurrent_k_heads;
  set_int state 11 qwen35_recurrent_v_heads;
  set_int state 12 qwen35_recurrent_state;
  set_int state 13 qwen35_recurrent_state;
  ignore (run_vm_program state [| VM.GATED_DELTA_RULE_FP (0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13); VM.STOP |] (prefix ^ ".gated_delta_net"));
  let recurrent_output_hash =
    hash_f64 (memory_f64 state addrs.recurrent_output qwen35_recurrent_inner)
  in
  for head = 0 to qwen35_recurrent_v_heads - 1 do
    ignore
      (run_rmsnorm
         state
         (addrs.recurrent_output + (head * qwen35_recurrent_state))
         qwen35_recurrent_state
         addrs.ssm_norm
         epsilon
         (prefix ^ ".ssm_rmsnorm"))
  done;
  ignore (run_unary state addrs.z_gate qwen35_recurrent_inner (VM.SILU_FP (0, 1)) (prefix ^ ".z_silu"));
  ignore (run_elemwise_mul state addrs.recurrent_output addrs.z_gate qwen35_recurrent_inner (prefix ^ ".ssm_gate"));
  let gated_norm_hash =
    hash_f64 (memory_f64 state addrs.recurrent_output qwen35_recurrent_inner)
  in
  begin
    match layer0_wrapper with
    | Some wrapper when layer_index = 0 ->
        let stats =
          compare_arrays
            (memory_f64 state addrs.recurrent_output qwen35_recurrent_inner)
            wrapper.expected.ssm_out_lhs
            "ssm_out_lhs"
        in
        Printf.printf
          "layer0_wrapper_output_conformance name=ssm_out_lhs actual_sha256=%s reference_sha256=%s max_abs_delta=%.17g max_rel_delta=%.17g rms_delta=%.17g worst_index=%d actual_non_finite=%d expected_non_finite=%d comparison=numeric\n%!"
          gated_norm_hash
          wrapper.expected.gated_norm_output_sha256
          stats.max_abs_delta
          stats.max_rel_delta
          stats.rms_delta
          stats.worst_index
          stats.actual_non_finite
          stats.expected_non_finite
    | _ -> ()
  end;
  if layer_index = 0 then
    Printf.printf
      "layer0_recurrent_debug qkv_sha256=%s z_gate_sha256=%s beta_logits_sha256=%s alpha_logits_sha256=%s conv_output_sha256=%s q_l2_sha256=%s k_l2_sha256=%s beta_sha256=%s gate_sha256=%s recurrent_output_sha256=%s gated_norm_output_sha256=%s\n%!"
      qkv_hash
      z_gate_hash
      beta_logits_hash
      alpha_logits_hash
      conv_output_hash
      q_l2_hash
      k_l2_hash
      beta_hash
      gate_hash
      recurrent_output_hash
      gated_norm_hash;
  ignore (run_q1_projection_binding plan state (plan_binding_by_name plan (prefix ^ ".ssm_out.weight")) addrs.recurrent_output addrs.mixer (prefix ^ ".ssm_out"))

let split_q_gate_first_token state addrs =
  for head = 0 to qwen35_full_q_heads - 1 do
    let src = addrs.q_gate + (head * qwen35_full_head_dim * 2) in
    let q_dst = addrs.full_q + (head * qwen35_full_head_dim) in
    let gate_dst = addrs.full_gate + (head * qwen35_full_head_dim) in
    for i = 0 to qwen35_full_head_dim - 1 do
      set_fp64 state.VM.memory.data (q_dst + i) (get_fp64 state (src + i));
      set_fp64 state.VM.memory.data (gate_dst + i) (get_fp64 state (src + qwen35_full_head_dim + i))
    done
  done


let write_position_cells state positions_addr position pairs =
  for i = 0 to pairs - 1 do
    Hashtbl.replace state.VM.memory.data (positions_addr + i)
      (VM.VInt (Z.of_int position))
  done

let run_mrope_sections plan state addr heads positions_addr base_bits label =
  for head = 0 to heads - 1 do
    set_int state 0 (addr + (head * qwen35_full_head_dim));
    set_int state 1 qwen35_full_head_dim;
    set_int state 2 qwen35_full_head_dim;
    set_int state 3 plan.rope_dimension_count;
    set_int state 4 positions_addr;
    VM.setr state 5 (VM.VInt (fp64_to_z base_bits));
    ignore
      (run_vm_program
         state
         [| VM.ROPE_APPLY_INDEXED_FP (0, 1, 2, 3, 4, 5); VM.STOP |]
         (label ^ ".rope"))
  done

let run_attention_scores_head state q_addr k_addr v_addr ctx_addr scores_addr
    probs_addr key_count head_dim qh kvh _q_heads _kv_heads label =
  set_int state 0 scores_addr;
  set_int state 1 (q_addr + (qh * head_dim));
  set_int state 2 (k_addr + (kvh * head_dim));
  set_int state 3 key_count;
  set_int state 4 head_dim;
  ignore
    (run_vm_program
       state
       [| VM.ATTENTION_SCORES_FP (0, 1, 2, 3, 4); VM.STOP |]
       (label ^ ".scores"));
  set_int state 0 probs_addr;
  set_int state 1 scores_addr;
  set_int state 2 key_count;
  ignore
    (run_vm_program state [| VM.SOFTMAX_FP (0, 1, 2); VM.STOP |] (label ^ ".softmax"));
  set_int state 0 (ctx_addr + (qh * head_dim));
  set_int state 1 probs_addr;
  set_int state 2 (v_addr + (kvh * head_dim));
  set_int state 3 key_count;
  set_int state 4 head_dim;
  ignore
    (run_vm_program
       state
       [| VM.ATTENTION_WEIGHTED_SUM_FP (0, 1, 2, 3, 4); VM.STOP |]
       (label ^ ".weighted_sum"))

let run_full_attention_first_token_layer (plan : execution_plan) state addrs layer_index =
  let prefix = Printf.sprintf "blk.%d" layer_index in
  ignore (run_q1_projection_binding plan state (plan_binding_by_name plan (prefix ^ ".attn_q.weight")) addrs.norm addrs.q_gate (prefix ^ ".attn_q"));
  ignore (run_q1_projection_binding plan state (plan_binding_by_name plan (prefix ^ ".attn_k.weight")) addrs.norm addrs.full_k (prefix ^ ".attn_k"));
  ignore (run_q1_projection_binding plan state (plan_binding_by_name plan (prefix ^ ".attn_v.weight")) addrs.norm addrs.full_v (prefix ^ ".attn_v"));
  split_q_gate_first_token state addrs;
  ignore (run_rmsnorm state addrs.full_q qwen35_full_q_elements addrs.q_norm 1e-6 (prefix ^ ".attn_q_norm"));
  ignore (run_rmsnorm state addrs.full_k qwen35_full_kv_elements addrs.k_norm 1e-6 (prefix ^ ".attn_k_norm"));
  Hashtbl.replace state.VM.memory.data 4 (VM.VInt Z.zero);
  ignore (run_mrope_sections plan state addrs.full_q qwen35_full_q_heads 4 plan.rope_freq_base (prefix ^ ".attn_q_rope"));
  ignore (run_mrope_sections plan state addrs.full_k qwen35_full_kv_heads 4 plan.rope_freq_base (prefix ^ ".attn_k_rope"));
  set_int state 0 addrs.full_ctx;
  set_int state 1 addrs.full_q;
  set_int state 2 addrs.full_k;
  set_int state 3 1;
  set_int state 4 qwen35_full_head_dim;
  set_int state 5 addrs.full_ctx;
  set_int state 6 addrs.full_v;
  for kvh = 0 to qwen35_full_kv_heads - 1 do
    let group = qwen35_full_q_heads / qwen35_full_kv_heads in
    for g = 0 to group - 1 do
      let qh = (kvh * group) + g in
      ignore
        (run_attention_scores_head
           state addrs.full_q addrs.full_k addrs.full_v addrs.full_ctx
           addrs.full_ctx addrs.full_ctx 1 qwen35_full_head_dim qh kvh
           qwen35_full_q_heads qwen35_full_kv_heads (prefix ^ ".attn_t1"))
    done
  done;
  ignore (run_unary state addrs.full_gate qwen35_full_q_elements (VM.SIGMOID_FP (0, 1)) (prefix ^ ".attention_gate_sigmoid"));
  ignore (run_elemwise_mul state addrs.full_ctx addrs.full_gate qwen35_full_q_elements (prefix ^ ".attention_gate_mul"));
  ignore (run_q1_projection_binding plan state (plan_binding_by_name plan (prefix ^ ".attn_output.weight")) addrs.full_ctx addrs.mixer (prefix ^ ".attn_output"))

let run_full_attention_session_layer (plan : execution_plan) state addrs layer_index position =
  let prefix = Printf.sprintf "blk.%d" layer_index in
  require
    (Array.length plan.rope_dimension_sections = 4)
    "qwen35 plan rope_dimension_sections must contain four values";
  ignore (load_f32_name plan state (prefix ^ ".attn_q_norm.weight") addrs.q_norm qwen35_full_head_dim);
  ignore (load_f32_name plan state (prefix ^ ".attn_k_norm.weight") addrs.k_norm qwen35_full_head_dim);
  ignore (run_q1_projection_binding plan state (plan_binding_by_name plan (prefix ^ ".attn_q.weight")) addrs.norm addrs.q_gate (prefix ^ ".attn_q"));
  ignore (run_q1_projection_binding plan state (plan_binding_by_name plan (prefix ^ ".attn_k.weight")) addrs.norm addrs.full_k (prefix ^ ".attn_k"));
  ignore (run_q1_projection_binding plan state (plan_binding_by_name plan (prefix ^ ".attn_v.weight")) addrs.norm addrs.full_v (prefix ^ ".attn_v"));
  split_q_gate_first_token state addrs;
  write_position_cells state 4 0 (plan.rope_dimension_count / 2);
  for h = 0 to qwen35_full_q_heads - 1 do
    ignore
      (run_rmsnorm
         state
         (addrs.full_q + (h * qwen35_full_head_dim))
         qwen35_full_head_dim
         addrs.q_norm
         1e-6
         (prefix ^ ".attn_q_norm"))
  done;
  for h = 0 to qwen35_full_kv_heads - 1 do
    ignore
      (run_rmsnorm
         state
         (addrs.full_k + (h * qwen35_full_head_dim))
         qwen35_full_head_dim
         addrs.k_norm
         1e-6
         (prefix ^ ".attn_k_norm"))
  done;
  Hashtbl.replace state.VM.memory.data 4 (VM.VInt Z.zero);
  ignore (run_mrope_sections plan state addrs.full_q qwen35_full_q_heads 4 plan.rope_freq_base (prefix ^ ".attn_q_rope"));
  ignore (run_mrope_sections plan state addrs.full_k qwen35_full_kv_heads 4 plan.rope_freq_base (prefix ^ ".attn_k_rope"));
  let k_cache = session_k_cache_addr layer_index in
  let v_cache = session_v_cache_addr layer_index in
  let key_count = position + 1 in
  let kv_cells = qwen35_full_kv_heads * qwen35_full_head_dim in
  copy_memory state addrs.full_k (k_cache + (position * kv_cells)) kv_cells;
  copy_memory state addrs.full_v (v_cache + (position * kv_cells)) kv_cells;
  let scores_addr = addrs.conv_combined in
  let probs_addr = addrs.conv_combined_output in
  for kvh = 0 to qwen35_full_kv_heads - 1 do
    let group = qwen35_full_q_heads / qwen35_full_kv_heads in
    for g = 0 to group - 1 do
      let qh = (kvh * group) + g in
      ignore
        (run_attention_scores_head
           state addrs.full_q k_cache v_cache addrs.full_ctx
           scores_addr probs_addr key_count qwen35_full_head_dim qh kvh
           qwen35_full_q_heads qwen35_full_kv_heads (prefix ^ ".attn"))
    done
  done;
  ignore (run_unary state addrs.full_gate qwen35_full_q_elements (VM.SIGMOID_FP (0, 1)) (prefix ^ ".attention_gate_sigmoid"));
  ignore (run_elemwise_mul state addrs.full_ctx addrs.full_gate qwen35_full_q_elements (prefix ^ ".attention_gate_mul"));
  ignore (run_q1_projection_binding plan state (plan_binding_by_name plan (prefix ^ ".attn_output.weight")) addrs.full_ctx addrs.mixer (prefix ^ ".attn_output"))

let run_ffn_layer (plan : execution_plan) state addrs layer_index epsilon =
  let prefix = Printf.sprintf "blk.%d" layer_index in
  copy_memory state addrs.hidden addrs.post_norm qwen35_hidden_dim;
  ignore (load_f32_name plan state (prefix ^ ".post_attention_norm.weight") addrs.gamma qwen35_hidden_dim);
  ignore (run_rmsnorm state addrs.post_norm qwen35_hidden_dim addrs.gamma epsilon (prefix ^ ".post_attention_norm"));
  ignore (run_q1_projection_binding plan state (plan_binding_by_name plan (prefix ^ ".ffn_gate.weight")) addrs.post_norm addrs.ffn_gate (prefix ^ ".ffn_gate"));
  ignore (run_q1_projection_binding plan state (plan_binding_by_name plan (prefix ^ ".ffn_up.weight")) addrs.post_norm addrs.ffn_up (prefix ^ ".ffn_up"));
  ignore (run_unary state addrs.ffn_gate qwen35_ffn_dim (VM.SILU_FP (0, 1)) (prefix ^ ".ffn_gate_silu"));
  ignore (run_elemwise_mul state addrs.ffn_gate addrs.ffn_up qwen35_ffn_dim (prefix ^ ".ffn_down_lhs"));
  ignore (run_q1_projection_binding plan state (plan_binding_by_name plan (prefix ^ ".ffn_down.weight")) addrs.ffn_gate addrs.ffn_down (prefix ^ ".ffn_down"));
  let ffn_hash = hash_f64 (memory_f64 state addrs.ffn_down qwen35_hidden_dim) in
  ignore (run_residual_add state addrs.hidden addrs.ffn_down qwen35_hidden_dim (prefix ^ ".ffn_residual"));
  ffn_hash

let zero_session_states plan state =
  List.iter
    (fun layer ->
      if String.equal layer.layer_kind "recurrent" then begin
        let conv = session_conv_state_addr layer.layer_index in
        for i = 0 to session_conv_state_elements - 1 do
          VM.mem_set_fp64 state.VM.memory.data (conv + i) 0.0
        done;
        let recurrent = session_recurrent_state_addr layer.layer_index in
        for i = 0 to session_recurrent_state_elements - 1 do
          VM.mem_set_fp64 state.VM.memory.data (recurrent + i) 0.0
        done
      end)
    plan.layers

let run_session_forward_token (plan : execution_plan) state addrs position token_id =
  let forward_started = Unix.gettimeofday () in
  let gather_started = Unix.gettimeofday () in
  ignore
    (run_q1_gather_row
       plan
       state
       (plan_binding_by_name plan "token_embd.weight")
       token_id
       addrs.hidden
       "token_embd.weight");
  profile_gather_ms :=
    !profile_gather_ms
    + int_of_float ((Unix.gettimeofday () -. gather_started) *. 1000.0);
  List.iter
    (fun layer ->
      let layer_started = Unix.gettimeofday () in
      copy_memory state addrs.hidden addrs.norm qwen35_hidden_dim;
      ignore
        (load_f32_name
           plan
           state
           (Printf.sprintf "blk.%d.attn_norm.weight" layer.layer_index)
           addrs.gamma
           qwen35_hidden_dim);
      ignore
        (run_rmsnorm
           state
           addrs.norm
           qwen35_hidden_dim
           addrs.gamma
           1e-6
           (Printf.sprintf "blk.%d.attn_norm" layer.layer_index));
      profile_attention_norm_ms :=
        !profile_attention_norm_ms
        + int_of_float ((Unix.gettimeofday () -. layer_started) *. 1000.0);
      let kind_started = Unix.gettimeofday () in
      begin
        match layer.layer_kind with
        | "recurrent" ->
            run_recurrent_layer ~stateful:true plan state addrs layer.layer_index 1e-6
        | "full_attention" ->
            run_full_attention_session_layer plan state addrs layer.layer_index position
        | other -> failwith ("unsupported qwen35 layer kind: " ^ other)
      end;
      let layer_elapsed =
        int_of_float ((Unix.gettimeofday () -. kind_started) *. 1000.0)
      in
      if String.equal layer.layer_kind "recurrent" then
        profile_recurrent_layer_ms := !profile_recurrent_layer_ms + layer_elapsed
      else
        profile_full_attention_layer_ms :=
          !profile_full_attention_layer_ms + layer_elapsed;
      let mixer_started = Unix.gettimeofday () in
      ignore
        (run_residual_add
           state
           addrs.hidden
           addrs.mixer
           qwen35_hidden_dim
           (Printf.sprintf "blk.%d.mixer_residual" layer.layer_index));
      profile_mixer_residual_ms :=
        !profile_mixer_residual_ms
        + int_of_float ((Unix.gettimeofday () -. mixer_started) *. 1000.0);
      let ffn_started = Unix.gettimeofday () in
      ignore (run_ffn_layer plan state addrs layer.layer_index 1e-6);
      profile_ffn_layer_ms :=
        !profile_ffn_layer_ms
        + int_of_float ((Unix.gettimeofday () -. ffn_started) *. 1000.0))
    plan.layers;
  profile_forward_ms :=
    !profile_forward_ms
    + int_of_float ((Unix.gettimeofday () -. forward_started) *. 1000.0);
  profile_token_elapsed_ms :=
    !profile_token_elapsed_ms
    @ [ int_of_float ((Unix.gettimeofday () -. forward_started) *. 1000.0) ]

let run_session_lm_head (plan : execution_plan) state addrs =
  copy_memory state addrs.hidden addrs.norm qwen35_hidden_dim;
  ignore (load_f32_name plan state "output_norm.weight" addrs.gamma qwen35_hidden_dim);
  ignore (run_rmsnorm state addrs.norm qwen35_hidden_dim addrs.gamma 1e-6 "output_norm");
  let output_binding = plan_binding_by_name plan "output.weight" in
  let _, vocab_size = binding_q1_shape output_binding "output.weight" in
  let output_chunks, output_ms =
    run_q1_projection_binding plan state output_binding addrs.norm addrs.logits "output.weight"
  in
  set_int state 0 addrs.logits;
  set_int state 1 vocab_size;
  ignore (run_vm_program state [| VM.ARGMAX_FP (2, 0, 1); VM.STOP |] "output_argmax");
  let token =
    match VM.getr state 2 with
    | VM.VInt z -> Z.to_int z
    | _ -> failwith "ARGMAX_FP returned non-int"
  in
  token, output_chunks, output_ms, hash_f64 (memory_f64 state addrs.hidden qwen35_hidden_dim),
  hash_f64 (memory_f64 state addrs.norm qwen35_hidden_dim),
  hash_f64 (memory_f64 state addrs.logits vocab_size)


let parse_token_csv value =
  value
  |> String.split_on_char ','
  |> List.filter (fun part -> String.trim part <> "")
  |> List.map (fun part ->
    let token = int_of_string (String.trim part) in
    require (token >= 0) "prompt token ids must be nonnegative";
    token)
  |> Array.of_list

let string_of_int_array values =
  values |> Array.to_list |> List.map string_of_int |> String.concat ","

let verify_generation_fixture (plan : execution_plan) token_ids max_new_tokens fixture =
  let expected_source_sha256 =
    match !active_packed_model with
    | Some packed -> Some packed.packed_source_sha256
    | None -> plan.source_sha256
  in
  begin
    match expected_source_sha256, fixture.generation_source_sha256 with
    | Some expected, Some actual ->
        require
          (String.equal expected actual)
          "generation fixture source hash does not match execution plan"
    | Some _, None -> failwith "generation fixture is not source-hash-bound"
    | None, _ -> failwith "execution evidence has no source provenance"
  end;
  require
    (Array.length fixture.generation_prompt_token_ids = Array.length token_ids)
    "generation fixture prompt token count mismatch";
  Array.iteri
    (fun i token ->
      require
        (token = token_ids.(i))
        (Printf.sprintf
           "generation fixture prompt token mismatch at %d: actual=%d expected=%d"
           i
           token_ids.(i)
           token))
    fixture.generation_prompt_token_ids;
  require
    (Array.length fixture.generation_generated_token_ids >= max_new_tokens)
    "generation fixture does not contain enough generated tokens"

let zero_session_states plan state =
  List.iter
    (fun layer ->
      if String.equal layer.layer_kind "recurrent" then begin
        let conv = session_conv_state_addr layer.layer_index in
        for i = 0 to session_conv_state_elements - 1 do
          VM.mem_set_fp64 state.VM.memory.data (conv + i) 0.0
        done;
        let recurrent = session_recurrent_state_addr layer.layer_index in
        for i = 0 to session_recurrent_state_elements - 1 do
          VM.mem_set_fp64 state.VM.memory.data (recurrent + i) 0.0
        done
      end)
    plan.layers

let run_session_forward_token (plan : execution_plan) state addrs position token_id =
  let forward_started = Unix.gettimeofday () in
  let gather_started = Unix.gettimeofday () in
  ignore
    (run_q1_gather_row
       plan
       state
       (plan_binding_by_name plan "token_embd.weight")
       token_id
       addrs.hidden
       "token_embd.weight");
  profile_gather_ms :=
    !profile_gather_ms
    + int_of_float ((Unix.gettimeofday () -. gather_started) *. 1000.0);
  List.iter
    (fun layer ->
      let layer_started = Unix.gettimeofday () in
      copy_memory state addrs.hidden addrs.norm qwen35_hidden_dim;
      ignore
        (load_f32_name
           plan
           state
           (Printf.sprintf "blk.%d.attn_norm.weight" layer.layer_index)
           addrs.gamma
           qwen35_hidden_dim);
      ignore
        (run_rmsnorm
           state
           addrs.norm
           qwen35_hidden_dim
           addrs.gamma
           1e-6
           (Printf.sprintf "blk.%d.attn_norm" layer.layer_index));
      profile_attention_norm_ms :=
        !profile_attention_norm_ms
        + int_of_float ((Unix.gettimeofday () -. layer_started) *. 1000.0);
      let kind_started = Unix.gettimeofday () in
      begin
        match layer.layer_kind with
        | "recurrent" ->
            run_recurrent_layer ~stateful:true plan state addrs layer.layer_index 1e-6
        | "full_attention" ->
            run_full_attention_session_layer plan state addrs layer.layer_index position
        | other -> failwith ("unsupported qwen35 layer kind: " ^ other)
      end;
      let layer_elapsed =
        int_of_float ((Unix.gettimeofday () -. kind_started) *. 1000.0)
      in
      if String.equal layer.layer_kind "recurrent" then
        profile_recurrent_layer_ms := !profile_recurrent_layer_ms + layer_elapsed
      else
        profile_full_attention_layer_ms :=
          !profile_full_attention_layer_ms + layer_elapsed;
      let mixer_started = Unix.gettimeofday () in
      ignore
        (run_residual_add
           state
           addrs.hidden
           addrs.mixer
           qwen35_hidden_dim
           (Printf.sprintf "blk.%d.mixer_residual" layer.layer_index));
      profile_mixer_residual_ms :=
        !profile_mixer_residual_ms
        + int_of_float ((Unix.gettimeofday () -. mixer_started) *. 1000.0);
      let ffn_started = Unix.gettimeofday () in
      ignore (run_ffn_layer plan state addrs layer.layer_index 1e-6);
      profile_ffn_layer_ms :=
        !profile_ffn_layer_ms
        + int_of_float ((Unix.gettimeofday () -. ffn_started) *. 1000.0))
    plan.layers;
  profile_forward_ms :=
    !profile_forward_ms
    + int_of_float ((Unix.gettimeofday () -. forward_started) *. 1000.0);
  profile_token_elapsed_ms :=
    !profile_token_elapsed_ms
    @ [ int_of_float ((Unix.gettimeofday () -. forward_started) *. 1000.0) ]

let run_session_lm_head (plan : execution_plan) state addrs =
  copy_memory state addrs.hidden addrs.norm qwen35_hidden_dim;
  ignore (load_f32_name plan state "output_norm.weight" addrs.gamma qwen35_hidden_dim);
  ignore (run_rmsnorm state addrs.norm qwen35_hidden_dim addrs.gamma 1e-6 "output_norm");
  let output_binding = plan_binding_by_name plan "output.weight" in
  let _, vocab_size = binding_q1_shape output_binding "output.weight" in
  let output_chunks, output_ms =
    run_q1_projection_binding plan state output_binding addrs.norm addrs.logits "output.weight"
  in
  set_int state 0 addrs.logits;
  set_int state 1 vocab_size;
  ignore (run_vm_program state [| VM.ARGMAX_FP (2, 0, 1); VM.STOP |] "output_argmax");
  let token =
    match VM.getr state 2 with
    | VM.VInt z -> Z.to_int z
    | _ -> failwith "ARGMAX_FP returned non-int"
  in
  token, output_chunks, output_ms, hash_f64 (memory_f64 state addrs.hidden qwen35_hidden_dim),
  hash_f64 (memory_f64 state addrs.norm qwen35_hidden_dim),
  hash_f64 (memory_f64 state addrs.logits vocab_size)


let run_session_prefill
    ?generation_fixture
    ?checkpoint_after_tokens
    ?(stop_tokens = [||])
    (plan : execution_plan)
    token_ids
    max_new_tokens =
  require (String.equal plan.status "candidate_complete") "execution plan is not candidate_complete";
  require (String.equal plan.architecture "qwen35") "execution plan architecture is not qwen35";
  require (plan.missing_required_tensors = []) "execution plan reports missing required tensors";
  require (Array.length token_ids > 0) "--session-prefill requires at least one prompt token";
  require (max_new_tokens > 0) "--max-new-tokens must be positive";
  require (max_new_tokens <= 128) "--session-prefill currently caps --max-new-tokens at 128";
  require
    (Array.length token_ids + max_new_tokens - 1 <= session_max_context_tokens)
    (Printf.sprintf
       "session context exceeds harness cache capacity of %d tokens"
       session_max_context_tokens);
  Option.iter
    (fun count ->
      require
        (count > 0 && count < max_new_tokens)
        "--checkpoint-after-tokens must be positive and less than --max-new-tokens")
    checkpoint_after_tokens;
  Option.iter (verify_generation_fixture plan token_ids max_new_tokens) generation_fixture;
  let source_sha256 = verify_execution_model plan in
  let addrs = all_layer_addresses in
  let state =
    ref
      (create_harness_state
         ~limit:max_int
         "octBonsaiSessionHarnessCaller"
         "octBonsaiSessionHarnessContract")
  in
  let started = Unix.gettimeofday () in
  zero_session_states plan !state;
  let generated = Array.make max_new_tokens (-1) in
  let prefill_started = Unix.gettimeofday () in
  Array.iteri
    (fun position token_id -> run_session_forward_token plan !state addrs position token_id)
    token_ids;
  profile_prefill_ms :=
    int_of_float ((Unix.gettimeofday () -. prefill_started) *. 1000.0);
  let generation_started = Unix.gettimeofday () in
  let completed_effort = ref 0 in
  let checkpoint_root = ref "-" in
  let last_output_chunks = ref 0 in
  let last_output_ms = ref 0 in
  let last_hidden_hash = ref "-" in
  let last_norm_hash = ref "-" in
  let last_logits_hash = ref "-" in
  (try
    for index = 0 to max_new_tokens - 1 do
      let lm_started = Unix.gettimeofday () in
      let token, output_chunks, output_ms, hidden_hash, norm_hash, logits_hash =
        run_session_lm_head plan !state addrs
      in
      profile_lm_head_ms :=
        !profile_lm_head_ms
        + int_of_float ((Unix.gettimeofday () -. lm_started) *. 1000.0);
      let token_started = Unix.gettimeofday () in
      generated.(index) <- token;
      last_output_chunks := output_chunks;
      last_output_ms := output_ms;
      if Array.exists (fun stop -> stop = token) stop_tokens then raise Exit;
      let token_elapsed =
        int_of_float ((Unix.gettimeofday () -. token_started) *. 1000.0)
      in
      profile_token_elapsed_ms :=
        (!profile_token_elapsed_ms @ [ token_elapsed ]);
      profile_generated_forward_ms := !profile_generated_forward_ms + token_elapsed;
    last_hidden_hash := hidden_hash;
    last_norm_hash := norm_hash;
    last_logits_hash := logits_hash;
    if index + 1 < max_new_tokens then begin
      begin
        match checkpoint_after_tokens with
        | Some count when count = index + 1 ->
            let effort = (!state).VM.effort_used in
            let restored, root =
              restart_session_state plan !state (Array.length token_ids + index)
            in
            completed_effort := !completed_effort + effort;
            checkpoint_root := root;
            state := restored
        | _ -> ()
      end;
      run_session_forward_token
        plan
        !state
        addrs
        (Array.length token_ids + index)
        token
    end
  done
  with Exit -> ());
  profile_generation_ms :=
    int_of_float ((Unix.gettimeofday () -. generation_started) *. 1000.0);
  begin
    match generation_fixture with
    | None -> ()
    | Some fixture ->
        Array.iteri
          (fun i token ->
            let expected = fixture.generation_generated_token_ids.(i) in
            require
              (token = expected)
              (Printf.sprintf
                 "VM generated token mismatch at %d: actual=%d expected=%d"
                 i
                 token
                 expected))
          generated
  end;
  let elapsed_ms = int_of_float ((Unix.gettimeofday () -. started) *. 1000.0) in
  let model_root = execution_model_root () in
  let execution_root = execution_semantics_root () in
  let prompt_csv = string_of_int_array token_ids in
  let prompt_sha256 =
    sha256_hex ("octra-inference/prompt-token-ids/1\n" ^ prompt_csv)
  in
  let request_root =
    sha256_hex
      (Printf.sprintf
         "octra-inference/inference-request/1\nmodel_root=%s\nexecution_semantics_root=%s\nprompt_sha256=%s\nprompt_token_ids=%s\n"
         model_root execution_root prompt_sha256 prompt_csv)
  in
  let session_id =
    sha256_hex
      (Printf.sprintf
         "octra-inference/inference-session/1\nmodel_root=%s\nexecution_semantics_root=%s\nrequest_root=%s\n"
         model_root execution_root request_root)
  in
  let stop_reason =
    if
      Array.exists
        (fun stop -> stop = generated.(max_new_tokens - 1))
        stop_tokens
    then "stop_token"
    else "max_new_tokens"
  in
  Printf.printf
    "bonsai_session_prefill_vm_harness ok model=%s model_root=%s execution_semantics_root=%s source_sha256=%s prompt_sha256=%s request_root=%s session_id=%s stop_reason=%s prompt_tokens=%s max_new_tokens=%d generated_token_ids=%s final_check_token_id=%d layers_per_token=%d total_layer_visits=%d recurrent_transitions=%d full_attention_transitions=%d output_chunks=%d output_projection_elapsed_ms=%d elapsed_ms=%d effort=%d final_hidden_sha256=%s final_norm_sha256=%s logits_sha256=%s comparison=%s restart_resume=%s checkpoint_root=%s storage_boundary=%s boundary=local_candidate_vm_full_prompt_prefill_host_orchestrated prefill_elapsed_ms=%d generation_elapsed_ms=%d generated_forward_elapsed_ms=%d lm_head_elapsed_ms=%d token_elapsed_ms=%s profile_forward_elapsed_ms=%d profile_gather_elapsed_ms=%d profile_attention_norm_elapsed_ms=%d profile_recurrent_layer_elapsed_ms=%d profile_full_attention_layer_elapsed_ms=%d profile_mixer_residual_elapsed_ms=%d profile_ffn_layer_elapsed_ms=%d profile_q1_projection_elapsed_ms=%d profile_q1_span_read_elapsed_ms=%d profile_q1_vm_run_elapsed_ms=%d profile_q1_projection_count=%d profile_q1_projection_chunks=%d profile_q1_physical_bundle_count=%d profile_q1_dynamic_bundle_count=%d profile_vm_program_count=%d profile_vm_program_elapsed_ms=%d profile_load_f32_elapsed_ms=%d\n%!"
    (Option.value plan.model_name ~default:"-")
    (execution_model_root ())
    (execution_semantics_root ())
    source_sha256
    prompt_sha256
    request_root
    session_id
    stop_reason
    (string_of_int_array token_ids)
    max_new_tokens
    (string_of_int_array generated)
    generated.(max_new_tokens - 1)
    (List.length plan.layers)
    (List.length plan.layers * (Array.length token_ids + max_new_tokens - 1))
    (List.length (List.filter (fun layer -> String.equal layer.layer_kind "recurrent") plan.layers)
     * (Array.length token_ids + max_new_tokens - 1))
    (List.length (List.filter (fun layer -> String.equal layer.layer_kind "full_attention") plan.layers)
     * (Array.length token_ids + max_new_tokens - 1))
    !last_output_chunks
    !last_output_ms
    elapsed_ms
    (!completed_effort + (!state).VM.effort_used)
    !last_hidden_hash
    !last_norm_hash
    !last_logits_hash
    (if Option.is_some generation_fixture then "token_match_fail_closed" else "not_checked")
    (if Option.is_some checkpoint_after_tokens then "host_checkpoint_roundtrip" else "not_performed")
    !checkpoint_root
    (execution_storage_boundary ())
    !profile_prefill_ms
    !profile_generation_ms
    !profile_generated_forward_ms
    !profile_lm_head_ms
    (String.concat "," (List.map string_of_int !profile_token_elapsed_ms))
    !profile_forward_ms
    !profile_gather_ms
    !profile_attention_norm_ms
    !profile_recurrent_layer_ms
    !profile_full_attention_layer_ms
    !profile_mixer_residual_ms
    !profile_ffn_layer_ms
    !profile_q1_projection_ms
    !profile_q1_span_read_ms
    !profile_q1_vm_run_ms
    !profile_q1_projection_count
    !profile_q1_projection_chunks
    !profile_q1_physical_bundle_count
    !profile_q1_dynamic_bundle_count
    !profile_vm_program_count
    !profile_vm_program_elapsed_ms
    !profile_load_f32_elapsed_ms

let run_all_layers_first_token ?activation ?single_token_fixture ?layer0_wrapper (plan : execution_plan) token_id =
  require (String.equal plan.status "candidate_complete") "execution plan is not candidate_complete";
  require (String.equal plan.architecture "qwen35") "execution plan architecture is not qwen35";
  require (plan.missing_required_tensors = []) "execution plan reports missing required tensors";
  let addrs = all_layer_addresses in
  let state =
    create_harness_state
      "octBonsaiAllLayersHarnessCaller"
      "octBonsaiAllLayersHarnessContract"
  in
  let started = Unix.gettimeofday () in
  let source_sha256 = verify_execution_model plan in
  ignore
    (run_q1_gather_row
       plan
       state
       (plan_binding_by_name plan "token_embd.weight")
       token_id
       addrs.hidden
       "token_embd.weight");
  List.iter
    (fun layer ->
      let input_hash = hash_f64 (memory_f64 state addrs.hidden qwen35_hidden_dim) in
      copy_memory state addrs.hidden addrs.norm qwen35_hidden_dim;
      ignore
        (load_f32_name
           plan
           state
           (Printf.sprintf "blk.%d.attn_norm.weight" layer.layer_index)
           addrs.gamma
           qwen35_hidden_dim);
      ignore
        (run_rmsnorm
           state
           addrs.norm
           qwen35_hidden_dim
           addrs.gamma
           1e-6
           (Printf.sprintf "blk.%d.attn_norm" layer.layer_index));
      let norm_hash = hash_f64 (memory_f64 state addrs.norm qwen35_hidden_dim) in
      begin
        match activation with
        | Some expected when layer.layer_index = 0 ->
            require
              (expected.token_id = token_id)
              "layer0 activation fixture token_id does not match --token-id";
            let actual = memory_f64 state addrs.norm qwen35_hidden_dim in
            let delta =
              assert_arrays_close
                ~atol:1e-8
                actual
                expected.activation
                "all-layers layer0 attn_norm"
            in
            Printf.printf
              "layer0_norm_conformance actual_sha256=%s reference_sha256=%s max_abs_delta=%.17g comparison=tolerance_bounded atol=1e-8 rtol=0\n%!"
              norm_hash
              expected.activation_sha256
              delta
        | _ -> ()
      end;
      begin
        match layer.layer_kind with
        | "recurrent" ->
            run_recurrent_layer ?layer0_wrapper plan state addrs layer.layer_index 1e-6
        | "full_attention" ->
            run_full_attention_first_token_layer plan state addrs layer.layer_index
        | other -> failwith ("unsupported qwen35 layer kind: " ^ other)
      end;
      ignore
        (run_residual_add
           state
           addrs.hidden
           addrs.mixer
           qwen35_hidden_dim
           (Printf.sprintf "blk.%d.mixer_residual" layer.layer_index));
      let mixer_hash = hash_f64 (memory_f64 state addrs.mixer qwen35_hidden_dim) in
      let ffn_hash = run_ffn_layer plan state addrs layer.layer_index 1e-6 in
      let output_hash = hash_f64 (memory_f64 state addrs.hidden qwen35_hidden_dim) in
      begin
        match single_token_fixture with
        | Some fixture ->
            require
              (Array.length fixture.single_token_layer_outputs > layer.layer_index)
              "single-token fixture is missing layer output";
            let stats =
              compare_arrays
                (memory_f64 state addrs.hidden qwen35_hidden_dim)
                fixture.single_token_layer_outputs.(layer.layer_index)
                (Printf.sprintf "layer_%d_output" layer.layer_index)
            in
            Printf.printf
              "layer_output_conformance layer=%d kind=%s actual_sha256=%s max_abs_delta=%.17g max_rel_delta=%.17g rms_delta=%.17g worst_index=%d actual_non_finite=%d expected_non_finite=%d comparison=numeric\n%!"
              layer.layer_index
              layer.layer_kind
              output_hash
              stats.max_abs_delta
              stats.max_rel_delta
              stats.rms_delta
              stats.worst_index
              stats.actual_non_finite
              stats.expected_non_finite
        | None -> ()
      end;
      Printf.printf
        "layer_report layer=%d kind=%s input_sha256=%s norm_sha256=%s mixer_sha256=%s ffn_sha256=%s output_sha256=%s\n%!"
        layer.layer_index
        layer.layer_kind
        input_hash
        norm_hash
        mixer_hash
        ffn_hash
        output_hash)
    plan.layers;
  copy_memory state addrs.hidden addrs.norm qwen35_hidden_dim;
  ignore (load_f32_name plan state "output_norm.weight" addrs.gamma qwen35_hidden_dim);
  ignore (run_rmsnorm state addrs.norm qwen35_hidden_dim addrs.gamma 1e-6 "output_norm");
  let output_binding = plan_binding_by_name plan "output.weight" in
  let _, vocab_size = binding_q1_shape output_binding "output.weight" in
  let output_chunks, output_ms =
    run_q1_projection_binding plan state output_binding addrs.norm addrs.logits "output.weight"
  in
  set_int state 0 addrs.logits;
  set_int state 1 vocab_size;
  ignore (run_vm_program state [| VM.ARGMAX_FP (2, 0, 1); VM.STOP |] "output_argmax");
  let token =
    match VM.getr state 2 with
    | VM.VInt z -> Z.to_int z
    | _ -> failwith "ARGMAX_FP returned non-int"
  in
  let elapsed_ms = int_of_float ((Unix.gettimeofday () -. started) *. 1000.0) in
  let hidden_hash = hash_f64 (memory_f64 state addrs.hidden qwen35_hidden_dim) in
  let norm_hash = hash_f64 (memory_f64 state addrs.norm qwen35_hidden_dim) in
  let logits_hash = hash_f64 (memory_f64 state addrs.logits vocab_size) in
  begin
    match single_token_fixture with
    | Some fixture ->
        require
          (fixture.single_token_evaluated_token_id = token_id)
          "single-token fixture evaluated_token_id does not match --token-id";
        let hidden_actual = memory_f64 state addrs.hidden qwen35_hidden_dim in
        let norm_actual = memory_f64 state addrs.norm qwen35_hidden_dim in
        let logits_actual = memory_f64 state addrs.logits vocab_size in
        let hidden_stats =
          compare_arrays hidden_actual fixture.single_token_final_hidden "final_hidden"
        in
        let norm_stats =
          compare_arrays norm_actual fixture.single_token_final_norm "final_norm"
        in
        let logits_stats =
          compare_arrays logits_actual fixture.single_token_logits "logits"
        in
        let print_stats name actual_hash expected_hash stats =
          Printf.printf
            "final_tensor_conformance name=%s actual_sha256=%s reference_sha256=%s max_abs_delta=%.17g max_rel_delta=%.17g rms_delta=%.17g worst_index=%d actual_non_finite=%d expected_non_finite=%d comparison=numeric\n%!"
            name
            actual_hash
            expected_hash
            stats.max_abs_delta
            stats.max_rel_delta
            stats.rms_delta
            stats.worst_index
            stats.actual_non_finite
            stats.expected_non_finite
        in
        Printf.printf
          "final_token_conformance actual_token_id=%d reference_token_id=%d match=%b\n%!"
          token
          fixture.single_token_generated_token_id
          (token = fixture.single_token_generated_token_id);
        print_stats
          "final_hidden"
          hidden_hash
          fixture.single_token_final_hidden_sha256
          hidden_stats;
        print_stats "final_norm" norm_hash fixture.single_token_final_norm_sha256 norm_stats;
        print_stats "logits" logits_hash fixture.single_token_logits_sha256 logits_stats
    | None -> ()
  end;
  Printf.printf
    "bonsai_all_layers_first_token_vm_harness ok model=%s model_root=%s execution_semantics_root=%s source_sha256=%s token_id=%d layers=%d recurrent_layers=%d full_attention_layers=%d generated_token_id=%d hidden_sha256=%s final_norm_sha256=%s logits_sha256=%s output_chunks=%d output_projection_elapsed_ms=%d elapsed_ms=%d effort=%d comparison=not_checked storage_boundary=%s boundary=first_token_zero_state_all_distinct_layers\n%!"
    (Option.value plan.model_name ~default:"-")
    (execution_model_root ())
    (execution_semantics_root ())
    source_sha256
    token_id
    (List.length plan.layers)
    (List.length (List.filter (fun layer -> String.equal layer.layer_kind "recurrent") plan.layers))
    (List.length (List.filter (fun layer -> String.equal layer.layer_kind "full_attention") plan.layers))
    token
    hidden_hash
    norm_hash
    logits_hash
    output_chunks
    output_ms
    elapsed_ms
    state.VM.effort_used
    (execution_storage_boundary ())

let () =
  let plan_path = ref None in
  let packed_model_path = ref None in
  let vector_path = ref None in
  let bundle_path = ref None in
  let wrapper_path = ref None in
  let activation_path = ref None in
  let expected_output_sha256 = ref None in
  let source_backed_qkv = ref false in
  let layer0_full = ref false in
  let benchmark = ref false in
  let repeat_layer0 = ref 1 in
  let all_layers_first_token = ref false in
  let session_prefill = ref false in
  let all_layers_token_id = ref None in
  let prompt_token_csv = ref None in
  let eos_token_csv = ref None in
  let max_new_tokens = ref 1 in
  let checkpoint_after_tokens = ref None in
  let single_token_fixture_path = ref None in
  let generation_fixture_path = ref None in
  let qkv_path = ref None in
  let attn_gate_path = ref None in
  let ssm_beta_path = ref None in
  let ssm_alpha_path = ref None in
  let ssm_out_path = ref None in
  let post_attn_path = ref None in
  let ffn_gate_path = ref None in
  let ffn_up_path = ref None in
  let ffn_down_input_path = ref None in
  let ffn_down_path = ref None in
  let layer0_output_path = ref None in
  Arg.parse
    [
      ("--plan", Arg.String (fun path -> plan_path := Some path), "qwen35 execution plan JSON");
      ("--packed-model", Arg.String (fun path -> packed_model_path := Some path), "verified octra-inference ModelRelease directory");
      ("--vector", Arg.String (fun path -> vector_path := Some path), "layer 0 Q1 vector JSON");
      ("--bundle", Arg.String (fun path -> bundle_path := Some path), "layer 0 Q1 projection bundle JSON");
      ("--wrapper", Arg.String (fun path -> wrapper_path := Some path), "layer 0 recurrent wrapper fixture JSON");
      ("--activation", Arg.String (fun path -> activation_path := Some path), "activation fixture JSON");
      ("--expected-output-sha256", Arg.String (fun hash -> expected_output_sha256 := Some hash), "expected output f64 sha256");
      ("--source-backed-qkv", Arg.Set source_backed_qkv, "run layer 0 QKV from GGUF byte span declared in --plan");
      ("--layer0-full", Arg.Set layer0_full, "run source-backed layer 0 through wrapper, SSM out, FFN, and residual output");
      ("--benchmark", Arg.Set benchmark, "skip conformance hashing/comparison during layer0-full timing");
      ("--repeat-layer0", Arg.Int (fun count -> repeat_layer0 := count), "repeat layer0-full n times after loading artifacts");
      ("--all-layers-first-token", Arg.Set all_layers_first_token, "run one first-token zero-state pass across all distinct qwen35 layers");
      ("--session-prefill", Arg.Set session_prefill, "run a full prompt prefill/generation session in one persistent local VM state");
      ("--token-id", Arg.Int (fun token -> all_layers_token_id := Some token), "token id for --all-layers-first-token");
      ("--prompt-token-csv", Arg.String (fun csv -> prompt_token_csv := Some csv), "comma-separated prompt token ids for --session-prefill");
      ("--eos-token-csv", Arg.String (fun csv -> eos_token_csv := Some csv), "comma-separated stop token ids for --session-prefill");
      ("--max-new-tokens", Arg.Int (fun count -> max_new_tokens := count), "number of greedy tokens to generate for --session-prefill");
      ("--checkpoint-after-tokens", Arg.Int (fun count -> checkpoint_after_tokens := Some count), "restart from persistent session state after n generated tokens");
      ("--single-token-fixture", Arg.String (fun path -> single_token_fixture_path := Some path), "single-token tensor fixture JSON for all-layer comparison");
      ("--generation-fixture", Arg.String (fun path -> generation_fixture_path := Some path), "Rust generation fixture JSON for session-prefill fail-closed comparison");
      ("--qkv", Arg.String (fun path -> qkv_path := Some path), "layer 0 qkv projection bundle JSON");
      ("--attn-gate", Arg.String (fun path -> attn_gate_path := Some path), "layer 0 attention gate projection bundle JSON");
      ("--ssm-beta", Arg.String (fun path -> ssm_beta_path := Some path), "layer 0 ssm_beta projection bundle JSON");
      ("--ssm-alpha", Arg.String (fun path -> ssm_alpha_path := Some path), "layer 0 ssm_alpha projection bundle JSON");
      ("--ssm-out", Arg.String (fun path -> ssm_out_path := Some path), "layer 0 ssm_out projection bundle JSON");
      ("--post-attn", Arg.String (fun path -> post_attn_path := Some path), "layer 0 post-attention workspace vector JSON");
      ("--ffn-gate", Arg.String (fun path -> ffn_gate_path := Some path), "layer 0 ffn_gate projection bundle JSON");
      ("--ffn-up", Arg.String (fun path -> ffn_up_path := Some path), "layer 0 ffn_up projection bundle JSON");
      ("--ffn-down-input", Arg.String (fun path -> ffn_down_input_path := Some path), "layer 0 ffn_down input workspace vector JSON");
      ("--ffn-down", Arg.String (fun path -> ffn_down_path := Some path), "layer 0 ffn_down projection bundle JSON");
      ("--layer0-output", Arg.String (fun path -> layer0_output_path := Some path), "layer 0 output workspace vector JSON");
    ]
    (fun value -> raise (Arg.Bad ("unexpected argument: " ^ value)))
    "bonsai_layer0_vm_harness --plan execution-plan.hashed.json (--vector layer0-q1-vector.json | --bundle layer0-projection-fixture.cjson | --wrapper layer0-wrapper-fixture.cjson | --source-backed-qkv --activation activation.cjson --expected-output-sha256 <sha256>)";
  let plan =
    match !plan_path, !packed_model_path with
    | Some path, None -> load_plan path
    | None, Some root ->
        require
          (!session_prefill || !all_layers_first_token)
          "--packed-model currently requires --session-prefill or --all-layers-first-token";
        let packed = load_packed_model root in
        active_packed_model := Some packed;
        load_packed_execution_plan packed
    | Some _, Some _ ->
        failwith
          "--plan and --packed-model are mutually exclusive; packed execution uses its rooted descriptor"
    | None, None -> failwith "--plan or --packed-model is required"
  in
  let require_path option name =
    match option with
    | Some path -> path
    | None -> failwith (name ^ " is required")
  in
  if !session_prefill then begin
    let token_ids =
      match !prompt_token_csv with
      | Some csv -> parse_token_csv csv
      | None -> failwith "--session-prefill requires --prompt-token-csv"
    in
    let generation_fixture = Option.map load_generation_fixture !generation_fixture_path in
    let stop_tokens =
      match !eos_token_csv with
      | Some csv -> parse_token_csv csv
      | None -> [||]
    in
    run_session_prefill
      ?generation_fixture
      ?checkpoint_after_tokens:!checkpoint_after_tokens
      ~stop_tokens
      plan
      token_ids
      !max_new_tokens
  end
  else if !all_layers_first_token then begin
    let token_id =
      match !all_layers_token_id with
      | Some token when token >= 0 -> token
      | Some _ -> failwith "--token-id must be nonnegative"
      | None -> failwith "--all-layers-first-token requires --token-id"
    in
    let activation = Option.map load_activation !activation_path in
    let single_token_fixture = Option.map load_single_token_fixture !single_token_fixture_path in
    let layer0_wrapper = Option.map load_wrapper_fixture !wrapper_path in
    run_all_layers_first_token ?activation ?single_token_fixture ?layer0_wrapper plan token_id
  end
  else if !source_backed_qkv then begin
    match !activation_path, !expected_output_sha256, !vector_path, !bundle_path, !wrapper_path with
    | Some activation_file, Some expected_hash, None, None, None ->
        let activation = load_activation activation_file in
        run_source_backed_qkv plan activation expected_hash None
    | Some activation_file, _, None, Some bundle_file, None ->
        let activation = load_activation activation_file in
        let bundle = load_projection_bundle bundle_file in
        let expected_hash =
          Option.value !expected_output_sha256 ~default:bundle.bundle_expected_output_sha256
        in
        run_source_backed_qkv plan activation expected_hash (Some bundle)
    | None, _, _, _, _ -> failwith "--source-backed-qkv requires --activation"
    | _, None, _, None, None -> failwith "--source-backed-qkv requires --expected-output-sha256 or --bundle"
    | _, _, Some _, _, _ -> failwith "--source-backed-qkv cannot be combined with --vector"
    | _, _, _, _, Some _ -> failwith "--source-backed-qkv cannot be combined with --wrapper"
  end
  else if !layer0_full then begin
    let activation = load_activation (require_path !activation_path "--activation") in
    let wrapper = load_wrapper_fixture (require_path !wrapper_path "--wrapper") in
    let qkv = Option.map load_projection_bundle !qkv_path in
    let attn_gate = Option.map load_projection_bundle !attn_gate_path in
    let ssm_beta = Option.map load_projection_bundle !ssm_beta_path in
    let ssm_alpha = Option.map load_projection_bundle !ssm_alpha_path in
    let ssm_out = load_projection_bundle (require_path !ssm_out_path "--ssm-out") in
    let post_attn = load_workspace_vector (require_path !post_attn_path "--post-attn") in
    let ffn_gate = load_projection_bundle (require_path !ffn_gate_path "--ffn-gate") in
    let ffn_up = load_projection_bundle (require_path !ffn_up_path "--ffn-up") in
    let ffn_down_input =
      load_workspace_vector (require_path !ffn_down_input_path "--ffn-down-input")
    in
    let ffn_down = load_projection_bundle (require_path !ffn_down_path "--ffn-down") in
    let layer0_output =
      load_workspace_vector (require_path !layer0_output_path "--layer0-output")
    in
    if !repeat_layer0 <= 0 then
      failwith "--repeat-layer0 must be positive";
    for _ = 1 to !repeat_layer0 do
      run_layer0_full
        ~verify:(not !benchmark)
        ?qkv
        ?attn_gate
        ?ssm_beta
        ?ssm_alpha
        plan
        activation
        wrapper
        ssm_out
        post_attn
        ffn_gate
        ffn_up
        ffn_down_input
        ffn_down
        layer0_output
    done
  end
  else match !vector_path, !bundle_path, !wrapper_path with
  | Some vector_file, None, None ->
      let vector = load_vector vector_file in
      validate_plan_and_vector plan vector;
      let gather_effort, gather_bytecode_size, gather_hash = run_direct_gather vector in
      let linear_effort, linear_bytecode_size, linear_hash, linear_max_abs_delta =
        run_direct_linear vector
      in
      let aml_effort, compiler_instructions, aml_max_abs_delta = run_compiled_aml vector in
      Printf.printf
        "bonsai_layer0_vm_harness ok model=%s source_sha256=%s tensor=%s dims=%s payload_offset=%d m=%d k=%d n=%d q1_blocks_sha256=%s gather_effort=%d gather_bytecode_size=%d gather_sha256=%s linear_effort=%d linear_bytecode_size=%d linear_output_sha256=%s linear_max_abs_delta=%.17g aml_effort=%d compiler_instructions=%d aml_max_abs_delta=%.17g expected_output_sha256=%s\n%!"
        (Option.value plan.model_name ~default:"-")
        (Option.value plan.source_sha256 ~default:"-")
        vector.tensor_name
        (String.concat "x" (List.map string_of_int vector.tensor_dimensions))
        vector.tensor_payload_offset
        vector.m
        vector.k
        vector.n
        vector.q1_sha256
        gather_effort
        gather_bytecode_size
        gather_hash
        linear_effort
        linear_bytecode_size
        linear_hash
        linear_max_abs_delta
        aml_effort
        compiler_instructions
        aml_max_abs_delta
        vector.expected_sha256
  | None, Some bundle_file, None ->
      let bundle = load_projection_bundle bundle_file in
      run_projection_bundle plan bundle
  | None, None, Some wrapper_file ->
      let wrapper = load_wrapper_fixture wrapper_file in
      run_layer0_wrapper wrapper
  | Some _, _, _ | _, Some _, Some _ -> failwith "choose --vector, --bundle, or --wrapper"
  | None, None, None -> failwith "--vector, --bundle, --wrapper, or --source-backed-qkv is required"
