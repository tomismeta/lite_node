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


module Admission = Octra_vm.Admission
module Assembler = Octra_vm.Assembler
module Bytecode = Octra_vm.Bytecode
module Abi = Octra_vm.Inference_session_abi
module Execution = Octra_vm.Inference_execution
module Gen = Octra_vm.Oct_gen
module Lang = Octra_vm.Oct_lang
module Model = Octra_vm.Inference_model
module Plan = Octra_vm.Inference_plan
module Program_effects = Octra_vm.Program_effects
module Req = Octra_vm.Execution_requirement
module Request = Octra_vm.Inference_request
module Store = Octra_vm.Inference_store
module Target = Octra_vm.Inference_target
module VM = Octra_vm.Contract_vm

let check label condition =
  if not condition then failwith label

let starts_with prefix value =
  String.length value >= String.length prefix
  && String.sub value 0 (String.length prefix) = prefix

let hex_root char =
  String.make 64 char

let sha256 raw =
  Digestif.SHA256.(digest_string raw |> to_hex)

let hex_value = function
  | '0'..'9' as c -> Char.code c - Char.code '0'
  | 'a'..'f' as c -> 10 + Char.code c - Char.code 'a'
  | 'A'..'F' as c -> 10 + Char.code c - Char.code 'A'
  | _ -> failwith "invalid hex"

let bytes_of_hex hex =
  let compact =
    String.to_seq hex
    |> Seq.filter (function '\n' | '\r' | '\t' | ' ' -> false | _ -> true)
    |> String.of_seq
  in
  let len = String.length compact in
  if len mod 2 <> 0 then failwith "odd hex length";
  String.init (len / 2) (fun i ->
    Char.chr ((hex_value compact.[i * 2] lsl 4)
              lor hex_value compact.[(i * 2) + 1]))

let int64_le bytes offset =
  let value = ref 0L in
  for byte = 0 to 7 do
    value :=
      Int64.logor
        !value
        (Int64.shift_left
           (Int64.of_int (Char.code bytes.[offset + byte]))
           (byte * 8))
  done;
  !value

let fixture_bits bytes =
  if String.length bytes mod 8 <> 0 then failwith "invalid f64 fixture";
  List.init (String.length bytes / 8) (fun i -> int64_le bytes (i * 8))

let set_int_reg state reg value =
  state.VM.regs.(reg) <- VM.VInt (Z.of_int value)

let set_f64_bits state addr bits =
  Hashtbl.replace state.VM.memory.data addr (VM.VInt (Z.of_int64 bits))

let f64_bits state addr =
  match Hashtbl.find_opt state.VM.memory.data addr with
  | Some (VM.VInt bits) when Z.fits_int64 bits -> Z.to_int64 bits
  | _ -> failwith "missing f64 cell"

let set_fixture state addr bytes =
  List.iteri
    (fun index bits -> set_f64_bits state (addr + index) bits)
    (fixture_bits bytes)

let check_cells label state base expected =
  List.iteri
    (fun index bits ->
      check
        (label ^ " output " ^ string_of_int index)
        (f64_bits state (base + index) = bits))
    expected

type dims = {
  timesteps : int;
  q_heads : int;
  k_heads : int;
  v_heads : int;
  key_dim : int;
  value_dim : int;
}

type fixture = {
  name : string;
  dims : dims;
  q : string;
  k : string;
  v : string;
  log_decay : string;
  beta : string;
  initial_state : string;
  expected_output : string;
  expected_state : string;
}

let zero_state_one_timestep =
  {
    name = "zero state one timestep";
    dims = {
      timesteps = 1;
      q_heads = 1;
      k_heads = 1;
      v_heads = 1;
      key_dim = 2;
      value_dim = 2;
    };
    q = bytes_of_hex "333333333333e33f9a9999999999e9bf";
    k = bytes_of_hex "000000000000e03f000000000000d03f";
    v = bytes_of_hex "000000000000f03f000000000000e0bf";
    log_decay = bytes_of_hex "000000000000c0bf";
    beta = bytes_of_hex "000000000000e83f";
    initial_state =
      bytes_of_hex
        "0000000000000000000000000000000000000000000000000000000000000000";
    expected_output =
      bytes_of_hex "8a14ff7a2427ab3f8a14ff7a24279bbf";
    expected_state =
      bytes_of_hex
        "000000000000d83f000000000000c83f000000000000c8bf000000000000b8bf";
  }

let nonzero_state_one_timestep =
  {
    name = "nonzero state one timestep";
    dims = {
      timesteps = 1;
      q_heads = 1;
      k_heads = 1;
      v_heads = 1;
      key_dim = 2;
      value_dim = 2;
    };
    q = bytes_of_hex "333333333333e33f9a9999999999e9bf";
    k = bytes_of_hex "000000000000e03f000000000000d03f";
    v = bytes_of_hex "000000000000f03f000000000000e0bf";
    log_decay = bytes_of_hex "000000000000c0bf";
    beta = bytes_of_hex "000000000000e83f";
    initial_state =
      bytes_of_hex
        "9a9999999999b93f9a9999999999c9bf333333333333d33f9a9999999999d9bf";
    expected_output =
      bytes_of_hex "9ed1874b515cc83fcbdc8b932e1fd23f";
    expected_state =
      bytes_of_hex
        "a0b36207e2a5dd3fe01753277e87863f98c92ca65d14af3f590f3c4e151fddbf";
  }

let nonzero_state_multiple_timesteps =
  {
    name = "nonzero state multiple timesteps";
    dims = {
      timesteps = 2;
      q_heads = 2;
      k_heads = 2;
      v_heads = 3;
      key_dim = 2;
      value_dim = 2;
    };
    q =
      bytes_of_hex
        "7b14ae47e17ae43fec51b81e85ebe13fb81e85eb51b8de3f9a9999999999d93f\
         7b14ae47e17ad43fb81e85eb51b8ce3f7b14ae47e17ac43f7b14ae47e17ab43f";
    k =
      bytes_of_hex
        "2a5c8fc2f528e4bf2a5c8fc2f528e43fec51b81e85ebe13f5d8fc2f5285cdf3f\
         e27a14ae47e1da3f676666666666d63fec51b81e85ebd13fe27a14ae47e1ca3f";
    v =
      bytes_of_hex
        "333333333333e3bf0ad7a3703d0ae7bfe17a14ae47e1eabfb81e85eb51b8eebf\
         48e17a14ae47f1bf48e17a14ae47f13fb81e85eb51b8ee3fe17a14ae47e1ea3f\
         0ad7a3703d0ae73f333333333333e33fb81e85eb51b8de3f0ad7a3703d0ad73f";
    log_decay =
      bytes_of_hex
        "ec51b81e85eba1bf7b14ae47e17ab4bf7b14ae47e17a94bfa4703d0ad7a3b0bf\
         295c8fc2f528bcbf9a9999999999a9bf";
    beta =
      bytes_of_hex
        "15ae47e17a14de3f52b81e85eb51d83f90c2f5285c8fd23f9a9999999999c93f\
         ae47e17a14aee73fcccccccccccce43f";
    initial_state =
      bytes_of_hex
        "cdcccccccccccc3f9a9999999999c93f676666666666c63f343333333333c33f\
         000000000000c03f9a9999999999b93f343333333333b33f9a9999999999a93f\
         9a9999999999993f00000000000000009a999999999999bf9a9999999999a9bf";
    expected_output =
      bytes_of_hex
        "9dc1a96798a0c73f1a6dc6ef0ba4c23fc25a7ed24b51abbfc33aa066674cb7bf\
         6a950c74499f963fff214b322d69a5bf4ba1b68d3054bc3f9196813c7488b73f\
         1e7ddd6a6e3c873f3ebe0f52207f4dbf90e942dd4e8faf3f48e1750f0b87903f";
    expected_state =
      bytes_of_hex
        "282b144fc7b6db3f5e8415541e1fb33fc6b57a02a47ada3fd0ebdfa5836a84bf\
         ec63c097f94bb43f586dc26cb380a53fc031a1e0973f743f907a55743e6b9abf\
         4b6cdc3f5a52d53f8a57f8e852bbb4bf570fc436b5c9babf380ae44c4308ce3f";
  }

let zero_state_multiple_timesteps =
  {
    name = "zero state multiple timesteps";
    dims = {
      timesteps = 3;
      q_heads = 2;
      k_heads = 1;
      v_heads = 3;
      key_dim = 3;
      value_dim = 3;
    };
    q =
      bytes_of_hex
        "1f85eb51b81ee5bfa4703d0ad7a3e8bf295c8fc2f528ecbfae47e17a14aeefbf\
         ae47e17a14aeef3f295c8fc2f528ec3fa4703d0ad7a3e83f1f85eb51b81ee53f\
         9a9999999999e13f295c8fc2f528dc3f1f85eb51b81ed53f295c8fc2f528cc3f\
         295c8fc2f528bc3f0000000000000000295c8fc2f528bcbf295c8fc2f528ccbf\
         1f85eb51b81ed5bf295c8fc2f528dcbf";
    k =
      bytes_of_hex
        "0ad7a3703d0ad7bfccccccccccccdcbf48e17a14ae47e1bf295c8fc2f528e4bf\
         0ad7a3703d0ae7bfeb51b81e85ebe9bfeb51b81e85ebe93f0ad7a3703d0ae73f\
         295c8fc2f528e43f";
    v =
      bytes_of_hex
        "a4703d0ad7a3d0bff6285c8fc2f5d8bfa4703d0ad7a3e0bfcdcccccccccce4bf\
         f6285c8fc2f5e8bf1f85eb51b81eedbfa4703d0ad7a3f0bfb81e85eb51b8f2bf\
         b81e85eb51b8f23fa4703d0ad7a3f03f1f85eb51b81eed3ff6285c8fc2f5e83f\
         cdcccccccccce43fa4703d0ad7a3e03ff6285c8fc2f5d83fa4703d0ad7a3d03f\
         a4703d0ad7a3c03f0000000000000000a4703d0ad7a3c0bfa4703d0ad7a3d0bf\
         f6285c8fc2f5d8bfa4703d0ad7a3e0bfcdcccccccccce4bff6285c8fc2f5e8bf\
         1f85eb51b81eedbfa4703d0ad7a3f0bfb81e85eb51b8f2bf";
    log_decay =
      bytes_of_hex
        "7b14ae47e17ab4bf7b14ae47e17a94bfa4703d0ad7a3b0bf295c8fc2f528bcbf\
         9a9999999999a9bf52b81e85eb51b8bfec51b81e85eba1bf7b14ae47e17ab4bf\
         7b14ae47e17a94bf";
    beta =
      bytes_of_hex
        "ae47e17a14aee73fcccccccccccce43fec51b81e85ebe13f15ae47e17a14de3f\
         52b81e85eb51d83f90c2f5285c8fd23f9a9999999999c93fae47e17a14aee73f\
         cccccccccccce43f";
    initial_state =
      bytes_of_hex
        "0000000000000000000000000000000000000000000000000000000000000000\
         0000000000000000000000000000000000000000000000000000000000000000\
         0000000000000000000000000000000000000000000000000000000000000000\
         0000000000000000000000000000000000000000000000000000000000000000\
         0000000000000000000000000000000000000000000000000000000000000000\
         0000000000000000000000000000000000000000000000000000000000000000\
         000000000000000000000000000000000000000000000000";
    expected_output =
      bytes_of_hex
        "d908b45d941fbebfa4064746af97c6bfd908b45d941fcebf5c5603ae839ec13f\
         3a0104049e24c53f19ac045ab8aac83fe99e1239cacbd6bfc7f2348043a5d9bf\
         c7f2348043a5d93fdf5e55d02400d8bf04da9390b521d4bf2a55d2504643d0bf\
         feea0a5fe3f0aebfacbeaac9c1e3a0bfcd9254a201b576bf9065b23556feb43f\
         5d5e541164b4c03f8f62d4530d9fc4bf390de882d822773f00f21a930608703f\
         94ad9b4669da613fb657953ddcd9c23f626c8319fefcc73f118171f51f20cd3f\
         69445b16cd1f87bfd7aa596fdf6c8cbf33931fc419cd5bbf";
    expected_state =
      bytes_of_hex
        "027d77102517c9bf727dde9957c8cebff2bea211c53cd2bfaaf4b512f99bc8bf\
         fcf4a0a7af8dccbfa8fa451eb33fd0bf506cf414cd20c8bf866c63b50753cabf\
         bb6cd2554285ccbfcb94e910a120d2bfc485e375bbc9d0bf77edbab5abe5cebf\
         1aa13b122275d8bf7ac8bbefa39dd5bfd8ef3bcd25c6d2bf6aad8d13a3c9debf\
         300b94698c71dabff6689abf7519d6bf97dd07fe266de0bf3428a435db29dbbf\
         3895386f6879d5bf9c1ea39e1a05e3bf8cd9a51de10bdfbfde7505fe8c0dd8bf\
         f18cf5993e00e2bf4f268c48ce92e1bfadbf22f75d25e1bf";
  }

let irregular_dimensions =
  {
    name = "irregular dimensions";
    dims = {
      timesteps = 2;
      q_heads = 2;
      k_heads = 1;
      v_heads = 3;
      key_dim = 5;
      value_dim = 3;
    };
    q =
      bytes_of_hex
        "0ad7a3703d0ad7bfe17a14ae47e1dabfb81e85eb51b8debf48e17a14ae47e1bf\
         48e17a14ae47e13fb81e85eb51b8de3fe17a14ae47e1da3f0ad7a3703d0ad73f\
         333333333333d33fb81e85eb51b8ce3f0ad7a3703d0ac73fb81e85eb51b8be3f\
         b81e85eb51b8ae3f0000000000000000b81e85eb51b8aebfb81e85eb51b8bebf\
         0ad7a3703d0ac7bfb81e85eb51b8cebf333333333333d3bf0ad7a3703d0ad7bf";
    k =
      bytes_of_hex
        "9a9999999999c9bf000000000000d0bf343333333333d3bf676666666666d6bf\
         9a9999999999d9bfcdccccccccccdcbfcdccccccccccdc3f9a9999999999d93f\
         676666666666d63f343333333333d33f";
    v =
      bytes_of_hex
        "00000000000000009a9999999999b9bf9a9999999999c9bf343333333333d3bf\
         9a9999999999d9bf000000000000e0bf343333333333e3bf676666666666e6bf\
         9a9999999999e9bfcdccccccccccecbfcdccccccccccec3f9a9999999999e93f\
         676666666666e63f343333333333e33f000000000000e03f9a9999999999d93f\
         343333333333d33f9a9999999999c93f";
    log_decay =
      bytes_of_hex
        "7b14ae47e17ab4bf7b14ae47e17a94bfa4703d0ad7a3b0bf295c8fc2f528bcbf\
         9a9999999999a9bf52b81e85eb51b8bf";
    beta =
      bytes_of_hex
        "15ae47e17a14de3f52b81e85eb51d83f90c2f5285c8fd23f9a9999999999c93f\
         ae47e17a14aee73fcccccccccccce43f";
    initial_state =
      bytes_of_hex
        "333333333333b3bf0ad7a3703d0ab7bfe17a14ae47e1babfb81e85eb51b8bebf\
         48e17a14ae47c1bf48e17a14ae47c13fb81e85eb51b8be3fe17a14ae47e1ba3f\
         0ad7a3703d0ab73f333333333333b33fb81e85eb51b8ae3f0ad7a3703d0aa73f\
         b81e85eb51b89e3fb81e85eb51b88e3f0000000000000000b81e85eb51b88ebf\
         b81e85eb51b89ebf0ad7a3703d0aa7bfb81e85eb51b8aebf333333333333b3bf\
         0ad7a3703d0ab7bfe17a14ae47e1babfb81e85eb51b8bebf48e17a14ae47c1bf\
         48e17a14ae47c13fb81e85eb51b8be3fe17a14ae47e1ba3f0ad7a3703d0ab73f\
         333333333333b33fb81e85eb51b8ae3f0ad7a3703d0aa73fb81e85eb51b89e3f\
         b81e85eb51b88e3f0000000000000000b81e85eb51b88ebfb81e85eb51b89ebf\
         0ad7a3703d0aa7bfb81e85eb51b8aebf333333333333b3bf0ad7a3703d0ab7bf\
         e17a14ae47e1babfb81e85eb51b8bebf48e17a14ae47c1bf48e17a14ae47c13f\
         b81e85eb51b8be3f";
    expected_output =
      bytes_of_hex
        "b73cd98b87d1a13f570efd896909b0bfa340567b848ca2bfb43648d807914a3f\
         58ff2b15135294bf46e753d1ab82bb3ff445b56bb5e6a4bfd1da5dc3778a82bf\
         cc14ee9a02159f3fef0144e87bc174bf75c2d6b92f958a3f3afc3bbebf097d3f\
         cad60714ae8fb3bf2d7e2d009514b3bf9d0ca020ca6abbbf37fdf1a8206e7d3f\
         e4cd84e5c2b5393f74ad780c45a38bbf";
    expected_state =
      bytes_of_hex
        "f0e79c650ed7983f4213d4865fe0c0bfd6ec85ec5402c1bf6ac637524a24c1bf\
         ff9fe9b73f46c1bf4e0ff02d7e24a13f7c281138fdc5c53fe45645293704c33f\
         4d85791a7142c03f6a675b175601bb3f8001a1e21d8d75bf66f9bce4ec98bf3f\
         9390a23d235bbb3fc0278896591db73fedbe6def8fdfb23f9e5a7524dfaaccbf\
         6ec0b6b2da12cf3f3fac2f7cf2c6ca3f1298a8450a7bc63fe483210f222fc23f\
         5ceecde77197cfbfd1fd799ab125c23fb86d1e4b454bbd3fcbdf4861274bb63f\
         7fdc822884d4d43f409003480f028a3fbee6b2fa8f84d03f7740414b1751ce3f\
         70b31ca10e99cb3f6a26f8f605e1c83fd273085f3535a0bf165813ac1422c53f\
         8b7706e7030dc33f0097f921f3f7c03fec6cd9b9c4c5bd3fa125a495a6aab1bf\
         3cd504afeb58b93f2bfb0e9cc952b63f19211989a74cb33f084723768546b03f\
         96ae89bdd4a2b3bfc95609d0e9ff94bf6a54d64e27a99abf769c45f5c40bcb3f\
         c1fc6b459d56ca3f";
  }

let signed_zero_subnormal_one_timestep =
  {
    name = "signed zero subnormal one timestep";
    dims = {
      timesteps = 1;
      q_heads = 1;
      k_heads = 1;
      v_heads = 1;
      key_dim = 1;
      value_dim = 1;
    };
    q = bytes_of_hex "000000000000f03f";
    k = bytes_of_hex "0100000000000000";
    v = bytes_of_hex "0000000000000080";
    log_decay = bytes_of_hex "0000000000000000";
    beta = bytes_of_hex "000000000000f03f";
    initial_state = bytes_of_hex "0000000000000080";
    expected_output = bytes_of_hex "0000000000000000";
    expected_state = bytes_of_hex "0000000000000080";
  }

let negative_zero_decay_one_timestep =
  {
    signed_zero_subnormal_one_timestep with
    name = "negative zero decay one timestep";
    log_decay = bytes_of_hex "0000000000000080";
  }

let late_add_mul_overflow =
  {
    name = "late add/mul overflow";
    dims = {
      timesteps = 1;
      q_heads = 1;
      k_heads = 1;
      v_heads = 1;
      key_dim = 1;
      value_dim = 1;
    };
    q = bytes_of_hex "0000000000000040";
    k = bytes_of_hex "000000000000f03f";
    v = bytes_of_hex "ffffffffffffef7f";
    log_decay = bytes_of_hex "0000000000000000";
    beta = bytes_of_hex "000000000000f03f";
    initial_state = bytes_of_hex "0000000000000000";
    expected_output = bytes_of_hex "0000000000000000";
    expected_state = bytes_of_hex "ffffffffffffef7f";
  }

let fixtures =
  [
    zero_state_one_timestep;
    nonzero_state_one_timestep;
    zero_state_multiple_timesteps;
    nonzero_state_multiple_timesteps;
    irregular_dimensions;
    signed_zero_subnormal_one_timestep;
    negative_zero_decay_one_timestep;
  ]

let output_cells fixture =
  fixture.dims.timesteps * fixture.dims.v_heads * fixture.dims.value_dim

let state_cells fixture =
  fixture.dims.v_heads * fixture.dims.value_dim * fixture.dims.key_dim

let set_result_sentinel state fixture =
  for index = 0 to output_cells fixture - 1 do
    set_f64_bits state (100 + index)
      (Int64.bits_of_float (42.0 +. float_of_int index))
  done;
  for index = 0 to state_cells fixture - 1 do
    set_f64_bits state (200 + index)
      (Int64.bits_of_float (77.0 +. float_of_int index))
  done

let check_result_sentinel label state fixture =
  for index = 0 to output_cells fixture - 1 do
    check
      (label ^ " keeps output " ^ string_of_int index)
      (f64_bits state (100 + index)
       = Int64.bits_of_float (42.0 +. float_of_int index))
  done;
  for index = 0 to state_cells fixture - 1 do
    check
      (label ^ " keeps next state " ^ string_of_int index)
      (f64_bits state (200 + index)
       = Int64.bits_of_float (77.0 +. float_of_int index))
  done

let dynamic_effort dims =
  let inner = dims.timesteps * dims.v_heads * dims.value_dim * dims.key_dim in
  let rows = dims.timesteps * dims.v_heads * dims.value_dim in
  let heads = dims.timesteps * dims.v_heads in
  (4 * inner) + (2 * rows) + heads

let op =
  VM.GATED_DELTA_RULE_FP
    (0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13)

let code = [|op; VM.STOP|]
let op_only = [|op|]

let make_state ?(limit = 1_000_000) ?(strict_values = false)
    ?(output_base = 100) ?(state_dst_base = 200) ?(q_base = 300)
    ?(k_base = 400) ?(v_base = 500) ?(log_decay_base = 600)
    ?(beta_base = 700) ?(state_base = 800) fixture =
  let state =
    VM.create_state
      ~limit
      ~strict_values
      ~caller:"caller"
      ~origin:"origin"
      ~address:"contract"
      ~value:Z.zero
      ~storage:(Hashtbl.create 0)
      ()
  in
  set_int_reg state 0 output_base;
  set_int_reg state 1 state_dst_base;
  set_int_reg state 2 q_base;
  set_int_reg state 3 k_base;
  set_int_reg state 4 v_base;
  set_int_reg state 5 log_decay_base;
  set_int_reg state 6 beta_base;
  set_int_reg state 7 state_base;
  set_int_reg state 8 fixture.dims.timesteps;
  set_int_reg state 9 fixture.dims.q_heads;
  set_int_reg state 10 fixture.dims.k_heads;
  set_int_reg state 11 fixture.dims.v_heads;
  set_int_reg state 12 fixture.dims.key_dim;
  set_int_reg state 13 fixture.dims.value_dim;
  set_fixture state q_base fixture.q;
  set_fixture state k_base fixture.k;
  set_fixture state v_base fixture.v;
  set_fixture state log_decay_base fixture.log_decay;
  set_fixture state beta_base fixture.beta;
  set_fixture state state_base fixture.initial_state;
  state

let run_fixture ?limit ?output_base ?state_dst_base ?q_base ?k_base ?v_base
    ?log_decay_base ?beta_base ?state_base fixture =
  let state =
    make_state
      ?limit
      ?output_base
      ?state_dst_base
      ?q_base
      ?k_base
      ?v_base
      ?log_decay_base
      ?beta_base
      ?state_base
      fixture
  in
  state, VM.run state code

let check_golden () =
  List.iter
    (fun fixture ->
      let state, ok = run_fixture fixture in
      check (fixture.name ^ " succeeds") ok;
      check_cells
        (fixture.name ^ " recurrent output")
        state
        100
        (fixture_bits fixture.expected_output);
      check_cells
        (fixture.name ^ " next recurrent state")
        state
        200
        (fixture_bits fixture.expected_state))
    fixtures

let check_state_in_place_alias () =
  let fixture = nonzero_state_multiple_timesteps in
  let state, ok = run_fixture ~state_dst_base:800 fixture in
  check "state in-place alias succeeds" ok;
  check_cells "state in-place output" state 100 (fixture_bits fixture.expected_output);
  check_cells "state in-place next state" state 800 (fixture_bits fixture.expected_state)

let check_alias_rejections () =
  let fixture = zero_state_one_timestep in
  let state = make_state ~state_dst_base:101 fixture in
  set_f64_bits state 100 (Int64.bits_of_float 42.0);
  check "output/state overlap rejects" (not (VM.run state code));
  check "output/state overlap keeps output" (f64_bits state 100 = Int64.bits_of_float 42.0);
  let state = make_state fixture in
  set_int_reg state 2 100;
  set_f64_bits state 100 (Int64.bits_of_float 42.0);
  check "input/output alias rejects" (not (VM.run state code));
  check "input/output alias keeps output" (f64_bits state 100 = Int64.bits_of_float 42.0);
  let state = make_state ~state_dst_base:801 fixture in
  set_f64_bits state 801 (Int64.bits_of_float 42.0);
  check "partial state alias rejects" (not (VM.run state code));
  check "partial state alias keeps state" (f64_bits state 801 = Int64.bits_of_float 42.0)

let check_missing_and_nonfinite_reverts () =
  let fixture = zero_state_one_timestep in
  let state = make_state fixture in
  set_result_sentinel state fixture;
  Hashtbl.remove state.VM.memory.data 300;
  check "missing q rejects" (not (VM.run state code));
  check_result_sentinel "missing q" state fixture;
  let state = make_state fixture in
  set_result_sentinel state fixture;
  set_f64_bits state 600 0x7ff0000000000000L;
  check "nonfinite log decay rejects" (not (VM.run state code));
  check_result_sentinel "nonfinite log decay" state fixture;
  let state = make_state fixture in
  set_result_sentinel state fixture;
  set_f64_bits state 600 (Int64.bits_of_float min_float);
  check "positive log decay rejects" (not (VM.run state code));
  check_result_sentinel "positive log decay" state fixture

let check_invalid_shape_and_effort_revert () =
  let fixture = zero_state_one_timestep in
  let state = make_state fixture in
  set_result_sentinel state fixture;
  set_int_reg state 8 0;
  check "zero timesteps rejects" (not (VM.run state code));
  check_result_sentinel "zero timesteps" state fixture;
  let exact_limit = 200 + dynamic_effort fixture.dims in
  let state = make_state ~limit:(exact_limit - 1) fixture in
  set_result_sentinel state fixture;
  check "one-under effort rejects" (not (VM.run state op_only));
  check_result_sentinel "one-under effort" state fixture;
  let state = make_state ~limit:exact_limit fixture in
  check "exact effort succeeds" (VM.run state op_only);
  check_cells "exact effort output" state 100 (fixture_bits fixture.expected_output);
  let state = make_state fixture in
  set_result_sentinel state fixture;
  set_int_reg state 8 max_int;
  set_int_reg state 11 max_int;
  check "product overflow rejects" (not (VM.run state code));
  check_result_sentinel "product overflow" state fixture;
  let state = make_state fixture in
  set_f64_bits state 100 (Int64.bits_of_float 42.0);
  set_int_reg state 0 max_int;
  check "address overflow rejects" (not (VM.run state code));
  check "address overflow keeps sentinel" (f64_bits state 100 = Int64.bits_of_float 42.0)

let check_late_add_mul_overflow_reverts () =
  let fixture = late_add_mul_overflow in
  let state = make_state fixture in
  set_result_sentinel state fixture;
  check "late add/mul overflow rejects" (not (VM.run state code));
  check_result_sentinel "late add/mul overflow" state fixture;
  let state = make_state ~state_dst_base:800 fixture in
  set_f64_bits state 100 (Int64.bits_of_float 42.0);
  check "late add/mul overflow rejects with in-place state" (not (VM.run state code));
  check
    "late add/mul overflow keeps output"
    (f64_bits state 100 = Int64.bits_of_float 42.0);
  check_cells
    "late add/mul overflow keeps in-place state"
    state
    800
    (fixture_bits fixture.initial_state)

let check_strict_operands () =
  let state = make_state ~strict_values:true zero_state_one_timestep in
  state.VM.regs.(2) <- VM.VString "not-an-address";
  check "strict operands reject text q address" (not (VM.run state code))

let capability name capability_root =
  Req.{ name; root = capability_root }

let limits =
  Req.{
    max_model_bytes = 64;
    max_view_bytes = 64;
    max_session_bytes = 4096;
    max_scratch_bytes = 4096;
    max_output_bytes = 512;
    max_advance_effort = 10000;
  }

let requirement capabilities =
  Req.{
    vm_semantics_root = hex_root 'a';
    numerical_root = hex_root 'b';
    effort_root = hex_root 'c';
    capabilities;
    limits;
  }

let support capabilities =
  Req.{
    support_vm_semantics_root = hex_root 'a';
    support_numerical_roots = [hex_root 'b'];
    support_effort_roots = [hex_root 'c'];
    support_capabilities = capabilities;
    support_limits = limits;
  }

let admission_code =
  [|
    VM.JDEST 100;
    VM.LDI (0, VM.VInt (Z.of_int 100));
    VM.LDI (1, VM.VInt (Z.of_int 200));
    VM.LDI (2, VM.VInt (Z.of_int 300));
    VM.LDI (3, VM.VInt (Z.of_int 400));
    VM.LDI (4, VM.VInt (Z.of_int 500));
    VM.LDI (5, VM.VInt (Z.of_int 600));
    VM.LDI (6, VM.VInt (Z.of_int 700));
    VM.LDI (7, VM.VInt (Z.of_int 800));
    VM.LDI (8, VM.VInt Z.one);
    VM.LDI (9, VM.VInt Z.one);
    VM.LDI (10, VM.VInt Z.one);
    VM.LDI (11, VM.VInt Z.one);
    VM.LDI (12, VM.VInt (Z.of_int 2));
    VM.LDI (13, VM.VInt (Z.of_int 2));
    op;
    VM.STOP;
  |]

let check_capability_gate () =
  let cap = capability "sequence.delta-rule" (hex_root 'd') in
  (match
     Admission.of_inference_code_with_requirement
       ~support:(support [cap])
       ~requirement:(requirement [cap])
       admission_code
   with
   | Ok _ -> ()
   | Error error -> failwith (Admission.error_message error));
  match
    Admission.of_inference_code_with_requirement
      ~support:(support [])
      ~requirement:(requirement [])
      admission_code
  with
  | Error (Admission.Unsafe_error message) ->
    check
      "delta capability is named"
      (starts_with
         "inference opcode GATED_DELTA_RULE_FP at pc 15 requires capability sequence.delta-rule"
         message)
  | _ -> failwith "expected delta capability rejection"

let check_generic_admission_rejection () =
  let check_rejection label = function
    | Error (Admission.Unsafe_error message) ->
      check label (starts_with "consensus unsafe opcode GATED_DELTA_RULE_FP" message)
    | _ -> failwith ("expected " ^ label)
  in
  check_rejection "legacy rejects delta" (Admission.of_code [|op; VM.STOP|]);
  check_rejection "program rejects delta" (Admission.of_program [|op; VM.STOP|])

let check_wire_roundtrip () =
  let raw = Bytecode.encode [|op; VM.STOP|] in
  (match Bytecode.decode raw with
   | Ok [|decoded; VM.STOP|] -> check "delta bytecode roundtrip" (decoded = op)
   | Ok _ -> failwith "unexpected decoded delta code"
   | Error error -> failwith error);
  let asm =
    "GATED_DELTA_RULE_FP r0, r1, r2, r3, r4, r5, r6, r7, r8, r9, r10, r11, r12, r13\nSTOP"
  in
  check "delta assembler parse" (Assembler.parse asm = [|op; VM.STOP|]);
  check "delta assembler emit" (String.equal (Assembler.emit [|op; VM.STOP|]) asm)

let compiler_contract declaration =
  let open Lang in
  let args = List.init 14 (fun index -> EInt (Z.of_int index)) in
  let fn =
    {
      fn_name = "advance";
      fn_params = [];
      fn_ret = TVoid;
      fn_view = false;
      fn_pure = false;
      fn_payable = false;
      fn_nonreentrant = false;
      fn_vis = Public;
      fn_body = [SExpr (ECall ("gated_delta_rule_fp", args))];
    }
  in
  {
    declaration;
    name = "Delta";
    imports = [];
    structs = [];
    enums = [];
    consts = [];
    invariants_decl = [];
    state = [];
    events = [];
    errors = [];
    interfaces = [];
    implements = [];
    ctor = None;
    funcs = [fn];
  }

let check_compiler_surface () =
  let program_code = Gen.generate (compiler_contract Lang.ProgramDecl) in
  check
    "compiler emits delta"
    (Array.exists (function VM.GATED_DELTA_RULE_FP _ -> true | _ -> false) program_code);
  match Gen.generate (compiler_contract Lang.ContractDecl) with
  | _ -> failwith "delta should reject outside Program"
  | exception Gen.GenError (message, _) ->
    check
      "delta compiler rejection"
      (String.equal message "gated_delta_rule_fp is available only in Program")

let check_effects () =
  let effects = Program_effects.names (Program_effects.scan [|op|]) in
  check "delta effects" (effects = ["memory_read"; "memory_write"])

let store_cells base raw =
  fixture_bits raw
  |> List.mapi (fun index bits ->
    [|
      VM.LDI (14, VM.VInt (Z.of_int64 bits));
      VM.MSTORE (base + index, 14);
    |])
  |> Array.concat

let output_payload base count state =
  String.concat
    "|"
    [
      "base=" ^ string_of_int base;
      "length=" ^ string_of_int count;
      "values="
      ^ String.concat
          ","
          (List.init count (fun index ->
             match Hashtbl.find_opt state.VM.memory.data (base + index) with
             | Some (VM.VInt value) -> "int:" ^ Z.to_string value
             | Some _ -> failwith "unexpected output cell"
             | None -> ""));
    ]

let output_root ~target_root ~session_abi_root payload =
  Digestif.SHA256.(
    digest_string
      ("octra:inference:output\000"
       ^ target_root ^ "\000" ^ session_abi_root ^ "\000" ^ payload)
    |> to_hex)

let read_file path =
  let ic = open_in_bin path in
  Fun.protect
    ~finally:(fun () -> close_in_noerr ic)
    (fun () -> really_input_string ic (in_channel_length ic))

let f64le_cells state base count =
  let raw = Bytes.create (count * 8) in
  for index = 0 to count - 1 do
    let bits = f64_bits state (base + index) in
    for byte = 0 to 7 do
      Bytes.set
        raw
        ((index * 8) + byte)
        (Char.chr
           (Int64.to_int
              (Int64.logand
                 (Int64.shift_right_logical bits (byte * 8))
                 0xffL)))
    done
  done;
  Bytes.unsafe_to_string raw

let check_optional_bonsai_binding () =
  match Sys.getenv_opt "OCTRA_GATED_DELTA_EVIDENCE" with
  | None -> ()
  | Some evidence ->
    let binding = Filename.concat evidence "binding" in
    let file name = read_file (Filename.concat binding name) in
    let expected_output_sha =
      "bbf4208fac10547525d6a6aadac188b01414f104e38b5f01c4677fb7012e86c2"
    in
    let expected_state_sha =
      "e9e56282b8a3840a5e660f2799523eda426f7ff2e141419cd391d94c021621d1"
    in
    let fixture =
      {
        name = "Bonsai Qwen35 layer-0 binding";
        dims = {
          timesteps = 1;
          q_heads = 16;
          k_heads = 16;
          v_heads = 48;
          key_dim = 128;
          value_dim = 128;
        };
        q = file "q-l2.f64le";
        k = file "k-l2.f64le";
        v = file "v.f64le";
        log_decay = file "prepared-log-decay.f64le";
        beta = file "prepared-beta.f64le";
        initial_state = file "zero-recurrent-state.f64le";
        expected_output = file "expected-recurrent-output.f64le";
        expected_state = file "expected-next-recurrent-state.f64le";
      }
    in
    let state, ok =
      run_fixture
        ~limit:4_000_000
        ~output_base:100
        ~state_dst_base:10_000
        ~q_base:900_000
        ~k_base:903_000
        ~v_base:906_000
        ~log_decay_base:913_000
        ~beta_base:914_000
        ~state_base:915_000
        fixture
    in
    check "Bonsai binding succeeds" ok;
    check
      "Bonsai recurrent output sha"
      (String.equal
         (sha256 (f64le_cells state 100 (output_cells fixture)))
         expected_output_sha);
    check
      "Bonsai next recurrent state sha"
      (String.equal
         (sha256 (f64le_cells state 10_000 (state_cells fixture)))
         expected_state_sha)

let session_code =
  let fixture = zero_state_one_timestep in
  Array.concat
    [
      [|VM.JDEST Abi.advance_label|];
      store_cells 300 fixture.q;
      store_cells 400 fixture.k;
      store_cells 500 fixture.v;
      store_cells 600 fixture.log_decay;
      store_cells 700 fixture.beta;
      store_cells 800 fixture.initial_state;
      [|
        VM.LDI (0, VM.VInt (Z.of_int 100));
        VM.LDI (1, VM.VInt (Z.of_int 200));
        VM.LDI (2, VM.VInt (Z.of_int 300));
        VM.LDI (3, VM.VInt (Z.of_int 400));
        VM.LDI (4, VM.VInt (Z.of_int 500));
        VM.LDI (5, VM.VInt (Z.of_int 600));
        VM.LDI (6, VM.VInt (Z.of_int 700));
        VM.LDI (7, VM.VInt (Z.of_int 800));
        VM.LDI (8, VM.VInt Z.one);
        VM.LDI (9, VM.VInt Z.one);
        VM.LDI (10, VM.VInt Z.one);
        VM.LDI (11, VM.VInt Z.one);
        VM.LDI (12, VM.VInt (Z.of_int 2));
        VM.LDI (13, VM.VInt (Z.of_int 2));
        op;
        VM.LDI (0, VM.VInt (Z.of_int 100));
        VM.LDI (1, VM.VInt (Z.of_int (output_cells fixture)));
        VM.STOP;
      |];
    ]

let check_session_execution () =
  let fixture = zero_state_one_timestep in
  let cap = capability "sequence.delta-rule" (hex_root 'd') in
  let requirement = requirement [cap] in
  let support = support [cap] in
  let admitted = Inference_cert.admit ~support ~requirement session_code in
  let target =
    Target.{
      program_root = Target.program_root admitted;
      requirement_root = Req.root requirement;
      model_root = hex_root 'e';
      execution_descriptor_root = hex_root '1';
      store_root = hex_root '2';
      session_abi_root = Abi.v1_root;
      entrypoints = [{
        entry_name = Abi.advance_entrypoint;
        entry_label = Abi.advance_label;
      }];
    }
  in
  let owner = "range owner" in
  let owner_root = sha256 owner in
  let model =
    Model.{
      model_root = target.model_root;
      store_root = target.store_root;
      ranges = [{
        owner_root;
        offset = 0;
        length = String.length owner;
        encoding = "octets";
        shape_root = None;
      }];
    }
  in
  let pins =
    match
      Store.pin
        ~limits:requirement.limits
        ~read:(fun root -> if String.equal root owner_root then Some owner else None)
        model
    with
    | Ok pins -> pins
    | Error error -> failwith (Store.error_message error)
  in
  let request =
    Request.{
      schema = Abi.request_schema;
      target_root = Target.root target;
      entrypoint = Abi.advance_entrypoint;
      input_root = sha256 "";
      request_nonce = hex_root '5';
      max_output_bytes = 512;
      max_advance_effort = 10000;
    }
  in
  let plan =
    match Plan.create ~admitted ~target ~request ~model ~pins ~input:"" with
    | Ok plan -> plan
    | Error error -> failwith (Plan.error_message error)
  in
  match Execution.run ~plan () with
  | Ok result ->
    let state =
      VM.create_state
        ~limit:1_000_000
        ~caller:"caller"
        ~origin:"origin"
        ~address:"contract"
        ~value:Z.zero
        ~storage:(Hashtbl.create 0)
        ()
    in
    check "local session code succeeds" (VM.run state session_code);
    let expected =
      output_root
        ~target_root:(Target.root target)
        ~session_abi_root:Abi.v1_root
        (output_payload 100 (output_cells fixture) state)
    in
    check "session output root" (String.equal result.Execution.output_root expected);
    check_cells "session output" state 100 (fixture_bits fixture.expected_output);
    check_cells "session next state" state 200 (fixture_bits fixture.expected_state)
  | Error error -> failwith (Execution.error_message error)

let () =
  check_golden ();
  check_state_in_place_alias ();
  check_alias_rejections ();
  check_missing_and_nonfinite_reverts ();
  check_invalid_shape_and_effort_revert ();
  check_late_add_mul_overflow_reverts ();
  check_strict_operands ();
  check_capability_gate ();
  check_generic_admission_rejection ();
  check_wire_roundtrip ();
  check_compiler_surface ();
  check_effects ();
  check_session_execution ();
  check_optional_bonsai_binding ()
