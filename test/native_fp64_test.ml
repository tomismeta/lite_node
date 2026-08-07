(* Bit-exact verification of the native Q256 fixed-point transcendental
   kernels (exp_nonpositive, log1p, sin/cos, rope theta+sin/cos) against
   the sealed Inference_fp64 scalar semantics across seeded and edge
   inputs. *)

let check name condition =
  if not condition then failwith ("FAIL: " ^ name)

let lfsr state =
  let x = ref state in
  let next () =
    let xv = !x in
    x := Int64.logxor xv (Int64.shift_right_logical xv 12);
    let xv = !x in
    x := Int64.logxor xv (Int64.shift_left xv 25);
    let xv = !x in
    x := Int64.logxor xv (Int64.shift_right_logical xv 27);
    Int64.mul !x (Int64.of_string "0x2545F4914F6CDD1D")
  in
  next

let bits_of f = Int64.bits_of_float f

let check_exp_nonpositive () =
  let module F = Octra_vm.Inference_fp64 in
  let inputs = [
    0.0; -0.0; -1e-300; -1e-20; -0.0001; -0.1; -0.5; -1.0; -2.0;
    -3.0; -5.0; -10.0; -100.0; -700.0; -745.0; -746.0; -750.0; -1000.0;
    -0.6931471805599453; (* -ln2 *)
    -1.3862943611198906; (* -2*ln2 *)
    -0.6931471805599452; (* just above -ln2 *)
    -0.6931471805599454; (* just below -ln2 *)
    -1e200; -1e-200;
  ] in
  let next = lfsr 0xFEEDFACE12345678L in
  let random = List.init 2000 (fun _ ->
    let exp = Int64.to_int (Int64.logand (next ()) 0x1fL) in
    let frac = Int64.logand (next ()) 0x000fffffffffffffL in
    let sign = if Int64.logand (next ()) 1L = 0L then 0L else Int64.min_int in
    let bits =
      if exp < 5 then frac
      else Int64.logor (Int64.shift_left (Int64.of_int (exp - 4 + 1023)) 52) frac
    in
    Int64.logor sign bits) in
  let all = List.map bits_of inputs @ random in
  let out = Array.make 1 0L in
  List.iteri (fun i bits ->
    let native_status = Native_math.fp64_exp_nonpositive bits out in
    match F.exp_nonpositive bits with
    | None ->
      if native_status = 0 then
        Printf.printf "exp[%d] native=ok ocaml=None (bits=%Lx val=%g)\n%!"
          i bits (Int64.float_of_bits bits);
      check (Printf.sprintf "exp[%d] None parity" i) (native_status <> 0)
    | Some expected ->
      if native_status <> 0 then
        Printf.printf "exp[%d] native=%d ocaml=Some (bits=%Lx val=%g)\n%!"
          i native_status bits (Int64.float_of_bits bits);
      check (Printf.sprintf "exp[%d] ok parity" i) (native_status = 0);
      check
        (Printf.sprintf "exp[%d] bit-exact (bits=%Lx)" i bits)
        (Int64.equal out.(0) expected)) all;
  Printf.printf "exp_nonpositive: %d cases bit-exact\n%!" (List.length all)

let check_log1p () =
  let module F = Octra_vm.Inference_fp64 in
  let inputs = [ 0.0; 1e-300; 1e-20; 0.001; 0.5; 1.0 ] in
  let next = lfsr 0x1111222233334444L in
  let random = List.init 2000 (fun _ ->
    let exp = Int64.to_int (Int64.logand (next ()) 0x1fL) in
    let frac = Int64.logand (next ()) 0x000fffffffffffffL in
    let bits =
      if exp < 20 then frac
      else Int64.logor (Int64.shift_left (Int64.of_int (exp - 20 + 1023)) 52) frac
    in
    Int64.logand bits 0x7fffffffffffffffL) in
  let all = List.map bits_of inputs @ random in
  let out = Array.make 1 0L in
  List.iteri (fun i bits ->
    let native_status = Native_math.fp64_log1p_nonnegative bits out in
    match F.log1p_nonnegative bits with
    | None ->
      if native_status = 0 then
        Printf.printf "log1p[%d] native=ok ocaml=None (bits=%Lx val=%g)\n%!"
          i bits (Int64.float_of_bits bits);
      check (Printf.sprintf "log1p[%d] None parity" i) (native_status <> 0)
    | Some expected ->
      if native_status <> 0 then
        Printf.printf "log1p[%d] native=%d ocaml=Some (bits=%Lx val=%g)\n%!"
          i native_status bits (Int64.float_of_bits bits);
      check (Printf.sprintf "log1p[%d] ok parity" i) (native_status = 0);
      if not (Int64.equal out.(0) expected) then
        Printf.printf "log1p[%d] mismatch native=%Lx ocaml=%Lx (bits=%Lx val=%g)\n%!"
          i out.(0) expected bits (Int64.float_of_bits bits);
      check (Printf.sprintf "log1p[%d] bit-exact" i) (Int64.equal out.(0) expected)) all;
  Printf.printf "log1p: %d cases bit-exact\n%!" (List.length all)

let check_sin_cos () =
  let module F = Octra_vm.Inference_fp64 in
  let inputs = [
    0.0; 0.1; 0.5; 1.0; 1.5707963267948966; 3.141592653589793;
    4.71238898038469; 6.283185307179586; 6.28; -0.5; -3.14;
    100.0; 1000.0; 1e6; 2.0 ** 40.0; 9.0e15;
  ] in
  let next = lfsr 0xABCDEF0123456789L in
  let random = List.init 2000 (fun _ ->
    let exp = Int64.to_int (Int64.logand (next ()) 0x2fL) in
    let frac = Int64.logand (next ()) 0x000fffffffffffffL in
    let sign = if Int64.logand (next ()) 1L = 0L then 0L else Int64.min_int in
    let bits =
      if exp < 10 then frac
      else Int64.logor (Int64.shift_left (Int64.of_int (exp - 10 + 1023)) 52) frac
    in
    Int64.logor sign bits) in
  let all = List.map bits_of inputs @ random in
  let out = Array.make 2 0L in
  List.iteri (fun i bits ->
    let native_status = Native_math.fp64_sin_cos bits out in
    if native_status <> 0 && i < 30 then
      Printf.printf "sincos[%d] native=%d (bits=%Lx val=%g)\n%!"
        i native_status bits (Int64.float_of_bits bits);
    match F.sin_cos bits with
    | None, None -> check (Printf.sprintf "sincos[%d] None parity" i) (native_status <> 0)
    | Some s, Some c ->
      check (Printf.sprintf "sincos[%d] ok parity" i) (native_status = 0);
      if not (Int64.equal out.(0) s) then
        Printf.printf "sincos[%d] sin native=%Lx ocaml=%Lx (bits=%Lx val=%g)\n%!"
          i out.(0) s bits (Int64.float_of_bits bits);
      check (Printf.sprintf "sincos[%d] sin bit-exact" i) (Int64.equal out.(0) s);
      check (Printf.sprintf "sincos[%d] cos bit-exact" i) (Int64.equal out.(1) c)
    | _ -> failwith "sincos partial result") all;
  Printf.printf "sin_cos: %d cases bit-exact\n%!" (List.length all)

let check_rope () =
  let module F = Octra_vm.Inference_fp64 in
  let rope_ref position base_bits i rot_dim =
    match F.ln_positive_fixed base_bits with
    | Some ln_fixed ->
      let er = Z.div (Z.mul (Z.of_int (2 * i)) ln_fixed) (Z.of_int rot_dim) in
      (match F.rope_theta_fixed position ln_fixed er with
       | Some theta_fixed ->
         (match F.fixed_to_bits theta_fixed with
          | Some theta_bits -> F.sin_cos theta_bits
          | None -> None, None)
       | None -> None, None)
    | None -> None, None
  in
  let cases = [
    0L, 10000.0, 0, 8;
    1L, 10000.0, 1, 8;
    7L, 10000.0, 3, 8;
    100L, 10000.0, 1, 4;
    0L, 2.0, 0, 2;
    5L, 1.0001, 1, 2;
    Int64.shift_left 1L 40, 10000.0, 2, 8;
    9007199254740991L, 10000.0, 3, 8;  (* 2^53-1 *)
    (-5L), 10000.0, 1, 8;
    3L, 1e10, 1, 16;
    42L, 65536.0, 5, 32;
  ] in
  let out = Array.make 2 0L in
  List.iteri (fun idx (position, base, i, rot) ->
    let base_bits = bits_of base in
    let native_status =
      Native_math.fp64_rope_pair_sin_cos position base_bits i rot out
    in
    match rope_ref position base_bits i rot with
    | None, None ->
      check (Printf.sprintf "rope[%d] None parity" idx) (native_status <> 0)
    | Some s, Some c ->
      check (Printf.sprintf "rope[%d] ok parity" idx) (native_status = 0);
      check (Printf.sprintf "rope[%d] sin bit-exact" idx) (Int64.equal out.(0) s);
      check (Printf.sprintf "rope[%d] cos bit-exact" idx) (Int64.equal out.(1) c)
    | _ -> failwith "rope partial result") cases;
  Printf.printf "rope: %d cases bit-exact\n%!" (List.length cases)

let check_binops () =
  let module F = Octra_vm.Inference_fp64 in
  let edge = [
    0L, 0L;
    Int64.min_int, Int64.min_int;
    Int64.min_int, 0L;
    0L, Int64.min_int;
    0x0010000000000000L, 0x0010000000000000L;  (* min normal *)
    0x0000000000000001L, 0x0000000000000001L;  (* min subnormal *)
    0x7fefffffffffffffL, 0x7fefffffffffffffL;  (* max *)
    0x7fefffffffffffffL, 0x4000000000000000L;  (* overflow mul *)
    0x3ff0000000000000L, 0L;                   (* div by zero *)
    0x3ff0000000000000L, 0x8000000000000000L;  (* div by -0 *)
    0x7ff0000000000000L, 0x3ff0000000000000L;  (* Inf input *)
    0x7ff8000000000000L, 0x3ff0000000000000L;  (* NaN input *)
    0x3ff0000000000000L, 0x3ff0000000000000L;
    0xbfd0000000000000L, 0x3fd0000000000000L;  (* -0.25 + 0.25 -> +0 *)
  ] in
  let next = lfsr 0xCAFEBABE12345678L in
  let random = List.init 3000 (fun _ ->
    let exp = Int64.logand (next ()) 0x7ffL in
    let frac = Int64.logand (next ()) 0x000fffffffffffffL in
    let sign = if Int64.logand (next ()) 1L = 0L then 0L else Int64.min_int in
    Int64.logor sign (Int64.logor (Int64.shift_left exp 52) frac)) in
  let ops = [
    "add", Native_math.fp64_add, F.add;
    "sub", Native_math.fp64_sub, F.sub;
    "mul", Native_math.fp64_mul, F.mul;
    "div", Native_math.fp64_div, F.div;
  ] in
  List.iter (fun (name, native_f, ocaml_f) ->
    let cases = edge @
      (List.map (fun x -> x, Int64.bits_of_float 0.5) random) @
      (List.map (fun x -> Int64.bits_of_float 2.0, x) random) in
    let out = Array.make 1 0L in
    List.iteri (fun i (a, b) ->
      let st = native_f a b out in
      match ocaml_f a b with
      | None -> check (Printf.sprintf "%s[%d] None parity" name i) (st <> 0)
      | Some expected ->
        check (Printf.sprintf "%s[%d] ok parity" name i) (st = 0);
        if not (Int64.equal out.(0) expected) then
          Printf.printf "%s[%d] native=%Lx ocaml=%Lx (a=%Lx b=%Lx)\n%!"
            name i out.(0) expected a b;
        check (Printf.sprintf "%s[%d] bit-exact" name i) (Int64.equal out.(0) expected))
      cases;
    Printf.printf "%s: %d cases bit-exact\n%!" name (List.length cases)) ops

let () =
  check_exp_nonpositive ();
  check_log1p ();
  check_sin_cos ();
  check_rope ();
  check_binops ();
  Printf.printf "native fp64 kernels: all bit-exact checks passed\n"
