(* Bit-exact verification of the native Q1-G128 linear kernel against the
   sealed scalar semantics (contract_vm.ml + Inference_fp64). The native
   kernel must reproduce the reference bit-for-bit across seeded and
   irregular shapes, and must reject non-finite scales before any write. *)

let check name condition =
  if not condition then failwith ("FAIL: " ^ name);
  Printf.printf "ok: %s\n" name

let lfsr state =
  (* xorshift64* style bit generator *)
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

let random_finite_bits next =
  (* finite f64 bit patterns: random exponent in [-10, 10], random fraction *)
  let exp = Int64.sub (Int64.logand (next ()) 0x1fL) 10L in
  let frac = Int64.logand (next ()) 0x000fffffffffffffL in
  let sign = if Int64.logand (next ()) 1L = 0L then 0L else Int64.min_int in
  let bits =
    if Int64.compare exp 0L < 0 then
      (* subnormal-ish *)
      frac
    else
      Int64.logor (Int64.shift_left (Int64.of_int (Int64.to_int exp + 1023)) 52) frac
  in
  Int64.logor sign bits

let random_binary16 next =
  (* finite binary16 bits (never exponent 0x1f), sometimes edge values *)
  let rec pick () =
    match Int64.to_int (Int64.logand (next ()) 7L) with
    | 0 -> 0x0000
    | 1 -> 0x8000
    | 2 -> 0x0001
    | 3 -> 0x03ff
    | 4 -> 0x7bff (* max finite *)
    | 5 -> 0x3c00 (* 1.0 *)
    | 6 ->
      let bits = Int64.to_int (Int64.logand (next ()) 0xffffL) in
      if (bits land 0x7c00) = 0x7c00 then pick () else bits
    | _ ->
      let bits = Int64.to_int (Int64.logand (next ()) 0x7fffL) in
      if (bits land 0x7c00) = 0x7c00 then pick () else bits
  in
  pick ()

let scalar_q1_linear (lhs : int64 array) (q1 : string) off m k n =
  let module F = Octra_vm.Inference_fp64 in
  let group = 128 in
  let block_bytes = 18 in
  let blocks_per_output = k / group in
  let blocks = n * blocks_per_output in
  let scales = Array.make blocks 0L in
  for block = 0 to blocks - 1 do
    let block_offset = off + (block * block_bytes) in
    let bits =
      Char.code q1.[block_offset]
      lor (Char.code q1.[block_offset + 1] lsl 8)
    in
    match F.of_binary16 bits with
    | None -> invalid_arg "nonfinite scale in scalar reference"
    | Some scale -> scales.(block) <- scale
  done;
  let output = Array.make (m * n) 0L in
  for row = 0 to m - 1 do
    for col = 0 to n - 1 do
      let acc = ref 0L in
      for block = 0 to blocks_per_output - 1 do
        let q1_block = (col * blocks_per_output) + block in
        let scale = scales.(q1_block) in
        let block_offset = off + (q1_block * block_bytes) in
        for item = 0 to group - 1 do
          let sign_byte = Char.code q1.[block_offset + 2 + (item lsr 3)] in
          let scale =
            if (sign_byte lsr (item land 7)) land 1 = 1 then scale
            else F.negate scale
          in
          let lhs_value = lhs.((row * k) + (block * group) + item) in
          match F.mul lhs_value scale with
          | Some product ->
            (match F.add !acc product with
             | Some next -> acc := next
             | None -> invalid_arg "scalar add failed")
          | None -> invalid_arg "scalar mul failed"
        done
      done;
      output.((row * n) + col) <- !acc
    done
  done;
  output

let check_case seed m k n =
  let next = lfsr seed in
  let lhs = Array.init (m * k) (fun _ -> random_finite_bits next) in
  let blocks = n * (k / 128) in
  let q1 = Bytes.create (blocks * 18) in
  for block = 0 to blocks - 1 do
    let scale = random_binary16 next in
    Bytes.set q1 (block * 18) (Char.chr (scale land 0xff));
    Bytes.set q1 ((block * 18) + 1) (Char.chr ((scale lsr 8) land 0xff));
    for b = 2 to 17 do
      Bytes.set q1 ((block * 18) + b) (Char.chr (Int64.to_int (Int64.logand (next ()) 0xffL)))
    done
  done;
  (* Fixtures may carry a random non-finite scale (0x7c00); when the native
     kernel rejects, the scalar reference must reject the same input
     (atomicity parity), otherwise the outputs must match bit-for-bit. *)
  let q1 = Bytes.to_string q1 in
  let got_floats = Array.make (m * n) 0.0 in
  let status = Native_math.q1_g128_linear lhs got_floats q1 0 m k n in
  if status <> 0 then begin
    let scalar_rejects =
      try
        ignore (scalar_q1_linear lhs q1 0 m k n);
        false
      with Invalid_argument _ -> true
    in
    check
      (Printf.sprintf "seed=%Ld m=%d k=%d n=%d rejection parity" seed m k n)
      scalar_rejects
  end else begin
    let expected = scalar_q1_linear lhs q1 0 m k n in
    let got = Array.map Int64.bits_of_float got_floats in
    check
      (Printf.sprintf "seed=%Ld m=%d k=%d n=%d bit-exact" seed m k n)
      (Array.for_all2 (fun a b -> Int64.equal a b) expected got)
  end

let check_rejection () =
  (* non-finite scale (inf) must reject before write *)
  let k = 128 and n = 1 and m = 2 in
  let lhs = Array.make (m * k) 0L in
  let q1 = Bytes.make 18 '\000' in
  Bytes.set q1 0 '\000';
  Bytes.set q1 1 '\x7c';
  let got_floats = Array.make (m * n) 0.0 in
  let status = Native_math.q1_g128_linear lhs got_floats (Bytes.to_string q1) 0 m k n in
  check "nonfinite scale rejected" (status <> 0)

let () =
  let cases = [
    0x123456789ABCDEF0L, 1, 128, 1;
    0xDEADBEEFCAFEBABEL, 3, 128, 2;
    0x0F0F0F0F0F0F0F0FL, 2, 384, 3;
    0x1111222233334444L, 8, 5120, 4;
    0xABCDEF0123456789L, 1, 5120, 5120;
    0xFEEDFACE12345678L, 5, 256, 7;
    0xCAFEF00DDEADBEEFL, 4, 1280, 128;
    0x0123456789ABCDEFL, 2, 512, 512;
  ] in
  List.iter (fun (seed, m, k, n) -> check_case seed m k n) cases;
  check_rejection ();
  Printf.printf "native Q1 kernel: all bit-exact checks passed\n"
