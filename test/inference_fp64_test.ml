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


module Fp64 = Octra_vm.Inference_fp64

let check label condition =
  if not condition then failwith label

let expect_bits label actual expected =
  match actual with
  | Some bits -> check label (Int64.equal bits expected)
  | None -> failwith (label ^ " returned non-finite")

let expect_compare label actual expected =
  match actual with
  | Some value -> check label (value = expected)
  | None -> failwith (label ^ " returned non-finite")

let check_arithmetic_edges () =
  expect_bits
    "fp64 int conversion"
    (Fp64.of_int 3)
    0x4008000000000000L;
  expect_bits
    "fp64 add min-subnormal"
    (Fp64.add 1L 1L)
    2L;
  expect_bits
    "fp64 positive zero plus negative zero"
    (Fp64.add 0L Int64.min_int)
    0L;
  expect_bits
    "fp64 negative zero plus negative zero"
    (Fp64.add Int64.min_int Int64.min_int)
    Int64.min_int;
  check
    "fp64 add overflow rejects"
    (Fp64.add 0x7fefffffffffffffL 0x7fefffffffffffffL = None);
  expect_bits
    "fp64 sub finite"
    (Fp64.sub 0x4008000000000000L 0x4000000000000000L)
    0x3ff0000000000000L;
  expect_bits
    "fp64 sub exact cancellation"
    (Fp64.sub 0x3ff0000000000000L 0x3ff0000000000000L)
    0L;
  expect_bits
    "fp64 sub negative zero minus positive zero"
    (Fp64.sub Int64.min_int 0L)
    Int64.min_int;
  check
    "fp64 sub overflow rejects"
    (Fp64.sub
       0x7fefffffffffffffL
       (Int64.logor Int64.min_int 0x7fefffffffffffffL)
     = None);
  check
    "fp64 sub non-finite rejects"
    (Fp64.sub 0x7ff0000000000000L 0x3ff0000000000000L = None);
  expect_bits
    "fp64 mul underflow tie to even"
    (Fp64.mul 1L 0x3fe0000000000000L)
    0L;
  expect_bits
    "fp64 mul finite"
    (Fp64.mul 0x3ff8000000000000L 0x4000000000000000L)
    0x4008000000000000L;
  check
    "fp64 mul overflow rejects"
    (Fp64.mul 0x7fefffffffffffffL 0x4000000000000000L = None)

let check_division_edges () =
  expect_bits
    "fp64 div finite"
    (Fp64.div 0x4008000000000000L 0x4000000000000000L)
    0x3ff8000000000000L;
  expect_bits
    "fp64 div one third"
    (Fp64.div 0x3ff0000000000000L 0x4008000000000000L)
    0x3fd5555555555555L;
  expect_bits
    "fp64 div negative third"
    (Fp64.div 0xbff0000000000000L 0x4008000000000000L)
    0xbfd5555555555555L;
  expect_bits
    "fp64 div signed zero"
    (Fp64.div Int64.min_int 0x4000000000000000L)
    Int64.min_int;
  expect_bits
    "fp64 div negative zero by negative"
    (Fp64.div Int64.min_int 0xc000000000000000L)
    0L;
  expect_bits
    "fp64 div min-subnormal underflow tie to even"
    (Fp64.div 1L 0x4000000000000000L)
    0L;
  expect_bits
    "fp64 div min normal to subnormal"
    (Fp64.div 0x0010000000000000L 0x4000000000000000L)
    0x0008000000000000L;
  expect_bits
    "fp64 div min normal by half"
    (Fp64.div 0x0010000000000000L 0x3fe0000000000000L)
    0x0020000000000000L;
  expect_bits
    "fp64 div max by max"
    (Fp64.div 0x7fefffffffffffffL 0x7fefffffffffffffL)
    0x3ff0000000000000L;
  expect_bits
    "fp64 div one by max"
    (Fp64.div 0x3ff0000000000000L 0x7fefffffffffffffL)
    0x0004000000000000L;
  check
    "fp64 div by zero rejects"
    (Fp64.div 0x3ff0000000000000L 0L = None);
  check
    "fp64 div overflow rejects"
    (Fp64.div 0x7fefffffffffffffL 0x3fe0000000000000L = None)

let check_sqrt_edges () =
  expect_bits
    "fp64 sqrt four"
    (Fp64.sqrt 0x4010000000000000L)
    0x4000000000000000L;
  expect_bits
    "fp64 sqrt two"
    (Fp64.sqrt 0x4000000000000000L)
    0x3ff6a09e667f3bcdL;
  expect_bits
    "fp64 sqrt positive zero"
    (Fp64.sqrt 0L)
    0L;
  expect_bits
    "fp64 sqrt negative zero"
    (Fp64.sqrt Int64.min_int)
    Int64.min_int;
  expect_bits
    "fp64 sqrt min-subnormal"
    (Fp64.sqrt 1L)
    0x1e60000000000000L;
  expect_bits
    "fp64 sqrt max finite"
    (Fp64.sqrt 0x7fefffffffffffffL)
    0x5fefffffffffffffL;
  check
    "fp64 sqrt negative finite rejects"
    (Fp64.sqrt 0xbff0000000000000L = None);
  check
    "fp64 sqrt non-finite rejects"
    (Fp64.sqrt 0x7ff0000000000000L = None)

let check_compare_edges () =
  expect_compare
    "fp64 compare equal zeros"
    (Fp64.compare 0L Int64.min_int)
    0;
  expect_compare
    "fp64 compare min-subnormal greater zero"
    (Fp64.compare 1L 0L)
    1;
  expect_compare
    "fp64 compare negative subnormal less negative zero"
    (Fp64.compare (Int64.logor Int64.min_int 1L) Int64.min_int)
    (-1);
  expect_compare
    "fp64 compare max finite greater one"
    (Fp64.compare 0x7fefffffffffffffL 0x3ff0000000000000L)
    1;
  expect_compare
    "fp64 compare negative order"
    (Fp64.compare 0xc000000000000000L 0xbff0000000000000L)
    (-1);
  check
    "fp64 compare non-finite rejects"
    (Fp64.compare 0x7ff0000000000000L 0x3ff0000000000000L = None)

let () =
  check_arithmetic_edges ();
  check_division_edges ();
  check_sqrt_edges ();
  check_compare_edges ()
