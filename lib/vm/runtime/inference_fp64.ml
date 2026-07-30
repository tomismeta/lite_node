(* SPDX-License-Identifier: BSD-3-Clause *)
(* Copyright (c) 2023-2026 Octra Labs <dev@octra.org> *)

let sign_mask = Int64.min_int
let fraction_mask = 0x000fffffffffffffL
let hidden_bit = Z.shift_left Z.one 52
let max_significand = Z.pred (Z.shift_left Z.one 53)
let min_normal_significand = Z.shift_left Z.one 52

type finite_value = {
  negative : bool;
  significand : Z.t;
  exponent : int;
}

let negative bits =
  not (Int64.equal (Int64.logand bits sign_mask) 0L)

let exponent_bits bits =
  Int64.to_int
    (Int64.logand
       0x7ffL
       (Int64.shift_right_logical bits 52))

let fraction_bits bits =
  Int64.logand bits fraction_mask

let finite bits =
  exponent_bits bits <> 0x7ff

let zero negative =
  if negative then sign_mask else 0L

let negate bits =
  Int64.logxor bits sign_mask

let decode bits =
  let exponent = exponent_bits bits in
  if exponent = 0x7ff then
    None
  else
    let fraction = Z.of_int64 (fraction_bits bits) in
    let significand, exponent =
      if exponent = 0 then
        fraction, -1074
      else
        Z.add hidden_bit fraction, exponent - 1075
    in
    Some { negative = negative bits; significand; exponent }

let round_shift_right_even value shift =
  if shift <= 0 then
    Z.shift_left value (-shift)
  else
    let divisor = Z.shift_left Z.one shift in
    let quotient, remainder = Z.ediv_rem value divisor in
    let doubled = Z.shift_left remainder 1 in
    match Z.compare doubled divisor with
    | -1 -> quotient
    | 1 -> Z.succ quotient
    | _ ->
      if Z.testbit quotient 0 then Z.succ quotient else quotient

let round_scaled_integer significand shift =
  if shift >= 0 then
    Z.shift_left significand shift
  else
    round_shift_right_even significand (-shift)

let compose ~negative ~exponent significand =
  let exponent_bits = Int64.shift_left (Int64.of_int exponent) 52 in
  let fraction = Z.sub significand min_normal_significand in
  let fraction_bits = Z.to_int64 fraction in
  let magnitude = Int64.logor exponent_bits fraction_bits in
  if negative then Int64.logor sign_mask magnitude else magnitude

let compose_subnormal ~negative fraction =
  let magnitude = Z.to_int64 fraction in
  if negative then Int64.logor sign_mask magnitude else magnitude

let round_positive ~negative significand exponent =
  if Z.sign significand = 0 then
    Some (zero negative)
  else
    let floor_log2 = Z.numbits significand - 1 + exponent in
    if floor_log2 > 1023 then
      None
    else if floor_log2 < -1022 then
      let fraction = round_scaled_integer significand (exponent + 1074) in
      if Z.sign fraction = 0 then
        Some (zero negative)
      else if Z.lt fraction min_normal_significand then
        Some (compose_subnormal ~negative fraction)
      else
        Some (compose ~negative ~exponent:1 min_normal_significand)
    else
      let encoded_exponent = floor_log2 + 1023 in
      let rounded =
        round_scaled_integer significand (exponent - floor_log2 + 52)
      in
      if Z.gt rounded max_significand then
        let encoded_exponent = encoded_exponent + 1 in
        if encoded_exponent >= 0x7ff then
          None
        else
          Some (compose ~negative ~exponent:encoded_exponent min_normal_significand)
      else
        Some (compose ~negative ~exponent:encoded_exponent rounded)

let signed_value value =
  if value.negative then Z.neg value.significand else value.significand

let add left right =
  match decode left, decode right with
  | Some left, Some right ->
    if Z.sign left.significand = 0 && Z.sign right.significand = 0 then
      Some (zero (left.negative && right.negative))
    else
      let exponent = min left.exponent right.exponent in
      let left_value =
        Z.shift_left (signed_value left) (left.exponent - exponent)
      in
      let right_value =
        Z.shift_left (signed_value right) (right.exponent - exponent)
      in
      let sum = Z.add left_value right_value in
      if Z.sign sum = 0 then
        Some (zero false)
      else
        round_positive
          ~negative:(Z.sign sum < 0)
          (Z.abs sum)
          exponent
  | _ -> None

let mul left right =
  match decode left, decode right with
  | Some left, Some right ->
    let negative = left.negative <> right.negative in
    if Z.sign left.significand = 0 || Z.sign right.significand = 0 then
      Some (zero negative)
    else
      round_positive
        ~negative
        (Z.mul left.significand right.significand)
        (left.exponent + right.exponent)
  | _ -> None
