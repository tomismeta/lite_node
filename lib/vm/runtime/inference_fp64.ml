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

let round_ratio_even numerator denominator =
  let quotient, remainder = Z.ediv_rem numerator denominator in
  let doubled = Z.shift_left remainder 1 in
  match Z.compare doubled denominator with
  | -1 -> quotient
  | 1 -> Z.succ quotient
  | _ ->
    if Z.testbit quotient 0 then Z.succ quotient else quotient

let shifted_ratio numerator denominator shift =
  if shift >= 0 then
    Z.shift_left numerator shift, denominator
  else
    numerator, Z.shift_left denominator (-shift)

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

let floor_log2_ratio numerator denominator =
  let candidate = Z.numbits numerator - Z.numbits denominator in
  let cmp =
    if candidate >= 0 then
      Z.compare numerator (Z.shift_left denominator candidate)
    else
      Z.compare (Z.shift_left numerator (-candidate)) denominator
  in
  if cmp < 0 then candidate - 1 else candidate

let round_positive_ratio ~negative numerator denominator exponent =
  if Z.sign numerator = 0 then
    Some (zero negative)
  else
    let floor_log2 = floor_log2_ratio numerator denominator + exponent in
    if floor_log2 > 1023 then
      None
    else if floor_log2 < -1022 then
      let numerator, denominator =
        shifted_ratio numerator denominator (exponent + 1074)
      in
      let fraction = round_ratio_even numerator denominator in
      if Z.sign fraction = 0 then
        Some (zero negative)
      else if Z.lt fraction min_normal_significand then
        Some (compose_subnormal ~negative fraction)
      else
        Some (compose ~negative ~exponent:1 min_normal_significand)
    else
      let encoded_exponent = floor_log2 + 1023 in
      let numerator, denominator =
        shifted_ratio numerator denominator (exponent - floor_log2 + 52)
      in
      let rounded = round_ratio_even numerator denominator in
      if Z.sign rounded = 0 then
        Some (zero negative)
      else if Z.gt rounded max_significand then
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

let sub left right =
  add left (negate right)

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

let div left right =
  match decode left, decode right with
  | Some left, Some right ->
    let negative = left.negative <> right.negative in
    if Z.sign right.significand = 0 then
      None
    else if Z.sign left.significand = 0 then
      Some (zero negative)
    else
      round_positive_ratio
        ~negative
        left.significand
        right.significand
        (left.exponent - right.exponent)
  | _ -> None

let sqrt bits =
  match decode bits with
  | Some value ->
    if Z.sign value.significand = 0 then
      Some (zero value.negative)
    else if value.negative then
      None
    else
      let floor_log2 =
        Z.numbits value.significand - 1 + value.exponent
      in
      let output_floor_log2 =
        if floor_log2 >= 0 then
          floor_log2 / 2
        else
          -(((-floor_log2) + 1) / 2)
      in
      let output_exponent = output_floor_log2 - 52 in
      let scaled_exponent = value.exponent - (2 * output_exponent) in
      let radicand =
        Z.shift_left value.significand scaled_exponent
      in
      let root = Z.sqrt radicand in
      let remainder =
        Z.sub radicand (Z.mul root root)
      in
      let rounded =
        if Z.gt remainder root then Z.succ root else root
      in
      if Z.gt rounded max_significand then
        let encoded_exponent = output_floor_log2 + 1024 in
        if encoded_exponent >= 0x7ff then
          None
        else
          Some
            (compose
               ~negative:false
               ~exponent:encoded_exponent
               min_normal_significand)
      else
        Some
          (compose
             ~negative:false
             ~exponent:(output_floor_log2 + 1023)
             rounded)
  | _ -> None

let compare_magnitude left right =
  let exponent = min left.exponent right.exponent in
  let left_value =
    Z.shift_left left.significand (left.exponent - exponent)
  in
  let right_value =
    Z.shift_left right.significand (right.exponent - exponent)
  in
  Z.compare left_value right_value

let compare left right =
  match decode left, decode right with
  | Some left, Some right ->
    let left_zero = Z.sign left.significand = 0 in
    let right_zero = Z.sign right.significand = 0 in
    if left_zero && right_zero then
      Some 0
    else if left.negative <> right.negative then
      Some (if left.negative then -1 else 1)
    else
      let cmp = compare_magnitude left right in
      Some (if left.negative then -cmp else cmp)
  | _ -> None

let of_int value =
  let value = Z.of_int value in
  if Z.sign value = 0 then
    Some (zero false)
  else
    round_positive
      ~negative:(Z.sign value < 0)
      (Z.abs value)
      0
