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

let of_binary16 bits =
  if bits < 0 || bits > 0xffff then
    None
  else
    let sign =
      if bits land 0x8000 = 0 then 0L else sign_mask
    in
    let exponent = (bits lsr 10) land 0x1f in
    let fraction = bits land 0x03ff in
    match exponent, fraction with
    | 0, 0 -> Some sign
    | 0, _ ->
      let top = ref 0 in
      for bit = 1 to 9 do
        if fraction land (1 lsl bit) <> 0 then top := bit
      done;
      let exponent_bits =
        Int64.shift_left (Int64.of_int (!top + 999)) 52
      in
      let significand =
        Int64.shift_left (Int64.of_int fraction) (52 - !top)
      in
      let fraction_bits =
        Int64.logand significand fraction_mask
      in
      Some (Int64.logor sign (Int64.logor exponent_bits fraction_bits))
    | 31, _ -> None
    | _ ->
      let exponent_bits =
        Int64.shift_left (Int64.of_int (exponent + 1008)) 52
      in
      let fraction_bits =
        Int64.shift_left (Int64.of_int fraction) 42
      in
      Some (Int64.logor sign (Int64.logor exponent_bits fraction_bits))

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

let square bits =
  mul bits bits

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

let positive bits =
  match compare bits 0L with
  | Some cmp -> cmp > 0
  | None -> false

let one_bits = 0x3ff0000000000000L

let inverse_sqrt bits =
  if positive bits then
    match sqrt bits with
    | Some root -> div one_bits root
    | None -> None
  else
    None

let exp_fraction_bits = 256

let exp_one =
  Z.shift_left Z.one exp_fraction_bits

let exp_ln_two =
  Z.of_string "80260960185991308862233904206310070533990667611589946606122867505419956976172"

let exp_min_input =
  Z.neg (Z.mul (Z.of_int 750) exp_one)

let exp_taylor_terms = 80

let round_signed_ratio_even numerator denominator =
  if Z.sign numerator < 0 then
    Z.neg (round_ratio_even (Z.neg numerator) denominator)
  else
    round_ratio_even numerator denominator

let round_signed_shift_right_even value shift =
  if shift <= 0 then
    Z.shift_left value (-shift)
  else if Z.sign value < 0 then
    Z.neg (round_shift_right_even (Z.neg value) shift)
  else
    round_shift_right_even value shift

let fixed_of_value value =
  let magnitude =
    round_scaled_integer
      value.significand
      (value.exponent + exp_fraction_bits)
  in
  if value.negative then Z.neg magnitude else magnitude

let fixed_mul left right =
  round_signed_shift_right_even (Z.mul left right) exp_fraction_bits

let fixed_div_int value divisor =
  round_signed_ratio_even value (Z.of_int divisor)

let exp_reduced_nonpositive value =
  let rec loop index term sum =
    if index > exp_taylor_terms || Z.sign term = 0 then
      sum
    else
      let next = fixed_div_int (fixed_mul term value) index in
      loop (index + 1) next (Z.add sum next)
  in
  loop 1 exp_one exp_one

let normalize_exp_reduction quotient remainder =
  let rec lower quotient remainder =
    if Z.sign remainder > 0 then
      lower
        (quotient + 1)
        (Z.sub remainder exp_ln_two)
    else
      quotient, remainder
  in
  let rec raise quotient remainder =
    if Z.lt remainder (Z.neg exp_ln_two) then
      raise
        (quotient - 1)
        (Z.add remainder exp_ln_two)
    else
      quotient, remainder
  in
  let quotient, remainder = lower quotient remainder in
  raise quotient remainder

let exp_nonpositive bits =
  match compare bits 0L, decode bits with
  | Some cmp, Some value when cmp <= 0 ->
    if Z.sign value.significand = 0 then
      Some one_bits
    else
      let fixed = fixed_of_value value in
      if Z.leq fixed exp_min_input then
        Some 0L
      else
        let quotient =
          Z.to_int (Z.div (Z.neg fixed) exp_ln_two)
        in
        let remainder =
          Z.add fixed (Z.mul (Z.of_int quotient) exp_ln_two)
        in
        let quotient, remainder =
          normalize_exp_reduction quotient remainder
        in
        let output = exp_reduced_nonpositive remainder in
        if Z.sign output <= 0 then
          None
        else
          round_positive_ratio
            ~negative:false
            output
            Z.one
            (-(exp_fraction_bits + quotient))
  | _ -> None

(* Q256 fixed-point exp for nonpositive fixed values. Returns the fixed-point
   result (2^256-scaled) or None on arithmetic failure. *)
let exp_nonpositive_fixed fixed =
  if Z.sign fixed = 0 then
    Some exp_one
  else if Z.leq fixed exp_min_input then
    Some Z.zero
  else
    let quotient =
      Z.to_int (Z.div (Z.neg fixed) exp_ln_two)
    in
    let remainder =
      Z.add fixed (Z.mul (Z.of_int quotient) exp_ln_two)
    in
    let quotient, remainder =
      normalize_exp_reduction quotient remainder
    in
    let output = exp_reduced_nonpositive remainder in
    if Z.sign output <= 0 then
      None
    else
      Some (round_signed_shift_right_even output (exp_fraction_bits + quotient))

(* Protocol ln(2) in Q256 fixed point (same constant as the exp core). *)
let ln_two = exp_ln_two

(* Two pi in Q256 fixed point (checked-in constant). *)
let two_pi =
  Z.of_string
    "727543153783532648560469885555459725306786754656784859917544302235877788932370"

(* ln(base) in Q256 fixed point for finite base > 1: base = m * 2^(e_total)
   with m in [1,2); ln(base) = e_total * ln2 + log1p(m - 1). *)
let ln_positive_fixed bits =
  match decode bits with
  | Some value when not value.negative && Z.sign value.significand <> 0 ->
    let m_minus_one_fixed =
      Z.shift_left
        (Z.sub value.significand min_normal_significand)
        (exp_fraction_bits - 52)
    in
    let e_total = value.exponent + 52 in
    let log_m =
      if Z.sign m_minus_one_fixed = 0 then
        Z.zero
      else
        let t_fixed = m_minus_one_fixed in
        let two_plus_t =
          Z.add (Z.shift_left Z.one (exp_fraction_bits + 1)) t_fixed
        in
        let u =
          round_signed_ratio_even
            (Z.shift_left t_fixed exp_fraction_bits)
            two_plus_t
        in
        let u_squared = fixed_mul u u in
        let rec loop term denominator sum =
          if Z.sign term = 0 then
            sum
          else
            let sum = Z.add sum term in
            let next_denominator = denominator + 2 in
            if next_denominator > (2 * exp_taylor_terms) + 1 then
              sum
            else
              let next_term =
                fixed_div_int
                  (fixed_mul
                     (Z.mul term (Z.of_int denominator))
                     u_squared)
                  next_denominator
              in
              loop next_term next_denominator sum
        in
        Z.shift_left (loop u 1 Z.zero) 1
    in
    Some (Z.add (Z.mul (Z.of_int e_total) ln_two) log_m)
  | _ -> None

(* Deterministic sin/cos in Q256 fixed point for a fixed-point angle in
   [0, 2*pi): alternating Taylor series with the factorial recurrence, all
   integer arithmetic. Returns (sin_fixed, cos_fixed). *)
let sin_cos_fixed angle_fixed =
  let angle_squared = fixed_mul angle_fixed angle_fixed in
  let rec loop k sin_term cos_term sin_sum cos_sum =
    if k > 100 || (Z.sign sin_term = 0 && Z.sign cos_term = 0) then
      sin_sum, cos_sum
    else
      let sin_sum = Z.add sin_sum sin_term in
      let cos_sum = Z.add cos_sum cos_term in
      let sin_denominator = ((2 * k) + 2) * ((2 * k) + 3) in
      let cos_denominator = ((2 * k) + 1) * ((2 * k) + 2) in
      let sin_next =
        fixed_div_int (fixed_mul sin_term angle_squared) sin_denominator
      in
      let cos_next =
        fixed_div_int (fixed_mul cos_term angle_squared) cos_denominator
      in
      loop (k + 1) (Z.neg sin_next) (Z.neg cos_next) sin_sum cos_sum
  in
  loop 0 angle_fixed exp_one Z.zero Z.zero

(* Deterministic sin and cos binary64 bits for any finite angle. Range
   reduces to [0, 2*pi) in Q256 fixed point, applies the alternating
   series, and rounds ties-to-even. Returns (sin_bits, cos_bits). *)
let sin_cos bits =
  match decode bits with
  | Some value when Z.sign value.significand = 0 ->
    Some (zero value.negative), Some one_bits
  | Some value ->
    let negative_angle = value.negative in
    let angle_fixed = fixed_of_value { value with negative = false } in
    let quadrant = Z.div angle_fixed two_pi in
    let reduced = Z.sub angle_fixed (Z.mul quadrant two_pi) in
    let sin_fixed, cos_fixed = sin_cos_fixed reduced in
    let to_bits sign fixed =
      if Z.sign fixed = 0 then
        Some (zero sign)
      else if Z.sign fixed < 0 then
        round_positive_ratio ~negative:true (Z.neg fixed) exp_one 0
      else
        round_positive_ratio ~negative:false fixed exp_one 0
    in
    let sin_negative = negative_angle in
    (match to_bits sin_negative sin_fixed, to_bits false cos_fixed with
     | Some sin_bits, Some cos_bits ->
       if negative_angle then
         Some (negate sin_bits), Some cos_bits
       else
         Some sin_bits, Some cos_bits
     | _ -> None, None)
  | _ -> None, None

(* Convert a Q256 fixed-point value to binary64 bits (ties-to-even). *)
let fixed_to_bits fixed =
  if Z.sign fixed = 0 then
    Some 0L
  else if Z.sign fixed < 0 then
    round_positive_ratio ~negative:true (Z.neg fixed) exp_one 0
  else
    round_positive_ratio ~negative:false fixed exp_one 0

(* theta = position / base^exponent in Q256 fixed point for nonnegative
   position and a fixed-point exponent term. *)
let rope_theta_fixed position _base_ln_fixed exponent_ratio =
  (* exponent_ratio is the fixed-point exponent e * ln(base) (>= 0);
     base^(-e) = exp(-e * ln base). *)
  match exp_nonpositive_fixed (Z.neg exponent_ratio) with
  | Some factor ->
    Some (fixed_mul (Z.mul (Z.of_int64 position) exp_one) factor)
  | None -> None

(* log(1 + t) for finite nonnegative t via the artanh series:
   log(1+t) = 2 * sum_k u^(2k+1)/(2k+1), u = t/(2+t) in (0, 1/3].
   All arithmetic is integer fixed-point at exp_fraction_bits, so the result
   is bit-exact across platforms for every finite t >= 0. *)
let log1p_nonnegative bits =
  match decode bits, compare bits 0L with
  | Some value, Some cmp when cmp >= 0 ->
    if Z.sign value.significand = 0 then
      Some 0L
    else
      let t_fixed = fixed_of_value value in
      let two_plus_t =
        Z.add (Z.shift_left Z.one (exp_fraction_bits + 1)) t_fixed
      in
      let u =
        round_signed_ratio_even
          (Z.shift_left t_fixed exp_fraction_bits)
          two_plus_t
      in
      let u_squared = fixed_mul u u in
      let rec loop term denominator sum =
        if Z.sign term = 0 then
          sum
        else
          let sum = Z.add sum term in
          let next_denominator = denominator + 2 in
          if next_denominator > (2 * exp_taylor_terms) + 1 then
            sum
          else
            (* term_{k+1} = u^(2k+3)/(2k+3) =
               term_k * u^2 * (2k+1)/(2k+3). *)
            let next_term =
              fixed_div_int
                (fixed_mul
                   (Z.mul term (Z.of_int denominator))
                   u_squared)
                next_denominator
            in
            loop next_term next_denominator sum
      in
      let series = loop u 1 Z.zero in
      round_positive_ratio
        ~negative:false
        (Z.shift_left series 1)
        (Z.shift_left Z.one exp_fraction_bits)
        0
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
