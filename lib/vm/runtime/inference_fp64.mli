(* SPDX-License-Identifier: BSD-3-Clause *)
(* Copyright (c) 2023-2026 Octra Labs <dev@octra.org> *)

val finite : int64 -> bool
val of_binary16 : int -> int64 option
val add : int64 -> int64 -> int64 option
val sub : int64 -> int64 -> int64 option
val mul : int64 -> int64 -> int64 option
val square : int64 -> int64 option
val div : int64 -> int64 -> int64 option
val sqrt : int64 -> int64 option
val compare : int64 -> int64 -> int option
val positive : int64 -> bool
val inverse_sqrt : int64 -> int64 option
val exp_nonpositive : int64 -> int64 option
val log1p_nonnegative : int64 -> int64 option
val sin_cos : int64 -> int64 option * int64 option
val ln_positive_fixed : int64 -> Z.t option
val fixed_to_bits : Z.t -> int64 option
val rope_theta_fixed : int64 -> Z.t -> Z.t -> Z.t option
val of_int : int -> int64 option
val negate : int64 -> int64
