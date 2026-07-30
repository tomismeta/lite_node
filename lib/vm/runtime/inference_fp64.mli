(* SPDX-License-Identifier: BSD-3-Clause *)
(* Copyright (c) 2023-2026 Octra Labs <dev@octra.org> *)

val finite : int64 -> bool
val add : int64 -> int64 -> int64 option
val sub : int64 -> int64 -> int64 option
val mul : int64 -> int64 -> int64 option
val div : int64 -> int64 -> int64 option
val sqrt : int64 -> int64 option
val compare : int64 -> int64 -> int option
val of_int : int -> int64 option
val negate : int64 -> int64
