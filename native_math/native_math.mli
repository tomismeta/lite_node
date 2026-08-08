val q1_g128_linear :
  int64 array -> float array -> string -> int -> int -> int -> int -> int

val fp64_exp_nonpositive : int64 -> int64 array -> int

val fp64_log1p_nonnegative : int64 -> int64 array -> int

val fp64_sin_cos : int64 -> int64 array -> int

val fp64_rope_pair_sin_cos : int64 -> int64 -> int -> int -> int64 array -> int

val fp64_add : int64 -> int64 -> int64 array -> int

val fp64_sub : int64 -> int64 -> int64 array -> int

val fp64_mul : int64 -> int64 -> int64 array -> int

val fp64_div : int64 -> int64 -> int64 array -> int

val fp64_silu : int64 array -> int -> int

val gdn_kernel :
  int64 array -> int64 array -> int64 array -> int64 array -> int64 array ->
  int64 array -> int64 -> int64 -> int64 -> int64 -> int64 -> int64 ->
  int64 -> int64 array -> int

val candidate_root :
  int64 array -> 'a array -> string -> string array -> int64 array -> int
