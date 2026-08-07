external q1_g128_linear :
  int64 array -> float array -> string -> int -> int -> int -> int -> int
  = "octra_native_q1_g128_linear_bytecode" "octra_native_q1_g128_linear"

external fp64_exp_nonpositive : int64 -> int64 array -> int
  = "octra_native_fp64_exp_nonpositive"

external fp64_log1p_nonnegative : int64 -> int64 array -> int
  = "octra_native_fp64_log1p_nonnegative"

external fp64_sin_cos : int64 -> int64 array -> int
  = "octra_native_fp64_sin_cos"

external fp64_rope_pair_sin_cos : int64 -> int64 -> int -> int -> int64 array -> int
  = "octra_native_fp64_rope_pair_sin_cos"


external fp64_add : int64 -> int64 -> int64 array -> int
  = "octra_native_fp64_add"

external fp64_sub : int64 -> int64 -> int64 array -> int
  = "octra_native_fp64_sub"

external fp64_mul : int64 -> int64 -> int64 array -> int
  = "octra_native_fp64_mul"

external fp64_div : int64 -> int64 -> int64 array -> int
  = "octra_native_fp64_div"
