/* Native Q1-G128 linear kernel, bit-exact mirror of the LiteNode scalar
   semantics in contract_vm.ml.

   Block layout (18 bytes per 128-weight group):
     [0..1]  little-endian binary16 scale
     [2..17] 128 lsb-first sign bits (sign bit 1 -> +scale, 0 -> -scale)

   Loop order: row, col, block, item. The accumulator starts at +0.0 and
   every product uses IEEE-754 binary64 roundTiesToEven multiply and add.

   This file MUST be compiled with -ffp-contract=off (no FMA) and without
   any fast-math flags so the IEEE results are bit-identical to the sealed
   scalar semantics.

   The OCaml side passes lhs/out as int64 arrays holding binary64 bit
   patterns (matching the VM's bit-cell storage); the C side reinterprets
   them as doubles without any conversion. */

#include <cmath>
#include <cstdint>
#include <cstring>

#include <caml/alloc.h>
#include <caml/memory.h>
#include <caml/mlvalues.h>

namespace {

// Exact binary16 -> binary64 bit pattern, mirroring
// Inference_fp64.of_binary16. Returns false for NaN/Inf (exponent 0x1f).
inline bool fp16_to_f64_bits(uint16_t bits, uint64_t* out) {
  const uint64_t sign = (bits & 0x8000u) ? 0x8000000000000000ull : 0ull;
  const uint32_t exponent = (bits >> 10) & 0x1f;
  const uint32_t fraction = bits & 0x03ff;
  if (exponent == 0) {
    if (fraction == 0) {
      *out = sign;
      return true;
    }
    int top = 0;
    for (int bit = 1; bit <= 9; ++bit) {
      if (fraction & (1u << bit)) top = bit;
    }
    const uint64_t exponent_bits = static_cast<uint64_t>(top + 999) << 52;
    const uint64_t significand =
        (static_cast<uint64_t>(fraction) << (52 - top)) & 0x000fffffffffffffull;
    *out = sign | exponent_bits | significand;
    return true;
  }
  if (exponent == 0x1f) {
    return false;
  }
  const uint64_t exponent_bits = static_cast<uint64_t>(exponent + 1008) << 52;
  const uint64_t fraction_bits = static_cast<uint64_t>(fraction) << 42;
  *out = sign | exponent_bits | fraction_bits;
  return true;
}

}  // namespace

extern "C" {

CAMLprim value octra_native_q1_g128_linear(value lhs_arr, value out_arr,
                                           value q1_bytes, value v_off,
                                           value v_m, value v_k, value v_n) {
  CAMLparam5(lhs_arr, out_arr, q1_bytes, v_off, v_m);
  CAMLxparam2(v_k, v_n);

  const int64_t off = Long_val(v_off);
  const int64_t m = Long_val(v_m);
  const int64_t k = Long_val(v_k);
  const int64_t n = Long_val(v_n);
  const uint64_t group = 128;
  const uint64_t block_bytes = 18;
  const uint64_t blocks_per_output = static_cast<uint64_t>(k) / group;
  const uint64_t blocks = static_cast<uint64_t>(n) * blocks_per_output;

  const uint8_t* q1 =
      reinterpret_cast<const uint8_t*>(String_val(q1_bytes));

  // Decode all scales first (mirrors decode_q1_g128_scale_bits); a
  // non-finite scale rejects the whole opcode before any output write.
  // All exits go through CAMLreturn so the roots chain stays valid.
  uint64_t* scale_bits = new uint64_t[blocks == 0 ? 1 : blocks];
  bool ok = true;
  for (uint64_t block = 0; block < blocks; ++block) {
    const uint64_t block_offset =
        static_cast<uint64_t>(off) + (block * block_bytes);
    const uint16_t bits = static_cast<uint16_t>(q1[block_offset]) |
                          (static_cast<uint16_t>(q1[block_offset + 1]) << 8);
    if (!fp16_to_f64_bits(bits, &scale_bits[block])) {
      ok = false;
      break;
    }
  }
  if (!ok) {
    delete[] scale_bits;
    CAMLreturn(Val_int(-1));
  }

  for (int64_t row = 0; row < m; ++row) {
    for (int64_t col = 0; col < n; ++col) {
      double acc = 0.0;
      bool cell_ok = true;
      for (uint64_t block = 0; block < blocks_per_output; ++block) {
        const uint64_t q1_block =
            (static_cast<uint64_t>(col) * blocks_per_output) + block;
        const uint64_t scale = scale_bits[q1_block];
        const uint64_t block_offset =
            static_cast<uint64_t>(off) + (q1_block * block_bytes);
        for (uint64_t item = 0; item < group; ++item) {
          const uint8_t sign_byte = q1[block_offset + 2 + (item >> 3)];
          const uint64_t signed_scale =
              ((sign_byte >> (item & 7)) & 1) == 1
                  ? scale
                  : (scale ^ 0x8000000000000000ull);
          double scale_value;
          std::memcpy(&scale_value, &signed_scale, sizeof(scale_value));
          const int64_t lhs_bits =
              Int64_val(Field(lhs_arr, (row * k) + (block * group) + item));
          double lhs_value;
          std::memcpy(&lhs_value, &lhs_bits, sizeof(lhs_value));
          const double next = acc + (lhs_value * scale_value);
          // Scalar parity: a non-finite intermediate rejects the whole
          // opcode. NaN/Inf intermediates can never return to a finite
          // accumulator, so the per-cell final check is equivalent.
          if (!std::isfinite(next)) cell_ok = false;
          acc = next;
        }
      }
      if (!cell_ok) {
        delete[] scale_bits;
        CAMLreturn(Val_int(-1));
      }
      Store_double_field(out_arr, (row * n) + col, acc);
    }
  }

  delete[] scale_bits;
  CAMLreturn(Val_int(0));
}

}  // extern "C"
