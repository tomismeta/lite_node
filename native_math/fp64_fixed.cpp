/* Native Q256 fixed-point transcendental kernels, bit-exact mirrors of the
   LiteNode Inference_fp64 scalar semantics (exp_nonpositive, log1p,
   sin/cos, ln, rope theta). All arithmetic is integer fixed-point at 2^256
   with round-half-even, using 320-bit two's complement words. Values the
   VM feeds all fit: |exp| <= 750, t in [0,1], angles up to 2^53,
   ln(base) <= 710. */

#include <cstdint>
#include <cstring>

#include <caml/alloc.h>
#include <caml/memory.h>
#include <caml/mlvalues.h>

namespace {

constexpr int W = 5;  /* 320-bit words, two's complement */

using u320 = uint64_t[W];
using u640 = uint64_t[2 * W];

void set_zero(u320 a) {
  for (int i = 0; i < W; ++i) a[i] = 0;
}

void copy320(const u320 src, u320 dst) {
  for (int i = 0; i < W; ++i) dst[i] = src[i];
}

bool is_zero320(const u320 a) {
  for (int i = 0; i < W; ++i)
    if (a[i] != 0) return false;
  return true;
}

bool is_neg320(const u320 a) { return (a[W - 1] >> 63) != 0; }

void neg320(const u320 a, u320 out) {
  unsigned __int128 carry = 1;
  for (int i = 0; i < W; ++i) {
    unsigned __int128 s = (unsigned __int128)(~a[i]) + carry;
    out[i] = (uint64_t)s;
    carry = s >> 64;
  }
}

/* magnitude of a signed value into out */
void mag320(const u320 a, u320 out) {
  if (is_neg320(a)) neg320(a, out); else copy320(a, out);
}

int cmp_signed320(const u320 a, const u320 b) {
  bool an = is_neg320(a), bn = is_neg320(b);
  if (an != bn) return an ? -1 : 1;
  for (int i = W - 1; i >= 0; --i) {
    if (a[i] != b[i]) return a[i] < b[i] ? -1 : 1;
  }
  return 0;
}

void add320(const u320 a, const u320 b, u320 out) {
  unsigned __int128 carry = 0;
  for (int i = 0; i < W; ++i) {
    unsigned __int128 s = (unsigned __int128)a[i] + b[i] + carry;
    out[i] = (uint64_t)s;
    carry = s >> 64;
  }
}

void sub320(const u320 a, const u320 b, u320 out) {
  u320 nb;
  neg320(b, nb);
  add320(a, nb, out);
}

void shl320(const u320 a, int n, u320 out) {
  set_zero(out);
  if (n >= 320) return;
  int word = n / 64, bits = n % 64;
  for (int i = 0; i < W; ++i) {
    int src = i - word;
    uint64_t v = 0;
    if (src >= 0) {
      v = a[src] << bits;
      if (bits > 0 && src - 1 >= 0) v |= a[src - 1] >> (64 - bits);
    }
    out[i] = v;
  }
}

void shr320(const u320 a, int n, u320 out) {
  set_zero(out);
  if (n >= 320) return;
  int word = n / 64, bits = n % 64;
  for (int i = 0; i < W; ++i) {
    int src = i + word;
    uint64_t v = 0;
    if (src < W) {
      v = a[src] >> bits;
      if (bits > 0 && src + 1 < W) v |= a[src + 1] << (64 - bits);
    }
    out[i] = v;
  }
}

/* Round-half-even right shift of an UNSIGNED 320-bit value. */
void shr_round_even_unsigned320(const u320 a, int shift, u320 out) {
  if (shift <= 0) {
    copy320(a, out);
    return;
  }
  u320 q;
  shr320(a, shift, q);
  u320 shifted_q;
  shl320(q, shift, shifted_q);
  u320 rem;
  sub320(a, shifted_q, rem);
  u320 doubled;
  shl320(rem, 1, doubled);
  u320 divisor;
  set_zero(divisor);
  if (shift < 320) divisor[shift / 64] = uint64_t(1) << (shift % 64);
  int c = cmp_signed320(doubled, divisor);
  if (c > 0 || (c == 0 && (q[0] & 1))) {
    u320 one;
    set_zero(one);
    one[0] = 1;
    add320(q, one, out);
  } else {
    copy320(q, out);
  }
}

/* Signed round-half-even right shift (rounds the magnitude away from zero). */
void shr_round_even320(const u320 a, int shift, u320 out) {
  if (!is_neg320(a)) {
    shr_round_even_unsigned320(a, shift, out);
  } else {
    u320 mag;
    mag320(a, mag);
    shr_round_even_unsigned320(mag, shift, out);
    neg320(out, out);
  }
}

void mul320(const u320 a, const u320 b, u640 out) {
  for (int i = 0; i < 2 * W; ++i) out[i] = 0;
  for (int i = 0; i < W; ++i) {
    unsigned __int128 carry = 0;
    for (int j = 0; j < W; ++j) {
      unsigned __int128 cur =
          (unsigned __int128)a[i] * b[j] + out[i + j] + carry;
      out[i + j] = (uint64_t)cur;
      carry = cur >> 64;
    }
    unsigned __int128 c = carry;
    for (int j = i + W; c != 0 && j < 2 * W; ++j) {
      unsigned __int128 s = (unsigned __int128)out[j] + c;
      out[j] = (uint64_t)s;
      c = s >> 64;
    }
  }
}

void shr_round_even640_unsigned(const u640 a, int shift, u320 out) {
  if (shift <= 0) {
    for (int i = 0; i < W; ++i) out[i] = a[i];
    return;
  }
  u640 q;
  for (int i = 0; i < 2 * W; ++i) q[i] = 0;
  if (shift < 640) {
    int word = shift / 64, bits = shift % 64;
    for (int i = 0; i < 2 * W; ++i) {
      int src = i + word;
      uint64_t v = 0;
      if (src < 2 * W) {
        v = a[src] >> bits;
        if (bits > 0 && src + 1 < 2 * W) v |= a[src + 1] << (64 - bits);
      }
      q[i] = v;
    }
  }
  u640 shifted_q;
  for (int i = 0; i < 2 * W; ++i) shifted_q[i] = 0;
  if (shift < 640) {
    int word = shift / 64, bits = shift % 64;
    for (int i = 0; i < 2 * W; ++i) {
      int src = i - word;
      uint64_t v = 0;
      if (src >= 0) {
        v = q[src];
        if (bits > 0 && src - 1 >= 0) v |= q[src - 1] >> (64 - bits);
      }
      shifted_q[i] = v;
    }
  }
  /* remainder = a - shifted_q, then double */
  unsigned __int128 borrow = 0;
  uint64_t rem2[2 * W];
  for (int i = 0; i < 2 * W; ++i) rem2[i] = 0;
  for (int i = 0; i < 2 * W; ++i) {
    unsigned __int128 s = (unsigned __int128)a[i] - shifted_q[i] - borrow;
    rem2[i] = (uint64_t)s;
    borrow = (s >> 64) & 1;
  }
  uint64_t carry = 0;
  for (int i = 0; i < 2 * W; ++i) {
    uint64_t next = rem2[i] >> 63;
    rem2[i] = (rem2[i] << 1) | carry;
    carry = next;
  }
  int c = 0;
  if (shift < 640) {
    for (int i = 2 * W - 1; i >= 0; --i) {
      uint64_t d = (i == shift / 64) ? (uint64_t(1) << (shift % 64)) : 0;
      if (rem2[i] != d) {
        c = rem2[i] < d ? -1 : 1;
        break;
      }
    }
  } else {
    c = 1;
  }
  if (c > 0 || (c == 0 && (q[0] & 1))) {
    u320 one;
    set_zero(one);
    one[0] = 1;
    for (int i = 0; i < W; ++i) out[i] = q[i];
    add320(out, one, out);
  } else {
    for (int i = 0; i < W; ++i) out[i] = q[i];
  }
}

/* fixed_mul with sign (mirrors fixed_mul on signed Z values). */
void fixed_mul(const u320 a, const u320 b, u320 out) {
  bool an = is_neg320(a), bn = is_neg320(b);
  u320 am, bm;
  mag320(a, am);
  mag320(b, bm);
  u640 prod;
  mul320(am, bm, prod);
  shr_round_even640_unsigned(prod, 256, out);
  if (an != bn) neg320(out, out);
}

/* Round-half-even division of a signed 320-bit value by a small int. */
void div_small_round_even(const u320 a, uint64_t d, u320 out) {
  bool neg = is_neg320(a);
  u320 mag;
  mag320(a, mag);
  unsigned __int128 rem = 0;
  for (int i = W - 1; i >= 0; --i) {
    unsigned __int128 cur = (rem << 64) | mag[i];
    out[i] = (uint64_t)(cur / d);
    rem = cur % d;
  }
  unsigned __int128 doubled = rem << 1;
  if (doubled > d || (doubled == d && (out[0] & 1))) {
    u320 one;
    set_zero(one);
    one[0] = 1;
    add320(out, one, out);
  }
  if (neg) neg320(out, out);
}

void div_small_trunc(const u320 a, uint64_t d, u320 out) {
  bool neg = is_neg320(a);
  u320 mag;
  mag320(a, mag);
  unsigned __int128 rem = 0;
  for (int i = W - 1; i >= 0; --i) {
    unsigned __int128 cur = (rem << 64) | mag[i];
    out[i] = (uint64_t)(cur / d);
    rem = cur % d;
  }
  if (neg) neg320(out, out);
}

/* Round-half-even division: u640 (unsigned) numerator / u320 denominator. */
void div_round_even640(const u640 num, const u320 den, u320 out) {
  u320 q;
  set_zero(q);
  u640 r;
  for (int i = 0; i < 2 * W; ++i) r[i] = 0;
  for (int bit = 640 - 1; bit >= 0; --bit) {
    uint64_t carry = 0;
    uint64_t inbit = (num[bit / 64] >> (bit % 64)) & 1;
    for (int i = 0; i < 2 * W; ++i) {
      uint64_t next = r[i] >> 63;
      r[i] = (r[i] << 1) | carry | (i == 0 ? inbit : 0);
      carry = next;
    }
    if (bit < 320) {
      bool ge = true;
      for (int i = W - 1; i >= 0; --i) {
        if (r[i] != den[i]) {
          ge = r[i] > den[i];
          break;
        }
      }
      if (ge) {
        unsigned __int128 borrow = 0;
        for (int i = 0; i < W; ++i) {
          unsigned __int128 s = (unsigned __int128)r[i] - den[i] - borrow;
          r[i] = (uint64_t)s;
          borrow = (s >> 64) & 1;
        }
        q[bit / 64] |= uint64_t(1) << (bit % 64);
      }
    }
  }
  uint64_t carry = 0;
  uint64_t rem2[W];
  for (int i = 0; i < W; ++i) {
    uint64_t next = r[i] >> 63;
    rem2[i] = (r[i] << 1) | carry;
    carry = next;
  }
  int c = 0;
  for (int i = W - 1; i >= 0; --i) {
    if (rem2[i] != den[i]) {
      c = rem2[i] < den[i] ? -1 : 1;
      break;
    }
  }
  if (c > 0 || (c == 0 && (q[0] & 1))) {
    u320 one;
    set_zero(one);
    one[0] = 1;
    add320(q, one, out);
  } else {
    copy320(q, out);
  }
}

void const_one(u320 out) {
  set_zero(out);
  out[4] = 1;  /* 2^256 */
}

void const_ln_two(u320 out) {
  out[0] = 0x8a0d175b8baafa2cULL;
  out[1] = 0x40f343267298b62dULL;
  out[2] = 0xc9e3b39803f2f6afULL;
  out[3] = 0xb17217f7d1cf79abULL;
  out[4] = 0x0000000000000000ULL;
}

void const_two_pi(u320 out) {
  out[0] = 0x105df531d89cd912ULL;
  out[1] = 0x48127044533e63a0ULL;
  out[2] = 0x2633145c06e0e689ULL;
  out[3] = 0x487ed5110b4611a6ULL;
  out[4] = 0x0000000000000006ULL;
}

/* -750 * 2^256 as a signed value */
void const_exp_min_input(u320 out) {
  set_zero(out);
  out[4] = 750;
  neg320(out, out);
}

/* Convert a finite f64 bit pattern to its signed Q256 fixed-point value.
   Returns false when the magnitude exceeds 320 bits. */
bool bits_to_fixed(uint64_t bits, u320 out) {
  uint64_t exp = (bits >> 52) & 0x7ff;
  uint64_t frac = bits & 0x000fffffffffffffULL;
  int64_t e;
  uint64_t sig;
  if (exp == 0) {
    if (frac == 0) {
      set_zero(out);
      return true;
    }
    sig = frac;
    e = -1074;
  } else {
    sig = frac | 0x0010000000000000ULL;
    e = (int64_t)exp - 1075;
  }
  bool negative = (bits >> 63) != 0;
  int64_t shift = e + 256;
  set_zero(out);
  if (shift >= 0) {
    if (shift >= 320) return false;
    int word = (int)(shift / 64), bitsn = (int)(shift % 64);
    for (int i = W - 1; i >= 0; --i) {
      int src = i - word;
      uint64_t v = 0;
      if (src == 0) {
        if (bitsn == 0) {
          v = sig;
        } else {
          v = sig << bitsn;
        }
      } else if (src == 1 && bitsn > 0) {
        v = sig >> (64 - bitsn);
      }
      out[i] = v;
    }
  } else {
    int n = (int)(-shift);
    if (n >= 320) return false;
    u320 v;
    set_zero(v);
    v[0] = sig;
    shr_round_even_unsigned320(v, n, out);
  }
  if (negative) neg320(out, out);
  return true;
}

void exp_reduced_nonpositive(const u320 value, u320 out) {
  u320 term;
  const_one(term);
  copy320(term, out);
  for (int index = 1; index <= 80; ++index) {
    if (is_zero320(term)) break;
    u320 next;
    fixed_mul(term, value, next);
    div_small_round_even(next, index, next);
    copy320(next, term);
    add320(out, term, out);
  }
}

int bitlen320(const u320 a) {
  for (int i = W - 1; i >= 0; --i) {
    if (a[i] != 0) return 64 * i + (64 - __builtin_clzll(a[i]));
  }
  return 0;
}

/* Mirror of round_positive_ratio with denominator 2^den_scale: converts an
   unsigned magnitude fixed value (num) to f64 bits with round-half-even.
   Returns false on overflow (None). */
bool fixed_to_f64_bits_mag(const u320 num, int den_scale, int exponent,
                           uint64_t* out) {
  if (is_zero320(num)) {
    *out = 0;
    return true;
  }
  int floor_log2 = (bitlen320(num) - 1 - den_scale) + exponent;
  if (floor_log2 > 1023) return false;
  if (floor_log2 < -1022) {
    int shift = exponent + 1074 - den_scale;
    /* fraction = round_even(num * 2^shift) */
    u320 fraction;
    if (shift >= 0) {
      if (shift >= 320) return false;
      shl320(num, shift, fraction);
    } else {
      shr_round_even_unsigned320(num, -shift, fraction);
    }
    if (is_zero320(fraction)) {
      *out = 0;
      return true;
    }
    if (bitlen320(fraction) <= 52) {
      *out = fraction[0];  /* subnormal bits */
      return true;
    }
    /* fraction >= 2^52: normal with exponent 1 */
    *out = (uint64_t(1) << 52) | (fraction[0] & 0x000fffffffffffffULL);
    return true;
  }
  int encoded_exponent = floor_log2 + 1023;
  int shift = exponent - floor_log2 + 52 - den_scale;
  u320 rounded;
  if (shift >= 0) {
    if (shift >= 320) return false;
    shl320(num, shift, rounded);
  } else {
    shr_round_even_unsigned320(num, -shift, rounded);
  }
  if (bitlen320(rounded) > 53) {
    encoded_exponent += 1;
    if (encoded_exponent >= 0x7ff) return false;
    *out = (uint64_t(encoded_exponent) << 52);
    return true;
  }
  uint64_t magnitude =
      (uint64_t(encoded_exponent) << 52) |
      (rounded[0] & 0x000fffffffffffffULL);
  *out = magnitude;
  return true;
}

/* Signed version: handles the sign and returns (ok, bits). */
bool fixed_to_f64_bits(const u320 value, int den_scale, int exponent,
                       uint64_t* out) {
  bool neg = is_neg320(value);
  u320 mag;
  mag320(value, mag);
  if (!fixed_to_f64_bits_mag(mag, den_scale, exponent, out)) return false;
  if (neg) *out |= 0x8000000000000000ULL;
  return true;
}

/* exp of a nonpositive f64 bit pattern -> f64 bits. Returns false on None
   (non-finite input or arithmetic failure). */
bool kernel_exp_nonpositive(uint64_t bits, uint64_t* out) {
  uint64_t exp = (bits >> 52) & 0x7ff;
  if (exp == 0x7ff) return false;
  if ((bits >> 63) == 0 && (bits & 0x7fffffffffffffffULL) != 0) {
    /* positive inputs are out of the nonpositive domain (None) */
    return false;
  }
  if (exp >= 1087) {
    /* |x| >= 2^64: fixed <= -2^320 < exp_min_input -> underflow to 0 */
    *out = 0;
    return true;
  }
  if (exp <= 958 || exp == 0) {
    /* |x| < 2^-64: exp(x) rounds to 1.0 exactly (the second Taylor term is
       below half an ulp of 1.0). */
    *out = 0x3ff0000000000000ULL;
    return true;
  }
  u320 fixed;
  if (!bits_to_fixed(bits, fixed)) return false;
  if (is_zero320(fixed)) {
    *out = 0x3ff0000000000000ULL;  /* 1.0 */
    return true;
  }
  /* fixed <= 0 here (input nonpositive); magnitude */
  u320 mag;
  mag320(fixed, mag);
  u320 min_input;
  const_exp_min_input(min_input);
  u320 min_mag;
  mag320(min_input, min_mag);
  if (cmp_signed320(mag, min_mag) >= 0) {
    *out = 0;  /* underflow to zero */
    return true;
  }
  u320 ln2;
  const_ln_two(ln2);
  /* quotient = trunc(mag / ln2) */
  u320 q320;
  /* long division mag / ln2 truncating */
  {
    u320 q;
    set_zero(q);
    u640 r;
    for (int i = 0; i < 2 * W; ++i) r[i] = 0;
    for (int bit = 320 - 1; bit >= 0; --bit) {
      uint64_t carry = 0;
      uint64_t inbit = (mag[bit / 64] >> (bit % 64)) & 1;
      for (int i = 0; i < 2 * W; ++i) {
        uint64_t next = r[i] >> 63;
        r[i] = (r[i] << 1) | carry | (i == 0 ? inbit : 0);
        carry = next;
      }
      bool ge = true;
      for (int i = W - 1; i >= 0; --i) {
        if (r[i] != ln2[i]) {
          ge = r[i] > ln2[i];
          break;
        }
      }
      if (ge) {
        unsigned __int128 borrow = 0;
        for (int i = 0; i < W; ++i) {
          unsigned __int128 s = (unsigned __int128)r[i] - ln2[i] - borrow;
          r[i] = (uint64_t)s;
          borrow = (s >> 64) & 1;
        }
        q[bit / 64] |= uint64_t(1) << (bit % 64);
      }
    }
    copy320(q, q320);
  }
  /* quotient fits an int (<= ~1082) */
  int quotient = 0;
  {
    uint64_t qval = q320[0];
    if (q320[1] != 0 || q320[2] != 0 || q320[3] != 0 || q320[4] != 0)
      return false;
    quotient = (int)qval;
  }
  /* remainder = fixed + q*ln2 = -(mag - q*ln2) (nonpositive) */
  u320 qln2;
  set_zero(qln2);
  {
    u320 t;
    copy320(ln2, t);
    uint64_t mul = (uint64_t)quotient;
    u640 prod;
    set_zero(qln2);
    unsigned __int128 carry = 0;
    for (int i = 0; i < W; ++i) {
      unsigned __int128 cur = (unsigned __int128)ln2[i] * mul + carry;
      qln2[i] = (uint64_t)cur;
      carry = cur >> 64;
    }
  }
  /* magnitude of remainder = mag - q*ln2 (>= 0) */
  u320 rem_mag;
  sub320(mag, qln2, rem_mag);
  /* normalize: remainder in (-ln2, 0]; the OCaml raises while
     rem < -ln2 (mag > ln2). Defensive: adjust. */
  while (cmp_signed320(rem_mag, ln2) > 0) {
    sub320(rem_mag, ln2, rem_mag);
    quotient += 1;
  }
  /* remainder value = -rem_mag */
  u320 rem;
  neg320(rem_mag, rem);
  u320 output;
  exp_reduced_nonpositive(rem, output);
  /* bits = round_positive_ratio(output, 1, -(256+quotient)) */
  return fixed_to_f64_bits_mag(output, 0, -(256 + quotient), out);
}

/* Q256 fixed-point exp of a nonpositive fixed value (for ROPE). */
bool kernel_exp_nonpositive_fixed(const u320 fixed, u320 out) {
  if (is_zero320(fixed)) {
    const_one(out);
    return true;
  }
  u320 mag;
  mag320(fixed, mag);
  u320 min_input;
  const_exp_min_input(min_input);
  u320 min_mag;
  mag320(min_input, min_mag);
  if (cmp_signed320(mag, min_mag) >= 0) {
    set_zero(out);
    return true;
  }
  u320 ln2;
  const_ln_two(ln2);
  u320 q320;
  set_zero(q320);
  {
    u640 r;
    for (int i = 0; i < 2 * W; ++i) r[i] = 0;
    for (int bit = 320 - 1; bit >= 0; --bit) {
      uint64_t carry = 0;
      uint64_t inbit = (mag[bit / 64] >> (bit % 64)) & 1;
      for (int i = 0; i < 2 * W; ++i) {
        uint64_t next = r[i] >> 63;
        r[i] = (r[i] << 1) | carry | (i == 0 ? inbit : 0);
        carry = next;
      }
      bool ge = true;
      for (int i = W - 1; i >= 0; --i) {
        if (r[i] != ln2[i]) {
          ge = r[i] > ln2[i];
          break;
        }
      }
      if (ge) {
        unsigned __int128 borrow = 0;
        for (int i = 0; i < W; ++i) {
          unsigned __int128 s = (unsigned __int128)r[i] - ln2[i] - borrow;
          r[i] = (uint64_t)s;
          borrow = (s >> 64) & 1;
        }
        q320[bit / 64] |= uint64_t(1) << (bit % 64);
      }
    }
  }
  int quotient = 0;
  for (int i = W - 1; i >= 0; --i) {
    if (q320[i] != 0) {
      if (i != 0) return false;
      quotient = (int)q320[0];
    }
  }
  u320 qln2;
  unsigned __int128 carry = 0;
  for (int i = 0; i < W; ++i) {
    unsigned __int128 cur =
        (unsigned __int128)ln2[i] * (uint64_t)quotient + carry;
    qln2[i] = (uint64_t)cur;
    carry = cur >> 64;
  }
  u320 rem_mag;
  sub320(mag, qln2, rem_mag);
  while (cmp_signed320(rem_mag, ln2) > 0) {
    sub320(rem_mag, ln2, rem_mag);
    quotient += 1;
  }
  u320 rem;
  neg320(rem_mag, rem);
  u320 output;
  exp_reduced_nonpositive(rem, output);
  shr_round_even320(output, 256 + quotient, out);
  return true;
}

/* artanh-series log(1+t) for nonnegative t in [0,1]; returns the fixed
   result (2^256-scaled). Mirrors log1p_nonnegative's series core. */
bool kernel_log1p_fixed(uint64_t bits, u320 out) {
  uint64_t exp = (bits >> 52) & 0x7ff;
  if (exp == 0x7ff) return false;
  if ((bits >> 63) != 0) return false;
  u320 t_fixed;
  if (!bits_to_fixed(bits, t_fixed)) return false;
  if (is_zero320(t_fixed)) {
    set_zero(out);
    return true;
  }
  /* two_plus_t = 2^257 + t_fixed */
  u320 two_plus_t;
  set_zero(two_plus_t);
  two_plus_t[4] = 2;
  add320(two_plus_t, t_fixed, two_plus_t);
  /* u = round_even(t_fixed * 2^256 / two_plus_t) */
  u320 u;
  {
    u640 num;
    mul320(t_fixed, (u320){0, 0, 0, 0, 1}, num);
    /* shift t_fixed left 256 via mul with 2^256 */
    shr_round_even640_unsigned(num, 0, u);  /* placeholder, fixed below */
  }
  /* corrected u computation */
  {
    u320 shifted;
    shl320(t_fixed, 256, shifted);  /* t_fixed <= 2^256, 256+256 = 512 >= 320: handles small t */
    /* t_fixed < 2^256 so t_fixed << 256 fits 512 bits; shl320 caps at 320,
       which is insufficient for full t; but t < 2^256 and the quotient
       t/(2+t) < 1/2 so u fits; compute via 640-bit path instead: */
    u640 num;
    for (int i = 0; i < 2 * W; ++i) num[i] = 0;
    /* num = t_fixed * 2^256: t_fixed is < 2^256 -> shift into words 4..8 */
    for (int i = 0; i < W; ++i) num[i + 4] = t_fixed[i];
    /* numerator magnitude may exceed 320 bits; the quotient u < 1 so only
       the first 320 bits matter; use div_round_even640 with a 640-bit num */
    div_round_even640(num, two_plus_t, u);
  }
  u320 u_squared;
  fixed_mul(u, u, u_squared);
  u320 series;
  set_zero(series);
  u320 term;
  copy320(u, term);
  int denominator = 1;
  while (true) {
    if (is_zero320(term)) break;
    add320(series, term, series);
    int next_denominator = denominator + 2;
    if (next_denominator > 161) break;
    u320 next_term;
    /* term * u^2 * denominator / next_denominator */
    fixed_mul(term, u_squared, next_term);
    {
      u320 scaled;
      unsigned __int128 carry = 0;
      for (int i = 0; i < W; ++i) {
        unsigned __int128 cur =
            (unsigned __int128)next_term[i] * (uint64_t)denominator + carry;
        scaled[i] = (uint64_t)cur;
        carry = cur >> 64;
      }
      div_small_round_even(scaled, next_denominator, next_term);
    }
    copy320(next_term, term);
    denominator = next_denominator;
  }
  /* result = 2 * series */
  u320 doubled;
  shl320(series, 1, doubled);
  copy320(doubled, out);
  return true;
}

/* ln(base) fixed for finite base > 1: e_total*ln2 + log1p(m-1). */
bool kernel_ln_fixed(uint64_t bits, u320 out) {
  uint64_t exp = (bits >> 52) & 0x7ff;
  if (exp == 0x7ff) return false;
  if ((bits >> 63) != 0) return false;
  uint64_t frac = bits & 0x000fffffffffffffULL;
  if (exp == 0) {
    if (frac == 0) return false;
  }
  uint64_t sig = frac | 0x0010000000000000ULL;
  int64_t e_total = (int64_t)exp - 1075 + 52;
  /* m - 1 = sig - 2^52, fixed = (sig - 2^52) << (256 - 52) */
  u320 m_minus_one;
  set_zero(m_minus_one);
  uint64_t m1 = sig - 0x0010000000000000ULL;
  m_minus_one[0] = m1 << 12;
  m_minus_one[1] = m1 >> 52;
  u320 ln2;
  const_ln_two(ln2);
  u320 log_m;
  if (m1 == 0) {
    set_zero(log_m);
  } else {
    u320 two_plus_t;
    set_zero(two_plus_t);
    two_plus_t[4] = 2;
    add320(two_plus_t, m_minus_one, two_plus_t);
    u320 u;
    {
      u640 num;
      for (int i = 0; i < 2 * W; ++i) num[i] = 0;
      for (int i = 0; i < W; ++i) num[i + 4] = m_minus_one[i];
      div_round_even640(num, two_plus_t, u);
    }
    u320 u_squared;
    fixed_mul(u, u, u_squared);
    u320 series;
    set_zero(series);
    u320 term;
    copy320(u, term);
    int denominator = 1;
    while (true) {
      if (is_zero320(term)) break;
      add320(series, term, series);
      int next_denominator = denominator + 2;
      if (next_denominator > 161) break;
      u320 next_term;
      fixed_mul(term, u_squared, next_term);
      {
        u320 scaled;
        unsigned __int128 carry = 0;
        for (int i = 0; i < W; ++i) {
          unsigned __int128 cur =
              (unsigned __int128)next_term[i] * (uint64_t)denominator + carry;
          scaled[i] = (uint64_t)cur;
          carry = cur >> 64;
        }
        div_small_round_even(scaled, next_denominator, next_term);
      }
      copy320(next_term, term);
      denominator = next_denominator;
    }
    shl320(series, 1, log_m);
  }
  /* ln(base) = e_total * ln2 + log_m */
  u320 scaled_ln2;
  set_zero(scaled_ln2);
  {
    bool neg = e_total < 0;
    uint64_t mag = (uint64_t)(e_total < 0 ? -e_total : e_total);
    unsigned __int128 carry = 0;
    for (int i = 0; i < W; ++i) {
      unsigned __int128 cur = (unsigned __int128)ln2[i] * mag + carry;
      scaled_ln2[i] = (uint64_t)cur;
      carry = cur >> 64;
    }
    if (neg) neg320(scaled_ln2, scaled_ln2);
  }
  add320(scaled_ln2, log_m, out);
  return true;
}

/* sin/cos of a finite angle via range reduction and the alternating Taylor
   series. Returns (sin_bits, cos_bits). */
bool kernel_sin_cos(uint64_t bits, uint64_t* sin_out, uint64_t* cos_out) {
  uint64_t exp = (bits >> 52) & 0x7ff;
  if (exp == 0x7ff) return false;
  bool negative_angle = (bits >> 63) != 0;
  u320 angle_fixed;
  uint64_t mag_bits = bits & 0x7fffffffffffffffULL;
  if (!bits_to_fixed(mag_bits, angle_fixed)) {
    if (exp != 0x7ff && (exp == 0 || exp < 959)) {
      /* |angle| < 2^-64: the scalar fixed point underflows; the signed
         zero is negated once more for negative angles, so sin = +0. */
      *sin_out = 0;
      *cos_out = 0x3ff0000000000000ULL;
      return true;
    }
    return false;
  }
  if (is_zero320(angle_fixed)) {
    *sin_out = negative_angle ? 0x8000000000000000ULL : 0;
    *cos_out = 0x3ff0000000000000ULL;
    return true;
  }
  u320 two_pi;
  const_two_pi(two_pi);
  /* quadrant = angle / two_pi (truncating); reduced = angle - q*2pi */
  u320 quadrant;
  set_zero(quadrant);
  {
    u640 r;
    for (int i = 0; i < 2 * W; ++i) r[i] = 0;
    for (int bit = 320 - 1; bit >= 0; --bit) {
      uint64_t carry = 0;
      uint64_t inbit = (angle_fixed[bit / 64] >> (bit % 64)) & 1;
      for (int i = 0; i < 2 * W; ++i) {
        uint64_t next = r[i] >> 63;
        r[i] = (r[i] << 1) | carry | (i == 0 ? inbit : 0);
        carry = next;
      }
      bool ge = true;
      for (int i = W - 1; i >= 0; --i) {
        if (r[i] != two_pi[i]) {
          ge = r[i] > two_pi[i];
          break;
        }
      }
      if (ge) {
        unsigned __int128 borrow = 0;
        for (int i = 0; i < W; ++i) {
          unsigned __int128 s = (unsigned __int128)r[i] - two_pi[i] - borrow;
          r[i] = (uint64_t)s;
          borrow = (s >> 64) & 1;
        }
        quadrant[bit / 64] |= uint64_t(1) << (bit % 64);
      }
    }
  }
  /* reduced = angle - quadrant * two_pi */
  u320 qpi;
  {
    unsigned __int128 carry = 0;
    for (int i = 0; i < W; ++i) {
      unsigned __int128 cur =
          (unsigned __int128)two_pi[i] * quadrant[0] + carry;
      qpi[i] = (uint64_t)cur;
      carry = cur >> 64;
    }
  }
  u320 reduced;
  sub320(angle_fixed, qpi, reduced);
  /* series */
  u320 angle_squared;
  fixed_mul(reduced, reduced, angle_squared);
  u320 sin_sum;
  set_zero(sin_sum);
  u320 cos_sum;
  set_zero(cos_sum);
  u320 sin_term;
  copy320(reduced, sin_term);
  u320 cos_term;
  const_one(cos_term);
  for (int k = 0; k <= 100; ++k) {
    if (is_zero320(sin_term) && is_zero320(cos_term)) break;
    add320(sin_sum, sin_term, sin_sum);
    add320(cos_sum, cos_term, cos_sum);
    uint64_t sin_den = (uint64_t)((2 * k) + 2) * (uint64_t)((2 * k) + 3);
    uint64_t cos_den = (uint64_t)((2 * k) + 1) * (uint64_t)((2 * k) + 2);
    u320 sin_next;
    fixed_mul(sin_term, angle_squared, sin_next);
    div_small_round_even(sin_next, sin_den, sin_next);
    neg320(sin_next, sin_next);
    u320 cos_next;
    fixed_mul(cos_term, angle_squared, cos_next);
    div_small_round_even(cos_next, cos_den, cos_next);
    neg320(cos_next, cos_next);
    copy320(sin_next, sin_term);
    copy320(cos_next, cos_term);
  }
  uint64_t sin_bits, cos_bits;
  if (!fixed_to_f64_bits(sin_sum, 256, 0, &sin_bits)) return false;
  if (!fixed_to_f64_bits(cos_sum, 256, 0, &cos_bits)) return false;
  if (negative_angle) sin_bits ^= 0x8000000000000000ULL;
  *sin_out = sin_bits;
  *cos_out = cos_bits;
  return true;
}

/* theta = position / base^exponent (exponent = 2i/rot) in fixed point, then
   sin/cos. Mirrors the VM rope path. */
bool kernel_rope_pair_sin_cos(int64_t position, uint64_t base_bits, int i,
                              int rot_dim, uint64_t* sin_out,
                              uint64_t* cos_out) {
  u320 ln_base;
  if (!kernel_ln_fixed(base_bits, ln_base)) return false;
  u320 exponent_ratio;
  {
    /* (2i * ln_base) / rot_dim truncating */
    u320 scaled;
    unsigned __int128 carry = 0;
    uint64_t mul = (uint64_t)(2 * i);
    for (int w = 0; w < W; ++w) {
      unsigned __int128 cur = (unsigned __int128)ln_base[w] * mul + carry;
      scaled[w] = (uint64_t)cur;
      carry = cur >> 64;
    }
    div_small_trunc(scaled, (uint64_t)rot_dim, exponent_ratio);
  }
  /* factor = exp(-exponent_ratio) in fixed point */
  u320 neg_ratio;
  neg320(exponent_ratio, neg_ratio);
  u320 factor;
  if (!kernel_exp_nonpositive_fixed(neg_ratio, factor)) return false;
  /* theta_fixed = position * factor */
  u320 theta;
  {
    bool neg = position < 0;
    u320 pos_mag;
    set_zero(pos_mag);
    uint64_t pm = (uint64_t)(neg ? -position : position);
    pos_mag[0] = pm;
    u320 shifted_pos;
    shl320(pos_mag, 256, shifted_pos);
    fixed_mul(shifted_pos, factor, theta);
    if (neg) neg320(theta, theta);
  }
  uint64_t theta_bits;
  if (!fixed_to_f64_bits(theta, 256, 0, &theta_bits)) return false;
  return kernel_sin_cos(theta_bits, sin_out, cos_out);
}

}  // namespace

extern "C" {

/* All kernels write outputs into an int64 array passed from OCaml and
   return 0 on success, -1 on None/arithmetic failure. */
CAMLprim value octra_native_fp64_exp_nonpositive(value v_bits, value v_out) {
  CAMLparam2(v_bits, v_out);
  uint64_t bits = (uint64_t)Int64_val(v_bits);
  uint64_t out = 0;
  int status = kernel_exp_nonpositive(bits, &out) ? 0 : -1;
  if (status != 0) CAMLreturn(Val_int(status));
  Store_field(v_out, 0, caml_copy_int64((int64_t)out));
  CAMLreturn(Val_int(0));
}

CAMLprim value octra_native_fp64_log1p_nonnegative(value v_bits, value v_out) {
  CAMLparam2(v_bits, v_out);
  uint64_t bits = (uint64_t)Int64_val(v_bits);
  uint64_t exp = (bits >> 52) & 0x7ff;
  u320 fixed;
  if (!kernel_log1p_fixed(bits, fixed)) {
    if (exp != 0x7ff && (bits >> 63) == 0 && exp < 959) {
      /* t < 2^-64: the scalar fixed-point underflows; log1p(t) = 0. */
      Store_field(v_out, 0, caml_copy_int64(0));
      CAMLreturn(Val_int(0));
    }
    CAMLreturn(Val_int(-1));
  }
  uint64_t out = 0;
  if (!fixed_to_f64_bits(fixed, 256, 0, &out)) CAMLreturn(Val_int(-1));
  Store_field(v_out, 0, caml_copy_int64((int64_t)out));
  CAMLreturn(Val_int(0));
}

CAMLprim value octra_native_fp64_sin_cos(value v_bits, value v_out) {
  CAMLparam2(v_bits, v_out);
  uint64_t bits = (uint64_t)Int64_val(v_bits);
  uint64_t s = 0, c = 0;
  if (!kernel_sin_cos(bits, &s, &c)) CAMLreturn(Val_int(-1));
  Store_field(v_out, 0, caml_copy_int64((int64_t)s));
  Store_field(v_out, 1, caml_copy_int64((int64_t)c));
  CAMLreturn(Val_int(0));
}

CAMLprim value octra_native_fp64_rope_pair_sin_cos(value v_position,
                                                   value v_base, value v_i,
                                                   value v_rot, value v_out) {
  CAMLparam5(v_position, v_base, v_i, v_rot, v_out);
  int64_t position = Int64_val(v_position);
  uint64_t base = (uint64_t)Int64_val(v_base);
  int i = Int_val(v_i);
  int rot = Int_val(v_rot);
  uint64_t s = 0, c = 0;
  if (!kernel_rope_pair_sin_cos(position, base, i, rot, &s, &c))
    CAMLreturn(Val_int(-1));
  Store_field(v_out, 0, caml_copy_int64((int64_t)s));
  Store_field(v_out, 1, caml_copy_int64((int64_t)c));
  CAMLreturn(Val_int(0));
}

}  // extern "C"
