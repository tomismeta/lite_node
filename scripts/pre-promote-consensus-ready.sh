#!/usr/bin/env bash
# Pre-flight gate before landing any consensus_ready profile promotion.
#
# Rule: no consensus_ready promotion ships while the readiness suite is red.
# Full multi-OS matrix CI is intentionally out of scope here; this script only
# proves the local readiness-relevant suite is green.
#
# Usage (from repo root, with opam env active):
#   ./scripts/pre-promote-consensus-ready.sh
#
# Exit 0 only when every listed test exits 0.

set -euo pipefail

ROOT="$(cd "$(dirname "$0")/.." && pwd)"
cd "$ROOT"

if ! command -v dune >/dev/null 2>&1; then
  echo "error: dune not found; run: eval \$(opam env)" >&2
  exit 127
fi

# Readiness-relevant suite (conformance + numerics primitives that gate promote).
TESTS=(
  test/inference_conformance_template_test.exe
  test/inference_conformance_run_tool_test.exe
  test/inference_conformance_matrix_tool_test.exe
  test/inference_profile_catalog_tool_test.exe
  test/f32_load_primitive_test.exe
  test/fp_normalization_primitive_test.exe
  test/q1_g128_primitive_test.exe
  test/causal_depthwise_conv_primitive_test.exe
  test/gated_delta_rule_primitive_test.exe
  test/fp_activation_primitive_test.exe
)

echo "pre-promote-consensus-ready: building readiness suite"
dune build "${TESTS[@]}"

failed=0
for t in "${TESTS[@]}"; do
  echo "pre-promote-consensus-ready: dune exec $t"
  if ! dune exec "$t"; then
    echo "FAIL: $t" >&2
    failed=1
  else
    echo "ok: $t"
  fi
done

if [ "$failed" -ne 0 ]; then
  echo "pre-promote-consensus-ready: RED — do not land consensus_ready" >&2
  exit 1
fi

echo "pre-promote-consensus-ready: GREEN — readiness suite ok for promote"
exit 0
