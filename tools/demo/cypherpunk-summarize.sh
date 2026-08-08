#!/usr/bin/env bash
# Repeatable cypherpunk-manifesto summarization demo through the Octra
# inference stack: the octra-inference producer tokenizes the prompt,
# packs the Q1 model, emits per-transition session bundles, and streams
# the full response through the LiteNode VM harness (inference_admit,
# our octra_vm chassis with the native kernels).
#
# Usage:
#   tools/demo/cypherpunk-summarize.sh [output-dir]
#
# Env overrides:
#   PRODUCER_DIR   path to the octra-inference source worktree (default
#                  resolves ~/codex-worktrees/octra-inference-*)
#   MODEL_GGUF     path to the Bonsai-27B-Q1_0 GGUF (default
#                  /home/exedev/models/bonsai/Bonsai-27B-Q1_0.gguf)
#   VM_HARNESS     path to the LiteNode inference_admit executable
#   MAX_NEW_TOKENS tokens to generate (default 40)
set -euo pipefail

root="${1:-$PWD/tools/demo/out/cypherpunk-summarize-$(date -u +%Y%m%dT%H%M%SZ)}"
mkdir -p "$root"

producer_dir="${PRODUCER_DIR:-$(ls -d "$HOME"/codex-worktrees/octra-inference-* 2>/dev/null | head -1)}"
: "${producer_dir:?PRODUCER_DIR not found}"
producer="$producer_dir/target/release/octra-inference"

packed="${PACKED_MODEL:-/home/exedev/evidence/cypherpunk-demo/packed}"
repo_root="$(cd "$(dirname "$0")/../.." && pwd)"
vm="${VM_HARNESS:-$repo_root/_build/default/tools/bonsai_layer0_vm_harness/bonsai_layer0_vm_harness.exe}" 
max_new_tokens="${MAX_NEW_TOKENS:-40}"

echo "demo root:     $root"
echo "producer:      $producer"
echo "model gguf:    $model_gguf"
echo "vm harness:    $vm"
echo "max new tokens: $max_new_tokens"

if [ ! -x "$producer" ]; then
  echo "== building octra-inference producer (first run)"
  (cd "$producer_dir" && "$HOME/.cargo/env" 2>/dev/null; . "$HOME/.cargo/env" && cargo build --release)
fi

if [ ! -f "$packed/manifest.cjson" ]; then
  model_gguf="${MODEL_GGUF:?MODEL_GGUF required when PACKED_MODEL is absent}"
  echo "== packing Q1 model"
  mkdir -p "$packed"
  "$producer" pack-gguf-q1 "$model_gguf" --out "$packed"
fi

prompt='Summarize the first few paragraphs of the cypherpunk manifesto in a few sentences: Privacy is necessary for an open society in the electronic age. Privacy is not secrecy. A private matter is something one does not want the whole world to know; a secret matter is something one does not want anybody to know. Privacy is the power to selectively reveal oneself to the world. We must defend our own privacy if we expect to have any. We must come together and create systems which allow anonymous transactions to take place. Cypherpunks write code. We publish our code so that our fellow cypherpunks may practice our art and play with our code. We believe that cryptographic systems allow individuals to act autonomously.'

echo "== generating through the LiteNode VM harness"
"$producer" generate "$packed" \
  --vm-harness "$vm" \
  --prompt "$prompt" \
  --max-new-tokens "$max_new_tokens" \
  --qwen35-chat --disable-thinking \
  --quiet \
  --out "$root/result.cjson" >"$root/stdout.json" 2>"$root/stderr.log"

echo "== demo complete"
ROOT="$root" python3 - <<'EOF'
import json, os

with open(os.path.join(os.environ["ROOT"], "result.cjson")) as f:
    result = json.load(f)

status = result.get("status")
evidence = result.get("evidence", {})
generated = evidence.get("generated_text") or result.get("generated_text")
tokens = evidence.get("generated_tokens", [])
print(f"status:             {status}")
print(f"generated tokens:   {len(tokens)}")
print(f"model root:         {evidence.get('model_root')}")
print(f"session report sha: {evidence.get('session_report_sha256')}")
print(f"transition count:   {evidence.get('transition_count')}")
print()
print("=== generated summary ===")
print(generated)
EOF
sha256sum "$root/result.cjson" >"$root/SHA256SUMS"
echo
echo "evidence: $root/result.cjson"
