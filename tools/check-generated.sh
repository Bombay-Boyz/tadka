#!/bin/sh
# Phase 8 discipline check (vision §6): every deriveDiagnostic-generated method
# body must be a direct call to a shared function — no logic in the Q splice.
# Fails the build if a generated golden fixture contains logic constructs.
set -e
gen="test/golden/fixtures/generated-parseerror.txt"

if [ ! -f "$gen" ]; then
  echo "check-generated: missing $gen (run the golden suite in GEN_GOLDEN mode)"; exit 1
fi

# No control-flow / binding / lambda constructs anywhere in the generated code.
if grep -nE '\bcase\b|\bif\b|\bthen\b|\belse\b|\blet\b|\bdo\b|\\[a-zA-Z_]' "$gen"; then
  echo "check-generated: FAIL — generated method body contains logic (above)"; exit 1
fi

# Exactly one 'where' — the instance header. A second would be a method where-clause.
n=$(grep -oE '\bwhere\b' "$gen" | wc -l | tr -d ' ')
if [ "$n" != "1" ]; then
  echo "check-generated: FAIL — expected one 'where' (instance header), found $n"; exit 1
fi

echo "check-generated: OK — all generated method bodies are direct calls"
