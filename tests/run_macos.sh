#!/bin/sh
set -eu
if [ "$(uname -s)" != Darwin ]; then
  echo 'This smoke suite requires macOS and the Cocoa widget set.' >&2
  exit 1
fi
LAZBUILD=${LAZBUILD:-lazbuild}
cd "$(dirname "$0")/.."
mkdir -p tests/results/macos
"$LAZBUILD" --ws=cocoa --add-package-link lazhidcontrol/lazhidcontrol.lpk
for project in tests/macos_smoke.lpi hidctl/hidctl.lpi; do
  if ! "$LAZBUILD" --ws=cocoa "$project" >tests/results/macos/build.log 2>&1; then
    tail -n 30 tests/results/macos/build.log
    exit 1
  fi
done
if ./tests/macos_smoke >tests/results/macos/smoke.log 2>&1; then
  cat tests/results/macos/smoke.log
else
  cat tests/results/macos/smoke.log
  exit 1
fi
./hidctl/hidctl --help >tests/results/macos/cli-help.log
./hidctl/hidctl idle power getdisplaygeometry clipset 'LazHID CLI é 漢 😀' clipget >tests/results/macos/cli.log
cat tests/results/macos/cli.log
grep -q '^idle [0-9]' tests/results/macos/cli.log
grep -q '^power ac\|^power battery' tests/results/macos/cli.log
grep -qF 'LazHID CLI é 漢 😀' tests/results/macos/cli.log
echo 'PASS: hidctl help, idle, power, display geometry and Unicode clipboard round trip'
