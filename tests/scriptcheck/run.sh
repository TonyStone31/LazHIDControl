#!/bin/sh
# One hidctl script against the same small program, checked the same way on
# every system.  Linux: runs in its own nested X server (Xephyr and openbox),
# so the desk is left alone.  macOS: runs on the real screen; hidctl needs
# Accessibility.  Windows: run-windows.cmd does the same steps.
#
#   sh tests/scriptcheck/run.sh             build both, run, compare
#   WS=gtk2 sh tests/scriptcheck/run.sh     another widget set
#   LAZBUILD="lazbuild --pcp=..." ...       a lazbuild that needs options
set -u
cd "$(dirname "$0")/../.."
LAZBUILD=${LAZBUILD:-lazbuild}
OS=$(uname -s)
if [ "$OS" = Darwin ]; then WS=${WS:-cocoa}; else WS=${WS:-gtk2}; fi
OUT=tests/results/scriptcheck/$(echo "$OS" | tr 'A-Z' 'a-z')-$WS
mkdir -p "$OUT"
rm -f "$OUT"/*.png "$OUT"/result.txt

for project in tests/scriptcheck/scriptcheck.lpi hidctl/hidctl.lpi; do
  if ! $LAZBUILD --ws="$WS" "$project" >"$OUT/build.log" 2>&1; then
    tail -n 30 "$OUT/build.log"
    exit 1
  fi
done
HIDCTL=$PWD/hidctl/hidctl
RESULT=$PWD/$OUT/result.txt

if [ "$OS" = Darwin ]; then
  # an application started from a shell never becomes the active one unless
  # it is a bundle the system opens
  APP=$OUT/scriptcheck.app
  rm -rf "$APP"
  mkdir -p "$APP/Contents/MacOS"
  cp tests/scriptcheck/scriptcheck "$APP/Contents/MacOS/"
  cat >"$APP/Contents/Info.plist" <<EOF
<?xml version="1.0" encoding="UTF-8"?>
<!DOCTYPE plist PUBLIC "-//Apple//DTD PLIST 1.0//EN" "http://www.apple.com/DTDs/PropertyList-1.0.dtd">
<plist version="1.0"><dict>
<key>CFBundleExecutable</key><string>scriptcheck</string>
<key>CFBundleIdentifier</key><string>org.lazhidcontrol.scriptcheck</string>
<key>CFBundleName</key><string>scriptcheck</string>
<key>CFBundlePackageType</key><string>APPL</string>
<key>NSHighResolutionCapable</key><true/>
</dict></plist>
EOF
  open -n "$APP" --args "$RESULT"
  (cd "$OUT" && "$HIDCTL" ../../../scriptcheck/check.hid) >"$OUT/hidctl.log" 2>&1
  STATUS=$?
else
  D=:${XDISPLAY:-31}
  Xephyr "$D" -screen 1024x768 -nolisten tcp >/dev/null 2>&1 &
  XPID=$!
  sleep 1
  DISPLAY=$D openbox >/dev/null 2>&1 &
  WPID=$!
  sleep 1
  DISPLAY=$D tests/scriptcheck/scriptcheck "$RESULT" >/dev/null 2>&1 &
  (cd "$OUT" && DISPLAY=$D "$HIDCTL" ../../../scriptcheck/check.hid) >"$OUT/hidctl.log" 2>&1
  STATUS=$?
  sleep 1
  kill $WPID $XPID 2>/dev/null
fi

# the program writes its file when Done is clicked
for i in 1 2 3 4 5 6 7 8 9 10; do [ -f "$RESULT" ] && break; sleep 0.5; done
cat "$OUT/hidctl.log"
if [ $STATUS -ne 0 ] || [ ! -f "$RESULT" ]; then
  echo "FAIL: the script did not finish (hidctl exit $STATUS)"
  exit 1
fi
if diff -u tests/scriptcheck/expected.txt "$RESULT"; then
  echo "PASS: $OS $WS - the program received exactly what the script sent; pictures in $OUT"
else
  echo "FAIL: $OS $WS - see the differences above"
  exit 1
fi
