# The Windows half of run.sh: the same script against the same program.
# Put scriptcheck.exe and hidctl.exe beside this file with check.hid and
# expected.txt, then:
#   powershell -ExecutionPolicy Bypass -File run-windows.ps1
$here = Split-Path -Parent $MyInvocation.MyCommand.Path
$out = Join-Path $here 'results-windows'
New-Item -ItemType Directory -Force -Path $out | Out-Null
Remove-Item -ErrorAction SilentlyContinue (Join-Path $out '*.png'), (Join-Path $out 'result.txt')
$result = Join-Path $out 'result.txt'

Start-Process -FilePath (Join-Path $here 'scriptcheck.exe') -ArgumentList "`"$result`""
Push-Location $out
& (Join-Path $here 'hidctl.exe') (Join-Path $here 'check.hid')
$status = $LASTEXITCODE
Pop-Location

for ($i = 0; ($i -lt 20) -and -not (Test-Path $result); $i++) { Start-Sleep -Milliseconds 500 }
if (($status -ne 0) -or -not (Test-Path $result)) {
  Write-Output "FAIL: the script did not finish (hidctl exit $status)"
  exit 1
}
$want = Get-Content -Encoding UTF8 (Join-Path $here 'expected.txt')
$got = Get-Content -Encoding UTF8 $result
$diff = Compare-Object $want $got
if ($diff) {
  $diff | ForEach-Object { Write-Output ("{0} {1}" -f $_.SideIndicator, $_.InputObject) }
  Write-Output 'FAIL: Windows - the lines marked <= were expected, => arrived'
  exit 1
}
Write-Output "PASS: Windows - the program received exactly what the script sent; pictures in $out"
