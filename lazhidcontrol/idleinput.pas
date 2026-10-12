unit IdleInput;
{$mode objfpc}{$H+}
// Copyright (c) 2026 Tony Stone. GPL-2.0-or-later; see COPYING.GPL2.
interface
function QuerySystemIdle(out Milliseconds: QWord; out Message: string): Boolean;
implementation
uses Math
  {$ifdef windows}, Windows{$endif}
  {$ifdef linux}, LinuxIdle{$endif}
  {$ifdef darwin}, CGEventSource, CGEventTypes{$endif};
{$ifdef windows}
type
  TIdleInputInfo = record
    cbSize, dwTime: DWORD;
  end;
function GetLastInputInfo(var Info: TIdleInputInfo): BOOL; stdcall;
  external 'user32.dll' name 'GetLastInputInfo';
{$endif}
function QuerySystemIdle(out Milliseconds: QWord; out Message: string): Boolean;
{$ifdef windows}var Info: TIdleInputInfo; CurrentTick: DWORD;{$endif}
{$ifdef darwin}var Seconds: Double;{$endif}
begin
  Milliseconds := 0;
  Message := 'System idle detection is not implemented on this platform.';
  Result := False;
  {$ifdef windows}
  Info := Default(TIdleInputInfo);
  Info.cbSize := SizeOf(Info);
  if not GetLastInputInfo(Info) then begin Message := 'Windows could not query session idle time.'; Exit; end;
  CurrentTick := Windows.GetTickCount;
  // Unsigned 32-bit elapsed time, including a tick counter wrap.
  Milliseconds := (QWord(CurrentTick) + QWord($100000000) - Info.dwTime) and $FFFFFFFF;
  Result := True;
  {$endif}
  {$ifdef linux}
  Result := QueryLinuxIdle(Milliseconds, Message);
  {$endif}
  {$ifdef darwin}
  Seconds := CGEventSourceSecondsSinceLastEventType(kCGEventSourceStateCombinedSessionState, kCGAnyInputEventType);
  Result := not IsNan(Seconds) and not IsInfinite(Seconds) and (Seconds >= 0) and (Seconds < High(Int64) / 1000);
  if Result then Milliseconds := Trunc(Seconds * 1000)
  else Message := 'macOS could not query session idle time.';
  {$endif}
  if Result then Message := '';
end;
end.
