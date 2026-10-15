unit HotkeyInput;

{$mode objfpc}{$H+}
// Copyright (c) 2026 Tony Stone. 0BSD.

{ CreateHotkey makes a hotkey of the kind the running system needs. }

interface

uses
  Classes, HotkeyInputIntf;

type
  THotkey = HotkeyInputIntf.THotkey;
  TKeyNotifyEvent = HotkeyInputIntf.TKeyNotifyEvent;
  EHotkeyError = HotkeyInputIntf.EHotkeyError;

function CreateHotkey(AKey: word; AShift: TShiftState; AOnHotkey: TKeyNotifyEvent): THotkey;

implementation

uses
  SysUtils
  {$if defined(windows)}
  , WinHotkeyInput
  {$elseif defined(darwin)}
  , CocoaHotkeyInput
  {$elseif defined(unix) and not defined(android)}
  , XHotkeyInput, WaylandHotkeyInput
  {$endif};

function CreateHotkey(AKey: word; AShift: TShiftState; AOnHotkey: TKeyNotifyEvent): THotkey;
begin
  {$if defined(windows)}
  Result := TWinHotkey.Create(AKey, AShift, AOnHotkey);
  {$elseif defined(darwin)}
  Result := TCocoaHotkey.Create(AKey, AShift, AOnHotkey);
  {$elseif defined(unix) and not defined(android)}
  if GetEnvironmentVariable('WAYLAND_DISPLAY') <> '' then
    Result := TWaylandHotkey.Create(AKey, AShift, AOnHotkey)
  else
    Result := TXHotkey.Create(AKey, AShift, AOnHotkey);
  {$else}
  Result := THotkey.Create(AKey, AShift, AOnHotkey);
  {$endif}
end;

end.
