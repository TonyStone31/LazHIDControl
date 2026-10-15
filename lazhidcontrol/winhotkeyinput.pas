unit WinHotkeyInput;

{$mode objfpc}{$H+}
// Copyright (c) 2026 Tony Stone. 0BSD.

{ Hotkeys on Windows: RegisterHotKey on a hidden message-only window of this
  unit's own, made on the main thread at the first registration, so
  WM_HOTKEY arrives through the application's message loop. }

interface

uses
  Classes, HotkeyInputIntf;

type
  TWinHotkey = class(THotkey)
  private
    Id: integer;
  protected
    procedure DoRegister; override;
    procedure DoUnregister; override;
  end;

implementation

uses
  SysUtils, Windows;

const
  // Windows 7 and later: holding the combination does not repeat WM_HOTKEY
  MOD_NOREPEAT = $4000;
  HWND_MESSAGE = HWND(-3);
  // ids from $C000 up belong to shared DLLs
  LastAppId = $BFFF;
  WindowClassName: pwidechar = 'LazHIDControlHotkeys';

var
  Wnd: HWND;
  Hotkeys: TFPList;

function FindHotkey(AId: integer): TWinHotkey;
var
  I: integer;
begin
  Result := nil;
  if Hotkeys <> nil then
    for I := 0 to Hotkeys.Count - 1 do
      if TWinHotkey(Hotkeys[I]).Id = AId then Exit(TWinHotkey(Hotkeys[I]));
end;

function WindowProc(Window: HWND; Msg: UINT; WP: WPARAM; LP: LPARAM): LRESULT; stdcall;
var
  H: TWinHotkey;
begin
  if Msg = WM_HOTKEY then
  begin
    H := FindHotkey(integer(WP));
    if H <> nil then H.PressedLater;
    Result := 0;
  end
  else
    Result := DefWindowProcW(Window, Msg, WP, LP);
end;

function HotkeyWindow: HWND;
var
  WC: TWndClassW;
begin
  if Wnd = 0 then
  begin
    WC := Default(TWndClassW);
    WC.lpfnWndProc := @WindowProc;
    WC.hInstance := HInstance;
    WC.lpszClassName := WindowClassName;
    Windows.RegisterClassW(@WC);
    Wnd := CreateWindowExW(0, WindowClassName, nil, 0, 0, 0, 0, 0, HWND_MESSAGE, 0, HInstance, nil);
  end;
  Result := Wnd;
end;

procedure TWinHotkey.DoRegister;
var
  Mods: UINT;
  Err: DWORD;
  I: integer;
begin
  if GetCurrentThreadId <> MainThreadID then raise EHotkeyError.Create('Hotkeys must be registered from the main thread.');
  if Shift - [ssCtrl, ssAlt, ssShift] <> [] then raise EHotkeyError.Create('Only Ctrl, Alt and Shift can be part of a hotkey.');
  // LCL virtual keys below 255 are the Windows ones
  if (Key = 0) or (Key > $FE) then raise EHotkeyError.CreateFmt('Key %d cannot be used as a hotkey.', [Key]);
  // asked here, so the reason given says it is this program
  for I := 0 to Hotkeys.Count - 1 do
    if (TWinHotkey(Hotkeys[I]).Key = Key) and (TWinHotkey(Hotkeys[I]).Shift = Shift) then
      raise EHotkeyError.Create('This combination is already registered in this program.');
  if HotkeyWindow = 0 then raise EHotkeyError.Create(SysErrorMessage(GetLastError));

  Id := 1;
  while (Id <= LastAppId) and (FindHotkey(Id) <> nil) do Inc(Id);
  if Id > LastAppId then raise EHotkeyError.Create('Too many hotkeys.');

  Mods := MOD_NOREPEAT;
  if ssCtrl in Shift then Mods := Mods or MOD_CONTROL;
  if ssAlt in Shift then Mods := Mods or MOD_ALT;
  if ssShift in Shift then Mods := Mods or MOD_SHIFT;
  if not RegisterHotKey(Wnd, Id, Mods, Key) then
  begin
    Err := GetLastError;
    if Err = ERROR_HOTKEY_ALREADY_REGISTERED then raise EHotkeyError.Create('Another program already uses this combination.');
    raise EHotkeyError.Create(SysErrorMessage(Err));
  end;
  Hotkeys.Add(Self);
  Registered := True;
end;

procedure TWinHotkey.DoUnregister;
begin
  // the window is gone once this unit is finalized, and the hotkeys with it
  if Hotkeys = nil then Exit;
  UnregisterHotKey(Wnd, Id);
  Hotkeys.Remove(Self);
end;

initialization
  Hotkeys := TFPList.Create;

finalization
  if Wnd <> 0 then DestroyWindow(Wnd);
  Windows.UnregisterClassW(WindowClassName, HInstance);
  FreeAndNil(Hotkeys);
end.
