{ WinHotkeyInput

  Copyright (C) 2025 LazHIDControl Contributors

  Windows implementation of global hotkey registration using RegisterHotKey API.

  This source is free software; you can redistribute it and/or modify it under the terms of the
  GNU General Public License as published by the Free Software Foundation; either version 2 of the
  License, or (at your option) any later version.

  This code is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without
  even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
  General Public License for more details.

  A copy of the GNU General Public License is available on the World Wide Web at
  <http://www.gnu.org/copyleft/gpl.html>. You can also obtain it by writing to the Free Software
  Foundation, Inc., 51 Franklin Street - Fifth Floor, Boston, MA 02110-1335, USA.
}
unit WinHotkeyInput;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LCLType, Windows, HotkeyInputIntf;

type
  { TWinHotkey }
  TWinHotkey = class(THotkey)
  private
    AtomID: Word;
    WindowHandle: HWND;
  protected
    procedure DoRegister; override;
    procedure DoUnregister; override;
  public
    constructor Create(AKey: Word; AShift: TShiftState; AOnHotkey: TKeyNotifyEvent); override;
  end;

function InitializeHotkey(AKey: Word; AShift: TShiftState; AOnHotkey: TKeyNotifyEvent): THotkey;

implementation

const
  MOD_NOREPEAT = $4000;

var
  GlobalHotkeys: TList = nil;
  MessageWindow: HWND = 0;

function HotkeyWindowProc(hWnd: HWND; Msg: UINT; wParam: WPARAM; lParam: LPARAM): LRESULT; stdcall;
var
  I: Integer;
  Hotkey: TWinHotkey;
begin
  if Msg = WM_HOTKEY then
  begin
    for I := 0 to GlobalHotkeys.Count - 1 do
    begin
      Hotkey := TWinHotkey(GlobalHotkeys[I]);
      if Hotkey.AtomID = wParam then
      begin
        if Assigned(Hotkey.OnHotkeyEvent) then
          Hotkey.OnHotkeyEvent(Hotkey, Hotkey.Key, Hotkey.Shift);
        Exit(0);
      end;
    end;
  end;
  Result := DefWindowProc(hWnd, Msg, wParam, lParam);
end;

function InitializeHotkey(AKey: Word; AShift: TShiftState; AOnHotkey: TKeyNotifyEvent): THotkey;
begin
  Result := TWinHotkey.Create(AKey, AShift, AOnHotkey);
end;

{ TWinHotkey }

constructor TWinHotkey.Create(AKey: Word; AShift: TShiftState; AOnHotkey: TKeyNotifyEvent);
var
  WndClass: TWNDCLASSA;
begin
  inherited Create(AKey, AShift, AOnHotkey);
  AtomID := 0;

  if GlobalHotkeys = nil then
    GlobalHotkeys := TList.Create;

  if MessageWindow = 0 then
  begin
    FillChar(WndClass, SizeOf(WndClass), 0);
    WndClass.lpfnWndProc := @HotkeyWindowProc;
    WndClass.hInstance := HInstance;
    WndClass.lpszClassName := 'HotkeyInputWindow';
    RegisterClassA(WndClass);
    MessageWindow := CreateWindowEx(0, 'HotkeyInputWindow', '', 0, 0, 0, 0, 0, 0, 0, HInstance, nil);
  end;

  WindowHandle := MessageWindow;
end;

procedure TWinHotkey.DoRegister;
var
  Modifiers: UINT;
begin
  Modifiers := MOD_NOREPEAT;
  if ssShift in Shift then Modifiers := Modifiers or MOD_SHIFT;
  if ssCtrl in Shift then Modifiers := Modifiers or MOD_CONTROL;
  if ssAlt in Shift then Modifiers := Modifiers or MOD_ALT;

  AtomID := GlobalAddAtom(PChar(Format('Hotkey_%d_%d', [Key, Modifiers])));
  if RegisterHotKey(WindowHandle, AtomID, Modifiers, Key) then
  begin
    GlobalHotkeys.Add(Self);
    Registered := True;
  end
  else
    AtomID := 0;
end;

procedure TWinHotkey.DoUnregister;
begin
  if not Registered then Exit;

  UnregisterHotKey(WindowHandle, AtomID);
  GlobalDeleteAtom(AtomID);
  GlobalHotkeys.Remove(Self);

  AtomID := 0;
  Registered := False;
end;

end.
