{ WinGlobalKeyMonitor

  Copyright (C) 2025 LazHIDControl Contributors

  Windows implementation of global key monitoring using low-level keyboard hooks.

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
unit WinGlobalKeyMonitor;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LCLType, Windows, GlobalKeyMonitorIntf;

type
  { TWinGlobalKeyListener }
  TWinGlobalKeyListener = class(TGlobalKeyListener)
  private
    HookHandle: HHOOK;
  protected
    procedure DoStartMonitoring; override;
    procedure DoStopMonitoring; override;
  public
    constructor Create; override;
  end;

function InitializeGlobalKeyListener: TGlobalKeyListener;

implementation

const
  WH_KEYBOARD_LL = 13;

type
  tagKBDLLHOOKSTRUCT = record
    vkCode: DWORD;
    scanCode: DWORD;
    flags: DWORD;
    time: DWORD;
    dwExtraInfo: ULONG_PTR;
  end;
  KBDLLHOOKSTRUCT = tagKBDLLHOOKSTRUCT;
  PKBDLLHOOKSTRUCT = ^KBDLLHOOKSTRUCT;

var
  GlobalListener: TWinGlobalKeyListener = nil;

function LowLevelKeyboardProc(nCode: Integer; wParam: WPARAM; lParam: LPARAM): LRESULT; stdcall;
var
  KeyInfo: ^KBDLLHOOKSTRUCT;
  vk: Word;
  modifiers: TShiftState;
begin
  Result := CallNextHookEx(0, nCode, wParam, lParam);

  if (nCode = HC_ACTION) and (wParam = WM_KEYDOWN) and Assigned(GlobalListener) then
  begin
    KeyInfo := Pointer(lParam);
    vk := KeyInfo^.vkCode;

    modifiers := [];
    if GetKeyState(VK_SHIFT) < 0 then Include(modifiers, ssShift);
    if GetKeyState(VK_CONTROL) < 0 then Include(modifiers, ssCtrl);
    if GetKeyState(VK_MENU) < 0 then Include(modifiers, ssAlt);

    if Assigned(GlobalListener.OnKeyPress) then
      GlobalListener.OnKeyPress(vk, modifiers);
  end;
end;

function InitializeGlobalKeyListener: TGlobalKeyListener;
begin
  Result := TWinGlobalKeyListener.Create;
end;

{ TWinGlobalKeyListener }

constructor TWinGlobalKeyListener.Create;
begin
  inherited Create;
  HookHandle := 0;
end;

procedure TWinGlobalKeyListener.DoStartMonitoring;
begin
  if Active then Exit;

  GlobalListener := Self;
  HookHandle := SetWindowsHookEx(WH_KEYBOARD_LL, @LowLevelKeyboardProc, HInstance, 0);
  Active := (HookHandle <> 0);
end;

procedure TWinGlobalKeyListener.DoStopMonitoring;
begin
  if not Active then Exit;

  if HookHandle <> 0 then
  begin
    UnhookWindowsHookEx(HookHandle);
    HookHandle := 0;
  end;

  GlobalListener := nil;
  Active := False;
end;

end.
