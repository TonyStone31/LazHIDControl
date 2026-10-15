{ WinGlobalKeyMonitor

  Copyright (c) 2025-2026 Tony Stone. 0BSD.

  Windows implementation of global key monitoring using low-level keyboard hooks.
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

function LowLevelKeyboardProc(nCode: integer; wParam: WPARAM; lParam: LPARAM): LRESULT; stdcall;
var
  KeyInfo: ^KBDLLHOOKSTRUCT;
  vk: word;
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
