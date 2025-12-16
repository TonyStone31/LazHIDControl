{ WinGlobalMouseMonitor

  Copyright (C) 2025 LazHIDControl Contributors

  Windows implementation of global mouse monitoring using low-level mouse hooks.

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
unit WinGlobalMouseMonitor;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Controls, Windows, GlobalMouseMonitorIntf;

type
  { TWinGlobalMouseListener }
  TWinGlobalMouseListener = class(TGlobalMouseListener)
  private
    HookHandle: HHOOK;
  protected
    procedure DoStartMonitoring; override;
    procedure DoStopMonitoring; override;
  public
    constructor Create; override;
  end;

function InitializeGlobalMouseListener: TGlobalMouseListener;

implementation

const
  WH_MOUSE_LL = 14;

type
  tagMSLLHOOKSTRUCT = record
    pt: TPoint;
    mouseData: DWORD;
    flags: DWORD;
    time: DWORD;
    dwExtraInfo: ULONG_PTR;
  end;
  MSLLHOOKSTRUCT = tagMSLLHOOKSTRUCT;
  PMSLLHOOKSTRUCT = ^MSLLHOOKSTRUCT;

var
  GlobalListener: TWinGlobalMouseListener = nil;

function LowLevelMouseProc(nCode: Integer; wParam: WPARAM; lParam: LPARAM): LRESULT; stdcall;
var
  MouseInfo: ^MSLLHOOKSTRUCT;
  Button: TMouseButton;
  XButton: DWORD;
begin
  Result := CallNextHookEx(0, nCode, wParam, lParam);

  if (nCode = HC_ACTION) and Assigned(GlobalListener) then
  begin
    MouseInfo := Pointer(lParam);

    case wParam of
      WM_LBUTTONDOWN:
        Button := mbLeft;
      WM_RBUTTONDOWN:
        Button := mbRight;
      WM_MBUTTONDOWN:
        Button := mbMiddle;
      WM_XBUTTONDOWN:
        begin
          // Extract which X button was pressed (1 = back, 2 = forward)
          XButton := (MouseInfo^.mouseData shr 16) and $FFFF;
          if XButton = 1 then
            Button := mbExtra1  // Back button
          else
            Button := mbExtra2; // Forward button
        end;
    else
      Exit;
    end;

    if Assigned(GlobalListener.OnMouseClick) then
      GlobalListener.OnMouseClick(Button, MouseInfo^.pt.X, MouseInfo^.pt.Y);
  end;
end;

function InitializeGlobalMouseListener: TGlobalMouseListener;
begin
  Result := TWinGlobalMouseListener.Create;
end;

{ TWinGlobalMouseListener }

constructor TWinGlobalMouseListener.Create;
begin
  inherited Create;
  HookHandle := 0;
end;

procedure TWinGlobalMouseListener.DoStartMonitoring;
begin
  if IsActive then Exit;

  GlobalListener := Self;
  HookHandle := SetWindowsHookEx(WH_MOUSE_LL, @LowLevelMouseProc, HInstance, 0);
  IsActive := (HookHandle <> 0);
end;

procedure TWinGlobalMouseListener.DoStopMonitoring;
begin
  if not IsActive then Exit;

  if HookHandle <> 0 then
  begin
    UnhookWindowsHookEx(HookHandle);
    HookHandle := 0;
  end;

  GlobalListener := nil;
  IsActive := False;
end;

end.
