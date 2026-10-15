unit HIDWinMouseInput;

{$mode objfpc}{$H+}
// Copyright (c) 2026 Tony Stone. 0BSD.

{ The mouse on Windows, through SendInput.  Moves are absolute over the
  whole virtual desktop, every monitor included. }

interface

uses
  HIDMouseInputIntf;

function InitializeWinMouseInput: TMouseInput;

implementation

uses
  Classes, SysUtils, Controls, Windows;

type
  // qualified: the Windows unit has a TMouseInput record of its own
  TWinMouseInput = class(HIDMouseInputIntf.TMouseInput)
  protected
    procedure DoDown(Button: TMouseButton); override;
    procedure DoUp(Button: TMouseButton); override;
    procedure DoMove(ScreenX, ScreenY: integer); override;
    procedure DoScrollUp; override;
    procedure DoScrollDown; override;
  end;

function InitializeWinMouseInput: HIDMouseInputIntf.TMouseInput;
begin
  Result := TWinMouseInput.Create;
end;

procedure Send(Flags, Data: DWORD; X, Y: LONG);
var
  Input: TInput;
begin
  Input := Default(TInput);
  Input._Type := INPUT_MOUSE;
  Input.mi.dx := X;
  Input.mi.dy := Y;
  Input.mi.MouseData := Data;
  Input.mi.dwFlags := Flags;
  SendInput(1, @Input, SizeOf(TInput));
end;

// The middle of the pixel's share of 0..65535, so the pixel is hit whether
// Windows rounds or truncates on the way back.
function Normalized(Pixel, First, Size: integer): LONG;
var
  Value: int64;
begin
  if Size < 1 then Size := 1;
  Value := (int64(Pixel - First) * 2 + 1) * 65536 div (int64(Size) * 2);
  if Value < 0 then Value := 0;
  if Value > 65535 then Value := 65535;
  Result := Value;
end;

procedure TWinMouseInput.DoMove(ScreenX, ScreenY: integer);
begin
  Send(MOUSEEVENTF_MOVE or MOUSEEVENTF_ABSOLUTE or MOUSEEVENTF_VIRTUALDESK, 0,
    Normalized(ScreenX, GetSystemMetrics(SM_XVIRTUALSCREEN), GetSystemMetrics(SM_CXVIRTUALSCREEN)),
    Normalized(ScreenY, GetSystemMetrics(SM_YVIRTUALSCREEN), GetSystemMetrics(SM_CYVIRTUALSCREEN)));
end;

procedure TWinMouseInput.DoDown(Button: TMouseButton);
begin
  case Button of
    mbLeft: Send(MOUSEEVENTF_LEFTDOWN, 0, 0, 0);
    mbRight: Send(MOUSEEVENTF_RIGHTDOWN, 0, 0, 0);
    mbMiddle: Send(MOUSEEVENTF_MIDDLEDOWN, 0, 0, 0);
    mbExtra1: Send(MOUSEEVENTF_XDOWN, XBUTTON1, 0, 0);
    mbExtra2: Send(MOUSEEVENTF_XDOWN, XBUTTON2, 0, 0);
  end;
end;

procedure TWinMouseInput.DoUp(Button: TMouseButton);
begin
  case Button of
    mbLeft: Send(MOUSEEVENTF_LEFTUP, 0, 0, 0);
    mbRight: Send(MOUSEEVENTF_RIGHTUP, 0, 0, 0);
    mbMiddle: Send(MOUSEEVENTF_MIDDLEUP, 0, 0, 0);
    mbExtra1: Send(MOUSEEVENTF_XUP, XBUTTON1, 0, 0);
    mbExtra2: Send(MOUSEEVENTF_XUP, XBUTTON2, 0, 0);
  end;
end;

procedure TWinMouseInput.DoScrollUp;
begin
  Send(MOUSEEVENTF_WHEEL, DWORD(WHEEL_DELTA), 0, 0);
end;

procedure TWinMouseInput.DoScrollDown;
begin
  Send(MOUSEEVENTF_WHEEL, DWORD(-WHEEL_DELTA), 0, 0);
end;

end.
