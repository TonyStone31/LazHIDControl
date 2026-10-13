{ HIDWinMouseInput

  Copyright (C) 2008 Tom Gregorovic

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
unit HIDWinMouseInput;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Controls, Forms,
  Windows, JwaWinUser,
  HIDMouseInputIntf;

type

  { TWinMouseInput }

  TWinMouseInput = class(TMouseInput)
  protected
    procedure DoDown(Button: TMouseButton); override;
    procedure DoMove(ScreenX, ScreenY: integer); override;
    procedure DoUp(Button: TMouseButton); override;
    procedure DoScrollUp; override;
    procedure DoScrollDown; override;
  end;

function InitializeMouseInput: TMouseInput;

implementation

function InitializeMouseInput: TMouseInput;
begin
  Result := TWinMouseInput.Create;
end;

procedure SendMouseInput(Flag: DWORD; MouseData: DWORD = 0);
var
  Input: TInput;
begin
  Input := Default(TInput);
  Input.type_ := INPUT_MOUSE;
  Input.mi.mouseData := MouseData;
  Input.mi.dwFlags := Flag;
  SendInput(1, @Input, SizeOf(Input));
end;

// Absolute coordinates run 0..65535 across the whole desktop, every monitor included.
procedure SendMouseInput(Flag: DWORD; X, Y: integer);
const
  MOUSEEVENTF_VIRTUALDESK = $4000;
var
  Input: TInput;
begin
  Input := Default(TInput);
  Input.type_ := INPUT_MOUSE;
  Input.mi.dx := MulDiv(X - Screen.DesktopLeft, 65535, Screen.DesktopWidth - 1);
  Input.mi.dy := MulDiv(Y - Screen.DesktopTop, 65535, Screen.DesktopHeight - 1);
  Input.mi.dwFlags := Flag or MOUSEEVENTF_ABSOLUTE or MOUSEEVENTF_VIRTUALDESK;
  SendInput(1, @Input, SizeOf(Input));
end;

{ TWinMouseInput }

procedure TWinMouseInput.DoDown(Button: TMouseButton);
var
  Flag: DWORD;
begin
  case Button of
    mbRight: Flag := MOUSEEVENTF_RIGHTDOWN;
    mbMiddle: Flag := MOUSEEVENTF_MIDDLEDOWN;
    else
      Flag := MOUSEEVENTF_LEFTDOWN;
  end;
  SendMouseInput(Flag);
end;

procedure TWinMouseInput.DoMove(ScreenX, ScreenY: integer);
begin
  SendMouseInput(MOUSEEVENTF_MOVE, ScreenX, ScreenY);
end;

procedure TWinMouseInput.DoUp(Button: TMouseButton);
var
  Flag: DWORD;
begin
  case Button of
    mbRight: Flag := MOUSEEVENTF_RIGHTUP;
    mbMiddle: Flag := MOUSEEVENTF_MIDDLEUP;
    else
      Flag := MOUSEEVENTF_LEFTUP;
  end;
  SendMouseInput(Flag);
end;

procedure TWinMouseInput.DoScrollUp;
begin
  SendMouseInput(MOUSEEVENTF_WHEEL, WHEEL_DELTA);
end;

procedure TWinMouseInput.DoScrollDown;
begin
  SendMouseInput(MOUSEEVENTF_WHEEL, DWORD(-WHEEL_DELTA));
end;

end.
