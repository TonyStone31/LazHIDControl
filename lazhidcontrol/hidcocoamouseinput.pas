{ HIDCocoaMouseInput

  Copyright (C) 2019 Sammarco Francesco

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
unit HIDCocoaMouseInput;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Controls, Forms, MacOSAll, CocoaEventTap,
  HIDMouseInputIntf;

type

  { TCocoaMouseInput }

  TCocoaMouseInput = class(TMouseInput)
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
  Result := TCocoaMouseInput.Create;
end;

const
  ButtonNumber: array [TMouseButton] of integer = (kCGMouseButtonLeft, kCGMouseButtonRight, kCGMouseButtonCenter, 3, 4);
  // left and right have event kinds of their own; every other button shares one
  DownKind: array [TMouseButton] of CGEventType = (kCGEventLeftMouseDown, kCGEventRightMouseDown, kCGEventOtherMouseDown,
    kCGEventOtherMouseDown, kCGEventOtherMouseDown);
  UpKind: array [TMouseButton] of CGEventType = (kCGEventLeftMouseUp, kCGEventRightMouseUp, kCGEventOtherMouseUp,
    kCGEventOtherMouseUp, kCGEventOtherMouseUp);

procedure Post(Kind: CGEventType; X, Y: integer; Button: TMouseButton);
var
  Event: CGEventRef;
begin
  Event := CGEventCreateMouseEvent(nil, Kind, CGPointMake(X, Y), ButtonNumber[Button]);
  if Event = nil then Exit;
  CGEventPost(kCGHIDEventTap, Event);
  CFRelease(Event);
end;

procedure Wheel(Lines: integer);
var
  Event: CGEventRef;
begin
  Event := CGEventCreateScrollWheelEvent(nil, kCGScrollEventUnitLine, 1, Lines);
  if Event = nil then Exit;
  CGEventPost(kCGHIDEventTap, Event);
  CFRelease(Event);
end;

{ TCocoaMouseInput }

procedure TCocoaMouseInput.DoDown(Button: TMouseButton);
begin
  Post(DownKind[Button], Mouse.CursorPos.X, Mouse.CursorPos.Y, Button);
end;

procedure TCocoaMouseInput.DoMove(ScreenX, ScreenY: integer);
begin
  Post(kCGEventMouseMoved, ScreenX, ScreenY, mbLeft);
end;

procedure TCocoaMouseInput.DoUp(Button: TMouseButton);
begin
  Post(UpKind[Button], Mouse.CursorPos.X, Mouse.CursorPos.Y, Button);
end;

procedure TCocoaMouseInput.DoScrollUp;
begin
  Wheel(1);
end;

procedure TCocoaMouseInput.DoScrollDown;
begin
  Wheel(-1);
end;

end.
