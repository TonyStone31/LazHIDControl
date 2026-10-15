unit HIDMouseInputIntf;

{$mode objfpc}{$H+}
// Copyright (c) 2026 Tony Stone. 0BSD.

{ The mouse as the rest of the package sees it.  Every action that takes a
  Shift holds those modifiers through KeyInput while it runs. }

interface

uses
  Classes, SysUtils, Types, Controls;

type
  TMouseInput = class
  private
    procedure Hold(Shift: TShiftState);
    procedure Release(Shift: TShiftState);
    procedure Glide(ScreenX, ScreenY, Duration: integer);
  protected
    procedure DoDown(Button: TMouseButton); virtual; abstract;
    procedure DoUp(Button: TMouseButton); virtual; abstract;
    procedure DoMove(ScreenX, ScreenY: integer); virtual; abstract;
    procedure DoScrollUp; virtual; abstract;
    procedure DoScrollDown; virtual; abstract;
    // Where the pointer is now, in screen coordinates.
    function CurrentPos: TPoint; virtual;
  public
    procedure Down(Button: TMouseButton; Shift: TShiftState); overload;
    procedure Down(Button: TMouseButton; Shift: TShiftState; Control: TControl; X, Y: integer); overload;
    procedure Down(Button: TMouseButton; Shift: TShiftState; ScreenX, ScreenY: integer); overload;
    procedure Up(Button: TMouseButton; Shift: TShiftState); overload;
    procedure Up(Button: TMouseButton; Shift: TShiftState; Control: TControl; X, Y: integer); overload;
    procedure Up(Button: TMouseButton; Shift: TShiftState; ScreenX, ScreenY: integer); overload;
    procedure Click(Button: TMouseButton; Shift: TShiftState); overload;
    procedure Click(Button: TMouseButton; Shift: TShiftState; Control: TControl; X, Y: integer); overload;
    procedure Click(Button: TMouseButton; Shift: TShiftState; ScreenX, ScreenY: integer); overload;
    procedure DblClick(Button: TMouseButton; Shift: TShiftState); overload;
    procedure DblClick(Button: TMouseButton; Shift: TShiftState; Control: TControl; X, Y: integer); overload;
    procedure DblClick(Button: TMouseButton; Shift: TShiftState; ScreenX, ScreenY: integer); overload;
    procedure ScrollUp(Shift: TShiftState); overload;
    procedure ScrollUp(Shift: TShiftState; Control: TControl; X, Y: integer); overload;
    procedure ScrollUp(Shift: TShiftState; ScreenX, ScreenY: integer); overload;
    procedure ScrollDown(Shift: TShiftState); overload;
    procedure ScrollDown(Shift: TShiftState; Control: TControl; X, Y: integer); overload;
    procedure ScrollDown(Shift: TShiftState; ScreenX, ScreenY: integer); overload;
    procedure Move(Shift: TShiftState; ScreenX, ScreenY: integer); overload;
    procedure Move(Shift: TShiftState; ScreenX, ScreenY: integer; Duration: integer); overload;
    procedure Move(Shift: TShiftState; Control: TControl; X, Y: integer; Duration: integer = 0); overload;
    procedure MoveBy(Shift: TShiftState; DX, DY: integer; Duration: integer = 0);
  end;

implementation

uses
  Forms, HIDMouseAndKeyInput;

{ a point in Control's client area, turned into the same point on the screen }
procedure ToScreen(Control: TControl; var X, Y: integer);
var
  Spot: TPoint;
begin
  Spot := Control.ClientToScreen(Point(X, Y));
  X := Spot.X;
  Y := Spot.Y;
end;

const
  // between the two clicks of a double click: a fast hand, well inside
  // any toolkit's double-click time
  ClickGap = 30;

function TMouseInput.CurrentPos: TPoint;
begin
  Result := Mouse.CursorPos;
end;

procedure TMouseInput.Hold(Shift: TShiftState);
begin
  if (Shift * [ssShift, ssCtrl, ssAlt] <> []) and Assigned(KeyInput) then KeyInput.Apply(Shift);
end;

procedure TMouseInput.Release(Shift: TShiftState);
begin
  if (Shift * [ssShift, ssCtrl, ssAlt] <> []) and Assigned(KeyInput) then KeyInput.Unapply(Shift);
end;

// Small steps in a straight line, on time, ending exactly on the point.
procedure TMouseInput.Glide(ScreenX, ScreenY, Duration: integer);
var
  Start: TPoint;
  Steps, I: integer;
  T0, Due: QWord;
begin
  if Duration <= 0 then
  begin
    DoMove(ScreenX, ScreenY);
    Application.ProcessMessages;
    Exit;
  end;
  Start := CurrentPos;
  Steps := Duration div 10;
  if Steps < 1 then Steps := 1;
  T0 := GetTickCount64;
  for I := 1 to Steps do
  begin
    DoMove(Start.X + (ScreenX - Start.X) * I div Steps, Start.Y + (ScreenY - Start.Y) * I div Steps);
    Due := T0 + QWord(Duration) * QWord(I) div QWord(Steps);
    repeat
      Application.ProcessMessages;
      if GetTickCount64 < Due then Sleep(1);
    until GetTickCount64 >= Due;
  end;
end;

procedure TMouseInput.Down(Button: TMouseButton; Shift: TShiftState);
begin
  Hold(Shift);
  try
    DoDown(Button);
    Application.ProcessMessages;
  finally
    Release(Shift);
  end;
end;

procedure TMouseInput.Down(Button: TMouseButton; Shift: TShiftState; Control: TControl; X, Y: integer);
begin
  ToScreen(Control, X, Y);
  Down(Button, Shift, X, Y);
end;

procedure TMouseInput.Down(Button: TMouseButton; Shift: TShiftState; ScreenX, ScreenY: integer);
begin
  Hold(Shift);
  try
    DoMove(ScreenX, ScreenY);
    Application.ProcessMessages;
    DoDown(Button);
    Application.ProcessMessages;
  finally
    Release(Shift);
  end;
end;

procedure TMouseInput.Up(Button: TMouseButton; Shift: TShiftState);
begin
  Hold(Shift);
  try
    DoUp(Button);
    Application.ProcessMessages;
  finally
    Release(Shift);
  end;
end;

procedure TMouseInput.Up(Button: TMouseButton; Shift: TShiftState; Control: TControl; X, Y: integer);
begin
  ToScreen(Control, X, Y);
  Up(Button, Shift, X, Y);
end;

procedure TMouseInput.Up(Button: TMouseButton; Shift: TShiftState; ScreenX, ScreenY: integer);
begin
  Hold(Shift);
  try
    DoMove(ScreenX, ScreenY);
    Application.ProcessMessages;
    DoUp(Button);
    Application.ProcessMessages;
  finally
    Release(Shift);
  end;
end;

// The press and release go out back to back.  A toolkit handed the press
// before the release has been sent can miss the next click of a double click.
procedure TMouseInput.Click(Button: TMouseButton; Shift: TShiftState);
begin
  Hold(Shift);
  try
    DoDown(Button);
    DoUp(Button);
    Application.ProcessMessages;
  finally
    Release(Shift);
  end;
end;

procedure TMouseInput.Click(Button: TMouseButton; Shift: TShiftState; Control: TControl; X, Y: integer);
begin
  ToScreen(Control, X, Y);
  Click(Button, Shift, X, Y);
end;

procedure TMouseInput.Click(Button: TMouseButton; Shift: TShiftState; ScreenX, ScreenY: integer);
begin
  Hold(Shift);
  try
    DoMove(ScreenX, ScreenY);
    Application.ProcessMessages;
    DoDown(Button);
    DoUp(Button);
    Application.ProcessMessages;
  finally
    Release(Shift);
  end;
end;

// All four events go out before the application looks at any of them, so
// both clicks land well inside the toolkit's double-click time.
procedure TMouseInput.DblClick(Button: TMouseButton; Shift: TShiftState);
begin
  Hold(Shift);
  try
    DoDown(Button);
    DoUp(Button);
    Sleep(ClickGap);
    DoDown(Button);
    DoUp(Button);
    Application.ProcessMessages;
  finally
    Release(Shift);
  end;
end;

procedure TMouseInput.DblClick(Button: TMouseButton; Shift: TShiftState; Control: TControl; X, Y: integer);
begin
  ToScreen(Control, X, Y);
  DblClick(Button, Shift, X, Y);
end;

procedure TMouseInput.DblClick(Button: TMouseButton; Shift: TShiftState; ScreenX, ScreenY: integer);
begin
  Hold(Shift);
  try
    DoMove(ScreenX, ScreenY);
    Application.ProcessMessages;
    DblClick(Button, []);
  finally
    Release(Shift);
  end;
end;

procedure TMouseInput.ScrollUp(Shift: TShiftState);
begin
  Hold(Shift);
  try
    DoScrollUp;
    Application.ProcessMessages;
  finally
    Release(Shift);
  end;
end;

procedure TMouseInput.ScrollUp(Shift: TShiftState; Control: TControl; X, Y: integer);
begin
  ToScreen(Control, X, Y);
  ScrollUp(Shift, X, Y);
end;

procedure TMouseInput.ScrollUp(Shift: TShiftState; ScreenX, ScreenY: integer);
begin
  Hold(Shift);
  try
    DoMove(ScreenX, ScreenY);
    Application.ProcessMessages;
    DoScrollUp;
    Application.ProcessMessages;
  finally
    Release(Shift);
  end;
end;

procedure TMouseInput.ScrollDown(Shift: TShiftState);
begin
  Hold(Shift);
  try
    DoScrollDown;
    Application.ProcessMessages;
  finally
    Release(Shift);
  end;
end;

procedure TMouseInput.ScrollDown(Shift: TShiftState; Control: TControl; X, Y: integer);
begin
  ToScreen(Control, X, Y);
  ScrollDown(Shift, X, Y);
end;

procedure TMouseInput.ScrollDown(Shift: TShiftState; ScreenX, ScreenY: integer);
begin
  Hold(Shift);
  try
    DoMove(ScreenX, ScreenY);
    Application.ProcessMessages;
    DoScrollDown;
    Application.ProcessMessages;
  finally
    Release(Shift);
  end;
end;

procedure TMouseInput.Move(Shift: TShiftState; ScreenX, ScreenY: integer);
begin
  Move(Shift, ScreenX, ScreenY, 0);
end;

procedure TMouseInput.Move(Shift: TShiftState; ScreenX, ScreenY: integer; Duration: integer);
begin
  Hold(Shift);
  try
    Glide(ScreenX, ScreenY, Duration);
  finally
    Release(Shift);
  end;
end;

procedure TMouseInput.Move(Shift: TShiftState; Control: TControl; X, Y: integer; Duration: integer);
begin
  ToScreen(Control, X, Y);
  Move(Shift, X, Y, Duration);
end;

procedure TMouseInput.MoveBy(Shift: TShiftState; DX, DY: integer; Duration: integer);
var
  P: TPoint;
begin
  P := CurrentPos;
  Move(Shift, P.X + DX, P.Y + DY, Duration);
end;

end.
