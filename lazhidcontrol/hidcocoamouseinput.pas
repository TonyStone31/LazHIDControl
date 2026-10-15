unit HIDCocoaMouseInput;

{$mode objfpc}{$H+}
// Copyright (c) 2026 Tony Stone. 0BSD.

{ The mouse on macOS, through Quartz events posted to the HID event tap.
  Each event carries the modifiers held through KeyInput, and a move with
  a button down is posted as a drag of that button. }

interface

uses
  HIDMouseInputIntf;

function InitializeCocoaMouseInput: TMouseInput;

implementation

uses
  Classes, SysUtils, Types, Controls, MacOSAll, HIDCocoaKeyInput;

const
  // Quartz button numbers; left and right also have event types of their own
  ButtonNumbers: array[TMouseButton] of CGMouseButton = (0, 1, 2, 3, 4);
  // two presses closer than this, in time and place, make a double click
  DoubleClickMs = 500;
  DoubleClickDistance = 4;
  // MacOSAll leaves these out until IOKit is translated; values from IOLLEvent.h
  kCGEventLeftMouseDown = 1;
  kCGEventLeftMouseUp = 2;
  kCGEventRightMouseDown = 3;
  kCGEventRightMouseUp = 4;
  kCGEventMouseMoved = 5;
  kCGEventLeftMouseDragged = 6;
  kCGEventRightMouseDragged = 7;
  kCGEventOtherMouseDown = 25;
  kCGEventOtherMouseUp = 26;
  kCGEventOtherMouseDragged = 27;

type
  TCocoaMouseInput = class(TMouseInput)
  private
    Source: CGEventSourceRef;
    Moved: boolean;
    LastMove: CGPoint;
    Pressed: set of TMouseButton;
    LastButton: TMouseButton;
    LastPress: QWord;
    LastPressAt: CGPoint;
    Clicks: integer;
    function Where: CGPoint;
    procedure Post(Kind: CGEventType; Button: TMouseButton; const At: CGPoint; ClickState: integer);
  protected
    procedure DoDown(Button: TMouseButton); override;
    procedure DoUp(Button: TMouseButton); override;
    procedure DoMove(ScreenX, ScreenY: integer); override;
    procedure DoScrollUp; override;
    procedure DoScrollDown; override;
    function CurrentPos: TPoint; override;
  public
    constructor Create;
    destructor Destroy; override;
  end;

function InitializeCocoaMouseInput: TMouseInput;
begin
  Result := TCocoaMouseInput.Create;
end;

constructor TCocoaMouseInput.Create;
begin
  inherited Create;
  Source := CGEventSourceCreate(kCGEventSourceStateHIDSystemState);
end;

destructor TCocoaMouseInput.Destroy;
begin
  if Source <> nil then CFRelease(Source);
  inherited Destroy;
end;

// The system can still report the old cursor position just after a posted
// move, so our own last move is the better answer once there is one.
function TCocoaMouseInput.Where: CGPoint;
var
  Event: CGEventRef;
begin
  if Moved then Exit(LastMove);
  Result.x := 0;
  Result.y := 0;
  Event := CGEventCreate(nil);
  if Event = nil then Exit;
  Result := CGEventGetLocation(Event);
  CFRelease(Event);
end;

function TCocoaMouseInput.CurrentPos: TPoint;
var
  At: CGPoint;
begin
  At := Where;
  // not Point(): MacOSAll has a Point record that hides the Types function
  Result.X := Round(At.x);
  Result.Y := Round(At.y);
end;

procedure TCocoaMouseInput.Post(Kind: CGEventType; Button: TMouseButton; const At: CGPoint; ClickState: integer);
var
  Event: CGEventRef;
begin
  Event := CGEventCreateMouseEvent(Source, Kind, At, ButtonNumbers[Button]);
  if Event = nil then Exit;
  if ClickState > 0 then CGEventSetIntegerValueField(Event, kCGMouseEventClickState, ClickState);
  CGEventSetFlags(Event, HeldModifierFlags);
  CGEventPost(kCGHIDEventTap, Event);
  CFRelease(Event);
end;

procedure TCocoaMouseInput.DoDown(Button: TMouseButton);
var
  At: CGPoint;
begin
  At := Where;
  // an application counts clicks from the event's click state, not from timing
  if (Button = LastButton) and (GetTickCount64 - LastPress < DoubleClickMs) and (Abs(At.x - LastPressAt.x) <= DoubleClickDistance) and
    (Abs(At.y - LastPressAt.y) <= DoubleClickDistance) then
    Inc(Clicks)
  else
    Clicks := 1;
  LastButton := Button;
  LastPress := GetTickCount64;
  LastPressAt := At;
  Include(Pressed, Button);
  case Button of
    mbLeft: Post(kCGEventLeftMouseDown, Button, At, Clicks);
    mbRight: Post(kCGEventRightMouseDown, Button, At, Clicks);
    else
      Post(kCGEventOtherMouseDown, Button, At, Clicks);
  end;
end;

procedure TCocoaMouseInput.DoUp(Button: TMouseButton);
var
  At: CGPoint;
  ClickState: integer;
begin
  At := Where;
  if Button = LastButton then ClickState := Clicks
  else
    ClickState := 1;
  Exclude(Pressed, Button);
  case Button of
    mbLeft: Post(kCGEventLeftMouseUp, Button, At, ClickState);
    mbRight: Post(kCGEventRightMouseUp, Button, At, ClickState);
    else
      Post(kCGEventOtherMouseUp, Button, At, ClickState);
  end;
end;

procedure TCocoaMouseInput.DoMove(ScreenX, ScreenY: integer);
var
  At: CGPoint;
  Button: TMouseButton;
begin
  At.x := ScreenX;
  At.y := ScreenY;
  LastMove := At;
  Moved := True;
  // with a button held a move is a drag, or the control sees no button down
  if mbLeft in Pressed then
  begin
    Post(kCGEventLeftMouseDragged, mbLeft, At, 0);
    Exit;
  end;
  if mbRight in Pressed then
  begin
    Post(kCGEventRightMouseDragged, mbRight, At, 0);
    Exit;
  end;
  for Button := mbMiddle to mbExtra2 do
    if Button in Pressed then
    begin
      Post(kCGEventOtherMouseDragged, Button, At, 0);
      Exit;
    end;
  Post(kCGEventMouseMoved, mbLeft, At, 0);
end;

procedure Scroll(Source: CGEventSourceRef; Lines: integer);
var
  Event: CGEventRef;
begin
  Event := CGEventCreateScrollWheelEvent(Source, kCGScrollEventUnitLine, 1, Lines);
  if Event = nil then Exit;
  CGEventSetFlags(Event, HeldModifierFlags);
  CGEventPost(kCGHIDEventTap, Event);
  CFRelease(Event);
end;

procedure TCocoaMouseInput.DoScrollUp;
begin
  Scroll(Source, 1);
end;

procedure TCocoaMouseInput.DoScrollDown;
begin
  Scroll(Source, -1);
end;

end.
