unit HIDInput;
{$mode objfpc}{$H+}
// Copyright (c) 2026 Tony Stone. GPL-2.0-or-later; see COPYING.GPL2.
interface
uses Classes, SysUtils, Controls, WindowManagerIntf, HIDWindow;
type
  THIDKeyboard = class(TComponent)
  private
    Delay: Cardinal;
    KeyModifiers: TShiftState;
    procedure CheckAvailable;
  public
    procedure TypeText(const Text: string);
    procedure PressKey(Key: Word);
    procedure KeyDown(Key: Word);
    procedure KeyUp(Key: Word);
  published
    // Extra milliseconds between typed characters. Default 0 uses backend timing.
    // TypeText is synchronous and sends text to the currently focused application.
    property TypingInterval: Cardinal read Delay write Delay default 0;
    // Ctrl, Alt, and Shift applied around PressKey. TypeText types literal text.
    property Modifiers: TShiftState read KeyModifiers write KeyModifiers default [];
  end;
  THIDMouse = class(TComponent)
  private
    Duration: Cardinal;
    MouseModifiers: TShiftState;
    RelativeControl: TControl;
    TargetWindow: THIDWindow;
    procedure SetRelativeWindow(Value: THIDWindow);
    procedure SetRelativeControl(Value: TControl);
    procedure CheckAvailable;
  protected
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
  public
    procedure MoveTo(X, Y: Integer);
    // Screen-relative window bounds; ignores RelativeTo. Find the handle with THIDWindow.
    procedure MoveToWindow(Window: TWindowHandle; X, Y: Integer);
    procedure Click(Button: TMouseButton = mbLeft);
    procedure DoubleClick(Button: TMouseButton = mbLeft);
    procedure ButtonDown(Button: TMouseButton = mbLeft);
    procedure ButtonUp(Button: TMouseButton = mbLeft);
    procedure Scroll(Steps: Integer);
  published
    // Movement duration in milliseconds. Default 0 moves immediately.
    property MovementDuration: Cardinal read Duration write Duration default 0;
    // Ctrl, Alt, and Shift applied during mouse actions.
    property Modifiers: TShiftState read MouseModifiers write MouseModifiers default [];
    // Optional control whose client coordinates MoveTo uses, unless RelativeWindow is set.
    // With neither reference assigned, MoveTo uses screen coordinates.
    // Click and scrolling act at the current pointer position; nothing runs when the form loads.
    property RelativeTo: TControl read RelativeControl write SetRelativeControl;
    // Optional THIDWindow target. Takes precedence over RelativeTo; call its Find first.
    // MoveTo uses offsets from the target's reported screen rectangle, including native borders.
    property RelativeWindow: THIDWindow read TargetWindow write SetRelativeWindow;
  end;
implementation
uses MouseAndKeyInput, WindowManager;
procedure THIDKeyboard.CheckAvailable;
begin
  if csDesigning in ComponentState then raise EInvalidOperation.Create('Input is disabled in the designer.');
  {$ifdef linux}
  if GetEnvironmentVariable('WAYLAND_DISPLAY') <> '' then
    raise EInvalidOperation.Create('Wayland input is not implemented.');
  {$endif}
  if Modifiers - [ssCtrl, ssAlt, ssShift] <> [] then
    raise EInvalidOperation.Create('Supported modifiers are Ctrl, Alt, and Shift.');
  if not Assigned(KeyInput) then raise EInvalidOperation.Create('Keyboard input is unavailable.');
end;
procedure THIDKeyboard.TypeText(const Text: string);
begin CheckAvailable; KeyInput.PressString(Text, TypingInterval); end;
procedure THIDKeyboard.PressKey(Key: Word);
begin
  CheckAvailable;
  KeyInput.Apply(Modifiers);
  try KeyInput.Press(Key); finally KeyInput.Unapply(Modifiers); end;
end;
procedure THIDKeyboard.KeyDown(Key: Word);
begin CheckAvailable; KeyInput.Down(Key); end;
procedure THIDKeyboard.KeyUp(Key: Word);
begin CheckAvailable; KeyInput.Up(Key); end;
procedure THIDMouse.CheckAvailable;
begin
  if csDesigning in ComponentState then raise EInvalidOperation.Create('Input is disabled in the designer.');
  {$ifdef linux}
  if GetEnvironmentVariable('WAYLAND_DISPLAY') <> '' then
    raise EInvalidOperation.Create('Wayland input is not implemented.');
  {$endif}
  if Modifiers - [ssCtrl, ssAlt, ssShift] <> [] then
    raise EInvalidOperation.Create('Supported modifiers are Ctrl, Alt, and Shift.');
  if not Assigned(MouseInput) then raise EInvalidOperation.Create('Mouse input is unavailable.');
  if MovementDuration > Cardinal(High(Integer)) then raise ERangeError.Create('Movement duration is too large.');
end;
procedure THIDMouse.SetRelativeControl(Value: TControl);
begin
  if Value = RelativeControl then Exit;
  if Assigned(RelativeControl) then RelativeControl.RemoveFreeNotification(Self);
  RelativeControl := Value;
  if Assigned(RelativeControl) then RelativeControl.FreeNotification(Self);
end;
procedure THIDMouse.SetRelativeWindow(Value: THIDWindow);
begin
  if Value = TargetWindow then Exit;
  if Assigned(TargetWindow) then TargetWindow.RemoveFreeNotification(Self);
  TargetWindow := Value;
  if Assigned(TargetWindow) then TargetWindow.FreeNotification(Self);
end;
procedure THIDMouse.Notification(AComponent: TComponent; Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);
  if (Operation = opRemove) and (AComponent = RelativeControl) then RelativeControl := nil;
  if (Operation = opRemove) and (AComponent = TargetWindow) then TargetWindow := nil;
end;
procedure THIDMouse.MoveTo(X, Y: Integer);
begin
  CheckAvailable;
  if Assigned(RelativeWindow) then MoveToWindow(RelativeWindow.Handle, X, Y)
  else if Assigned(RelativeTo) then MouseInput.Move(Modifiers, RelativeTo, X, Y, MovementDuration)
  else MouseInput.Move(Modifiers, X, Y, MovementDuration);
end;
procedure THIDMouse.MoveToWindow(Window: TWindowHandle; X, Y: Integer);
var Info: TWindowInfo;
begin
  CheckAvailable;
  EnsureWindowManager;
  if (WindowMgr = nil) or (Window = 0) then raise EInvalidOperation.Create('Window control is unavailable.');
  if not WindowMgr.GetWindowInfo(Window, Info) or not Info.IsVisible then
    raise EInvalidOperation.Create('The target window is unavailable or hidden.');
  MouseInput.Move(Modifiers, Info.Rect.Left + X, Info.Rect.Top + Y, MovementDuration);
end;
procedure THIDMouse.Click(Button: TMouseButton);
begin CheckAvailable; MouseInput.Click(Button, Modifiers); end;
procedure THIDMouse.DoubleClick(Button: TMouseButton);
begin CheckAvailable; MouseInput.DblClick(Button, Modifiers); end;
procedure THIDMouse.ButtonDown(Button: TMouseButton);
begin CheckAvailable; MouseInput.Down(Button, Modifiers); end;
procedure THIDMouse.ButtonUp(Button: TMouseButton);
begin CheckAvailable; MouseInput.Up(Button, Modifiers); end;
procedure THIDMouse.Scroll(Steps: Integer);
begin
  CheckAvailable;
  while Steps > 0 do begin MouseInput.ScrollUp(Modifiers); Dec(Steps); end;
  while Steps < 0 do begin MouseInput.ScrollDown(Modifiers); Inc(Steps); end;
end;
end.
