unit HIDInput;
{$mode objfpc}{$H+}
// Copyright (c) 2026 Tony Stone. GPL-2.0-or-later; see COPYING.GPL2.
interface

uses Classes, SysUtils, Controls, Forms, WindowManagerIntf, HIDWindow;

type
  THIDKeyboard = class(TComponent)
  private
    Delay: cardinal;
    KeyModifiers: TShiftState;
    Note: string;
  public
    constructor Create(AOwner: TComponent); override;
    procedure TypeText(const Text: string);
    procedure PressKey(Key: word);
    procedure KeyDown(Key: word);
    procedure KeyUp(Key: word);
  published
    // Extra milliseconds to rest between typed characters. Zero types at the
    // backend's own pace. TypeText sends to whatever application has the focus.
    // On macOS nothing arrives until the user allows your program under
    // Privacy & Security > Accessibility. On Wayland typing needs a udev rule,
    // which THIDPermission installs, and text beyond ASCII goes through the
    // desktop, which asks the user once - see PermissionNote.
    property TypingInterval: cardinal read Delay write Delay default 0;
    // Ctrl, Alt, and Shift held around PressKey calls. TypeText ignores this
    // and types its text literally.
    property Modifiers: TShiftState read KeyModifiers write KeyModifiers default [];
    // Wayland: shown to the user once, just before the desktop asks whether
    // this program may control the keyboard - which it does the first time
    // TypeText has a character beyond ASCII. The desktop's own question is
    // not ours to word; this is. Empty shows nothing. %s is the program's title.
    property PermissionNote: string read Note write Note;
  end;

  THIDMouse = class(TComponent)
  private
    Duration: cardinal;
    MouseModifiers: TShiftState;
    RelativeControl: TControl;
    TargetWindow: THIDWindow;
    procedure SetRelativeWindow(Value: THIDWindow);
    procedure SetRelativeControl(Value: TControl);
  protected
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
  public
    procedure MoveTo(X, Y: integer);
    // Screen-relative window bounds; ignores RelativeTo. Find the handle with THIDWindow.
    procedure MoveToWindow(Window: TWindowHandle; X, Y: integer);
    procedure Click(Button: TMouseButton = mbLeft);
    procedure DoubleClick(Button: TMouseButton = mbLeft);
    procedure ButtonDown(Button: TMouseButton = mbLeft);
    procedure ButtonUp(Button: TMouseButton = mbLeft);
    procedure Scroll(Steps: integer);
  published
    // Milliseconds MoveTo spends walking the pointer to its target in small
    // steps. Zero jumps straight there. On macOS the pointer and clicks need
    // the user to allow your program under Privacy & Security > Accessibility;
    // on Wayland they need a udev rule, which THIDPermission installs.
    property MovementDuration: cardinal read Duration write Duration default 0;
    // Ctrl, Alt, and Shift held during clicks, moves, and scrolls.
    property Modifiers: TShiftState read MouseModifiers write MouseModifiers default [];
    // A control whose client area anchors MoveTo coordinates. Leave empty for
    // screen coordinates. Clicks and scrolls always act wherever the pointer is.
    // Not on Wayland, which does not tell a program where its windows are.
    property RelativeTo: TControl read RelativeControl write SetRelativeControl;
    // A THIDWindow whose found window anchors MoveTo coordinates, measured from
    // its screen rectangle including native borders. Wins over RelativeTo;
    // call Find on it first.
    property RelativeWindow: THIDWindow read TargetWindow write SetRelativeWindow;
  end;

implementation

uses HIDMouseAndKeyInput, WindowManager
  {$ifdef linux}, WaylandMouseAndKeyInput{$endif};

{ Raises when input cannot be sent from here, saying why. }
procedure CheckAvailable(Component: TComponent; Modifiers: TShiftState);
begin
  if csDesigning in Component.ComponentState then raise EInvalidOperation.Create('Input is disabled in the designer.');
  {$ifdef linux}
  if (GetEnvironmentVariable('WAYLAND_DISPLAY') <> '') and not UInputReady then raise EInvalidOperation.Create(UInputHelp);
  {$endif}
  if Modifiers - [ssCtrl, ssAlt, ssShift] <> [] then raise EInvalidOperation.Create('Supported modifiers are Ctrl, Alt, and Shift.');
  if not Assigned(KeyInput) or not Assigned(MouseInput) then raise EInvalidOperation.Create('Input is not available on this platform.');
end;

constructor THIDKeyboard.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  Note := 'To type into other programs, %s needs your desktop''s permission to control the keyboard. ' +
    'Your desktop will ask next. Leave the "allow restoring" box ticked and you will not be asked again.';
end;

procedure THIDKeyboard.TypeText(const Text: string);
begin
  CheckAvailable(Self, Modifiers);
  {$ifdef linux}
  PortalNote := StringReplace(Note, '%s', Application.Title, []);
  {$endif}
  KeyInput.PressString(Text, TypingInterval);
end;

procedure THIDKeyboard.PressKey(Key: word);
begin
  CheckAvailable(Self, Modifiers);
  KeyInput.Apply(Modifiers);
  try
    KeyInput.Press(Key);
  finally
    KeyInput.Unapply(Modifiers);
  end;
end;

procedure THIDKeyboard.KeyDown(Key: word);
begin
  CheckAvailable(Self, Modifiers);
  KeyInput.Down(Key);
end;

procedure THIDKeyboard.KeyUp(Key: word);
begin
  CheckAvailable(Self, Modifiers);
  KeyInput.Up(Key);
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

procedure THIDMouse.MoveTo(X, Y: integer);
begin
  CheckAvailable(Self, Modifiers);
  {$ifdef linux}
  // a Wayland program is never told where its own windows are on the screen
  if (Assigned(RelativeTo) or Assigned(RelativeWindow)) and (GetEnvironmentVariable('WAYLAND_DISPLAY') <> '') then
    raise EInvalidOperation.Create('On Wayland MoveTo takes screen coordinates only; RelativeTo and RelativeWindow cannot work there.');
  {$endif}
  if Assigned(RelativeWindow) then MoveToWindow(RelativeWindow.Handle, X, Y)
  else if Assigned(RelativeTo) then MouseInput.Move(Modifiers, RelativeTo, X, Y, MovementDuration)
  else
    MouseInput.Move(Modifiers, X, Y, MovementDuration);
end;

procedure THIDMouse.MoveToWindow(Window: TWindowHandle; X, Y: integer);
var
  Info: TWindowInfo;
begin
  CheckAvailable(Self, Modifiers);
  EnsureWindowManager;
  if (WindowMgr = nil) or (Window = 0) then raise EInvalidOperation.Create('Window control is unavailable.');
  if not WindowMgr.GetWindowInfo(Window, Info) or not Info.IsVisible then
    raise EInvalidOperation.Create('The target window is unavailable or hidden.');
  MouseInput.Move(Modifiers, Info.Rect.Left + X, Info.Rect.Top + Y, MovementDuration);
end;

procedure THIDMouse.Click(Button: TMouseButton);
begin
  CheckAvailable(Self, Modifiers);
  MouseInput.Click(Button, Modifiers);
end;

procedure THIDMouse.DoubleClick(Button: TMouseButton);
begin
  CheckAvailable(Self, Modifiers);
  MouseInput.DblClick(Button, Modifiers);
end;

procedure THIDMouse.ButtonDown(Button: TMouseButton);
begin
  CheckAvailable(Self, Modifiers);
  MouseInput.Down(Button, Modifiers);
end;

procedure THIDMouse.ButtonUp(Button: TMouseButton);
begin
  CheckAvailable(Self, Modifiers);
  MouseInput.Up(Button, Modifiers);
end;

procedure THIDMouse.Scroll(Steps: integer);
begin
  CheckAvailable(Self, Modifiers);
  while Steps > 0 do
  begin
    MouseInput.ScrollUp(Modifiers);
    Dec(Steps);
  end;
  while Steps < 0 do
  begin
    MouseInput.ScrollDown(Modifiers);
    Inc(Steps);
  end;
end;

end.
