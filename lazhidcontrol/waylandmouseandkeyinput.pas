unit WaylandMouseAndKeyInput;

{$mode objfpc}{$H+}
// Copyright (c) 2026 Tony Stone. GPL-2.0-or-later; see COPYING.GPL2.

{ Keyboard and mouse for Wayland sessions.  There is no XTest to ask there,
  so this makes two virtual devices through the kernel's uinput, and the
  compositor reads them like hardware - any compositor, and X11 too.

  It needs write access to /dev/uinput, which is root's by default; the
  manual has the udev rule.  What a virtual keyboard cannot do: it presses
  keys, not characters, so text comes out as a US layout would type it and
  nothing beyond ASCII can be typed; and nothing reports the lock keys. }

interface

uses
  Classes, SysUtils, Controls, Forms, LCLType, BaseUnix, Unix,
  HIDKeyInputIntf, HIDMouseInputIntf;

type
  TWaylandMouseInput = class(TMouseInput)
  protected
    procedure DoDown(Button: TMouseButton); override;
    procedure DoMove(ScreenX, ScreenY: integer); override;
    procedure DoUp(Button: TMouseButton); override;
    procedure DoScrollUp; override;
    procedure DoScrollDown; override;
  end;

  TWaylandKeyInput = class(TKeyInput)
  protected
    procedure DoDown(Key: word); override;
    procedure DoUp(Key: word); override;
  public
    function GetCapsLockState: boolean; override;
    function GetNumLockState: boolean; override;
    function GetScrollLockState: boolean; override;
    procedure ToggleCapsLock; override;
    procedure ToggleNumLock; override;
    procedure ToggleScrollLock; override;
  end;

function InitializeWaylandMouseInput: TMouseInput;
function InitializeWaylandKeyInput: TKeyInput;

// Makes the two devices on first use.  False when /dev/uinput would not open.
function UInputReady: boolean;

const
  UInputHelp = 'Wayland input needs write access to /dev/uinput; the manual has the udev rule.';

implementation

const
  EV_SYN = 0;
  EV_KEY = 1;
  EV_REL = 2;
  EV_ABS = 3;
  REL_WHEEL = 8;
  ABS_X = 0;
  ABS_Y = 1;
  BTN_LEFT = $110;
  BTN_RIGHT = $111;
  BTN_MIDDLE = $112;
  AbsMax = 32767;

  UI_SET_EVBIT = $40045564;
  UI_SET_KEYBIT = $40045565;
  UI_SET_RELBIT = $40045566;
  UI_SET_ABSBIT = $40045567;
  UI_DEV_CREATE = $5501;
  UI_DEV_DESTROY = $5502;

type
  // struct uinput_user_dev
  TDeviceSetup = record
    Name: array[0..79] of char;
    BusType, Vendor, Product, Version: word;
    EffectsMax: longword;
    AbsMaximum, AbsMinimum, AbsFuzz, AbsFlat: array[0..63] of longint;
  end;

  // struct input_event
  TInputEvent = record
    Time: TTimeVal;
    Kind, Code: word;
    Value: longint;
  end;

var
  KeyboardFD: cint = -1;
  PointerFD: cint = -1;
  Tried: boolean = False;
  ButtonWentDown: QWord = 0;

procedure Emit(FD: cint; Kind, Code: word; Value: longint);
var
  Event: TInputEvent;
begin
  Event := Default(TInputEvent);
  Event.Kind := Kind;
  Event.Code := Code;
  Event.Value := Value;
  fpWrite(FD, Event, SizeOf(Event));
end;

// FD by reference: the first call is what opens the devices.
procedure Send(var FD: cint; Kind, Code: word; Value: longint);
begin
  if not UInputReady then Exit;
  Emit(FD, Kind, Code, Value);
  Emit(FD, EV_SYN, 0, 0);
end;

{ The pointer is modelled on a virtual machine's tablet - absolute X and Y,
  three buttons and a wheel - which compositors treat as a mouse that can
  be put anywhere on the desktop. }
function UInputReady: boolean;
var
  Setup: TDeviceSetup;
  Code: integer;
begin
  if not Tried then
  begin
    Tried := True;
    KeyboardFD := fpOpen('/dev/uinput', O_WRONLY or O_NONBLOCK);
    PointerFD := fpOpen('/dev/uinput', O_WRONLY or O_NONBLOCK);
    if (KeyboardFD >= 0) and (PointerFD >= 0) then
    begin
      Setup := Default(TDeviceSetup);
      Setup.Name := 'LazHIDControl keyboard';
      Setup.BusType := 6; // BUS_VIRTUAL
      fpIOCtl(KeyboardFD, UI_SET_EVBIT, Pointer(EV_KEY));
      for Code := 1 to 248 do
        fpIOCtl(KeyboardFD, UI_SET_KEYBIT, Pointer(PtrUInt(Code)));
      fpWrite(KeyboardFD, Setup, SizeOf(Setup));
      fpIOCtl(KeyboardFD, UI_DEV_CREATE, nil);

      Setup.Name := 'LazHIDControl pointer';
      Setup.AbsMaximum[ABS_X] := AbsMax;
      Setup.AbsMaximum[ABS_Y] := AbsMax;
      fpIOCtl(PointerFD, UI_SET_EVBIT, Pointer(EV_KEY));
      fpIOCtl(PointerFD, UI_SET_KEYBIT, Pointer(BTN_LEFT));
      fpIOCtl(PointerFD, UI_SET_KEYBIT, Pointer(BTN_RIGHT));
      fpIOCtl(PointerFD, UI_SET_KEYBIT, Pointer(BTN_MIDDLE));
      fpIOCtl(PointerFD, UI_SET_EVBIT, Pointer(EV_ABS));
      fpIOCtl(PointerFD, UI_SET_ABSBIT, Pointer(ABS_X));
      fpIOCtl(PointerFD, UI_SET_ABSBIT, Pointer(ABS_Y));
      fpIOCtl(PointerFD, UI_SET_EVBIT, Pointer(EV_REL));
      fpIOCtl(PointerFD, UI_SET_RELBIT, Pointer(REL_WHEEL));
      fpWrite(PointerFD, Setup, SizeOf(Setup));
      fpIOCtl(PointerFD, UI_DEV_CREATE, nil);
      // the compositor needs a moment to notice new hardware
      Sleep(700);
    end;
  end;
  Result := (KeyboardFD >= 0) and (PointerFD >= 0);
end;

// LCL virtual key to the kernel's key code, as a US keyboard is laid out.
function KernelKey(Key: word): word;
const
  Letters: array[VK_A..VK_Z] of byte = (30, 48, 46, 32, 18, 33, 34, 35, 23, 36, 37, 38, 50, 49, 24, 25, 16, 19, 31, 20, 22, 47, 17, 45, 21, 44);
begin
  case Key of
    VK_A..VK_Z: Result := Letters[Key];
    VK_1..VK_9: Result := Key - VK_1 + 2;
    VK_0: Result := 11;
    VK_F1..VK_F10: Result := Key - VK_F1 + 59;
    VK_F11: Result := 87;
    VK_F12: Result := 88;
    VK_ESCAPE: Result := 1;
    VK_LCL_MINUS: Result := 12;
    VK_LCL_EQUAL: Result := 13;
    VK_BACK: Result := 14;
    VK_TAB: Result := 15;
    VK_LCL_OPEN_BRACKET: Result := 26;
    VK_LCL_CLOSE_BRACKET: Result := 27;
    VK_RETURN: Result := 28;
    VK_CONTROL, VK_LCONTROL: Result := 29;
    VK_LCL_SEMI_COMMA: Result := 39;
    VK_LCL_QUOTE: Result := 40;
    VK_LCL_TILDE: Result := 41;
    VK_SHIFT, VK_LSHIFT: Result := 42;
    VK_LCL_BACKSLASH: Result := 43;
    VK_LCL_COMMA: Result := 51;
    VK_LCL_POINT: Result := 52;
    VK_LCL_SLASH: Result := 53;
    VK_RSHIFT: Result := 54;
    VK_MULTIPLY: Result := 55;
    VK_MENU, VK_LMENU: Result := 56;
    VK_SPACE: Result := 57;
    VK_CAPITAL: Result := 58;
    VK_NUMLOCK: Result := 69;
    VK_SCROLL: Result := 70;
    VK_NUMPAD7..VK_NUMPAD9: Result := Key - VK_NUMPAD7 + 71;
    VK_SUBTRACT: Result := 74;
    VK_NUMPAD4..VK_NUMPAD6: Result := Key - VK_NUMPAD4 + 75;
    VK_ADD: Result := 78;
    VK_NUMPAD1..VK_NUMPAD3: Result := Key - VK_NUMPAD1 + 79;
    VK_NUMPAD0: Result := 82;
    VK_DECIMAL: Result := 83;
    VK_RCONTROL: Result := 97;
    VK_DIVIDE: Result := 98;
    VK_SNAPSHOT: Result := 99;
    VK_RMENU: Result := 100;
    VK_HOME: Result := 102;
    VK_UP: Result := 103;
    VK_PRIOR: Result := 104;
    VK_LEFT: Result := 105;
    VK_RIGHT: Result := 106;
    VK_END: Result := 107;
    VK_DOWN: Result := 108;
    VK_NEXT: Result := 109;
    VK_INSERT: Result := 110;
    VK_DELETE: Result := 111;
    VK_PAUSE: Result := 119;
    VK_LWIN: Result := 125;
    else
      Result := 0;
  end;
end;

function KernelButton(Button: TMouseButton): word;
begin
  case Button of
    mbRight: Result := BTN_RIGHT;
    mbMiddle: Result := BTN_MIDDLE;
    else
      Result := BTN_LEFT;
  end;
end;

{ TWaylandMouseInput }

procedure TWaylandMouseInput.DoDown(Button: TMouseButton);
begin
  Send(PointerFD, EV_KEY, KernelButton(Button), 1);
  ButtonWentDown := GetTickCount64;
end;

procedure TWaylandMouseInput.DoUp(Button: TMouseButton);
begin
  // a press and release in the same instant is not always taken for a click
  while GetTickCount64 - ButtonWentDown < 40 do Sleep(5);
  Send(PointerFD, EV_KEY, KernelButton(Button), 0);
end;

procedure TWaylandMouseInput.DoMove(ScreenX, ScreenY: integer);
begin
  if not UInputReady then Exit;
  Emit(PointerFD, EV_ABS, ABS_X, MulDiv(ScreenX - Screen.DesktopLeft, AbsMax, Screen.DesktopWidth - 1));
  Emit(PointerFD, EV_ABS, ABS_Y, MulDiv(ScreenY - Screen.DesktopTop, AbsMax, Screen.DesktopHeight - 1));
  Emit(PointerFD, EV_SYN, 0, 0);
end;

procedure TWaylandMouseInput.DoScrollUp;
begin
  Send(PointerFD, EV_REL, REL_WHEEL, 1);
end;

procedure TWaylandMouseInput.DoScrollDown;
begin
  Send(PointerFD, EV_REL, REL_WHEEL, -1);
end;

{ TWaylandKeyInput }

procedure TWaylandKeyInput.DoDown(Key: word);
begin
  if KernelKey(Key) <> 0 then
    Send(KeyboardFD, EV_KEY, KernelKey(Key), 1);
end;

procedure TWaylandKeyInput.DoUp(Key: word);
begin
  if KernelKey(Key) <> 0 then
    Send(KeyboardFD, EV_KEY, KernelKey(Key), 0);
end;

function TWaylandKeyInput.GetCapsLockState: boolean;
begin
  Result := False;
end;

function TWaylandKeyInput.GetNumLockState: boolean;
begin
  Result := False;
end;

function TWaylandKeyInput.GetScrollLockState: boolean;
begin
  Result := False;
end;

procedure TWaylandKeyInput.ToggleCapsLock;
begin
  Press(VK_CAPITAL);
end;

procedure TWaylandKeyInput.ToggleNumLock;
begin
  Press(VK_NUMLOCK);
end;

procedure TWaylandKeyInput.ToggleScrollLock;
begin
  Press(VK_SCROLL);
end;

function InitializeWaylandMouseInput: TMouseInput;
begin
  Result := TWaylandMouseInput.Create;
end;

function InitializeWaylandKeyInput: TKeyInput;
begin
  Result := TWaylandKeyInput.Create;
end;

finalization
  if KeyboardFD >= 0 then
  begin
    fpIOCtl(KeyboardFD, UI_DEV_DESTROY, nil);
    fpClose(KeyboardFD);
  end;
  if PointerFD >= 0 then
  begin
    fpIOCtl(PointerFD, UI_DEV_DESTROY, nil);
    fpClose(PointerFD);
  end;
end.
