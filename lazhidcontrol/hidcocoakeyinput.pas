unit HIDCocoaKeyInput;

{$mode objfpc}{$H+}
// Copyright (c) 2026 Tony Stone. 0BSD.

{ The keyboard on macOS, through Quartz events posted to the HID event tap.
  Without the Accessibility permission nothing arrives, quietly.  Text goes
  as Unicode strings on key events, so the layout and Caps Lock do not
  change it. }

interface

uses
  Classes, SysUtils, LCLType, MacOSAll, HIDKeyInputIntf;

const
  // The answer for a key a Mac does not have; 0 is the Mac's A key.
  NoMacCode = $FFFF;

// The Mac virtual key code (kVK_...) for an LCL virtual key.
function VKToMacCode(AKey: word): word;
// Shift, Control, Option and Command as held through KeyInput right now,
// for mouse events to carry.
function HeldModifierFlags: CGEventFlags;

function InitializeCocoaKeyInput: TKeyInput;

implementation

uses
  Forms;

  {$linkframework IOKit}

// Caps Lock is kept by the HID system; a posted key press does not switch it.
function IOServiceMatching(Name: pansichar): Pointer; cdecl; external;
function IOServiceGetMatchingService(MainPort: longword; Matching: Pointer): longword; cdecl; external;
function IOServiceOpen(Service, OwningTask, Kind: longword; out Connect: longword): longint; cdecl; external;
function IOServiceClose(Connect: longword): longint; cdecl; external;
function IOObjectRelease(AObject: longword): longint; cdecl; external;
function IOHIDSetModifierLockState(Connect: longword; Selector: longint; State: boolean): longint; cdecl; external;
function IOHIDGetModifierLockState(Connect: longword; Selector: longint; out State: boolean): longint; cdecl; external;

var
  mach_task_self_: longword; cvar; external;

const
  kVK_RightCommand = $36;
  // MacOSAll leaves these out until IOKit is translated; values from IOLLEvent.h
  kCGEventFlagMaskAlphaShift = $00010000;
  kCGEventFlagMaskShift = $00020000;
  kCGEventFlagMaskControl = $00040000;
  kCGEventFlagMaskAlternate = $00080000;
  kCGEventFlagMaskCommand = $00100000;
  // what the system marks a key with itself: keypad and arrows, and Fn on
  // the function and navigation keys
  KeyKindBits = $00200000 or $00800000;
  ParamConnection = 1;
  CapsLockSelector = 1;

type
  TCocoaKeyInput = class(TKeyInput)
  private
    procedure Post(Code: word; IsDown: boolean; Flags: CGEventFlags; KeepKind: boolean; Text: UniCharPtr; TextLength: integer);
    procedure PostKey(Code: word; IsDown: boolean);
  protected
    procedure DoDown(Key: word); override;
    procedure DoUp(Key: word); override;
  public
    procedure PressASCIIChar(ch: char); override;
    procedure PressUnicodeChar(unicode: cardinal); override;
    function GetCapsLockState: boolean; override;
    function GetNumLockState: boolean; override;
    function GetScrollLockState: boolean; override;
    procedure ToggleCapsLock; override;
    procedure ToggleNumLock; override;
    procedure ToggleScrollLock; override;
  end;

var
  // the modifier keys held down through this unit, by Mac key code
  HeldKeys: set of byte = [];

function InitializeCocoaKeyInput: TKeyInput;
begin
  Result := TCocoaKeyInput.Create;
end;

function VKToMacCode(AKey: word): word;
const
  Letters: array[VK_A..VK_Z] of byte = (kVK_ANSI_A, kVK_ANSI_B, kVK_ANSI_C, kVK_ANSI_D, kVK_ANSI_E,
    kVK_ANSI_F, kVK_ANSI_G, kVK_ANSI_H, kVK_ANSI_I, kVK_ANSI_J, kVK_ANSI_K, kVK_ANSI_L, kVK_ANSI_M,
    kVK_ANSI_N, kVK_ANSI_O, kVK_ANSI_P, kVK_ANSI_Q, kVK_ANSI_R, kVK_ANSI_S, kVK_ANSI_T, kVK_ANSI_U,
    kVK_ANSI_V, kVK_ANSI_W, kVK_ANSI_X, kVK_ANSI_Y, kVK_ANSI_Z);
  Digits: array[VK_0..VK_9] of byte = (kVK_ANSI_0, kVK_ANSI_1, kVK_ANSI_2, kVK_ANSI_3, kVK_ANSI_4,
    kVK_ANSI_5, kVK_ANSI_6, kVK_ANSI_7, kVK_ANSI_8, kVK_ANSI_9);
  Pad: array[VK_NUMPAD0..VK_NUMPAD9] of byte = (kVK_ANSI_Keypad0, kVK_ANSI_Keypad1, kVK_ANSI_Keypad2,
    kVK_ANSI_Keypad3, kVK_ANSI_Keypad4, kVK_ANSI_Keypad5, kVK_ANSI_Keypad6, kVK_ANSI_Keypad7,
    kVK_ANSI_Keypad8, kVK_ANSI_Keypad9);
  Functions: array[VK_F1..VK_F20] of byte = (kVK_F1, kVK_F2, kVK_F3, kVK_F4, kVK_F5, kVK_F6, kVK_F7,
    kVK_F8, kVK_F9, kVK_F10, kVK_F11, kVK_F12, kVK_F13, kVK_F14, kVK_F15, kVK_F16, kVK_F17, kVK_F18,
    kVK_F19, kVK_F20);
begin
  case AKey of
    VK_A..VK_Z: Result := Letters[AKey];
    VK_0..VK_9: Result := Digits[AKey];
    VK_NUMPAD0..VK_NUMPAD9: Result := Pad[AKey];
    VK_F1..VK_F20: Result := Functions[AKey];
    VK_LCL_MINUS: Result := kVK_ANSI_Minus;
    VK_LCL_EQUAL: Result := kVK_ANSI_Equal;
    VK_LCL_OPEN_BRACKET: Result := kVK_ANSI_LeftBracket;
    VK_LCL_CLOSE_BRACKET: Result := kVK_ANSI_RightBracket;
    VK_LCL_BACKSLASH: Result := kVK_ANSI_Backslash;
    VK_LCL_SEMI_COMMA: Result := kVK_ANSI_Semicolon;
    VK_LCL_QUOTE: Result := kVK_ANSI_Quote;
    VK_LCL_COMMA: Result := kVK_ANSI_Comma;
    VK_LCL_POINT: Result := kVK_ANSI_Period;
    VK_LCL_SLASH: Result := kVK_ANSI_Slash;
    VK_LCL_TILDE: Result := kVK_ANSI_Grave;
    VK_OEM_102: Result := kVK_ISO_Section;
    VK_MULTIPLY: Result := kVK_ANSI_KeypadMultiply;
    VK_ADD: Result := kVK_ANSI_KeypadPlus;
    VK_SUBTRACT: Result := kVK_ANSI_KeypadMinus;
    VK_DECIMAL: Result := kVK_ANSI_KeypadDecimal;
    VK_DIVIDE: Result := kVK_ANSI_KeypadDivide;
    VK_NUMLOCK, VK_CLEAR: Result := kVK_ANSI_KeypadClear;
    VK_RETURN: Result := kVK_Return;
    VK_TAB: Result := kVK_Tab;
    VK_SPACE: Result := kVK_Space;
    VK_ESCAPE: Result := kVK_Escape;
    VK_BACK: Result := kVK_Delete;
    VK_DELETE: Result := kVK_ForwardDelete;
    VK_HELP, VK_INSERT: Result := kVK_Help;
    VK_HOME: Result := kVK_Home;
    VK_END: Result := kVK_End;
    VK_PRIOR: Result := kVK_PageUp;
    VK_NEXT: Result := kVK_PageDown;
    VK_LEFT: Result := kVK_LeftArrow;
    VK_RIGHT: Result := kVK_RightArrow;
    VK_UP: Result := kVK_UpArrow;
    VK_DOWN: Result := kVK_DownArrow;
    VK_SHIFT, VK_LSHIFT: Result := kVK_Shift;
    VK_RSHIFT: Result := kVK_RightShift;
    VK_CONTROL, VK_LCONTROL: Result := kVK_Control;
    VK_RCONTROL: Result := kVK_RightControl;
    VK_MENU, VK_LMENU: Result := kVK_Option;
    VK_RMENU: Result := kVK_RightOption;
    VK_LWIN: Result := kVK_Command;
    VK_RWIN: Result := kVK_RightCommand;
    VK_CAPITAL: Result := kVK_CapsLock;
    VK_VOLUME_UP: Result := kVK_VolumeUp;
    VK_VOLUME_DOWN: Result := kVK_VolumeDown;
    VK_VOLUME_MUTE: Result := kVK_Mute;
    // as Mac keyboards label them
    VK_SNAPSHOT, VK_PRINT: Result := kVK_F13;
    VK_SCROLL: Result := kVK_F14;
    VK_PAUSE: Result := kVK_F15;
    else
      Result := NoMacCode;
  end;
end;

// The flag a Mac key code sets while it is down, or 0 for a key that is not
// a modifier.
function ModifierFlag(Code: word): CGEventFlags;
begin
  case Code of
    kVK_Shift, kVK_RightShift: Result := kCGEventFlagMaskShift;
    kVK_Control, kVK_RightControl: Result := kCGEventFlagMaskControl;
    kVK_Option, kVK_RightOption: Result := kCGEventFlagMaskAlternate;
    kVK_Command, kVK_RightCommand: Result := kCGEventFlagMaskCommand;
    else
      Result := 0;
  end;
end;

function HeldModifierFlags: CGEventFlags;
var
  Code: byte;
begin
  Result := 0;
  for Code in HeldKeys do Result := Result or ModifierFlag(Code);
end;

// One key event, built the way that matches letter and digit hotkeys: a
// fresh source on the HID system state, and the flags set whole.  KeepKind
// keeps only what the system marks the key itself with.
procedure TCocoaKeyInput.Post(Code: word; IsDown: boolean; Flags: CGEventFlags; KeepKind: boolean; Text: UniCharPtr; TextLength: integer);
var
  Source: CGEventSourceRef;
  Event: CGEventRef;
begin
  Source := CGEventSourceCreate(kCGEventSourceStateHIDSystemState);
  Event := CGEventCreateKeyboardEvent(Source, Code, Ord(IsDown));
  if Event <> nil then
  begin
    if KeepKind then Flags := Flags or (CGEventGetFlags(Event) and KeyKindBits);
    CGEventSetFlags(Event, Flags);
    if TextLength > 0 then CGEventKeyboardSetUnicodeString(Event, TextLength, Text);
    CGEventPost(kCGHIDEventTap, Event);
    CFRelease(Event);
  end;
  if Source <> nil then CFRelease(Source);
end;

// Modifiers go as plain key presses and releases; every event carries exactly
// the modifiers held at that moment.
procedure TCocoaKeyInput.PostKey(Code: word; IsDown: boolean);
var
  Flag: CGEventFlags;
begin
  Flag := ModifierFlag(Code);
  if Flag <> 0 then
    if IsDown then Include(HeldKeys, Code)
    else
      Exclude(HeldKeys, Code);
  Post(Code, IsDown, HeldModifierFlags, Flag = 0, nil, 0);
end;

procedure TCocoaKeyInput.DoDown(Key: word);
var
  Code: word;
begin
  Code := VKToMacCode(Key);
  if Code = NoMacCode then raise Exception.CreateFmt('Key %d does not exist on a Mac keyboard.', [Key]);
  PostKey(Code, True);
end;

procedure TCocoaKeyInput.DoUp(Key: word);
var
  Code: word;
begin
  Code := VKToMacCode(Key);
  if Code = NoMacCode then raise Exception.CreateFmt('Key %d does not exist on a Mac keyboard.', [Key]);
  PostKey(Code, False);
end;

procedure TCocoaKeyInput.PressUnicodeChar(unicode: cardinal);
var
  UTF16: array[0..1] of UniChar;
  Count: integer;
begin
  if (unicode = 0) or (unicode > $10FFFF) then Exit;
  if unicode < $10000 then
  begin
    UTF16[0] := unicode;
    UTF16[1] := 0;
    Count := 1;
  end
  else
  begin
    Dec(unicode, $10000);
    UTF16[0] := $D800 + (unicode shr 10);
    UTF16[1] := $DC00 + (unicode and $3FF);
    Count := 2;
  end;
  // the text itself on a key event, with no modifiers to change it
  Post(0, True, 0, False, @UTF16[0], Count);
  Post(0, False, 0, False, @UTF16[0], Count);
  Application.ProcessMessages;
end;

procedure TCocoaKeyInput.PressASCIIChar(ch: char);
begin
  case ch of
    #9: Press(VK_TAB);
    #10, #13: Press(VK_RETURN);
    #8: Press(VK_BACK);
    #27: Press(VK_ESCAPE);
    ' '..#126: PressUnicodeChar(Ord(ch));
  end;
end;

// A connection to the HID system, or 0 when there is none to be had.
function OpenHIDSystem: longword;
var
  Service: longword;
begin
  Result := 0;
  Service := IOServiceGetMatchingService(0, IOServiceMatching('IOHIDSystem'));
  if Service = 0 then Exit;
  if IOServiceOpen(Service, mach_task_self_, ParamConnection, Result) <> 0 then Result := 0;
  IOObjectRelease(Service);
end;

// The HID system knows the lock at once; the event source's flags are the
// fallback, and can lag just after a toggle.
function TCocoaKeyInput.GetCapsLockState: boolean;
var
  Connect: longword;
  State, Known: boolean;
begin
  Known := False;
  State := False;
  Connect := OpenHIDSystem;
  if Connect <> 0 then
  begin
    Known := IOHIDGetModifierLockState(Connect, CapsLockSelector, State) = 0;
    IOServiceClose(Connect);
  end;
  if Known then Result := State
  else
    Result := CGEventSourceFlagsState(kCGEventSourceStateHIDSystemState) and kCGEventFlagMaskAlphaShift <> 0;
end;

function TCocoaKeyInput.GetNumLockState: boolean;
begin
  Result := False;
end;

function TCocoaKeyInput.GetScrollLockState: boolean;
begin
  Result := False;
end;

// Reads and flips the lock on one connection, so two toggles in a row put
// it back as it was.
procedure TCocoaKeyInput.ToggleCapsLock;
var
  Connect: longword;
  State, Done: boolean;
begin
  Done := False;
  State := False;
  Connect := OpenHIDSystem;
  if Connect <> 0 then
  begin
    Done := (IOHIDGetModifierLockState(Connect, CapsLockSelector, State) = 0) and (IOHIDSetModifierLockState(Connect, CapsLockSelector, not State) = 0);
    IOServiceClose(Connect);
  end;
  if not Done then Press(VK_CAPITAL);
  Application.ProcessMessages;
end;

procedure TCocoaKeyInput.ToggleNumLock;
begin
end;

procedure TCocoaKeyInput.ToggleScrollLock;
begin
end;

end.
