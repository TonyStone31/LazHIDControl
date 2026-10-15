unit XHotkeyInput;

{$mode objfpc}{$H+}
// Copyright (c) 2026 Tony Stone. 0BSD.

{ Hotkeys on X11: passive key grabs on the root window, on an X connection
  of this unit's own.  The connection is watched with the LCL's file-handle
  events, or a short timer where the widgetset has none, so nothing here
  depends on the toolkit. }

interface

uses
  Classes, ctypes, HotkeyInputIntf;

type
  TKeycodes = array of cint;

  TXHotkey = class(THotkey)
  private
    Codes: TKeycodes;
    Mods: cuint;
    NumMask: cuint;
    Held: boolean;
    LastTime: culong;
    function HasCode(Code: cint): boolean;
    procedure Grab(Take: boolean);
  protected
    procedure DoRegister; override;
    procedure DoUnregister; override;
  end;

implementation

uses
  SysUtils, Forms, ExtCtrls, LCLType, LCLIntf, InterfaceBase, X, XLib, KeySym, XKB, XKBlib;

const
  // GTK and Qt both read a 1 as "readable"
  WatchRead = 1;
  PollInterval = 30;
  // presses of a held key closer than this are key repeat, not new presses
  RepeatGap = 1000;
  BadAccessCode = 10;

type
  TXConnection = class
  private
    Watch: PEventHandler;
    Poll: TTimer;
    Detectable: boolean;
    procedure Readable(AData: PtrInt; AFlags: dword);
    procedure PollTimer(Sender: TObject);
    procedure KeyDown(const E: TXKeyEvent);
    procedure KeyUp(const E: TXKeyEvent);
  public
    Dpy: PDisplay;
    Root: TWindow;
    Hotkeys: TFPList;
    constructor Create;
    destructor Destroy; override;
    function Open: boolean;
    procedure ReadEvents;
  end;

var
  Conn: TXConnection;
  // the error trap; only one synchronous grab runs at a time, on the main thread
  TrapDpy: PDisplay;
  TrapCode: integer;
  OldHandler: TXErrorHandler;

function Trap(D: PDisplay; E: PXErrorEvent): cint; cdecl;
begin
  Result := 0;
  if D = TrapDpy then
  begin
    if TrapCode = 0 then TrapCode := E^.error_code;
  end
  else if Assigned(OldHandler) then
    Result := OldHandler(D, E);
end;

function SymOf(Key: word): TKeySym;
begin
  case Key of
    VK_A..VK_Z: Result := XK_a + Key - VK_A;
    VK_0..VK_9: Result := XK_0 + Key - VK_0;
    VK_F1..VK_F24: Result := XK_F1 + Key - VK_F1;
    VK_NUMPAD0..VK_NUMPAD9: Result := XK_KP_0 + Key - VK_NUMPAD0;
    VK_MULTIPLY: Result := XK_KP_Multiply;
    VK_ADD: Result := XK_KP_Add;
    VK_SEPARATOR: Result := XK_KP_Separator;
    VK_SUBTRACT: Result := XK_KP_Subtract;
    VK_DECIMAL: Result := XK_KP_Decimal;
    VK_DIVIDE: Result := XK_KP_Divide;
    VK_SPACE: Result := XK_space;
    VK_RETURN: Result := XK_Return;
    VK_ESCAPE: Result := XK_Escape;
    VK_TAB: Result := XK_Tab;
    VK_BACK: Result := XK_BackSpace;
    VK_INSERT: Result := XK_Insert;
    VK_DELETE: Result := XK_Delete;
    VK_HOME: Result := XK_Home;
    VK_END: Result := XK_End;
    VK_PRIOR: Result := XK_Prior;
    VK_NEXT: Result := XK_Next;
    VK_LEFT: Result := XK_Left;
    VK_RIGHT: Result := XK_Right;
    VK_UP: Result := XK_Up;
    VK_DOWN: Result := XK_Down;
    VK_PAUSE: Result := XK_Pause;
    VK_SNAPSHOT: Result := XK_Print;
    VK_APPS: Result := XK_Menu;
    VK_OEM_3: Result := XK_grave;
    VK_OEM_MINUS: Result := XK_minus;
    VK_OEM_PLUS: Result := XK_equal;
    VK_OEM_4: Result := XK_bracketleft;
    VK_OEM_6: Result := XK_bracketright;
    VK_OEM_5: Result := XK_backslash;
    VK_OEM_1: Result := XK_semicolon;
    VK_OEM_7: Result := XK_apostrophe;
    VK_OEM_COMMA: Result := XK_comma;
    VK_OEM_PERIOD: Result := XK_period;
    VK_OEM_2: Result := XK_slash;
    VK_OEM_102: Result := XK_less;
    else
      Result := NoSymbol;
  end;
end;

// The XKB name of the key's place on a US keyboard, for keys a layout may
// give another symbol, and for F13 and up, which X often maps to XF86 symbols.
function PlaceOf(Key: word): string;
const
  Rows: array[0..2] of string = ('QWERTYUIOP', 'ASDFGHJKL', 'ZXCVBNM');
  RowNames: array[0..2] of string = ('AD', 'AC', 'AB');
var
  R, I: integer;
begin
  Result := '';
  case Key of
    VK_A..VK_Z:
      for R := 0 to 2 do
      begin
        I := Pos(Chr(Key), Rows[R]);
        if I > 0 then Result := Format('%s%.2d', [RowNames[R], I]);
      end;
    VK_1..VK_9: Result := Format('AE%.2d', [Key - VK_0]);
    VK_0: Result := 'AE10';
    VK_F1..VK_F24: Result := Format('FK%.2d', [Key - VK_F1 + 1]);
    VK_OEM_3: Result := 'TLDE';
    VK_OEM_MINUS: Result := 'AE11';
    VK_OEM_PLUS: Result := 'AE12';
    VK_OEM_4: Result := 'AD11';
    VK_OEM_6: Result := 'AD12';
    VK_OEM_5: Result := 'BKSL';
    VK_OEM_1: Result := 'AC10';
    VK_OEM_7: Result := 'AC11';
    VK_OEM_COMMA: Result := 'AB08';
    VK_OEM_PERIOD: Result := 'AB09';
    VK_OEM_2: Result := 'AB10';
    VK_OEM_102: Result := 'LSGT';
  end;
end;

function KeycodeAtPlace(Dpy: PDisplay; const Place: string): cint;
var
  Desc: PXkbDescPtr;
  Code: integer;
begin
  Result := 0;
  if Place = '' then Exit;
  Desc := XkbGetMap(Dpy, 0, XkbUseCoreKbd);
  if Desc = nil then Exit;
  if (XkbGetNames(Dpy, XkbKeyNamesMask, Desc) = Success) and (Desc^.names <> nil) and (Desc^.names^.keys <> nil) then
    for Code := Desc^.min_key_code to Desc^.max_key_code do
      if Desc^.names^.keys[Code].Name = Place then
      begin
        Result := Code;
        Break;
      end;
  XkbFreeKeyboard(Desc, 0, True);
end;

// Every keycode with the symbol on its unshifted level; failing that, on
// its shifted level, as digits are on French layouts; failing that, the key
// at the US place.  Grabs go by keycode, so Shift+5 still matches the 5 key.
function KeycodesOf(Dpy: PDisplay; Sym: TKeySym; const Place: string): TKeycodes;
var
  Low, High, PerCode, Code, Column, N: cint;
  Map: PKeySym;
begin
  Result := nil;
  N := 0;
  Low := 0;
  High := 0;
  PerCode := 0;
  XDisplayKeycodes(Dpy, @Low, @High);
  Map := XGetKeyboardMapping(Dpy, Low, High - Low + 1, @PerCode);
  if Map <> nil then
  begin
    for Column := 0 to 1 do
      if (N = 0) and (Column < PerCode) then
        for Code := Low to High do
          if Map[(Code - Low) * PerCode + Column] = Sym then
          begin
            SetLength(Result, N + 1);
            Result[N] := Code;
            Inc(N);
          end;
    XFree(Map);
  end;
  if N = 0 then
  begin
    Code := KeycodeAtPlace(Dpy, Place);
    if Code <> 0 then
    begin
      SetLength(Result, 1);
      Result[0] := Code;
    end;
  end;
end;

// The modifier bit that a key with this symbol sets, or 0.
function MaskOf(Dpy: PDisplay; Sym: TKeySym): cuint;
var
  Map: PXModifierKeymap;
  Code: TKeyCode;
  I: integer;
begin
  Result := 0;
  Code := XKeysymToKeycode(Dpy, Sym);
  if Code = 0 then Exit;
  Map := XGetModifierMapping(Dpy);
  if Map = nil then Exit;
  for I := 0 to 8 * Map^.max_keypermod - 1 do
    if Map^.modifiermap[I] = Code then
    begin
      Result := 1 shl (I div Map^.max_keypermod);
      Break;
    end;
  XFreeModifiermap(Map);
end;

{ TXConnection }

constructor TXConnection.Create;
begin
  inherited Create;
  Hotkeys := TFPList.Create;
end;

destructor TXConnection.Destroy;
begin
  if (Watch <> nil) and (WidgetSet <> nil) then RemoveEventHandler(Watch);
  Poll.Free;
  if Dpy <> nil then XCloseDisplay(Dpy);
  Hotkeys.Free;
  inherited Destroy;
end;

function TXConnection.Open: boolean;
begin
  if Dpy = nil then
  begin
    Dpy := XOpenDisplay(nil);
    if Dpy = nil then Exit(False);
    Root := XDefaultRootWindow(Dpy);
    // a held key then repeats as presses alone, with no release between
    Detectable := XkbSetDetectableAutoRepeat(Dpy, True, nil);
    Watch := AddEventHandler(XConnectionNumber(Dpy), WatchRead, @Readable, 0);
    if Watch = nil then
    begin
      Poll := TTimer.Create(nil);
      Poll.Interval := PollInterval;
      Poll.OnTimer := @PollTimer;
    end;
  end;
  Result := True;
end;

procedure TXConnection.Readable(AData: PtrInt; AFlags: dword);
begin
  ReadEvents;
end;

procedure TXConnection.PollTimer(Sender: TObject);
begin
  ReadEvents;
end;

// Also called after our own synchronous requests, which can leave events
// in Xlib's queue where the file handle no longer shows them.
procedure TXConnection.ReadEvents;
var
  E: TXEvent;
begin
  if Dpy = nil then Exit;
  while XPending(Dpy) > 0 do
  begin
    XNextEvent(Dpy, @E);
    case E._type of
      KeyPress: KeyDown(E.xkey);
      KeyRelease: KeyUp(E.xkey);
    end;
  end;
end;

procedure TXConnection.KeyDown(const E: TXKeyEvent);
var
  I: integer;
  H: TXHotkey;
  Repeated: boolean;
begin
  for I := 0 to Hotkeys.Count - 1 do
  begin
    H := TXHotkey(Hotkeys[I]);
    if H.HasCode(E.keycode) and (E.state and $FF and not (LockMask or H.NumMask) = H.Mods) then
    begin
      Repeated := H.Held and (E.time - H.LastTime < RepeatGap);
      H.Held := True;
      H.LastTime := E.time;
      // queued, so a handler that opens a modal form does not hold up this watch
      if not Repeated then H.PressedLater;
      Exit;
    end;
  end;
end;

procedure TXConnection.KeyUp(const E: TXKeyEvent);
var
  Next: TXEvent;
  I: integer;
begin
  // without detectable repeat a held key sends release and press with one time
  if not Detectable and (XEventsQueued(Dpy, QueuedAfterReading) > 0) then
  begin
    XPeekEvent(Dpy, @Next);
    if (Next._type = KeyPress) and (Next.xkey.keycode = E.keycode) and (Next.xkey.time = E.time) then Exit;
  end;
  for I := 0 to Hotkeys.Count - 1 do
    if TXHotkey(Hotkeys[I]).HasCode(E.keycode) then TXHotkey(Hotkeys[I]).Held := False;
end;

{ TXHotkey }

function TXHotkey.HasCode(Code: cint): boolean;
var
  C: cint;
begin
  Result := False;
  for C in Codes do
    if C = Code then Exit(True);
end;

// X counts Caps Lock and Num Lock as modifiers, so each state of them
// needs a grab of its own.
procedure TXHotkey.Grab(Take: boolean);
var
  Code: cint;
  J: integer;
  Locks: cuint;
begin
  for Code in Codes do
    for J := 0 to 3 do
    begin
      Locks := 0;
      if J and 1 <> 0 then Locks := LockMask;
      if J and 2 <> 0 then Locks := Locks or NumMask;
      if Take then XGrabKey(Conn.Dpy, Code, Mods or Locks, Conn.Root, 0, GrabModeAsync, GrabModeAsync)
      else
        XUngrabKey(Conn.Dpy, Code, Mods or Locks, Conn.Root);
    end;
end;

procedure TXHotkey.DoRegister;
var
  Dpy: PDisplay;
  Sym: TKeySym;
  Alt: cuint;
  Code, I: integer;
  Other: TXHotkey;
begin
  if GetCurrentThreadId <> MainThreadID then raise EHotkeyError.Create('Hotkeys must be registered from the main thread.');
  if Shift - [ssCtrl, ssAlt, ssShift] <> [] then raise EHotkeyError.Create('Only Ctrl, Alt and Shift can be part of a hotkey.');
  Sym := SymOf(Key);
  if Sym = NoSymbol then raise EHotkeyError.CreateFmt('Key %d cannot be used as a hotkey.', [Key]);
  if Conn = nil then Conn := TXConnection.Create;
  if not Conn.Open then Exit;
  Dpy := Conn.Dpy;

  Codes := KeycodesOf(Dpy, Sym, PlaceOf(Key));
  if Codes = nil then raise EHotkeyError.CreateFmt('This keyboard has no %s key.', [XKeysymToString(Sym)]);
  Mods := 0;
  if ssShift in Shift then Mods := Mods or ShiftMask;
  if ssCtrl in Shift then Mods := Mods or ControlMask;
  if ssAlt in Shift then
  begin
    Alt := MaskOf(Dpy, XK_Alt_L);
    if Alt = 0 then Alt := MaskOf(Dpy, XK_Alt_R);
    if Alt = 0 then Alt := Mod1Mask;
    Mods := Mods or Alt;
  end;
  NumMask := MaskOf(Dpy, XK_Num_Lock);

  // a second grab of ours would not fail, so ask the list
  for I := 0 to Conn.Hotkeys.Count - 1 do
  begin
    Other := TXHotkey(Conn.Hotkeys[I]);
    if Other.Mods = Mods then
      for Code in Codes do
        if Other.HasCode(Code) then
        begin
          Codes := nil;
          raise EHotkeyError.Create('This combination is already registered in this program.');
        end;
  end;

  // another client's grab comes back as an error, so trap it and wait for it
  XSync(Dpy, False);
  TrapDpy := Dpy;
  TrapCode := 0;
  OldHandler := XSetErrorHandler(@Trap);
  Grab(True);
  XSync(Dpy, False);
  XSetErrorHandler(OldHandler);
  TrapDpy := nil;

  if TrapCode <> 0 then
  begin
    Grab(False);
    XSync(Dpy, False);
    Codes := nil;
    Conn.ReadEvents;
    if TrapCode = BadAccessCode then raise EHotkeyError.Create('Another program already uses this combination.');
    raise EHotkeyError.CreateFmt('The X server refused this combination (error %d).', [TrapCode]);
  end;

  Held := False;
  Conn.Hotkeys.Add(Self);
  Registered := True;
  Conn.ReadEvents;
end;

procedure TXHotkey.DoUnregister;
begin
  // the connection is gone once this unit is finalized, and its grabs with it
  if Conn = nil then Exit;
  Conn.Hotkeys.Remove(Self);
  Grab(False);
  XSync(Conn.Dpy, False);
  Codes := nil;
  Conn.ReadEvents;
end;

finalization
  FreeAndNil(Conn);
end.
