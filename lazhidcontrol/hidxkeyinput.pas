unit HIDXKeyInput;

{$mode objfpc}{$H+}
// Copyright (c) 2026 Tony Stone. 0BSD.

{ The keyboard on X11, through the XTEST extension.  Keys and characters
  are looked up in the active XKB keymap.  A character no key carries is
  typed by lending a spare keycode its keysym for a while, so it arrives
  without any input method's help. }

interface

uses
  HIDKeyInputIntf;

function InitializeXKeyInput: TKeyInput;

implementation

uses
  Classes, SysUtils, Forms, LCLType, ctypes, X, XLib, XKB, XKBlib, KeySym;

function XTestQueryExtension(Dpy: PDisplay; EventBase, ErrorBase, Major, Minor: Pcint): TBool; cdecl; external 'Xtst';
function XTestFakeKeyEvent(Dpy: PDisplay; KeyCode: cuint; IsPress: TBool; Delay: culong): cint; cdecl; external 'Xtst';

const
  // a few dozen spare keycodes, so typing rarely has to rebind one in use
  SpareCount = 40;
  // how long a slow client may take to read a press before its keycode is rebound
  ReuseDelay = 500;
  // input-method daemons such as IBus can swallow a key while they reload the map
  RebindDelay = 40;
  // the keymap is fetched again when older than this, in case the layout changed
  KeymapAge = 1000;

type
  { The parts of XKB's client keymap read here, laid out as XKBstr.h has
    them.  The XKB unit of FPC 3.2 gets some of them wrong (a one-byte
    "active", a one-byte "offset"), so they are not taken from it. }
  {$push}{$packrecords c}
  TKbMods = record
    mask, real_mods: byte;
    vmods: word;
  end;

  TKbMapEntry = record
    active: TBool;
    level: byte;
    mods: TKbMods;
  end;
  PKbMapEntry = ^TKbMapEntry;

  TKbKeyType = record
    mods: TKbMods;
    num_levels, map_count: byte;
    map: PKbMapEntry;
    preserve: Pointer;
    Name: TAtom;
    level_names: Pointer;
  end;
  PKbKeyType = ^TKbKeyType;

  TKbSymMap = record
    kt_index: array[0..XkbNumKbdGroups - 1] of byte;
    group_info, Width: byte;
    offset: word;
  end;
  PKbSymMap = ^TKbSymMap;

  TKbClientMap = record
    size_types, num_types: byte;
    types: PKbKeyType;
    size_syms, num_syms: word;
    syms: PKeySym;
    key_sym_map: PKbSymMap;
    modmap: pbyte;
  end;
  PKbClientMap = ^TKbClientMap;
  {$pop}

  TSpareKey = record
    Code: TKeyCode;
    Saved: array of TKeySym;
    Sym: TKeySym;
    Pressed: QWord;
  end;

  TXKeyInput = class(TKeyInput)
  private
    Dpy: PDisplay;
    Desc: PXkbDescPtr;
    DescTime: QWord;
    Spares: array of TSpareKey;
    SparesReady: boolean;
    NextSpare: integer;
    SymsPerCode: cint;
    CapsSwitchedOff: boolean;
    function Keymap: PXkbDescPtr;
    function IsSpare(Code: integer): boolean;
    function KeycodeOf(Sym: TKeySym): TKeyCode;
    function KeycodeAt(const Place: string): TKeyCode;
    function IsFunctionKeyPlace(Code: integer): boolean;
    function KeycodeFor(Key: word): TKeyCode;
    function FindChar(Sym: TKeySym; Group: integer; out Code: TKeyCode; out NeedShift: boolean): boolean;
    function Holds(Code: TKeyCode; Sym: TKeySym): boolean;
    procedure SendKey(Code: TKeyCode; IsPress: boolean);
    procedure PrepareSpares;
    procedure RestoreSpares;
    procedure ReturnSpares(Sym: TKeySym);
    procedure LockCaps(TurnOn: boolean);
    function Led(Bit: integer): boolean;
  protected
    procedure DoDown(Key: word); override;
    procedure DoUp(Key: word); override;
    procedure StringDone; override;
  public
    constructor Create;
    destructor Destroy; override;
    procedure PressASCIIChar(ch: char); override;
    procedure PressUnicodeChar(unicode: cardinal); override;
    function GetCapsLockState: boolean; override;
    function GetNumLockState: boolean; override;
    function GetScrollLockState: boolean; override;
    procedure ToggleCapsLock; override;
    procedure ToggleNumLock; override;
    procedure ToggleScrollLock; override;
  end;

function InitializeXKeyInput: TKeyInput;
begin
  Result := TXKeyInput.Create;
end;

function KeysymOf(Key: word): TKeySym;
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
    VK_LEFT: Result := XK_Left;
    VK_UP: Result := XK_Up;
    VK_RIGHT: Result := XK_Right;
    VK_DOWN: Result := XK_Down;
    VK_PRIOR: Result := XK_Prior;
    VK_NEXT: Result := XK_Next;
    VK_HOME: Result := XK_Home;
    VK_END: Result := XK_End;
    VK_INSERT: Result := XK_Insert;
    VK_DELETE: Result := XK_Delete;
    VK_RETURN: Result := XK_Return;
    VK_TAB: Result := XK_Tab;
    VK_ESCAPE: Result := XK_Escape;
    VK_BACK: Result := XK_BackSpace;
    VK_SPACE: Result := XK_space;
    VK_LCL_TILDE: Result := XK_grave;
    VK_LCL_MINUS: Result := XK_minus;
    VK_LCL_EQUAL: Result := XK_equal;
    VK_LCL_OPEN_BRACKET: Result := XK_bracketleft;
    VK_LCL_CLOSE_BRACKET: Result := XK_bracketright;
    VK_LCL_BACKSLASH: Result := XK_backslash;
    VK_LCL_SEMI_COMMA: Result := XK_semicolon;
    VK_LCL_QUOTE: Result := XK_apostrophe;
    VK_LCL_COMMA: Result := XK_comma;
    VK_LCL_POINT: Result := XK_period;
    VK_LCL_SLASH: Result := XK_slash;
    VK_OEM_102: Result := XK_less;
    VK_SHIFT, VK_LSHIFT: Result := XK_Shift_L;
    VK_RSHIFT: Result := XK_Shift_R;
    VK_CONTROL, VK_LCONTROL: Result := XK_Control_L;
    VK_RCONTROL: Result := XK_Control_R;
    VK_MENU, VK_LMENU: Result := XK_Alt_L;
    VK_RMENU: Result := XK_Alt_R;
    VK_CAPITAL: Result := XK_Caps_Lock;
    VK_NUMLOCK: Result := XK_Num_Lock;
    VK_SCROLL: Result := XK_Scroll_Lock;
    VK_PAUSE: Result := XK_Pause;
    VK_SNAPSHOT, VK_PRINT: Result := XK_Print;
    VK_LWIN: Result := XK_Super_L;
    VK_RWIN: Result := XK_Super_R;
    VK_APPS: Result := XK_Menu;
    else
      Result := NoSymbol;
  end;
end;

// The XKB name of the place a key has on a US keyboard, for the keys whose
// symbol depends on the layout or keymap (F13 and up often carry media keys);
// empty for the rest.
function PlaceOf(Key: word): string;
const
  // A to Z
  Letters = 'AC01AB05AB03AC03AD03AC04AC05AC06AD08AC07AC08AC09AB07' + 'AB06AD09AD10AD01AD04AC02AD05AD07AB04AD02AB02AD06AB01';
begin
  case Key of
    VK_A..VK_Z: Result := Copy(Letters, (Key - VK_A) * 4 + 1, 4);
    VK_1..VK_9: Result := 'AE0' + Chr(Ord('1') + Key - VK_1);
    VK_0: Result := 'AE10';
    VK_LCL_MINUS: Result := 'AE11';
    VK_LCL_EQUAL: Result := 'AE12';
    VK_LCL_OPEN_BRACKET: Result := 'AD11';
    VK_LCL_CLOSE_BRACKET: Result := 'AD12';
    VK_LCL_BACKSLASH: Result := 'BKSL';
    VK_LCL_SEMI_COMMA: Result := 'AC10';
    VK_LCL_QUOTE: Result := 'AC11';
    VK_LCL_COMMA: Result := 'AB08';
    VK_LCL_POINT: Result := 'AB09';
    VK_LCL_SLASH: Result := 'AB10';
    VK_LCL_TILDE: Result := 'TLDE';
    VK_OEM_102: Result := 'LSGT';
    VK_F1..VK_F24: Result := Format('FK%.2d', [Key - VK_F1 + 1]);
    else
      Result := '';
  end;
end;

// The group a key really uses when the keyboard is in Group, by the key's
// own rule for groups it does not have.
function KeyGroup(Info: byte; Group: integer): integer;
var
  Count: integer;
begin
  Count := Info and $0F;
  Result := Group;
  if Result < Count then Exit;
  case Info and $C0 of
    $40: Result := Count - 1;
    $80:
    begin
      Result := (Info shr 4) and 3;
      if Result >= Count then Result := 0;
    end;
    else
      Result := Group mod Count;
  end;
end;

// True when Shift alone selects the second level of keys of this type.
function ShiftGivesLevel2(Kind: PKbKeyType): boolean;
var
  I: integer;
begin
  Result := False;
  for I := 0 to Kind^.map_count - 1 do
    if (Kind^.map[I].active <> 0) and (Kind^.map[I].level = 1) and (Kind^.map[I].mods.mask = ShiftMask) then Exit(True);
end;

constructor TXKeyInput.Create;
var
  Dummy: cint;
begin
  inherited Create;
  Dpy := XOpenDisplay(nil);
  if (Dpy <> nil) and (XTestQueryExtension(Dpy, @Dummy, @Dummy, @Dummy, @Dummy) = 0) then
  begin
    XCloseDisplay(Dpy);
    Dpy := nil;
  end;
  if Dpy <> nil then XkbUseExtension(Dpy, nil, nil);
end;

destructor TXKeyInput.Destroy;
begin
  if Dpy <> nil then
  begin
    RestoreSpares;
    if Desc <> nil then XkbFreeKeyboard(Desc, 0, True);
    XCloseDisplay(Dpy);
  end;
  inherited Destroy;
end;

function TXKeyInput.Keymap: PXkbDescPtr;
begin
  if (Desc = nil) or (GetTickCount64 - DescTime > KeymapAge) then
  begin
    if Desc <> nil then XkbFreeKeyboard(Desc, 0, True);
    Desc := XkbGetMap(Dpy, XkbKeyTypesMask or XkbKeySymsMask, XkbUseCoreKbd);
    // the key names say where each key sits, whatever the layout
    if Desc <> nil then XkbGetNames(Dpy, XkbKeyNamesMask, Desc);
    DescTime := GetTickCount64;
  end;
  Result := Desc;
  if (Result <> nil) and (Result^.map = nil) then Result := nil;
end;

function TXKeyInput.IsSpare(Code: integer): boolean;
var
  I: integer;
begin
  Result := False;
  for I := 0 to High(Spares) do
    if Spares[I].Code = Code then Exit(True);
end;

// The keycode that carries Sym anywhere in the keymap, first group and first
// level first, the way XKeysymToKeycode searches.
function TXKeyInput.KeycodeOf(Sym: TKeySym): TKeyCode;
var
  Map: PXkbDescPtr;
  Client: PKbClientMap;
  Group, Level, Code: integer;
  Entry: PKbSymMap;
begin
  Result := 0;
  Map := Keymap;
  if (Map = nil) or (Sym = NoSymbol) then Exit;
  Client := PKbClientMap(Map^.map);
  for Group := 0 to XkbNumKbdGroups - 1 do
    for Level := 0 to 1 do
      for Code := Map^.min_key_code to Map^.max_key_code do
      begin
        Entry := @Client^.key_sym_map[Code];
        if (Group < Entry^.group_info and $0F) and (Level < Entry^.Width) and (Client^.syms[Entry^.offset + Group * Entry^.Width + Level] = Sym) and
          not IsSpare(Code) then
          Exit(Code);
      end;
end;

// Where the layout has Sym in Group: on a key's first level, or its second
// with Shift.
function TXKeyInput.FindChar(Sym: TKeySym; Group: integer; out Code: TKeyCode; out NeedShift: boolean): boolean;
var
  Map: PXkbDescPtr;
  Client: PKbClientMap;
  K, G, Base: integer;
  Entry: PKbSymMap;
  Kind: PKbKeyType;
begin
  Result := False;
  Code := 0;
  NeedShift := False;
  Map := Keymap;
  if Map = nil then Exit;
  Client := PKbClientMap(Map^.map);
  for K := Map^.min_key_code to Map^.max_key_code do
  begin
    Entry := @Client^.key_sym_map[K];
    if (Entry^.group_info and $0F = 0) or IsSpare(K) then Continue;
    G := KeyGroup(Entry^.group_info, Group);
    Kind := @Client^.types[Entry^.kt_index[G]];
    Base := Entry^.offset + G * Entry^.Width;
    if Client^.syms[Base] = Sym then
    begin
      Code := K;
      Exit(True);
    end;
    if (Kind^.num_levels > 1) and (Client^.syms[Base + 1] = Sym) and ShiftGivesLevel2(Kind) then
    begin
      Code := K;
      NeedShift := True;
      Exit(True);
    end;
  end;
end;

function TXKeyInput.Holds(Code: TKeyCode; Sym: TKeySym): boolean;
var
  Syms: PKeySym;
  Count: cint;
begin
  Syms := XGetKeyboardMapping(Dpy, Code, 1, @Count);
  Result := (Syms <> nil) and (Count > 0) and (Syms[0] = Sym);
  if Syms <> nil then XFree(Syms);
end;

procedure TXKeyInput.SendKey(Code: TKeyCode; IsPress: boolean);
begin
  if Code = 0 then Exit;
  XTestFakeKeyEvent(Dpy, Code, Ord(IsPress), 0);
  XSync(Dpy, False);
end;

// The keycode of the key at Place, by its XKB name.
function TXKeyInput.KeycodeAt(const Place: string): TKeyCode;
var
  Map: PXkbDescPtr;
  Code, I: integer;
  Same: boolean;
begin
  Result := 0;
  Map := Keymap;
  if (Map = nil) or (Map^.names = nil) or (Map^.names^.keys = nil) or (Length(Place) <> XkbKeyNameLength) then Exit;
  for Code := Map^.min_key_code to Map^.max_key_code do
  begin
    Same := True;
    for I := 0 to XkbKeyNameLength - 1 do
      if Map^.names^.keys[Code].Name[I] <> Place[I + 1] then Same := False;
    if Same then Exit(Code);
  end;
end;

// True when Code is one of the function keys <FK01>..<FK99>.
function TXKeyInput.IsFunctionKeyPlace(Code: integer): boolean;
var
  Map: PXkbDescPtr;
  Key: PXkbKeyNamePtr;
begin
  Result := False;
  Map := Keymap;
  if (Map = nil) or (Map^.names = nil) or (Map^.names^.keys = nil) then Exit;
  if (Code < Map^.min_key_code) or (Code > Map^.max_key_code) then Exit;
  Key := @Map^.names^.keys[Code];
  Result := (Key^.Name[0] = 'F') and (Key^.Name[1] = 'K') and (Key^.Name[2] in ['0'..'9']) and (Key^.Name[3] in ['0'..'9']);
end;

// The key the layout has for the US symbol; failing that, the key at its US
// place, so Ctrl+A is still Ctrl and the A key on a Russian layout.
function TXKeyInput.KeycodeFor(Key: word): TKeyCode;
begin
  Result := KeycodeOf(KeysymOf(Key));
  // layouts with AltGr have no right Alt; that key is the third-level shift
  if (Result = 0) and (Key = VK_RMENU) then Result := KeycodeOf(XK_ISO_Level3_Shift);
  if Result = 0 then Result := KeycodeAt(PlaceOf(Key));
end;

// A spare lent a symbol that a real key also has makes toolkits match a
// shortcut such as Ctrl+A to the spare and not the key, so it is given back.
procedure TXKeyInput.ReturnSpares(Sym: TKeySym);
var
  I: integer;
  Other: TKeySym;
  Returned: boolean;
begin
  Other := Sym;
  if (Sym >= XK_a) and (Sym <= XK_z) then Other := Sym - XK_a + XK_A;
  Returned := False;
  for I := 0 to High(Spares) do
    if (Spares[I].Sym <> NoSymbol) and ((Spares[I].Sym = Sym) or (Spares[I].Sym = Other)) then
    begin
      if (Spares[I].Pressed > 0) and (GetTickCount64 - Spares[I].Pressed < ReuseDelay) then
        Pause(ReuseDelay - (GetTickCount64 - Spares[I].Pressed));
      if Holds(Spares[I].Code, Spares[I].Sym) then
        XChangeKeyboardMapping(Dpy, Spares[I].Code, SymsPerCode, @Spares[I].Saved[0], 1);
      Spares[I].Sym := NoSymbol;
      Returned := True;
    end;
  if Returned then
  begin
    XSync(Dpy, False);
    Pause(RebindDelay);
  end;
end;

procedure TXKeyInput.DoDown(Key: word);
begin
  if Dpy = nil then Exit;
  if SparesReady then ReturnSpares(KeysymOf(Key));
  SendKey(KeycodeFor(Key), True);
end;

procedure TXKeyInput.DoUp(Key: word);
begin
  if Dpy <> nil then SendKey(KeycodeFor(Key), False);
end;

procedure TXKeyInput.PressASCIIChar(ch: char);
var
  State: TXkbStateRec;
  Code, ShiftCode: TKeyCode;
  NeedShift: boolean;
begin
  if (Dpy = nil) or (ch < ' ') or (ch = #127) then
  begin
    inherited PressASCIIChar(ch);
    Exit;
  end;
  State := Default(TXkbStateRec);
  XkbGetState(Dpy, XkbUseCoreKbd, @State);
  if not FindChar(Ord(ch), State.group, Code, NeedShift) then
  begin
    PressUnicodeChar(Ord(ch));
    Exit;
  end;
  if (State.locked_mods and LockMask <> 0) and (ch in ['a'..'z', 'A'..'Z']) then NeedShift := not NeedShift;
  ShiftCode := 0;
  if NeedShift then
  begin
    ShiftCode := KeycodeOf(XK_Shift_L);
    if ShiftCode = 0 then ShiftCode := KeycodeOf(XK_Shift_R);
    SendKey(ShiftCode, True);
  end;
  SendKey(Code, True);
  SendKey(Code, False);
  if NeedShift then SendKey(ShiftCode, False);
  Application.ProcessMessages;
end;

procedure TXKeyInput.PrepareSpares;
var
  Low, High, Code: integer;
  Map: PKeySym;
  Mods: PXModifierKeymap;

  function Unused(C: integer): boolean;
  var
    J: integer;
  begin
    for J := 0 to SymsPerCode - 1 do
      if Map[(C - Low) * SymsPerCode + J] <> NoSymbol then Exit(False);
    Result := True;
  end;

  function IsModifier(C: integer): boolean;
  var
    J: integer;
  begin
    Result := False;
    if Mods <> nil then
      for J := 0 to 8 * Mods^.max_keypermod - 1 do
        if Mods^.modifiermap[J] = C then Exit(True);
  end;

  procedure Add(C: integer);
  var
    I, Column: integer;
  begin
    I := Length(Spares);
    SetLength(Spares, I + 1);
    Spares[I].Code := C;
    SetLength(Spares[I].Saved, SymsPerCode);
    for Column := 0 to SymsPerCode - 1 do Spares[I].Saved[Column] := Map[(C - Low) * SymsPerCode + Column];
    Spares[I].Sym := NoSymbol;
    Spares[I].Pressed := 0;
  end;

begin
  SparesReady := True;
  Spares := nil;
  NextSpare := 0;
  Low := 0;
  High := 0;
  XDisplayKeycodes(Dpy, @Low, @High);
  Map := XGetKeyboardMapping(Dpy, Low, High - Low + 1, @SymsPerCode);
  if Map = nil then Exit;
  Mods := XGetModifierMapping(Dpy);
  // keycodes no key uses first; then borrow from the top of the range
  for Code := Low to High do
    if (Length(Spares) < SpareCount) and Unused(Code) then Add(Code);
  // never a function key: VK_F13 and up may need it at its place
  for Code := High downto Low do
    if (Length(Spares) < SpareCount) and not Unused(Code) and not IsModifier(Code) and not IsFunctionKeyPlace(Code) then
      Add(Code);
  if Mods <> nil then XFreeModifiermap(Mods);
  XFree(Map);
end;

procedure TXKeyInput.RestoreSpares;
var
  I: integer;
  Last: QWord;
begin
  Last := 0;
  for I := 0 to High(Spares) do
    if Spares[I].Pressed > Last then Last := Spares[I].Pressed;
  // let slow clients read the last presses with the mapping they were sent with
  while (Last > 0) and (GetTickCount64 - Last < ReuseDelay) do Sleep(10);
  for I := 0 to High(Spares) do
    if (Spares[I].Sym <> NoSymbol) and Holds(Spares[I].Code, Spares[I].Sym) then
      XChangeKeyboardMapping(Dpy, Spares[I].Code, SymsPerCode, @Spares[I].Saved[0], 1);
  XSync(Dpy, False);
  Spares := nil;
  SparesReady := False;
end;

procedure TXKeyInput.LockCaps(TurnOn: boolean);
begin
  if TurnOn then XkbLockModifiers(Dpy, XkbUseCoreKbd, LockMask, LockMask)
  else
    XkbLockModifiers(Dpy, XkbUseCoreKbd, LockMask, 0);
  XSync(Dpy, False);
end;

procedure TXKeyInput.StringDone;
begin
  if CapsSwitchedOff and (Dpy <> nil) then LockCaps(True);
  CapsSwitchedOff := False;
end;

procedure TXKeyInput.PressUnicodeChar(unicode: cardinal);
var
  Sym: TKeySym;
  I, Column: integer;
  Row: array of TKeySym;
begin
  if (Dpy = nil) or (unicode = 0) or (unicode > $10FFFF) then Exit;
  if unicode < $100 then Sym := unicode
  else
    Sym := $01000000 + unicode;
  if not SparesReady then PrepareSpares;
  if Length(Spares) = 0 then Exit;

  // Caps Lock would change a letter typed this way
  if not CapsSwitchedOff and ((Typing and CapsLockAtStart) or (not Typing and GetCapsLockState)) then
  begin
    LockCaps(False);
    CapsSwitchedOff := True;
  end;

  I := High(Spares);
  while (I >= 0) and (Spares[I].Sym <> Sym) do Dec(I);
  // a spare that lost its keysym means a new keymap was loaded over ours
  if (I >= 0) and not Holds(Spares[I].Code, Sym) then
  begin
    PrepareSpares;
    I := -1;
  end;
  if I < 0 then
  begin
    I := NextSpare;
    NextSpare := (NextSpare + 1) mod Length(Spares);
    // never rebind a keycode a client may not have read yet
    if (Spares[I].Pressed > 0) and (GetTickCount64 - Spares[I].Pressed < ReuseDelay) then
      Pause(ReuseDelay - (GetTickCount64 - Spares[I].Pressed));
    Row := nil;
    SetLength(Row, SymsPerCode);
    for Column := 0 to SymsPerCode - 1 do Row[Column] := NoSymbol;
    // both levels, so a lone letter is not read as its other case
    Row[0] := Sym;
    if SymsPerCode > 1 then Row[1] := Sym;
    XChangeKeyboardMapping(Dpy, Spares[I].Code, SymsPerCode, @Row[0], 1);
    XSync(Dpy, False);
    Spares[I].Sym := Sym;
    Pause(RebindDelay);
  end;
  SendKey(Spares[I].Code, True);
  SendKey(Spares[I].Code, False);
  Spares[I].Pressed := GetTickCount64;
  Application.ProcessMessages;
  if CapsSwitchedOff and not Typing then StringDone;
end;

// LED 1 is Caps Lock, 2 Num Lock and 3 Scroll Lock on X servers.
function TXKeyInput.Led(Bit: integer): boolean;
var
  State: TXKeyboardState;
begin
  Result := False;
  if Dpy = nil then Exit;
  State := Default(TXKeyboardState);
  XGetKeyboardControl(Dpy, @State);
  Result := State.led_mask and (culong(1) shl Bit) <> 0;
end;

function TXKeyInput.GetCapsLockState: boolean;
begin
  Result := Led(0);
end;

function TXKeyInput.GetNumLockState: boolean;
begin
  Result := Led(1);
end;

function TXKeyInput.GetScrollLockState: boolean;
begin
  Result := Led(2);
end;

procedure TXKeyInput.ToggleCapsLock;
begin
  Press(VK_CAPITAL);
end;

procedure TXKeyInput.ToggleNumLock;
begin
  Press(VK_NUMLOCK);
end;

procedure TXKeyInput.ToggleScrollLock;
begin
  Press(VK_SCROLL);
end;

end.
