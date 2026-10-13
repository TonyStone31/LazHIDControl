{ XHotkeyInput

  Copyright (C) 2025 LazHIDControl Contributors

  X11 implementation of global hotkey registration using XGrabKey.

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
unit XHotkeyInput;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LCLType, X, XLib, KeySym, HotkeyInputIntf,
  {$ifdef LCLgtk3}
  LazGdk3;
  {$else}
  Gdk2, Gdk2x;
  {$endif}

type
  { TXHotkey }
  TXHotkey = class(THotkey)
  private
    Display: PDisplay;
    RootWindow: Pointer;
    procedure GrabBoth(Grab: boolean);
  protected
    procedure DoRegister; override;
    procedure DoUnregister; override;
  public
    constructor Create(AKey: word; AShift: TShiftState; AOnHotkey: TKeyNotifyEvent); override;
  end;

function InitializeHotkey(AKey: word; AShift: TShiftState; AOnHotkey: TKeyNotifyEvent): THotkey;

implementation

{$ifdef LCLgtk3}
{ The LCL gtk3 bindings stop short of the gdk_x11_* helpers, so bind the two
  this unit needs straight from libgdk-3 }
function gdk_x11_window_get_xid(window: PGdkWindow): TWindow; cdecl;
  external LazGdk3_library name 'gdk_x11_window_get_xid';
function gdk_x11_get_default_xdisplay: PDisplay; cdecl;
  external LazGdk3_library name 'gdk_x11_get_default_xdisplay';
{$endif}

function DefaultXDisplay: PDisplay;
begin
  {$ifdef LCLgtk3}
  Result := gdk_x11_get_default_xdisplay;
  {$else}
  Result := GDK_DISPLAY_XDISPLAY(gdk_display_get_default);
  {$endif}
end;

function RootXid(Root: Pointer): TWindow;
begin
  {$ifdef LCLgtk3}
  Result := gdk_x11_window_get_xid(Root);
  {$else}
  Result := gdk_x11_drawable_get_xid(Root);
  {$endif}
end;

const
  AltMask = Mod1Mask;
  CapLock = LockMask;
  NumLock = Mod2Mask;
  NotLock = integer(not (CapLock or NumLock));

var
  GlobalHotkeys: TList = nil;
  FilterInstalled: boolean = False;
  GrabFailed: boolean = False;

function GrabError(Display: PDisplay; Event: PXErrorEvent): integer; cdecl;
begin
  GrabFailed := True;
  Result := 0;
end;

function ShiftToMod(ShiftState: TShiftState): integer;
begin
  Result := 0;
  if ssShift in ShiftState then
    Result := Result or ShiftMask;
  if ssAlt in ShiftState then
    Result := Result or AltMask;
  if ssCtrl in ShiftState then
    Result := Result or ControlMask;
end;

function ModToShift(Modifiers: integer): TShiftState;
begin
  Result := [];
  if (ShiftMask and Modifiers) > 0 then
    Include(Result, ssShift);
  if (AltMask and Modifiers) > 0 then
    Include(Result, ssAlt);
  if (ControlMask and Modifiers) > 0 then
    Include(Result, ssCtrl);
end;

function KeyToSym(Key: word): TKeySym;
begin
  case Key of
    VK_F1..VK_F12: Result := XK_F1 + (Key - VK_F1);
    VK_0..VK_9: Result := XK_0 + (Key - VK_0);
    VK_A..VK_Z: Result := XK_A + (Key - VK_A);
    VK_SPACE: Result := XK_SPACE;
    VK_RETURN: Result := XK_RETURN;
    VK_ESCAPE: Result := XK_ESCAPE;
    VK_TAB: Result := XK_TAB;
    else
      Result := 0;
  end;
end;

function SymToKey(Sym: TKeySym): word;
begin
  if (Sym >= XK_F1) and (Sym <= XK_F12) then
    Result := VK_F1 + (Sym - XK_F1)
  else if (Sym >= XK_0) and (Sym <= XK_9) then
    Result := VK_0 + (Sym - XK_0)
  else if (Sym >= XK_A) and (Sym <= XK_Z) then
    Result := VK_A + (Sym - XK_A)
  else if (Sym >= XK_a) and (Sym <= XK_z) then
    Result := VK_A + (Sym - XK_a)
  else
    case Sym of
      XK_SPACE: Result := VK_SPACE;
      XK_RETURN: Result := VK_RETURN;
      XK_ESCAPE: Result := VK_ESCAPE;
      XK_TAB: Result := VK_TAB;
      else
        Result := 0;
    end;
end;

function FilterKeys(XEvent: PGdkXEvent; Event: PGdkEvent; Data: Pointer): TGdkFilterReturn; cdecl;
var
  AnyEvent: PXAnyEvent absolute XEvent;
  KeyEvent: PXKeyEvent absolute XEvent;
  Sym: TKeySym;
  Key: word;
  ShiftState: TShiftState;
  I: integer;
  Hotkey: TXHotkey;
  Display: PDisplay;
begin
  if AnyEvent^._type <> KeyPress then
    Exit(GDK_FILTER_CONTINUE);

  Display := DefaultXDisplay;
  Sym := XKeycodeToKeysym(Display, KeyEvent^.keycode, 0);
  Key := SymToKey(Sym);
  ShiftState := ModToShift(KeyEvent^.state);

  for I := 0 to GlobalHotkeys.Count - 1 do
  begin
    Hotkey := TXHotkey(GlobalHotkeys[I]);
    if (Hotkey.Key = Key) and (Hotkey.Shift = ShiftState) then
    begin
      if Assigned(Hotkey.OnHotkeyEvent) then
        Hotkey.OnHotkeyEvent(Hotkey, Key, ShiftState);
      Exit(GDK_FILTER_REMOVE);
    end;
  end;

  Result := GDK_FILTER_CONTINUE;
end;

function InitializeHotkey(AKey: word; AShift: TShiftState; AOnHotkey: TKeyNotifyEvent): THotkey;
begin
  Result := TXHotkey.Create(AKey, AShift, AOnHotkey);
end;

{ TXHotkey }

constructor TXHotkey.Create(AKey: word; AShift: TShiftState; AOnHotkey: TKeyNotifyEvent);
begin
  inherited Create(AKey, AShift, AOnHotkey);

  RootWindow := gdk_get_default_root_window;
  Display := DefaultXDisplay;

  if GlobalHotkeys = nil then
    GlobalHotkeys := TList.Create;
end;

{ X grabs a key for one exact set of modifiers, and Caps Lock and Num Lock
  count as modifiers, so a hotkey is grabbed once for each combination of them. }
procedure GrabKey(Display: PDisplay; KeyCode, Modifier: longword; Window: TWindow; Grab: boolean);
const
  Locks: array[0..3] of longword = (0, CapLock, NumLock, CapLock or NumLock);
var
  I: integer;
begin
  for I := Low(Locks) to High(Locks) do
    if Grab then XGrabKey(Display, KeyCode, (Modifier and NotLock) or Locks[I], Window, 1, GrabModeAsync, GrabModeAsync)
    else
      XUngrabKey(Display, KeyCode, (Modifier and NotLock) or Locks[I], Window);
end;

{ Grabs or releases the key, and the key its shifted symbol sits on when
  that is a different one. }
procedure TXHotkey.GrabBoth(Grab: boolean);
var
  KeySym, ShiftedSym: TKeySym;
  Code: longword;
begin
  KeySym := KeyToSym(Key);
  Code := XKeysymToKeycode(Display, KeySym);
  GrabKey(Display, Code, ShiftToMod(Shift), RootXid(RootWindow), Grab);
  ShiftedSym := XKeycodeToKeysym(Display, Code, 1);
  if ShiftedSym <> KeySym then
    GrabKey(Display, XKeysymToKeycode(Display, ShiftedSym), ShiftToMod(Shift), RootXid(RootWindow), Grab);
end;

procedure TXHotkey.DoRegister;
var
  I: integer;
  PreviousHandler: TXErrorHandler;
begin
  for I := 0 to GlobalHotkeys.Count - 1 do
    if (TXHotkey(GlobalHotkeys[I]).Key = Key) and (TXHotkey(GlobalHotkeys[I]).Shift = Shift) then Exit;
  if not Assigned(Display) or not Assigned(RootWindow) then Exit;
  if (KeyToSym(Key) = 0) or (XKeysymToKeycode(Display, KeyToSym(Key)) = 0) then Exit;

  // a grab somebody else already holds comes back as an X error, not a result
  XSync(Display, False);
  GrabFailed := False;
  PreviousHandler := XSetErrorHandler(@GrabError);
  GrabBoth(True);
  XSync(Display, False);
  XSetErrorHandler(PreviousHandler);
  if GrabFailed then
  begin
    GrabBoth(False);
    Exit;
  end;

  GlobalHotkeys.Add(Self);
  if not FilterInstalled then
  begin
    gdk_window_add_filter(RootWindow, @FilterKeys, nil);
    FilterInstalled := True;
  end;
  Registered := True;
end;

procedure TXHotkey.DoUnregister;
begin
  if not Registered then Exit;
  GlobalHotkeys.Remove(Self);
  GrabBoth(False);
  if (GlobalHotkeys.Count = 0) and FilterInstalled then
  begin
    gdk_window_remove_filter(RootWindow, @FilterKeys, nil);
    FilterInstalled := False;
  end;
  Registered := False;
end;

end.
