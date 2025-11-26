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
  Classes, SysUtils, LCLType, X, XLib, Gdk2, Gdk2x, KeySym, HotkeyInputIntf;

type
  { TXHotkey }
  TXHotkey = class(THotkey)
  private
    Display: PDisplay;
    RootWindow: Pointer;
  protected
    procedure DoRegister; override;
    procedure DoUnregister; override;
  public
    constructor Create(AKey: Word; AShift: TShiftState; AOnHotkey: TKeyNotifyEvent); override;
  end;

function InitializeHotkey(AKey: Word; AShift: TShiftState; AOnHotkey: TKeyNotifyEvent): THotkey;

implementation

const
  AltMask = Mod1Mask;
  CapLock = LockMask;
  NumLock = Mod2Mask;
  NotLock = Integer(not (CapLock or NumLock));

var
  GlobalHotkeys: TList = nil;
  FilterInstalled: Boolean = False;

function ShiftToMod(ShiftState: TShiftState): Integer;
begin
  Result := 0;
  if ssShift in ShiftState then
    Result := Result or ShiftMask;
  if ssAlt in ShiftState then
    Result := Result or AltMask;
  if ssCtrl in ShiftState then
    Result := Result or ControlMask;
end;

function ModToShift(Modifiers: Integer): TShiftState;
begin
  Result := [];
  if (ShiftMask and Modifiers) > 0 then
    Include(Result, ssShift);
  if (AltMask and Modifiers) > 0 then
    Include(Result, ssAlt);
  if (ControlMask and Modifiers) > 0 then
    Include(Result, ssCtrl);
end;

function KeyToSym(Key: Word): TKeySym;
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

function SymToKey(Sym: TKeySym): Word;
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
  Key: Word;
  ShiftState: TShiftState;
  I: Integer;
  Hotkey: TXHotkey;
  Display: PDisplay;
begin
  if AnyEvent^._type <> KeyPress then
    Exit(GDK_FILTER_CONTINUE);

  Display := GDK_DISPLAY_XDISPLAY(gdk_display_get_default);
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

function InitializeHotkey(AKey: Word; AShift: TShiftState; AOnHotkey: TKeyNotifyEvent): THotkey;
begin
  Result := TXHotkey.Create(AKey, AShift, AOnHotkey);
end;

{ TXHotkey }

constructor TXHotkey.Create(AKey: Word; AShift: TShiftState; AOnHotkey: TKeyNotifyEvent);
begin
  inherited Create(AKey, AShift, AOnHotkey);

  RootWindow := gdk_get_default_root_window;
  Display := GDK_DISPLAY_XDISPLAY(gdk_display_get_default);

  if GlobalHotkeys = nil then
    GlobalHotkeys := TList.Create;
end;

procedure TXHotkey.DoRegister;
var
  Modifier: LongWord;
  KeySym, ShiftedSym: TKeySym;
  XKeyCode: LongWord;
  Window: TWindow;

  procedure CaptureKey(Disp: PDisplay; KC: LongWord; Modif: LongWord; Win: TWindow);
  begin
    { Capture keys without cap or num lock }
    XGrabKey(Disp, KC, Modif and NotLock, Win, 1, GrabModeAsync, GrabModeAsync);
    { Capture keys with cap lock }
    XGrabKey(Disp, KC, Modif or CapLock, Win, 1, GrabModeAsync, GrabModeAsync);
    { Capture keys with num lock }
    XGrabKey(Disp, KC, Modif or NumLock, Win, 1, GrabModeAsync, GrabModeAsync);
    { Capture keys with cap or num lock }
    XGrabKey(Disp, KC, Modif or CapLock or NumLock, Win, 1, GrabModeAsync, GrabModeAsync);
  end;

begin
  Modifier := ShiftToMod(Shift);
  KeySym := KeyToSym(Key);
  if KeySym = 0 then Exit;

  XKeyCode := XKeysymToKeycode(Display, KeySym);
  Window := gdk_x11_drawable_get_xid(RootWindow);
  CaptureKey(Display, XKeyCode, Modifier, Window);

  { Also capture the shifted variant if it's different (e.g., lowercase vs uppercase) }
  ShiftedSym := XKeycodeToKeysym(Display, XKeyCode, 1);
  if KeySym <> ShiftedSym then
  begin
    XKeyCode := XKeysymToKeycode(Display, ShiftedSym);
    CaptureKey(Display, XKeyCode, Modifier, Window);
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
var
  Modifier: LongWord;
  KeySym, ShiftedSym: TKeySym;
  XKeyCode: LongWord;
  Window: TWindow;

  procedure ReleaseKey(Disp: PDisplay; KC: LongWord; Modif: LongWord; Win: TWindow);
  begin
    { Release keys without cap or num lock }
    XUngrabKey(Disp, KC, Modif and NotLock, Win);
    { Release keys with cap lock }
    XUngrabKey(Disp, KC, Modif or CapLock, Win);
    { Release keys with num lock }
    XUngrabKey(Disp, KC, Modif or NumLock, Win);
    { Release keys with cap or num lock }
    XUngrabKey(Disp, KC, Modif or CapLock or NumLock, Win);
  end;

begin
  if not Registered then Exit;

  GlobalHotkeys.Remove(Self);

  Modifier := ShiftToMod(Shift);
  KeySym := KeyToSym(Key);
  if KeySym = 0 then Exit;

  XKeyCode := XKeysymToKeycode(Display, KeySym);
  Window := gdk_x11_drawable_get_xid(RootWindow);
  ReleaseKey(Display, XKeyCode, Modifier, Window);

  { Also release the shifted variant if it's different (e.g., lowercase vs uppercase) }
  ShiftedSym := XKeycodeToKeysym(Display, XKeyCode, 1);
  if KeySym <> ShiftedSym then
  begin
    XKeyCode := XKeysymToKeycode(Display, ShiftedSym);
    ReleaseKey(Display, XKeyCode, Modifier, Window);
  end;

  if (GlobalHotkeys.Count = 0) and FilterInstalled then
  begin
    gdk_window_remove_filter(RootWindow, @FilterKeys, nil);
    FilterInstalled := False;
  end;

  Registered := False;
end;

end.
