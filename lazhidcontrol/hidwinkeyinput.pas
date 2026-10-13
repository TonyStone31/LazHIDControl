{ HIDWinKeyInput

  Copyright (C) 2008 Tom Gregorovic

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
unit HIDWinKeyInput;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms,
  Windows, JwaWinUser,
  HIDKeyInputIntf;

type

  { TWinKeyInput }

  TWinKeyInput = class(TKeyInput)
  private
    function GetKeyStateVK(Key: byte): boolean;
  protected
    procedure DoDown(Key: word); override;
    procedure DoUp(Key: word); override;
  public
    procedure PressUnicodeChar(unicode: cardinal); override;
    procedure PressASCIIChar(ch: char); override;
    function GetCapsLockState: boolean; override;
    function GetNumLockState: boolean; override;
    function GetScrollLockState: boolean; override;
    procedure ToggleCapsLock; override;
    procedure ToggleNumLock; override;
    procedure ToggleScrollLock; override;
  end;

function InitializeKeyInput: TKeyInput;

implementation

function InitializeKeyInput: TKeyInput;
begin
  Result := TWinKeyInput.Create;
end;

{ Keys that share a scan code with the number pad, or with a twin on the
  left.  Without the extended flag an arrow is a digit while NumLock is on. }
function IsExtendedKey(Key: word): boolean;
begin
  case Key of
    VK_PRIOR, VK_NEXT, VK_END, VK_HOME, VK_LEFT, VK_UP, VK_RIGHT, VK_DOWN, VK_INSERT, VK_DELETE,
    VK_DIVIDE, VK_NUMLOCK, VK_RCONTROL, VK_RMENU, VK_LWIN, VK_RWIN, VK_APPS: Result := True;
    else
      Result := False;
  end;
end;

// Scan codes rather than virtual keys: applications get proper WM_CHAR from them.
function MakeScanCodeInput(Scan: word; KeyUp: boolean; extended: boolean = False): TInput;
begin
  Result := Default(TInput);
  Result.type_ := INPUT_KEYBOARD;
  Result.ki.wScan := Scan;
  Result.ki.dwFlags := KEYEVENTF_SCANCODE;
  if extended then
    Result.ki.dwFlags := Result.ki.dwFlags or KEYEVENTF_EXTENDEDKEY;
  if KeyUp then
    Result.ki.dwFlags := Result.ki.dwFlags or KEYEVENTF_KEYUP;
end;

{ TWinKeyInput }

procedure TWinKeyInput.DoDown(Key: word);
var
  Inp: TInput;
begin
  Inp := MakeScanCodeInput(MapVirtualKey(Key, 0), False, IsExtendedKey(Key));
  SendInput(1, @Inp, SizeOf(Inp));
  Application.ProcessMessages;
  Sleep(1);
end;

procedure TWinKeyInput.DoUp(Key: word);
var
  Inp: TInput;
begin
  Inp := MakeScanCodeInput(MapVirtualKey(Key, 0), True, IsExtendedKey(Key));
  SendInput(1, @Inp, SizeOf(Inp));
  Application.ProcessMessages;
  Sleep(1);
end;

function TWinKeyInput.GetKeyStateVK(Key: byte): boolean;
begin
  Result := (GetKeyState(Key) and $01) <> 0;
end;

procedure TWinKeyInput.PressUnicodeChar(unicode: cardinal);
var
  Inputs: array[0..3] of TInput;
  Count: integer;

// one UTF-16 unit, down then up
  procedure Add(Code: word);
  begin
    Inputs[Count].type_ := INPUT_KEYBOARD;
    Inputs[Count].ki.dwFlags := KEYEVENTF_UNICODE;
    Inputs[Count].ki.wScan := Code;
    Inputs[Count + 1] := Inputs[Count];
    Inputs[Count + 1].ki.dwFlags := KEYEVENTF_UNICODE or KEYEVENTF_KEYUP;
    Inc(Count, 2);
  end;

begin
  FillChar(Inputs, SizeOf(Inputs), 0);
  Count := 0;
  // past the 16-bit range (emoji and the like) a character is a surrogate pair
  if unicode > $FFFF then
  begin
    Add(((unicode - $10000) shr 10) + $D800);
    Add(((unicode - $10000) and $3FF) + $DC00);
  end
  else
    Add(unicode);
  SendInput(Count, @Inputs[0], SizeOf(TInput));
  Application.ProcessMessages;
  Sleep(35);
end;

{ A character the keyboard layout has a key for goes out as that key, with
  whatever modifiers the layout says it takes; anything else as Unicode. }
procedure TWinKeyInput.PressASCIIChar(ch: char);
const
  // VkKeyScan's high byte: bit 0 Shift, bit 1 Ctrl, bit 2 Alt
  Modifiers: array[0..2] of word = (VK_SHIFT, VK_CONTROL, VK_MENU);
var
  VkResult: SHORT;
  VK, ShiftState: byte;
  Inputs: array[0..7] of TInput;
  Count, I: integer;

  procedure Add(Key: word; Up: boolean);
  begin
    Inputs[Count] := MakeScanCodeInput(MapVirtualKey(Key, 0), Up);
    Inc(Count);
  end;

begin
  VkResult := VkKeyScan(ch);
  if VkResult = -1 then
  begin
    PressUnicodeChar(Ord(ch));
    Exit;
  end;
  VK := LoByte(VkResult);
  ShiftState := HiByte(VkResult);
  // with Caps Lock on, a letter needs Shift exactly when it would not otherwise
  if ch in ['A'..'Z', 'a'..'z'] then
    if NeedsShiftWithCapsLock(ch) then ShiftState := ShiftState or 1
    else
      ShiftState := ShiftState and not 1;

  Count := 0;
  for I := 0 to 2 do
    if ShiftState and (1 shl I) <> 0 then Add(Modifiers[I], False);
  Add(VK, False);
  Add(VK, True);
  for I := 2 downto 0 do
    if ShiftState and (1 shl I) <> 0 then Add(Modifiers[I], True);
  SendInput(Count, @Inputs[0], SizeOf(TInput));
  Application.ProcessMessages;
  Sleep(1);
end;

function TWinKeyInput.GetCapsLockState: boolean;
begin
  Result := GetKeyStateVK(VK_CAPITAL);
end;

function TWinKeyInput.GetNumLockState: boolean;
begin
  Result := GetKeyStateVK(VK_NUMLOCK);
end;

function TWinKeyInput.GetScrollLockState: boolean;
begin
  Result := GetKeyStateVK(VK_SCROLL);
end;

procedure TWinKeyInput.ToggleCapsLock;
begin
  DoDown(VK_CAPITAL);
  DoUp(VK_CAPITAL);
end;

procedure TWinKeyInput.ToggleNumLock;
begin
  DoDown(VK_NUMLOCK);
  DoUp(VK_NUMLOCK);
end;

procedure TWinKeyInput.ToggleScrollLock;
begin
  DoDown(VK_SCROLL);
  DoUp(VK_SCROLL);
end;

end.
