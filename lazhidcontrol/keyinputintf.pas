{ KeyInputIntf

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
unit KeyInputIntf;

{$mode objfpc}{$H+}

interface

uses
  Classes,
  Forms,
  LazUTF8,
  LCLIntf, LCLType,
  SysUtils;

type
  { TKeyInput }

  TKeyInput = class
  private
    procedure UnapplyAllKeys;
  protected
    CapsLockStateAtStringStart: Boolean;
    procedure DoDown(Key: word); dynamic; abstract;
    procedure DoUp(Key: word); dynamic; abstract;
    procedure capsLockGetSaveState; dynamic; abstract;
    procedure capsLockRestoreState; dynamic; abstract;
    function CharToKeySym(ch: char): word;
    function NeedsShift(ch: char): boolean;
    function NeedsShiftWithCapsLock(ch: char): boolean;
  public
    procedure Down(Key: word);
    procedure Up(Key: word);
    procedure Press(Key: word);
    procedure PressString(StringValue: string);
    procedure Apply(Shift: TShiftState);
    procedure Unapply(Shift: TShiftState);
    procedure PressUnicodeChar(unicode: cardinal); virtual;
    procedure PressASCIIChar(ch: char); virtual;
    function GetCapsLockState: Boolean; dynamic; abstract;
  end;

implementation

  { TKeyInput }

procedure TKeyInput.Down(Key: word);
begin
  DoDown(Key);
  Application.ProcessMessages;
end;

procedure TKeyInput.Up(Key: word);
begin
  DoUp(Key);
  Application.ProcessMessages;
  sleep(8);
end;

procedure TKeyInput.Press(Key: word);
begin
  Down(Key);
  Up(Key);
end;

function TKeyInput.NeedsShift(ch: char): boolean;
begin
  Result := ch in ['A'..'Z', '~', '!', '@', '#', '$', '%', '^', '&', '*', '(', ')', '_', '+', '{', '}', '|', ':', '"', '<', '>', '?'];
end;

function TKeyInput.NeedsShiftWithCapsLock(ch: char): boolean;
var
  isUpperCase: boolean;
  normallyNeedsShift: boolean;
begin
  isUpperCase := ch in ['A'..'Z'];

  if isUpperCase or (ch in ['a'..'z']) then
  begin
    // For letters: CapsLock inverts the shift requirement
    if CapsLockStateAtStringStart then
      Result := not isUpperCase  // Shift needed for lowercase when CapsLock ON
    else
      Result := isUpperCase;     // Shift needed for uppercase when CapsLock OFF
  end
  else
  begin
    normallyNeedsShift := ch in ['~', '!', '@', '#', '$', '%', '^', '&', '*', '(', ')', '_', '+', '{', '}', '|', ':', '"', '<', '>', '?'];
    Result := normallyNeedsShift;
  end;
end;

function TKeyInput.CharToKeySym(ch: char): word;
begin
  ch := UpCase(ch);
  case ch of
    'A'..'Z': Result := VK_A + (Ord(ch) - Ord('A'));
    '0'..'9': Result := VK_0 + (Ord(ch) - Ord('0'));
    '!': Result := VK_1;
    '@': Result := VK_2;
    '#': Result := VK_3;
    '$': Result := VK_4;
    '%': Result := VK_5;
    '^': Result := VK_6;
    '&': Result := VK_7;
    '*': Result := VK_8;
    '(': Result := VK_9;
    ')': Result := VK_0;
    '~', '`': Result := VK_LCL_TILDE;
    '_', '-': Result := VK_LCL_MINUS;
    '+', '=': Result := VK_LCL_EQUAL;
    '{', '[': Result := VK_LCL_OPEN_BRACKET;
    '}', ']': Result := VK_LCL_CLOSE_BRACKET;
    '|', '\': Result := VK_LCL_BACKSLASH;
    ':', ';': Result := VK_LCL_SEMI_COMMA;
    '"', '''': Result := VK_LCL_QUOTE;
    '<', ',': Result := VK_LCL_COMMA;
    '>', '.': Result := VK_LCL_POINT;
    '?', '/': Result := VK_LCL_SLASH;
    #32: Result := VK_SPACE;
    #9: Result := VK_TAB;
    #10, #13: Result := VK_RETURN;
    else
      Result := VK_SPACE;
  end;
end;

procedure TKeyInput.PressUnicodeChar(unicode: cardinal);
begin
  // Base class fallback implementation
  // Platform-specific implementations should override this method:
  //   - TXKeyInput (Linux): Uses Ctrl+Shift+U + hex digits
  //   - TWinKeyInput (Windows): Uses KEYEVENTF_UNICODE with SendInput
  //   - TCarbonKeyInput (macOS): Uses Option key combinations
  //
  // This fallback does nothing - Unicode input requires platform-specific handling
end;

procedure TKeyInput.PressString(StringValue: string);
var
  p: PChar;
  unicode: cardinal;
  CPLen: integer;
begin
  try
    CapsLockStateAtStringStart := GetCapsLockState;
  except
    CapsLockStateAtStringStart := False;
  end;

  UnapplyAllKeys;

  p := PChar(StringValue);
  while p^ <> #0 do
  begin
    unicode := UTF8CodepointToUnicode(p, CPLen);

    // Normalize line endings: CR, LF, or CRLF all become single RETURN
    if (unicode = 13) then
    begin
      Inc(p, CPLen);
      if (p^ = #10) then
        Inc(p, 1);
      PressASCIIChar(#10);
      Sleep(5);
      Continue;
    end;

    if (CPLen = 1) and (unicode < 128) then
    begin
      PressASCIIChar(char(unicode));
      Sleep(5);
    end else if unicode > 0 then
    begin
      PressUnicodeChar(unicode);
    end;

    Inc(p, CPLen);
  end;
end;

procedure TKeyInput.UnapplyAllKeys;
var
  keyCode: word;
begin
  Up(VK_SHIFT);
  Up(VK_CONTROL);
  Up(VK_MENU);

  for keyCode := VK_0 to VK_9 do
    Up(keyCode);

  for keyCode := VK_A to VK_Z do
    Up(keyCode);

  for keyCode := VK_F1 to VK_F12 do
    Up(keyCode);
end;

procedure TKeyInput.Apply(Shift: TShiftState);
begin
  if ssShift in Shift then Down(VK_SHIFT);
  if ssCtrl in Shift then Down(VK_CONTROL);
  if ssAlt in Shift then Down(VK_MENU);
end;

procedure TKeyInput.Unapply(Shift: TShiftState);
begin
  if ssShift in Shift then Up(VK_SHIFT);
  if ssCtrl in Shift then Up(VK_CONTROL);
  if ssAlt in Shift then Up(VK_MENU);
end;

procedure TKeyInput.PressASCIIChar(ch: char);
var
  keySym: word;
begin
  keySym := CharToKeySym(ch);
  if NeedsShift(ch) then Apply([ssShift]);
  Press(keySym);
  if NeedsShift(ch) then Unapply([ssShift]);
end;

end.
