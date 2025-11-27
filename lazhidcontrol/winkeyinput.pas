{ WinKeyInput

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
unit WinKeyInput;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms,
  Windows, JwaWinUser,
  KeyInputIntf;
  
type

  { TWinKeyInput }

  TWinKeyInput = class(TKeyInput)
  private
    capsLockBeginState: Boolean;
    function GetKeyStateVK(Key: Byte): Boolean;
  protected
    procedure DoDown(Key: Word); override;
    procedure DoUp(Key: Word); override;
    procedure capsLockGetSaveState; override;
    procedure capsLockRestoreState; override;
  public
    procedure PressUnicodeChar(unicode: cardinal); override;
    procedure PressASCIIChar(ch: char); override;
    function GetCapsLockState: Boolean; override;
  end;
  
function InitializeKeyInput: TKeyInput;

implementation

function InitializeKeyInput: TKeyInput;
begin
  Result := TWinKeyInput.Create;
end;

function MakeScanCodeInput(Scan: Word; KeyUp: Boolean): TInput;
begin
  FillChar(Result, SizeOf(Result), 0);
  Result.type_ := INPUT_KEYBOARD;
  Result.ki.wVk := 0;  // Using scan code only for better WM_CHAR generation
  Result.ki.wScan := Scan;
  Result.ki.dwFlags := KEYEVENTF_SCANCODE;

  if (Scan and $E000) <> 0 then
    Result.ki.dwFlags := Result.ki.dwFlags or KEYEVENTF_EXTENDEDKEY;

  if KeyUp then
    Result.ki.dwFlags := Result.ki.dwFlags or KEYEVENTF_KEYUP;
end;


{ TWinKeyInput }

procedure TWinKeyInput.DoDown(Key: Word);
var
  Sc: Word;
  Inp: TInput;
begin
  Sc := MapVirtualKey(Key, 0);
  Inp := MakeScanCodeInput(Sc, False);
  SendInput(1, @Inp, SizeOf(Inp));
  Application.ProcessMessages;
  Sleep(1);
end;

procedure TWinKeyInput.DoUp(Key: Word);
var
  Sc: Word;
  Inp: TInput;
begin
  Sc := MapVirtualKey(Key, 0);
  Inp := MakeScanCodeInput(Sc, True);
  SendInput(1, @Inp, SizeOf(Inp));
  Application.ProcessMessages;
  Sleep(1);
end;

procedure TWinKeyInput.capsLockGetSaveState;
begin
  capsLockBeginState := GetKeyStateVK(VK_CAPITAL);

  if capsLockBeginState then
  begin
    DoDown(VK_CAPITAL);
    DoUp(VK_CAPITAL);
  end;
end;

function TWinKeyInput.GetKeyStateVK(Key: Byte): Boolean;
begin
  Result := (GetKeyState(Key) and $01) <> 0;
end;

procedure TWinKeyInput.capsLockRestoreState;
begin
  if capsLockBeginState and not GetKeyStateVK(VK_CAPITAL) then
  begin
    DoDown(VK_CAPITAL);
    DoUp(VK_CAPITAL);
  end;
end;

procedure TWinKeyInput.PressUnicodeChar(unicode: cardinal);
var
  Inputs: array[0..3] of TInput;
  InputCount: Integer;
  HighSurrogate, LowSurrogate: Word;
begin
  FillChar(Inputs, SizeOf(Inputs), 0);

  // Characters outside BMP (emoji, etc.) need UTF-16 surrogate pairs
  if unicode > $FFFF then
  begin
    HighSurrogate := ((unicode - $10000) shr 10) + $D800;
    LowSurrogate := ((unicode - $10000) and $3FF) + $DC00;

    Inputs[0].type_ := INPUT_KEYBOARD;
    Inputs[0].ki.dwFlags := KEYEVENTF_UNICODE;
    Inputs[0].ki.wScan := HighSurrogate;
    Inputs[0].ki.wVk := 0;

    Inputs[1].type_ := INPUT_KEYBOARD;
    Inputs[1].ki.dwFlags := KEYEVENTF_UNICODE or KEYEVENTF_KEYUP;
    Inputs[1].ki.wScan := HighSurrogate;
    Inputs[1].ki.wVk := 0;

    Inputs[2].type_ := INPUT_KEYBOARD;
    Inputs[2].ki.dwFlags := KEYEVENTF_UNICODE;
    Inputs[2].ki.wScan := LowSurrogate;
    Inputs[2].ki.wVk := 0;

    Inputs[3].type_ := INPUT_KEYBOARD;
    Inputs[3].ki.dwFlags := KEYEVENTF_UNICODE or KEYEVENTF_KEYUP;
    Inputs[3].ki.wScan := LowSurrogate;
    Inputs[3].ki.wVk := 0;

    InputCount := 4;
  end
  else
  begin
    Inputs[0].type_ := INPUT_KEYBOARD;
    Inputs[0].ki.dwFlags := KEYEVENTF_UNICODE;
    Inputs[0].ki.wScan := unicode;
    Inputs[0].ki.wVk := 0;

    Inputs[1].type_ := INPUT_KEYBOARD;
    Inputs[1].ki.dwFlags := KEYEVENTF_UNICODE or KEYEVENTF_KEYUP;
    Inputs[1].ki.wScan := unicode;
    Inputs[1].ki.wVk := 0;

    InputCount := 2;
  end;

  SendInput(InputCount, @Inputs[0], SizeOf(TInput));
  Application.ProcessMessages;
  Sleep(35);  // Keep original timing for Unicode
end;

procedure TWinKeyInput.PressASCIIChar(ch: char);
var
  VkResult: SHORT;
  VK: Byte;
  ShiftState: Byte;
  Inputs: array[0..7] of TInput;
  InputCount: Integer;
  Sc: Word;
  requiresShift: Boolean;
begin
  VkResult := VkKeyScan(ch);

  if VkResult = -1 then
  begin
    PressUnicodeChar(Ord(ch));
    Exit;
  end;

  VK := LoByte(VkResult);
  ShiftState := HiByte(VkResult);

  // Override VkKeyScan shift state for letters when CapsLock is active
  if (ch in ['A'..'Z', 'a'..'z']) then
  begin
    requiresShift := NeedsShiftWithCapsLock(ch);
    if requiresShift then
      ShiftState := ShiftState or 1
    else
      ShiftState := ShiftState and (not 1);
  end;

  InputCount := 0;

  // Build input sequence: modifiers down, key down/up, modifiers up (reverse order)

  if (ShiftState and 1) <> 0 then
  begin
    Inputs[InputCount] := MakeScanCodeInput(MapVirtualKey(VK_SHIFT, 0), False);
    Inc(InputCount);
  end;
  if (ShiftState and 2) <> 0 then
  begin
    Inputs[InputCount] := MakeScanCodeInput(MapVirtualKey(VK_CONTROL, 0), False);
    Inc(InputCount);
  end;
  if (ShiftState and 4) <> 0 then
  begin
    Inputs[InputCount] := MakeScanCodeInput(MapVirtualKey(VK_MENU, 0), False);
    Inc(InputCount);
  end;

  Sc := MapVirtualKey(VK, 0);
  Inputs[InputCount] := MakeScanCodeInput(Sc, False);
  Inc(InputCount);

  Inputs[InputCount] := MakeScanCodeInput(Sc, True);
  Inc(InputCount);

  if (ShiftState and 4) <> 0 then
  begin
    Inputs[InputCount] := MakeScanCodeInput(MapVirtualKey(VK_MENU, 0), True);
    Inc(InputCount);
  end;
  if (ShiftState and 2) <> 0 then
  begin
    Inputs[InputCount] := MakeScanCodeInput(MapVirtualKey(VK_CONTROL, 0), True);
    Inc(InputCount);
  end;
  if (ShiftState and 1) <> 0 then
  begin
    Inputs[InputCount] := MakeScanCodeInput(MapVirtualKey(VK_SHIFT, 0), True);
    Inc(InputCount);
  end;

  SendInput(InputCount, @Inputs[0], SizeOf(TInput));
  Application.ProcessMessages;
  Sleep(1);
end;

function TWinKeyInput.GetCapsLockState: Boolean;
begin
  Result := GetKeyStateVK(VK_CAPITAL);
end;

end.

