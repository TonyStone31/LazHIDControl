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
  Classes, SysUtils, Controls, Forms,
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

  // Scan-code only — don't use VK for compatibility with Notepad
  // Notepad requires proper WM_CHAR events from TranslateMessage
  Result.ki.wVk := 0;
  Result.ki.wScan := Scan;
  Result.ki.dwFlags := KEYEVENTF_SCANCODE;

  // Add extended bit if needed (numpad, arrows, etc.)
  if (Scan and $E000) <> 0 then
    Result.ki.dwFlags := Result.ki.dwFlags or KEYEVENTF_EXTENDEDKEY;

  // Handle key release
  if KeyUp then
    Result.ki.dwFlags := Result.ki.dwFlags or KEYEVENTF_KEYUP;
end;


{ TWinKeyInput }

procedure TWinKeyInput.DoDown(Key: Word);
var
  Sc: Word;
  Inp: TInput;
begin
  Sc := MapVirtualKey(Key, 0);  // MAPVK_VK_TO_VSC = 0
  Inp := MakeScanCodeInput(Sc, False);
  SendInput(1, @Inp, SizeOf(Inp));
  Sleep(1);
end;

procedure TWinKeyInput.DoUp(Key: Word);
var
  Sc: Word;
  Inp: TInput;
begin
  Sc := MapVirtualKey(Key, 0);  // MAPVK_VK_TO_VSC = 0
  Inp := MakeScanCodeInput(Sc, True);
  SendInput(1, @Inp, SizeOf(Inp));
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
  // Initialize input array
  FillChar(Inputs, SizeOf(Inputs), 0);

  // Check if this is a character that needs surrogate pairs (emoji, etc.)
  if unicode > $FFFF then
  begin
    // Calculate surrogate pair for characters outside BMP (Basic Multilingual Plane)
    // Formula: High = ((codepoint - 0x10000) shr 10) + 0xD800
    //          Low  = ((codepoint - 0x10000) and 0x3FF) + 0xDC00
    HighSurrogate := ((unicode - $10000) shr 10) + $D800;
    LowSurrogate := ((unicode - $10000) and $3FF) + $DC00;

    // High surrogate key down
    Inputs[0].type_ := INPUT_KEYBOARD;
    Inputs[0].ki.dwFlags := KEYEVENTF_UNICODE;
    Inputs[0].ki.wScan := HighSurrogate;
    Inputs[0].ki.wVk := 0;

    // High surrogate key up
    Inputs[1].type_ := INPUT_KEYBOARD;
    Inputs[1].ki.dwFlags := KEYEVENTF_UNICODE or KEYEVENTF_KEYUP;
    Inputs[1].ki.wScan := HighSurrogate;
    Inputs[1].ki.wVk := 0;

    // Low surrogate key down
    Inputs[2].type_ := INPUT_KEYBOARD;
    Inputs[2].ki.dwFlags := KEYEVENTF_UNICODE;
    Inputs[2].ki.wScan := LowSurrogate;
    Inputs[2].ki.wVk := 0;

    // Low surrogate key up
    Inputs[3].type_ := INPUT_KEYBOARD;
    Inputs[3].ki.dwFlags := KEYEVENTF_UNICODE or KEYEVENTF_KEYUP;
    Inputs[3].ki.wScan := LowSurrogate;
    Inputs[3].ki.wVk := 0;

    InputCount := 4;
  end
  else
  begin
    // BMP character - send directly
    Inputs[0].type_ := INPUT_KEYBOARD;
    Inputs[0].ki.dwFlags := KEYEVENTF_UNICODE;
    Inputs[0].ki.wScan := unicode;
    Inputs[0].ki.wVk := 0;

    // Key up event
    Inputs[1].type_ := INPUT_KEYBOARD;
    Inputs[1].ki.dwFlags := KEYEVENTF_UNICODE or KEYEVENTF_KEYUP;
    Inputs[1].ki.wScan := unicode;
    Inputs[1].ki.wVk := 0;

    InputCount := 2;
  end;

  // Send all inputs atomically to avoid timing issues
  SendInput(InputCount, @Inputs[0], SizeOf(TInput));
  // Delay to ensure the Unicode character is processed
  // Some applications need more time to process complex Unicode (emoji, etc.)
  Sleep(35);
end;

procedure TWinKeyInput.PressASCIIChar(ch: char);
var
  VkResult: SHORT;
  VK: Byte;
  ShiftState: Byte;
  Inputs: array[0..7] of TInput;
  InputCount: Integer;
  Sc: Word;
begin
  // Use VkKeyScan to determine the correct VK and shift state for this character
  VkResult := VkKeyScan(ch);

  if VkResult = -1 then
  begin
    // Character not available on current keyboard layout - fall back to Unicode
    PressUnicodeChar(Ord(ch));
    Exit;
  end;

  VK := LoByte(VkResult);
  ShiftState := HiByte(VkResult);
  InputCount := 0;

  // Build input sequence: modifiers down, key down, key up, modifiers up
  // Bit 0: Shift, Bit 1: Ctrl, Bit 2: Alt

  // Modifiers DOWN
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

  // Main key DOWN
  Sc := MapVirtualKey(VK, 0);
  Inputs[InputCount] := MakeScanCodeInput(Sc, False);
  Inc(InputCount);

  // Main key UP
  Inputs[InputCount] := MakeScanCodeInput(Sc, True);
  Inc(InputCount);

  // Modifiers UP (in reverse order)
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

  // Send all inputs atomically
  SendInput(InputCount, @Inputs[0], SizeOf(TInput));
  Sleep(1);
end;

end.

