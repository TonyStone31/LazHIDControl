unit HIDWinKeyInput;

{$mode objfpc}{$H+}
// Copyright (c) 2026 Tony Stone. 0BSD.

{ The keyboard on Windows, through SendInput.  Keys go as scan codes so
  applications get their usual character messages. }

interface

uses
  HIDKeyInputIntf;

function InitializeWinKeyInput: TKeyInput;

implementation

uses
  Classes, SysUtils, Forms, Windows, LCLType;

// Declared here so the unit does not depend on which FPC declares it.
function ToUnicodeEx(VirtKey, ScanCode: UINT; KeyState: pbyte; Buffer: pwidechar; BufferSize: longint; Flags: UINT; Layout: HKL): longint;
  stdcall; external 'user32' name 'ToUnicodeEx';

const
  VkToScanCode = 0;
  // ToUnicodeEx leaves the keyboard's dead-key state alone (Windows 10 1607 on)
  KeepKeyboardState = 4;

type
  TKeyStates = array[0..255] of byte;

  TWinKeyInput = class(TKeyInput)
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

function InitializeWinKeyInput: TKeyInput;
begin
  Result := TWinKeyInput.Create;
end;

// Keys that share a scan code with a number pad key or a left-hand twin.
function IsExtended(Key: word): boolean;
begin
  case Key of
    VK_LEFT, VK_UP, VK_RIGHT, VK_DOWN, VK_INSERT, VK_DELETE, VK_HOME, VK_END, VK_PRIOR, VK_NEXT,
    VK_DIVIDE, VK_NUMLOCK, VK_RCONTROL, VK_RMENU, VK_LWIN, VK_RWIN, VK_APPS: Result := True;
    else
      Result := False;
  end;
end;

function KeyEvent(Key: word; Release: boolean): TInput;
var
  Scan: UINT;
begin
  Result := Default(TInput);
  Result._Type := INPUT_KEYBOARD;
  Result.ki.wVk := Key;
  Scan := MapVirtualKeyW(Key, VkToScanCode);
  // Pause has a prefixed scan code SendInput cannot carry; it goes by its key
  if (Scan <> 0) and (Key <> VK_PAUSE) then
  begin
    Result.ki.wScan := Scan;
    Result.ki.dwFlags := KEYEVENTF_SCANCODE;
    if IsExtended(Key) then Result.ki.dwFlags := Result.ki.dwFlags or KEYEVENTF_EXTENDEDKEY;
  end;
  if Release then Result.ki.dwFlags := Result.ki.dwFlags or KEYEVENTF_KEYUP;
end;

function CharEvent(UTF16: word; Release: boolean): TInput;
begin
  Result := Default(TInput);
  Result._Type := INPUT_KEYBOARD;
  Result.ki.wScan := UTF16;
  Result.ki.dwFlags := KEYEVENTF_UNICODE;
  if Release then Result.ki.dwFlags := Result.ki.dwFlags or KEYEVENTF_KEYUP;
end;

procedure TWinKeyInput.DoDown(Key: word);
var
  Input: TInput;
begin
  Input := KeyEvent(Key, False);
  SendInput(1, @Input, SizeOf(TInput));
end;

procedure TWinKeyInput.DoUp(Key: word);
var
  Input: TInput;
begin
  Input := KeyEvent(Key, True);
  SendInput(1, @Input, SizeOf(TInput));
end;

// Asks the layout of the window being typed into which key and modifiers
// give the character, and checks the answer before using it.
procedure TWinKeyInput.PressASCIIChar(ch: char);
const
  ModifierKeys: array[0..2] of word = (VK_SHIFT, VK_CONTROL, VK_MENU);
var
  Layout: HKL;
  Found: SHORT;
  Key, Mods: word;
  States: TKeyStates;
  Typed: array[0..3] of widechar;
  Inputs: array[0..7] of TInput;
  Count, I: integer;
begin
  if ch < ' ' then
  begin
    inherited PressASCIIChar(ch);
    Exit;
  end;
  Layout := GetKeyboardLayout(GetWindowThreadProcessId(GetForegroundWindow, nil));
  Found := VkKeyScanExW(widechar(Ord(ch)), Layout);
  Key := Found and $FF;
  Mods := (Found shr 8) and $FF;
  if (Found = -1) or (Mods and not 7 <> 0) then
  begin
    PressUnicodeChar(Ord(ch));
    Exit;
  end;
  if ch in ['a'..'z', 'A'..'Z'] then
    if (ch in ['A'..'Z']) <> GetCapsLockState then Mods := Mods or 1
    else
      Mods := Mods and not 1;

  // a dead key, or a key that gives something else, is no good: type it instead
  States := Default(TKeyStates);
  for I := 0 to 2 do
    if Mods and (1 shl I) <> 0 then States[ModifierKeys[I]] := $80;
  States[VK_CAPITAL] := GetKeyState(VK_CAPITAL) and 1;
  if (ToUnicodeEx(Key, MapVirtualKeyW(Key, VkToScanCode), @States[0], @Typed[0], Length(Typed), KeepKeyboardState, Layout) <> 1) or (Typed[0] <> widechar(Ord(ch))) then
  begin
    PressUnicodeChar(Ord(ch));
    Exit;
  end;

  Count := 0;
  for I := 0 to 2 do
    if Mods and (1 shl I) <> 0 then
    begin
      Inputs[Count] := KeyEvent(ModifierKeys[I], False);
      Inc(Count);
    end;
  Inputs[Count] := KeyEvent(Key, False);
  Inputs[Count + 1] := KeyEvent(Key, True);
  Inc(Count, 2);
  for I := 2 downto 0 do
    if Mods and (1 shl I) <> 0 then
    begin
      Inputs[Count] := KeyEvent(ModifierKeys[I], True);
      Inc(Count);
    end;
  SendInput(Count, @Inputs[0], SizeOf(TInput));
  Application.ProcessMessages;
end;

procedure TWinKeyInput.PressUnicodeChar(unicode: cardinal);
var
  Inputs: array[0..3] of TInput;
  High, Low: word;
begin
  if (unicode = 0) or (unicode > $10FFFF) then Exit;
  if unicode < $10000 then
  begin
    Inputs[0] := CharEvent(unicode, False);
    Inputs[1] := CharEvent(unicode, True);
    SendInput(2, @Inputs[0], SizeOf(TInput));
  end
  else
  begin
    Dec(unicode, $10000);
    High := $D800 + (unicode shr 10);
    Low := $DC00 + (unicode and $3FF);
    Inputs[0] := CharEvent(High, False);
    Inputs[1] := CharEvent(High, True);
    Inputs[2] := CharEvent(Low, False);
    Inputs[3] := CharEvent(Low, True);
    SendInput(4, @Inputs[0], SizeOf(TInput));
  end;
  Application.ProcessMessages;
end;

function TWinKeyInput.GetCapsLockState: boolean;
begin
  Result := GetKeyState(VK_CAPITAL) and 1 <> 0;
end;

function TWinKeyInput.GetNumLockState: boolean;
begin
  Result := GetKeyState(VK_NUMLOCK) and 1 <> 0;
end;

function TWinKeyInput.GetScrollLockState: boolean;
begin
  Result := GetKeyState(VK_SCROLL) and 1 <> 0;
end;

procedure TWinKeyInput.ToggleCapsLock;
begin
  Press(VK_CAPITAL);
end;

procedure TWinKeyInput.ToggleNumLock;
begin
  Press(VK_NUMLOCK);
end;

procedure TWinKeyInput.ToggleScrollLock;
begin
  Press(VK_SCROLL);
end;

end.
