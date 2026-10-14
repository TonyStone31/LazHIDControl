unit LinuxKeyCodes;

{$mode objfpc}{$H+}
// Copyright (c) 2026 Tony Stone. GPL-2.0-or-later; see COPYING.GPL2.

{ The kernel's key codes against the LCL's virtual keys, as a US keyboard is
  laid out.  uinput presses them and evdev reports them. }

interface

// 0 when the LCL key has no kernel code.
function KernelKey(Key: word): word;
// 0 when the kernel code has no LCL key.
function KeyFromKernel(Code: word): word;

implementation

uses LCLType;

function KernelKey(Key: word): word;
const
  Letters: array[VK_A..VK_Z] of byte = (30, 48, 46, 32, 18, 33, 34, 35, 23, 36, 37, 38, 50, 49, 24, 25, 16, 19, 31, 20, 22, 47, 17, 45, 21, 44);
begin
  case Key of
    VK_A..VK_Z: Result := Letters[Key];
    VK_1..VK_9: Result := Key - VK_1 + 2;
    VK_0: Result := 11;
    VK_F1..VK_F10: Result := Key - VK_F1 + 59;
    VK_F11: Result := 87;
    VK_F12: Result := 88;
    VK_ESCAPE: Result := 1;
    VK_LCL_MINUS: Result := 12;
    VK_LCL_EQUAL: Result := 13;
    VK_BACK: Result := 14;
    VK_TAB: Result := 15;
    VK_LCL_OPEN_BRACKET: Result := 26;
    VK_LCL_CLOSE_BRACKET: Result := 27;
    VK_RETURN: Result := 28;
    VK_CONTROL, VK_LCONTROL: Result := 29;
    VK_LCL_SEMI_COMMA: Result := 39;
    VK_LCL_QUOTE: Result := 40;
    VK_LCL_TILDE: Result := 41;
    VK_SHIFT, VK_LSHIFT: Result := 42;
    VK_LCL_BACKSLASH: Result := 43;
    VK_LCL_COMMA: Result := 51;
    VK_LCL_POINT: Result := 52;
    VK_LCL_SLASH: Result := 53;
    VK_RSHIFT: Result := 54;
    VK_MULTIPLY: Result := 55;
    VK_MENU, VK_LMENU: Result := 56;
    VK_SPACE: Result := 57;
    VK_CAPITAL: Result := 58;
    VK_NUMLOCK: Result := 69;
    VK_SCROLL: Result := 70;
    VK_NUMPAD7..VK_NUMPAD9: Result := Key - VK_NUMPAD7 + 71;
    VK_SUBTRACT: Result := 74;
    VK_NUMPAD4..VK_NUMPAD6: Result := Key - VK_NUMPAD4 + 75;
    VK_ADD: Result := 78;
    VK_NUMPAD1..VK_NUMPAD3: Result := Key - VK_NUMPAD1 + 79;
    VK_NUMPAD0: Result := 82;
    VK_DECIMAL: Result := 83;
    VK_RCONTROL: Result := 97;
    VK_DIVIDE: Result := 98;
    VK_SNAPSHOT: Result := 99;
    VK_RMENU: Result := 100;
    VK_HOME: Result := 102;
    VK_UP: Result := 103;
    VK_PRIOR: Result := 104;
    VK_LEFT: Result := 105;
    VK_RIGHT: Result := 106;
    VK_END: Result := 107;
    VK_DOWN: Result := 108;
    VK_NEXT: Result := 109;
    VK_INSERT: Result := 110;
    VK_DELETE: Result := 111;
    VK_PAUSE: Result := 119;
    VK_LWIN: Result := 125;
    VK_RWIN: Result := 126;
    else
      Result := 0;
  end;
end;

var
  Inverse: array[0..255] of word;

function KeyFromKernel(Code: word): word;
begin
  if Code < Length(Inverse) then Result := Inverse[Code]
  else
    Result := 0;
end;

procedure BuildInverse;
var
  Key, Code: word;
begin
  // the general key wins where two share a code: VK_SHIFT before VK_LSHIFT
  for Key := 1 to 255 do
  begin
    Code := KernelKey(Key);
    if (Code <> 0) and (Inverse[Code] = 0) then Inverse[Code] := Key;
  end;
end;

initialization
  BuildInverse;
end.
