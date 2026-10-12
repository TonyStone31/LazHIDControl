unit HIDRegister;
{$mode objfpc}{$H+}
// Copyright (c) 2026 Tony Stone. GPL-2.0-or-later; see COPYING.GPL2.
interface
procedure Register;
implementation
uses Classes, HIDHotkey, HIDInput, HIDScreenshot, HIDWindow, HIDIdle, HIDPower, HIDClipboard;
procedure Register;
begin
  RegisterComponents('LazHIDControl', [THIDHotkey, THIDKeyboard, THIDMouse, THIDScreenshot, THIDWindow, THIDIdle, THIDPowerMonitor, THIDClipboardMonitor]);
end;
end.
