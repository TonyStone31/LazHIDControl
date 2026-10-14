unit HIDRegister;
{$mode objfpc}{$H+}
// Copyright (c) 2026 Tony Stone. GPL-2.0-or-later; see COPYING.GPL2.
interface

procedure Register;

implementation

uses LResources, Classes, HIDHotkey, HIDInput, HIDScreenshot, HIDWindow, HIDSystemIdle, HIDPower, HIDClipboard, HIDMonitor, HIDPermission;

procedure Register;
begin
  RegisterComponents('LazHIDControl', [THIDHotkey, THIDKeyboard, THIDMouse, THIDScreenshot, THIDWindow, THIDSystemIdle, THIDPowerMonitor, THIDClipboardMonitor, THIDInputMonitor, THIDPermission]);
end;

initialization
  {$I design/resources/HIDPalette.lrs}
end.
