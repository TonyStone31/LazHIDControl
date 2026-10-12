{ This file was automatically created by Lazarus. Do not edit!
  This source is only used to compile and install the package.
 }

unit LazHIDControl;

{$warn 5023 off : no warning about unused units}
interface

uses
  HIDMouseAndKeyInput, HotkeyInput, GlobalKeyMonitor, GlobalMouseMonitor,
  WindowManagerIntf, WindowManager, HIDHotkey, HIDRegister, HIDInput,
  HIDScreenshot, HIDWindow, HIDSystemIdle, HIDPower, HIDClipboard, LazarusPackageIntf;

implementation

procedure Register;
begin
  RegisterUnit('HIDRegister', @HIDRegister.Register);
end;

initialization
  RegisterPackage('LazHIDControl', @Register);
end.
