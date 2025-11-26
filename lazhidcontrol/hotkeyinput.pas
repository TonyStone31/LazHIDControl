{ HotkeyInput

  Copyright (C) 2025 LazHIDControl Contributors

  Adapted from Codebot Pascal Library
  http://cross.codebot.org
  Original Author: Codebot Development Team

  Provides cross-platform global hotkey registration for
  Windows, X11, Wayland (via portal), and macOS systems.

  Usage:
    var
      MyHotkey: THotkey;

    MyHotkey := THotkey.Create(VK_F9, [ssCtrl, ssShift], @MyHotkeyHandler);
    MyHotkey.Register;

    // In your handler:
    procedure TForm1.MyHotkeyHandler(Sender: TObject; Key: Word; Shift: TShiftState);
    begin
      // Use Application.QueueAsyncCall to ensure modifiers are released
      Application.QueueAsyncCall(@ActualHandler, 0);
    end;

    // Later...
    MyHotkey.Unregister;
    MyHotkey.Free;

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
unit HotkeyInput;

{$mode objfpc}{$H+}

interface

uses
  HotkeyInputIntf,
  {$IFDEF WINDOWS}
  WinHotkeyInput,
  {$ENDIF}
  {$IFDEF UNIX}
    {$IFDEF LCLcarbon}
    CocoaHotkeyInput,
    {$ELSE}
    WaylandHotkeyInput,
    XHotkeyInput,
    {$ENDIF}
  {$ENDIF}
  Classes, SysUtils, LCLType;

// Re-export the base type and event for convenience
type
  TKeyNotifyEvent = HotkeyInputIntf.TKeyNotifyEvent;
  THotkey = HotkeyInputIntf.THotkey;

function CreateHotkey(AKey: Word; AShift: TShiftState; AOnHotkey: TKeyNotifyEvent): THotkey;

implementation

{$IFDEF UNIX}
function IsWayland: Boolean;
begin
  Result := GetEnvironmentVariable('WAYLAND_DISPLAY') <> '';
end;
{$ENDIF}

function CreateHotkey(AKey: Word; AShift: TShiftState; AOnHotkey: TKeyNotifyEvent): THotkey;
begin
  {$IFDEF WINDOWS}
  Result := InitializeHotkey(AKey, AShift, AOnHotkey);
  {$ELSE}
    {$IFDEF UNIX}
      {$IFDEF LCLcarbon}
      Result := CocoaHotkeyInput.InitializeHotkey(AKey, AShift, AOnHotkey);
      {$ELSE}
      if IsWayland then
        Result := WaylandHotkeyInput.InitializeHotkey(AKey, AShift, AOnHotkey)
      else
        Result := XHotkeyInput.InitializeHotkey(AKey, AShift, AOnHotkey);
      {$ENDIF}
    {$ELSE}
    // Unsupported platform - return a stub that always fails to register
    Result := THotkey.Create(AKey, AShift, AOnHotkey);
    {$ENDIF}
  {$ENDIF}
end;

end.
