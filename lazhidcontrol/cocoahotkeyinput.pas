{ CocoaHotkeyInput

  Copyright (C) 2025 LazHIDControl Contributors

  macOS/Cocoa implementation of global hotkey registration (stub).

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
unit CocoaHotkeyInput;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LCLType, HotkeyInputIntf;

type
  { TCocoaHotkey }
  TCocoaHotkey = class(THotkey)
  protected
    procedure DoRegister; override;
    procedure DoUnregister; override;
  public
    constructor Create(AKey: Word; AShift: TShiftState; AOnHotkey: TKeyNotifyEvent); override;
  end;

function InitializeHotkey(AKey: Word; AShift: TShiftState; AOnHotkey: TKeyNotifyEvent): THotkey;

implementation

{ TCocoaHotkey }

function InitializeHotkey(AKey: Word; AShift: TShiftState; AOnHotkey: TKeyNotifyEvent): THotkey;
begin
  Result := TCocoaHotkey.Create(AKey, AShift, AOnHotkey);
end;

constructor TCocoaHotkey.Create(AKey: Word; AShift: TShiftState; AOnHotkey: TKeyNotifyEvent);
begin
  inherited Create(AKey, AShift, AOnHotkey);
  // TODO: Initialize Cocoa-specific hotkey handler
  // Reference: https://developer.apple.com/documentation/carbon/1390584-registereventhottarget
end;

procedure TCocoaHotkey.DoRegister;
begin
  // TODO: Implement macOS global hotkey registration using Carbon Event Manager or NSEvent
  // For Carbon: RegisterEventHotKey()
  // For modern Cocoa: NSEvent.addGlobalMonitorForEventsMatchingMask
  Registered := False;
end;

procedure TCocoaHotkey.DoUnregister;
begin
  if not Registered then Exit;

  // TODO: Implement macOS hotkey unregistration
  // For Carbon: UnregisterEventHotKey()

  Registered := False;
end;

end.
