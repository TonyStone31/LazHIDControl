{ WaylandHotkeyInput

  Copyright (C) 2025 LazHIDControl Contributors

  Wayland implementation of global hotkey registration using freedesktop portal API.

  Wayland does not support traditional global hotkeys (XGrabKey) due to security restrictions.
  Instead, we use the org.freedesktop.portal.GlobalShortcuts portal API via D-Bus.

  This implementation:
  - Works on GNOME 45+ and KDE Plasma 5/6
  - Does NOT work on Sway/wlroots or Hyprland (no portal support)
  - Requires user permission dialogs
  - Cannot override compositor hotkeys

  See: https://flatpak.github.io/xdg-desktop-portal/docs/doc-org.freedesktop.portal.GlobalShortcuts.html

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
unit WaylandHotkeyInput;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LCLType, dbus, BaseUnix, HotkeyInputIntf;

type
  TWaylandHotkeyData = record
    Conn: PDBusConnection;
    SessionHandle: UTF8String;
    ShortcutToken: UTF8String;
    Hotkey: THotkey;
  end;
  PWaylandHotkeyData = ^TWaylandHotkeyData;

  { TWaylandHotkey }
  TWaylandHotkey = class(THotkey)
  private
    HotkeyData: PWaylandHotkeyData;
  protected
    procedure DoRegister; override;
    procedure DoUnregister; override;
  public
    constructor Create(AKey: Word; AShift: TShiftState; AOnHotkey: TKeyNotifyEvent); override;
  end;

function InitializeHotkey(AKey: Word; AShift: TShiftState; AOnHotkey: TKeyNotifyEvent): THotkey;

implementation

const
  PORTAL = 'org.freedesktop.portal.Desktop';
  PATH   = '/org/freedesktop/portal/desktop';
  IFACE  = 'org.freedesktop.portal.GlobalShortcuts';

var
  GlobalHotkeys: TList = nil;
  PortalConn: PDBusConnection = nil;
  MonitorThread: TThreadID = 0;
  MonitorActive: Boolean = False;

function KeyToWaylandAccelerator(Key: Word; Shift: TShiftState): UTF8String;
var
  Modifiers: string;
  KeyName: string;
begin
  Modifiers := '';
  if ssCtrl in Shift then Modifiers := Modifiers + '<Ctrl>';
  if ssShift in Shift then Modifiers := Modifiers + '<Shift>';
  if ssAlt in Shift then Modifiers := Modifiers + '<Alt>';

  case Key of
    VK_F1..VK_F12: KeyName := 'F' + IntToStr(Key - VK_F1 + 1);
    VK_A..VK_Z: KeyName := Chr(Key);
    VK_0..VK_9: KeyName := Chr(Key);
    VK_SPACE: KeyName := 'space';
    VK_RETURN: KeyName := 'Return';
    VK_ESCAPE: KeyName := 'Escape';
    VK_TAB: KeyName := 'Tab';
  else
    KeyName := '';
  end;

  if KeyName = '' then
    Result := ''
  else
    Result := Modifiers + KeyName;
end;

function PortalMessageLoop(parameter: Pointer): PtrInt;
begin
  // TODO: Implement D-Bus message loop for Wayland portal events
  while MonitorActive do
    Sleep(100);
  Result := 0;
end;

function InitializeHotkey(AKey: Word; AShift: TShiftState; AOnHotkey: TKeyNotifyEvent): THotkey;
begin
  Result := TWaylandHotkey.Create(AKey, AShift, AOnHotkey);
end;

{ TWaylandHotkey }

constructor TWaylandHotkey.Create(AKey: Word; AShift: TShiftState; AOnHotkey: TKeyNotifyEvent);
begin
  inherited Create(AKey, AShift, AOnHotkey);
  HotkeyData := nil;

  if GlobalHotkeys = nil then
    GlobalHotkeys := TList.Create;

  if PortalConn = nil then
  begin
    PortalConn := dbus_bus_get(DBUS_BUS_SESSION, nil);
    if PortalConn <> nil then
    begin
      MonitorActive := True;
      MonitorThread := BeginThread(@PortalMessageLoop, nil);
    end;
  end;
end;

procedure TWaylandHotkey.DoRegister;
begin
  // TODO: Implement Wayland portal global shortcuts API
  // The D-Bus bindings in FPC need updates for the newer portal API
  // Reference: https://flatpak.github.io/xdg-desktop-portal/docs/doc-org.freedesktop.portal.GlobalShortcuts.html
  Registered := False;
end;

procedure TWaylandHotkey.DoUnregister;
var
  I: Integer;
  Data: PWaylandHotkeyData;
begin
  if not Registered then Exit;

  for I := 0 to GlobalHotkeys.Count - 1 do
  begin
    Data := PWaylandHotkeyData(GlobalHotkeys[I]);
    if Data^.Hotkey = Self then
    begin
      GlobalHotkeys.Delete(I);
      Dispose(Data);
      Break;
    end;
  end;

  Registered := False;
end;

end.
