{ WaylandHotkeyInput

  Copyright (C) 2025 LazHIDControl Contributors

  Wayland implementation of global hotkey registration using freedesktop portal API.

  Wayland does not support traditional global hotkeys (XGrabKey) due to security restrictions.
  Instead, we use the org.freedesktop.portal.GlobalShortcuts portal API via D-Bus.

  Not written yet: registration always fails, and THIDHotkey reports that.
  The portal is on KDE Plasma, GNOME 48+ and Hyprland; the user approves each
  shortcut in a dialog, and compositor shortcuts cannot be overridden.

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
  Classes, SysUtils, LCLType, HotkeyInputIntf;

type
  TWaylandHotkey = class(THotkey)
  protected
    procedure DoRegister; override;
    procedure DoUnregister; override;
  end;

function InitializeHotkey(AKey: word; AShift: TShiftState; AOnHotkey: TKeyNotifyEvent): THotkey;

implementation

function InitializeHotkey(AKey: word; AShift: TShiftState; AOnHotkey: TKeyNotifyEvent): THotkey;
begin
  Result := TWaylandHotkey.Create(AKey, AShift, AOnHotkey);
end;

procedure TWaylandHotkey.DoRegister;
begin
end;

procedure TWaylandHotkey.DoUnregister;
begin
end;

end.
