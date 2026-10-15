unit WaylandHotkeyInput;

{$mode objfpc}{$H+}
// Copyright (c) 2026 Tony Stone. 0BSD.

{ Wayland gives no client a system-wide key grab.  The way in is the
  GlobalShortcuts portal (org.freedesktop.portal.GlobalShortcuts); until that
  is written a hotkey here never registers. }

interface

uses
  HotkeyInputIntf;

type
  TWaylandHotkey = class(THotkey)
  end;

implementation

end.
