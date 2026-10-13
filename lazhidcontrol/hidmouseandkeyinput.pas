{ HIDMouseAndKeyInput

  Copyright (C) 2008 Tom Gregorovic

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
unit HIDMouseAndKeyInput;

interface

uses
  HIDMouseInputIntf,
  HIDKeyInputIntf,
  {$IFDEF WINDOWS}
  HIDWinMouseInput,
  HIDWinKeyInput,
  {$ENDIF}
  {$IFDEF UNIX}
    {$IF DEFINED(LCLcarbon)}
    HIDCarbonMouseInput,
    HIDCarbonKeyInput,
    {$ELSEIF DEFINED(LCLcocoa)}
    HIDCocoaMouseInput,
    HIDCocoaKeyInput,
    {$ELSE}
    WaylandMouseAndKeyInput,
    HIDXMouseInput,
    HIDXKeyInput,
    {$ENDIF}
  {$ENDIF}
  Classes, SysUtils;

var
  MouseInput: TMouseInput;
  KeyInput: TKeyInput;

implementation

initialization

  {$IF DEFINED(UNIX) AND NOT DEFINED(DARWIN)}
  if GetEnvironmentVariable('WAYLAND_DISPLAY') <> '' then
  begin
    MouseInput := InitializeWaylandMouseInput;
    KeyInput := InitializeWaylandKeyInput;
  end
  else
  {$ENDIF}
  begin
    MouseInput := InitializeMouseInput;
    KeyInput := InitializeKeyInput;
  end;

finalization

  FreeAndNil(MouseInput);
  FreeAndNil(KeyInput);

end.
