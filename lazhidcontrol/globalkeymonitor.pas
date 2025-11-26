{ GlobalKeyMonitor

  Copyright (C) 2025 LazHIDControl Contributors

  Provides cross-platform system-wide keyboard monitoring for
  Windows, X11, and macOS systems.

  Note: Wayland does not support global key monitoring due to security restrictions.

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
unit GlobalKeyMonitor;

{$mode objfpc}{$H+}

interface

uses
  GlobalKeyMonitorIntf,
  {$IFDEF WINDOWS}
  WinGlobalKeyMonitor,
  {$ENDIF}
  {$IFDEF UNIX}
    {$IFDEF LCLcarbon}
    CocoaGlobalKeyMonitor,
    {$ELSE}
    XGlobalKeyMonitor,
    {$ENDIF}
  {$ENDIF}
  Classes, SysUtils, LCLType;

// Re-export the base type and event for convenience
type
  TKeyEvent = GlobalKeyMonitorIntf.TKeyEvent;
  TGlobalKeyListener = GlobalKeyMonitorIntf.TGlobalKeyListener;

function CreateGlobalKeyListener: TGlobalKeyListener;

implementation

function CreateGlobalKeyListener: TGlobalKeyListener;
begin
  {$IFDEF WINDOWS}
  Result := InitializeGlobalKeyListener;
  {$ELSE}
    {$IFDEF UNIX}
      {$IFDEF LCLcarbon}
      Result := CocoaGlobalKeyMonitor.InitializeGlobalKeyListener;
      {$ELSE}
      // Note: X11 only - Wayland does not support global key monitoring
      Result := XGlobalKeyMonitor.InitializeGlobalKeyListener;
      {$ENDIF}
    {$ELSE}
    // Unsupported platform - return a stub that always fails
    Result := TGlobalKeyListener.Create;
    {$ENDIF}
  {$ENDIF}
end;

end.
