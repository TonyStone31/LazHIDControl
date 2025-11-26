{ CocoaGlobalKeyMonitor

  Copyright (C) 2025 LazHIDControl Contributors

  macOS/Cocoa implementation of global key monitoring (stub).

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
unit CocoaGlobalKeyMonitor;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LCLType, GlobalKeyMonitorIntf;

type
  { TCocoaGlobalKeyListener }
  TCocoaGlobalKeyListener = class(TGlobalKeyListener)
  protected
    procedure DoStartMonitoring; override;
    procedure DoStopMonitoring; override;
  public
    constructor Create; override;
  end;

function InitializeGlobalKeyListener: TGlobalKeyListener;

implementation

function InitializeGlobalKeyListener: TGlobalKeyListener;
begin
  Result := TCocoaGlobalKeyListener.Create;
end;

{ TCocoaGlobalKeyListener }

constructor TCocoaGlobalKeyListener.Create;
begin
  inherited Create;
  // TODO: Initialize Cocoa-specific event monitor
end;

procedure TCocoaGlobalKeyListener.DoStartMonitoring;
begin
  // TODO: Implement macOS global key monitoring using NSEvent
  // Reference: NSEvent.addGlobalMonitorForEventsMatchingMask
  Active := False;
end;

procedure TCocoaGlobalKeyListener.DoStopMonitoring;
begin
  if not Active then Exit;

  // TODO: Implement macOS key monitoring stop
  // Reference: NSEvent.removeMonitor

  Active := False;
end;

end.
