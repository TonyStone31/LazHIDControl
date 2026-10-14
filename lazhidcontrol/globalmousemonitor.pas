{ GlobalMouseMonitor

  Platform dispatcher for global mouse monitoring

  Copyright (C) 2025 LazHIDControl Contributors

  This source is free software; you can redistribute it and/or modify it under the terms of the
  GNU General Public License as published by the Free Software Foundation; either version 2 of the
  License, or (at your option) any later version.
}
unit GlobalMouseMonitor;

{$mode objfpc}{$H+}

interface

uses
  GlobalMouseMonitorIntf, Controls;

  // Re-export the base type and event for convenience
type
  TMouseClickEvent = GlobalMouseMonitorIntf.TMouseClickEvent;
  TGlobalMouseListener = GlobalMouseMonitorIntf.TGlobalMouseListener;

function CreateGlobalMouseListener: TGlobalMouseListener;

implementation

uses
  {$IF DEFINED(WINDOWS)}
  WinGlobalMouseMonitor
  {$ELSEIF DEFINED(DARWIN)}
  CocoaGlobalMouseMonitor
  {$ELSEIF DEFINED(LINUX)}
  XGlobalMouseMonitor, SysUtils
  {$ELSE}
  XGlobalMouseMonitor
  {$ENDIF};

function CreateGlobalMouseListener: TGlobalMouseListener;
begin
  {$IF DEFINED(LINUX)}
  // a Wayland desktop tells no program where the pointer is, so there is nothing honest to report
  if GetEnvironmentVariable('WAYLAND_DISPLAY') <> '' then Exit(nil);
  {$ENDIF}
  Result := InitializeGlobalMouseListener;
end;

end.
