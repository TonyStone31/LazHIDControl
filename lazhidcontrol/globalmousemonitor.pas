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

{$IFDEF UNIX}
  {$IFDEF DARWIN}
    // macOS - not yet implemented, return nil
  {$ELSE}
    uses XGlobalMouseMonitor;
  {$ENDIF}
{$ENDIF}

{$IFDEF WINDOWS}
  uses WinGlobalMouseMonitor;
{$ENDIF}

function CreateGlobalMouseListener: TGlobalMouseListener;
begin
  {$IFDEF UNIX}
    {$IFDEF DARWIN}
      Result := nil;  // macOS not yet implemented
    {$ELSE}
      Result := InitializeGlobalMouseListener;  // X11
    {$ENDIF}
  {$ENDIF}

  {$IFDEF WINDOWS}
    Result := InitializeGlobalMouseListener;  // Windows
  {$ENDIF}
end;

end.
