{ GlobalKeyMonitor

  Copyright (c) 2025-2026 Tony Stone. 0BSD.

  Provides cross-platform system-wide keyboard monitoring for
  Windows, X11, and macOS systems.

  Note: Wayland does not support global key monitoring due to security restrictions.
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
    {$IFDEF DARWIN}
    CocoaGlobalKeyMonitor,
    {$ELSE}
    XGlobalKeyMonitor,
    {$ENDIF}
    {$IFDEF LINUX}
    EvdevMonitor,
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
  {$IFDEF LINUX}
  // a Wayland compositor shows a program no keys but its own; the kernel shows them all
  if GetEnvironmentVariable('WAYLAND_DISPLAY') <> '' then Exit(InitializeEvdevKeyListener);
  {$ENDIF}
  Result := InitializeGlobalKeyListener;
end;

end.
