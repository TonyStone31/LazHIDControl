{ WindowManager

  Copyright (c) 2025-2026 Tony Stone. 0BSD.

  Cross-platform window management dispatcher unit.
  Automatically selects the correct platform implementation.
}
unit WindowManager;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Types,
  WindowManagerIntf
  {$IFDEF WINDOWS}
  , WinWindowManager
  {$ENDIF}
  {$IFDEF DARWIN}
  , CocoaWindowManager
  {$ENDIF}
  {$IFDEF UNIX}
    {$IFNDEF DARWIN}
    , XWindowManager, WaylandWindowManager
    {$ENDIF}
  {$ENDIF}
  ;

type
  // Re-export types from interface
  TWindowHandle = WindowManagerIntf.TWindowHandle;
  TWindowInfo = WindowManagerIntf.TWindowInfo;

// Create and return a window manager instance for the current platform
function CreateWindowManager: TWindowManager;

// Get window at specific screen coordinates
function GetWindowAtPoint(X, Y: integer): TWindowHandle;

// Get information about window at point
function GetWindowInfoAtPoint(X, Y: integer; out AInfo: TWindowInfo): boolean;

// Convenience: Get window at current mouse position
function GetWindowUnderMouse: TWindowHandle;

// Ensure the global window manager is created
procedure EnsureWindowManager;

var
  // Global window manager instance (created on first use)
  WindowMgr: TWindowManager = nil;

implementation

uses
  Controls;

function CreateWindowManager: TWindowManager;
begin
  {$IFDEF WINDOWS}
  Result := WinWindowManager.InitializeWindowManager;
  {$ELSE}
  {$IFDEF UNIX}
      {$IFDEF DARWIN}
      Result := CocoaWindowManager.InitializeWindowManager;
      {$ELSE}
      // Do not use X11 as a substitute for native Wayland window control.
      if GetEnvironmentVariable('WAYLAND_DISPLAY') <> '' then
        Result := WaylandWindowManager.InitializeWindowManager
      else
        Result := XWindowManager.InitializeWindowManager;
      {$ENDIF}
  {$ELSE}
  Result := nil;
  {$ENDIF}
  {$ENDIF}
end;

procedure EnsureWindowManager;
begin
  if WindowMgr = nil then
    WindowMgr := CreateWindowManager;
end;

function GetWindowAtPoint(X, Y: integer): TWindowHandle;
begin
  EnsureWindowManager;
  if WindowMgr = nil then Result := 0
  else
    Result := WindowMgr.GetWindowAtPoint(X, Y);
end;

function GetWindowInfoAtPoint(X, Y: integer; out AInfo: TWindowInfo): boolean;
var
  Handle: TWindowHandle;
begin
  Result := False;
  FillByte(AInfo, SizeOf(AInfo), 0);

  Handle := GetWindowAtPoint(X, Y);
  if Handle = 0 then Exit;

  EnsureWindowManager;
  if WindowMgr = nil then Exit;

  Result := WindowMgr.GetWindowInfo(Handle, AInfo);
end;

function GetWindowUnderMouse: TWindowHandle;
begin
  Result := GetWindowAtPoint(Mouse.CursorPos.X, Mouse.CursorPos.Y);
end;

initialization

finalization
  if WindowMgr <> nil then
  begin
    WindowMgr.Free;
    WindowMgr := nil;
  end;

end.
