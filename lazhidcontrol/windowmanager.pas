{ WindowManager

  Copyright (C) 2025 LazHIDControl Contributors

  Cross-platform window management dispatcher unit.
  Automatically selects the correct platform implementation.

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
unit WindowManager;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Types,
  WindowManagerIntf
  {$IFDEF WINDOWS}
  , WinWindowManager
  {$ENDIF}
  {$IFDEF UNIX}
    {$IFNDEF DARWIN}
    , XWindowManager
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
function GetWindowAtPoint(X, Y: Integer): TWindowHandle;

// Get information about window at point
function GetWindowInfoAtPoint(X, Y: Integer; out AInfo: TWindowInfo): Boolean;

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
      // macOS - not yet implemented
      Result := nil;
      {$ELSE}
      // Linux/BSD - X11
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

function GetWindowAtPoint(X, Y: Integer): TWindowHandle;
begin
  Result := 0;
  EnsureWindowManager;
  if WindowMgr = nil then Exit;

  {$IFDEF WINDOWS}
  Result := TWinWindowManager(WindowMgr).GetWindowAtPoint(X, Y);
  {$ELSE}
    {$IFDEF UNIX}
      {$IFNDEF DARWIN}
      Result := TXWindowManager(WindowMgr).GetWindowAtPoint(X, Y);
      {$ENDIF}
    {$ENDIF}
  {$ENDIF}
end;

function GetWindowInfoAtPoint(X, Y: Integer; out AInfo: TWindowInfo): Boolean;
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
