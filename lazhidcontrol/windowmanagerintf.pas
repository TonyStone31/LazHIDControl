{ WindowManagerIntf

  Copyright (C) 2025 LazHIDControl Contributors

  Cross-platform window management interface providing xdotool-like functionality
  for window automation tasks.

  This interface provides common window management operations such as:
  - Getting active window information
  - Window focus and activation
  - Window positioning and resizing
  - Window state changes (minimize, maximize, etc.)
  - Window enumeration and search

  Platform-specific implementations:
  - Windows: Win32 API (FindWindow, SetForegroundWindow, etc.)
  - X11: Extended Window Manager Hints (EWMH) via Xlib
  - macOS: Cocoa/AppKit (NSWindow, NSWorkspace, etc.)
  - Wayland: Limited support via wlr-foreign-toplevel protocol

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
unit WindowManagerIntf;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Types;

type
  TWindowHandle = PtrUInt;  // Platform-agnostic window handle

  TWindowInfo = record
    Handle: TWindowHandle;
    Title: String;
    ClassName: String;
    ProcessID: LongWord;
    ProcessName: String;
    Rect: TRect;
    IsVisible: Boolean;
    IsMinimized: Boolean;
    IsMaximized: Boolean;
  end;

  { TWindowManager - Abstract base class for window management operations }
  TWindowManager = class
  protected
    // Window query methods
    function DoGetActiveWindow: TWindowHandle; virtual; abstract;
    function DoGetWindowInfo(AHandle: TWindowHandle; out AInfo: TWindowInfo): Boolean; virtual; abstract;
    function DoGetWindowTitle(AHandle: TWindowHandle): String; virtual; abstract;
    function DoGetWindowPID(AHandle: TWindowHandle): LongWord; virtual; abstract;
    function DoGetWindowRect(AHandle: TWindowHandle): TRect; virtual; abstract;

    // Window search methods
    function DoFindWindow(const ATitle: String): TWindowHandle; virtual; abstract;
    function DoFindWindowByClass(const AClassName: String): TWindowHandle; virtual; abstract;
    function DoFindWindowByPID(APID: LongWord): TWindowHandle; virtual; abstract;
    function DoEnumerateWindows: TList; virtual; abstract;  // Returns list of TWindowHandle

    // Window activation and focus
    function DoActivateWindow(AHandle: TWindowHandle): Boolean; virtual; abstract;
    function DoFocusWindow(AHandle: TWindowHandle): Boolean; virtual; abstract;
    function DoRaiseWindow(AHandle: TWindowHandle): Boolean; virtual; abstract;

    // Window positioning and sizing
    function DoMoveWindow(AHandle: TWindowHandle; X, Y: Integer): Boolean; virtual; abstract;
    function DoResizeWindow(AHandle: TWindowHandle; Width, Height: Integer): Boolean; virtual; abstract;
    function DoMoveResizeWindow(AHandle: TWindowHandle; X, Y, Width, Height: Integer): Boolean; virtual; abstract;

    // Window state management
    function DoMinimizeWindow(AHandle: TWindowHandle): Boolean; virtual; abstract;
    function DoMaximizeWindow(AHandle: TWindowHandle): Boolean; virtual; abstract;
    function DoRestoreWindow(AHandle: TWindowHandle): Boolean; virtual; abstract;
    function DoCloseWindow(AHandle: TWindowHandle): Boolean; virtual; abstract;

    // Window visibility
    function DoShowWindow(AHandle: TWindowHandle): Boolean; virtual; abstract;
    function DoHideWindow(AHandle: TWindowHandle): Boolean; virtual; abstract;
    function DoIsWindowVisible(AHandle: TWindowHandle): Boolean; virtual; abstract;

    // Desktop and workspace management (X11/macOS specific)
    function DoGetDesktopCount: Integer; virtual; abstract;
    function DoGetCurrentDesktop: Integer; virtual; abstract;
    function DoSetCurrentDesktop(ADesktop: Integer): Boolean; virtual; abstract;
    function DoGetWindowDesktop(AHandle: TWindowHandle): Integer; virtual; abstract;
    function DoSetWindowDesktop(AHandle: TWindowHandle; ADesktop: Integer): Boolean; virtual; abstract;
  public
    // Window query methods
    function GetActiveWindow: TWindowHandle;
    function GetWindowInfo(AHandle: TWindowHandle; out AInfo: TWindowInfo): Boolean;
    function GetWindowTitle(AHandle: TWindowHandle): String;
    function GetWindowPID(AHandle: TWindowHandle): LongWord;
    function GetWindowRect(AHandle: TWindowHandle): TRect;

    // Window search methods
    function FindWindow(const ATitle: String): TWindowHandle;
    function FindWindowByClass(const AClassName: String): TWindowHandle;
    function FindWindowByPID(APID: LongWord): TWindowHandle;
    function EnumerateWindows: TList;  // Returns list of TWindowHandle - caller must free

    // Window activation and focus
    function ActivateWindow(AHandle: TWindowHandle): Boolean;
    function FocusWindow(AHandle: TWindowHandle): Boolean;
    function RaiseWindow(AHandle: TWindowHandle): Boolean;

    // Window positioning and sizing
    function MoveWindow(AHandle: TWindowHandle; X, Y: Integer): Boolean;
    function ResizeWindow(AHandle: TWindowHandle; Width, Height: Integer): Boolean;
    function MoveResizeWindow(AHandle: TWindowHandle; X, Y, Width, Height: Integer): Boolean;

    // Window state management
    function MinimizeWindow(AHandle: TWindowHandle): Boolean;
    function MaximizeWindow(AHandle: TWindowHandle): Boolean;
    function RestoreWindow(AHandle: TWindowHandle): Boolean;
    function CloseWindow(AHandle: TWindowHandle): Boolean;

    // Window visibility
    function ShowWindow(AHandle: TWindowHandle): Boolean;
    function HideWindow(AHandle: TWindowHandle): Boolean;
    function IsWindowVisible(AHandle: TWindowHandle): Boolean;

    // Desktop and workspace management
    function GetDesktopCount: Integer;
    function GetCurrentDesktop: Integer;
    function SetCurrentDesktop(ADesktop: Integer): Boolean;
    function GetWindowDesktop(AHandle: TWindowHandle): Integer;
    function SetWindowDesktop(AHandle: TWindowHandle; ADesktop: Integer): Boolean;
  end;

implementation

{ TWindowManager }

// Window query methods
function TWindowManager.GetActiveWindow: TWindowHandle;
begin
  Result := DoGetActiveWindow;
end;

function TWindowManager.GetWindowInfo(AHandle: TWindowHandle; out AInfo: TWindowInfo): Boolean;
begin
  Result := DoGetWindowInfo(AHandle, AInfo);
end;

function TWindowManager.GetWindowTitle(AHandle: TWindowHandle): String;
begin
  Result := DoGetWindowTitle(AHandle);
end;

function TWindowManager.GetWindowPID(AHandle: TWindowHandle): LongWord;
begin
  Result := DoGetWindowPID(AHandle);
end;

function TWindowManager.GetWindowRect(AHandle: TWindowHandle): TRect;
begin
  Result := DoGetWindowRect(AHandle);
end;

// Window search methods
function TWindowManager.FindWindow(const ATitle: String): TWindowHandle;
begin
  Result := DoFindWindow(ATitle);
end;

function TWindowManager.FindWindowByClass(const AClassName: String): TWindowHandle;
begin
  Result := DoFindWindowByClass(AClassName);
end;

function TWindowManager.FindWindowByPID(APID: LongWord): TWindowHandle;
begin
  Result := DoFindWindowByPID(APID);
end;

function TWindowManager.EnumerateWindows: TList;
begin
  Result := DoEnumerateWindows;
end;

// Window activation and focus
function TWindowManager.ActivateWindow(AHandle: TWindowHandle): Boolean;
begin
  Result := DoActivateWindow(AHandle);
end;

function TWindowManager.FocusWindow(AHandle: TWindowHandle): Boolean;
begin
  Result := DoFocusWindow(AHandle);
end;

function TWindowManager.RaiseWindow(AHandle: TWindowHandle): Boolean;
begin
  Result := DoRaiseWindow(AHandle);
end;

// Window positioning and sizing
function TWindowManager.MoveWindow(AHandle: TWindowHandle; X, Y: Integer): Boolean;
begin
  Result := DoMoveWindow(AHandle, X, Y);
end;

function TWindowManager.ResizeWindow(AHandle: TWindowHandle; Width, Height: Integer): Boolean;
begin
  Result := DoResizeWindow(AHandle, Width, Height);
end;

function TWindowManager.MoveResizeWindow(AHandle: TWindowHandle; X, Y, Width, Height: Integer): Boolean;
begin
  Result := DoMoveResizeWindow(AHandle, X, Y, Width, Height);
end;

// Window state management
function TWindowManager.MinimizeWindow(AHandle: TWindowHandle): Boolean;
begin
  Result := DoMinimizeWindow(AHandle);
end;

function TWindowManager.MaximizeWindow(AHandle: TWindowHandle): Boolean;
begin
  Result := DoMaximizeWindow(AHandle);
end;

function TWindowManager.RestoreWindow(AHandle: TWindowHandle): Boolean;
begin
  Result := DoRestoreWindow(AHandle);
end;

function TWindowManager.CloseWindow(AHandle: TWindowHandle): Boolean;
begin
  Result := DoCloseWindow(AHandle);
end;

// Window visibility
function TWindowManager.ShowWindow(AHandle: TWindowHandle): Boolean;
begin
  Result := DoShowWindow(AHandle);
end;

function TWindowManager.HideWindow(AHandle: TWindowHandle): Boolean;
begin
  Result := DoHideWindow(AHandle);
end;

function TWindowManager.IsWindowVisible(AHandle: TWindowHandle): Boolean;
begin
  Result := DoIsWindowVisible(AHandle);
end;

// Desktop and workspace management
function TWindowManager.GetDesktopCount: Integer;
begin
  Result := DoGetDesktopCount;
end;

function TWindowManager.GetCurrentDesktop: Integer;
begin
  Result := DoGetCurrentDesktop;
end;

function TWindowManager.SetCurrentDesktop(ADesktop: Integer): Boolean;
begin
  Result := DoSetCurrentDesktop(ADesktop);
end;

function TWindowManager.GetWindowDesktop(AHandle: TWindowHandle): Integer;
begin
  Result := DoGetWindowDesktop(AHandle);
end;

function TWindowManager.SetWindowDesktop(AHandle: TWindowHandle; ADesktop: Integer): Boolean;
begin
  Result := DoSetWindowDesktop(AHandle, ADesktop);
end;

end.
