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
  - Capturing a window as a picture

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
  Classes, SysUtils, Types, Graphics, LCLType, LCLIntf;

type
  TWindowHandle = PtrUInt;  // Platform-agnostic window handle

  TWindowInfo = record
    Handle: TWindowHandle;
    Title: string;
    ClassName: string;
    ProcessID: longword;
    ProcessName: string;
    Rect: TRect;
    IsVisible: boolean;
    IsMinimized: boolean;
    IsMaximized: boolean;
  end;

  { TWindowManager - Abstract base class for window management operations }
  TWindowManager = class
  protected
    // Window query methods
    function DoGetActiveWindow: TWindowHandle; virtual; abstract;
    function DoGetWindowInfo(AHandle: TWindowHandle; out AInfo: TWindowInfo): boolean; virtual; abstract;
    function DoGetWindowTitle(AHandle: TWindowHandle): string; virtual; abstract;
    function DoGetWindowPID(AHandle: TWindowHandle): longword; virtual; abstract;
    function DoGetWindowRect(AHandle: TWindowHandle): TRect; virtual; abstract;

    { Not abstract: the generic way works everywhere the LCL does, so a
      platform only overrides it to do something better. }
    function DoCaptureWindow(AHandle: TWindowHandle; ABitmap: TBitmap): boolean; virtual;

    // Window search methods
    function DoFindWindow(const ATitle: string): TWindowHandle; virtual; abstract;
    function DoFindWindowByClass(const AClassName: string): TWindowHandle; virtual; abstract;
    function DoFindWindowByPID(APID: longword): TWindowHandle; virtual; abstract;
    function DoEnumerateWindows: TList; virtual; abstract;  // Returns list of TWindowHandle
    function DoGetWindowChildren(AHandle: TWindowHandle): TList; virtual;  // Returns list of child window handles

    // Window activation and focus
    function DoActivateWindow(AHandle: TWindowHandle): boolean; virtual; abstract;
    function DoFocusWindow(AHandle: TWindowHandle): boolean; virtual; abstract;
    function DoRaiseWindow(AHandle: TWindowHandle): boolean; virtual; abstract;

    // Window positioning and sizing
    function DoMoveWindow(AHandle: TWindowHandle; X, Y: integer): boolean; virtual; abstract;
    function DoResizeWindow(AHandle: TWindowHandle; Width, Height: integer): boolean; virtual; abstract;
    function DoMoveResizeWindow(AHandle: TWindowHandle; X, Y, Width, Height: integer): boolean; virtual; abstract;

    // Window state management
    function DoMinimizeWindow(AHandle: TWindowHandle): boolean; virtual; abstract;
    function DoMaximizeWindow(AHandle: TWindowHandle): boolean; virtual; abstract;
    function DoRestoreWindow(AHandle: TWindowHandle): boolean; virtual; abstract;
    function DoCloseWindow(AHandle: TWindowHandle): boolean; virtual; abstract;

    // Window visibility
    function DoShowWindow(AHandle: TWindowHandle): boolean; virtual; abstract;
    function DoHideWindow(AHandle: TWindowHandle): boolean; virtual; abstract;
    function DoIsWindowVisible(AHandle: TWindowHandle): boolean; virtual; abstract;

    // Desktop and workspace management (X11/macOS specific)
    function DoGetDesktopCount: integer; virtual; abstract;
    function DoGetCurrentDesktop: integer; virtual; abstract;
    function DoSetCurrentDesktop(ADesktop: integer): boolean; virtual; abstract;
    function DoGetWindowDesktop(AHandle: TWindowHandle): integer; virtual; abstract;
    function DoSetWindowDesktop(AHandle: TWindowHandle; ADesktop: integer): boolean; virtual; abstract;
  public
    // Window query methods
    function GetActiveWindow: TWindowHandle;
    function GetWindowInfo(AHandle: TWindowHandle; out AInfo: TWindowInfo): boolean;
    function GetWindowTitle(AHandle: TWindowHandle): string;
    function GetWindowPID(AHandle: TWindowHandle): longword;
    function GetWindowRect(AHandle: TWindowHandle): TRect;

    // Window search methods
    function FindWindow(const ATitle: string): TWindowHandle;
    function FindWindowByClass(const AClassName: string): TWindowHandle;
    function FindWindowByPID(APID: longword): TWindowHandle;
    function EnumerateWindows: TList;  // Returns list of TWindowHandle - caller must free
    function GetWindowChildren(AHandle: TWindowHandle): TList;  // Returns list of child handles - caller must free

    // Window activation and focus
    function ActivateWindow(AHandle: TWindowHandle): boolean;
    function FocusWindow(AHandle: TWindowHandle): boolean;
    function RaiseWindow(AHandle: TWindowHandle): boolean;

    // Window positioning and sizing
    function MoveWindow(AHandle: TWindowHandle; X, Y: integer): boolean;
    function ResizeWindow(AHandle: TWindowHandle; Width, Height: integer): boolean;
    function MoveResizeWindow(AHandle: TWindowHandle; X, Y, Width, Height: integer): boolean;

    // Window state management
    function MinimizeWindow(AHandle: TWindowHandle): boolean;
    function MaximizeWindow(AHandle: TWindowHandle): boolean;
    function RestoreWindow(AHandle: TWindowHandle): boolean;
    function CloseWindow(AHandle: TWindowHandle): boolean;

    // Window visibility
    function ShowWindow(AHandle: TWindowHandle): boolean;
    function HideWindow(AHandle: TWindowHandle): boolean;
    function IsWindowVisible(AHandle: TWindowHandle): boolean;

    { Capture a window into a bitmap - the picture half of an xdotool
      replacement, and the thing that lets a test show its result rather than
      describe it.  ABitmap is resized to the window and must not be nil.
      Pass 0 for AHandle to take the whole screen. }
    function CaptureWindow(AHandle: TWindowHandle; ABitmap: TBitmap): boolean;
    function CaptureWindowToFile(AHandle: TWindowHandle; const AFileName: string): boolean;

    // Desktop and workspace management
    function GetDesktopCount: integer;
    function GetCurrentDesktop: integer;
    function SetCurrentDesktop(ADesktop: integer): boolean;
    function GetWindowDesktop(AHandle: TWindowHandle): integer;
    function SetWindowDesktop(AHandle: TWindowHandle; ADesktop: integer): boolean;
  end;

implementation

{ TWindowManager }

// Window query methods
function TWindowManager.GetActiveWindow: TWindowHandle;
begin
  Result := DoGetActiveWindow;
end;

function TWindowManager.GetWindowInfo(AHandle: TWindowHandle; out AInfo: TWindowInfo): boolean;
begin
  Result := DoGetWindowInfo(AHandle, AInfo);
end;

function TWindowManager.GetWindowTitle(AHandle: TWindowHandle): string;
begin
  Result := DoGetWindowTitle(AHandle);
end;

function TWindowManager.GetWindowPID(AHandle: TWindowHandle): longword;
begin
  Result := DoGetWindowPID(AHandle);
end;

function TWindowManager.GetWindowRect(AHandle: TWindowHandle): TRect;
begin
  Result := DoGetWindowRect(AHandle);
end;

{ The screen, cut down to the window.

  Deliberately not asking the window for its own contents.  Under a compositor
  a window's backing store is redirected and what it holds is not always what
  is on the screen; ask the screen and the answer is what a person would see,
  which is the only answer a test or a bug report wants.  The cost is that
  anything sitting on top of the window is captured with it, and a window
  scrolled off the edge is clipped - both true of a photograph as well.

  A platform with something better - XComposite here, PrintWindow on
  Windows - can override DoCaptureWindow and say so. }
function TWindowManager.DoCaptureWindow(AHandle: TWindowHandle; ABitmap: TBitmap): boolean;
var
  Screen: TBitmap;
  R: TRect;
  DC: HDC;
begin
  Result := False;
  if ABitmap = nil then Exit;

  Screen := TBitmap.Create;
  try
    DC := GetDC(0);
    if DC = 0 then Exit;
    try
      Screen.LoadFromDevice(DC);
    finally
      ReleaseDC(0, DC);
    end;
    if (Screen.Width <= 0) or (Screen.Height <= 0) then Exit;

    if AHandle = 0 then
      R := Rect(0, 0, Screen.Width, Screen.Height)
    else
      R := GetWindowRect(AHandle);

    { a window half off the screen still gives a picture of the half that
      is on it, rather than nothing at all }
    if R.Left < 0 then R.Left := 0;
    if R.Top < 0 then R.Top := 0;
    if R.Right > Screen.Width then R.Right := Screen.Width;
    if R.Bottom > Screen.Height then R.Bottom := Screen.Height;
    if (R.Right <= R.Left) or (R.Bottom <= R.Top) then Exit;

    ABitmap.SetSize(R.Right - R.Left, R.Bottom - R.Top);
    ABitmap.Canvas.CopyRect(Rect(0, 0, ABitmap.Width, ABitmap.Height),
      Screen.Canvas, R);
    Result := True;
  finally
    Screen.Free;
  end;
end;

function TWindowManager.CaptureWindow(AHandle: TWindowHandle; ABitmap: TBitmap): boolean;
begin
  Result := DoCaptureWindow(AHandle, ABitmap);
end;

{ The extension decides the format, the way every other Save does. }
function TWindowManager.CaptureWindowToFile(AHandle: TWindowHandle; const AFileName: string): boolean;
var
  Bmp: TBitmap;
  Pic: TPicture;
begin
  Result := False;
  Bmp := TBitmap.Create;
  Pic := TPicture.Create;
  try
    if not CaptureWindow(AHandle, Bmp) then Exit;
    try
      Pic.Assign(Bmp);
      Pic.SaveToFile(AFileName);
      Result := True;
    except
      Result := False;
    end;
  finally
    Pic.Free;
    Bmp.Free;
  end;
end;

// Window search methods
function TWindowManager.FindWindow(const ATitle: string): TWindowHandle;
begin
  Result := DoFindWindow(ATitle);
end;

function TWindowManager.FindWindowByClass(const AClassName: string): TWindowHandle;
begin
  Result := DoFindWindowByClass(AClassName);
end;

function TWindowManager.FindWindowByPID(APID: longword): TWindowHandle;
begin
  Result := DoFindWindowByPID(APID);
end;

function TWindowManager.EnumerateWindows: TList;
begin
  Result := DoEnumerateWindows;
end;

function TWindowManager.GetWindowChildren(AHandle: TWindowHandle): TList;
begin
  Result := DoGetWindowChildren(AHandle);
end;

function TWindowManager.DoGetWindowChildren(AHandle: TWindowHandle): TList;
begin
  // Default implementation returns empty list - override in platform-specific classes
  Result := TList.Create;
end;

// Window activation and focus
function TWindowManager.ActivateWindow(AHandle: TWindowHandle): boolean;
begin
  Result := DoActivateWindow(AHandle);
end;

function TWindowManager.FocusWindow(AHandle: TWindowHandle): boolean;
begin
  Result := DoFocusWindow(AHandle);
end;

function TWindowManager.RaiseWindow(AHandle: TWindowHandle): boolean;
begin
  Result := DoRaiseWindow(AHandle);
end;

// Window positioning and sizing
function TWindowManager.MoveWindow(AHandle: TWindowHandle; X, Y: integer): boolean;
begin
  Result := DoMoveWindow(AHandle, X, Y);
end;

function TWindowManager.ResizeWindow(AHandle: TWindowHandle; Width, Height: integer): boolean;
begin
  Result := DoResizeWindow(AHandle, Width, Height);
end;

function TWindowManager.MoveResizeWindow(AHandle: TWindowHandle; X, Y, Width, Height: integer): boolean;
begin
  Result := DoMoveResizeWindow(AHandle, X, Y, Width, Height);
end;

// Window state management
function TWindowManager.MinimizeWindow(AHandle: TWindowHandle): boolean;
begin
  Result := DoMinimizeWindow(AHandle);
end;

function TWindowManager.MaximizeWindow(AHandle: TWindowHandle): boolean;
begin
  Result := DoMaximizeWindow(AHandle);
end;

function TWindowManager.RestoreWindow(AHandle: TWindowHandle): boolean;
begin
  Result := DoRestoreWindow(AHandle);
end;

function TWindowManager.CloseWindow(AHandle: TWindowHandle): boolean;
begin
  Result := DoCloseWindow(AHandle);
end;

// Window visibility
function TWindowManager.ShowWindow(AHandle: TWindowHandle): boolean;
begin
  Result := DoShowWindow(AHandle);
end;

function TWindowManager.HideWindow(AHandle: TWindowHandle): boolean;
begin
  Result := DoHideWindow(AHandle);
end;

function TWindowManager.IsWindowVisible(AHandle: TWindowHandle): boolean;
begin
  Result := DoIsWindowVisible(AHandle);
end;

// Desktop and workspace management
function TWindowManager.GetDesktopCount: integer;
begin
  Result := DoGetDesktopCount;
end;

function TWindowManager.GetCurrentDesktop: integer;
begin
  Result := DoGetCurrentDesktop;
end;

function TWindowManager.SetCurrentDesktop(ADesktop: integer): boolean;
begin
  Result := DoSetCurrentDesktop(ADesktop);
end;

function TWindowManager.GetWindowDesktop(AHandle: TWindowHandle): integer;
begin
  Result := DoGetWindowDesktop(AHandle);
end;

function TWindowManager.SetWindowDesktop(AHandle: TWindowHandle; ADesktop: integer): boolean;
begin
  Result := DoSetWindowDesktop(AHandle, ADesktop);
end;

end.
