{ WindowManagerIntf

  Copyright (c) 2025-2026 Tony Stone. 0BSD.

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

  TWindowManager = class
  public
    function GetActiveWindow: TWindowHandle; virtual; abstract;
    // The topmost window at a point on the screen, 0 if there is none.
    function GetWindowAtPoint(X, Y: integer): TWindowHandle; virtual; abstract;
    function GetWindowInfo(AHandle: TWindowHandle; out AInfo: TWindowInfo): boolean; virtual; abstract;
    function GetWindowTitle(AHandle: TWindowHandle): string; virtual; abstract;
    function GetWindowClass(AHandle: TWindowHandle): string; virtual; abstract;
    function GetWindowPID(AHandle: TWindowHandle): longword; virtual; abstract;
    function GetWindowRect(AHandle: TWindowHandle): TRect; virtual; abstract;

    // The first window whose title or class contains the text, any case.
    function FindWindow(const ATitle: string): TWindowHandle;
    function FindWindowByClass(const AClassName: string): TWindowHandle;
    function FindWindowByPID(APID: longword): TWindowHandle;
    // Lists of TWindowHandle; the caller frees them.
    function EnumerateWindows: TList; virtual; abstract;
    function GetWindowChildren(AHandle: TWindowHandle): TList; virtual; abstract;

    function ActivateWindow(AHandle: TWindowHandle): boolean; virtual; abstract;
    function FocusWindow(AHandle: TWindowHandle): boolean; virtual; abstract;
    function RaiseWindow(AHandle: TWindowHandle): boolean; virtual; abstract;

    function MoveWindow(AHandle: TWindowHandle; X, Y: integer): boolean; virtual; abstract;
    function ResizeWindow(AHandle: TWindowHandle; Width, Height: integer): boolean; virtual; abstract;
    function MoveResizeWindow(AHandle: TWindowHandle; X, Y, Width, Height: integer): boolean; virtual; abstract;

    function MinimizeWindow(AHandle: TWindowHandle): boolean; virtual; abstract;
    function MaximizeWindow(AHandle: TWindowHandle): boolean; virtual; abstract;
    function RestoreWindow(AHandle: TWindowHandle): boolean; virtual; abstract;
    function CloseWindow(AHandle: TWindowHandle): boolean; virtual; abstract;

    function ShowWindow(AHandle: TWindowHandle): boolean; virtual; abstract;
    function HideWindow(AHandle: TWindowHandle): boolean; virtual; abstract;
    function IsWindowVisible(AHandle: TWindowHandle): boolean; virtual; abstract;

    // The screen, cut down to the window; 0 takes the whole screen.
    // Whatever covers the window is in the picture.
    function CaptureWindow(AHandle: TWindowHandle; ABitmap: TBitmap): boolean; virtual;
    function CaptureWindowToFile(AHandle: TWindowHandle; const AFileName: string): boolean;

    // Virtual desktops; Windows reports a single one.
    function GetDesktopCount: integer; virtual; abstract;
    function GetCurrentDesktop: integer; virtual; abstract;
    function SetCurrentDesktop(ADesktop: integer): boolean; virtual; abstract;
    function GetWindowDesktop(AHandle: TWindowHandle): integer; virtual; abstract;
    function SetWindowDesktop(AHandle: TWindowHandle; ADesktop: integer): boolean; virtual; abstract;
  end;

implementation

uses LazUTF8;

function TWindowManager.FindWindow(const ATitle: string): TWindowHandle;
var
  Windows: TList;
  Item: Pointer;
begin
  Result := 0;
  Windows := EnumerateWindows;
  try
    for Item in Windows do
      if Pos(UTF8LowerCase(ATitle), UTF8LowerCase(GetWindowTitle(TWindowHandle(Item)))) > 0 then
        Exit(TWindowHandle(Item));
  finally
    Windows.Free;
  end;
end;

function TWindowManager.FindWindowByClass(const AClassName: string): TWindowHandle;
var
  Windows: TList;
  Item: Pointer;
begin
  Result := 0;
  Windows := EnumerateWindows;
  try
    for Item in Windows do
      if Pos(UTF8LowerCase(AClassName), UTF8LowerCase(GetWindowClass(TWindowHandle(Item)))) > 0 then
        Exit(TWindowHandle(Item));
  finally
    Windows.Free;
  end;
end;

function TWindowManager.FindWindowByPID(APID: longword): TWindowHandle;
var
  Windows: TList;
  Item: Pointer;
begin
  Result := 0;
  Windows := EnumerateWindows;
  try
    for Item in Windows do
      if GetWindowPID(TWindowHandle(Item)) = APID then
        Exit(TWindowHandle(Item));
  finally
    Windows.Free;
  end;
end;

{ The screen, cut down to the window.

  Deliberately not asking the window for its own contents.  Under a compositor
  a window's backing store is redirected and what it holds is not always what
  is on the screen; ask the screen and the answer is what a person would see,
  which is the only answer a test or a bug report wants.  The cost is that
  anything sitting on top of the window is captured with it, and a window
  scrolled off the edge is clipped - both true of a photograph as well.

  A platform with something better - XComposite here, PrintWindow on
  Windows - can override this and say so. }
function TWindowManager.CaptureWindow(AHandle: TWindowHandle; ABitmap: TBitmap): boolean;
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

end.
