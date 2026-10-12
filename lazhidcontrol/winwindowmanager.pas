{ WinWindowManager

  Copyright (C) 2025 LazHIDControl Contributors

  Windows implementation of window management providing xdotool-like functionality.
  Uses Win32 API for window operations.

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
unit WinWindowManager;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Types, Windows,
  WindowManagerIntf;

type
  { TWinWindowManager }
  TWinWindowManager = class(TWindowManager)
  private
    function GetProcessNameFromPID(APID: DWORD): string;
  protected
    // Window query methods
    function DoGetActiveWindow: TWindowHandle; override;
    function DoGetWindowInfo(AHandle: TWindowHandle; out AInfo: TWindowInfo): boolean; override;
    function DoGetWindowTitle(AHandle: TWindowHandle): string; override;
    function DoGetWindowPID(AHandle: TWindowHandle): longword; override;
    function DoGetWindowRect(AHandle: TWindowHandle): TRect; override;
    function DoGetWindowClass(AHandle: TWindowHandle): string;

    // Window search methods
    function DoFindWindow(const ATitle: string): TWindowHandle; override;
    function DoFindWindowByClass(const AClassName: string): TWindowHandle; override;
    function DoFindWindowByPID(APID: longword): TWindowHandle; override;
    function DoEnumerateWindows: TList; override;
    function DoGetWindowChildren(AHandle: TWindowHandle): TList; override;

    // Window activation and focus
    function DoActivateWindow(AHandle: TWindowHandle): boolean; override;
    function DoFocusWindow(AHandle: TWindowHandle): boolean; override;
    function DoRaiseWindow(AHandle: TWindowHandle): boolean; override;

    // Window positioning and sizing
    function DoMoveWindow(AHandle: TWindowHandle; X, Y: integer): boolean; override;
    function DoResizeWindow(AHandle: TWindowHandle; Width, Height: integer): boolean; override;
    function DoMoveResizeWindow(AHandle: TWindowHandle; X, Y, Width, Height: integer): boolean; override;

    // Window state management
    function DoMinimizeWindow(AHandle: TWindowHandle): boolean; override;
    function DoMaximizeWindow(AHandle: TWindowHandle): boolean; override;
    function DoRestoreWindow(AHandle: TWindowHandle): boolean; override;
    function DoCloseWindow(AHandle: TWindowHandle): boolean; override;

    // Window visibility
    function DoShowWindow(AHandle: TWindowHandle): boolean; override;
    function DoHideWindow(AHandle: TWindowHandle): boolean; override;
    function DoIsWindowVisible(AHandle: TWindowHandle): boolean; override;

    // Desktop and workspace management (limited on Windows)
    function DoGetDesktopCount: integer; override;
    function DoGetCurrentDesktop: integer; override;
    function DoSetCurrentDesktop(ADesktop: integer): boolean; override;
    function DoGetWindowDesktop(AHandle: TWindowHandle): integer; override;
    function DoSetWindowDesktop(AHandle: TWindowHandle; ADesktop: integer): boolean; override;
  public
    constructor Create;

    // Windows-specific methods
    function GetWindowAtPoint(X, Y: integer): TWindowHandle;
    function GetWindowChildren(AHandle: TWindowHandle): TList;
    function GetWindowParent(AHandle: TWindowHandle): TWindowHandle;
    function GetWindowClass(AHandle: TWindowHandle): string;
    function GetDesktopWindow: TWindowHandle;
  end;

function InitializeWindowManager: TWindowManager;

implementation

var
  EnumWindowsList: TList;
  EnumChildrenList: TList;
  FindByPIDTarget: DWORD;
  FindByPIDResult: HWND;

function EnumWindowsCallback(hWnd: HWND; lParam: LPARAM): longbool; stdcall;
begin
  // Only add visible top-level windows
  if IsWindowVisible(hWnd) then
    EnumWindowsList.Add(Pointer(hWnd));
  Result := True;  // Continue enumeration
end;

function EnumChildCallback(hWnd: HWND; lParam: LPARAM): longbool; stdcall;
begin
  EnumChildrenList.Add(Pointer(hWnd));
  Result := True;
end;

function FindByPIDCallback(hWnd: HWND; lParam: LPARAM): longbool; stdcall;
var
  ProcessID: DWORD;
begin
  ProcessID := 0;
  GetWindowThreadProcessId(hWnd, @ProcessID);
  if ProcessID = FindByPIDTarget then
  begin
    FindByPIDResult := hWnd;
    Result := False;  // Stop enumeration
  end
  else
    Result := True;
end;

function InitializeWindowManager: TWindowManager;
begin
  try
    Result := TWinWindowManager.Create;
  except
    Result := nil;
  end;
end;

{ TWinWindowManager }

constructor TWinWindowManager.Create;
begin
  inherited Create;
end;

function TWinWindowManager.GetProcessNameFromPID(APID: DWORD): string;
const
  PROCESS_QUERY_LIMITED_INFORMATION = $1000;
var
  hProcess: THandle;
  ModuleName: array[0..MAX_PATH - 1] of widechar;
  Size: DWORD;
  QueryFullProcessImageNameW: function(hProcess: THandle; dwFlags: DWORD; lpExeName: pwidechar; var lpdwSize: DWORD): BOOL; stdcall;
  hKernel32: THandle;
begin
  Result := '';
  if APID = 0 then Exit;

  // Try to get process name using QueryFullProcessImageNameW (Vista+)
  hKernel32 := GetModuleHandle('kernel32.dll');
  if hKernel32 <> 0 then
  begin
    Pointer(QueryFullProcessImageNameW) := GetProcAddress(hKernel32, 'QueryFullProcessImageNameW');
    if Assigned(QueryFullProcessImageNameW) then
    begin
      hProcess := OpenProcess(PROCESS_QUERY_LIMITED_INFORMATION, False, APID);
      if hProcess <> 0 then
      begin
        try
          Size := MAX_PATH;
          if QueryFullProcessImageNameW(hProcess, 0, ModuleName, Size) then
            Result := ExtractFileName(UTF8Encode(WideString(ModuleName)));
        finally
          CloseHandle(hProcess);
        end;
      end;
    end;
  end;
end;

// Window query methods

function TWinWindowManager.DoGetActiveWindow: TWindowHandle;
begin
  Result := TWindowHandle(GetForegroundWindow);
end;

function TWinWindowManager.DoGetWindowInfo(AHandle: TWindowHandle; out AInfo: TWindowInfo): boolean;
var
  WndRect: Windows.TRect;
  Placement: TWindowPlacement;
begin
  FillByte(AInfo, SizeOf(AInfo), 0);
  AInfo.Handle := AHandle;

  Result := IsWindow(HWND(AHandle));
  if not Result then Exit;

  AInfo.Title := DoGetWindowTitle(AHandle);
  AInfo.ClassName := DoGetWindowClass(AHandle);
  AInfo.ProcessID := DoGetWindowPID(AHandle);
  AInfo.ProcessName := GetProcessNameFromPID(AInfo.ProcessID);

  WndRect := Default(Windows.TRect);
  if Windows.GetWindowRect(HWND(AHandle), WndRect) then
  begin
    AInfo.Rect.Left := WndRect.Left;
    AInfo.Rect.Top := WndRect.Top;
    AInfo.Rect.Right := WndRect.Right;
    AInfo.Rect.Bottom := WndRect.Bottom;
  end;

  AInfo.IsVisible := Windows.IsWindowVisible(HWND(AHandle));

  FillByte(Placement, SizeOf(Placement), 0);
  Placement.length := SizeOf(TWindowPlacement);
  if Windows.GetWindowPlacement(HWND(AHandle), Placement) then
  begin
    AInfo.IsMinimized := Placement.showCmd = SW_SHOWMINIMIZED;
    AInfo.IsMaximized := Placement.showCmd = SW_SHOWMAXIMIZED;
  end;
end;

function TWinWindowManager.DoGetWindowTitle(AHandle: TWindowHandle): string;
var
  Len: integer;
  Buffer: array[0..1023] of char;
begin
  Result := '';
  Len := GetWindowText(HWND(AHandle), Buffer, SizeOf(Buffer));
  if Len > 0 then
    Result := StrPas(Buffer);
end;

function TWinWindowManager.DoGetWindowPID(AHandle: TWindowHandle): longword;
var
  ProcessID: DWORD;
begin
  ProcessID := 0;
  GetWindowThreadProcessId(HWND(AHandle), @ProcessID);
  Result := ProcessID;
end;

function TWinWindowManager.DoGetWindowRect(AHandle: TWindowHandle): TRect;
var
  R: Windows.TRect;
begin
  R := Default(Windows.TRect);
  Windows.GetWindowRect(HWND(AHandle), R);
  Result.Left := R.Left;
  Result.Top := R.Top;
  Result.Right := R.Right;
  Result.Bottom := R.Bottom;
end;

function TWinWindowManager.DoGetWindowClass(AHandle: TWindowHandle): string;
var
  Buffer: array[0..255] of char;
begin
  Result := '';
  if GetClassName(HWND(AHandle), Buffer, SizeOf(Buffer)) > 0 then
    Result := StrPas(Buffer);
end;

// Window search methods

function TWinWindowManager.DoFindWindow(const ATitle: string): TWindowHandle;
begin
  Result := TWindowHandle(Windows.FindWindow(nil, PChar(ATitle)));
end;

function TWinWindowManager.DoFindWindowByClass(const AClassName: string): TWindowHandle;
begin
  Result := TWindowHandle(Windows.FindWindow(PChar(AClassName), nil));
end;

function TWinWindowManager.DoFindWindowByPID(APID: longword): TWindowHandle;
begin
  FindByPIDTarget := APID;
  FindByPIDResult := 0;
  EnumWindows(@FindByPIDCallback, 0);
  Result := TWindowHandle(FindByPIDResult);
end;

function TWinWindowManager.DoEnumerateWindows: TList;
begin
  EnumWindowsList := TList.Create;
  EnumWindows(@EnumWindowsCallback, 0);
  Result := EnumWindowsList;
end;

function TWinWindowManager.DoGetWindowChildren(AHandle: TWindowHandle): TList;
begin
  Result := GetWindowChildren(AHandle);
end;

// Window activation and focus

function TWinWindowManager.DoActivateWindow(AHandle: TWindowHandle): boolean;
begin
  Result := SetForegroundWindow(HWND(AHandle));
end;

function TWinWindowManager.DoFocusWindow(AHandle: TWindowHandle): boolean;
begin
  Result := SetFocus(HWND(AHandle)) <> 0;
end;

function TWinWindowManager.DoRaiseWindow(AHandle: TWindowHandle): boolean;
begin
  Result := BringWindowToTop(HWND(AHandle));
end;

// Window positioning and sizing

function TWinWindowManager.DoMoveWindow(AHandle: TWindowHandle; X, Y: integer): boolean;
var
  R: Windows.TRect;
begin
  R := Default(Windows.TRect);
  if Windows.GetWindowRect(HWND(AHandle), R) then
    Result := Windows.MoveWindow(HWND(AHandle), X, Y, R.Right - R.Left, R.Bottom - R.Top, True)
  else
    Result := False;
end;

function TWinWindowManager.DoResizeWindow(AHandle: TWindowHandle; Width, Height: integer): boolean;
var
  R: Windows.TRect;
begin
  R := Default(Windows.TRect);
  if Windows.GetWindowRect(HWND(AHandle), R) then
    Result := Windows.MoveWindow(HWND(AHandle), R.Left, R.Top, Width, Height, True)
  else
    Result := False;
end;

function TWinWindowManager.DoMoveResizeWindow(AHandle: TWindowHandle; X, Y, Width, Height: integer): boolean;
begin
  Result := Windows.MoveWindow(HWND(AHandle), X, Y, Width, Height, True);
end;

// Window state management

function TWinWindowManager.DoMinimizeWindow(AHandle: TWindowHandle): boolean;
begin
  Result := Windows.ShowWindow(HWND(AHandle), SW_MINIMIZE);
end;

function TWinWindowManager.DoMaximizeWindow(AHandle: TWindowHandle): boolean;
begin
  Result := Windows.ShowWindow(HWND(AHandle), SW_MAXIMIZE);
end;

function TWinWindowManager.DoRestoreWindow(AHandle: TWindowHandle): boolean;
begin
  Result := Windows.ShowWindow(HWND(AHandle), SW_RESTORE);
end;

function TWinWindowManager.DoCloseWindow(AHandle: TWindowHandle): boolean;
begin
  Result := PostMessage(HWND(AHandle), WM_CLOSE, 0, 0);
end;

// Window visibility

function TWinWindowManager.DoShowWindow(AHandle: TWindowHandle): boolean;
begin
  Result := Windows.ShowWindow(HWND(AHandle), SW_SHOW);
end;

function TWinWindowManager.DoHideWindow(AHandle: TWindowHandle): boolean;
begin
  Result := Windows.ShowWindow(HWND(AHandle), SW_HIDE);
end;

function TWinWindowManager.DoIsWindowVisible(AHandle: TWindowHandle): boolean;
begin
  Result := IsWindowVisible(HWND(AHandle));
end;

// Desktop and workspace management (limited on Windows - virtual desktops API is complex)

function TWinWindowManager.DoGetDesktopCount: integer;
begin
  // Windows 10+ has virtual desktops but the API is complex (COM-based)
  // For now, return 1
  Result := 1;
end;

function TWinWindowManager.DoGetCurrentDesktop: integer;
begin
  Result := 0;
end;

function TWinWindowManager.DoSetCurrentDesktop(ADesktop: integer): boolean;
begin
  Result := False;  // Not implemented for basic Windows API
end;

function TWinWindowManager.DoGetWindowDesktop(AHandle: TWindowHandle): integer;
begin
  Result := 0;
end;

function TWinWindowManager.DoSetWindowDesktop(AHandle: TWindowHandle; ADesktop: integer): boolean;
begin
  Result := False;
end;

// Windows-specific methods

function TWinWindowManager.GetWindowAtPoint(X, Y: integer): TWindowHandle;
var
  Pt: TPoint;
begin
  Pt.X := X;
  Pt.Y := Y;
  Result := TWindowHandle(WindowFromPoint(Pt));
end;

function TWinWindowManager.GetWindowChildren(AHandle: TWindowHandle): TList;
begin
  EnumChildrenList := TList.Create;
  EnumChildWindows(HWND(AHandle), @EnumChildCallback, 0);
  Result := EnumChildrenList;
end;

function TWinWindowManager.GetWindowParent(AHandle: TWindowHandle): TWindowHandle;
begin
  Result := TWindowHandle(Windows.GetParent(HWND(AHandle)));
end;

function TWinWindowManager.GetWindowClass(AHandle: TWindowHandle): string;
begin
  Result := DoGetWindowClass(AHandle);
end;

function TWinWindowManager.GetDesktopWindow: TWindowHandle;
begin
  Result := TWindowHandle(Windows.GetDesktopWindow);
end;

end.
