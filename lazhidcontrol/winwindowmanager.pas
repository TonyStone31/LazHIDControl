{ WinWindowManager

  Copyright (c) 2025-2026 Tony Stone. 0BSD.

  Windows implementation of window management providing xdotool-like functionality.
  Uses Win32 API for window operations.
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
  public
    function GetActiveWindow: TWindowHandle; override;
    function GetWindowInfo(AHandle: TWindowHandle; out AInfo: TWindowInfo): boolean; override;
    function GetWindowTitle(AHandle: TWindowHandle): string; override;
    function GetWindowClass(AHandle: TWindowHandle): string; override;
    function GetWindowPID(AHandle: TWindowHandle): longword; override;
    function GetWindowRect(AHandle: TWindowHandle): TRect; override;
    function EnumerateWindows: TList; override;
    function GetWindowChildren(AHandle: TWindowHandle): TList; override;

    function ActivateWindow(AHandle: TWindowHandle): boolean; override;
    function FocusWindow(AHandle: TWindowHandle): boolean; override;
    function RaiseWindow(AHandle: TWindowHandle): boolean; override;

    function MoveWindow(AHandle: TWindowHandle; X, Y: integer): boolean; override;
    function ResizeWindow(AHandle: TWindowHandle; Width, Height: integer): boolean; override;
    function MoveResizeWindow(AHandle: TWindowHandle; X, Y, Width, Height: integer): boolean; override;

    function MinimizeWindow(AHandle: TWindowHandle): boolean; override;
    function MaximizeWindow(AHandle: TWindowHandle): boolean; override;
    function RestoreWindow(AHandle: TWindowHandle): boolean; override;
    function CloseWindow(AHandle: TWindowHandle): boolean; override;

    function ShowWindow(AHandle: TWindowHandle): boolean; override;
    function HideWindow(AHandle: TWindowHandle): boolean; override;
    function IsWindowVisible(AHandle: TWindowHandle): boolean; override;

    function GetDesktopCount: integer; override;
    function GetCurrentDesktop: integer; override;
    function SetCurrentDesktop(ADesktop: integer): boolean; override;
    function GetWindowDesktop(AHandle: TWindowHandle): integer; override;
    function SetWindowDesktop(AHandle: TWindowHandle; ADesktop: integer): boolean; override;

    // Windows only
    function GetWindowAtPoint(X, Y: integer): TWindowHandle; override;
    function GetWindowParent(AHandle: TWindowHandle): TWindowHandle;
    function GetDesktopWindow: TWindowHandle;
  end;

function InitializeWindowManager: TWindowManager;

implementation

function QueryFullProcessImageNameW(Process: THandle; Flags: DWORD; ExeName: pwidechar; var Size: DWORD): BOOL; stdcall;
  external 'kernel32.dll' name 'QueryFullProcessImageNameW';

function AddWindow(hWnd: HWND; lParam: LPARAM): longbool; stdcall;
begin
  TList(lParam).Add(Pointer(hWnd));
  Result := True;
end;

function AddVisibleWindow(hWnd: HWND; lParam: LPARAM): longbool; stdcall;
begin
  if Windows.IsWindowVisible(hWnd) then
    TList(lParam).Add(Pointer(hWnd));
  Result := True;
end;

function InitializeWindowManager: TWindowManager;
begin
  Result := TWinWindowManager.Create;
end;

{ TWinWindowManager }

function TWinWindowManager.GetProcessNameFromPID(APID: DWORD): string;
const
  PROCESS_QUERY_LIMITED_INFORMATION = $1000;
var
  Process: THandle;
  Name: array[0..MAX_PATH - 1] of widechar;
  Size: DWORD;
begin
  Result := '';
  Process := OpenProcess(PROCESS_QUERY_LIMITED_INFORMATION, False, APID);
  if Process = 0 then Exit;
  Size := MAX_PATH;
  if QueryFullProcessImageNameW(Process, 0, Name, Size) then
    Result := ExtractFileName(UTF8Encode(WideString(Name)));
  CloseHandle(Process);
end;

function TWinWindowManager.GetActiveWindow: TWindowHandle;
begin
  Result := TWindowHandle(GetForegroundWindow);
end;

function TWinWindowManager.GetWindowInfo(AHandle: TWindowHandle; out AInfo: TWindowInfo): boolean;
var
  Placement: TWindowPlacement;
begin
  AInfo := Default(TWindowInfo);
  AInfo.Handle := AHandle;
  Result := IsWindow(HWND(AHandle));
  if not Result then Exit;

  AInfo.Title := GetWindowTitle(AHandle);
  AInfo.ClassName := GetWindowClass(AHandle);
  AInfo.ProcessID := GetWindowPID(AHandle);
  AInfo.ProcessName := GetProcessNameFromPID(AInfo.ProcessID);
  AInfo.Rect := GetWindowRect(AHandle);
  AInfo.IsVisible := IsWindowVisible(AHandle);

  Placement := Default(TWindowPlacement);
  Placement.length := SizeOf(Placement);
  if GetWindowPlacement(HWND(AHandle), Placement) then
  begin
    AInfo.IsMinimized := Placement.showCmd = SW_SHOWMINIMIZED;
    AInfo.IsMaximized := Placement.showCmd = SW_SHOWMAXIMIZED;
  end;
end;

function TWinWindowManager.GetWindowTitle(AHandle: TWindowHandle): string;
var
  Buffer: array[0..1023] of widechar;
begin
  Result := '';
  if GetWindowTextW(HWND(AHandle), Buffer, Length(Buffer)) > 0 then
    Result := UTF8Encode(WideString(Buffer));
end;

function TWinWindowManager.GetWindowClass(AHandle: TWindowHandle): string;
var
  Buffer: array[0..255] of widechar;
begin
  Result := '';
  if GetClassNameW(HWND(AHandle), Buffer, Length(Buffer)) > 0 then
    Result := UTF8Encode(WideString(Buffer));
end;

function TWinWindowManager.GetWindowPID(AHandle: TWindowHandle): longword;
begin
  Result := 0;
  GetWindowThreadProcessId(HWND(AHandle), @Result);
end;

function TWinWindowManager.GetWindowRect(AHandle: TWindowHandle): TRect;
begin
  Result := Default(TRect);
  Windows.GetWindowRect(HWND(AHandle), Result);
end;

// Visible top-level windows only.
function TWinWindowManager.EnumerateWindows: TList;
begin
  Result := TList.Create;
  EnumWindows(@AddVisibleWindow, LPARAM(Result));
end;

function TWinWindowManager.GetWindowChildren(AHandle: TWindowHandle): TList;
begin
  Result := TList.Create;
  EnumChildWindows(HWND(AHandle), @AddWindow, LPARAM(Result));
end;

function TWinWindowManager.ActivateWindow(AHandle: TWindowHandle): boolean;
begin
  // a minimized window comes back up, as it does on X11
  if IsIconic(HWND(AHandle)) then
    Windows.ShowWindow(HWND(AHandle), SW_RESTORE);
  Result := SetForegroundWindow(HWND(AHandle));
end;

function TWinWindowManager.FocusWindow(AHandle: TWindowHandle): boolean;
begin
  Result := SetFocus(HWND(AHandle)) <> 0;
end;

function TWinWindowManager.RaiseWindow(AHandle: TWindowHandle): boolean;
begin
  Result := BringWindowToTop(HWND(AHandle));
end;

function TWinWindowManager.MoveWindow(AHandle: TWindowHandle; X, Y: integer): boolean;
begin
  Result := SetWindowPos(HWND(AHandle), 0, X, Y, 0, 0, SWP_NOSIZE or SWP_NOZORDER or SWP_NOACTIVATE);
end;

function TWinWindowManager.ResizeWindow(AHandle: TWindowHandle; Width, Height: integer): boolean;
begin
  Result := SetWindowPos(HWND(AHandle), 0, 0, 0, Width, Height, SWP_NOMOVE or SWP_NOZORDER or SWP_NOACTIVATE);
end;

function TWinWindowManager.MoveResizeWindow(AHandle: TWindowHandle; X, Y, Width, Height: integer): boolean;
begin
  Result := Windows.MoveWindow(HWND(AHandle), X, Y, Width, Height, True);
end;

function TWinWindowManager.MinimizeWindow(AHandle: TWindowHandle): boolean;
begin
  Result := Windows.ShowWindow(HWND(AHandle), SW_MINIMIZE);
end;

function TWinWindowManager.MaximizeWindow(AHandle: TWindowHandle): boolean;
begin
  Result := Windows.ShowWindow(HWND(AHandle), SW_MAXIMIZE);
end;

function TWinWindowManager.RestoreWindow(AHandle: TWindowHandle): boolean;
begin
  Result := Windows.ShowWindow(HWND(AHandle), SW_RESTORE);
end;

function TWinWindowManager.CloseWindow(AHandle: TWindowHandle): boolean;
begin
  Result := PostMessage(HWND(AHandle), WM_CLOSE, 0, 0);
end;

function TWinWindowManager.ShowWindow(AHandle: TWindowHandle): boolean;
begin
  Result := Windows.ShowWindow(HWND(AHandle), SW_SHOW);
end;

function TWinWindowManager.HideWindow(AHandle: TWindowHandle): boolean;
begin
  Result := Windows.ShowWindow(HWND(AHandle), SW_HIDE);
end;

function TWinWindowManager.IsWindowVisible(AHandle: TWindowHandle): boolean;
begin
  Result := Windows.IsWindowVisible(HWND(AHandle));
end;

// Windows virtual desktops sit behind an undocumented COM interface; one desktop is reported.

function TWinWindowManager.GetDesktopCount: integer;
begin
  Result := 1;
end;

function TWinWindowManager.GetCurrentDesktop: integer;
begin
  Result := 0;
end;

function TWinWindowManager.SetCurrentDesktop(ADesktop: integer): boolean;
begin
  Result := False;
end;

function TWinWindowManager.GetWindowDesktop(AHandle: TWindowHandle): integer;
begin
  Result := 0;
end;

function TWinWindowManager.SetWindowDesktop(AHandle: TWindowHandle; ADesktop: integer): boolean;
begin
  Result := False;
end;

function TWinWindowManager.GetWindowAtPoint(X, Y: integer): TWindowHandle;
begin
  Result := TWindowHandle(WindowFromPoint(Types.Point(X, Y)));
end;

function TWinWindowManager.GetWindowParent(AHandle: TWindowHandle): TWindowHandle;
begin
  Result := TWindowHandle(Windows.GetParent(HWND(AHandle)));
end;

function TWinWindowManager.GetDesktopWindow: TWindowHandle;
begin
  Result := TWindowHandle(Windows.GetDesktopWindow);
end;

end.
