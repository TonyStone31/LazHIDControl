{ XWindowManager

  Copyright (C) 2025 LazHIDControl Contributors

  X11 implementation of window management providing xdotool-like functionality.
  Uses EWMH (Extended Window Manager Hints) and ICCCM protocols for window operations.

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
unit XWindowManager;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Types, ctypes, X, XLib, XUtil, XAtom,
  WindowManagerIntf;

type
  { TXWindowManager }
  TXWindowManager = class(TWindowManager)
  private
    XDisplayHandle: PDisplay;
    RootWindowHandle: TWindow;
    ScreenNumber: cint;

    // EWMH atoms
    AtomNetActiveWindow: TAtom;
    AtomNetClientList: TAtom;
    AtomNetCurrentDesktop: TAtom;
    AtomNetNumberOfDesktops: TAtom;
    AtomNetWMDesktop: TAtom;
    AtomNetWMName: TAtom;
    AtomNetWMPID: TAtom;
    AtomNetWMState: TAtom;
    AtomNetWMStateHidden: TAtom;
    AtomNetWMStateMaximizedVert: TAtom;
    AtomNetWMStateMaximizedHorz: TAtom;
    AtomNetCloseWindow: TAtom;
    AtomNetMoveResizeWindow: TAtom;
    AtomNetSupportingWMCheck: TAtom;

    // ICCCM atoms
    AtomWMName: TAtom;

    procedure InitializeAtoms;
    function SendClientMessage(AWindow: TWindow; AAtom: TAtom; Data0: clong = 0; Data1: clong = 0; Data2: clong = 0; Data3: clong = 0; Data4: clong = 0): boolean;
    function GetWindowProperty(AWindow: TWindow; AProperty: TAtom; out AData: Pointer; out ACount: culong): boolean;
    function GetWindowPropertyString(AWindow: TWindow; AProperty: TAtom): string;
    function GetWindowPropertyCardinal(AWindow: TWindow; AProperty: TAtom): longword;
    function GetProcessNameFromPID(APID: longword): string;
    function HasWMState(AWindow: TWindow; AState: TAtom): boolean;
    function EwmhManaged: boolean;
  public
    constructor Create;
    destructor Destroy; override;

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

    // X11 only
    function GetWindowAtPoint(X, Y: integer): TWindowHandle; override;
    function GetWindowParent(AHandle: TWindowHandle): TWindowHandle;
    function GetRootWindow: TWindowHandle;

    property Display: PDisplay read XDisplayHandle;
  end;

function InitializeWindowManager: TWindowManager;

implementation

const
  _NET_WM_STATE_REMOVE = 0;
  _NET_WM_STATE_ADD = 1;

function InitializeWindowManager: TWindowManager;
begin
  try
    Result := TXWindowManager.Create;
  except
    Result := nil;
  end;
end;

{ TXWindowManager }

constructor TXWindowManager.Create;
begin
  inherited Create;

  XDisplayHandle := XOpenDisplay(nil);
  if XDisplayHandle = nil then
    raise Exception.Create('Failed to open X display');

  ScreenNumber := XDefaultScreen(XDisplayHandle);
  RootWindowHandle := XRootWindow(XDisplayHandle, ScreenNumber);

  InitializeAtoms;
end;

destructor TXWindowManager.Destroy;
begin
  if XDisplayHandle <> nil then
  begin
    XCloseDisplay(XDisplayHandle);
    XDisplayHandle := nil;
  end;

  inherited Destroy;
end;

procedure TXWindowManager.InitializeAtoms;
begin
  // EWMH atoms
  AtomNetActiveWindow := XInternAtom(XDisplayHandle, '_NET_ACTIVE_WINDOW', False);
  AtomNetMoveResizeWindow := XInternAtom(XDisplayHandle, '_NET_MOVERESIZE_WINDOW', False);
  AtomNetSupportingWMCheck := XInternAtom(XDisplayHandle, '_NET_SUPPORTING_WM_CHECK', False);
  AtomNetClientList := XInternAtom(XDisplayHandle, '_NET_CLIENT_LIST', False);
  AtomNetCurrentDesktop := XInternAtom(XDisplayHandle, '_NET_CURRENT_DESKTOP', False);
  AtomNetNumberOfDesktops := XInternAtom(XDisplayHandle, '_NET_NUMBER_OF_DESKTOPS', False);
  AtomNetWMDesktop := XInternAtom(XDisplayHandle, '_NET_WM_DESKTOP', False);
  AtomNetWMName := XInternAtom(XDisplayHandle, '_NET_WM_NAME', False);
  AtomNetWMPID := XInternAtom(XDisplayHandle, '_NET_WM_PID', False);
  AtomNetWMState := XInternAtom(XDisplayHandle, '_NET_WM_STATE', False);
  AtomNetWMStateHidden := XInternAtom(XDisplayHandle, '_NET_WM_STATE_HIDDEN', False);
  AtomNetWMStateMaximizedVert := XInternAtom(XDisplayHandle, '_NET_WM_STATE_MAXIMIZED_VERT', False);
  AtomNetWMStateMaximizedHorz := XInternAtom(XDisplayHandle, '_NET_WM_STATE_MAXIMIZED_HORZ', False);
  AtomNetCloseWindow := XInternAtom(XDisplayHandle, '_NET_CLOSE_WINDOW', False);

  // ICCCM atoms
  AtomWMName := XInternAtom(XDisplayHandle, 'WM_NAME', False);
end;

function TXWindowManager.SendClientMessage(AWindow: TWindow; AAtom: TAtom; Data0: clong; Data1: clong; Data2: clong; Data3: clong; Data4: clong): boolean;
var
  Event: TXEvent;
begin
  FillByte(Event, SizeOf(Event), 0);
  Event._type := ClientMessage;
  Event.xclient.window := AWindow;
  Event.xclient.message_type := AAtom;
  Event.xclient.format := 32;
  Event.xclient.Data.l[0] := Data0;
  Event.xclient.Data.l[1] := Data1;
  Event.xclient.Data.l[2] := Data2;
  Event.xclient.Data.l[3] := Data3;
  Event.xclient.Data.l[4] := Data4;

  Result := XSendEvent(XDisplayHandle, RootWindowHandle, False, SubstructureNotifyMask or SubstructureRedirectMask, @Event) <> 0;
  XFlush(XDisplayHandle);
end;

function TXWindowManager.GetWindowProperty(AWindow: TWindow; AProperty: TAtom; out AData: Pointer; out ACount: culong): boolean;
var
  ActualType: TAtom;
  ActualFormat: cint;
  BytesAfter: culong;
begin
  AData := nil;
  ACount := 0;
  Result := XGetWindowProperty(XDisplayHandle, AWindow, AProperty, 0, $7FFFFFFF, False, AnyPropertyType, @ActualType, @ActualFormat, @ACount, @BytesAfter, @AData) = Success;
  Result := Result and (AData <> nil) and (ACount > 0);
end;

function TXWindowManager.GetWindowPropertyString(AWindow: TWindow; AProperty: TAtom): string;
var
  Data: pchar;
  Count: culong;
begin
  Result := '';
  if GetWindowProperty(AWindow, AProperty, Pointer(Data), Count) then
  begin
    if Count > 0 then
      Result := StrPas(Data);
    XFree(Data);
  end;
end;

function TXWindowManager.GetWindowPropertyCardinal(AWindow: TWindow; AProperty: TAtom): longword;
var
  Data: PLongWord;
  Count: culong;
begin
  Result := 0;
  if GetWindowProperty(AWindow, AProperty, Pointer(Data), Count) then
  begin
    if Count > 0 then
      Result := Data^;
    XFree(Data);
  end;
end;

function TXWindowManager.GetProcessNameFromPID(APID: longword): string;
var
  ProcFile: string;
  F: TextFile;
begin
  Result := '';
  if APID = 0 then Exit;

  ProcFile := Format('/proc/%d/comm', [APID]);
  if FileExists(ProcFile) then
  begin
    try
      AssignFile(F, ProcFile);
      Reset(F);
      if not EOF(F) then
        ReadLn(F, Result);
      CloseFile(F);
    except
      Result := '';
    end;
  end;
end;

function TXWindowManager.HasWMState(AWindow: TWindow; AState: TAtom): boolean;
var
  Data: PAtom;
  Count: culong;
  i: integer;
begin
  Result := False;
  if GetWindowProperty(AWindow, AtomNetWMState, Pointer(Data), Count) then
  begin
    for i := 0 to integer(Count) - 1 do
    begin
      if PAtom(pbyte(Data) + i * SizeOf(TAtom))^ = AState then
      begin
        Result := True;
        Break;
      end;
    end;
    XFree(Data);
  end;
end;

// Window query methods

function TXWindowManager.GetActiveWindow: TWindowHandle;
var
  Data: PWindow;
  Count: culong;
begin
  Result := 0;
  if GetWindowProperty(RootWindowHandle, AtomNetActiveWindow, Pointer(Data), Count) then
  begin
    if Count > 0 then
      Result := TWindowHandle(Data^);
    XFree(Data);
  end;
end;

function TXWindowManager.GetWindowInfo(AHandle: TWindowHandle; out AInfo: TWindowInfo): boolean;
var
  Attrs: TXWindowAttributes;
begin
  FillByte(AInfo, SizeOf(AInfo), 0);
  AInfo.Handle := AHandle;

  Result := XGetWindowAttributes(XDisplayHandle, TWindow(AHandle), @Attrs) <> 0;
  if not Result then Exit;

  AInfo.Title := GetWindowTitle(AHandle);
  AInfo.ClassName := GetWindowClass(AHandle);
  AInfo.ProcessID := GetWindowPID(AHandle);
  AInfo.ProcessName := GetProcessNameFromPID(AInfo.ProcessID);
  AInfo.Rect := GetWindowRect(AHandle);
  AInfo.IsVisible := (Attrs.map_state = IsViewable) and not HasWMState(TWindow(AHandle), AtomNetWMStateHidden);
  AInfo.IsMinimized := HasWMState(TWindow(AHandle), AtomNetWMStateHidden);
  AInfo.IsMaximized := HasWMState(TWindow(AHandle), AtomNetWMStateMaximizedVert) and HasWMState(TWindow(AHandle), AtomNetWMStateMaximizedHorz);
end;

function TXWindowManager.GetWindowTitle(AHandle: TWindowHandle): string;
begin
  // Try _NET_WM_NAME first (UTF-8)
  Result := GetWindowPropertyString(TWindow(AHandle), AtomNetWMName);

  // Fall back to WM_NAME
  if Result = '' then
    Result := GetWindowPropertyString(TWindow(AHandle), AtomWMName);
end;

function TXWindowManager.GetWindowPID(AHandle: TWindowHandle): longword;
begin
  Result := GetWindowPropertyCardinal(TWindow(AHandle), AtomNetWMPID);
end;

function TXWindowManager.GetWindowRect(AHandle: TWindowHandle): TRect;
var
  Attrs: TXWindowAttributes;
  ChildReturn: TWindow;
  RootX, RootY: cint;
begin
  Result := Rect(0, 0, 0, 0);

  if XGetWindowAttributes(XDisplayHandle, TWindow(AHandle), @Attrs) = 0 then
    Exit;

  // Translate to root coordinates
  XTranslateCoordinates(XDisplayHandle, TWindow(AHandle), RootWindowHandle,
    0, 0, @RootX, @RootY, @ChildReturn);

  Result.Left := RootX;
  Result.Top := RootY;
  Result.Right := RootX + Attrs.Width;
  Result.Bottom := RootY + Attrs.Height;
end;

function TXWindowManager.GetWindowClass(AHandle: TWindowHandle): string;
var
  ClassHint: TXClassHint;
begin
  Result := '';
  ClassHint.res_name := nil;
  ClassHint.res_class := nil;

  if XGetClassHint(XDisplayHandle, TWindow(AHandle), @ClassHint) <> 0 then
  begin
    if ClassHint.res_class <> nil then
    begin
      Result := StrPas(ClassHint.res_class);
      XFree(ClassHint.res_class);
    end;
    if ClassHint.res_name <> nil then
      XFree(ClassHint.res_name);
  end;
end;

function TXWindowManager.EnumerateWindows: TList;
var
  Data: PWindow;
  Count, BytesAfter: culong;
  ActualType: TAtom;
  ActualFormat: cint;
  i: integer;
begin
  Result := TList.Create;
  Data := nil;
  if (XGetWindowProperty(XDisplayHandle, RootWindowHandle, AtomNetClientList, 0, $7FFFFFFF, False, XA_WINDOW, @ActualType, @ActualFormat,
    @Count, @BytesAfter, @Data) = Success) and (ActualType = XA_WINDOW) and (ActualFormat = 32) then
  begin
    for i := 0 to integer(Count) - 1 do
      Result.Add(Pointer(PWindow(pbyte(Data) + i * SizeOf(TWindow))^));
  end
  else
  begin
    // Bare X servers may have no EWMH client list.
    Result.Free;
    Result := GetWindowChildren(RootWindowHandle);
  end;
  if Data <> nil then XFree(Data);
end;

// Window activation and focus

function TXWindowManager.ActivateWindow(AHandle: TWindowHandle): boolean;
begin
  // Use _NET_ACTIVE_WINDOW message for proper EWMH activation
  Result := SendClientMessage(TWindow(AHandle), AtomNetActiveWindow, 2, CurrentTime, 0);
end;

function TXWindowManager.FocusWindow(AHandle: TWindowHandle): boolean;
begin
  XSetInputFocus(XDisplayHandle, TWindow(AHandle), RevertToParent, CurrentTime);
  XFlush(XDisplayHandle);
  Result := True;
end;

function TXWindowManager.RaiseWindow(AHandle: TWindowHandle): boolean;
begin
  XRaiseWindow(XDisplayHandle, TWindow(AHandle));
  XFlush(XDisplayHandle);
  Result := True;
end;

// Window positioning and sizing

{ A window manager re-frames a raw XMoveWindow, so a window moved to its own
  reported position would drift by the frame.  _NET_MOVERESIZE_WINDOW with
  static gravity names the client's root position - the same thing
  GetWindowRect reports - and the manager does its own frame arithmetic.
  Bits: gravity 10 (static), 8-11 which fields are present, 12 the source. }
function TXWindowManager.EwmhManaged: boolean;
var
  Data: Pointer;
  Count: culong;
begin
  Result := GetWindowProperty(RootWindowHandle, AtomNetSupportingWMCheck, Data, Count);
  if Result then
  begin
    Result := Count > 0;
    XFree(Data);
  end;
end;

function TXWindowManager.MoveWindow(AHandle: TWindowHandle; X, Y: integer): boolean;
begin
  if EwmhManaged then
    Result := SendClientMessage(TWindow(AHandle), AtomNetMoveResizeWindow, 10 or (1 shl 8) or (1 shl 9) or (1 shl 12), X, Y, 0, 0)
  else
  begin
    XMoveWindow(XDisplayHandle, TWindow(AHandle), X, Y);
    XFlush(XDisplayHandle);
    Result := True;
  end;
end;

function TXWindowManager.ResizeWindow(AHandle: TWindowHandle; Width, Height: integer): boolean;
begin
  if EwmhManaged then
    Result := SendClientMessage(TWindow(AHandle), AtomNetMoveResizeWindow, 10 or (1 shl 10) or (1 shl 11) or (1 shl 12), 0, 0, Width, Height)
  else
  begin
    XResizeWindow(XDisplayHandle, TWindow(AHandle), Width, Height);
    XFlush(XDisplayHandle);
    Result := True;
  end;
end;

function TXWindowManager.MoveResizeWindow(AHandle: TWindowHandle; X, Y, Width, Height: integer): boolean;
begin
  if EwmhManaged then
    Result := SendClientMessage(TWindow(AHandle), AtomNetMoveResizeWindow, 10 or (1 shl 8) or (1 shl 9) or (1 shl 10) or (1 shl 11) or (1 shl 12), X, Y, Width, Height)
  else
  begin
    XMoveResizeWindow(XDisplayHandle, TWindow(AHandle), X, Y, Width, Height);
    XFlush(XDisplayHandle);
    Result := True;
  end;
end;

// Window state management

function TXWindowManager.MinimizeWindow(AHandle: TWindowHandle): boolean;
begin
  // Use WM_CHANGE_STATE with IconicState
  Result := SendClientMessage(TWindow(AHandle), AtomNetWMState, _NET_WM_STATE_ADD, AtomNetWMStateHidden, 0, 1);

  // Alternative: XIconifyWindow
  if not Result then
  begin
    XIconifyWindow(XDisplayHandle, TWindow(AHandle), ScreenNumber);
    XFlush(XDisplayHandle);
    Result := True;
  end;
end;

function TXWindowManager.MaximizeWindow(AHandle: TWindowHandle): boolean;
begin
  Result := SendClientMessage(TWindow(AHandle), AtomNetWMState, _NET_WM_STATE_ADD, AtomNetWMStateMaximizedVert, AtomNetWMStateMaximizedHorz, 1);
end;

function TXWindowManager.RestoreWindow(AHandle: TWindowHandle): boolean;
begin
  // First unmaximize
  SendClientMessage(TWindow(AHandle), AtomNetWMState, _NET_WM_STATE_REMOVE,
    AtomNetWMStateMaximizedVert, AtomNetWMStateMaximizedHorz, 1);

  // Then unhide (deiconify)
  SendClientMessage(TWindow(AHandle), AtomNetWMState, _NET_WM_STATE_REMOVE,
    AtomNetWMStateHidden, 0, 1);

  // Map the window
  XMapWindow(XDisplayHandle, TWindow(AHandle));
  XFlush(XDisplayHandle);
  Result := True;
end;

function TXWindowManager.CloseWindow(AHandle: TWindowHandle): boolean;
begin
  Result := SendClientMessage(TWindow(AHandle), AtomNetCloseWindow, CurrentTime, 1);
end;

// Window visibility

function TXWindowManager.ShowWindow(AHandle: TWindowHandle): boolean;
begin
  XMapWindow(XDisplayHandle, TWindow(AHandle));
  XFlush(XDisplayHandle);
  Result := True;
end;

function TXWindowManager.HideWindow(AHandle: TWindowHandle): boolean;
begin
  XUnmapWindow(XDisplayHandle, TWindow(AHandle));
  XFlush(XDisplayHandle);
  Result := True;
end;

function TXWindowManager.IsWindowVisible(AHandle: TWindowHandle): boolean;
var
  Attrs: TXWindowAttributes;
begin
  Result := False;
  if XGetWindowAttributes(XDisplayHandle, TWindow(AHandle), @Attrs) <> 0 then
    Result := (Attrs.map_state = IsViewable) and not HasWMState(TWindow(AHandle), AtomNetWMStateHidden);
end;

// Desktop and workspace management

function TXWindowManager.GetDesktopCount: integer;
begin
  Result := GetWindowPropertyCardinal(RootWindowHandle, AtomNetNumberOfDesktops);
  if Result = 0 then
    Result := 1;  // At least one desktop
end;

function TXWindowManager.GetCurrentDesktop: integer;
begin
  Result := GetWindowPropertyCardinal(RootWindowHandle, AtomNetCurrentDesktop);
end;

function TXWindowManager.SetCurrentDesktop(ADesktop: integer): boolean;
begin
  Result := SendClientMessage(RootWindowHandle, AtomNetCurrentDesktop, ADesktop, CurrentTime);
end;

function TXWindowManager.GetWindowDesktop(AHandle: TWindowHandle): integer;
begin
  Result := GetWindowPropertyCardinal(TWindow(AHandle), AtomNetWMDesktop);
end;

function TXWindowManager.SetWindowDesktop(AHandle: TWindowHandle; ADesktop: integer): boolean;
begin
  Result := SendClientMessage(TWindow(AHandle), AtomNetWMDesktop, ADesktop, 1);
end;

// X11-specific methods

function TXWindowManager.GetWindowAtPoint(X, Y: integer): TWindowHandle;
var
  ChildReturn: TWindow;
  DestX, DestY: cint;
  CurrentWindow: TWindow;
begin
  Result := 0;

  // Start from root and traverse down to find the deepest window at X,Y
  CurrentWindow := RootWindowHandle;

  while CurrentWindow <> 0 do
  begin
    // Translate root coordinates to the current window's coordinate space
    // and find what child (if any) contains that point
    if XTranslateCoordinates(XDisplayHandle, RootWindowHandle, CurrentWindow, X, Y, @DestX, @DestY, @ChildReturn) = 0 then
      Break;

    // If no child contains the point, we've found the deepest window
    if ChildReturn = 0 then
      Break;

    // Otherwise, continue traversing into the child
    CurrentWindow := ChildReturn;
  end;

  // Return the deepest window found (but not root)
  if CurrentWindow <> RootWindowHandle then
    Result := TWindowHandle(CurrentWindow)
  else
    Result := 0;
end;

function TXWindowManager.GetWindowChildren(AHandle: TWindowHandle): TList;
var
  RootReturn, ParentReturn: TWindow;
  Children: PWindow;
  NumChildren: cuint;
  i: integer;
begin
  Result := TList.Create;

  if XQueryTree(XDisplayHandle, TWindow(AHandle), @RootReturn, @ParentReturn, @Children, @NumChildren) <> 0 then
  begin
    for i := 0 to integer(NumChildren) - 1 do
      Result.Add(Pointer(PWindow(pbyte(Children) + i * SizeOf(TWindow))^));

    if Children <> nil then
      XFree(Children);
  end;
end;

function TXWindowManager.GetWindowParent(AHandle: TWindowHandle): TWindowHandle;
var
  RootReturn, ParentReturn: TWindow;
  Children: PWindow;
  NumChildren: cuint;
begin
  Result := 0;

  if XQueryTree(XDisplayHandle, TWindow(AHandle), @RootReturn, @ParentReturn, @Children, @NumChildren) <> 0 then
  begin
    Result := TWindowHandle(ParentReturn);
    if Children <> nil then
      XFree(Children);
  end;
end;

function TXWindowManager.GetRootWindow: TWindowHandle;
begin
  Result := TWindowHandle(RootWindowHandle);
end;

end.
