{ XGlobalMouseMonitor

  Copyright (C) 2025 LazHIDControl Contributors

  X11 implementation of global mouse monitoring using XInput2.

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
unit XGlobalMouseMonitor;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Controls, DynLibs, X, XLib, ctypes,
  GlobalMouseMonitorIntf;

type
  { TXGlobalMouseListener }
  TXGlobalMouseListener = class(TGlobalMouseListener)
  private
    Display: PDisplay;
    RootWindow: TWindow;
    MonitorThread: TThread;
    XI2Opcode: cint;
  protected
    procedure DoStartMonitoring; override;
    procedure DoStopMonitoring; override;
  public
    constructor Create; override;
    destructor Destroy; override;
  end;

function InitializeGlobalMouseListener: TGlobalMouseListener;

implementation

// XInput2 Constants
const
  XI_RawButtonPress = 15;
  XI_RawButtonRelease = 16;
  XIAllMasterDevices = 1;

type
  PXIEventMask = ^TXIEventMask;
  TXIEventMask = record
    deviceid: cint;
    mask_len: cint;
    mask: PByte;
  end;

  // Simplified XIRawEvent structure
  PXIRawEvent = ^TXIRawEvent;
  TXIRawEvent = record
    _type: cint;
    serial: culong;
    send_event: LongBool;
    display: PDisplay;
    extension: cint;
    evtype: cint;
    time: TTime;
    deviceid: cint;
    sourceid: cint;
    detail: cint;  // This is the button number!
    flags: cint;
  end;

// XInput2 functions - we'll load them dynamically
var
  XI2Lib: TLibHandle = NilHandle;
  XIQueryVersion: function(dpy: PDisplay; major_version_inout, minor_version_inout: pcint): cint; cdecl;
  XISelectEvents: function(dpy: PDisplay; win: TWindow; masks: PXIEventMask; num_masks: cint): cint; cdecl;
  XGetEventData: function(dpy: PDisplay; cookie: pointer): Boolean; cdecl;
  XFreeEventData: procedure(dpy: PDisplay; cookie: pointer); cdecl;

type
  TMouseMonitorThread = class(TThread)
  private
    Listener: TXGlobalMouseListener;
    DetectedButton: TMouseButton;
    DetectedX: Integer;
    DetectedY: Integer;
    procedure NotifyMouseClick;
  public
    constructor Create(AListener: TXGlobalMouseListener);
    procedure Execute; override;
  end;

function LoadXInput2Functions: Boolean;
begin
  Result := False;

  XI2Lib := LoadLibrary('libXi.so.6');
  if XI2Lib = NilHandle then
    XI2Lib := LoadLibrary('libXi.so');
  if XI2Lib = NilHandle then
    Exit;

  Pointer(XIQueryVersion) := GetProcAddress(XI2Lib, 'XIQueryVersion');
  Pointer(XISelectEvents) := GetProcAddress(XI2Lib, 'XISelectEvents');
  Pointer(XGetEventData) := GetProcAddress(XI2Lib, 'XGetEventData');
  Pointer(XFreeEventData) := GetProcAddress(XI2Lib, 'XFreeEventData');

  Result := Assigned(XIQueryVersion) and Assigned(XISelectEvents) and
            Assigned(XGetEventData) and Assigned(XFreeEventData);
end;

function ButtonToMouseButton(ButtonNum: cint): TMouseButton;
begin
  case ButtonNum of
    1: Result := mbLeft;
    2: Result := mbMiddle;
    3: Result := mbRight;
    // Buttons 4-5 are typically scroll wheel (up/down)
    // Buttons 6-9 are extra mouse buttons
    8: Result := mbExtra1;  // Back button
    9: Result := mbExtra2;  // Forward button
  else
    Result := mbLeft;  // Default for unknown buttons
  end;
end;

constructor TMouseMonitorThread.Create(AListener: TXGlobalMouseListener);
begin
  inherited Create(False);
  FreeOnTerminate := False;
  Listener := AListener;
end;

procedure TMouseMonitorThread.NotifyMouseClick;
begin
  if Assigned(Listener.OnMouseClick) then
    Listener.OnMouseClick(DetectedButton, DetectedX, DetectedY);
end;

procedure TMouseMonitorThread.Execute;
var
  Event: TXEvent;
  RawEvent: PXIRawEvent;
  rootX, rootY, winX, winY: cint;
  mask: cuint;
  root, child: TWindow;
begin
  while not Terminated do
  begin
    if XPending(Listener.Display) > 0 then
    begin
      XNextEvent(Listener.Display, @Event);

      // Check for GenericEvent (XInput2 events)
      if Event._type = GenericEvent then
      begin
        // Get the event data
        if XGetEventData(Listener.Display, @Event.xcookie) then
        begin
          if (Event.xcookie.extension = Listener.XI2Opcode) and
             (Event.xcookie.evtype = XI_RawButtonPress) then
          begin
            // Cast the data to XIRawEvent structure
            RawEvent := PXIRawEvent(Event.xcookie.data);

            // Get mouse position
            XQueryPointer(Listener.Display, Listener.RootWindow,
                         @root, @child, @rootX, @rootY, @winX, @winY, @mask);

            DetectedButton := ButtonToMouseButton(RawEvent^.detail);
            DetectedX := rootX;
            DetectedY := rootY;

            // Notify in main thread
            Synchronize(@NotifyMouseClick);
          end;

          XFreeEventData(Listener.Display, @Event.xcookie);
        end;
      end;
    end
    else
      Sleep(10);  // Avoid busy waiting
  end;
end;

{ TXGlobalMouseListener }

constructor TXGlobalMouseListener.Create;
var
  major, minor: cint;
  event_base, error_base: cint;
  mask: array[0..1] of Byte;
  eventmask: TXIEventMask;
begin
  inherited Create;

  MonitorThread := nil;
  XI2Opcode := 0;

  // Load XInput2 functions
  if not LoadXInput2Functions then
    raise Exception.Create('Failed to load XInput2 library');

  // Open connection to X server
  Display := XOpenDisplay(nil);
  if Display = nil then
    raise Exception.Create('Failed to open X display');

  RootWindow := XDefaultRootWindow(Display);

  // Check if XInput2 is available
  major := 2;
  minor := 0;
  if XIQueryVersion(Display, @major, @minor) <> 0 then
    raise Exception.Create('XInput2 not available');

  // Get XI2 opcode
  if XQueryExtension(Display, 'XInputExtension', @XI2Opcode, @event_base, @error_base) = False then
    raise Exception.Create('XInput extension not available');

  // Select for raw button events
  FillByte(mask, SizeOf(mask), 0);
  mask[XI_RawButtonPress shr 3] := 1 shl (XI_RawButtonPress and 7);

  eventmask.deviceid := XIAllMasterDevices;
  eventmask.mask_len := Length(mask);
  eventmask.mask := @mask[0];

  XISelectEvents(Display, RootWindow, @eventmask, 1);
  XFlush(Display);
end;

destructor TXGlobalMouseListener.Destroy;
begin
  StopMonitoring;

  if Display <> nil then
  begin
    XCloseDisplay(Display);
    Display := nil;
  end;

  if XI2Lib <> NilHandle then
  begin
    UnloadLibrary(XI2Lib);
    XI2Lib := NilHandle;
  end;

  inherited Destroy;
end;

procedure TXGlobalMouseListener.DoStartMonitoring;
begin
  if MonitorThread <> nil then
    Exit;

  MonitorThread := TMouseMonitorThread.Create(Self);
  IsActive := True;
end;

procedure TXGlobalMouseListener.DoStopMonitoring;
begin
  if MonitorThread = nil then
    Exit;

  MonitorThread.Terminate;
  MonitorThread.WaitFor;
  FreeAndNil(MonitorThread);
  IsActive := False;
end;

function InitializeGlobalMouseListener: TGlobalMouseListener;
begin
  try
    Result := TXGlobalMouseListener.Create;
  except
    Result := nil;
  end;
end;

end.
