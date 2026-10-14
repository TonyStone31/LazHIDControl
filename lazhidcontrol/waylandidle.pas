unit WaylandIdle;

{$mode objfpc}{$H+}
// Copyright (c) 2026 Tony Stone. GPL-2.0-or-later; see COPYING.GPL2.

{ Idle time on Wayland from the ext-idle-notify protocol, which KWin and the
  wlroots compositors have.  A compositor will not say how long the session
  has been idle.  It says when it has been idle for a time asked for in
  advance, and when that ends - so this asks for one second and counts on
  from there.  The answer is 0 until a second has passed without input; the
  compositor is only heard when this is called, so the count can run behind
  by the time between two calls; and nothing is known about the time before
  the first call. }

interface

function QueryWaylandIdle(out Milliseconds: QWord): boolean;

implementation

uses SysUtils, DynLibs, ctypes;

  {$packrecords c}
type
  PInterface = ^TInterface;

  // struct wl_message
  TMessage = record
    Name, Signature: PChar;
    Types: ^PInterface;
  end;

  // struct wl_interface
  TInterface = record
    Name: PChar;
    Version, CallCount: cint;
    Calls: ^TMessage;
    EventCount: cint;
    Events: ^TMessage;
  end;

  // union wl_argument
  TArgument = record
    case integer of
      0: (Number: cuint32);
      1: (Text: PChar);
      2: (Proxy: Pointer);
  end;

const
  AskedFor = 1000;

var
  // what the three arguments of get_idle_notification are: the new object,
  // a number, the seat
  ArgTypes: array[0..2] of PInterface;

const
  // the protocol, as wayland-scanner would write it out of ext-idle-notify-v1.xml
  NotificationCalls: array[0..0] of TMessage = ((Name: 'destroy'; Signature: ''; Types: @ArgTypes));
  NotificationEvents: array[0..1] of TMessage = ((Name: 'idled'; Signature: ''; Types: @ArgTypes), (Name: 'resumed'; Signature: ''; Types: @ArgTypes));
  NotificationIntf: TInterface = (Name: 'ext_idle_notification_v1'; Version: 2; CallCount: 1; Calls: @NotificationCalls; EventCount: 2; Events: @NotificationEvents);
  NotifierCalls: array[0..2] of TMessage = ((Name: 'destroy'; Signature: ''; Types: @ArgTypes), (Name: 'get_idle_notification'; Signature: 'nuo'; Types: @ArgTypes),
    (Name: 'get_input_idle_notification'; Signature: '2nuo'; Types: @ArgTypes));
  NotifierIntf: TInterface = (Name: 'ext_idle_notifier_v1'; Version: 2; CallCount: 3; Calls: @NotifierCalls; EventCount: 0; Events: nil);

var
  Lib: TLibHandle = NilHandle;
  Tried: boolean = False;
  Display: Pointer = nil;
  Seat: Pointer = nil;
  Notifier: Pointer = nil;
  NotifierVersion: cuint32;
  SeatIntf: PInterface;
  // when the idle spell began; 0 while there is input
  IdleSince: QWord = 0;

  Connect: function(Name: PChar): Pointer; cdecl;
  Disconnect: procedure(Display: Pointer); cdecl;
  Roundtrip: function(Display: Pointer): cint; cdecl;
  Listen: function(Proxy, Handlers, Data: Pointer): cint; cdecl;
  // wl_proxy_marshal_array_constructor and its _versioned twin
  Construct: function(Proxy: Pointer; Call: cuint32; Args: Pointer; Intf: PInterface): Pointer; cdecl;
  ConstructVersion: function(Proxy: Pointer; Call: cuint32; Args: Pointer; Intf: PInterface; Version: cuint32): Pointer; cdecl;

// wl_registry.bind
function Bind(Registry: Pointer; Name: cuint32; Intf: PInterface; Version: cuint32): Pointer;
var
  Args: array[0..3] of TArgument;
begin
  FillChar(Args, SizeOf(Args), 0);
  Args[0].Number := Name;
  Args[1].Text := Intf^.Name;
  Args[2].Number := Version;
  Result := ConstructVersion(Registry, 0, @Args, Intf, Version);
end;

procedure Announced(Data, Registry: Pointer; Name: cuint32; Intf: PChar; Version: cuint32); cdecl;
begin
  if (Seat = nil) and (StrPas(Intf) = 'wl_seat') then
    Seat := Bind(Registry, Name, SeatIntf, 1)
  else if (Notifier = nil) and (StrPas(Intf) = NotifierIntf.Name) then
  begin
    NotifierVersion := Version;
    if NotifierVersion > 2 then NotifierVersion := 2;
    Notifier := Bind(Registry, Name, @NotifierIntf, NotifierVersion);
  end;
end;

procedure Withdrawn(Data, Registry: Pointer; Name: cuint32); cdecl;
begin
end;

procedure Idled(Data, Notification: Pointer); cdecl;
begin
  IdleSince := GetTickCount64 - AskedFor;
end;

procedure Resumed(Data, Notification: Pointer); cdecl;
begin
  IdleSince := 0;
end;

const
  RegistryHandlers: array[0..1] of Pointer = (@Announced, @Withdrawn);
  NotificationHandlers: array[0..1] of Pointer = (@Idled, @Resumed);

function Start: boolean;
var
  Args: array[0..2] of TArgument;
  Registry: Pointer;
  Call: cuint32;
begin
  Result := False;
  Lib := LoadLibrary('libwayland-client.so.0');
  if Lib = NilHandle then Exit;
  Pointer(Connect) := GetProcedureAddress(Lib, 'wl_display_connect');
  Pointer(Disconnect) := GetProcedureAddress(Lib, 'wl_display_disconnect');
  Pointer(Roundtrip) := GetProcedureAddress(Lib, 'wl_display_roundtrip');
  Pointer(Listen) := GetProcedureAddress(Lib, 'wl_proxy_add_listener');
  Pointer(Construct) := GetProcedureAddress(Lib, 'wl_proxy_marshal_array_constructor');
  Pointer(ConstructVersion) := GetProcedureAddress(Lib, 'wl_proxy_marshal_array_constructor_versioned');
  SeatIntf := GetProcedureAddress(Lib, 'wl_seat_interface');
  if not (Assigned(Connect) and Assigned(Disconnect) and Assigned(Roundtrip) and Assigned(Listen) and Assigned(Construct) and Assigned(ConstructVersion)) or (SeatIntf = nil) then
    Exit;
  Display := Connect(nil);
  if Display = nil then Exit;
  ArgTypes[0] := @NotificationIntf;
  ArgTypes[2] := SeatIntf;

  // wl_display.get_registry, then one round trip brings the list of globals
  FillChar(Args, SizeOf(Args), 0);
  Registry := Construct(Display, 1, @Args, GetProcedureAddress(Lib, 'wl_registry_interface'));
  Listen(Registry, @RegistryHandlers, nil);
  Roundtrip(Display);
  if (Seat = nil) or (Notifier = nil) then Exit;

  // Version 2 can count input alone, as the other platforms do; version 1
  // also counts a program holding the screen awake, a video player say.
  if NotifierVersion >= 2 then Call := 2
  else
    Call := 1;
  Args[1].Number := AskedFor;
  Args[2].Proxy := Seat;
  Listen(Construct(Notifier, Call, @Args, @NotificationIntf), @NotificationHandlers, nil);
  Result := True;
end;

function QueryWaylandIdle(out Milliseconds: QWord): boolean;
begin
  Milliseconds := 0;
  if not Tried then
  begin
    Tried := True;
    if not Start and (Display <> nil) then
    begin
      Disconnect(Display);
      Display := nil;
    end;
  end;
  // the round trip is what delivers idled and resumed
  Result := (Display <> nil) and (Roundtrip(Display) >= 0);
  if Result and (IdleSince <> 0) then Milliseconds := GetTickCount64 - IdleSince;
end;

finalization
  if Display <> nil then Disconnect(Display);
  if Lib <> NilHandle then UnloadLibrary(Lib);
end.
