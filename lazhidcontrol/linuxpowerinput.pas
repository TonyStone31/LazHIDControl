unit LinuxPowerInput;
{$mode objfpc}{$H+}
// Copyright (c) 2026 Tony Stone. GPL-2.0-or-later; see COPYING.GPL2.
interface
uses dbus, PowerInput;
type
  TLinuxPowerInput = class(TPowerInput)
  private
    Connection: PDBusConnection;
    HasUPower: Boolean;
    function Call(const Service, Path, InterfaceName, Method: string;
      const Argument1: string = ''; const Argument2: string = ''): PDBusMessage;
    function HasService(const Name: string): Boolean;
  public
    destructor Destroy; override;
    function Start: Boolean; override;
    procedure Stop; override;
    function Poll(out Events: TPowerEvents): Boolean; override;
    function ReadStatus(out Status: TPowerStatus): Boolean; override;
  end;
implementation
uses SysUtils, Math;
const
  LoginService = 'org.freedesktop.login1';
  PowerService = 'org.freedesktop.UPower';
function TLinuxPowerInput.Call(const Service, Path, InterfaceName, Method: string;
  const Argument1: string; const Argument2: string): PDBusMessage;
var Request: PDBusMessage; Error: DBusError; Iter: DBusMessageIter; Text: PChar;
begin
  Result := nil;
  Request := dbus_message_new_method_call(PChar(Service), PChar(Path), PChar(InterfaceName), PChar(Method));
  if Request = nil then begin ErrorText := 'Cannot allocate a D-Bus request.'; Exit; end;
  dbus_error_init(@Error);
  try
    dbus_message_set_auto_start(Request, 0);
    dbus_message_iter_init_append(Request, @Iter);
    if Argument1 <> '' then begin Text := PChar(Argument1); dbus_message_iter_append_basic(@Iter, DBUS_TYPE_STRING, @Text); end;
    if Argument2 <> '' then begin Text := PChar(Argument2); dbus_message_iter_append_basic(@Iter, DBUS_TYPE_STRING, @Text); end;
    Result := dbus_connection_send_with_reply_and_block(Connection, Request, 200, @Error);
    if (Result = nil) and (Error.message <> nil) then ErrorText := StrPas(Error.message);
  finally dbus_error_free(@Error); dbus_message_unref(Request); end;
end;
function TLinuxPowerInput.HasService(const Name: string): Boolean;
var Reply: PDBusMessage; Iter: DBusMessageIter; Present: dbus_bool_t;
begin
  Result := False;
  Reply := Call('org.freedesktop.DBus', '/org/freedesktop/DBus', 'org.freedesktop.DBus', 'NameHasOwner', Name);
  if Reply = nil then Exit;
  try
    if (dbus_message_iter_init(Reply, @Iter) <> 0) and (dbus_message_iter_get_arg_type(@Iter) = DBUS_TYPE_BOOLEAN) then begin
      dbus_message_iter_get_basic(@Iter, @Present); Result := Present <> 0;
    end;
  finally dbus_message_unref(Reply); end;
end;
function TLinuxPowerInput.Start: Boolean;
var Error: DBusError;
begin
  Result := False;
  dbus_error_init(@Error);
  try
    Connection := dbus_bus_get_private(DBUS_BUS_SYSTEM, @Error);
    if Connection = nil then begin ErrorText := 'Cannot connect to the system D-Bus.'; Exit; end;
    dbus_connection_set_exit_on_disconnect(Connection, 0);
    if not HasService(LoginService) then begin ErrorText := 'Power notifications require logind.'; Exit; end;
    HasUPower := HasService(PowerService);
    dbus_bus_add_match(Connection, 'type=''signal'',sender=''org.freedesktop.login1'',interface=''org.freedesktop.login1.Manager'',member=''PrepareForSleep''', @Error);
    if dbus_error_is_set(@Error) <> 0 then begin ErrorText := StrPas(Error.message); Exit; end;
    if HasUPower then begin
      dbus_bus_add_match(Connection, 'type=''signal'',sender=''org.freedesktop.UPower'',interface=''org.freedesktop.DBus.Properties'',member=''PropertiesChanged''', @Error);
      if dbus_error_is_set(@Error) <> 0 then begin ErrorText := StrPas(Error.message); Exit; end;
    end;
    dbus_connection_flush(Connection);
    ErrorText := '';
    Result := True;
  finally dbus_error_free(@Error); if not Result then Stop; end;
end;
procedure TLinuxPowerInput.Stop;
begin
  if Connection <> nil then begin dbus_connection_close(Connection); dbus_connection_unref(Connection); end;
  Connection := nil;
  Pending := nil;
end;
destructor TLinuxPowerInput.Destroy;
begin Stop; inherited Destroy; end;
function TLinuxPowerInput.Poll(out Events: TPowerEvents): Boolean;
var Message: PDBusMessage; Iter: DBusMessageIter; Sleeping: dbus_bool_t;
begin
  Events := nil;
  Result := (Connection <> nil) and (dbus_connection_read_write(Connection, 0) <> 0);
  if not Result then begin ErrorText := 'The system D-Bus connection was lost.'; Exit; end;
  Message := dbus_connection_pop_message(Connection);
  while Message <> nil do begin
    try
      if dbus_message_is_signal(Message, PChar(LoginService + '.Manager'), 'PrepareForSleep') <> 0 then begin
        if (dbus_message_iter_init(Message, @Iter) <> 0) and (dbus_message_iter_get_arg_type(@Iter) = DBUS_TYPE_BOOLEAN) then begin
          dbus_message_iter_get_basic(@Iter, @Sleeping);
          if Sleeping <> 0 then AddEvent(peSuspend) else AddEvent(peResume);
        end;
      end else if dbus_message_is_signal(Message, 'org.freedesktop.DBus.Properties', 'PropertiesChanged') <> 0 then AddEvent(peChanged);
    finally dbus_message_unref(Message); end;
    Message := dbus_connection_pop_message(Connection);
  end;
  inherited Poll(Events);
end;
function TLinuxPowerInput.ReadStatus(out Status: TPowerStatus): Boolean;
var Reply: PDBusMessage; Iter, ArrayIter, Entry, Value: DBusMessageIter;
  Name: PChar; Flag: dbus_bool_t; Percent: Double; State: dbus_uint32_t;
begin
  Status := Default(TPowerStatus);
  Status.Source := psUnknown;
  Status.BatteryPercent := -1;
  if not HasUPower then Exit(True); // Sleep notifications still work without battery services.
  Reply := Call(PowerService, '/org/freedesktop/UPower', 'org.freedesktop.DBus.Properties', 'Get', PowerService, 'OnBattery');
  if Reply = nil then Exit(False);
  try
    Result := (dbus_message_iter_init(Reply, @Iter) <> 0) and (dbus_message_iter_get_arg_type(@Iter) = DBUS_TYPE_VARIANT);
    if Result then begin
      dbus_message_iter_recurse(@Iter, @Value);
      Result := dbus_message_iter_get_arg_type(@Value) = DBUS_TYPE_BOOLEAN;
      if Result then begin dbus_message_iter_get_basic(@Value, @Flag); if Flag <> 0 then Status.Source := psBattery else Status.Source := psAC; end;
    end;
  finally dbus_message_unref(Reply); end;
  if not Result then begin ErrorText := 'UPower returned an invalid power-source value.'; Exit; end;
  Reply := Call(PowerService, '/org/freedesktop/UPower/devices/DisplayDevice', 'org.freedesktop.DBus.Properties', 'GetAll', PowerService + '.Device');
  if Reply = nil then Exit(False);
  try
    Result := (dbus_message_iter_init(Reply, @Iter) <> 0) and (dbus_message_iter_get_arg_type(@Iter) = DBUS_TYPE_ARRAY);
    if not Result then begin ErrorText := 'UPower returned invalid battery properties.'; Exit; end;
    dbus_message_iter_recurse(@Iter, @ArrayIter);
    Flag := 0; Percent := -1; State := 0;
    while dbus_message_iter_get_arg_type(@ArrayIter) = DBUS_TYPE_DICT_ENTRY do begin
      dbus_message_iter_recurse(@ArrayIter, @Entry);
      dbus_message_iter_get_basic(@Entry, @Name);
      dbus_message_iter_next(@Entry);
      dbus_message_iter_recurse(@Entry, @Value);
      if (StrPas(Name) = 'IsPresent') and (dbus_message_iter_get_arg_type(@Value) = DBUS_TYPE_BOOLEAN) then dbus_message_iter_get_basic(@Value, @Flag);
      if (StrPas(Name) = 'Percentage') and (dbus_message_iter_get_arg_type(@Value) = DBUS_TYPE_DOUBLE) then dbus_message_iter_get_basic(@Value, @Percent);
      if (StrPas(Name) = 'State') and (dbus_message_iter_get_arg_type(@Value) = DBUS_TYPE_UINT32) then dbus_message_iter_get_basic(@Value, @State);
      dbus_message_iter_next(@ArrayIter);
    end;
    if Flag <> 0 then begin
      if not IsNan(Percent) and not IsInfinite(Percent) and (Percent >= 0) and (Percent <= 100) then Status.BatteryPercent := Round(Percent);
      Status.Charging := State = 1;
    end;
  finally dbus_message_unref(Reply); end;
end;
end.
