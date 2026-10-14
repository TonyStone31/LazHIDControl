unit LinuxPortal;

{$mode objfpc}{$H+}
// Copyright (c) 2026 Tony Stone. GPL-2.0-or-later; see COPYING.GPL2.

{ Talking to org.freedesktop.portal.Desktop, which is how a Wayland session
  hands out what a program may not take for itself.  A portal call answers
  at once with the path of a request object; the real answer comes later in
  that object's Response signal, after the desktop has asked the user if it
  means to. }

interface

uses ctypes, dbus;

const
  PortalName = 'org.freedesktop.portal.Desktop';
  PortalPath = '/org/freedesktop/portal/desktop';

// A connection of our own, so the signals are not mixed with anyone else's.
function PortalConnect: PDBusConnection;
procedure PortalClose(var Connection: PDBusConnection);
// One entry of an a{sv} options dictionary: a string ('s') or a number ('u').
procedure AddOption(var Options: DBusMessageIter; Key: PChar; Kind: cint; Signature: PChar; Value: Pointer);
{ Sends Call, frees it, and waits for the Response.  True when the desktop
  agreed.  Value is the string the results hold under Key, if they do. }
function PortalAsk(Connection: PDBusConnection; Call: PDBusMessage; WaitMs: cardinal; const Key: string; out Value: string): boolean;
// The version of one portal, such as org.freedesktop.portal.RemoteDesktop; 0 when this desktop has none.
function PortalVersion(Connection: PDBusConnection; const Name: string): cardinal;

implementation

uses SysUtils, Forms;

function PortalConnect: PDBusConnection;
begin
  Result := dbus_bus_get_private(DBUS_BUS_SESSION, nil);
  if Result = nil then Exit;
  dbus_connection_set_exit_on_disconnect(Result, 0);
  dbus_bus_add_match(Result, 'type=''signal'',interface=''org.freedesktop.portal.Request'',member=''Response''', nil);
end;

procedure PortalClose(var Connection: PDBusConnection);
begin
  if Connection = nil then Exit;
  dbus_connection_close(Connection);
  dbus_connection_unref(Connection);
  Connection := nil;
end;

procedure AddOption(var Options: DBusMessageIter; Key: PChar; Kind: cint; Signature: PChar; Value: Pointer);
var
  Entry, variant: DBusMessageIter;
begin
  dbus_message_iter_open_container(@Options, DBUS_TYPE_DICT_ENTRY, nil, @Entry);
  dbus_message_iter_append_basic(@Entry, DBUS_TYPE_STRING, @Key);
  dbus_message_iter_open_container(@Entry, DBUS_TYPE_VARIANT, Signature, @variant);
  dbus_message_iter_append_basic(@variant, Kind, Value);
  dbus_message_iter_close_container(@Entry, @variant);
  dbus_message_iter_close_container(@Options, @Entry);
end;

// Response carries (u code, a{sv} results); code 0 is yes.
function ReadResponse(Signal: PDBusMessage; const Key: string; out Value: string): boolean;
var
  Args, Results, Entry, variant: DBusMessageIter;
  Code: dbus_uint32_t;
  Name, Text: PChar;
begin
  Value := '';
  Code := 1;
  if dbus_message_iter_init(Signal, @Args) = 0 then Exit(False);
  if dbus_message_iter_get_arg_type(@Args) = DBUS_TYPE_UINT32 then dbus_message_iter_get_basic(@Args, @Code);
  if (Code = 0) and (dbus_message_iter_next(@Args) <> 0) and (dbus_message_iter_get_arg_type(@Args) = DBUS_TYPE_ARRAY) then
  begin
    dbus_message_iter_recurse(@Args, @Results);
    while dbus_message_iter_get_arg_type(@Results) = DBUS_TYPE_DICT_ENTRY do
    begin
      dbus_message_iter_recurse(@Results, @Entry);
      dbus_message_iter_get_basic(@Entry, @Name);
      dbus_message_iter_next(@Entry);
      dbus_message_iter_recurse(@Entry, @variant);
      if (StrPas(Name) = Key) and (dbus_message_iter_get_arg_type(@variant) = DBUS_TYPE_STRING) then
      begin
        dbus_message_iter_get_basic(@variant, @Text);
        Value := Text;
      end;
      dbus_message_iter_next(@Results);
    end;
  end;
  Result := Code = 0;
end;

function PortalAsk(Connection: PDBusConnection; Call: PDBusMessage; WaitMs: cardinal; const Key: string; out Value: string): boolean;
var
  Reply, Signal: PDBusMessage;
  Args: DBusMessageIter;
  Path: PChar;
  Request: string;
  Deadline: QWord;
  Answered: boolean;
begin
  Result := False;
  Value := '';
  Request := '';
  Reply := dbus_connection_send_with_reply_and_block(Connection, Call, 5000, nil);
  dbus_message_unref(Call);
  if Reply = nil then Exit;
  if (dbus_message_iter_init(Reply, @Args) <> 0) and (dbus_message_iter_get_arg_type(@Args) = DBUS_TYPE_OBJECT_PATH) then
  begin
    dbus_message_iter_get_basic(@Args, @Path);
    Request := Path;
  end;
  dbus_message_unref(Reply);
  if Request = '' then Exit;

  // The user may be looking at a permission dialog, so this can wait a
  // while, and it keeps the application's own windows alive meanwhile.
  Answered := False;
  Deadline := GetTickCount64 + WaitMs;
  while not Answered and (GetTickCount64 < Deadline) and (dbus_connection_read_write(Connection, 50) <> 0) do
  begin
    Signal := dbus_connection_pop_message(Connection);
    while Signal <> nil do
    begin
      if (dbus_message_is_signal(Signal, 'org.freedesktop.portal.Request', 'Response') <> 0) and (StrPas(dbus_message_get_path(Signal)) = Request) then
      begin
        Answered := True;
        Result := ReadResponse(Signal, Key, Value);
      end;
      dbus_message_unref(Signal);
      Signal := dbus_connection_pop_message(Connection);
    end;
    Application.ProcessMessages;
  end;
end;

function PortalVersion(Connection: PDBusConnection; const Name: string): cardinal;
var
  Call, Reply: PDBusMessage;
  Args, variant: DBusMessageIter;
  Iface, Prop: PChar;
  Version: dbus_uint32_t;
begin
  Result := 0;
  Call := dbus_message_new_method_call(PortalName, PortalPath, 'org.freedesktop.DBus.Properties', 'Get');
  Iface := PChar(Name);
  Prop := 'version';
  dbus_message_iter_init_append(Call, @Args);
  dbus_message_iter_append_basic(@Args, DBUS_TYPE_STRING, @Iface);
  dbus_message_iter_append_basic(@Args, DBUS_TYPE_STRING, @Prop);
  Reply := dbus_connection_send_with_reply_and_block(Connection, Call, 2000, nil);
  dbus_message_unref(Call);
  if Reply = nil then Exit;
  if (dbus_message_iter_init(Reply, @Args) <> 0) and (dbus_message_iter_get_arg_type(@Args) = DBUS_TYPE_VARIANT) then
  begin
    dbus_message_iter_recurse(@Args, @variant);
    if dbus_message_iter_get_arg_type(@variant) = DBUS_TYPE_UINT32 then
    begin
      dbus_message_iter_get_basic(@variant, @Version);
      Result := Version;
    end;
  end;
  dbus_message_unref(Reply);
end;

end.
