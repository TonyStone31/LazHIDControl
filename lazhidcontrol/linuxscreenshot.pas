unit LinuxScreenshot;

{$mode objfpc}{$H+}
// Copyright (c) 2026 Tony Stone. GPL-2.0-or-later; see COPYING.GPL2.

{ A picture of the desktop from org.freedesktop.portal.Screenshot - the way a
  Wayland session hands one out, since a program there cannot read the screen
  itself.  The desktop may ask the user for permission the first time. }

interface

uses Graphics;

function PortalScreenshot(Destination: TBitmap; out Message: string): boolean;

implementation

uses SysUtils, Forms, URIParser, dbus;

const
  Portal = 'org.freedesktop.portal.Desktop';

{ Screenshot answers with the path of a request object; the picture's
  address arrives later in that object's Response signal. }
function RequestScreenshot(Connection: PDBusConnection; out Request: string): boolean;
var
  Call, Reply: PDBusMessage;
  Args, Options: DBusMessageIter;
  Parent, Path: PChar;
begin
  Result := False;
  Call := dbus_message_new_method_call(Portal, '/org/freedesktop/portal/desktop', 'org.freedesktop.portal.Screenshot', 'Screenshot');
  if Call = nil then Exit;
  Parent := '';
  dbus_message_iter_init_append(Call, @Args);
  dbus_message_iter_append_basic(@Args, DBUS_TYPE_STRING, @Parent);
  dbus_message_iter_open_container(@Args, DBUS_TYPE_ARRAY, '{sv}', @Options);
  dbus_message_iter_close_container(@Args, @Options);
  Reply := dbus_connection_send_with_reply_and_block(Connection, Call, 5000, nil);
  dbus_message_unref(Call);
  if Reply = nil then Exit;
  if (dbus_message_iter_init(Reply, @Args) <> 0) and (dbus_message_iter_get_arg_type(@Args) = DBUS_TYPE_OBJECT_PATH) then
  begin
    dbus_message_iter_get_basic(@Args, @Path);
    Request := Path;
    Result := True;
  end;
  dbus_message_unref(Reply);
end;

// Response carries (u code, a{sv} results); code 0 is success and results has "uri".
function ReadResponse(Signal: PDBusMessage; out URI: string): boolean;
var
  Args, Results, Entry, Value: DBusMessageIter;
  Code: dbus_uint32_t;
  Name, Text: PChar;
begin
  URI := '';
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
      dbus_message_iter_recurse(@Entry, @Value);
      if (StrPas(Name) = 'uri') and (dbus_message_iter_get_arg_type(@Value) = DBUS_TYPE_STRING) then
      begin
        dbus_message_iter_get_basic(@Value, @Text);
        URI := Text;
      end;
      dbus_message_iter_next(@Results);
    end;
  end;
  Result := URI <> '';
end;

function PortalScreenshot(Destination: TBitmap; out Message: string): boolean;
var
  Connection: PDBusConnection;
  Signal: PDBusMessage;
  Request, URI, FileName: string;
  Deadline: QWord;
  Answered: boolean;
  Picture: TPicture;
begin
  Result := False;
  Message := 'The desktop portal is not available.';
  Connection := dbus_bus_get_private(DBUS_BUS_SESSION, nil);
  if Connection = nil then Exit;
  try
    dbus_connection_set_exit_on_disconnect(Connection, 0);
    dbus_bus_add_match(Connection, 'type=''signal'',interface=''org.freedesktop.portal.Request'',member=''Response''', nil);
    if not RequestScreenshot(Connection, Request) then Exit;

    // The user may be looking at a permission dialog, so this waits a while
    // and keeps the application's own windows alive meanwhile.
    Message := 'The desktop did not answer the screenshot request.';
    Answered := False;
    Deadline := GetTickCount64 + 60000;
    while not Answered and (GetTickCount64 < Deadline) and (dbus_connection_read_write(Connection, 50) <> 0) do
    begin
      Signal := dbus_connection_pop_message(Connection);
      while Signal <> nil do
      begin
        if (dbus_message_is_signal(Signal, 'org.freedesktop.portal.Request', 'Response') <> 0) and (StrPas(dbus_message_get_path(Signal)) = Request) then
        begin
          Answered := True;
          if not ReadResponse(Signal, URI) then Message := 'The desktop refused the screenshot.';
        end;
        dbus_message_unref(Signal);
        Signal := dbus_connection_pop_message(Connection);
      end;
      Application.ProcessMessages;
    end;
    if URI = '' then Exit;

    // The portal wrote the picture to a file of its own for us; it is ours to remove.
    if not URIToFilename(URI, FileName) then Exit;
    Picture := TPicture.Create;
    try
      try
        Picture.LoadFromFile(FileName);
        Destination.Assign(Picture.Graphic);
        Result := True;
        Message := '';
      except
        on E: Exception do Message := E.Message;
      end;
    finally
      Picture.Free;
      DeleteFile(FileName);
    end;
  finally
    dbus_connection_close(Connection);
    dbus_connection_unref(Connection);
  end;
end;

end.
