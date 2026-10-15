unit LinuxScreenshot;

{$mode objfpc}{$H+}
// Copyright (c) 2026 Tony Stone. 0BSD.

{ A picture of the desktop from org.freedesktop.portal.Screenshot - the way a
  Wayland session hands one out, since a program there cannot read the screen
  itself.  The desktop may ask the user for permission the first time. }

interface

uses Graphics;

function PortalScreenshot(Destination: TBitmap; out Message: string): boolean;

implementation

uses SysUtils, URIParser, dbus, LinuxPortal;

function PortalScreenshot(Destination: TBitmap; out Message: string): boolean;
var
  Connection: PDBusConnection;
  Call: PDBusMessage;
  Args, Options: DBusMessageIter;
  Parent: PChar;
  URI, FileName: string;
  Picture: TPicture;
begin
  Result := False;
  Message := 'The desktop portal is not available.';
  Connection := PortalConnect;
  if Connection = nil then Exit;
  try
    // Screenshot(parent window, options); the answer holds the picture's address
    Call := dbus_message_new_method_call(PortalName, PortalPath, 'org.freedesktop.portal.Screenshot', 'Screenshot');
    if Call = nil then Exit;
    Parent := '';
    dbus_message_iter_init_append(Call, @Args);
    dbus_message_iter_append_basic(@Args, DBUS_TYPE_STRING, @Parent);
    dbus_message_iter_open_container(@Args, DBUS_TYPE_ARRAY, '{sv}', @Options);
    dbus_message_iter_close_container(@Args, @Options);
    Message := 'The desktop refused the screenshot, or did not answer.';
    if not PortalAsk(Connection, Call, 60000, 'uri', URI) then Exit;

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
    PortalClose(Connection);
  end;
end;

end.
