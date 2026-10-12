unit LinuxIdle;
{$mode objfpc}{$H+}
// Copyright (c) 2026 Tony Stone. GPL-2.0-or-later; see COPYING.GPL2.
interface
function QueryLinuxIdle(out Milliseconds: QWord; out Message: string): Boolean;
implementation
uses SysUtils, DynLibs, ctypes, X, XLib, dbus;
{$push}{$packrecords c}
type
  TXIdleInfo = record
    Window: TWindow;
    State, Kind: cint;
    Since, Idle: culong;
    EventMask: culong;
  end;
  TXIdleQuery = function(Display: PDisplay; Window: TDrawable; Info: Pointer): TStatus; cdecl;
{$pop}
function QueryXIdle(out Milliseconds: QWord; out Message: string): Boolean;
var LibraryHandle: TLibHandle; Query: TXIdleQuery; Display: PDisplay; Info: TXIdleInfo;
begin
  Result := False;
  LibraryHandle := LoadLibrary('libXss.so.1');
  if LibraryHandle = NilHandle then begin Message := 'X11 idle detection requires libXss.so.1.'; Exit; end;
  try
    Pointer(Query) := GetProcedureAddress(LibraryHandle, 'XScreenSaverQueryInfo');
    if not Assigned(Query) then begin Message := 'The X11 idle query is unavailable.'; Exit; end;
    Display := XOpenDisplay(nil);
    if Display = nil then begin Message := 'Cannot connect to the X11 display.'; Exit; end;
    try
      Info := Default(TXIdleInfo);
      Result := Query(Display, XDefaultRootWindow(Display), @Info) <> 0;
      if Result then Milliseconds := Info.Idle
      else Message := 'The X11 screen-saver extension could not report idle time.';
    finally XCloseDisplay(Display); end;
  finally UnloadLibrary(LibraryHandle); end;
end;

function QueryGnomeIdle(out Milliseconds: QWord): Boolean;
var Connection: PDBusConnection; Request, Reply: PDBusMessage;
  Error: DBusError; Iter: DBusMessageIter;
begin
  Result := False;
  dbus_error_init(@Error);
  Connection := dbus_bus_get_private(DBUS_BUS_SESSION, @Error);
  try
    if Connection = nil then Exit;
    dbus_connection_set_exit_on_disconnect(Connection, 0);
    try
      Request := dbus_message_new_method_call('org.gnome.Mutter.IdleMonitor',
        '/org/gnome/Mutter/IdleMonitor/Core', 'org.gnome.Mutter.IdleMonitor', 'GetIdletime');
      if Request = nil then Exit;
      try
        // Query an existing desktop service; never launch one ourselves.
        dbus_message_set_auto_start(Request, 0);
        Reply := dbus_connection_send_with_reply_and_block(Connection, Request, 200, @Error);
        if Reply = nil then Exit;
        try
          Result := (dbus_message_iter_init(Reply, @Iter) <> 0) and
            (dbus_message_iter_get_arg_type(@Iter) = DBUS_TYPE_UINT64);
          if Result then dbus_message_iter_get_basic(@Iter, @Milliseconds);
        finally dbus_message_unref(Reply); end;
      finally dbus_message_unref(Request); end;
    finally dbus_connection_close(Connection); dbus_connection_unref(Connection); end;
  finally dbus_error_free(@Error); end;
end;

function QueryLinuxIdle(out Milliseconds: QWord; out Message: string): Boolean;
begin
  Milliseconds := 0;
  Message := '';
  if GetEnvironmentVariable('WAYLAND_DISPLAY') = '' then begin
    Result := QueryXIdle(Milliseconds, Message);
    if Result then Exit;
  end;
  Result := QueryGnomeIdle(Milliseconds);
  if Result then Message := ''
  else if GetEnvironmentVariable('WAYLAND_DISPLAY') <> '' then
    Message := 'No supported Wayland idle service is available. GNOME IdleMonitor is supported; native idle-notify is not implemented.';
end;
end.
