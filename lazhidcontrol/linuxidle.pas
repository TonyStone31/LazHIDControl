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
// Callers poll every second, so the library and display connection are kept.
var
  XssLib: TLibHandle = NilHandle;
  XssTried: Boolean = False;
  XssQuery: TXIdleQuery = nil;
  IdleDisplay: PDisplay = nil;
function QueryXIdle(out Milliseconds: QWord; out Message: string): Boolean;
var Info: TXIdleInfo;
begin
  Result := False;
  if not XssTried then begin
    XssTried := True;
    XssLib := LoadLibrary('libXss.so.1');
    if XssLib <> NilHandle then
      Pointer(XssQuery) := GetProcedureAddress(XssLib, 'XScreenSaverQueryInfo');
  end;
  if not Assigned(XssQuery) then begin Message := 'X11 idle detection requires libXss.so.1.'; Exit; end;
  if IdleDisplay = nil then IdleDisplay := XOpenDisplay(nil);
  if IdleDisplay = nil then begin Message := 'Cannot connect to the X11 display.'; Exit; end;
  Info := Default(TXIdleInfo);
  Result := XssQuery(IdleDisplay, XDefaultRootWindow(IdleDisplay), @Info) <> 0;
  if Result then Milliseconds := Info.Idle
  else Message := 'The X11 screen-saver extension could not report idle time.';
end;

// GNOME's IdleMonitor answers in milliseconds; the freedesktop ScreenSaver
// interface, which KDE provides, answers in whole seconds.
function QueryBusIdle(const Dest, Path, Intf, Method: string; out Milliseconds: QWord): Boolean;
var Connection: PDBusConnection; Request, Reply: PDBusMessage;
  Error: DBusError; Iter: DBusMessageIter; Seconds: dbus_uint32_t;
begin
  Result := False;
  dbus_error_init(@Error);
  // The shared connection; unref it, never close it.
  Connection := dbus_bus_get(DBUS_BUS_SESSION, @Error);
  try
    if Connection = nil then Exit;
    dbus_connection_set_exit_on_disconnect(Connection, 0);
    Request := dbus_message_new_method_call(PChar(Dest), PChar(Path), PChar(Intf), PChar(Method));
    if Request = nil then Exit;
    try
      // Query an existing desktop service; never launch one ourselves.
      dbus_message_set_auto_start(Request, 0);
      Reply := dbus_connection_send_with_reply_and_block(Connection, Request, 200, @Error);
      if Reply = nil then Exit;
      try
        if dbus_message_iter_init(Reply, @Iter) <> 0 then
          case dbus_message_iter_get_arg_type(@Iter) of
            DBUS_TYPE_UINT64: begin dbus_message_iter_get_basic(@Iter, @Milliseconds); Result := True; end;
            DBUS_TYPE_UINT32: begin
              dbus_message_iter_get_basic(@Iter, @Seconds);
              Milliseconds := QWord(Seconds) * 1000;
              Result := True;
            end;
          end;
      finally dbus_message_unref(Reply); end;
    finally dbus_message_unref(Request); end;
  finally
    if Connection <> nil then dbus_connection_unref(Connection);
    dbus_error_free(@Error);
  end;
end;

function QueryLinuxIdle(out Milliseconds: QWord; out Message: string): Boolean;
begin
  Milliseconds := 0;
  Message := '';
  if GetEnvironmentVariable('WAYLAND_DISPLAY') = '' then begin
    Result := QueryXIdle(Milliseconds, Message);
    if Result then Exit;
  end;
  Result := QueryBusIdle('org.gnome.Mutter.IdleMonitor', '/org/gnome/Mutter/IdleMonitor/Core',
      'org.gnome.Mutter.IdleMonitor', 'GetIdletime', Milliseconds)
    or QueryBusIdle('org.freedesktop.ScreenSaver', '/org/freedesktop/ScreenSaver',
      'org.freedesktop.ScreenSaver', 'GetSessionIdleTime', Milliseconds);
  if Result then Message := ''
  else if GetEnvironmentVariable('WAYLAND_DISPLAY') <> '' then
    Message := 'No supported Wayland idle service answered; GNOME and KDE are supported.';
end;
finalization
  if IdleDisplay <> nil then XCloseDisplay(IdleDisplay);
  if XssLib <> NilHandle then UnloadLibrary(XssLib);
end.
