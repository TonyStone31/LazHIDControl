unit HIDClipboard;
{$mode objfpc}{$H+}
{$ifdef darwin}{$modeswitch objectivec1}{$endif}
// Copyright (c) 2026 Tony Stone. 0BSD.
interface

uses Classes, SysUtils, ExtCtrls;

type
  THIDClipboardErrorEvent = procedure(Sender: TObject; const Message: string) of object;

  THIDClipboardMonitor = class(TComponent)
  private
    Poll: TTimer;
    Requested, Known: boolean;
    LastSerial, Serial: QWord;
    NativeClipboard: Pointer;
    SignalID: PtrUInt;
    XDisplay: Pointer;
    XEventBase: integer;
    ErrorText: string;
    ChangeHandler: TNotifyEvent;
    ErrorHandler: THIDClipboardErrorEvent;
    procedure SetEnabled(Value: boolean);
    procedure Disconnect;
    function WatchX: boolean;
    procedure WatchGtk;
    procedure Tick(Sender: TObject);
    function GetInterval: cardinal;
    procedure SetInterval(Value: cardinal);
  protected
    procedure Loaded; override;
    function QuerySerial(out Value: QWord; out Message: string): boolean; virtual;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    // Detects a change without reading contents; read data with the LCL Clipboard.
    function Check: boolean;
    property LastError: string read ErrorText;
  published
    // True watches the clipboard once the form loads. Nothing runs in the designer.
    property Enabled: boolean read Requested write SetEnabled default False;
    // Milliseconds between change checks. Default 250; several changes inside
    // one interval can land as a single event.
    property PollInterval: cardinal read GetInterval write SetInterval default 250;
    // Something new is on the clipboard, text or not; read it with the LCL
    // Clipboard if wanted. Enabling takes the contents already there as the
    // baseline without firing.
    property OnChanged: TNotifyEvent read ChangeHandler write ChangeHandler;
    // The clipboard query failed, which also disables monitoring; the message
    // stays in LastError.
    property OnError: THIDClipboardErrorEvent read ErrorHandler write ErrorHandler;
  end;

implementation

uses
  {$if defined(windows)}
  Windows;
  {$elseif defined(darwin)}
  CocoaAll;
  {$else}
  ctypes, dynlibs, X, XLib
  {$if defined(LCLgtk2)}, glib2, gdk2, gtk2{$endif}
  {$if defined(LCLgtk3)}, LazGtk3, LazGdk3, LazGObject2{$endif};
  {$endif}

  {$ifdef windows}
function GetClipboardSequenceNumber: DWORD; stdcall;
  external 'user32.dll' name 'GetClipboardSequenceNumber';
  {$endif}
{$if not defined(windows) and not defined(darwin)}
const
  XFixesSelectionNotify = 0;
  XFixesSetSelectionOwnerNotifyMask = 1;

var
  XFixesLib: TLibHandle = NilHandle;
  XFixesQueryExtension: function(Dpy: PDisplay; EventBase, ErrorBase: Pcint): TBool; cdecl;
  XFixesSelectSelectionInput: procedure(Dpy: PDisplay; Win: TWindow; Selection: TAtom; Mask: culong); cdecl;

// Loaded when first wanted, so building needs no X development files.
function LoadXFixes: boolean;
begin
  if XFixesLib = NilHandle then
  begin
    XFixesLib := LoadLibrary('libXfixes.so.3');
    if XFixesLib = NilHandle then XFixesLib := LoadLibrary('libXfixes.so');
    if XFixesLib <> NilHandle then
    begin
      Pointer(XFixesQueryExtension) := GetProcAddress(XFixesLib, 'XFixesQueryExtension');
      Pointer(XFixesSelectSelectionInput) := GetProcAddress(XFixesLib, 'XFixesSelectSelectionInput');
    end;
  end;
  Result := Assigned(XFixesQueryExtension) and Assigned(XFixesSelectSelectionInput);
end;
{$endif}
{$if defined(LCLgtk2) or defined(LCLgtk3)}
procedure ClipboardChanged(Clipboard, Event, Data: Pointer); cdecl;
begin Inc(THIDClipboardMonitor(Data).Serial); end;
{$endif}

constructor THIDClipboardMonitor.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  Poll := TTimer.Create(Self);
  Poll.Enabled := False;
  Poll.Interval := 250;
  Poll.OnTimer := @Tick;
end;

destructor THIDClipboardMonitor.Destroy;
begin
  Disconnect;
  inherited Destroy;
end;

procedure THIDClipboardMonitor.Disconnect;
begin
  {$if defined(LCLgtk2) or defined(LCLgtk3)}
  if (NativeClipboard <> nil) and (SignalID <> 0) then
    g_signal_handler_disconnect(NativeClipboard, SignalID);
  {$endif}
  {$if not defined(windows) and not defined(darwin)}
  if XDisplay <> nil then XCloseDisplay(XDisplay);
  {$endif}
  NativeClipboard := nil;
  SignalID := 0;
  XDisplay := nil;
end;

// X11 under any widgetset: a connection of our own, told by XFixes whenever
// the clipboard gets a new owner.
function THIDClipboardMonitor.WatchX: boolean;
{$if not defined(windows) and not defined(darwin)}
var
  EventBase, ErrorBase: cint;
begin
  Result := False;
  if not LoadXFixes then Exit;
  XDisplay := XOpenDisplay(nil);
  if XDisplay = nil then Exit;
  if XFixesQueryExtension(XDisplay, @EventBase, @ErrorBase) <> 0 then
  begin
    XEventBase := EventBase;
    XFixesSelectSelectionInput(XDisplay, DefaultRootWindow(XDisplay), XInternAtom(XDisplay, 'CLIPBOARD', False), XFixesSetSelectionOwnerNotifyMask);
    XFlush(XDisplay);
    Exit(True);
  end;
  XCloseDisplay(XDisplay);
  XDisplay := nil;
end;
{$else}
begin
  Result := False;
end;
{$endif}

// GTK's own signal, for a GTK program on Wayland with no X server to ask.
procedure THIDClipboardMonitor.WatchGtk;
begin
  {$if defined(LCLgtk2) or defined(LCLgtk3)}
  NativeClipboard := gtk_clipboard_get(gdk_atom_intern('CLIPBOARD', False));
  if NativeClipboard <> nil then
    {$ifdef LCLgtk2}
    SignalID := g_signal_connect(NativeClipboard, 'owner-change', TGCallback(@ClipboardChanged), Self);
    {$else}
    SignalID := g_signal_connect_data(NativeClipboard, 'owner-change', TGCallback(@ClipboardChanged), Self, nil, []);
    {$endif}
  {$endif}
end;

procedure THIDClipboardMonitor.Loaded;
begin
  inherited Loaded;
  if Enabled then SetEnabled(True);
end;

procedure THIDClipboardMonitor.SetEnabled(Value: boolean);
begin
  Requested := Value;
  Poll.Enabled := False;
  Known := False;
  if (csLoading in ComponentState) or (csDesigning in ComponentState) then Exit;
  Disconnect;
  if Value and Check then Poll.Enabled := Enabled;
end;

function THIDClipboardMonitor.GetInterval: cardinal;
begin
  Result := Poll.Interval;
end;

procedure THIDClipboardMonitor.SetInterval(Value: cardinal);
begin
  if Value = 0 then raise ERangeError.Create('PollInterval must be greater than zero.');
  Poll.Interval := Value;
end;

procedure THIDClipboardMonitor.Tick(Sender: TObject);
begin
  Check;
end;

function THIDClipboardMonitor.Check: boolean;
var
  Value: QWord;
  Message: string;
  Changed: boolean;
begin
  if csDesigning in ComponentState then Exit(False);
  Result := QuerySerial(Value, Message);
  if not Result then
  begin
    SetEnabled(False);
    ErrorText := Message;
    if Assigned(ErrorHandler) then ErrorHandler(Self, Message);
    Exit;
  end;
  Changed := Known and (Value <> LastSerial);
  LastSerial := Value;
  Known := True;
  ErrorText := '';
  if Changed and Assigned(ChangeHandler) then ChangeHandler(Self);
end;

function THIDClipboardMonitor.QuerySerial(out Value: QWord; out Message: string): boolean;
  {$if not defined(windows) and not defined(darwin)}
var
  Event: TXEvent;
  {$endif}
begin
  Value := 0;
  Message := '';
  {$if defined(windows)}
  Value := GetClipboardSequenceNumber;
  Result := True;
  {$elseif defined(darwin)}
  Value := NSPasteboard.generalPasteboard.changeCount;
  Result := True;
  {$else}
  // XWayland passes on Wayland clipboard changes too; GTK's signal only
  // reports them while the program has the focus
  if (XDisplay = nil) and (SignalID = 0) and not WatchX then WatchGtk;
  if XDisplay <> nil then
    while XPending(XDisplay) > 0 do
    begin
      XNextEvent(XDisplay, @Event);
      if Event._type = XEventBase + XFixesSelectionNotify then Inc(Serial);
    end;
  Result := (XDisplay <> nil) or (SignalID <> 0);
  Value := Serial;
  if not Result then Message := 'Cannot watch the clipboard: there is no X display, and this widgetset offers no other way.';
  {$endif}
end;

end.
