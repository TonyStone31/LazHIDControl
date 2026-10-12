unit HIDClipboard;
{$mode objfpc}{$H+}
{$ifdef darwin}{$modeswitch objectivec1}{$endif}
// Copyright (c) 2026 Tony Stone. GPL-2.0-or-later; see COPYING.GPL2.
interface
uses Classes, SysUtils, ExtCtrls;
type
  THIDClipboardErrorEvent = procedure(Sender: TObject; const Message: string) of object;
  THIDClipboardMonitor = class(TComponent)
  private
    Poll: TTimer;
    Requested, Known: Boolean;
    LastSerial, Serial: QWord;
    NativeClipboard: Pointer;
    SignalID: PtrUInt;
    ErrorText: string;
    ChangeHandler: TNotifyEvent;
    ErrorHandler: THIDClipboardErrorEvent;
    procedure SetEnabled(Value: Boolean);
    procedure Disconnect;
    procedure Tick(Sender: TObject);
    function GetInterval: Cardinal;
    procedure SetInterval(Value: Cardinal);
  protected
    procedure Loaded; override;
    function QuerySerial(out Value: QWord; out Message: string): Boolean; virtual;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    // Detects a change without reading contents; read data with the LCL Clipboard.
    function Check: Boolean;
    property LastError: string read ErrorText;
  published
    // True watches the clipboard once the form loads. Nothing runs in the designer.
    property Enabled: Boolean read Requested write SetEnabled default False;
    // Milliseconds between change checks. Default 250; several changes inside
    // one interval can land as a single event.
    property PollInterval: Cardinal read GetInterval write SetInterval default 250;
    // Something new is on the clipboard, text or not; read it with the LCL
    // Clipboard if wanted. Enabling takes the contents already there as the
    // baseline without firing.
    property OnChanged: TNotifyEvent read ChangeHandler write ChangeHandler;
    // The clipboard query failed, which also disables monitoring; the message
    // stays in LastError.
    property OnError: THIDClipboardErrorEvent read ErrorHandler write ErrorHandler;
  end;
implementation
{$if defined(windows)}uses Windows;
{$elseif defined(darwin)}uses CocoaAll;
{$elseif defined(LCLgtk2)}uses glib2, gdk2, gtk2;
{$elseif defined(LCLgtk3)}uses LazGtk3, LazGdk3, LazGObject2;
{$endif}
{$ifdef windows}
function GetClipboardSequenceNumber: DWORD; stdcall;
  external 'user32.dll' name 'GetClipboardSequenceNumber';
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
begin Disconnect; inherited Destroy; end;
procedure THIDClipboardMonitor.Disconnect;
begin
  {$if defined(LCLgtk2) or defined(LCLgtk3)}
  if (NativeClipboard <> nil) and (SignalID <> 0) then
    g_signal_handler_disconnect(NativeClipboard, SignalID);
  {$endif}
  NativeClipboard := nil;
  SignalID := 0;
end;
procedure THIDClipboardMonitor.Loaded;
begin inherited Loaded; if Enabled then SetEnabled(True); end;
procedure THIDClipboardMonitor.SetEnabled(Value: Boolean);
begin
  Requested := Value;
  Poll.Enabled := False;
  Known := False;
  if (csLoading in ComponentState) or (csDesigning in ComponentState) then Exit;
  Disconnect;
  if Value and Check then Poll.Enabled := Enabled;
end;
function THIDClipboardMonitor.GetInterval: Cardinal;
begin Result := Poll.Interval; end;
procedure THIDClipboardMonitor.SetInterval(Value: Cardinal);
begin
  if Value = 0 then raise ERangeError.Create('PollInterval must be greater than zero.');
  Poll.Interval := Value;
end;
procedure THIDClipboardMonitor.Tick(Sender: TObject);
begin Check; end;
function THIDClipboardMonitor.Check: Boolean;
var Value: QWord; Message: string; Changed: Boolean;
begin
  if csDesigning in ComponentState then Exit(False);
  Result := QuerySerial(Value, Message);
  if not Result then begin
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
function THIDClipboardMonitor.QuerySerial(out Value: QWord; out Message: string): Boolean;
begin
  Value := 0;
  Message := '';
  {$if defined(windows)}
  Value := GetClipboardSequenceNumber;
  Result := True;
  {$elseif defined(darwin)}
  Value := NSPasteboard.generalPasteboard.changeCount;
  Result := True;
  {$elseif defined(LCLgtk2)}
  if NativeClipboard = nil then begin
    NativeClipboard := gtk_clipboard_get(gdk_atom_intern('CLIPBOARD', False));
    if NativeClipboard <> nil then
      SignalID := g_signal_connect(NativeClipboard, 'owner-change', TGCallback(@ClipboardChanged), Self);
  end;
  Result := (NativeClipboard <> nil) and (SignalID <> 0);
  Value := Serial;
  if not Result then Message := 'Cannot subscribe to GTK clipboard changes.';
  {$elseif defined(LCLgtk3)}
  if NativeClipboard = nil then begin
    NativeClipboard := gtk_clipboard_get(gdk_atom_intern('CLIPBOARD', False));
    if NativeClipboard <> nil then
      SignalID := g_signal_connect_data(NativeClipboard, 'owner-change', TGCallback(@ClipboardChanged), Self, nil, []);
  end;
  Result := (NativeClipboard <> nil) and (SignalID <> 0);
  Value := Serial;
  if not Result then Message := 'Cannot subscribe to GTK clipboard changes.';
  {$else}
  Result := False;
  Message := 'Clipboard change monitoring is not implemented for this widgetset.';
  {$endif}
end;
end.
