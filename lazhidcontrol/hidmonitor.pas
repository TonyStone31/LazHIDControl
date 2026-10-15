unit HIDMonitor;
{$mode objfpc}{$H+}
// Copyright (c) 2026 Tony Stone. 0BSD.
interface

uses Classes, SysUtils, Controls, GlobalKeyMonitor, GlobalMouseMonitor{$ifdef linux}, EvdevMonitor{$endif};

type
  THIDMonitorKeyEvent = procedure(Sender: TObject; Key: word; Shift: TShiftState) of object;
  THIDMonitorClickEvent = procedure(Sender: TObject; Button: TMouseButton; X, Y: integer) of object;
  THIDMonitorErrorEvent = procedure(Sender: TObject; const Message: string) of object;

  { Key presses and mouse clicks anywhere on the system, not only in your own
    windows: for recording what somebody does so it can be played back, or
    for seeing what a key really sends. }
  THIDInputMonitor = class(TComponent)
  private
    KeyListener: TGlobalKeyListener;
    ClickListener: TGlobalMouseListener;
    Requested, WantKeys, WantClicks: boolean;
    ErrorText: string;
    KeyHandler: THIDMonitorKeyEvent;
    ClickHandler: THIDMonitorClickEvent;
    ErrorHandler: THIDMonitorErrorEvent;
    procedure SetEnabled(Value: boolean);
    procedure KeyPressed(Key: word; Shift: TShiftState);
    procedure Clicked(Button: TMouseButton; X, Y: integer);
    procedure Fail(const Message: string);
  protected
    procedure Loaded; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    // Whether each half is really running; the system may allow one and not the other.
    function WatchingKeys: boolean;
    function WatchingClicks: boolean;
    property LastError: string read ErrorText;
  published
    // True starts watching once the form loads. Nothing runs in the designer.
    property Enabled: boolean read Requested write SetEnabled default False;
    // Watch the keyboard. Set before enabling. On macOS the user must allow
    // the program under Privacy & Security > Input Monitoring. On Wayland the
    // keys come from the kernel's input devices, which takes a udev rule that
    // THIDPermission installs; letters are then reported as a US keyboard has them.
    property WatchKeys: boolean read WantKeys write WantKeys default True;
    // Watch mouse buttons. Set before enabling. Not on Wayland, which tells
    // no program where the pointer is: OnError says so.
    property WatchClicks: boolean read WantClicks write WantClicks default True;
    // A key went down somewhere. Key is an LCL virtual key (VK_...), Shift the
    // modifiers held with it. Runs on the main thread. Keep passwords in mind:
    // this sees everything typed.
    property OnKeyPress: THIDMonitorKeyEvent read KeyHandler write KeyHandler;
    // A mouse button went down somewhere; X and Y are screen coordinates.
    property OnMouseClick: THIDMonitorClickEvent read ClickHandler write ClickHandler;
    // Watching could not start, or only half of it could. The message also
    // stays in LastError.
    property OnError: THIDMonitorErrorEvent read ErrorHandler write ErrorHandler;
  end;

implementation

constructor THIDInputMonitor.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  WantKeys := True;
  WantClicks := True;
end;

destructor THIDInputMonitor.Destroy;
begin
  SetEnabled(False);
  inherited Destroy;
end;

procedure THIDInputMonitor.Loaded;
begin
  inherited Loaded;
  if Enabled then SetEnabled(True);
end;

procedure THIDInputMonitor.Fail(const Message: string);
begin
  if ErrorText <> '' then ErrorText := ErrorText + ' ';
  ErrorText := ErrorText + Message;
end;

{ What stopped it, on this platform. }
function Blocked(const What: string): string;
begin
  {$if defined(darwin)}
  if What = 'Keys' then Exit('Keys cannot be watched until this program is allowed under Privacy & Security > Input Monitoring.');
  {$elseif defined(linux)}
  if (What = 'Mouse clicks') and (GetEnvironmentVariable('WAYLAND_DISPLAY') <> '') then
    Exit('Mouse clicks cannot be watched on Wayland: the desktop tells no program where the pointer is, so a click could not say where it was.');
  if GetEnvironmentVariable('WAYLAND_DISPLAY') <> '' then Exit(What + ' cannot be watched. ' + EvdevHelp);
  {$endif}
  Result := What + ' cannot be watched in this session.';
end;

procedure THIDInputMonitor.SetEnabled(Value: boolean);
begin
  Requested := Value;
  if (csLoading in ComponentState) or (csDesigning in ComponentState) then Exit;
  FreeAndNil(KeyListener);
  FreeAndNil(ClickListener);
  ErrorText := '';
  if not Value then Exit;
  if WatchKeys then
  begin
    KeyListener := CreateGlobalKeyListener;
    if Assigned(KeyListener) then
    begin
      KeyListener.OnKeyPress := @KeyPressed;
      KeyListener.StartMonitoring;
    end;
    if not WatchingKeys then Fail(Blocked('Keys'));
  end;
  if WatchClicks then
  begin
    ClickListener := CreateGlobalMouseListener;
    if Assigned(ClickListener) then
    begin
      ClickListener.OnMouseClick := @Clicked;
      ClickListener.StartMonitoring;
    end;
    if not WatchingClicks then Fail(Blocked('Mouse clicks'));
  end;
  if (ErrorText <> '') and Assigned(ErrorHandler) then ErrorHandler(Self, ErrorText);
end;

function THIDInputMonitor.WatchingKeys: boolean;
begin
  Result := Assigned(KeyListener) and KeyListener.Active;
end;

function THIDInputMonitor.WatchingClicks: boolean;
begin
  Result := Assigned(ClickListener) and ClickListener.Active;
end;

procedure THIDInputMonitor.KeyPressed(Key: word; Shift: TShiftState);
begin
  if Assigned(KeyHandler) then KeyHandler(Self, Key, Shift);
end;

procedure THIDInputMonitor.Clicked(Button: TMouseButton; X, Y: integer);
begin
  if Assigned(ClickHandler) then ClickHandler(Self, Button, X, Y);
end;

end.
