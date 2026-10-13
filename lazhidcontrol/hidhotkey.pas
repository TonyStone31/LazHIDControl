unit HIDHotkey;
{$mode objfpc}{$H+}
{$if defined(windows) or defined(darwin) or (defined(linux) and (defined(LCLgtk2) or defined(LCLgtk3)))}{$define HOTKEYBACKEND}{$endif}
// Copyright (c) 2026 Tony Stone. GPL-2.0-or-later; see COPYING.GPL2.
interface

uses Classes, SysUtils, LCLType, Menus, HotkeyInputIntf;

type
  THIDHotkeyErrorEvent = procedure(Sender: TObject; const Message: string) of object;

  THIDHotkey = class(TComponent)
  private
    ShortcutValue: TShortCut;
    Requested: boolean;
    Binding: THotkey;
    Handler: TNotifyEvent;
    ErrorHandler: THIDHotkeyErrorEvent;
    ErrorText: string;
    procedure SetShortCut(Value: TShortCut);
    procedure SetEnabled(Value: boolean);
    function GetRegistered: boolean;
  protected
    procedure KeyPressed(Sender: TObject; Key: word; Shift: TShiftState);
    procedure Loaded; override;
    function CreateBinding(Key: word; Shift: TShiftState): THotkey; virtual;
  public
    destructor Destroy; override;
    function RegisterHotkey: boolean;
    procedure UnregisterHotkey;
    property Registered: boolean read GetRegistered;
    property LastError: string read ErrorText;
  published
    // The key combination to register globally, so it works while any
    // application has the focus. Ctrl, Alt, and Shift are the supported modifiers.
    property ShortCut: TShortCut read ShortcutValue write SetShortCut default 0;
    // True registers the shortcut once the form loads and keeps it while the
    // program runs. Nothing registers in the designer; read Registered for the outcome.
    property Enabled: boolean read Requested write SetEnabled default False;
    // Runs on the GUI thread each time the combination is pressed, in whatever
    // application. The modifier keys may still be physically held at that moment.
    property OnHotkey: TNotifyEvent read Handler write Handler;
    // The shortcut could not be registered - already taken, or no backend for
    // this platform. The message stays in LastError; no dialog is shown.
    property OnError: THIDHotkeyErrorEvent read ErrorHandler write ErrorHandler;
  end;

implementation

{$ifdef HOTKEYBACKEND}uses HotkeyInput;{$endif}

function THIDHotkey.CreateBinding(Key: word; Shift: TShiftState): THotkey;
begin
  {$ifdef HOTKEYBACKEND}
Result := CreateHotkey(Key, Shift, @KeyPressed);
  {$else}
  Result := nil;
  {$endif}
end;

destructor THIDHotkey.Destroy;
begin
  UnregisterHotkey;
  inherited Destroy;
end;

procedure THIDHotkey.Loaded;
begin
  inherited Loaded;
  if Enabled then RegisterHotkey;
end;

procedure THIDHotkey.SetShortCut(Value: TShortCut);
begin
  if Value = ShortcutValue then Exit;
  FreeAndNil(Binding);
  ShortcutValue := Value;
  if Enabled and not (csLoading in ComponentState) then RegisterHotkey;
end;

procedure THIDHotkey.SetEnabled(Value: boolean);
begin
  Requested := Value;
  if (csLoading in ComponentState) or (csDesigning in ComponentState) then Exit;
  if Value then RegisterHotkey
  else
    UnregisterHotkey;
end;

function THIDHotkey.GetRegistered: boolean;
begin
  Result := Assigned(Binding) and Binding.Registered;
end;

procedure THIDHotkey.KeyPressed(Sender: TObject; Key: word; Shift: TShiftState);
begin
  if Assigned(Handler) then Handler(Self);
end;

function THIDHotkey.RegisterHotkey: boolean;
var
  Key: word;
  Shift: TShiftState;
begin
  Result := False;
  if csDesigning in ComponentState then Exit;
  Requested := True;
  if Registered then Exit(True);
  ErrorText := '';
  FreeAndNil(Binding);
  ShortCutToKey(ShortCut, Key, Shift);
  if (Key = 0) or (Shift - [ssCtrl, ssAlt, ssShift] <> []) then
    ErrorText := 'Choose a valid shortcut; supported modifiers are Ctrl, Alt, and Shift.'
  else
  try
    Binding := CreateBinding(Key, Shift);
    if Assigned(Binding) then Binding.Register;
    Result := Registered;
    if not Result then ErrorText := 'The shortcut is unavailable or unsupported on this platform.';
  except
    on E: Exception do ErrorText := E.Message;
  end;
  if not Result then
  begin
    FreeAndNil(Binding);
    if Assigned(ErrorHandler) then ErrorHandler(Self, LastError);
  end;
end;

procedure THIDHotkey.UnregisterHotkey;
begin
  Requested := False;
  FreeAndNil(Binding);
end;

end.
