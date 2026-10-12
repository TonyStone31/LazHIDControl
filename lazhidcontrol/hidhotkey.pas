unit HIDHotkey;
{$mode objfpc}{$H+}
{$if defined(windows) or (defined(linux) and (defined(LCLgtk2) or defined(LCLgtk3)))}{$define HOTKEYBACKEND}{$endif}
// Copyright (c) 2026 Tony Stone. GPL-2.0-or-later; see COPYING.GPL2.
interface
uses Classes, SysUtils, LCLType, Menus, HotkeyInputIntf;
type
  THIDHotkeyErrorEvent = procedure(Sender: TObject; const Message: string) of object;
  THIDHotkey = class(TComponent)
  private
    ShortcutValue: TShortCut;
    Requested: Boolean;
    Binding: THotkey;
    Handler: TNotifyEvent;
    ErrorHandler: THIDHotkeyErrorEvent;
    ErrorText: string;
    procedure SetShortCut(Value: TShortCut);
    procedure SetEnabled(Value: Boolean);
    function GetRegistered: Boolean;
  protected
    procedure KeyPressed(Sender: TObject; Key: Word; Shift: TShiftState);
    procedure Loaded; override;
    function CreateBinding(Key: Word; Shift: TShiftState): THotkey; virtual;
  public
    destructor Destroy; override;
    function RegisterHotkey: Boolean;
    procedure UnregisterHotkey;
    property Registered: Boolean read GetRegistered;
    property LastError: string read ErrorText;
  published
    // Global shortcut, such as Ctrl+Shift+F9. Supported modifiers: Ctrl, Alt, Shift.
    property ShortCut: TShortCut read ShortcutValue write SetShortCut default 0;
    // Registers after the form loads; never in the designer. Registered reports success.
    property Enabled: Boolean read Requested write SetEnabled default False;
    property OnHotkey: TNotifyEvent read Handler write Handler;
    // An unavailable shortcut or backend; the message is also in LastError.
    property OnError: THIDHotkeyErrorEvent read ErrorHandler write ErrorHandler;
  end;
implementation
{$ifdef HOTKEYBACKEND}uses HotkeyInput;{$endif}
function THIDHotkey.CreateBinding(Key: Word; Shift: TShiftState): THotkey;
begin
  {$ifdef HOTKEYBACKEND}Result := CreateHotkey(Key, Shift, @KeyPressed);
  {$else}Result := nil;{$endif}
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
procedure THIDHotkey.SetEnabled(Value: Boolean);
begin
  Requested := Value;
  if (csLoading in ComponentState) or (csDesigning in ComponentState) then Exit;
  if Value then RegisterHotkey else UnregisterHotkey;
end;
function THIDHotkey.GetRegistered: Boolean;
begin Result := Assigned(Binding) and Binding.Registered; end;
procedure THIDHotkey.KeyPressed(Sender: TObject; Key: Word; Shift: TShiftState);
begin
  if Assigned(Handler) then Handler(Self);
end;
function THIDHotkey.RegisterHotkey: Boolean;
var Key: Word; Shift: TShiftState;
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
  else try
    Binding := CreateBinding(Key, Shift);
    if Assigned(Binding) then Binding.Register;
    Result := Registered;
    if not Result then ErrorText := 'The shortcut is unavailable or unsupported on this platform.';
  except
    on E: Exception do ErrorText := E.Message;
  end;
  if not Result then begin
    FreeAndNil(Binding);
    if Assigned(ErrorHandler) then ErrorHandler(Self, LastError);
  end;
end;
procedure THIDHotkey.UnregisterHotkey;
begin Requested := False; FreeAndNil(Binding); end;
end.
