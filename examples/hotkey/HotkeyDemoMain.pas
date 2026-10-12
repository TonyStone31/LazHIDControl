unit HotkeyDemoMain;
{$mode objfpc}{$H+}
// Copyright (c) 2026 Tony Stone. GPL-2.0-or-later; see COPYING.GPL2.
interface
uses Classes, Forms, StdCtrls, HIDHotkey;
type
  THotkeyDemoForm = class(TForm)
    Instructions, Status: TLabel;
    Hotkey: THIDHotkey;
    procedure FormCreate(Sender: TObject);
    procedure HotkeyPressed(Sender: TObject);
    procedure HotkeyError(Sender: TObject; const Message: string);
  end;
var HotkeyDemoForm: THotkeyDemoForm;
implementation
{$R *.lfm}
procedure THotkeyDemoForm.FormCreate(Sender: TObject);
begin
  if Hotkey.Registered then Status.Caption := 'Registered. Try Ctrl+Shift+F9 from another application.';
end;
procedure THotkeyDemoForm.HotkeyPressed(Sender: TObject);
begin
  Status.Caption := 'Hotkey received.';
  if WindowState = wsMinimized then WindowState := wsNormal;
  Show;
  BringToFront;
end;
procedure THotkeyDemoForm.HotkeyError(Sender: TObject; const Message: string);
begin Status.Caption := Message; end;
initialization
  RegisterClass(THIDHotkey);
end.
