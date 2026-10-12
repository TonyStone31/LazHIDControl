unit AutomationDemoMain;
{$mode objfpc}{$H+}
// Copyright (c) 2026 Tony Stone. GPL-2.0-or-later; see COPYING.GPL2.
interface
uses Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, ExtCtrls,
  Spin, WindowManagerIntf, WindowManager, HIDHotkey, HIDInput, HIDScreenshot,
  HIDWindow, HIDSystemIdle, HIDPower, HIDClipboard, PowerInput, Clipbrd;
type
  TAutomationForm = class(TForm)
    InputPanel, CapturePanel, IdlePanel, PowerPanel, ClipboardPanel: TPanel;
    TextLabel, OutputLabel, WindowLabel, MonitorLabel, Status: TLabel;
    TextInput, Output: TEdit;
    WindowInput: TComboBox;
    TypeButton, TypeLaterButton, MouseButton, SampleButton, CaptureButton, SaveButton: TButton;
    DelayInput: TSpinEdit;
    Countdown: TTimer;
    SourceBox: TComboBox;
    MonitorInput: TSpinEdit;
    Preview: TImage;
    Keyboard: THIDKeyboard;
    Pointer: THIDMouse;
    Screenshot: THIDScreenshot;
    Hotkey: THIDHotkey;
    SaveDialog: TSaveDialog;
    Window: THIDWindow;
    SystemIdle: THIDSystemIdle;
    IdleBox: TCheckBox;
    IdleStatus: TLabel;
    FindButton, MoveWindowButton, CopyButton, CopyImageButton, RefreshPowerButton: TButton;
    WindowPointer: THIDMouse;
    WindowX, WindowY: TSpinEdit;
    Power: THIDPowerMonitor;
    ClipboardMonitor: THIDClipboardMonitor;
    PowerBox, ClipboardBox: TCheckBox;
    PowerStatus, ClipboardStatus: TLabel;
    ClipboardText: TEdit;
    PowerLog: TMemo;
    procedure MoveWindowButtonClick(Sender: TObject);
    procedure PowerBoxChange(Sender: TObject);
    procedure PowerChanged(Sender: TObject);
    procedure PowerSuspend(Sender: TObject);
    procedure PowerResume(Sender: TObject);
    procedure PowerError(Sender: TObject; const Message: string);
    procedure RefreshPowerButtonClick(Sender: TObject);
    procedure ClipboardBoxChange(Sender: TObject);
    procedure ClipboardChanged(Sender: TObject);
    procedure ClipboardError(Sender: TObject; const Message: string);
    procedure CopyButtonClick(Sender: TObject);
    procedure CopyImageButtonClick(Sender: TObject);
    procedure IdleBoxChange(Sender: TObject);
    procedure BecameIdle(Sender: TObject);
    procedure BecameActive(Sender: TObject);
    procedure IdleError(Sender: TObject; const Message: string);
    procedure FindButtonClick(Sender: TObject);
    procedure WindowInputDropDown(Sender: TObject);
    procedure WindowFound(Sender: TObject);
    procedure WindowTimeout(Sender: TObject);
    procedure TypeButtonClick(Sender: TObject);
    procedure TypeLaterButtonClick(Sender: TObject);
    procedure CountdownTick(Sender: TObject);
    procedure MouseButtonClick(Sender: TObject);
    procedure SampleButtonClick(Sender: TObject);
    procedure CaptureButtonClick(Sender: TObject);
    procedure CaptureError(Sender: TObject; const Message: string);
    procedure Captured(Sender: TObject);
    procedure SaveButtonClick(Sender: TObject);
    procedure Error(Sender: TObject; const Message: string);
  end;
var AutomationForm: TAutomationForm;
implementation
{$R *.lfm}
procedure TAutomationForm.TypeButtonClick(Sender: TObject);
begin
  Output.SetFocus;
  Keyboard.TypeText(TextInput.Text);
end;
{ Time to put the focus in some other application - handy for the ones that
  take typing but not a paste. }
procedure TAutomationForm.TypeLaterButtonClick(Sender: TObject);
begin
  Countdown.Tag := DelayInput.Value;
  Countdown.Enabled := True;
  TypeLaterButton.Enabled := False;
  Status.Caption := Format('Typing wherever the focus is in %d...', [Countdown.Tag]);
end;
procedure TAutomationForm.CountdownTick(Sender: TObject);
begin
  Countdown.Tag := Countdown.Tag - 1;
  if Countdown.Tag > 0 then begin
    Status.Caption := Format('Typing wherever the focus is in %d...', [Countdown.Tag]);
    Exit;
  end;
  Countdown.Enabled := False;
  TypeLaterButton.Enabled := True;
  Keyboard.TypeText(TextInput.Text);
  Status.Caption := 'Typed into the focused control.';
end;
procedure TAutomationForm.MouseButtonClick(Sender: TObject);
begin
  Pointer.MoveTo(SampleButton.Width div 2, SampleButton.Height div 2);
  Pointer.Click;
end;
procedure TAutomationForm.SampleButtonClick(Sender: TObject);
begin
  SampleButton.Tag := SampleButton.Tag + 1;
  SampleButton.Caption := Format('Sample clicked %d times', [SampleButton.Tag]);
end;
procedure TAutomationForm.CaptureButtonClick(Sender: TObject);
begin
  if not CaptureButton.Enabled then Exit;
  CaptureButton.Enabled := False;
  try
    Screenshot.Source := THIDCaptureSource(SourceBox.ItemIndex);
    Screenshot.WindowTitle := WindowInput.Text;
    Screenshot.MonitorIndex := MonitorInput.Value;
    if not Screenshot.Capture and (Screenshot.LastError = '') then Status.Caption := 'Selection canceled.';
  finally CaptureButton.Enabled := True; end;
end;
procedure TAutomationForm.MoveWindowButtonClick(Sender: TObject);
begin
  Window.WindowTitle := WindowInput.Text;
  if Window.Find then WindowPointer.MoveTo(WindowX.Value, WindowY.Value);
end;
procedure TAutomationForm.PowerBoxChange(Sender: TObject);
begin
  Power.Enabled := PowerBox.Checked;
  if Power.Enabled then PowerChanged(Power)
  else if Power.LastError = '' then PowerStatus.Caption := 'Power monitoring is off.';
end;
procedure TAutomationForm.PowerChanged(Sender: TObject);
const Sources: array[TPowerSource] of string = ('Unknown source', 'AC power', 'Battery power');
var Battery: string;
begin
  if Power.BatteryPercent < 0 then Battery := 'no battery reading'
  else Battery := IntToStr(Power.BatteryPercent) + '%';
  PowerStatus.Caption := Sources[Power.PowerSource] + ' | ' + Battery;
  if Power.Charging then PowerStatus.Caption := PowerStatus.Caption + ' | charging';
end;
procedure TAutomationForm.PowerSuspend(Sender: TObject);
begin PowerLog.Lines.Add('Sleep notification received.'); end;
procedure TAutomationForm.PowerResume(Sender: TObject);
begin PowerChanged(Sender); PowerLog.Lines.Add('Resume notification received.'); end;
procedure TAutomationForm.PowerError(Sender: TObject; const Message: string);
begin PowerBox.Checked := False; PowerStatus.Caption := Message; end;
procedure TAutomationForm.RefreshPowerButtonClick(Sender: TObject);
begin if Power.Refresh then PowerChanged(Sender); end;
procedure TAutomationForm.ClipboardBoxChange(Sender: TObject);
begin
  ClipboardMonitor.Enabled := ClipboardBox.Checked;
  if ClipboardMonitor.Enabled then ClipboardStatus.Caption := 'Copy text or an image here or in another application.'
  else if ClipboardMonitor.LastError = '' then ClipboardStatus.Caption := 'Clipboard monitoring is off.';
end;
procedure TAutomationForm.ClipboardChanged(Sender: TObject);
begin
  ClipboardStatus.Tag := ClipboardStatus.Tag + 1;
  ClipboardStatus.Caption := Format('Clipboard changed %d times.', [ClipboardStatus.Tag]);
  if Clipboard.HasFormat(CF_Text) then ClipboardText.Text := Clipboard.AsText
  else ClipboardText.Text := '(Clipboard has no text; try copying an image.)';
end;
procedure TAutomationForm.ClipboardError(Sender: TObject; const Message: string);
begin ClipboardBox.Checked := False; ClipboardStatus.Caption := Message; end;
procedure TAutomationForm.CopyImageButtonClick(Sender: TObject);
begin Clipboard.Assign(Screenshot.Bitmap); end;
procedure TAutomationForm.CopyButtonClick(Sender: TObject);
begin Clipboard.AsText := TextInput.Text; end;
procedure TAutomationForm.IdleBoxChange(Sender: TObject);
begin
  IdleStatus.Caption := 'Active; leave mouse and keyboard alone for 5 seconds.';
  SystemIdle.Enabled := IdleBox.Checked;
  if not IdleBox.Checked then IdleStatus.Caption := 'Idle detection is off.';
end;
procedure TAutomationForm.BecameIdle(Sender: TObject);
begin IdleStatus.Caption := 'Idle: no session input for at least 5 seconds.'; end;
procedure TAutomationForm.BecameActive(Sender: TObject);
begin IdleStatus.Caption := 'Active again: session input resumed.'; end;
procedure TAutomationForm.IdleError(Sender: TObject; const Message: string);
begin IdleBox.Checked := False; IdleStatus.Caption := Message; end;
procedure TAutomationForm.FindButtonClick(Sender: TObject);
begin
  Window.WindowTitle := WindowInput.Text;
  Status.Caption := 'Waiting for a visible window (up to 5 seconds)...';
  Window.WaitForWindow;
end;
procedure TAutomationForm.WindowInputDropDown(Sender: TObject);
var
  Windows: TList;
  Item: Pointer;
  Info: TWindowInfo;
  Keep: string;
begin
  Keep := WindowInput.Text;
  WindowInput.Items.BeginUpdate;
  WindowInput.Items.Clear;
  EnsureWindowManager;
  if WindowMgr <> nil then
  begin
    Windows := WindowMgr.EnumerateWindows;
    try
      for Item in Windows do
        if WindowMgr.GetWindowInfo(TWindowHandle(Item), Info) and Info.IsVisible
          and (Info.Title <> '') then
          WindowInput.Items.Add(Info.Title);
    finally
      Windows.Free;
    end;
  end;
  WindowInput.Items.EndUpdate;
  WindowInput.Text := Keep;
end;
procedure TAutomationForm.WindowFound(Sender: TObject);
var Info: TWindowInfo;
begin
  if Window.GetInfo(Info) then
    Status.Caption := Format('Found "%s", process %d. Activation requested: %s.',
      [Info.Title, Info.ProcessID, BoolToStr(Window.Activate, True)]);
end;
procedure TAutomationForm.WindowTimeout(Sender: TObject);
begin Status.Caption := 'No matching window appeared. Check the title and try again.'; end;
procedure TAutomationForm.Captured(Sender: TObject);
begin
  Preview.Picture.Assign(Screenshot.Bitmap);
  Status.Caption := Format('Captured %d x %d pixels.', [Screenshot.Bitmap.Width, Screenshot.Bitmap.Height]);
  SaveButton.Enabled := True;
  CopyImageButton.Enabled := True;
end;
procedure TAutomationForm.SaveButtonClick(Sender: TObject);
var Image: TPortableNetworkGraphic;
begin
  if not SaveDialog.Execute then Exit;
  Image := TPortableNetworkGraphic.Create;
  try Image.Assign(Screenshot.Bitmap); Image.SaveToFile(SaveDialog.FileName); finally Image.Free; end;
end;
procedure TAutomationForm.Error(Sender: TObject; const Message: string);
begin Status.Caption := Message; end;
{ A stale picture above a failure line reads as a result, so it goes. }
procedure TAutomationForm.CaptureError(Sender: TObject; const Message: string);
begin
  Preview.Picture.Clear;
  SaveButton.Enabled := False;
  CopyImageButton.Enabled := False;
  Status.Caption := Message;
end;
end.
