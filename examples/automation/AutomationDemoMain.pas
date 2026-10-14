unit AutomationDemoMain;
{$mode objfpc}{$H+}
// Copyright (c) 2026 Tony Stone. GPL-2.0-or-later; see COPYING.GPL2.

{ Every LazHIDControl component at work, a tab for each job.  All the settings
  are on the components in the form; the code here is only what happens in
  their events and behind the buttons. }

interface

uses Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, ExtCtrls, ComCtrls, Spin, Menus, LCLType, LCLIntf, LCLProc, LazUTF8, Clipbrd,
  WindowManagerIntf, WindowManager, HIDHotkey, HIDInput, HIDScreenshot, HIDWindow, HIDSystemIdle, HIDPower, HIDClipboard,
  HIDMonitor, HIDPermission, PowerInput;

type
  TAutomationForm = class(TForm)
    Pages: TPageControl;
    TypingTab, MouseTab, WindowsTab, CaptureTab, MonitorsTab: TTabSheet;
    StatusBar: TStatusBar;
    WatchPanel: TPanel;
    WatchBox: TCheckBox;
    PointerTimer: TTimer;
    // typing
    TypingTop: TPanel;
    TypeButton, TypeLaterButton: TButton;
    DelayInput: TSpinEdit;
    MatchLabel, HotkeyLabel: TLabel;
    SourceMemo, TargetMemo: TMemo;
    TypingSplitter: TSplitter;
    Countdown: TTimer;
    Keyboard: THIDKeyboard;
    TypeHotkey: THIDHotkey;
    // mouse
    MouseButton, SampleButton, ScrollButton: TButton;
    ScrollList: TListBox;
    MouseHint: TLabel;
    Pointer, ListPointer: THIDMouse;
    // other programs' windows
    WindowsTop, WindowsBottom: TPanel;
    RefreshButton, ActivateButton, MinimizeButton, RestoreButton, CloseButton, PointerIntoButton: TButton;
    FollowBox: TCheckBox;
    WindowList: TListView;
    UnderLabel: TLabel;
    Window: THIDWindow;
    WindowPointer: THIDMouse;
    // screenshots
    CaptureTop: TPanel;
    SourceLabel, MonitorLabel, TitleLabel: TLabel;
    SourceBox: TComboBox;
    MonitorInput: TSpinEdit;
    CaptureTitle: TEdit;
    CaptureButton, SaveButton, CopyImageButton: TButton;
    Preview: TImage;
    Screenshot: THIDScreenshot;
    CaptureHotkey: THIDHotkey;
    SaveDialog: TSaveDialog;
    // idle, power, clipboard
    IdleGroup, PowerGroup, ClipboardGroup: TGroupBox;
    IdleBox, PowerBox, ClipboardBox: TCheckBox;
    IdleStatus, PowerStatus, ClipboardStatus: TLabel;
    RefreshPowerButton, CopyButton: TButton;
    PowerLog: TMemo;
    ClipboardText: TEdit;
    SystemIdle: THIDSystemIdle;
    Power: THIDPowerMonitor;
    ClipboardMonitor: THIDClipboardMonitor;
    InputMonitor: THIDInputMonitor;
    Permission: THIDPermission;
    PermissionButton: TButton;
    procedure FormCreate(Sender: TObject);
    procedure PointerTimerTick(Sender: TObject);
    procedure WatchBoxChange(Sender: TObject);
    procedure PermissionButtonClick(Sender: TObject);
    procedure KeySeen(Sender: TObject; Key: word; Shift: TShiftState);
    procedure ClickSeen(Sender: TObject; Button: TMouseButton; X, Y: integer);
    procedure Error(Sender: TObject; const Message: string);
    procedure TypeButtonClick(Sender: TObject);
    procedure TypeLaterButtonClick(Sender: TObject);
    procedure CountdownTick(Sender: TObject);
    procedure TypeHotkeyPressed(Sender: TObject);
    procedure TargetMemoChange(Sender: TObject);
    procedure MouseButtonClick(Sender: TObject);
    procedure SampleButtonClick(Sender: TObject);
    procedure ScrollButtonClick(Sender: TObject);
    procedure RefreshButtonClick(Sender: TObject);
    procedure ActivateButtonClick(Sender: TObject);
    procedure MinimizeButtonClick(Sender: TObject);
    procedure RestoreButtonClick(Sender: TObject);
    procedure CloseButtonClick(Sender: TObject);
    procedure PointerIntoButtonClick(Sender: TObject);
    procedure CaptureButtonClick(Sender: TObject);
    procedure Captured(Sender: TObject);
    procedure CaptureError(Sender: TObject; const Message: string);
    procedure SaveButtonClick(Sender: TObject);
    procedure CopyImageButtonClick(Sender: TObject);
    procedure IdleBoxChange(Sender: TObject);
    procedure BecameIdle(Sender: TObject);
    procedure BecameActive(Sender: TObject);
    procedure IdleError(Sender: TObject; const Message: string);
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
  private
    procedure Say(const Message: string);
    procedure TypeSource;
    procedure TypeAfterHotkey(Data: PtrInt);
    function Selected: TWindowHandle;
  end;

var
  AutomationForm: TAutomationForm;

implementation

{$R *.lfm}

procedure TAutomationForm.FormCreate(Sender: TObject);
var
  I: integer;
begin
  for I := 1 to 60 do ScrollList.Items.Add('Line ' + IntToStr(I));
  Pages.ActivePage := TypingTab;
  // watching the whole system's keys is for the user to switch on, not us
  Say('Tick the box above to watch keys and clicks everywhere.');
  // Wayland and macOS want the user's say-so first; elsewhere there is nothing to ask
  PermissionButton.Visible := not Permission.Granted;
  if PermissionButton.Visible then Say('This desktop needs permission before typing and watching work - press "Allow on this computer".');
end;

procedure TAutomationForm.PermissionButtonClick(Sender: TObject);
begin
  if Permission.Ask then
  begin
    PermissionButton.Visible := False;
    Say('Allowed. Typing, the mouse and watching work now.');
  end
  else if Permission.LastError = '' then
    Say('Not allowed yet.');
end;

procedure TAutomationForm.Say(const Message: string);
begin
  StatusBar.Panels[3].Text := Message;
end;

procedure TAutomationForm.Error(Sender: TObject; const Message: string);
begin
  Say(Message);
end;

{ ---- the status bar: THIDInputMonitor ----

  It sees keys and clicks in every program, which is what recording a task
  to play back needs - and why macOS asks the user's permission first and
  Wayland refuses outright. }

procedure TAutomationForm.WatchBoxChange(Sender: TObject);
begin
  InputMonitor.Enabled := WatchBox.Checked;
  if not WatchBox.Checked then Say('Not watching keys and clicks.')
  else if InputMonitor.LastError = '' then Say('Watching every key and click on the system.');
end;

procedure TAutomationForm.KeySeen(Sender: TObject; Key: word; Shift: TShiftState);
var
  Pressed: string;
begin
  Pressed := ShortCutToText(KeyToShortCut(Key, Shift * [ssShift, ssCtrl, ssAlt, ssMeta]));
  if Pressed = '' then Pressed := 'a key with no name';
  StatusBar.Panels[2].Text := Format('Last key: %s (%d)', [Pressed, Key]);
end;

procedure TAutomationForm.ClickSeen(Sender: TObject; Button: TMouseButton; X, Y: integer);
const
  Names: array[TMouseButton] of string = ('left', 'right', 'middle', 'extra 1', 'extra 2');
begin
  StatusBar.Panels[1].Text := Format('Last click: %s at %d, %d', [Names[Button], X, Y]);
end;

procedure TAutomationForm.PointerTimerTick(Sender: TObject);
var
  Info: TWindowInfo;
begin
  StatusBar.Panels[0].Text := Format('Pointer: %d, %d', [Mouse.CursorPos.X, Mouse.CursorPos.Y]);
  if not FollowBox.Checked or (Pages.ActivePage <> WindowsTab) then Exit;
  if GetWindowInfoAtPoint(Mouse.CursorPos.X, Mouse.CursorPos.Y, Info) then
    UnderLabel.Caption := Format('Under the pointer: "%s"  -  %s, process %d, at %d, %d, %d x %d', [Info.Title, Info.ClassName,
      Info.ProcessID, Info.Rect.Left, Info.Rect.Top, Info.Rect.Width, Info.Rect.Height])
  else
    UnderLabel.Caption := 'Under the pointer: nothing this platform can tell us about.';
end;

{ ---- typing: THIDKeyboard and THIDHotkey ---- }

{ A line at a time with the message queue drained between them: when the
  typing lands in our own window, the keys are only read once we let go. }
procedure TAutomationForm.TypeSource;
var
  I: integer;
begin
  for I := 0 to SourceMemo.Lines.Count - 1 do
  begin
    Application.ProcessMessages;
    if I > 0 then Keyboard.PressKey(VK_RETURN);
    Keyboard.TypeText(SourceMemo.Lines[I]);
  end;
  Application.ProcessMessages;
end;

procedure TAutomationForm.TypeButtonClick(Sender: TObject);
begin
  TargetMemo.Clear;
  TargetMemo.SetFocus;
  Say('Typing...');
  TypeSource;
  Say('Typed into the box on the right.');
end;

{ Time to click into some other program first - handy for the ones that take
  typing but will not take a paste. }
procedure TAutomationForm.TypeLaterButtonClick(Sender: TObject);
begin
  Countdown.Tag := DelayInput.Value;
  Countdown.Enabled := True;
  TypeLaterButton.Enabled := False;
  Say(Format('Typing wherever the focus is in %d...', [Countdown.Tag]));
end;

procedure TAutomationForm.CountdownTick(Sender: TObject);
begin
  Countdown.Tag := Countdown.Tag - 1;
  if Countdown.Tag > 0 then
  begin
    Say(Format('Typing wherever the focus is in %d...', [Countdown.Tag]));
    Exit;
  end;
  Countdown.Enabled := False;
  TypeLaterButton.Enabled := True;
  TypeSource;
  Say('Typed into whatever had the focus.');
end;

procedure TAutomationForm.TypeHotkeyPressed(Sender: TObject);
begin
  Application.QueueAsyncCall(@TypeAfterHotkey, 0);
end;

{ The hotkey fires while Ctrl and Shift are still down, and text typed then
  would arrive as shortcuts.  Wait for the fingers to come off first. }
procedure TAutomationForm.TypeAfterHotkey(Data: PtrInt);
var
  GiveUp: QWord;
begin
  GiveUp := GetTickCount64 + 1500;
  while ((GetKeyState(VK_CONTROL) < 0) or (GetKeyState(VK_SHIFT) < 0) or (GetKeyState(VK_MENU) < 0)) and (GetTickCount64 < GiveUp) do
  begin
    Sleep(10);
    Application.ProcessMessages;
  end;
  Sleep(50);
  TypeSource;
  Say('Typed into whatever had the focus.');
end;

{ Says whether what arrived on the right is what was sent from the left, and
  if not, where it first goes wrong. }
procedure TAutomationForm.TargetMemoChange(Sender: TObject);
var
  Line, Column: integer;
  Want, Got: string;
begin
  MatchLabel.Font.Color := clDefault;
  if TargetMemo.Lines.Count = 0 then
  begin
    MatchLabel.Caption := 'Nothing typed yet.';
    Exit;
  end;
  for Line := 0 to SourceMemo.Lines.Count - 1 do
  begin
    Want := SourceMemo.Lines[Line];
    Got := '';
    if Line < TargetMemo.Lines.Count then Got := TargetMemo.Lines[Line];
    if Want = Got then Continue;
    Column := 1;
    while UTF8Copy(Want, Column, 1) = UTF8Copy(Got, Column, 1) do Inc(Column);
    MatchLabel.Font.Color := clRed;
    MatchLabel.Caption := Format('Differs at line %d, character %d: "%s" came out as "%s"', [Line + 1, Column, UTF8Copy(Want, Column, 1), UTF8Copy(Got, Column, 1)]);
    Exit;
  end;
  MatchLabel.Font.Color := clGreen;
  MatchLabel.Caption := 'The text matches.';
end;

{ ---- the mouse: THIDMouse ---- }

procedure TAutomationForm.MouseButtonClick(Sender: TObject);
begin
  // Pointer.RelativeTo is the sample button, so this is the middle of it
  try
    Pointer.MoveTo(SampleButton.Width div 2, SampleButton.Height div 2);
    Pointer.Click;
  except
    on E: EInvalidOperation do Say(E.Message);
  end;
end;

procedure TAutomationForm.SampleButtonClick(Sender: TObject);
begin
  SampleButton.Tag := SampleButton.Tag + 1;
  SampleButton.Caption := Format('Sample clicked %d times', [SampleButton.Tag]);
end;

procedure TAutomationForm.ScrollButtonClick(Sender: TObject);
begin
  try
    ListPointer.MoveTo(ScrollList.Width div 2, ScrollList.Height div 2);
    ListPointer.Scroll(-5);
  except
    on E: EInvalidOperation do Say(E.Message);
  end;
end;

{ ---- other programs' windows: WindowManager and THIDWindow ---- }

procedure TAutomationForm.RefreshButtonClick(Sender: TObject);
var
  Windows: TList;
  Item: Pointer;
  Info: TWindowInfo;
begin
  WindowList.Items.BeginUpdate;
  try
    WindowList.Items.Clear;
    EnsureWindowManager;
    if WindowMgr = nil then
    begin
      Say('Other programs'' windows cannot be reached on this platform or session.');
      Exit;
    end;
    Windows := WindowMgr.EnumerateWindows;
    try
      for Item in Windows do
        if WindowMgr.GetWindowInfo(TWindowHandle(Item), Info) and (Info.IsVisible or Info.IsMinimized) then
          with WindowList.Items.Add do
          begin
            Caption := Info.Title;
            Data := Item;
            SubItems.Add(Info.ClassName);
            SubItems.Add(IntToStr(Info.ProcessID));
            SubItems.Add(Format('%d, %d', [Info.Rect.Left, Info.Rect.Top]));
            SubItems.Add(Format('%d x %d', [Info.Rect.Width, Info.Rect.Height]));
          end;
    finally
      Windows.Free;
    end;
    Say(Format('%d windows.', [WindowList.Items.Count]));
  finally
    WindowList.Items.EndUpdate;
  end;
end;

function TAutomationForm.Selected: TWindowHandle;
begin
  Result := 0;
  if WindowList.Selected <> nil then Result := TWindowHandle(WindowList.Selected.Data)
  else
    Say('Pick a window in the list first.');
end;

procedure TAutomationForm.ActivateButtonClick(Sender: TObject);
begin
  if Selected <> 0 then Say('Activate: ' + BoolToStr(WindowMgr.ActivateWindow(Selected), 'done', 'refused'));
end;

procedure TAutomationForm.MinimizeButtonClick(Sender: TObject);
begin
  if Selected <> 0 then Say('Minimize: ' + BoolToStr(WindowMgr.MinimizeWindow(Selected), 'done', 'refused'));
end;

procedure TAutomationForm.RestoreButtonClick(Sender: TObject);
begin
  if Selected <> 0 then Say('Restore: ' + BoolToStr(WindowMgr.RestoreWindow(Selected), 'done', 'refused'));
end;

procedure TAutomationForm.CloseButtonClick(Sender: TObject);
begin
  if Selected <> 0 then Say('Close: ' + BoolToStr(WindowMgr.CloseWindow(Selected), 'asked', 'refused'));
end;

{ THIDWindow finds a window by its title, and a THIDMouse whose RelativeWindow
  is that component then aims inside it. }
procedure TAutomationForm.PointerIntoButtonClick(Sender: TObject);
begin
  if WindowList.Selected = nil then
  begin
    Say('Pick a window in the list first.');
    Exit;
  end;
  Window.WindowTitle := WindowList.Selected.Caption;
  if Window.Find then
  begin
    WindowPointer.MoveTo(40, 40);
    Say('The pointer is 40, 40 inside "' + Window.WindowTitle + '".');
  end;
end;

{ ---- screenshots: THIDScreenshot ---- }

procedure TAutomationForm.CaptureButtonClick(Sender: TObject);
begin
  if not CaptureButton.Enabled then Exit;
  CaptureButton.Enabled := False;
  try
    Screenshot.Source := THIDCaptureSource(SourceBox.ItemIndex);
    Screenshot.WindowTitle := CaptureTitle.Text;
    Screenshot.MonitorIndex := MonitorInput.Value;
    if not Screenshot.Capture and (Screenshot.LastError = '') then Say('Selection canceled.');
  finally
    CaptureButton.Enabled := True;
  end;
end;

procedure TAutomationForm.Captured(Sender: TObject);
begin
  Preview.Picture.Assign(Screenshot.Bitmap);
  Say(Format('Captured %d x %d pixels.', [Screenshot.Bitmap.Width, Screenshot.Bitmap.Height]));
  SaveButton.Enabled := True;
  CopyImageButton.Enabled := True;
end;

{ A stale picture above a failure line reads as a result, so it goes. }
procedure TAutomationForm.CaptureError(Sender: TObject; const Message: string);
begin
  Preview.Picture.Clear;
  SaveButton.Enabled := False;
  CopyImageButton.Enabled := False;
  Say(Message);
end;

procedure TAutomationForm.SaveButtonClick(Sender: TObject);
var
  Image: TPortableNetworkGraphic;
begin
  if not SaveDialog.Execute then Exit;
  Image := TPortableNetworkGraphic.Create;
  try
    Image.Assign(Screenshot.Bitmap);
    Image.SaveToFile(SaveDialog.FileName);
  finally
    Image.Free;
  end;
end;

procedure TAutomationForm.CopyImageButtonClick(Sender: TObject);
begin
  Clipboard.Assign(Screenshot.Bitmap);
end;

{ ---- THIDSystemIdle ---- }

procedure TAutomationForm.IdleBoxChange(Sender: TObject);
begin
  IdleStatus.Caption := 'Active; leave mouse and keyboard alone for 5 seconds.';
  SystemIdle.Enabled := IdleBox.Checked;
  if not IdleBox.Checked and (SystemIdle.LastError = '') then IdleStatus.Caption := 'Idle detection is off.';
end;

procedure TAutomationForm.BecameIdle(Sender: TObject);
begin
  IdleStatus.Caption := 'Idle: nobody has touched the mouse or keyboard for 5 seconds.';
end;

procedure TAutomationForm.BecameActive(Sender: TObject);
begin
  IdleStatus.Caption := 'Active again.';
end;

procedure TAutomationForm.IdleError(Sender: TObject; const Message: string);
begin
  IdleBox.Checked := False;
  IdleStatus.Caption := Message;
end;

{ ---- THIDPowerMonitor ---- }

procedure TAutomationForm.PowerBoxChange(Sender: TObject);
begin
  Power.Enabled := PowerBox.Checked;
  if Power.Enabled then PowerChanged(Power)
  else if Power.LastError = '' then PowerStatus.Caption := 'Power monitoring is off.';
end;

procedure TAutomationForm.PowerChanged(Sender: TObject);
const
  Sources: array[TPowerSource] of string = ('Unknown source', 'AC power', 'Battery power');
var
  Battery: string;
begin
  if Power.BatteryPercent < 0 then Battery := 'no battery reading'
  else
    Battery := IntToStr(Power.BatteryPercent) + '%';
  PowerStatus.Caption := Sources[Power.PowerSource] + ' | ' + Battery;
  if Power.Charging then PowerStatus.Caption := PowerStatus.Caption + ' | charging';
end;

procedure TAutomationForm.PowerSuspend(Sender: TObject);
begin
  PowerLog.Lines.Add('Going to sleep.');
end;

procedure TAutomationForm.PowerResume(Sender: TObject);
begin
  PowerChanged(Sender);
  PowerLog.Lines.Add('Awake again.');
end;

procedure TAutomationForm.PowerError(Sender: TObject; const Message: string);
begin
  PowerBox.Checked := False;
  PowerStatus.Caption := Message;
end;

procedure TAutomationForm.RefreshPowerButtonClick(Sender: TObject);
begin
  if Power.Refresh then PowerChanged(Sender);
end;

{ ---- THIDClipboardMonitor ---- }

procedure TAutomationForm.ClipboardBoxChange(Sender: TObject);
begin
  ClipboardMonitor.Enabled := ClipboardBox.Checked;
  if ClipboardMonitor.Enabled then ClipboardStatus.Caption := 'Copy something, here or in another program.'
  else if ClipboardMonitor.LastError = '' then ClipboardStatus.Caption := 'Clipboard monitoring is off.';
end;

procedure TAutomationForm.ClipboardChanged(Sender: TObject);
begin
  ClipboardStatus.Tag := ClipboardStatus.Tag + 1;
  ClipboardStatus.Caption := Format('The clipboard has changed %d times.', [ClipboardStatus.Tag]);
  if Clipboard.HasFormat(CF_Text) then ClipboardText.Text := Clipboard.AsText
  else
    ClipboardText.Text := '(no text on it - an image, perhaps)';
end;

procedure TAutomationForm.ClipboardError(Sender: TObject; const Message: string);
begin
  ClipboardBox.Checked := False;
  ClipboardStatus.Caption := Message;
end;

procedure TAutomationForm.CopyButtonClick(Sender: TObject);
begin
  Clipboard.AsText := SourceMemo.Lines[0];
end;

end.
