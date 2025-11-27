unit MainForm;

{$mode objfpc}{$H+}

interface

uses
  Classes,
  Controls,
  Dialogs,
  ExtCtrls,
  Forms,
  Graphics,
  LCLType,
  LMessages,
  LCLIntf,
  Math,
  MouseAndKeyInput,
  StdCtrls,
  ComCtrls,
  SysUtils,
  WaylandInfo,
  GlobalKeyMonitor,
  HotkeyInput;

type

  { TMainForm }

  TMainForm = class(TForm)
    Bevel1: TBevel;
    btnAltTab: TButton;
    btnMoveMouseClick: TButton;
    btnToggleCapsLock: TToggleBox;
    btnWaylandInfo: TButton;
    btnToggleGlobalKeys: TButton;
    lblHotKeyCombo: TLabel;
    lblMatchStatus: TLabel;
    memTestSource: TMemo;
    memTestDestination: TMemo;
    PaintBox1: TPaintBox;
    StatusBar1: TStatusBar;
    Timer1: TTimer;
    procedure btnAltTabClick(Sender: TObject);
    procedure btnMoveMouseClickClick(Sender: TObject);
    procedure btnToggleCapsLockChange(Sender: TObject);
    procedure btnToggleCapsLockClick(Sender: TObject);
    procedure btnWaylandInfoClick(Sender: TObject);
    procedure btnToggleGlobalKeysClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: word; Shift: TShiftState);
    procedure FormResize(Sender: TObject);
    procedure memTestDestinationChange(Sender: TObject);
    procedure PaintBox1MouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: integer);
    procedure PaintBox1MouseMove(Sender: TObject; Shift: TShiftState; X, Y: integer);
    procedure PaintBox1MouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: integer);
    procedure PaintBox1Paint(Sender: TObject);
    procedure Timer1Timer(Sender: TObject);
  private
    DrawingActive: boolean;
    LastDrawingPoint: TPoint;
    BitmapCanvasDemo: TBitmap;
    GlobalKeyListener: TGlobalKeyListener;
    ReplayHotkey: THotkey;
    UpdatingCapsLock: boolean;  // Prevent recursive CapsLock updates

    procedure RunMouseDemo(Data: PtrInt);
    procedure RunReplayHotkey(Data: PtrInt);
    procedure UpdateKeyDisplay(Key: word; Shift: TShiftState);
    procedure OnGlobalKeyPress(Key: word; Shift: TShiftState);
    procedure OnReplayHotkey(Sender: TObject; Key: word; Shift: TShiftState);
  public

  end;

var
  frmMain: TMainForm;

implementation

{$R *.lfm}

{ TMainForm }

procedure TMainForm.memTestDestinationChange(Sender: TObject);
var
  i: integer;
  allMatch: boolean;
begin
  // Compare line by line
  allMatch := (memTestSource.Lines.Count = memTestDestination.Lines.Count);

  if allMatch then
  begin
    for i := 0 to memTestSource.Lines.Count - 1 do
    begin
      if memTestSource.Lines[i] <> memTestDestination.Lines[i] then
      begin
        allMatch := False;
        Break;
      end;
    end;
  end;

  if memTestDestination.Lines.Count = 0 then
  begin
    lblMatchStatus.Caption := 'Ready to test...';
    lblMatchStatus.Font.Color := clDefault;
  end
  else if allMatch then
  begin
    lblMatchStatus.Caption := '✓ Text matches!';
    lblMatchStatus.Font.Color := clGreen;
  end
  else
  begin
    lblMatchStatus.Caption := '✗ Text does not match';
    lblMatchStatus.Font.Color := clRed;
  end;
end;

// Demonstrates modifier keys - simulates Alt+Tab to switch windows
procedure TMainForm.btnAltTabClick(Sender: TObject);
var
  i: integer;
begin
  StatusBar1.Panels[2].Text := 'Simulating Alt+Tab...';

  // Hold down Alt key
  KeyInput.Down(VK_MENU);

  for i := 0 to 5 do
  begin
    KeyInput.Press(VK_TAB);
    Sleep(400);
  end;

  KeyInput.Up(VK_MENU);
  StatusBar1.Panels[2].Text := 'Alt+Tab complete';
end;

procedure TMainForm.btnMoveMouseClickClick(Sender: TObject);
begin
  Application.QueueAsyncCall(@RunMouseDemo, 0);
end;

procedure TMainForm.btnToggleCapsLockChange(Sender: TObject);
begin
  if UpdatingCapsLock then Exit;  // Prevent recursive updates

  if btnToggleCapsLock.Checked then
    btnToggleCapsLock.Caption := '🔒 CAPS Lock ON'
  else
    btnToggleCapsLock.Caption := '🔓 CAPS Lock OFF';
end;

procedure TMainForm.btnToggleCapsLockClick(Sender: TObject);
var
  newState: boolean;
begin
  if UpdatingCapsLock then Exit;  // Prevent recursive calls

  UpdatingCapsLock := True;
  try
    // Toggle CapsLock by simulating key press
    KeyInput.Press(VK_CAPITAL);

    Sleep(100);

    newState := KeyInput.GetCapsLockState;
    btnToggleCapsLock.Checked := newState;

    if newState then
      StatusBar1.Panels[2].Text := 'CapsLock is ON'
    else
      StatusBar1.Panels[2].Text := 'CapsLock is OFF';
  finally
    UpdatingCapsLock := False;
  end;
end;

procedure TMainForm.RunMouseDemo(Data: PtrInt);
var
  adjustedPoints: array[0..4] of TPoint;
  paintBoxScreenPos: TPoint;
  paintBoxWidth, paintBoxHeight: integer;
  i, n: integer;
  angle: double;
  offsets: integer = 5;
  radius: integer = 50;
  offset: integer = 75;
  steps: integer = 4;
begin
  StatusBar1.Panels[2].Text := 'Running mouse demo...';

  // Get the PaintBox's screen coordinates (relative to desktop)
  paintBoxScreenPos := PaintBox1.ClientToScreen(Point(0, 0));

  paintBoxWidth := PaintBox1.Width;
  paintBoxHeight := PaintBox1.Height;

  // Draw two border rectangles by moving mouse with button held down
  for i := 0 to 1 do
  begin
    MouseInput.Move([], paintBoxScreenPos.X + offsets, paintBoxScreenPos.Y + offsets, 200);
    MouseInput.Down(mbLeft, []);
    MouseInput.Move([], paintBoxScreenPos.X + PaintBox1.Width - offsets, paintBoxScreenPos.Y + offsets, 200);
    MouseInput.Move([], paintBoxScreenPos.X + PaintBox1.Width - offsets, paintBoxScreenPos.Y + PaintBox1.Height - offsets, 200);
    MouseInput.Move([], paintBoxScreenPos.X + offsets, paintBoxScreenPos.Y + PaintBox1.Height - offsets, 200);
    MouseInput.Move([], paintBoxScreenPos.X + offsets, paintBoxScreenPos.Y + offsets, 200);
    MouseInput.Up(mbLeft, []);
    Inc(offsets, 15);
  end;

  // Define points: 4 corners and center
  adjustedPoints[0] := Point(paintBoxScreenPos.X + offset, paintBoxScreenPos.Y + offset);
  adjustedPoints[1] := Point(paintBoxScreenPos.X + paintBoxWidth - offset, paintBoxScreenPos.Y + offset);
  adjustedPoints[2] := Point(paintBoxScreenPos.X + offset, paintBoxScreenPos.Y + paintBoxHeight - offset);
  adjustedPoints[3] := Point(paintBoxScreenPos.X + paintBoxWidth - offset, paintBoxScreenPos.Y + paintBoxHeight - offset);
  adjustedPoints[4] := Point(paintBoxScreenPos.X + (paintBoxWidth div 2), paintBoxScreenPos.Y + (paintBoxHeight div 2));

  // Draw circles at each point with increasing complexity
  for i := 0 to High(adjustedPoints) do
  begin
    MouseInput.Move([], adjustedPoints[i].X + radius, adjustedPoints[i].Y, 1000);
    MouseInput.Down(mbLeft, []);

    // Draw circular motion
    for n := 0 to steps do
    begin
      angle := n * (2 * Pi / steps);
      MouseInput.Move([],
        adjustedPoints[i].X + Round(radius * Cos(angle)),
        adjustedPoints[i].Y + Round(radius * Sin(angle)), (200 div steps));
    end;
    steps := steps * 2;  // Each circle gets more detailed
    MouseInput.Up(mbLeft, []);
  end;

  StatusBar1.Panels[2].Text := 'Mouse demo complete';
end;

procedure TMainForm.btnWaylandInfoClick(Sender: TObject);
var
  InfoForm: TfrmWaylandInfo;
begin
  InfoForm := TfrmWaylandInfo.Create(nil);
  try
    InfoForm.ShowModal;
  finally
    InfoForm.Free;
  end;
end;

procedure TMainForm.FormCreate(Sender: TObject);
begin
  // Initialize flags
  UpdatingCapsLock := False;

  // Initialize the drawing canvas for the PaintBox demo
  BitmapCanvasDemo := TBitmap.Create;
  BitmapCanvasDemo.SetSize(PaintBox1.Width, PaintBox1.Height);
  BitmapCanvasDemo.Canvas.Brush.Color := clWhite;
  BitmapCanvasDemo.Canvas.FillRect(BitmapCanvasDemo.Canvas.ClipRect);

  // Create global key listener
  GlobalKeyListener := CreateGlobalKeyListener;
  GlobalKeyListener.OnKeyPress := @OnGlobalKeyPress;

  // Register global hotkey for text replay (Ctrl+Shift+F9)
  ReplayHotkey := CreateHotkey(VK_F9, [ssCtrl, ssShift], @OnReplayHotkey);
  ReplayHotkey.Register;

  if not ReplayHotkey.Registered then
  begin
    lblHotKeyCombo.Caption := 'Hotkey: Ctrl+Shift+F9 (not available on this platform)';
    lblHotKeyCombo.Font.Color := clRed;
    StatusBar1.Panels[2].Text := 'Hotkey registration failed';
  end
  else
  begin
    lblHotKeyCombo.Caption := 'Hotkey: Press Ctrl+Shift+F9 anywhere to replay text';
    lblHotKeyCombo.Font.Color := clGreen;
    StatusBar1.Panels[2].Text := 'Hotkey registered successfully';
  end;

  // Initialize CapsLock button state from actual system state
  try
    btnToggleCapsLock.Checked := KeyInput.GetCapsLockState;
  except
    // If we can't read state at startup, default to unchecked
    btnToggleCapsLock.Checked := False;
  end;
end;

procedure TMainForm.FormDestroy(Sender: TObject);
begin
  if Assigned(ReplayHotkey) then
  begin
    ReplayHotkey.Unregister;
    ReplayHotkey.Free;
  end;

  if Assigned(GlobalKeyListener) then
  begin
    GlobalKeyListener.StopMonitoring;
    GlobalKeyListener.Free;
  end;
end;

procedure TMainForm.btnToggleGlobalKeysClick(Sender: TObject);
begin
  if GlobalKeyListener.Active then
  begin
    GlobalKeyListener.StopMonitoring;
    btnToggleGlobalKeys.Caption := 'Enable Global Key Monitor';
    StatusBar1.Panels[2].Text := 'Global monitoring disabled';
  end
  else
  begin
    GlobalKeyListener.StartMonitoring;
    if GlobalKeyListener.Active then
    begin
      btnToggleGlobalKeys.Caption := 'Disable Global Key Monitor';
      StatusBar1.Panels[2].Text := 'Global monitoring active (system-wide)';
    end
    else
    begin
      ShowMessage('Global key monitoring not supported on this platform.' + LineEnding +
        'X11 and Windows are supported. Wayland does not allow system-wide key capture for security reasons.');
      StatusBar1.Panels[2].Text := 'Global monitoring not available';
    end;
  end;
end;

procedure TMainForm.OnGlobalKeyPress(Key: word; Shift: TShiftState);
begin
  UpdateKeyDisplay(Key, Shift);
end;

// Capture all keypresses in the form (KeyPreview must be True)
procedure TMainForm.FormKeyDown(Sender: TObject; var Key: word; Shift: TShiftState);
begin
  UpdateKeyDisplay(Key, Shift);
end;

procedure TMainForm.FormResize(Sender: TObject);
begin
  // Resize the drawing canvas when form is resized
  if Assigned(BitmapCanvasDemo) then
    BitmapCanvasDemo.Free;

  BitmapCanvasDemo := TBitmap.Create;
  BitmapCanvasDemo.SetSize(PaintBox1.Width, PaintBox1.Height);
  BitmapCanvasDemo.Canvas.Brush.Color := clWhite;
  BitmapCanvasDemo.Canvas.FillRect(BitmapCanvasDemo.Canvas.ClipRect);
end;


procedure TMainForm.OnReplayHotkey(Sender: TObject; Key: word; Shift: TShiftState);
begin
  // Queue the actual typing to happen after event processing
  // This ensures modifier keys are released and target app is ready
  Application.QueueAsyncCall(@RunReplayHotkey, 0);
end;

procedure TMainForm.RunReplayHotkey(Data: PtrInt);
var
  startTime: QWord;
  timeout: boolean;
begin
  StatusBar1.Panels[2].Text := 'Hotkey triggered - waiting for key release...';
  Application.ProcessMessages;

  // Wait for all modifier keys to be released before typing
  // This prevents the hotkey modifiers (Ctrl+Shift) from affecting the typed text
  startTime := GetTickCount64;
  timeout := False;

  while not timeout do
  begin
    // Check if all modifier keys are released
    if (GetKeyState(VK_CONTROL) >= 0) and (GetKeyState(VK_SHIFT) >= 0) and (GetKeyState(VK_MENU) >= 0) then
      Break;  // All modifiers released

    Sleep(10);
    Application.ProcessMessages;

    // Timeout after 500ms to prevent long delays
    if GetTickCount64 - startTime > 500 then
      timeout := True;
  end;

  if timeout then
    StatusBar1.Panels[2].Text := 'Timeout waiting for keys - typing anyway...'
  else
    StatusBar1.Panels[2].Text := 'Keys released - typing...';
  Application.ProcessMessages;

  // Extra delay to ensure target application is ready to receive input
  Sleep(50);

  KeyInput.PressString(memTestSource.Text);
  StatusBar1.Panels[2].Text := 'Text replay complete';
end;

procedure TMainForm.UpdateKeyDisplay(Key: word; Shift: TShiftState);
var
  modifiers: string;
  keyName: string;
begin
  modifiers := '';
  if ssShift in Shift then modifiers := modifiers + 'Shift+';
  if ssCtrl in Shift then modifiers := modifiers + 'Ctrl+';
  if ssAlt in Shift then modifiers := modifiers + 'Alt+';

  // Friendly name for common keys
  case Key of
    VK_RETURN: keyName := 'Enter';
    VK_ESCAPE: keyName := 'Esc';
    VK_BACK: keyName := 'Backspace';
    VK_TAB: keyName := 'Tab';
    VK_DELETE: keyName := 'Del';
    VK_HOME: keyName := 'Home';
    VK_END: keyName := 'End';
    VK_PRIOR: keyName := 'PgUp';
    VK_NEXT: keyName := 'PgDn';
    VK_LEFT: keyName := '←';
    VK_RIGHT: keyName := '→';
    VK_UP: keyName := '↑';
    VK_DOWN: keyName := '↓';
    VK_F1..VK_F12: keyName := 'F' + IntToStr(Key - VK_F1 + 1);
    else
      if (Key >= 32) and (Key <= 126) then
        keyName := Chr(Key)  // Printable ASCII
      else
        keyName := 'VK_' + IntToStr(Key);
  end;

  StatusBar1.Panels[1].Text := Format('Last Key: %s%s (0x%s)', [modifiers, keyName, IntToHex(Key, 2)]);
end;

procedure TMainForm.PaintBox1MouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: integer);
begin
  if Button = mbLeft then
  begin
    DrawingActive := True;
    LastDrawingPoint := Point(X, Y);
  end;
end;

procedure TMainForm.PaintBox1MouseMove(Sender: TObject; Shift: TShiftState; X, Y: integer);
begin
  if DrawingActive then
  begin
    BitmapCanvasDemo.Canvas.Pen.Color := clRed;
    BitmapCanvasDemo.Canvas.Pen.Width := 2;
    BitmapCanvasDemo.Canvas.Line(LastDrawingPoint.X, LastDrawingPoint.Y, X, Y);

    PaintBox1.Invalidate;
    LastDrawingPoint := Point(X, Y);
  end;
end;

procedure TMainForm.PaintBox1MouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: integer);
begin
  if Button = mbLeft then
    DrawingActive := False;
end;

procedure TMainForm.PaintBox1Paint(Sender: TObject);
begin
  PaintBox1.Canvas.Draw(0, 0, BitmapCanvasDemo);
end;


// Update the status bar with current cursor position
procedure TMainForm.Timer1Timer(Sender: TObject);
var
  mouseX, mouseY: integer;
  capsState: boolean;
begin
  mouseX := Mouse.CursorPos.X;
  mouseY := Mouse.CursorPos.Y;
  StatusBar1.Panels[0].Text := Format('Mouse: (%d, %d)', [mouseX, mouseY]);

  // Update CapsLock button state to reflect actual system state
  if not UpdatingCapsLock then
  begin
    try
      capsState := KeyInput.GetCapsLockState;
      if btnToggleCapsLock.Checked <> capsState then
      begin
        UpdatingCapsLock := True;
        try
          btnToggleCapsLock.Checked := capsState;
          // Manually update caption since OnChange is blocked by UpdatingCapsLock flag
          if capsState then
            btnToggleCapsLock.Caption := '🔒 CAPS Lock ON'
          else
            btnToggleCapsLock.Caption := '🔓 CAPS Lock OFF';
        finally
          UpdatingCapsLock := False;
        end;
      end;
    except
      // Ignore errors during CapsLock state check (e.g., X11 not ready)
    end;
  end;
end;

end.
