unit MainForm;

{$mode objfpc}{$H+}

interface

uses
  Classes,
  Controls,
  Dialogs,
  ExtCtrls,
  Forms,
  LCLType,
  LCLIntf,
  Math,
  StdCtrls,
  ComCtrls,
  SysUtils,
  Types,
  WaylandInfo,
  StayOnTopTestForm,
  GlobalKeyMonitor,
  GlobalMouseMonitor,
  HIDHotkey,
  WindowManager,
  WindowManagerIntf,
  Graphics,
  MouseAndKeyInput,
  Clipbrd;

type

  { TMainForm }

  TMainForm = class(TForm)
    Bevel1: TBevel;
    btnAltTab: TButton;
    btnTypeToRight: TButton;
    ReplayHotkey: THIDHotkey;
    btnMoveMouseClick: TButton;
    btnSpyBringToTop: TButton;
    btnSpyClose: TButton;
    btnSpyCopyInfo: TButton;
    btnSpyFlash: TButton;
    btnSpyFocus: TButton;
    btnSpyHide: TButton;
    btnSpyLocate: TButton;
    btnSpyMaximize: TButton;
    btnSpyMinimize: TButton;
    btnSpyRefresh: TButton;
    btnSpyRestore: TButton;
    btnSpySearch: TButton;
    btnSpyShow: TButton;
    btnToggleCapsLock: TToggleBox;
    btnToggleGlobalKeys: TButton;
    btnToggleNumLock: TToggleBox;
    btnToggleScrollLock: TToggleBox;
    btnWaylandInfo: TButton;
    btnTestStayOnTop: TButton;
    chkStayOnTop: TCheckBox;
    chkSpyAutoRefresh: TCheckBox;
    edtSpyCaption: TEdit;
    edtSpyClass: TEdit;
    edtSpyHandle: TEdit;
    edtSpyState: TEdit;
    lblSpyState: TLabel;
    edtSpyProcess: TEdit;
    edtSpyRect: TEdit;
    edtSpySearch: TEdit;
    gbSpyWindowActions: TGroupBox;
    lblFinderHint: TLabel;
    lblHotKeyCombo: TLabel;
    lblMatchStatus: TLabel;
    lblSpyCaption: TLabel;
    lblSpyClass: TLabel;
    lblSpyHandle: TLabel;
    lblSpyProcess: TLabel;
    lblSpyRect: TLabel;
    memTestDestination: TMemo;
    memTestSource: TMemo;
    PageControl1: TPageControl;
    PaintBox1: TPaintBox;
    pnlFinderTarget: TPanel;
    pnlSpyLeft: TPanel;
    pnlSpyRight: TPanel;
    pnlSpyToolbar: TPanel;
    SpySplitter: TSplitter;
    StatusBar1: TStatusBar;
    TabSheet1: TTabSheet;
    TabSheet2: TTabSheet;
    TabSheet3: TTabSheet;
    Timer1: TTimer;
    TimerSpyRefresh: TTimer;
    tvWindows: TTreeView;
    procedure btnAltTabClick(Sender: TObject);
    procedure btnTypeToRightClick(Sender: TObject);
    procedure ReplayHotkeyPressed(Sender: TObject);
    procedure ReplayHotkeyError(Sender: TObject; const Message: string);
    procedure btnMoveMouseClickClick(Sender: TObject);
    procedure btnSpyBringToTopClick(Sender: TObject);
    procedure btnSpyCloseClick(Sender: TObject);
    procedure btnSpyCopyInfoClick(Sender: TObject);
    procedure btnSpyFlashClick(Sender: TObject);
    procedure btnSpyFocusClick(Sender: TObject);
    procedure btnSpyHideClick(Sender: TObject);
    procedure btnSpyLocateClick(Sender: TObject);
    procedure btnSpyMaximizeClick(Sender: TObject);
    procedure btnSpyMinimizeClick(Sender: TObject);
    procedure btnSpyRefreshClick(Sender: TObject);
    procedure btnSpyRestoreClick(Sender: TObject);
    procedure btnSpySearchClick(Sender: TObject);
    procedure btnSpyShowClick(Sender: TObject);
    procedure btnToggleCapsLockChange(Sender: TObject);
    procedure btnToggleCapsLockClick(Sender: TObject);
    procedure btnToggleNumLockChange(Sender: TObject);
    procedure btnToggleNumLockClick(Sender: TObject);
    procedure btnToggleScrollLockChange(Sender: TObject);
    procedure btnToggleScrollLockClick(Sender: TObject);
    procedure btnWaylandInfoClick(Sender: TObject);
    procedure btnTestStayOnTopClick(Sender: TObject);
    procedure btnToggleGlobalKeysClick(Sender: TObject);
    procedure chkSpyAutoRefreshChange(Sender: TObject);
    procedure chkStayOnTopChange(Sender: TObject);
    procedure edtSpySearchChange(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: word; Shift: TShiftState);
    procedure FormResize(Sender: TObject);
    procedure memTestDestinationChange(Sender: TObject);
    procedure memTestSourceChange(Sender: TObject);
    procedure PaintBox1MouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: integer);
    procedure PaintBox1MouseMove(Sender: TObject; Shift: TShiftState; X, Y: integer);
    procedure PaintBox1MouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: integer);
    procedure PaintBox1Paint(Sender: TObject);
    procedure pnlFinderTargetMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure pnlFinderTargetMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
    procedure pnlFinderTargetMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure pnlFinderTargetPaint(Sender: TObject);
    procedure Timer1Timer(Sender: TObject);
    procedure TimerSpyRefreshTimer(Sender: TObject);
    procedure tvWindowsDblClick(Sender: TObject);
    procedure tvWindowsSelectionChanged(Sender: TObject);
  private
    DrawingActive: boolean;
    LastDrawingPoint: TPoint;
    BitmapCanvasDemo: TBitmap;
    GlobalKeyListener: TGlobalKeyListener;
    GlobalMouseListener: TGlobalMouseListener;
    UpdatingCapsLock: boolean;
    UpdatingNumLock: boolean;
    UpdatingScrollLock: boolean;
    LastClickPos: TPoint;

    // Window spy variables
    FinderDragging: Boolean;
    SelectedSpyWindow: TWindowHandle;
    SpyFlashCount: Integer;

    procedure RunMouseDemo(Data: PtrInt);
    procedure RunReplayHotkey(Data: PtrInt);
    procedure RunTypeToRight(Data: PtrInt);
    procedure UpdateKeyDisplay(Key: word; Shift: TShiftState);
    procedure OnGlobalKeyPress(Key: word; Shift: TShiftState);
    procedure OnGlobalMouseClick(Button: TMouseButton; X, Y: Integer);
    procedure DrawFinderTarget(ACanvas: TCanvas; ARect: TRect);
    procedure ResizeStatusBarPanels;

    // Window Spy methods
    procedure RefreshWindowTree;
    procedure AddWindowToTree(AHandle: TWindowHandle; AParentNode: TTreeNode);
    procedure UpdateSpyWindowDetails(AHandle: TWindowHandle);
    procedure ClearSpyDetails;
    function GetSelectedSpyWindow: TWindowHandle;
    procedure FlashWindow(AHandle: TWindowHandle);
    procedure LocateWindowInTree(AHandle: TWindowHandle);
    function FindWindowNode(AHandle: TWindowHandle; AStartNode: TTreeNode): TTreeNode;
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

procedure TMainForm.memTestSourceChange(Sender: TObject);
begin
end;

procedure TMainForm.btnAltTabClick(Sender: TObject);
var
  i: integer;
begin
  StatusBar1.Panels[2].Text := 'Simulating Alt+Tab...';
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

procedure TMainForm.btnSpyBringToTopClick(Sender: TObject);
var
  h: TWindowHandle;
begin
  h := GetSelectedSpyWindow;
  if h <> 0 then
    WindowMgr.RaiseWindow(h);
end;

procedure TMainForm.btnSpyCloseClick(Sender: TObject);
var
  h: TWindowHandle;
begin
  h := GetSelectedSpyWindow;
  if h <> 0 then
    if MessageDlg('Close Window', 'Are you sure you want to close this window?', mtConfirmation, [mbYes, mbNo], 0) = mrYes then
      WindowMgr.CloseWindow(h);
end;

procedure TMainForm.btnSpyCopyInfoClick(Sender: TObject);
var
  h: TWindowHandle;
  Info: TWindowInfo;
  R: TRect;
  InfoText: String;
begin
  h := GetSelectedSpyWindow;
  if (h = 0) or (WindowMgr = nil) then Exit;

  if WindowMgr.GetWindowInfo(h, Info) then
  begin
    R := Info.Rect;
    InfoText := Format(
      'Handle: 0x%x' + LineEnding +
      'Title: %s' + LineEnding +
      'Class: %s' + LineEnding +
      'Rect: (%d, %d) - (%d, %d) [%dx%d]' + LineEnding +
      'PID: %d' + LineEnding +
      'Process: %s',
      [h, Info.Title, Info.ClassName,
       R.Left, R.Top, R.Right, R.Bottom, R.Right - R.Left, R.Bottom - R.Top,
       Info.ProcessID, Info.ProcessName]);
    Clipboard.AsText := InfoText;
    StatusBar1.Panels[2].Text := 'Window info copied to clipboard';
  end;
end;

procedure TMainForm.btnSpyFlashClick(Sender: TObject);
var
  h: TWindowHandle;
begin
  h := GetSelectedSpyWindow;
  if h <> 0 then
    FlashWindow(h);
end;

procedure TMainForm.btnSpyFocusClick(Sender: TObject);
var
  h: TWindowHandle;
begin
  h := GetSelectedSpyWindow;
  if h <> 0 then
  begin
    WindowMgr.ActivateWindow(h);
    WindowMgr.FocusWindow(h);
  end;
end;

procedure TMainForm.btnSpyHideClick(Sender: TObject);
var
  h: TWindowHandle;
begin
  h := GetSelectedSpyWindow;
  if h <> 0 then
    WindowMgr.HideWindow(h);
end;

procedure TMainForm.btnSpyLocateClick(Sender: TObject);
var
  h: TWindowHandle;
begin
  h := GetSelectedSpyWindow;
  if h <> 0 then
    LocateWindowInTree(h);
end;

procedure TMainForm.btnSpyMaximizeClick(Sender: TObject);
var
  h: TWindowHandle;
begin
  h := GetSelectedSpyWindow;
  if h <> 0 then
    WindowMgr.MaximizeWindow(h);
end;

procedure TMainForm.btnSpyMinimizeClick(Sender: TObject);
var
  h: TWindowHandle;
begin
  h := GetSelectedSpyWindow;
  if h <> 0 then
    WindowMgr.MinimizeWindow(h);
end;

procedure TMainForm.btnSpyRefreshClick(Sender: TObject);
begin
  RefreshWindowTree;
end;

procedure TMainForm.btnSpyRestoreClick(Sender: TObject);
var
  h: TWindowHandle;
begin
  h := GetSelectedSpyWindow;
  if h <> 0 then
    WindowMgr.RestoreWindow(h);
end;

procedure TMainForm.btnSpySearchClick(Sender: TObject);
var
  SearchText: String;
  h: TWindowHandle;
begin
  SearchText := Trim(edtSpySearch.Text);
  if SearchText = '' then Exit;

  h := WindowMgr.FindWindow(SearchText);
  if h <> 0 then
  begin
    LocateWindowInTree(h);
    FlashWindow(h);
  end
  else
    ShowMessage('No window found matching: ' + SearchText);
end;

procedure TMainForm.btnSpyShowClick(Sender: TObject);
var
  h: TWindowHandle;
begin
  h := GetSelectedSpyWindow;
  if h <> 0 then
    WindowMgr.ShowWindow(h);
end;

procedure TMainForm.btnToggleCapsLockChange(Sender: TObject);
begin
  if UpdatingCapsLock then Exit;
  if btnToggleCapsLock.Checked then
    btnToggleCapsLock.Caption := '🔒 CAPS Lock ON'
  else
    btnToggleCapsLock.Caption := '🔓 CAPS Lock OFF';
end;

procedure TMainForm.btnToggleCapsLockClick(Sender: TObject);
var
  newState: boolean;
begin
  if UpdatingCapsLock then Exit;
  UpdatingCapsLock := True;
  try
    KeyInput.ToggleCapsLock;
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

procedure TMainForm.btnToggleNumLockChange(Sender: TObject);
begin
  if UpdatingNumLock then Exit;
  if btnToggleNumLock.Checked then
    btnToggleNumLock.Caption := '🔒 NUM ON'
  else
    btnToggleNumLock.Caption := 'NUM Off';
end;

procedure TMainForm.btnToggleNumLockClick(Sender: TObject);
var
  newState: boolean;
begin
  if UpdatingNumLock then Exit;
  UpdatingNumLock := True;
  try
    KeyInput.ToggleNumLock;
    Sleep(100);
    newState := KeyInput.GetNumLockState;
    btnToggleNumLock.Checked := newState;
    if newState then
      StatusBar1.Panels[2].Text := 'NumLock is ON'
    else
      StatusBar1.Panels[2].Text := 'NumLock is OFF';
  finally
    UpdatingNumLock := False;
  end;
end;

procedure TMainForm.btnToggleScrollLockChange(Sender: TObject);
begin
  if UpdatingScrollLock then Exit;
  if btnToggleScrollLock.Checked then
    btnToggleScrollLock.Caption := '🔒 SCR ON'
  else
    btnToggleScrollLock.Caption := 'SCR Off';
end;

procedure TMainForm.btnToggleScrollLockClick(Sender: TObject);
var
  newState: boolean;
begin
  if UpdatingScrollLock then Exit;
  UpdatingScrollLock := True;
  try
    KeyInput.ToggleScrollLock;
    Sleep(100);
    newState := KeyInput.GetScrollLockState;
    btnToggleScrollLock.Checked := newState;
    if newState then
      StatusBar1.Panels[2].Text := 'ScrollLock is ON'
    else
      StatusBar1.Panels[2].Text := 'ScrollLock is OFF';
  finally
    UpdatingScrollLock := False;
  end;
end;

procedure TMainForm.RunMouseDemo(Data: PtrInt);
var
  Origin: TPoint;
  W, H: Integer;

  { press, walk the pointer through the points, release - one drawn stroke }
  procedure Stroke(const Pts: array of TPoint);
  var
    I: Integer;
  begin
    MouseInput.Move([], Origin.X + Pts[0].X, Origin.Y + Pts[0].Y, 200);
    MouseInput.Down(mbLeft, []);
    for I := 1 to High(Pts) do
      MouseInput.Move([], Origin.X + Pts[I].X, Origin.Y + Pts[I].Y, 250);
    MouseInput.Up(mbLeft, []);
  end;

begin
  StatusBar1.Panels[2].Text := 'Running mouse demo...';
  Origin := PaintBox1.ClientToScreen(Types.Point(0, 0));
  W := PaintBox1.Width;
  H := PaintBox1.Height;
  Stroke([Types.Point(10, 10), Types.Point(W - 10, 10),
    Types.Point(W - 10, H - 10), Types.Point(10, H - 10), Types.Point(10, 10)]);
  Stroke([Types.Point(10, 10), Types.Point(W - 10, H - 10)]);
  Stroke([Types.Point(W - 10, 10), Types.Point(10, H - 10)]);
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
  UpdatingCapsLock := False;
  UpdatingNumLock := False;
  UpdatingScrollLock := False;
  LastClickPos := Types.Point(0, 0);

  // Initialize window spy
  FinderDragging := False;
  SelectedSpyWindow := 0;
  SpyFlashCount := 0;

  BitmapCanvasDemo := TBitmap.Create;
  BitmapCanvasDemo.SetSize(PaintBox1.Width, PaintBox1.Height);
  BitmapCanvasDemo.Canvas.Brush.Color := clWhite;
  BitmapCanvasDemo.Canvas.FillRect(BitmapCanvasDemo.Canvas.ClipRect);

  GlobalKeyListener := CreateGlobalKeyListener;
  GlobalKeyListener.OnKeyPress := @OnGlobalKeyPress;

  GlobalMouseListener := CreateGlobalMouseListener;
  if GlobalMouseListener <> nil then
    GlobalMouseListener.OnMouseClick := @OnGlobalMouseClick;

  if ReplayHotkey.Registered then
  begin
    lblHotKeyCombo.Caption := 'Or focus any application and press Ctrl+Shift+F9 to type it there.';
    lblHotKeyCombo.Font.Color := clGreen;
    StatusBar1.Panels[2].Text := 'Hotkey registered successfully';
  end;

  try
    btnToggleCapsLock.Checked := KeyInput.GetCapsLockState;
  except
    btnToggleCapsLock.Checked := False;
  end;

  // Initial window tree refresh
  RefreshWindowTree;
end;

procedure TMainForm.FormDestroy(Sender: TObject);
begin
  if Assigned(GlobalKeyListener) then
  begin
    GlobalKeyListener.StopMonitoring;
    GlobalKeyListener.Free;
  end;

  if Assigned(GlobalMouseListener) then
  begin
    GlobalMouseListener.StopMonitoring;
    GlobalMouseListener.Free;
  end;

  BitmapCanvasDemo.Free;
end;

procedure TMainForm.btnToggleGlobalKeysClick(Sender: TObject);
begin
  if GlobalKeyListener.Active then
  begin
    GlobalKeyListener.StopMonitoring;
    if GlobalMouseListener <> nil then
      GlobalMouseListener.StopMonitoring;
    btnToggleGlobalKeys.Caption := 'Enable Global Monitoring';
    StatusBar1.Panels[2].Text := 'Global monitoring disabled';
  end
  else
  begin
    GlobalKeyListener.StartMonitoring;
    if GlobalMouseListener <> nil then
      GlobalMouseListener.StartMonitoring;

    if GlobalKeyListener.Active then
    begin
      btnToggleGlobalKeys.Caption := 'Disable Global Monitoring';
      StatusBar1.Panels[2].Text := 'Global monitoring active (keyboard & mouse system-wide)';
    end
    else
    begin
      ShowMessage('Global monitoring not supported on this platform.' + LineEnding +
        'X11 and Windows are supported. Wayland does not allow system-wide capture for security reasons.');
      StatusBar1.Panels[2].Text := 'Global monitoring not available';
    end;
  end;
end;

procedure TMainForm.chkSpyAutoRefreshChange(Sender: TObject);
begin
  TimerSpyRefresh.Enabled := chkSpyAutoRefresh.Checked;
end;

procedure TMainForm.chkStayOnTopChange(Sender: TObject);
begin
  if chkStayOnTop.Checked then
    FormStyle := fsSystemStayOnTop
  else
    FormStyle := fsNormal;
end;

procedure TMainForm.btnTestStayOnTopClick(Sender: TObject);
begin
  if frmStayOnTopTest = nil then
    Application.CreateForm(TfrmStayOnTopTest, frmStayOnTopTest);
  frmStayOnTopTest.Show;
end;

procedure TMainForm.edtSpySearchChange(Sender: TObject);
begin
  // Could implement live search filtering here
end;

procedure TMainForm.OnGlobalKeyPress(Key: word; Shift: TShiftState);
begin
  UpdateKeyDisplay(Key, Shift);
end;

procedure TMainForm.OnGlobalMouseClick(Button: TMouseButton; X, Y: Integer);
var
  ButtonName: string;
begin
  LastClickPos.X := X;
  LastClickPos.Y := Y;

  case Button of
    mbLeft: ButtonName := 'Left';
    mbRight: ButtonName := 'Right';
    mbMiddle: ButtonName := 'Middle';
    mbExtra1: ButtonName := 'Extra1 (Back)';
    mbExtra2: ButtonName := 'Extra2 (Forward)';
  end;

  StatusBar1.Panels[2].Text := Format('Last Click: %s @ (%d, %d)', [ButtonName, X, Y]);
end;

procedure TMainForm.FormKeyDown(Sender: TObject; var Key: word; Shift: TShiftState);
begin
  UpdateKeyDisplay(Key, Shift);
end;

procedure TMainForm.FormResize(Sender: TObject);
begin
  if Assigned(BitmapCanvasDemo) then
    BitmapCanvasDemo.Free;

  BitmapCanvasDemo := TBitmap.Create;
  BitmapCanvasDemo.SetSize(PaintBox1.Width, PaintBox1.Height);
  BitmapCanvasDemo.Canvas.Brush.Color := clWhite;
  BitmapCanvasDemo.Canvas.FillRect(BitmapCanvasDemo.Canvas.ClipRect);
end;

procedure TMainForm.ReplayHotkeyPressed(Sender: TObject);
begin
  Application.QueueAsyncCall(@RunReplayHotkey, 0);
end;

procedure TMainForm.ReplayHotkeyError(Sender: TObject; const Message: string);
begin
  lblHotKeyCombo.Caption := 'The global hotkey Ctrl+Shift+F9 is not available: ' + Message;
  lblHotKeyCombo.Font.Color := clRed;
  StatusBar1.Panels[2].Text := 'Hotkey registration failed';
end;

procedure TMainForm.btnTypeToRightClick(Sender: TObject);
begin
  memTestDestination.Clear;
  memTestDestination.SetFocus;
  Application.QueueAsyncCall(@RunTypeToRight, 0);
end;

{ Typing into our own memo blocks our own message loop, so the arriving
  keys would all be translated only after the last one was sent.  A line at
  a time with the queue drained between lines keeps the receiver current -
  and looks like typing. }
procedure TMainForm.RunTypeToRight(Data: PtrInt);
var
  I: Integer;
begin
  StatusBar1.Panels[2].Text := 'Typing the test sheet...';
  for I := 0 to memTestSource.Lines.Count - 1 do
  begin
    Application.ProcessMessages;
    if I > 0 then KeyInput.Press(VK_RETURN);
    KeyInput.PressString(memTestSource.Lines[I]);
  end;
  Application.ProcessMessages;
  StatusBar1.Panels[2].Text := 'Text replay complete';
end;

procedure TMainForm.RunReplayHotkey(Data: PtrInt);
var
  startTime: QWord;
  timeout: boolean;
begin
  StatusBar1.Panels[2].Text := 'Hotkey triggered - waiting for key release...';
  Application.ProcessMessages;

  startTime := GetTickCount64;
  timeout := False;

  while not timeout do
  begin
    if (GetKeyState(VK_CONTROL) >= 0) and (GetKeyState(VK_SHIFT) >= 0) and (GetKeyState(VK_MENU) >= 0) then
      Break;
    Sleep(10);
    Application.ProcessMessages;
    if GetTickCount64 - startTime > 500 then
      timeout := True;
  end;

  if timeout then
    StatusBar1.Panels[2].Text := 'Timeout waiting for keys - typing anyway...'
  else
    StatusBar1.Panels[2].Text := 'Keys released - typing...';
  Application.ProcessMessages;

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
        keyName := Chr(Key)
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
    LastDrawingPoint := Types.Point(X, Y);
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
    LastDrawingPoint := Types.Point(X, Y);
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

procedure TMainForm.DrawFinderTarget(ACanvas: TCanvas; ARect: TRect);
var
  CenterX, CenterY, Radius: Integer;
begin
  CenterX := (ARect.Left + ARect.Right) div 2;
  CenterY := (ARect.Top + ARect.Bottom) div 2;
  Radius := Min(ARect.Right - ARect.Left, ARect.Bottom - ARect.Top) div 2 - 2;

  ACanvas.Brush.Color := clWhite;
  ACanvas.FillRect(ARect);

  ACanvas.Pen.Color := clBlack;
  ACanvas.Pen.Width := 2;
  ACanvas.Brush.Style := bsClear;
  ACanvas.Ellipse(CenterX - Radius, CenterY - Radius, CenterX + Radius, CenterY + Radius);
  ACanvas.Ellipse(CenterX - Radius div 2, CenterY - Radius div 2,
                  CenterX + Radius div 2, CenterY + Radius div 2);

  ACanvas.Brush.Style := bsSolid;
  ACanvas.Brush.Color := clRed;
  ACanvas.Pen.Color := clRed;
  ACanvas.Ellipse(CenterX - 2, CenterY - 2, CenterX + 2, CenterY + 2);

  ACanvas.Pen.Color := clBlack;
  ACanvas.Pen.Width := 1;
  ACanvas.Line(ARect.Left + 1, CenterY, CenterX - Radius div 2 - 1, CenterY);
  ACanvas.Line(CenterX + Radius div 2 + 1, CenterY, ARect.Right - 1, CenterY);
  ACanvas.Line(CenterX, ARect.Top + 1, CenterX, CenterY - Radius div 2 - 1);
  ACanvas.Line(CenterX, CenterY + Radius div 2 + 1, CenterX, ARect.Bottom - 1);
end;

procedure TMainForm.pnlFinderTargetPaint(Sender: TObject);
begin
  DrawFinderTarget(pnlFinderTarget.Canvas, Types.Rect(0, 0, pnlFinderTarget.Width, pnlFinderTarget.Height));
end;

procedure TMainForm.pnlFinderTargetMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  if Button = mbLeft then
  begin
    FinderDragging := True;
    SelectedSpyWindow := 0;
    Screen.Cursor := crCross;
    SetCaptureControl(pnlFinderTarget);
    lblFinderHint.Caption := 'Release on target window';
  end;
end;

procedure TMainForm.pnlFinderTargetMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
var
  ScreenPos: TPoint;
  WinHandle: TWindowHandle;
begin
  if FinderDragging then
  begin
    ScreenPos := pnlFinderTarget.ClientToScreen(Types.Point(X, Y));
    WinHandle := GetWindowAtPoint(ScreenPos.X, ScreenPos.Y);

    if (WinHandle <> 0) and (WinHandle <> SelectedSpyWindow) then
    begin
      SelectedSpyWindow := WinHandle;
      UpdateSpyWindowDetails(WinHandle);
    end;
  end;
end;

procedure TMainForm.pnlFinderTargetMouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  if Button = mbLeft then
  begin
    FinderDragging := False;
    Screen.Cursor := crDefault;
    SetCaptureControl(nil);
    lblFinderHint.Caption := 'Drag to find window';

    if SelectedSpyWindow <> 0 then
    begin
      UpdateSpyWindowDetails(SelectedSpyWindow);
      LocateWindowInTree(SelectedSpyWindow);
    end;
  end;
end;

procedure TMainForm.Timer1Timer(Sender: TObject);
var
  mouseX, mouseY: integer;
  capsState, numState, scrState: boolean;
begin
  mouseX := Mouse.CursorPos.X;
  mouseY := Mouse.CursorPos.Y;
  if (LastClickPos.X <> 0) or (LastClickPos.Y <> 0) then
    StatusBar1.Panels[0].Text := Format('Mouse: (%d, %d) | Last Click: (%d, %d)',
      [mouseX, mouseY, LastClickPos.X, LastClickPos.Y])
  else
    StatusBar1.Panels[0].Text := Format('Mouse: (%d, %d)', [mouseX, mouseY]);

  if not UpdatingCapsLock then
  begin
    try
      capsState := KeyInput.GetCapsLockState;
      if btnToggleCapsLock.Checked <> capsState then
      begin
        UpdatingCapsLock := True;
        try
          btnToggleCapsLock.Checked := capsState;
          if capsState then
            btnToggleCapsLock.Caption := '🔒 CAPS ON'
          else
            btnToggleCapsLock.Caption := '🔓 CAPS OFF';
        finally
          UpdatingCapsLock := False;
        end;
      end;
    except
    end;
  end;

  if not UpdatingNumLock then
  begin
    try
      numState := KeyInput.GetNumLockState;
      if btnToggleNumLock.Checked <> numState then
      begin
        UpdatingNumLock := True;
        try
          btnToggleNumLock.Checked := numState;
          if numState then
            btnToggleNumLock.Caption := '🔒 NUM ON'
          else
            btnToggleNumLock.Caption := 'NUM Off';
        finally
          UpdatingNumLock := False;
        end;
      end;
    except
    end;
  end;

  if not UpdatingScrollLock then
  begin
    try
      scrState := KeyInput.GetScrollLockState;
      if btnToggleScrollLock.Checked <> scrState then
      begin
        UpdatingScrollLock := True;
        try
          btnToggleScrollLock.Checked := scrState;
          if scrState then
            btnToggleScrollLock.Caption := '🔒 SCR ON'
          else
            btnToggleScrollLock.Caption := 'SCR Off';
        finally
          UpdatingScrollLock := False;
        end;
      end;
    except
    end;
  end;

  ResizeStatusBarPanels;
end;

procedure TMainForm.TimerSpyRefreshTimer(Sender: TObject);
begin
  if SelectedSpyWindow <> 0 then
    UpdateSpyWindowDetails(SelectedSpyWindow);
end;

procedure TMainForm.tvWindowsDblClick(Sender: TObject);
var
  h: TWindowHandle;
begin
  h := GetSelectedSpyWindow;
  if h <> 0 then
    FlashWindow(h);
end;

procedure TMainForm.tvWindowsSelectionChanged(Sender: TObject);
var
  Node: TTreeNode;
begin
  Node := tvWindows.Selected;
  if (Node <> nil) and (Node.Data <> nil) then
  begin
    SelectedSpyWindow := TWindowHandle(Node.Data);
    UpdateSpyWindowDetails(SelectedSpyWindow);
  end;
end;

procedure TMainForm.ResizeStatusBarPanels;
var
  i, TotalWidth: Integer;
  PanelTextWidth: array of Integer;
begin
  // Guard against being called before form is fully initialized
  if not Assigned(StatusBar1) then Exit;
  if StatusBar1.Panels.Count = 0 then Exit;
  if StatusBar1.Width <= 0 then Exit;

  TotalWidth := 0;
  SetLength(PanelTextWidth, StatusBar1.Panels.Count);
  for i := 0 to StatusBar1.Panels.Count - 1 do
  begin
    PanelTextWidth[i] := StatusBar1.Canvas.TextWidth(StatusBar1.Panels[i].Text) + 16;
    TotalWidth := TotalWidth + PanelTextWidth[i];
  end;

  // Avoid division by zero
  if TotalWidth <= 0 then Exit;

  if TotalWidth <= StatusBar1.Width then
  begin
    for i := 0 to StatusBar1.Panels.Count - 1 do
      StatusBar1.Panels[i].Width := PanelTextWidth[i];
  end
  else
  begin
    for i := 0 to StatusBar1.Panels.Count - 1 do
      StatusBar1.Panels[i].Width := (PanelTextWidth[i] * StatusBar1.Width) div TotalWidth;
  end;
end;

// ============================================================================
// Window Spy Implementation
// ============================================================================

procedure TMainForm.RefreshWindowTree;
var
  WindowList: TList;
  i: Integer;
  WinHandle: TWindowHandle;
begin
  tvWindows.Items.BeginUpdate;
  try
    tvWindows.Items.Clear;

    EnsureWindowManager;
    if WindowMgr = nil then Exit;

    WindowList := WindowMgr.EnumerateWindows;
    if WindowList = nil then Exit;

    try
      for i := 0 to WindowList.Count - 1 do
      begin
        WinHandle := TWindowHandle(WindowList[i]);
        AddWindowToTree(WinHandle, nil);
      end;
    finally
      WindowList.Free;
    end;

    // Sort by handle for consistent display
    tvWindows.AlphaSort;
  finally
    tvWindows.Items.EndUpdate;
  end;

  StatusBar1.Panels[2].Text := Format('Found %d windows', [tvWindows.Items.Count]);
end;

procedure TMainForm.AddWindowToTree(AHandle: TWindowHandle; AParentNode: TTreeNode);
var
  Info: TWindowInfo;
  Node: TTreeNode;
  NodeText: String;
  ChildList: TList;
  i: Integer;
  ChildHandle: TWindowHandle;
begin
  if WindowMgr.GetWindowInfo(AHandle, Info) then
  begin
    if Info.Title <> '' then
      NodeText := Format('0x%x "%s" [%s]', [AHandle, Info.Title, Info.ClassName])
    else
      NodeText := Format('0x%x [%s]', [AHandle, Info.ClassName]);

    Node := tvWindows.Items.AddChild(AParentNode, NodeText);
    Node.Data := Pointer(AHandle);

    // Add child windows recursively
    ChildList := WindowMgr.GetWindowChildren(AHandle);
    if ChildList <> nil then
    begin
      try
        for i := 0 to ChildList.Count - 1 do
        begin
          ChildHandle := TWindowHandle(ChildList[i]);
          AddWindowToTree(ChildHandle, Node);
        end;
      finally
        ChildList.Free;
      end;
    end;
  end;
end;

procedure TMainForm.UpdateSpyWindowDetails(AHandle: TWindowHandle);
var
  Info: TWindowInfo;
  R: TRect;
  State: string;
begin
  if (AHandle = 0) or (WindowMgr = nil) or not WindowMgr.GetWindowInfo(AHandle, Info) then
  begin
    ClearSpyDetails;
    Exit;
  end;
  edtSpyHandle.Text := Format('0x%x (%d)', [AHandle, AHandle]);
  edtSpyCaption.Text := Info.Title;
  edtSpyClass.Text := Info.ClassName;
  R := Info.Rect;
  edtSpyRect.Text := Format('(%d, %d) - (%d, %d) [%dx%d]',
    [R.Left, R.Top, R.Right, R.Bottom, R.Right - R.Left, R.Bottom - R.Top]);
  edtSpyProcess.Text := Format('%s (PID: %d)', [Info.ProcessName, Info.ProcessID]);
  if Info.IsMinimized then State := 'minimized'
  else if Info.IsMaximized then State := 'maximized'
  else State := 'normal';
  if not Info.IsVisible then State := State + ', hidden';
  edtSpyState.Text := State;
end;

procedure TMainForm.ClearSpyDetails;
begin
  edtSpyHandle.Text := '';
  edtSpyCaption.Text := '';
  edtSpyClass.Text := '';
  edtSpyRect.Text := '';
  edtSpyProcess.Text := '';
  edtSpyState.Text := '';
end;

function TMainForm.GetSelectedSpyWindow: TWindowHandle;
begin
  Result := SelectedSpyWindow;
end;

{ Catch the eye with the package's own moves: come forward, wiggle. }
procedure TMainForm.FlashWindow(AHandle: TWindowHandle);
var
  R: TRect;
  I: Integer;
begin
  WindowMgr.ActivateWindow(AHandle);
  R := WindowMgr.GetWindowRect(AHandle);
  for I := 1 to 2 do
  begin
    WindowMgr.MoveWindow(AHandle, R.Left + 24, R.Top);
    Sleep(90);
    WindowMgr.MoveWindow(AHandle, R.Left, R.Top);
    Sleep(90);
  end;
end;

procedure TMainForm.LocateWindowInTree(AHandle: TWindowHandle);
var
  Node: TTreeNode;
begin
  Node := FindWindowNode(AHandle, nil);
  if Node <> nil then
  begin
    tvWindows.Selected := Node;
    Node.MakeVisible;
  end;
end;

function TMainForm.FindWindowNode(AHandle: TWindowHandle; AStartNode: TTreeNode): TTreeNode;
var
  Node: TTreeNode;
begin
  Result := nil;

  if AStartNode = nil then
    Node := tvWindows.Items.GetFirstNode
  else
    Node := AStartNode;

  while Node <> nil do
  begin
    if TWindowHandle(Node.Data) = AHandle then
    begin
      Result := Node;
      Exit;
    end;

    // Check children
    if Node.HasChildren then
    begin
      Result := FindWindowNode(AHandle, Node.GetFirstChild);
      if Result <> nil then Exit;
    end;

    Node := Node.GetNextSibling;
  end;
end;

end.
