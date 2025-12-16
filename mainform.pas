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
  HotkeyInput,
  WindowManager
  {$IFDEF WINDOWS}
  , WinWindowManager, Windows
  {$ENDIF}
  {$IFDEF UNIX}
    {$IFNDEF DARWIN}
    , XWindowManager
    {$ENDIF}
  {$ENDIF}
  // These must be after Windows unit to override shadowed types
  , WindowManagerIntf
  , Graphics
  , MouseAndKeyInput
  , Clipbrd
  ;

type

  { TMainForm }

  TMainForm = class(TForm)
    Bevel1: TBevel;
    btnAltTab: TButton;
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
    btnSpySetCaption: TButton;
    btnSpyShow: TButton;
    btnToggleCapsLock: TToggleBox;
    btnToggleGlobalKeys: TButton;
    btnToggleNumLock: TToggleBox;
    btnToggleScrollLock: TToggleBox;
    btnWaylandInfo: TButton;
    btnTestStayOnTop: TButton;
    chkStayOnTop: TCheckBox;
    chkSpyAutoRefresh: TCheckBox;
    chkSpyEnabled: TCheckBox;
    edtSpyCaption: TEdit;
    edtSpyClass: TEdit;
    edtSpyClassAtom: TEdit;
    edtSpyClassBytes: TEdit;
    edtSpyClassName: TEdit;
    edtSpyClassStyle: TEdit;
    edtSpyClientRect: TEdit;
    edtSpyExStyle: TEdit;
    edtSpyHandle: TEdit;
    edtSpyPID: TEdit;
    edtSpyProcess: TEdit;
    edtSpyProcName: TEdit;
    edtSpyProcPath: TEdit;
    edtSpyRect: TEdit;
    edtSpySearch: TEdit;
    edtSpyStyle: TEdit;
    edtSpyThread: TEdit;
    edtSpyTID: TEdit;
    edtSpyWndBytes: TEdit;
    edtSpyWndProc: TEdit;
    gbSpyWindowActions: TGroupBox;
    lblFinderHint: TLabel;
    lblHotKeyCombo: TLabel;
    lblMatchStatus: TLabel;
    lblSpyCaption: TLabel;
    lblSpyClass: TLabel;
    lblSpyClassAtom: TLabel;
    lblSpyClassBytes: TLabel;
    lblSpyClassName: TLabel;
    lblSpyClassStyle: TLabel;
    lblSpyClassStyleFlags: TLabel;
    lblSpyClientRect: TLabel;
    lblSpyExStyleInfo: TLabel;
    lblSpyExStylesList: TLabel;
    lblSpyHandle: TLabel;
    lblSpyPID: TLabel;
    lblSpyProcess: TLabel;
    lblSpyProcName: TLabel;
    lblSpyProcPath: TLabel;
    lblSpyPropertiesHint: TLabel;
    lblSpyQuickCaption: TLabel;
    lblSpyQuickCaptionVal: TLabel;
    lblSpyQuickClass: TLabel;
    lblSpyQuickClassVal: TLabel;
    lblSpyQuickHandle: TLabel;
    lblSpyQuickHandleVal: TLabel;
    lblSpyQuickRect: TLabel;
    lblSpyQuickRectVal: TLabel;
    lblSpyRect: TLabel;
    lblSpyStylesInfo: TLabel;
    lblSpyStylesList: TLabel;
    lblSpyThread: TLabel;
    lblSpyTID: TLabel;
    lblSpyWndBytes: TLabel;
    lblSpyWndProc: TLabel;
    lbSpyClassStyles: TListBox;
    lbSpyExStyles: TListBox;
    lbSpyStyles: TListBox;
    lvSpyProperties: TListView;
    memTestDestination: TMemo;
    memTestSource: TMemo;
    PageControl1: TPageControl;
    PaintBox1: TPaintBox;
    pcSpyDetails: TPageControl;
    pnlFinderTarget: TPanel;
    pnlSpyLeft: TPanel;
    pnlSpyQuickInfo: TPanel;
    pnlSpyRight: TPanel;
    pnlSpyToolbar: TPanel;
    SpySplitter: TSplitter;
    StatusBar1: TStatusBar;
    TabSheet1: TTabSheet;
    TabSheet2: TTabSheet;
    TabSheet3: TTabSheet;
    Timer1: TTimer;
    TimerSpyRefresh: TTimer;
    tsSpyClass: TTabSheet;
    tsSpyGeneral: TTabSheet;
    tsSpyProcess: TTabSheet;
    tsSpyProperties: TTabSheet;
    tsSpyStyles: TTabSheet;
    tvWindows: TTreeView;
    procedure btnAltTabClick(Sender: TObject);
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
    procedure btnSpySetCaptionClick(Sender: TObject);
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
    procedure chkSpyEnabledClick(Sender: TObject);
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
    ReplayHotkey: THotkey;
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
    procedure UpdateKeyDisplay(Key: word; Shift: TShiftState);
    procedure OnGlobalKeyPress(Key: word; Shift: TShiftState);
    procedure OnGlobalMouseClick(Button: TMouseButton; X, Y: Integer);
    procedure OnReplayHotkey(Sender: TObject; Key: word; Shift: TShiftState);
    procedure DrawFinderTarget(ACanvas: TCanvas; ARect: TRect);
    procedure ResizeStatusBarPanels;

    // Window Spy methods
    procedure RefreshWindowTree;
    procedure AddWindowToTree(AHandle: TWindowHandle; AParentNode: TTreeNode);
    procedure UpdateSpyWindowDetails(AHandle: TWindowHandle);
    procedure UpdateSpyQuickInfo(AHandle: TWindowHandle);
    procedure ClearSpyDetails;
    function GetSelectedSpyWindow: TWindowHandle;
    procedure FlashWindow(AHandle: TWindowHandle);
    procedure LocateWindowInTree(AHandle: TWindowHandle);
    function FindWindowNode(AHandle: TWindowHandle; AStartNode: TTreeNode): TTreeNode;
    procedure PopulateStylesList(AHandle: TWindowHandle);
    procedure PopulateClassInfo(AHandle: TWindowHandle);
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

procedure TMainForm.btnSpySetCaptionClick(Sender: TObject);
{$IFDEF WINDOWS}
var
  h: TWindowHandle;
begin
  h := GetSelectedSpyWindow;
  if h <> 0 then
  begin
    SetWindowText(HWND(h), PChar(edtSpyCaption.Text));
    UpdateSpyWindowDetails(h);
  end;
end;
{$ELSE}
begin
  // Setting window caption not supported on this platform - silently ignore
end;
{$ENDIF}

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
  paintBoxScreenPos := PaintBox1.ClientToScreen(Types.Point(0, 0));
  paintBoxWidth := PaintBox1.Width;
  paintBoxHeight := PaintBox1.Height;

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

  adjustedPoints[0] := Types.Point(paintBoxScreenPos.X + offset, paintBoxScreenPos.Y + offset);
  adjustedPoints[1] := Types.Point(paintBoxScreenPos.X + paintBoxWidth - offset, paintBoxScreenPos.Y + offset);
  adjustedPoints[2] := Types.Point(paintBoxScreenPos.X + offset, paintBoxScreenPos.Y + paintBoxHeight - offset);
  adjustedPoints[3] := Types.Point(paintBoxScreenPos.X + paintBoxWidth - offset, paintBoxScreenPos.Y + paintBoxHeight - offset);
  adjustedPoints[4] := Types.Point(paintBoxScreenPos.X + (paintBoxWidth div 2), paintBoxScreenPos.Y + (paintBoxHeight div 2));

  for i := 0 to High(adjustedPoints) do
  begin
    MouseInput.Move([], adjustedPoints[i].X + radius, adjustedPoints[i].Y, 1000);
    MouseInput.Down(mbLeft, []);
    for n := 0 to steps do
    begin
      angle := n * (2 * Pi / steps);
      MouseInput.Move([],
        adjustedPoints[i].X + Round(radius * Cos(angle)),
        adjustedPoints[i].Y + Round(radius * Sin(angle)), (200 div steps));
    end;
    steps := steps * 2;
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

procedure TMainForm.chkSpyEnabledClick(Sender: TObject);
{$IFDEF WINDOWS}
var
  h: TWindowHandle;
begin
  h := GetSelectedSpyWindow;
  if h <> 0 then
    EnableWindow(HWND(h), chkSpyEnabled.Checked);
end;
{$ELSE}
begin
  // Enable/Disable window not supported on this platform - silently ignore
end;
{$ENDIF}

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
  else
    ButtonName := 'Unknown';
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

procedure TMainForm.OnReplayHotkey(Sender: TObject; Key: word; Shift: TShiftState);
begin
  Application.QueueAsyncCall(@RunReplayHotkey, 0);
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
      UpdateSpyQuickInfo(WinHandle);
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
begin
  if (AHandle = 0) or (WindowMgr = nil) then
  begin
    ClearSpyDetails;
    Exit;
  end;

  if not WindowMgr.GetWindowInfo(AHandle, Info) then
  begin
    ClearSpyDetails;
    Exit;
  end;

  // General tab
  edtSpyHandle.Text := Format('0x%x (%d)', [AHandle, AHandle]);
  edtSpyCaption.Text := Info.Title;
  edtSpyClass.Text := Info.ClassName;

  R := Info.Rect;
  edtSpyRect.Text := Format('(%d, %d) - (%d, %d) [%dx%d]',
    [R.Left, R.Top, R.Right, R.Bottom, R.Right - R.Left, R.Bottom - R.Top]);

  // Client rect would require platform-specific calls
  edtSpyClientRect.Text := '(not available)';

  edtSpyProcess.Text := Format('%s (PID: %d)', [Info.ProcessName, Info.ProcessID]);
  edtSpyThread.Text := '(not available)';

  // Update enabled checkbox
  {$IFDEF WINDOWS}
  chkSpyEnabled.Checked := IsWindowEnabled(HWND(AHandle));
  {$ELSE}
  chkSpyEnabled.Checked := True;
  {$ENDIF}

  // Process tab
  edtSpyPID.Text := IntToStr(Info.ProcessID);
  edtSpyTID.Text := '(not available)';
  edtSpyProcName.Text := Info.ProcessName;
  edtSpyProcPath.Text := '(not available)';

  // Update styles
  PopulateStylesList(AHandle);

  // Update class info
  PopulateClassInfo(AHandle);

  // Update quick info panel
  UpdateSpyQuickInfo(AHandle);
end;

procedure TMainForm.UpdateSpyQuickInfo(AHandle: TWindowHandle);
var
  Info: TWindowInfo;
  R: TRect;
begin
  if (AHandle = 0) or (WindowMgr = nil) then Exit;

  if WindowMgr.GetWindowInfo(AHandle, Info) then
  begin
    lblSpyQuickHandleVal.Caption := Format('0x%x', [AHandle]);
    lblSpyQuickClassVal.Caption := Info.ClassName;
    lblSpyQuickCaptionVal.Caption := Info.Title;

    R := Info.Rect;
    lblSpyQuickRectVal.Caption := Format('(%d, %d) %dx%d',
      [R.Left, R.Top, R.Right - R.Left, R.Bottom - R.Top]);
  end;
end;

procedure TMainForm.ClearSpyDetails;
begin
  edtSpyHandle.Text := '';
  edtSpyCaption.Text := '';
  edtSpyClass.Text := '';
  edtSpyRect.Text := '';
  edtSpyClientRect.Text := '';
  edtSpyProcess.Text := '';
  edtSpyThread.Text := '';
  edtSpyStyle.Text := '';
  edtSpyExStyle.Text := '';
  edtSpyClassName.Text := '';
  edtSpyClassAtom.Text := '';
  edtSpyClassStyle.Text := '';
  edtSpyWndProc.Text := '';
  edtSpyClassBytes.Text := '';
  edtSpyWndBytes.Text := '';
  edtSpyPID.Text := '';
  edtSpyTID.Text := '';
  edtSpyProcName.Text := '';
  edtSpyProcPath.Text := '';

  lbSpyStyles.Clear;
  lbSpyExStyles.Clear;
  lbSpyClassStyles.Clear;
  lvSpyProperties.Clear;

  lblSpyQuickHandleVal.Caption := '0x00000000';
  lblSpyQuickClassVal.Caption := '-';
  lblSpyQuickCaptionVal.Caption := '-';
  lblSpyQuickRectVal.Caption := '-';
end;

function TMainForm.GetSelectedSpyWindow: TWindowHandle;
begin
  Result := SelectedSpyWindow;
end;

procedure TMainForm.FlashWindow(AHandle: TWindowHandle);
{$IFDEF WINDOWS}
var
  i: Integer;
begin
  // Flash the window by rapidly showing/hiding its border
  for i := 1 to 3 do
  begin
    // This uses the FlashWindow API for visual feedback
    Windows.FlashWindow(HWND(AHandle), True);
    Sleep(100);
    Windows.FlashWindow(HWND(AHandle), False);
    Sleep(100);
  end;
end;
{$ELSE}
begin
  // On Linux/X11, we could potentially draw a highlight rectangle
  // For now just bring it to front
  WindowMgr.ActivateWindow(AHandle);
end;
{$ENDIF}

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

procedure TMainForm.PopulateStylesList(AHandle: TWindowHandle);
{$IFDEF WINDOWS}
var
  Style, ExStyle: LONG_PTR;
begin
  Style := GetWindowLongPtr(HWND(AHandle), GWL_STYLE);
  ExStyle := GetWindowLongPtr(HWND(AHandle), GWL_EXSTYLE);

  edtSpyStyle.Text := Format('0x%x', [Style]);
  edtSpyExStyle.Text := Format('0x%x', [ExStyle]);

  lbSpyStyles.Clear;
  // Common window styles
  if (Style and WS_OVERLAPPED) <> 0 then lbSpyStyles.Items.Add('WS_OVERLAPPED');
  if (Style and WS_POPUP) <> 0 then lbSpyStyles.Items.Add('WS_POPUP');
  if (Style and WS_CHILD) <> 0 then lbSpyStyles.Items.Add('WS_CHILD');
  if (Style and WS_MINIMIZE) <> 0 then lbSpyStyles.Items.Add('WS_MINIMIZE');
  if (Style and WS_VISIBLE) <> 0 then lbSpyStyles.Items.Add('WS_VISIBLE');
  if (Style and WS_DISABLED) <> 0 then lbSpyStyles.Items.Add('WS_DISABLED');
  if (Style and WS_CLIPSIBLINGS) <> 0 then lbSpyStyles.Items.Add('WS_CLIPSIBLINGS');
  if (Style and WS_CLIPCHILDREN) <> 0 then lbSpyStyles.Items.Add('WS_CLIPCHILDREN');
  if (Style and WS_MAXIMIZE) <> 0 then lbSpyStyles.Items.Add('WS_MAXIMIZE');
  if (Style and WS_CAPTION) = WS_CAPTION then lbSpyStyles.Items.Add('WS_CAPTION');
  if (Style and WS_BORDER) <> 0 then lbSpyStyles.Items.Add('WS_BORDER');
  if (Style and WS_DLGFRAME) <> 0 then lbSpyStyles.Items.Add('WS_DLGFRAME');
  if (Style and WS_VSCROLL) <> 0 then lbSpyStyles.Items.Add('WS_VSCROLL');
  if (Style and WS_HSCROLL) <> 0 then lbSpyStyles.Items.Add('WS_HSCROLL');
  if (Style and WS_SYSMENU) <> 0 then lbSpyStyles.Items.Add('WS_SYSMENU');
  if (Style and WS_THICKFRAME) <> 0 then lbSpyStyles.Items.Add('WS_THICKFRAME');
  if (Style and WS_MINIMIZEBOX) <> 0 then lbSpyStyles.Items.Add('WS_MINIMIZEBOX');
  if (Style and WS_MAXIMIZEBOX) <> 0 then lbSpyStyles.Items.Add('WS_MAXIMIZEBOX');

  lbSpyExStyles.Clear;
  // Extended styles
  if (ExStyle and WS_EX_DLGMODALFRAME) <> 0 then lbSpyExStyles.Items.Add('WS_EX_DLGMODALFRAME');
  if (ExStyle and WS_EX_NOPARENTNOTIFY) <> 0 then lbSpyExStyles.Items.Add('WS_EX_NOPARENTNOTIFY');
  if (ExStyle and WS_EX_TOPMOST) <> 0 then lbSpyExStyles.Items.Add('WS_EX_TOPMOST');
  if (ExStyle and WS_EX_ACCEPTFILES) <> 0 then lbSpyExStyles.Items.Add('WS_EX_ACCEPTFILES');
  if (ExStyle and WS_EX_TRANSPARENT) <> 0 then lbSpyExStyles.Items.Add('WS_EX_TRANSPARENT');
  if (ExStyle and WS_EX_MDICHILD) <> 0 then lbSpyExStyles.Items.Add('WS_EX_MDICHILD');
  if (ExStyle and WS_EX_TOOLWINDOW) <> 0 then lbSpyExStyles.Items.Add('WS_EX_TOOLWINDOW');
  if (ExStyle and WS_EX_WINDOWEDGE) <> 0 then lbSpyExStyles.Items.Add('WS_EX_WINDOWEDGE');
  if (ExStyle and WS_EX_CLIENTEDGE) <> 0 then lbSpyExStyles.Items.Add('WS_EX_CLIENTEDGE');
  if (ExStyle and WS_EX_CONTEXTHELP) <> 0 then lbSpyExStyles.Items.Add('WS_EX_CONTEXTHELP');
  if (ExStyle and WS_EX_RIGHT) <> 0 then lbSpyExStyles.Items.Add('WS_EX_RIGHT');
  if (ExStyle and WS_EX_RTLREADING) <> 0 then lbSpyExStyles.Items.Add('WS_EX_RTLREADING');
  if (ExStyle and WS_EX_LEFTSCROLLBAR) <> 0 then lbSpyExStyles.Items.Add('WS_EX_LEFTSCROLLBAR');
  if (ExStyle and WS_EX_CONTROLPARENT) <> 0 then lbSpyExStyles.Items.Add('WS_EX_CONTROLPARENT');
  if (ExStyle and WS_EX_STATICEDGE) <> 0 then lbSpyExStyles.Items.Add('WS_EX_STATICEDGE');
  if (ExStyle and WS_EX_APPWINDOW) <> 0 then lbSpyExStyles.Items.Add('WS_EX_APPWINDOW');
  if (ExStyle and WS_EX_LAYERED) <> 0 then lbSpyExStyles.Items.Add('WS_EX_LAYERED');
  if (ExStyle and WS_EX_COMPOSITED) <> 0 then lbSpyExStyles.Items.Add('WS_EX_COMPOSITED');
  if (ExStyle and WS_EX_NOACTIVATE) <> 0 then lbSpyExStyles.Items.Add('WS_EX_NOACTIVATE');
end;
{$ELSE}
begin
  edtSpyStyle.Text := '(Windows only)';
  edtSpyExStyle.Text := '(Windows only)';
  lbSpyStyles.Clear;
  lbSpyExStyles.Clear;
end;
{$ENDIF}

procedure TMainForm.PopulateClassInfo(AHandle: TWindowHandle);
var
  Info: TWindowInfo;
begin
  if (WindowMgr = nil) or not WindowMgr.GetWindowInfo(AHandle, Info) then Exit;

  edtSpyClassName.Text := Info.ClassName;

  {$IFDEF WINDOWS}
  edtSpyClassAtom.Text := Format('0x%x', [GetClassLongPtr(HWND(AHandle), GCW_ATOM)]);
  edtSpyClassStyle.Text := Format('0x%x', [GetClassLongPtr(HWND(AHandle), GCL_STYLE)]);
  edtSpyWndProc.Text := Format('0x%x', [GetClassLongPtr(HWND(AHandle), GCLP_WNDPROC)]);
  edtSpyClassBytes.Text := IntToStr(GetClassLongPtr(HWND(AHandle), GCL_CBCLSEXTRA));
  edtSpyWndBytes.Text := IntToStr(GetClassLongPtr(HWND(AHandle), GCL_CBWNDEXTRA));

  lbSpyClassStyles.Clear;
  // Add class style flags here if needed
  {$ELSE}
  edtSpyClassAtom.Text := '(Windows only)';
  edtSpyClassStyle.Text := '(Windows only)';
  edtSpyWndProc.Text := '(Windows only)';
  edtSpyClassBytes.Text := '(Windows only)';
  edtSpyWndBytes.Text := '(Windows only)';
  lbSpyClassStyles.Clear;
  {$ENDIF}
end;

end.
