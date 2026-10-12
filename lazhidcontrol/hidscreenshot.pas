unit HIDScreenshot;
{$mode objfpc}{$H+}
// Copyright (c) 2026 Tony Stone. GPL-2.0-or-later; see COPYING.GPL2.
interface
uses Classes, SysUtils, Types, Graphics, WindowManagerIntf;
type
  THIDCaptureSource = (hcsDesktop, hcsMonitor, hcsWindow, hcsRegion, hcsSelection);
  THIDCaptureErrorEvent = procedure(Sender: TObject; const Message: string) of object;
  THIDScreenshot = class(TComponent)
  private
    CaptureSource: THIDCaptureSource;
    MonitorNumber, AreaLeft, AreaTop, AreaWidth, AreaHeight: Integer;
    Title: string;
    Image: TBitmap;
    ErrorText: string;
    CapturedHandler: TNotifyEvent;
    ErrorHandler: THIDCaptureErrorEvent;
    function Available: Boolean;
    function Failed(const Message: string): Boolean;
    function Complete(Value: Boolean): Boolean;
  protected
    function GrabArea(const Area: TRect; Destination: TBitmap): Boolean; virtual;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function Capture: Boolean;
    function CaptureArea(const Area: TRect): Boolean;
    function CaptureWindow(Handle: TWindowHandle): Boolean;
    property Bitmap: TBitmap read Image;
    property LastError: string read ErrorText;
  published
    property Source: THIDCaptureSource read CaptureSource write CaptureSource default hcsDesktop;
    // -1 is the primary monitor; otherwise a Screen.Monitors index.
    property MonitorIndex: Integer read MonitorNumber write MonitorNumber default -1;
    // For hcsWindow; blank captures the active window. What is grabbed is the
    // window's screen area, so anything overlapping it is in the picture.
    property WindowTitle: string read Title write Title;
    // The hcsRegion rectangle, in screen coordinates.
    property RegionLeft: Integer read AreaLeft write AreaLeft default 0;
    property RegionTop: Integer read AreaTop write AreaTop default 0;
    property RegionWidth: Integer read AreaWidth write AreaWidth default 0;
    property RegionHeight: Integer read AreaHeight write AreaHeight default 0;
    // Bitmap belongs to the component; copy it to keep the image.
    property OnCaptured: TNotifyEvent read CapturedHandler write CapturedHandler;
    // A canceled selection is not an error: Capture returns False with LastError empty.
    property OnError: THIDCaptureErrorEvent read ErrorHandler write ErrorHandler;
  end;
implementation
uses Forms, Controls, LCLIntf, LCLType, GraphType, Math, WindowManager;
type
  TRegionSelector = class(TForm)
  private
    Background: TBitmap;
    StartPoint, EndPoint: TPoint;
    Selecting: Boolean;
    procedure DrawSelection(Sender: TObject);
    procedure BeginSelection(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure MoveSelection(Sender: TObject; Shift: TShiftState; X, Y: Integer);
    procedure EndSelection(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure CancelSelection(Sender: TObject; var Key: Word; Shift: TShiftState);
  public
    constructor CreateSelector(Picture: TBitmap; const Bounds: TRect);
    function SelectedArea: TRect;
  end;
constructor TRegionSelector.CreateSelector(Picture: TBitmap; const Bounds: TRect);
begin
  inherited CreateNew(nil);
  Background := Picture;
  BorderStyle := bsNone;
  Position := poDesigned;
  DefaultMonitor := dmDesktop;
  Scaled := False;
  FormStyle := fsStayOnTop;
  AutoScroll := False;
  KeyPreview := True;
  Cursor := crCross;
  SetBounds(Bounds.Left, Bounds.Top, Bounds.Right - Bounds.Left, Bounds.Bottom - Bounds.Top);
  OnPaint := @DrawSelection;
  OnMouseDown := @BeginSelection;
  OnMouseMove := @MoveSelection;
  OnMouseUp := @EndSelection;
  OnKeyDown := @CancelSelection;
end;
function TRegionSelector.SelectedArea: TRect;
begin
  Result := Rect(Min(StartPoint.X, EndPoint.X), Min(StartPoint.Y, EndPoint.Y),
    Max(StartPoint.X, EndPoint.X), Max(StartPoint.Y, EndPoint.Y));
end;
procedure TRegionSelector.DrawSelection(Sender: TObject);
begin
  Canvas.Draw(0, 0, Background);
  Canvas.Brush.Style := bsClear;
  Canvas.Font.Color := clWhite;
  Canvas.TextOut(16, 16, 'Drag to select. Esc or right-click cancels.');
  Canvas.Pen.Color := clRed;
  Canvas.Pen.Width := 2;
  if Selecting then Canvas.Rectangle(SelectedArea);
end;
procedure TRegionSelector.BeginSelection(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  if Button = mbRight then begin ModalResult := mrCancel; Exit; end;
  if Button <> mbLeft then Exit;
  StartPoint := Point(X, Y);
  EndPoint := StartPoint;
  Selecting := True;
  MouseCapture := True;
end;
procedure TRegionSelector.MoveSelection(Sender: TObject; Shift: TShiftState; X, Y: Integer);
begin
  if not Selecting then Exit;
  EndPoint := Point(EnsureRange(X, 0, ClientWidth), EnsureRange(Y, 0, ClientHeight));
  Invalidate;
end;
procedure TRegionSelector.EndSelection(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var Area: TRect;
begin
  if not Selecting or (Button <> mbLeft) then Exit;
  MoveSelection(Sender, Shift, X, Y);
  MouseCapture := False;
  Area := SelectedArea;
  Selecting := False;
  Invalidate;
  if (Area.Right > Area.Left) and (Area.Bottom > Area.Top) then ModalResult := mrOK;
end;
procedure TRegionSelector.CancelSelection(Sender: TObject; var Key: Word; Shift: TShiftState);
begin if Key = VK_ESCAPE then begin Key := 0; ModalResult := mrCancel; end; end;
constructor THIDScreenshot.Create(AOwner: TComponent);
begin inherited Create(AOwner); MonitorNumber := -1; Image := TBitmap.Create; end;
destructor THIDScreenshot.Destroy;
begin Image.Free; inherited Destroy; end;
function THIDScreenshot.Failed(const Message: string): Boolean;
begin
  ErrorText := Message;
  if Assigned(ErrorHandler) then ErrorHandler(Self, Message);
  Result := False;
end;
function THIDScreenshot.Available: Boolean;
begin
  ErrorText := '';
  if csDesigning in ComponentState then Exit(Failed('Capture is disabled in the designer.'));
  {$ifdef linux}
  if GetEnvironmentVariable('WAYLAND_DISPLAY') <> '' then
    Exit(Failed('Wayland screenshot capture requires a portal backend, which is not implemented.'));
  {$endif}
  {$if not defined(windows) and not defined(linux)}
  Exit(Failed('Screenshot capture is not implemented on this platform.'));
  {$endif}
  Result := True;
end;
function THIDScreenshot.GrabArea(const Area: TRect; Destination: TBitmap): Boolean;
var DC: HDC; Raw: TRawImage; Whole: TBitmap;
begin
  Result := False;
  DC := GetDC(0);
  if DC = 0 then Exit;
  // BitBlt from the screen DC reads back black on GTK; the raw image road works.
  try
    if not RawImage_FromDevice(Raw, DC, Area) then Exit;
  finally ReleaseDC(0, DC); end;
  Whole := TBitmap.Create;
  try
    Whole.LoadFromRawImage(Raw, True);
    // Some widgetsets hand back the whole screen whatever rectangle was asked.
    if (Whole.Width = Area.Right - Area.Left) and (Whole.Height = Area.Bottom - Area.Top) then
      Destination.Assign(Whole)
    else begin
      Destination.SetSize(Area.Right - Area.Left, Area.Bottom - Area.Top);
      Destination.Canvas.CopyRect(Rect(0, 0, Destination.Width, Destination.Height), Whole.Canvas, Area);
    end;
    Result := True;
  finally Whole.Free; end;
end;
function THIDScreenshot.Complete(Value: Boolean): Boolean;
begin
  Result := Value;
  if not Value then Exit(Failed('The display could not be captured.'));
  if Assigned(CapturedHandler) then CapturedHandler(Self);
end;
function THIDScreenshot.CaptureArea(const Area: TRect): Boolean;
var Clipped: TRect;
begin
  if not Available then Exit(False);
  if not Types.IntersectRect(Clipped, Area, Screen.DesktopRect) then
    Exit(Failed('The capture rectangle is empty or outside the desktop.'));
  Result := Complete(GrabArea(Clipped, Bitmap));
end;
function THIDScreenshot.CaptureWindow(Handle: TWindowHandle): Boolean;
begin
  if not Available then Exit(False);
  EnsureWindowManager;
  if not Assigned(WindowMgr) then Exit(Failed('Window lookup is unavailable.'));
  if (Handle = 0) or not WindowMgr.IsWindowVisible(Handle) then Exit(Failed('The window is not visible.'));
  Result := CaptureArea(WindowMgr.GetWindowRect(Handle));
end;
function THIDScreenshot.Capture: Boolean;
var Area: TRect; Handle: TWindowHandle; Background: TBitmap; Selector: TRegionSelector;
begin
  if not Available then Exit(False);
  case Source of
    hcsDesktop: Result := CaptureArea(Screen.DesktopRect);
    hcsMonitor: begin
      if MonitorIndex = -1 then Area := Screen.PrimaryMonitor.BoundsRect
      else begin
        if (MonitorIndex < 0) or (MonitorIndex >= Screen.MonitorCount) then Exit(Failed('Monitor index is out of range.'));
        Area := Screen.Monitors[MonitorIndex].BoundsRect;
      end;
      Result := CaptureArea(Area);
    end;
    hcsWindow: begin
      EnsureWindowManager;
      if not Assigned(WindowMgr) then Exit(Failed('Window lookup is unavailable.'));
      if WindowTitle = '' then Handle := WindowMgr.GetActiveWindow else Handle := WindowMgr.FindWindow(WindowTitle);
      Result := CaptureWindow(Handle);
    end;
    hcsRegion: Result := CaptureArea(Rect(RegionLeft, RegionTop, RegionLeft + RegionWidth, RegionTop + RegionHeight));
    hcsSelection: begin
      Background := TBitmap.Create;
      try
        Area := Screen.DesktopRect;
        if not GrabArea(Area, Background) then Exit(Failed('The display could not be captured.'));
        Selector := TRegionSelector.CreateSelector(Background, Area);
        try
          Result := Selector.ShowModal = mrOK;
          if not Result then Exit;
          Area := Selector.SelectedArea;
          Bitmap.PixelFormat := pf24bit;
          Bitmap.SetSize(Area.Right - Area.Left, Area.Bottom - Area.Top);
          Bitmap.Canvas.CopyRect(Rect(0, 0, Bitmap.Width, Bitmap.Height), Background.Canvas, Area);
          Result := Complete(True);
        finally Selector.Free; end;
      finally Background.Free; end;
    end;
  end;
end;
end.
