unit HIDScreenshot;
{$mode objfpc}{$H+}
// Copyright (c) 2026 Tony Stone. 0BSD.
interface

uses Classes, SysUtils, Types, Graphics, WindowManagerIntf;

type
  THIDCaptureSource = (hcsDesktop, hcsMonitor, hcsWindow, hcsRegion, hcsSelection);
  THIDCaptureErrorEvent = procedure(Sender: TObject; const Message: string) of object;

  THIDScreenshot = class(TComponent)
  private
    CaptureSource: THIDCaptureSource;
    MonitorNumber, AreaLeft, AreaTop, AreaWidth, AreaHeight: integer;
    Title: string;
    Image: TBitmap;
    ErrorText: string;
    CapturedHandler: TNotifyEvent;
    ErrorHandler: THIDCaptureErrorEvent;
    function Available: boolean;
    function Failed(const Message: string): boolean;
    function Complete(Value: boolean): boolean;
  protected
    function GrabArea(const Area: TRect; Destination: TBitmap): boolean; virtual;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function Capture: boolean;
    function CaptureArea(const Area: TRect): boolean;
    function CaptureWindow(Handle: TWindowHandle): boolean;
    property Bitmap: TBitmap read Image;
    property LastError: string read ErrorText;
  published
    // What Capture photographs: every monitor together, one monitor, a window
    // found by title, the Region rectangle, or a rectangle the user drags out.
    // On Wayland the desktop may ask the user's permission, and hcsWindow is
    // unavailable. On macOS other programs' windows are in the picture only
    // once the user allows yours under Privacy & Security > Screen Recording.
    property Source: THIDCaptureSource read CaptureSource write CaptureSource default hcsDesktop;
    // Which monitor hcsMonitor captures: -1 is the primary monitor, otherwise
    // an index into Screen.Monitors.
    property MonitorIndex: integer read MonitorNumber write MonitorNumber default -1;
    // Title of the window hcsWindow captures; blank captures the active window.
    // The grab reads the screen, so the window is brought to the front first.
    property WindowTitle: string read Title write Title;
    // Left edge of the hcsRegion rectangle, in screen coordinates. Negative
    // reaches monitors left of the primary.
    property RegionLeft: integer read AreaLeft write AreaLeft default 0;
    // Top edge of the hcsRegion rectangle, in screen coordinates.
    property RegionTop: integer read AreaTop write AreaTop default 0;
    // Width of the hcsRegion rectangle, in pixels.
    property RegionWidth: integer read AreaWidth write AreaWidth default 0;
    // Height of the hcsRegion rectangle, in pixels.
    property RegionHeight: integer read AreaHeight write AreaHeight default 0;
    // Runs after each successful capture. The picture is in Bitmap, which the
    // component owns; copy it to keep it past the next capture.
    property OnCaptured: TNotifyEvent read CapturedHandler write CapturedHandler;
    // Why a capture failed; the message stays in LastError. A selection the
    // user cancels is not an error - Capture just returns False.
    property OnError: THIDCaptureErrorEvent read ErrorHandler write ErrorHandler;
  end;

implementation

uses Forms, Controls, LCLIntf, LCLType, GraphType, Math, WindowManager
  {$ifdef linux}, LinuxScreenshot{$endif}
  {$ifdef darwin}, CocoaCapture{$endif};

type
  TRegionSelector = class(TForm)
  private
    Background: TBitmap;
    StartPoint, EndPoint: TPoint;
    Selecting: boolean;
    procedure DrawSelection(Sender: TObject);
    procedure BeginSelection(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: integer);
    procedure MoveSelection(Sender: TObject; Shift: TShiftState; X, Y: integer);
    procedure EndSelection(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: integer);
    procedure CancelSelection(Sender: TObject; var Key: word; Shift: TShiftState);
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
  Result := Rect(Min(StartPoint.X, EndPoint.X), Min(StartPoint.Y, EndPoint.Y), Max(StartPoint.X, EndPoint.X), Max(StartPoint.Y, EndPoint.Y));
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

procedure TRegionSelector.BeginSelection(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: integer);
begin
  if Button = mbRight then
  begin
    ModalResult := mrCancel;
    Exit;
  end;
  if Button <> mbLeft then Exit;
  StartPoint := Point(X, Y);
  EndPoint := StartPoint;
  Selecting := True;
  MouseCapture := True;
end;

procedure TRegionSelector.MoveSelection(Sender: TObject; Shift: TShiftState; X, Y: integer);
begin
  if not Selecting then Exit;
  EndPoint := Point(EnsureRange(X, 0, ClientWidth), EnsureRange(Y, 0, ClientHeight));
  Invalidate;
end;

procedure TRegionSelector.EndSelection(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: integer);
var
  Area: TRect;
begin
  if not Selecting or (Button <> mbLeft) then Exit;
  MoveSelection(Sender, Shift, X, Y);
  MouseCapture := False;
  Area := SelectedArea;
  Selecting := False;
  Invalidate;
  if (Area.Right > Area.Left) and (Area.Bottom > Area.Top) then ModalResult := mrOk;
end;

procedure TRegionSelector.CancelSelection(Sender: TObject; var Key: word; Shift: TShiftState);
begin
  if Key = VK_ESCAPE then
  begin
    Key := 0;
    ModalResult := mrCancel;
  end;
end;

constructor THIDScreenshot.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  MonitorNumber := -1;
  Image := TBitmap.Create;
end;

destructor THIDScreenshot.Destroy;
begin
  Image.Free;
  inherited Destroy;
end;

function THIDScreenshot.Failed(const Message: string): boolean;
begin
  ErrorText := Message;
  if Assigned(ErrorHandler) then ErrorHandler(Self, Message);
  Result := False;
end;

function THIDScreenshot.Available: boolean;
begin
  ErrorText := '';
  if csDesigning in ComponentState then Exit(Failed('Capture is disabled in the designer.'));
  {$if not defined(windows) and not defined(linux) and not defined(darwin)}
  Exit(Failed('Screenshot capture is not implemented on this platform.'));
  {$endif}
  Result := True;
end;

function THIDScreenshot.GrabArea(const Area: TRect; Destination: TBitmap): boolean;
var
  DC: HDC;
  Raw: TRawImage;
  Whole: TBitmap;
  {$ifdef linux}
  Part: TRect;
  Message: string;
  {$endif}
begin
  {$ifdef darwin}
  // shows other programs' windows only once the user allows Screen Recording
  Exit(CaptureScreenArea(Area, Destination));
  {$endif}
  Result := False;
  Whole := TBitmap.Create;
  try
    {$ifdef linux}
    // A Wayland session keeps the screen to itself; the desktop portal hands
    // out the whole desktop, possibly at a different scale than the LCL sees.
    if GetEnvironmentVariable('WAYLAND_DISPLAY') <> '' then
    begin
      if not PortalScreenshot(Whole, Message) then
      begin
        ErrorText := Message;
        Exit;
      end;
      Part := Area;
      Part.Offset(-Screen.DesktopLeft, -Screen.DesktopTop);
      Part := Rect(MulDiv(Part.Left, Whole.Width, Screen.DesktopWidth), MulDiv(Part.Top, Whole.Height, Screen.DesktopHeight),
        MulDiv(Part.Right, Whole.Width, Screen.DesktopWidth), MulDiv(Part.Bottom, Whole.Height, Screen.DesktopHeight));
      Destination.SetSize(Area.Right - Area.Left, Area.Bottom - Area.Top);
      Destination.Canvas.CopyRect(Rect(0, 0, Destination.Width, Destination.Height), Whole.Canvas, Part);
      Exit(True);
    end;
    {$endif}
    DC := GetDC(0);
    if DC = 0 then Exit;
    // BitBlt from the screen DC reads back black on GTK; the raw image road works.
    try
      if not RawImage_FromDevice(Raw, DC, Area) then Exit;
    finally
      ReleaseDC(0, DC);
    end;
    Whole.LoadFromRawImage(Raw, True);
    // Some widgetsets hand back the whole screen whatever rectangle was asked.
    if (Whole.Width = Area.Right - Area.Left) and (Whole.Height = Area.Bottom - Area.Top) then
      Destination.Assign(Whole)
    else
    begin
      Destination.SetSize(Area.Right - Area.Left, Area.Bottom - Area.Top);
      Destination.Canvas.CopyRect(Rect(0, 0, Destination.Width, Destination.Height), Whole.Canvas, Area);
    end;
    Result := True;
  finally
    Whole.Free;
  end;
end;

function THIDScreenshot.Complete(Value: boolean): boolean;
begin
  Result := Value;
  if not Value then
  begin
    // GrabArea may have left a better reason
    if ErrorText = '' then ErrorText := 'The display could not be captured.';
    Exit(Failed(ErrorText));
  end;
  if Assigned(CapturedHandler) then CapturedHandler(Self);
end;

function THIDScreenshot.CaptureArea(const Area: TRect): boolean;
var
  Clipped: TRect;
begin
  if not Available then Exit(False);
  if not Types.IntersectRect(Clipped, Area, Screen.DesktopRect) then
    Exit(Failed('The capture rectangle is empty or outside the desktop.'));
  Result := Complete(GrabArea(Clipped, Bitmap));
end;

function THIDScreenshot.CaptureWindow(Handle: TWindowHandle): boolean;
begin
  if not Available then Exit(False);
  EnsureWindowManager;
  if not Assigned(WindowMgr) then Exit(Failed('Window lookup is unavailable.'));
  if (Handle = 0) or not WindowMgr.IsWindowVisible(Handle) then Exit(Failed('The window is not visible.'));
  // The grab reads the screen, so whatever covers the window would be in the
  // picture; the window comes to the front first and gets a beat to paint.
  WindowMgr.ActivateWindow(Handle);
  Sleep(200);
  Result := CaptureArea(WindowMgr.GetVisibleRect(Handle));
end;

function THIDScreenshot.Capture: boolean;
var
  Area: TRect;
  Handle: TWindowHandle;
  Background: TBitmap;
  Selector: TRegionSelector;
begin
  if not Available then Exit(False);
  case Source of
    hcsDesktop: Result := CaptureArea(Screen.DesktopRect);
    hcsMonitor: begin
      if MonitorIndex = -1 then Area := Screen.PrimaryMonitor.BoundsRect
      else
      begin
        if (MonitorIndex < 0) or (MonitorIndex >= Screen.MonitorCount) then Exit(Failed('Monitor index is out of range.'));
        Area := Screen.Monitors[MonitorIndex].BoundsRect;
      end;
      Result := CaptureArea(Area);
    end;
    hcsWindow: begin
      EnsureWindowManager;
      if not Assigned(WindowMgr) then Exit(Failed('Window lookup is unavailable.'));
      if WindowTitle = '' then Handle := WindowMgr.GetActiveWindow
      else
        Handle := WindowMgr.FindWindow(WindowTitle);
      Result := CaptureWindow(Handle);
    end;
    hcsRegion: Result := CaptureArea(Rect(RegionLeft, RegionTop, RegionLeft + RegionWidth, RegionTop + RegionHeight));
    hcsSelection: begin
      Background := TBitmap.Create;
      try
        Area := Screen.DesktopRect;
        if not GrabArea(Area, Background) then Exit(Complete(False));
        Selector := TRegionSelector.CreateSelector(Background, Area);
        try
          Result := Selector.ShowModal = mrOk;
          if not Result then Exit;
          Area := Selector.SelectedArea;
          Bitmap.PixelFormat := pf24bit;
          Bitmap.SetSize(Area.Right - Area.Left, Area.Bottom - Area.Top);
          Bitmap.Canvas.CopyRect(Rect(0, 0, Bitmap.Width, Bitmap.Height), Background.Canvas, Area);
          Result := Complete(True);
        finally
          Selector.Free;
        end;
      finally
        Background.Free;
      end;
    end;
  end;
end;

end.
