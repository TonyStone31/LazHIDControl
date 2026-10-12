unit HIDWindow;
{$mode objfpc}{$H+}
// Copyright (c) 2026 Tony Stone. GPL-2.0-or-later; see COPYING.GPL2.
interface
uses Classes, SysUtils, Types, ExtCtrls, WindowManagerIntf;
type
  THIDWindowErrorEvent = procedure(Sender: TObject; const Message: string) of object;
  THIDWindow = class(TComponent)
  private
    TitleFilter, ClassFilter, ErrorText: string;
    PIDFilter: LongWord;
    Exact, Visible: Boolean;
    TimeoutValue: Cardinal;
    Target: TWindowHandle;
    Poll: TTimer;
    Deadline: QWord;
    FoundHandler, TimeoutHandler: TNotifyEvent;
    ErrorHandler: THIDWindowErrorEvent;
    function Available: Boolean;
    function Fail(const Message: string): Boolean;
    function Matches(const Info: TWindowInfo): Boolean;
    procedure Tick(Sender: TObject);
    function GetWaiting: Boolean;
  protected
    function Backend: TWindowManager; virtual;
  public
    constructor Create(AOwner: TComponent); override;
    function Find: Boolean;
    // Caller owns the returned list. Each item is a window handle, not an object.
    function FindAll: TList;
    function WaitForWindow: Boolean;
    procedure CancelWait;
    function Activate: Boolean;
    function MoveTo(X, Y: Integer): Boolean;
    function Resize(Width, Height: Integer): Boolean;
    function Minimize: Boolean;
    function Restore: Boolean;
    function GetInfo(out Info: TWindowInfo): Boolean;
    property Handle: TWindowHandle read Target;
    property Waiting: Boolean read GetWaiting;
    property LastError: string read ErrorText;
  published
    // Title to find. Blank accepts any title. Searches run only when requested.
    property WindowTitle: string read TitleFilter write TitleFilter;
    // Optional native window class. Combined with title and process ID filters.
    property WindowClass: string read ClassFilter write ClassFilter;
    // Optional process ID. Zero accepts windows belonging to any process.
    property ProcessID: LongWord read PIDFilter write PIDFilter default 0;
    // False matches part of title/class; True matches the entire value. Case insensitive.
    property ExactMatch: Boolean read Exact write Exact default False;
    // True excludes hidden and minimized windows from searches.
    property VisibleOnly: Boolean read Visible write Visible default True;
    // Maximum milliseconds for WaitForWindow. Zero performs one immediate search.
    property WaitTimeout: Cardinal read TimeoutValue write TimeoutValue default 5000;
    // Called when Find or WaitForWindow finds a match. Handle identifies that window.
    property OnFound: TNotifyEvent read FoundHandler write FoundHandler;
    // Called when a requested wait expires. CancelWait does not fire this event.
    property OnTimeout: TNotifyEvent read TimeoutHandler write TimeoutHandler;
    // Reports unsupported backends and failed operations; no automatic dialogs.
    property OnError: THIDWindowErrorEvent read ErrorHandler write ErrorHandler;
  end;
implementation
uses WindowManager, LazUTF8;
constructor THIDWindow.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  Visible := True;
  TimeoutValue := 5000;
  Poll := TTimer.Create(Self);
  Poll.Enabled := False;
  Poll.Interval := 100;
  Poll.OnTimer := @Tick;
end;
function THIDWindow.Backend: TWindowManager;
begin EnsureWindowManager; Result := WindowMgr; end;
function THIDWindow.Fail(const Message: string): Boolean;
begin
  ErrorText := Message;
  Result := False;
  if Assigned(ErrorHandler) then ErrorHandler(Self, Message);
end;
function THIDWindow.Available: Boolean;
begin
  ErrorText := '';
  Result := False;
  if csDesigning in ComponentState then Exit(Fail('Window control is disabled in the designer.'));
  if Backend = nil then Exit(Fail('Window control is not implemented on this platform or session.'));
  Result := True;
end;
function THIDWindow.Matches(const Info: TWindowInfo): Boolean;
  function TextMatches(const Filter, Value: string): Boolean;
  begin
    if Filter = '' then Exit(True);
    if ExactMatch then Result := UTF8LowerCase(Filter) = UTF8LowerCase(Value)
    else Result := Pos(UTF8LowerCase(Filter), UTF8LowerCase(Value)) > 0;
  end;
begin
  Result := (not VisibleOnly or Info.IsVisible) and
    ((ProcessID = 0) or (ProcessID = Info.ProcessID)) and
    TextMatches(WindowTitle, Info.Title) and TextMatches(WindowClass, Info.ClassName);
end;
function THIDWindow.FindAll: TList;
var Windows: TList; Item: Pointer; Info: TWindowInfo;
begin
  Result := TList.Create;
  if not Available then Exit;
  Windows := Backend.EnumerateWindows;
  try
    for Item in Windows do
      if Backend.GetWindowInfo(TWindowHandle(Item), Info) and Matches(Info) then Result.Add(Item);
  finally Windows.Free; end;
end;
function THIDWindow.Find: Boolean;
var Windows: TList;
begin
  Target := 0;
  Windows := FindAll;
  try
    if Windows.Count > 0 then Target := TWindowHandle(Windows[0]);
  finally Windows.Free; end;
  Result := Target <> 0;
  if Result then begin
    CancelWait;
    if Assigned(FoundHandler) then FoundHandler(Self);
  end;
end;
function THIDWindow.GetWaiting: Boolean;
begin Result := Poll.Enabled; end;
procedure THIDWindow.CancelWait;
begin Poll.Enabled := False; end;
function THIDWindow.WaitForWindow: Boolean;
begin
  CancelWait;
  if Find then Exit(True);
  if LastError <> '' then Exit(False);
  if WaitTimeout = 0 then begin
    if Assigned(TimeoutHandler) then TimeoutHandler(Self);
    Exit(False);
  end;
  Deadline := GetTickCount64 + WaitTimeout;
  Poll.Enabled := True;
  Result := True;
end;
procedure THIDWindow.Tick(Sender: TObject);
begin
  if Find then Exit;
  if LastError <> '' then begin CancelWait; Exit; end;
  if GetTickCount64 >= Deadline then begin
    CancelWait;
    if Assigned(TimeoutHandler) then TimeoutHandler(Self);
  end;
end;
function THIDWindow.GetInfo(out Info: TWindowInfo): Boolean;
begin
  Info := Default(TWindowInfo);
  if not Available then Exit(False);
  if (Handle = 0) or not Backend.GetWindowInfo(Handle, Info) then Exit(Fail('Find a valid window first.'));
  Result := True;
end;
function THIDWindow.Activate: Boolean;
var Info: TWindowInfo;
begin
  if not GetInfo(Info) then Exit(False);
  Result := Backend.ActivateWindow(Handle);
  if not Result then Fail('The window could not be activated.');
end;
function THIDWindow.MoveTo(X, Y: Integer): Boolean;
var Info: TWindowInfo;
begin
  if not GetInfo(Info) then Exit(False);
  Result := Backend.MoveWindow(Handle, X, Y);
  if not Result then Fail('The window could not be moved.');
end;
function THIDWindow.Resize(Width, Height: Integer): Boolean;
var Info: TWindowInfo;
begin
  if (Width <= 0) or (Height <= 0) then Exit(Fail('Window dimensions must be positive.'));
  if not GetInfo(Info) then Exit(False);
  Result := Backend.ResizeWindow(Handle, Width, Height);
  if not Result then Fail('The window could not be resized.');
end;
function THIDWindow.Minimize: Boolean;
var Info: TWindowInfo;
begin
  if not GetInfo(Info) then Exit(False);
  Result := Backend.MinimizeWindow(Handle);
  if not Result then Fail('The window could not be minimized.');
end;
function THIDWindow.Restore: Boolean;
var Info: TWindowInfo;
begin
  if not GetInfo(Info) then Exit(False);
  Result := Backend.RestoreWindow(Handle);
  if not Result then Fail('The window could not be restored.');
end;
end.
