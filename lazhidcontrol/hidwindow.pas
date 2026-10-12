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
    PIDFilter: longword;
    Exact, Visible: boolean;
    TimeoutValue: cardinal;
    Target: TWindowHandle;
    Poll: TTimer;
    Deadline: QWord;
    FoundHandler, TimeoutHandler: TNotifyEvent;
    ErrorHandler: THIDWindowErrorEvent;
    function Available: boolean;
    function Fail(const Message: string): boolean;
    function Matches(const Info: TWindowInfo): boolean;
    procedure Tick(Sender: TObject);
    function GetWaiting: boolean;
  protected
    function Backend: TWindowManager; virtual;
  public
    constructor Create(AOwner: TComponent); override;
    function Find: boolean;
    // Caller owns the returned list. Each item is a window handle, not an object.
    function FindAll: TList;
    function WaitForWindow: boolean;
    procedure CancelWait;
    function Activate: boolean;
    function MoveTo(X, Y: integer): boolean;
    function Resize(Width, Height: integer): boolean;
    function Minimize: boolean;
    function Restore: boolean;
    function GetInfo(out Info: TWindowInfo): boolean;
    property Handle: TWindowHandle read Target;
    property Waiting: boolean read GetWaiting;
    property LastError: string read ErrorText;
  published
    // Title to look for; blank accepts any title. The filters that are set
    // must all match the same window.
    property WindowTitle: string read TitleFilter write TitleFilter;
    // Native window class to look for; blank accepts any class.
    property WindowClass: string read ClassFilter write ClassFilter;
    // Only accept windows belonging to this process. Zero accepts any process.
    property ProcessID: longword read PIDFilter write PIDFilter default 0;
    // True matches the whole title or class; False matches any part of it.
    // Case insensitive either way.
    property ExactMatch: boolean read Exact write Exact default False;
    // True skips hidden and minimized windows when searching.
    property VisibleOnly: boolean read Visible write Visible default True;
    // How many milliseconds WaitForWindow keeps looking before giving up.
    // Zero makes it a single immediate search.
    property WaitTimeout: cardinal read TimeoutValue write TimeoutValue default 5000;
    // Runs when Find or WaitForWindow matches a window; Handle then identifies it.
    property OnFound: TNotifyEvent read FoundHandler write FoundHandler;
    // Runs when WaitForWindow gives up. CancelWait never fires this.
    property OnTimeout: TNotifyEvent read TimeoutHandler write TimeoutHandler;
    // An unsupported platform or a failed window operation; the message stays
    // in LastError. No dialog is shown.
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
begin
  EnsureWindowManager;
  Result := WindowMgr;
end;

function THIDWindow.Fail(const Message: string): boolean;
begin
  ErrorText := Message;
  Result := False;
  if Assigned(ErrorHandler) then ErrorHandler(Self, Message);
end;

function THIDWindow.Available: boolean;
begin
  ErrorText := '';
  Result := False;
  if csDesigning in ComponentState then Exit(Fail('Window control is disabled in the designer.'));
  if Backend = nil then Exit(Fail('Window control is not implemented on this platform or session.'));
  Result := True;
end;

function THIDWindow.Matches(const Info: TWindowInfo): boolean;

  function TextMatches(const Filter, Value: string): boolean;
  begin
    if Filter = '' then Exit(True);
    if ExactMatch then Result := UTF8LowerCase(Filter) = UTF8LowerCase(Value)
    else
      Result := Pos(UTF8LowerCase(Filter), UTF8LowerCase(Value)) > 0;
  end;

begin
  Result := (not VisibleOnly or Info.IsVisible) and ((ProcessID = 0) or (ProcessID = Info.ProcessID)) and TextMatches(WindowTitle, Info.Title) and
    TextMatches(WindowClass, Info.ClassName);
end;

function THIDWindow.FindAll: TList;
var
  Windows: TList;
  Item: Pointer;
  Info: TWindowInfo;
begin
  Result := TList.Create;
  if not Available then Exit;
  Windows := Backend.EnumerateWindows;
  try
    for Item in Windows do
      if Backend.GetWindowInfo(TWindowHandle(Item), Info) and Matches(Info) then Result.Add(Item);
  finally
    Windows.Free;
  end;
end;

function THIDWindow.Find: boolean;
var
  Windows: TList;
begin
  Target := 0;
  Windows := FindAll;
  try
    if Windows.Count > 0 then Target := TWindowHandle(Windows[0]);
  finally
    Windows.Free;
  end;
  Result := Target <> 0;
  if Result then
  begin
    CancelWait;
    if Assigned(FoundHandler) then FoundHandler(Self);
  end;
end;

function THIDWindow.GetWaiting: boolean;
begin
  Result := Poll.Enabled;
end;

procedure THIDWindow.CancelWait;
begin
  Poll.Enabled := False;
end;

function THIDWindow.WaitForWindow: boolean;
begin
  CancelWait;
  if Find then Exit(True);
  if LastError <> '' then Exit(False);
  if WaitTimeout = 0 then
  begin
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
  if LastError <> '' then
  begin
    CancelWait;
    Exit;
  end;
  if GetTickCount64 >= Deadline then
  begin
    CancelWait;
    if Assigned(TimeoutHandler) then TimeoutHandler(Self);
  end;
end;

function THIDWindow.GetInfo(out Info: TWindowInfo): boolean;
begin
  Info := Default(TWindowInfo);
  if not Available then Exit(False);
  if (Handle = 0) or not Backend.GetWindowInfo(Handle, Info) then Exit(Fail('Find a valid window first.'));
  Result := True;
end;

function THIDWindow.Activate: boolean;
var
  Info: TWindowInfo;
begin
  if not GetInfo(Info) then Exit(False);
  Result := Backend.ActivateWindow(Handle);
  if not Result then Fail('The window could not be activated.');
end;

function THIDWindow.MoveTo(X, Y: integer): boolean;
var
  Info: TWindowInfo;
begin
  if not GetInfo(Info) then Exit(False);
  Result := Backend.MoveWindow(Handle, X, Y);
  if not Result then Fail('The window could not be moved.');
end;

function THIDWindow.Resize(Width, Height: integer): boolean;
var
  Info: TWindowInfo;
begin
  if (Width <= 0) or (Height <= 0) then Exit(Fail('Window dimensions must be positive.'));
  if not GetInfo(Info) then Exit(False);
  Result := Backend.ResizeWindow(Handle, Width, Height);
  if not Result then Fail('The window could not be resized.');
end;

function THIDWindow.Minimize: boolean;
var
  Info: TWindowInfo;
begin
  if not GetInfo(Info) then Exit(False);
  Result := Backend.MinimizeWindow(Handle);
  if not Result then Fail('The window could not be minimized.');
end;

function THIDWindow.Restore: boolean;
var
  Info: TWindowInfo;
begin
  if not GetInfo(Info) then Exit(False);
  Result := Backend.RestoreWindow(Handle);
  if not Result then Fail('The window could not be restored.');
end;

end.
