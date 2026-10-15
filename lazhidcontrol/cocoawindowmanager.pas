// Copyright (c) 2026 Tony Stone. 0BSD.
unit CocoaWindowManager;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Types, Graphics, MacOSAll, WindowManagerIntf;

type
  { Other programs' windows on macOS.  A handle is the window's Core Graphics
    number.  Listing windows and reading where they are needs no permission.
    Their titles need Screen Recording or Accessibility, and anything that
    changes a window - activate, move, size, minimize, close - goes through
    the Accessibility API, which the user allows under Privacy & Security >
    Accessibility.  Without it those calls answer False.

    Only windows on the screen are listed, so a minimized window is found
    by a handle taken before it was minimized, not by its title. }
  TCocoaWindowManager = class(TWindowManager)
  private
    function OnScreen: TList;
    function Element(AHandle: TWindowHandle; out App: AXUIElementRef): AXUIElementRef;
    function SetValue(AHandle: TWindowHandle; const Attribute: string; Value: CFTypeRef; OfApp: boolean = False): boolean;
    procedure Settle(AHandle: TWindowHandle; X, Y, Width, Height: integer);
    function Press(AHandle: TWindowHandle; const Button: string): boolean;
  public
    function GetActiveWindow: TWindowHandle; override;
    function GetWindowAtPoint(X, Y: integer): TWindowHandle; override;
    function GetWindowInfo(AHandle: TWindowHandle; out AInfo: TWindowInfo): boolean; override;
    function GetWindowTitle(AHandle: TWindowHandle): string; override;
    function GetWindowClass(AHandle: TWindowHandle): string; override;
    function GetWindowPID(AHandle: TWindowHandle): longword; override;
    function GetWindowRect(AHandle: TWindowHandle): TRect; override;
    function EnumerateWindows: TList; override;
    function GetWindowChildren(AHandle: TWindowHandle): TList; override;
    function ActivateWindow(AHandle: TWindowHandle): boolean; override;
    function FocusWindow(AHandle: TWindowHandle): boolean; override;
    function RaiseWindow(AHandle: TWindowHandle): boolean; override;
    function MoveWindow(AHandle: TWindowHandle; X, Y: integer): boolean; override;
    function ResizeWindow(AHandle: TWindowHandle; Width, Height: integer): boolean; override;
    function MoveResizeWindow(AHandle: TWindowHandle; X, Y, Width, Height: integer): boolean; override;
    function MinimizeWindow(AHandle: TWindowHandle): boolean; override;
    function MaximizeWindow(AHandle: TWindowHandle): boolean; override;
    function RestoreWindow(AHandle: TWindowHandle): boolean; override;
    function CloseWindow(AHandle: TWindowHandle): boolean; override;
    function ShowWindow(AHandle: TWindowHandle): boolean; override;
    function HideWindow(AHandle: TWindowHandle): boolean; override;
    function IsWindowVisible(AHandle: TWindowHandle): boolean; override;
    function CaptureWindow(AHandle: TWindowHandle; ABitmap: TBitmap): boolean; override;
    // macOS keeps its Spaces to itself, so there is always the one desktop
    function GetDesktopCount: integer; override;
    function GetCurrentDesktop: integer; override;
    function SetCurrentDesktop(ADesktop: integer): boolean; override;
    function GetWindowDesktop(AHandle: TWindowHandle): integer; override;
    function SetWindowDesktop(AHandle: TWindowHandle; ADesktop: integer): boolean; override;
  end;

function InitializeWindowManager: TWindowManager;

implementation

uses
  CocoaCapture;

function InitializeWindowManager: TWindowManager;
begin
  Result := TCocoaWindowManager.Create;
end;

function Name(const Value: string): CFStringRef;
begin
  Result := CFStringCreateWithCString(nil, PChar(Value), kCFStringEncodingUTF8);
end;

function Text(Value: CFTypeRef): string;
var
  Buffer: array[0..1023] of char;
begin
  Result := '';
  if (Value <> nil) and (CFGetTypeID(Value) = CFStringGetTypeID) and CFStringGetCString(CFStringRef(Value), @Buffer[0], SizeOf(Buffer), kCFStringEncodingUTF8) then
    Result := Buffer;
end;

function Number(Entry: CFDictionaryRef; Key: CFStringRef): int64;
var
  Value: CFTypeRef;
begin
  Result := 0;
  Value := CFDictionaryGetValue(Entry, Key);
  if Value <> nil then CFNumberGetValue(CFNumberRef(Value), kCFNumberSInt64Type, @Result);
end;

function Bounds(Entry: CFDictionaryRef): TRect;
var
  Box: CGRect;
begin
  Result := Types.Rect(0, 0, 0, 0);
  if CGRectMakeWithDictionaryRepresentation(CFDictionaryRef(CFDictionaryGetValue(Entry, kCGWindowBounds)), Box) <> 0 then
    Result := Types.Rect(Round(Box.origin.x), Round(Box.origin.y), Round(Box.origin.x + Box.size.Width), Round(Box.origin.y + Box.size.Height));
end;

{ The window list's entry for one window, on the screen or not, or nil; the
  caller releases List. }
function Entry(AHandle: TWindowHandle; out List: CFArrayRef): CFDictionaryRef;
var
  ID: Pointer;
  Wanted: CFArrayRef;
begin
  Result := nil;
  ID := Pointer(AHandle);
  Wanted := CFArrayCreate(nil, @ID, 1, nil);
  List := CGWindowListCreateDescriptionFromArray(Wanted);
  CFRelease(Wanted);
  if (List <> nil) and (CFArrayGetCount(List) > 0) then Result := CFDictionaryRef(CFArrayGetValueAtIndex(List, 0));
end;

{ An attribute of something in the Accessibility tree; the caller releases it. }
function Attribute(Item: AXUIElementRef; const Which: string): CFTypeRef;
var
  Key: CFStringRef;
begin
  Result := nil;
  Key := Name(Which);
  if AXUIElementCopyAttributeValue(Item, Key, Result) <> kAXErrorSuccess then Result := nil;
  CFRelease(Key);
end;

{ TCocoaWindowManager }

{ Ordinary windows on the screen, frontmost first. }
function TCocoaWindowManager.OnScreen: TList;
var
  List: CFArrayRef;
  Item: CFDictionaryRef;
  I: integer;
begin
  Result := TList.Create;
  List := CGWindowListCopyWindowInfo(kCGWindowListOptionOnScreenOnly or kCGWindowListExcludeDesktopElements, kCGNullWindowID);
  if List = nil then Exit;
  for I := 0 to CFArrayGetCount(List) - 1 do
  begin
    Item := CFDictionaryRef(CFArrayGetValueAtIndex(List, I));
    // layer 0 is where applications' windows live; the menu bar, the Dock and the like are above it
    if Number(Item, kCGWindowLayer) = 0 then Result.Add(Pointer(PtrUInt(Number(Item, kCGWindowNumber))));
  end;
  CFRelease(List);
end;

{ The same window in its application's Accessibility tree, matched by where
  it is and how big, since that tree does not carry window numbers.  The
  caller releases both. }
function TCocoaWindowManager.Element(AHandle: TWindowHandle; out App: AXUIElementRef): AXUIElementRef;
var
  Info: TWindowInfo;
  Windows: CFTypeRef;
  Candidate: AXUIElementRef;
  Value: CFTypeRef;
  Where: CGPoint;
  Size: CGSize;
  I: integer;
begin
  Result := nil;
  App := nil;
  if not GetWindowInfo(AHandle, Info) then Exit;
  App := AXUIElementCreateApplication(Info.ProcessID);
  Windows := Attribute(App, 'AXWindows');
  if Windows = nil then Exit;
  for I := 0 to CFArrayGetCount(CFArrayRef(Windows)) - 1 do
  begin
    Candidate := AXUIElementRef(CFArrayGetValueAtIndex(CFArrayRef(Windows), I));
    Where := CGPointMake(-1, -1);
    Size := CGSizeMake(-1, -1);
    Value := Attribute(Candidate, 'AXPosition');
    if Value <> nil then
    begin
      AXValueGetValue(AXValueRef(Value), kAXValueCGPointType, @Where);
      CFRelease(Value);
    end;
    Value := Attribute(Candidate, 'AXSize');
    if Value <> nil then
    begin
      AXValueGetValue(AXValueRef(Value), kAXValueCGSizeType, @Size);
      CFRelease(Value);
    end;
    if (Abs(Where.x - Info.Rect.Left) < 2) and (Abs(Where.y - Info.Rect.Top) < 2) and (Abs(Size.Width - Info.Rect.Width) < 2) and
      (Abs(Size.Height - Info.Rect.Height) < 2) then
    begin
      Result := AXUIElementRef(CFRetain(Candidate));
      Break;
    end;
  end;
  CFRelease(Windows);
end;

function TCocoaWindowManager.SetValue(AHandle: TWindowHandle; const Attribute: string; Value: CFTypeRef; OfApp: boolean): boolean;
var
  App, Window, Target: AXUIElementRef;
  Key: CFStringRef;
begin
  Result := False;
  Window := Element(AHandle, App);
  if OfApp then Target := App
  else
    Target := Window;
  if Target <> nil then
  begin
    Key := Name(Attribute);
    Result := AXUIElementSetAttributeValue(Target, Key, Value) = kAXErrorSuccess;
    CFRelease(Key);
  end;
  if Window <> nil then CFRelease(Window);
  if App <> nil then CFRelease(App);
end;

{ Presses one of the buttons in the window's title bar. }
function TCocoaWindowManager.Press(AHandle: TWindowHandle; const Button: string): boolean;
var
  App, Window: AXUIElementRef;
  Target: CFTypeRef;
  Action: CFStringRef;
begin
  Result := False;
  Window := Element(AHandle, App);
  if Window <> nil then
  begin
    Target := Attribute(Window, Button);
    if Target <> nil then
    begin
      Action := Name('AXPress');
      Result := AXUIElementPerformAction(AXUIElementRef(Target), Action) = kAXErrorSuccess;
      CFRelease(Action);
      CFRelease(Target);
    end;
    CFRelease(Window);
  end;
  if App <> nil then CFRelease(App);
end;

function TCocoaWindowManager.GetActiveWindow: TWindowHandle;
var
  Windows: TList;
begin
  Result := 0;
  Windows := OnScreen;
  if Windows.Count > 0 then Result := TWindowHandle(Windows[0]);
  Windows.Free;
end;

function TCocoaWindowManager.GetWindowAtPoint(X, Y: integer): TWindowHandle;
var
  Windows: TList;
  I: integer;
begin
  Result := 0;
  Windows := OnScreen;
  for I := 0 to Windows.Count - 1 do
    if GetWindowRect(TWindowHandle(Windows[I])).Contains(Types.Point(X, Y)) then
    begin
      Result := TWindowHandle(Windows[I]);
      Break;
    end;
  Windows.Free;
end;

function TCocoaWindowManager.GetWindowInfo(AHandle: TWindowHandle; out AInfo: TWindowInfo): boolean;
var
  List: CFArrayRef;
  Item: CFDictionaryRef;
  App, Window: AXUIElementRef;
  Title: CFTypeRef;
begin
  AInfo := Default(TWindowInfo);
  Item := Entry(AHandle, List);
  Result := Item <> nil;
  if Result then
  begin
    AInfo.Handle := AHandle;
    AInfo.Title := Text(CFDictionaryGetValue(Item, kCGWindowName));
    // macOS has no window classes; the application's name is the nearest thing
    AInfo.ClassName := Text(CFDictionaryGetValue(Item, kCGWindowOwnerName));
    AInfo.ProcessName := AInfo.ClassName;
    AInfo.ProcessID := Number(Item, kCGWindowOwnerPID);
    AInfo.Rect := Bounds(Item);
    AInfo.IsVisible := CFDictionaryGetValue(Item, kCGWindowIsOnscreen) = CFTypeRef(kCFBooleanTrue);
    AInfo.IsMinimized := not AInfo.IsVisible;
  end;
  if List <> nil then CFRelease(List);
  // the window list holds titles back without Screen Recording; Accessibility may still have it
  if Result and (AInfo.Title = '') and AXIsProcessTrusted then
  begin
    Window := Element(AHandle, App);
    if Window <> nil then
    begin
      Title := Attribute(Window, 'AXTitle');
      AInfo.Title := Text(Title);
      if Title <> nil then CFRelease(Title);
      CFRelease(Window);
    end;
    if App <> nil then CFRelease(App);
  end;
end;

function TCocoaWindowManager.GetWindowTitle(AHandle: TWindowHandle): string;
var
  Info: TWindowInfo;
begin
  GetWindowInfo(AHandle, Info);
  Result := Info.Title;
end;

function TCocoaWindowManager.GetWindowClass(AHandle: TWindowHandle): string;
var
  Info: TWindowInfo;
begin
  GetWindowInfo(AHandle, Info);
  Result := Info.ClassName;
end;

function TCocoaWindowManager.GetWindowPID(AHandle: TWindowHandle): longword;
var
  Info: TWindowInfo;
begin
  GetWindowInfo(AHandle, Info);
  Result := Info.ProcessID;
end;

function TCocoaWindowManager.GetWindowRect(AHandle: TWindowHandle): TRect;
var
  List: CFArrayRef;
  Item: CFDictionaryRef;
begin
  Result := Types.Rect(0, 0, 0, 0);
  Item := Entry(AHandle, List);
  if Item <> nil then Result := Bounds(Item);
  if List <> nil then CFRelease(List);
end;

function TCocoaWindowManager.EnumerateWindows: TList;
begin
  Result := OnScreen;
end;

function TCocoaWindowManager.GetWindowChildren(AHandle: TWindowHandle): TList;
begin
  Result := TList.Create;
end;

function TCocoaWindowManager.ActivateWindow(AHandle: TWindowHandle): boolean;
begin
  // a minimized window comes back up, as it does on the other platforms
  SetValue(AHandle, 'AXMinimized', kCFBooleanFalse);
  Result := SetValue(AHandle, 'AXFrontmost', kCFBooleanTrue, True) and RaiseWindow(AHandle);
end;

function TCocoaWindowManager.FocusWindow(AHandle: TWindowHandle): boolean;
begin
  Result := ActivateWindow(AHandle);
end;

function TCocoaWindowManager.RaiseWindow(AHandle: TWindowHandle): boolean;
var
  App, Window: AXUIElementRef;
  Action: CFStringRef;
begin
  Result := False;
  Window := Element(AHandle, App);
  if Window <> nil then
  begin
    Action := Name('AXRaise');
    Result := AXUIElementPerformAction(Window, Action) = kAXErrorSuccess;
    CFRelease(Action);
    CFRelease(Window);
  end;
  if App <> nil then CFRelease(App);
end;

{ The window list hears of a move a moment after the Accessibility call
  returns, and the next call finds the window by where that list says it is. }
procedure TCocoaWindowManager.Settle(AHandle: TWindowHandle; X, Y, Width, Height: integer);
var
  Deadline: QWord;
  Now: TRect;
begin
  Deadline := GetTickCount64 + 500;
  repeat
    Now := GetWindowRect(AHandle);
    if ((X = MaxInt) or ((Now.Left = X) and (Now.Top = Y))) and ((Width = MaxInt) or ((Now.Width = Width) and (Now.Height = Height))) then Exit;
    Sleep(10);
  until GetTickCount64 > Deadline;
end;

function TCocoaWindowManager.MoveWindow(AHandle: TWindowHandle; X, Y: integer): boolean;
var
  Where: CGPoint;
  Value: AXValueRef;
begin
  Where := CGPointMake(X, Y);
  Value := AXValueCreate(kAXValueCGPointType, @Where);
  Result := SetValue(AHandle, 'AXPosition', Value);
  CFRelease(Value);
  if Result then Settle(AHandle, X, Y, MaxInt, MaxInt);
end;

function TCocoaWindowManager.ResizeWindow(AHandle: TWindowHandle; Width, Height: integer): boolean;
var
  Size: CGSize;
  Value: AXValueRef;
begin
  Size := CGSizeMake(Width, Height);
  Value := AXValueCreate(kAXValueCGSizeType, @Size);
  Result := SetValue(AHandle, 'AXSize', Value);
  CFRelease(Value);
  if Result then Settle(AHandle, MaxInt, MaxInt, Width, Height);
end;

function TCocoaWindowManager.MoveResizeWindow(AHandle: TWindowHandle; X, Y, Width, Height: integer): boolean;
begin
  // size first: the window is found by where it is, and moving it changes that
  Result := ResizeWindow(AHandle, Width, Height) and MoveWindow(AHandle, X, Y);
end;

function TCocoaWindowManager.MinimizeWindow(AHandle: TWindowHandle): boolean;
begin
  Result := SetValue(AHandle, 'AXMinimized', kCFBooleanTrue);
end;

function TCocoaWindowManager.MaximizeWindow(AHandle: TWindowHandle): boolean;
begin
  Result := Press(AHandle, 'AXZoomButton');
end;

function TCocoaWindowManager.RestoreWindow(AHandle: TWindowHandle): boolean;
begin
  Result := SetValue(AHandle, 'AXMinimized', kCFBooleanFalse);
end;

function TCocoaWindowManager.CloseWindow(AHandle: TWindowHandle): boolean;
begin
  Result := Press(AHandle, 'AXCloseButton');
end;

// on a Mac it is the application that hides, with all its windows

function TCocoaWindowManager.ShowWindow(AHandle: TWindowHandle): boolean;
begin
  Result := SetValue(AHandle, 'AXHidden', kCFBooleanFalse, True);
end;

function TCocoaWindowManager.HideWindow(AHandle: TWindowHandle): boolean;
begin
  Result := SetValue(AHandle, 'AXHidden', kCFBooleanTrue, True);
end;

function TCocoaWindowManager.IsWindowVisible(AHandle: TWindowHandle): boolean;
var
  Info: TWindowInfo;
begin
  Result := GetWindowInfo(AHandle, Info) and Info.IsVisible;
end;

function TCocoaWindowManager.CaptureWindow(AHandle: TWindowHandle; ABitmap: TBitmap): boolean;
var
  Box: CGRect;
begin
  if AHandle <> 0 then Exit(CaptureWindowImage(CGWindowID(AHandle), ABitmap));
  Box := CGDisplayBounds(CGMainDisplayID);
  Result := CaptureScreenArea(Types.Rect(0, 0, Round(Box.size.Width), Round(Box.size.Height)), ABitmap);
end;

function TCocoaWindowManager.GetDesktopCount: integer;
begin
  Result := 1;
end;

function TCocoaWindowManager.GetCurrentDesktop: integer;
begin
  Result := 0;
end;

function TCocoaWindowManager.SetCurrentDesktop(ADesktop: integer): boolean;
begin
  Result := ADesktop = 0;
end;

function TCocoaWindowManager.GetWindowDesktop(AHandle: TWindowHandle): integer;
begin
  Result := 0;
end;

function TCocoaWindowManager.SetWindowDesktop(AHandle: TWindowHandle; ADesktop: integer): boolean;
begin
  Result := ADesktop = 0;
end;

end.
