unit HIDXMouseInput;

{$mode objfpc}{$H+}
// Copyright (c) 2026 Tony Stone. 0BSD.

{ The mouse on X11, through the XTEST extension: absolute motion on the
  root window and core button numbers. }

interface

uses
  HIDMouseInputIntf;

function InitializeXMouseInput: TMouseInput;

implementation

uses
  Classes, SysUtils, Types, Controls, ctypes, X, XLib;

function XTestQueryExtension(Dpy: PDisplay; EventBase, ErrorBase, Major, Minor: Pcint): TBool; cdecl; external 'Xtst';
function XTestFakeButtonEvent(Dpy: PDisplay; Button: cuint; IsPress: TBool; Delay: culong): cint; cdecl; external 'Xtst';
function XTestFakeMotionEvent(Dpy: PDisplay; Screen, X, Y: cint; Delay: culong): cint; cdecl; external 'Xtst';

type
  TXMouseInput = class(TMouseInput)
  private
    Dpy: PDisplay;
    procedure SendButton(Number: cuint; IsPress: boolean);
  protected
    procedure DoDown(Button: TMouseButton); override;
    procedure DoUp(Button: TMouseButton); override;
    procedure DoMove(ScreenX, ScreenY: integer); override;
    procedure DoScrollUp; override;
    procedure DoScrollDown; override;
    function CurrentPos: TPoint; override;
  public
    constructor Create;
    destructor Destroy; override;
  end;

const
  // core button numbers: 4 and 5 are the wheel, 8 and 9 back and forward
  ButtonNumbers: array[TMouseButton] of cuint = (1, 3, 2, 8, 9);
  WheelUp = 4;
  WheelDown = 5;

function InitializeXMouseInput: TMouseInput;
begin
  Result := TXMouseInput.Create;
end;

constructor TXMouseInput.Create;
var
  Dummy: cint;
begin
  inherited Create;
  Dpy := XOpenDisplay(nil);
  if (Dpy <> nil) and (XTestQueryExtension(Dpy, @Dummy, @Dummy, @Dummy, @Dummy) = 0) then
  begin
    XCloseDisplay(Dpy);
    Dpy := nil;
  end;
end;

destructor TXMouseInput.Destroy;
begin
  if Dpy <> nil then XCloseDisplay(Dpy);
  inherited Destroy;
end;

procedure TXMouseInput.SendButton(Number: cuint; IsPress: boolean);
begin
  if Dpy = nil then Exit;
  XTestFakeButtonEvent(Dpy, Number, Ord(IsPress), 0);
  XSync(Dpy, False);
end;

procedure TXMouseInput.DoDown(Button: TMouseButton);
begin
  SendButton(ButtonNumbers[Button], True);
end;

procedure TXMouseInput.DoUp(Button: TMouseButton);
begin
  SendButton(ButtonNumbers[Button], False);
end;

procedure TXMouseInput.DoMove(ScreenX, ScreenY: integer);
begin
  if Dpy = nil then Exit;
  // screen -1 is the screen the pointer is on
  XTestFakeMotionEvent(Dpy, -1, ScreenX, ScreenY, 0);
  XSync(Dpy, False);
end;

procedure TXMouseInput.DoScrollUp;
begin
  SendButton(WheelUp, True);
  SendButton(WheelUp, False);
end;

procedure TXMouseInput.DoScrollDown;
begin
  SendButton(WheelDown, True);
  SendButton(WheelDown, False);
end;

// Asked of the server, which has seen our own last move already.
function TXMouseInput.CurrentPos: TPoint;
var
  Root, Child: TWindow;
  RootX, RootY, WinX, WinY: cint;
  Mask: cuint;
begin
  if (Dpy = nil) or not XQueryPointer(Dpy, XDefaultRootWindow(Dpy), @Root, @Child, @RootX, @RootY, @WinX, @WinY, @Mask) then Exit(inherited CurrentPos);
  Result := Point(RootX, RootY);
end;

end.
