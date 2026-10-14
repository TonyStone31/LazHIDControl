unit EvdevMonitor;

{$mode objfpc}{$H+}
// Copyright (c) 2026 Tony Stone. GPL-2.0-or-later; see COPYING.GPL2.

{ Keys and mouse buttons straight from the kernel's input devices, for a
  Wayland session, where the compositor shows a program nothing but its own.
  Reading /dev/input/event* needs permission; the manual has the udev rule.

  The kernel knows keys, not layouts, so letters are reported as a US
  keyboard has them.  And it does not know where the pointer is - the
  compositor keeps that to itself - so a click arrives without a position. }

interface

uses GlobalKeyMonitorIntf, GlobalMouseMonitorIntf;

function InitializeEvdevKeyListener: TGlobalKeyListener;
function InitializeEvdevMouseListener: TGlobalMouseListener;

const
  EvdevHelp = 'Watching the keyboard and mouse on Wayland needs read access to /dev/input; the manual has the udev rule.';

implementation

uses Classes, SysUtils, Controls, LCLType, BaseUnix, LinuxKeyCodes;

const
  EV_KEY = 1;
  BTN_LEFT = $110;
  BTN_RIGHT = $111;
  BTN_MIDDLE = $112;
  BTN_SIDE = $113;
  BTN_EXTRA = $114;

type
  // struct input_event
  TInputEvent = record
    Time: TTimeVal;
    Kind, Code: word;
    Value: longint;
  end;

  TKeyHandler = procedure(Code: word; Down: boolean) of object;

  { One thread reads every device it may open and reports each key or
    button going down or up.  New devices are looked for every two seconds,
    so a keyboard plugged in later, or access granted later, is picked up. }
  TReader = class(TThread)
  private
    Devices: array of cint;
    Names: TStringList;
    Handler: TKeyHandler;
    Code: word;
    Down: boolean;
    procedure OpenNew;
    procedure Drop(Index: integer);
    procedure Deliver;
  protected
    procedure Execute; override;
  public
    constructor Create(AHandler: TKeyHandler);
    destructor Destroy; override;
    function Opened: boolean;
  end;

  TEvdevKeyListener = class(TGlobalKeyListener)
  private
    Reader: TReader;
    Held: TShiftState;
    procedure Seen(Code: word; Down: boolean);
  protected
    procedure DoStartMonitoring; override;
    procedure DoStopMonitoring; override;
  end;

  TEvdevMouseListener = class(TGlobalMouseListener)
  private
    Reader: TReader;
    procedure Seen(Code: word; Down: boolean);
  protected
    procedure DoStartMonitoring; override;
    procedure DoStopMonitoring; override;
  end;

  { TReader }

constructor TReader.Create(AHandler: TKeyHandler);
begin
  inherited Create(True);
  Handler := AHandler;
  Names := TStringList.Create;
  // the first look is before the thread runs, so the caller knows at once
  OpenNew;
  Start;
end;

destructor TReader.Destroy;
begin
  while Length(Devices) > 0 do Drop(High(Devices));
  Names.Free;
  inherited Destroy;
end;

function TReader.Opened: boolean;
begin
  Result := Length(Devices) > 0;
end;

procedure TReader.OpenNew;
var
  Found: TSearchRec;
  FD: cint;
begin
  if FindFirst('/dev/input/event*', faAnyFile, Found) = 0 then
  begin
    repeat
      if Names.IndexOf(Found.Name) < 0 then
      begin
        FD := fpOpen('/dev/input/' + Found.Name, O_RDONLY or O_NONBLOCK);
        if FD >= 0 then
        begin
          SetLength(Devices, Length(Devices) + 1);
          Devices[High(Devices)] := FD;
          Names.Add(Found.Name);
        end;
      end;
    until FindNext(Found) <> 0;
    FindClose(Found);
  end;
end;

// Unplugged, or no longer readable.
procedure TReader.Drop(Index: integer);
begin
  fpClose(Devices[Index]);
  Delete(Devices, Index, 1);
  Names.Delete(Index);
end;

procedure TReader.Deliver;
begin
  Handler(Code, Down);
end;

procedure TReader.Execute;
var
  Polls: array of TPollFd;
  Event: TInputEvent;
  LastLook: QWord;
  I: integer;
begin
  LastLook := GetTickCount64;
  while not Terminated do
  begin
    if GetTickCount64 - LastLook >= 2000 then
    begin
      OpenNew;
      LastLook := GetTickCount64;
    end;
    if Length(Devices) = 0 then
    begin
      Sleep(200);
      Continue;
    end;
    SetLength(Polls, Length(Devices));
    for I := 0 to High(Devices) do
    begin
      Polls[I].fd := Devices[I];
      Polls[I].events := POLLIN;
      Polls[I].revents := 0;
    end;
    if fpPoll(@Polls[0], Length(Polls), 200) <= 0 then Continue;
    for I := High(Devices) downto 0 do
      if Polls[I].revents and (POLLERR or POLLHUP or POLLNVAL) <> 0 then Drop(I)
      else if Polls[I].revents and POLLIN <> 0 then
        while fpRead(Devices[I], Event, SizeOf(Event)) = SizeOf(Event) do
          // value 2 is autorepeat, which is not a press
          if (Event.Kind = EV_KEY) and (Event.Value in [0, 1]) then
          begin
            Code := Event.Code;
            Down := Event.Value = 1;
            Synchronize(@Deliver);
          end;
  end;
end;

{ TEvdevKeyListener }

procedure TEvdevKeyListener.DoStartMonitoring;
begin
  Reader := TReader.Create(@Seen);
  IsActive := Reader.Opened;
  if not IsActive then FreeAndNil(Reader);
end;

procedure TEvdevKeyListener.DoStopMonitoring;
begin
  if Reader <> nil then
  begin
    Reader.Terminate;
    Reader.WaitFor;
    FreeAndNil(Reader);
  end;
  IsActive := False;
end;

procedure TEvdevKeyListener.Seen(Code: word; Down: boolean);
var
  Key: word;
  Modifier: TShiftStateEnum;
begin
  Key := KeyFromKernel(Code);
  case Key of
    VK_RSHIFT: Key := VK_SHIFT;
    VK_RCONTROL: Key := VK_CONTROL;
    VK_RMENU: Key := VK_MENU;
    VK_RWIN: Key := VK_LWIN;
  end;
  // the modifiers are kept from the same stream, there being nobody to ask
  case Key of
    VK_SHIFT: Modifier := ssShift;
    VK_CONTROL: Modifier := ssCtrl;
    VK_MENU: Modifier := ssAlt;
    VK_LWIN: Modifier := ssMeta;
    else
      Modifier := ssDouble;
  end;
  if Modifier <> ssDouble then
    if Down then Include(Held, Modifier)
    else
      Exclude(Held, Modifier);
  if Down and (Key <> 0) and Assigned(OnKeyPress) then OnKeyPress(Key, Held);
end;

{ TEvdevMouseListener }

procedure TEvdevMouseListener.DoStartMonitoring;
begin
  Reader := TReader.Create(@Seen);
  IsActive := Reader.Opened;
  if not IsActive then FreeAndNil(Reader);
end;

procedure TEvdevMouseListener.DoStopMonitoring;
begin
  if Reader <> nil then
  begin
    Reader.Terminate;
    Reader.WaitFor;
    FreeAndNil(Reader);
  end;
  IsActive := False;
end;

procedure TEvdevMouseListener.Seen(Code: word; Down: boolean);
var
  Button: TMouseButton;
begin
  if not Down then Exit;
  case Code of
    BTN_LEFT: Button := mbLeft;
    BTN_RIGHT: Button := mbRight;
    BTN_MIDDLE: Button := mbMiddle;
    BTN_SIDE: Button := mbExtra1;
    BTN_EXTRA: Button := mbExtra2;
    else
      Exit;
  end;
  if Assigned(OnMouseClick) then OnMouseClick(Button, -1, -1);
end;

function InitializeEvdevKeyListener: TGlobalKeyListener;
begin
  Result := TEvdevKeyListener.Create;
end;

function InitializeEvdevMouseListener: TGlobalMouseListener;
begin
  Result := TEvdevMouseListener.Create;
end;

end.
