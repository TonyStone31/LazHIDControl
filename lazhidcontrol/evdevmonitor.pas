unit EvdevMonitor;

{$mode objfpc}{$H+}
// Copyright (c) 2026 Tony Stone. 0BSD.

{ Keys straight from the kernel's input devices, for a Wayland session,
  where the compositor shows a program nothing but its own.  Reading
  /dev/input/event* needs permission; the manual has the udev rule.

  The kernel knows keys, not layouts, so letters are reported as a US
  keyboard has them.  Clicks are not watched here: the kernel does not know
  where the pointer is, and a click without a position is no use. }

interface

uses GlobalKeyMonitorIntf;

function InitializeEvdevKeyListener: TGlobalKeyListener;

const
  EvdevHelp = 'Watching the keyboard on Wayland needs read access to /dev/input: THIDPermission gets it, or the manual has the udev rule.';

implementation

uses Classes, SysUtils, LCLType, BaseUnix, LinuxKeyCodes;

const
  EV_KEY = 1;

type
  // struct input_event
  TInputEvent = record
    Time: TTimeVal;
    Kind, Code: word;
    Value: longint;
  end;

  TKeyHandler = procedure(Code: word; Down: boolean) of object;

  { One thread reads every device it may open and reports each key going
    down or up.  New devices are looked for every two seconds,
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

function InitializeEvdevKeyListener: TGlobalKeyListener;
begin
  Result := TEvdevKeyListener.Create;
end;

end.
