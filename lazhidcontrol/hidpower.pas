unit HIDPower;
{$mode objfpc}{$H+}
// Copyright (c) 2026 Tony Stone. GPL-2.0-or-later; see COPYING.GPL2.
interface

uses Classes, SysUtils, ExtCtrls, PowerInput;

type
  THIDPowerErrorEvent = procedure(Sender: TObject; const Message: string) of object;

  THIDPowerMonitor = class(TComponent)
  private
    PollTimer: TTimer;
    Input: TPowerInput;
    Requested, KnownStatus: boolean;
    Snapshot: TPowerStatus;
    ErrorText: string;
    SuspendHandler, ResumeHandler, ChangeHandler: TNotifyEvent;
    ErrorHandler: THIDPowerErrorEvent;
    procedure SetEnabled(Value: boolean);
    procedure Tick(Sender: TObject);
    function Fail(const Message: string): boolean;
    function GetInterval: cardinal;
    procedure SetInterval(Value: cardinal);
    function GetSource: TPowerSource;
    function GetPercent: integer;
    function GetCharging: boolean;
  protected
    procedure Loaded; override;
    function CreateInput: TPowerInput; virtual;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function Refresh: boolean;
    // Manual event processing for console applications; GUI applications use the timer.
    function Check: boolean;
    property PowerSource: TPowerSource read GetSource;
    property BatteryPercent: integer read GetPercent;
    property Charging: boolean read GetCharging;
    property LastError: string read ErrorText;
  published
    // True subscribes to power events once the form loads. Nothing runs in
    // the designer.
    property Enabled: boolean read Requested write SetEnabled default False;
    // Milliseconds between checks of the event queue and power status. Default 250.
    property PollInterval: cardinal read GetInterval write SetInterval default 250;
    // The system is going to sleep. Informational only: it cannot delay or
    // veto the sleep, so keep the handler short.
    property OnSuspend: TNotifyEvent read SuspendHandler write SuspendHandler;
    // The system woke from sleep or hibernation. Power status is refreshed
    // before this fires.
    property OnResume: TNotifyEvent read ResumeHandler write ResumeHandler;
    // The power source, battery percentage, or charging state changed; read
    // PowerSource, BatteryPercent, and Charging. The status found at enabling
    // is the baseline and fires nothing.
    property OnPowerChanged: TNotifyEvent read ChangeHandler write ChangeHandler;
    // The subscription or a status query failed, which also disables
    // monitoring; the message stays in LastError.
    property OnError: THIDPowerErrorEvent read ErrorHandler write ErrorHandler;
  end;

implementation

constructor THIDPowerMonitor.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  Snapshot.BatteryPercent := -1;
  PollTimer := TTimer.Create(Self);
  PollTimer.Enabled := False;
  PollTimer.Interval := 250;
  PollTimer.OnTimer := @Tick;
end;

destructor THIDPowerMonitor.Destroy;
begin
  SetEnabled(False);
  inherited Destroy;
end;

function THIDPowerMonitor.CreateInput: TPowerInput;
begin
  Result := CreatePowerInput;
end;

procedure THIDPowerMonitor.Loaded;
begin
  inherited Loaded;
  if Enabled then SetEnabled(True);
end;

procedure THIDPowerMonitor.SetEnabled(Value: boolean);
var
  Message: string;
begin
  Requested := Value;
  if (csLoading in ComponentState) or (csDesigning in ComponentState) then Exit;
  PollTimer.Enabled := False;
  FreeAndNil(Input);
  KnownStatus := False;
  Snapshot := Default(TPowerStatus);
  Snapshot.BatteryPercent := -1;
  if not Value then Exit;
  ErrorText := '';
  Input := CreateInput;
  if Input = nil then
  begin
    Fail('Power monitoring is not implemented on this platform.');
    Exit;
  end;
  if not Input.Start then
  begin
    Message := Input.ErrorText;
    Fail(Message);
    Exit;
  end;
  if Refresh then PollTimer.Enabled := Enabled;
end;

function THIDPowerMonitor.Fail(const Message: string): boolean;
begin
  SetEnabled(False);
  ErrorText := Message;
  Result := False;
  if Assigned(ErrorHandler) then ErrorHandler(Self, Message);
end;

function THIDPowerMonitor.GetInterval: cardinal;
begin
  Result := PollTimer.Interval;
end;

procedure THIDPowerMonitor.SetInterval(Value: cardinal);
begin
  if Value = 0 then raise ERangeError.Create('PollInterval must be greater than zero.');
  PollTimer.Interval := Value;
end;

function THIDPowerMonitor.GetSource: TPowerSource;
begin
  Result := Snapshot.Source;
end;

function THIDPowerMonitor.GetPercent: integer;
begin
  Result := Snapshot.BatteryPercent;
end;

function THIDPowerMonitor.GetCharging: boolean;
begin
  Result := Snapshot.Charging;
end;

function THIDPowerMonitor.Refresh: boolean;
var
  Status: TPowerStatus;
  Changed: boolean;
  Message: string;
begin
  if (csDesigning in ComponentState) or (Input = nil) then Exit(False);
  if not Input.ReadStatus(Status) then
  begin
    Message := Input.ErrorText;
    Exit(Fail(Message));
  end;
  Changed := KnownStatus and ((Status.Source <> PowerSource) or (Status.BatteryPercent <> BatteryPercent) or (Status.Charging <> Charging));
  Snapshot := Status;
  KnownStatus := True;
  ErrorText := '';
  Result := True;
  if Changed and Assigned(ChangeHandler) then ChangeHandler(Self);
end;

function THIDPowerMonitor.Check: boolean;
var
  Events: TPowerEvents;
  Event: TPowerEvent;
  Message: string;
begin
  if (csDesigning in ComponentState) or (Input = nil) then Exit(False);
  if not Input.Poll(Events) then
  begin
    Message := Input.ErrorText;
    Exit(Fail(Message));
  end;
  Result := True;
  for Event in Events do
  begin
    if not Enabled then Break;
    case Event of
      peSuspend: if Assigned(SuspendHandler) then SuspendHandler(Self);
      peResume: begin
        if not Refresh then Exit(False);
        if Enabled and Assigned(ResumeHandler) then ResumeHandler(Self);
      end;
      peChanged: if not Refresh then Exit(False);
    end;
  end;
end;

procedure THIDPowerMonitor.Tick(Sender: TObject);
begin
  Check;
end;

end.
