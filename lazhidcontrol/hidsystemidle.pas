unit HIDSystemIdle;
{$mode objfpc}{$H+}
// Copyright (c) 2026 Tony Stone. GPL-2.0-or-later; see COPYING.GPL2.
interface

uses Classes, SysUtils, ExtCtrls;

type
  THIDSystemIdleErrorEvent = procedure(Sender: TObject; const Message: string) of object;

  THIDSystemIdle = class(TComponent)
  private
    Poll: TTimer;
    Requested, IdleState, KnownState: boolean;
    Threshold: cardinal;
    Elapsed: QWord;
    ErrorText: string;
    IdleHandler, ActiveHandler: TNotifyEvent;
    ErrorHandler: THIDSystemIdleErrorEvent;
    procedure SetEnabled(Value: boolean);
    function GetInterval: cardinal;
    procedure SetInterval(Value: cardinal);
    procedure SetThreshold(Value: cardinal);
    procedure Tick(Sender: TObject);
  protected
    procedure Loaded; override;
    function QueryIdle(out Milliseconds: QWord; out Message: string): boolean; virtual;
  public
    constructor Create(AOwner: TComponent); override;
    // Also usable without Enabled or a GUI timer, for example in a console loop.
    function Check: boolean;
    property IsIdle: boolean read IdleState;
    property IdleMilliseconds: QWord read Elapsed;
    property LastError: string read ErrorText;
  published
    // True watches session input once the form loads, firing OnIdle and
    // OnActive as the state changes. Nothing runs in the designer.
    property Enabled: boolean read Requested write SetEnabled default False;
    // Milliseconds without any session input - mouse or keyboard, in any
    // application - that count as idle. Default one minute.
    property IdleThreshold: cardinal read Threshold write SetThreshold default 60000;
    // Milliseconds between checks of the session's idle time. Default one second.
    property PollInterval: cardinal read GetInterval write SetInterval default 1000;
    // The session crossed the threshold. Fires once per idle period, including
    // on the first check if the session is already idle.
    property OnIdle: TNotifyEvent read IdleHandler write IdleHandler;
    // Input resumed after an idle period. Fires once; a session that was
    // active all along does not fire it.
    property OnActive: TNotifyEvent read ActiveHandler write ActiveHandler;
    // The idle query failed, which also stops monitoring; the message stays
    // in LastError. Set Enabled again to retry.
    property OnError: THIDSystemIdleErrorEvent read ErrorHandler write ErrorHandler;
  end;

implementation

uses IdleInput;

constructor THIDSystemIdle.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  Threshold := 60000;
  Poll := TTimer.Create(Self);
  Poll.Enabled := False;
  Poll.Interval := 1000;
  Poll.OnTimer := @Tick;
end;

procedure THIDSystemIdle.Loaded;
begin
  inherited Loaded;
  if Enabled then SetEnabled(True);
end;

procedure THIDSystemIdle.SetEnabled(Value: boolean);
begin
  Requested := Value;
  Poll.Enabled := False;
  KnownState := False;
  IdleState := False;
  if (csLoading in ComponentState) or (csDesigning in ComponentState) then Exit;
  if Value and Check then Poll.Enabled := Requested;
end;

function THIDSystemIdle.GetInterval: cardinal;
begin
  Result := Poll.Interval;
end;

procedure THIDSystemIdle.SetInterval(Value: cardinal);
begin
  if Value = 0 then raise ERangeError.Create('PollInterval must be greater than zero.');
  Poll.Interval := Value;
end;

procedure THIDSystemIdle.SetThreshold(Value: cardinal);
begin
  if Value = 0 then raise ERangeError.Create('IdleThreshold must be greater than zero.');
  Threshold := Value;
end;

procedure THIDSystemIdle.Tick(Sender: TObject);
begin
  Check;
end;

function THIDSystemIdle.Check: boolean;
var
  Value: QWord;
  Message: string;
  NowIdle, NotifyIdle, NotifyActive: boolean;
begin
  if csDesigning in ComponentState then Exit(False);
  Result := QueryIdle(Value, Message);
  if not Result then
  begin
    Poll.Enabled := False;
    Requested := False;
    KnownState := False;
    IdleState := False;
    Elapsed := 0;
    ErrorText := Message;
    if Assigned(ErrorHandler) then ErrorHandler(Self, Message);
    Exit;
  end;
  ErrorText := '';
  Elapsed := Value;
  NowIdle := Value >= IdleThreshold;
  NotifyIdle := NowIdle and (not KnownState or not IdleState);
  NotifyActive := not NowIdle and KnownState and IdleState;
  KnownState := True;
  IdleState := NowIdle;
  if NotifyIdle and Assigned(IdleHandler) then IdleHandler(Self);
  if NotifyActive and Assigned(ActiveHandler) then ActiveHandler(Self);
end;

function THIDSystemIdle.QueryIdle(out Milliseconds: QWord; out Message: string): boolean;
begin
  Result := QuerySystemIdle(Milliseconds, Message);
end;

end.
