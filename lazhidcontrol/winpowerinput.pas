unit WinPowerInput;
{$mode objfpc}{$H+}
// Copyright (c) 2026 Tony Stone. GPL-2.0-or-later; see COPYING.GPL2.
interface
uses Windows, LMessages, PowerInput;
type
  TWinPowerInput = class(TPowerInput)
  private
    Receiver: HWND;
    Previous: TPowerStatus;
    procedure WindowProc(var Message: TLMessage);
  public
    destructor Destroy; override;
    function Start: Boolean; override;
    procedure Stop; override;
    function Poll(out Events: TPowerEvents): Boolean; override;
    function ReadStatus(out Status: TPowerStatus): Boolean; override;
  end;
implementation
uses LCLIntf;
function TWinPowerInput.Start: Boolean;
begin
  Receiver := LCLIntf.AllocateHWnd(@WindowProc);
  Result := Receiver <> 0;
  if not Result then ErrorText := 'Cannot create the power notification window.'
  else Result := ReadStatus(Previous);
end;
procedure TWinPowerInput.Stop;
begin
  if Receiver <> 0 then LCLIntf.DeallocateHWnd(Receiver);
  Receiver := 0;
  Pending := nil;
end;
destructor TWinPowerInput.Destroy;
begin Stop; inherited Destroy; end;
procedure TWinPowerInput.WindowProc(var Message: TLMessage);
begin
  if Message.Msg = WM_POWERBROADCAST then begin
    case Message.WParam of
      $0004: AddEvent(peSuspend); // PBT_APMSUSPEND
      $0012: AddEvent(peResume);  // PBT_APMRESUMEAUTOMATIC: one notification per wake
      $000A: AddEvent(peChanged); // PBT_APMPOWERSTATUSCHANGE
    end;
    Message.Result := 1;
  end else Message.Result := Windows.DefWindowProc(Receiver, Message.Msg, Message.WParam, Message.LParam);
end;
function TWinPowerInput.Poll(out Events: TPowerEvents): Boolean;
var Current: TPowerStatus;
begin
  // The cheap status query also catches battery percentage changes between broadcasts.
  Result := ReadStatus(Current);
  if not Result then begin Events := nil; Exit; end;
  if (Current.Source <> Previous.Source) or (Current.BatteryPercent <> Previous.BatteryPercent) or
    (Current.Charging <> Previous.Charging) then AddEvent(peChanged);
  Previous := Current;
  inherited Poll(Events);
end;
function TWinPowerInput.ReadStatus(out Status: TPowerStatus): Boolean;
var Native: TSystemPowerStatus;
begin
  Status := Default(TPowerStatus);
  Status.Source := psUnknown;
  Status.BatteryPercent := -1;
  Result := GetSystemPowerStatus(Native);
  if not Result then begin ErrorText := 'Windows could not query power status.'; Exit; end;
  case Native.ACLineStatus of 0: Status.Source := psBattery; 1: Status.Source := psAC; end;
  if (Native.BatteryFlag <> 255) and ((Native.BatteryFlag and 128) = 0) then begin
    if Native.BatteryLifePercent <= 100 then Status.BatteryPercent := Native.BatteryLifePercent;
    Status.Charging := (Native.BatteryFlag and 8) <> 0;
  end;
end;
end.
