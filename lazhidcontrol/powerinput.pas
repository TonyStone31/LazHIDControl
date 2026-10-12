unit PowerInput;
{$mode objfpc}{$H+}
// Copyright (c) 2026 Tony Stone. GPL-2.0-or-later; see COPYING.GPL2.
interface
type
  TPowerSource = (psUnknown, psAC, psBattery);
  TPowerStatus = record
    Source: TPowerSource;
    BatteryPercent: Integer;
    Charging: Boolean;
  end;
  TPowerEvent = (peSuspend, peResume, peChanged);
  TPowerEvents = array of TPowerEvent;
  TPowerInput = class
  protected
    Pending: TPowerEvents;
    procedure AddEvent(Event: TPowerEvent);
  public
    ErrorText: string;
    function Start: Boolean; virtual; abstract;
    procedure Stop; virtual; abstract;
    function Poll(out Events: TPowerEvents): Boolean; virtual;
    function ReadStatus(out Status: TPowerStatus): Boolean; virtual; abstract;
  end;
function CreatePowerInput: TPowerInput;
implementation
uses
  {$ifdef windows}WinPowerInput{$else}
  {$ifdef linux}LinuxPowerInput{$else}
  {$ifdef darwin}MacPowerInput{$else}SysUtils{$endif}{$endif}{$endif};
procedure TPowerInput.AddEvent(Event: TPowerEvent);
begin SetLength(Pending, Length(Pending) + 1); Pending[High(Pending)] := Event; end;
function TPowerInput.Poll(out Events: TPowerEvents): Boolean;
begin Events := Pending; Pending := nil; Result := True; end;
function CreatePowerInput: TPowerInput;
begin
  {$ifdef windows}Result := TWinPowerInput.Create;{$else}
  {$ifdef linux}Result := TLinuxPowerInput.Create;{$else}
  {$ifdef darwin}Result := TMacPowerInput.Create;{$else}
  Result := nil;{$endif}{$endif}{$endif}
end;
end.
