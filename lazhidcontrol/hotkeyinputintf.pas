unit HotkeyInputIntf;

{$mode objfpc}{$H+}
// Copyright (c) 2026 Tony Stone. 0BSD.

{ A system-wide hotkey: one LCL virtual key with Ctrl, Alt and Shift.  This
  class registers nothing; each platform's backend overrides DoRegister and
  DoUnregister and calls PressedLater when the combination comes in. }

interface

uses
  Classes, SysUtils;

type
  TKeyNotifyEvent = procedure(Sender: TObject; Key: word; Shift: TShiftState) of object;

  EHotkeyError = class(Exception);

  THotkey = class
  private
    FKey: word;
    FShift: TShiftState;
    FOnHotkey: TKeyNotifyEvent;
    FRegistered: boolean;
    FSerial: PtrInt;
  protected
    // Take the combination and set Registered when that worked; may raise
    // an EHotkeyError that says why it did not.
    procedure DoRegister; virtual;
    // Give the combination back.  Only called while Registered.
    procedure DoUnregister; virtual;
    // Runs the handler now.  The handler may free this object, so a caller
    // must not touch it afterwards.
    procedure Pressed;
    // Runs the handler from the application's message loop, if this hotkey
    // still exists and is registered by then.
    procedure PressedLater;
  public
    constructor Create(AKey: word; AShift: TShiftState; AOnHotkey: TKeyNotifyEvent); virtual;
    destructor Destroy; override;
    procedure Register;
    procedure Unregister;
    property Key: word read FKey;
    property Shift: TShiftState read FShift;
    // Backends and test doubles set this.
    property Registered: boolean read FRegistered write FRegistered;
  end;

implementation

uses
  Forms;

type
  TDelivery = class
    procedure Deliver(Data: PtrInt);
  end;

var
  // every hotkey alive, so a queued press can tell whether its hotkey still is
  Live: TFPList;
  LastSerial: PtrInt;
  Delivery: TDelivery;

procedure TDelivery.Deliver(Data: PtrInt);
var
  I: integer;
  H: THotkey;
begin
  for I := 0 to Live.Count - 1 do
  begin
    H := THotkey(Live[I]);
    if H.FSerial = Data then
    begin
      if H.Registered then H.Pressed;
      Exit;
    end;
  end;
end;

constructor THotkey.Create(AKey: word; AShift: TShiftState; AOnHotkey: TKeyNotifyEvent);
begin
  inherited Create;
  FKey := AKey;
  FShift := AShift;
  FOnHotkey := AOnHotkey;
  Inc(LastSerial);
  FSerial := LastSerial;
  if Live <> nil then Live.Add(Self);
end;

destructor THotkey.Destroy;
begin
  Unregister;
  // forms are freed with the Application, after this unit's finalization
  if Live <> nil then Live.Remove(Self);
  inherited Destroy;
end;

procedure THotkey.DoRegister;
begin
end;

procedure THotkey.DoUnregister;
begin
end;

procedure THotkey.Pressed;
begin
  if Assigned(FOnHotkey) then FOnHotkey(Self, FKey, FShift);
end;

procedure THotkey.PressedLater;
begin
  if Delivery <> nil then Application.QueueAsyncCall(@Delivery.Deliver, FSerial);
end;

procedure THotkey.Register;
begin
  if not FRegistered then DoRegister;
end;

procedure THotkey.Unregister;
begin
  if not FRegistered then Exit;
  DoUnregister;
  FRegistered := False;
end;

initialization
  Live := TFPList.Create;
  Delivery := TDelivery.Create;

finalization
  if Application <> nil then Application.RemoveAsyncCalls(Delivery);
  FreeAndNil(Delivery);
  FreeAndNil(Live);
end.
