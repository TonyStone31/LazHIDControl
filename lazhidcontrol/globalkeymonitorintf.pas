{ GlobalKeyMonitorIntf

  Copyright (c) 2025-2026 Tony Stone. 0BSD.
}
unit GlobalKeyMonitorIntf;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LCLType;

type
  TKeyEvent = procedure(Key: word; Shift: TShiftState) of object;

  { TGlobalKeyListener - Abstract base class for system-wide key monitoring }
  TGlobalKeyListener = class
  protected
    OnKeyPressEvent: TKeyEvent;
    IsActive: boolean;
    procedure DoStartMonitoring; virtual; abstract;
    procedure DoStopMonitoring; virtual; abstract;
  public
    constructor Create; virtual;
    destructor Destroy; override;
    procedure StartMonitoring;
    procedure StopMonitoring;
    property OnKeyPress: TKeyEvent read OnKeyPressEvent write OnKeyPressEvent;
    property Active: boolean read IsActive write IsActive;
  end;

implementation

{ TGlobalKeyListener }

constructor TGlobalKeyListener.Create;
begin
  inherited Create;
  IsActive := False;
  OnKeyPressEvent := nil;
end;

destructor TGlobalKeyListener.Destroy;
begin
  StopMonitoring;
  inherited Destroy;
end;

procedure TGlobalKeyListener.StartMonitoring;
begin
  if not IsActive then
    DoStartMonitoring;
end;

procedure TGlobalKeyListener.StopMonitoring;
begin
  if IsActive then
    DoStopMonitoring;
end;

end.
