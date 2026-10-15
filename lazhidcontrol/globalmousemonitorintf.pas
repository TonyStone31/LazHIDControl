{ GlobalMouseMonitorIntf

  Copyright (c) 2025-2026 Tony Stone. 0BSD.
}
unit GlobalMouseMonitorIntf;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Controls;

type
  TMouseClickEvent = procedure(Button: TMouseButton; X, Y: integer) of object;

  { TGlobalMouseListener - Abstract base class for system-wide mouse monitoring }
  TGlobalMouseListener = class
  protected
    OnMouseClickEvent: TMouseClickEvent;
    IsActive: boolean;
    procedure DoStartMonitoring; virtual; abstract;
    procedure DoStopMonitoring; virtual; abstract;
  public
    constructor Create; virtual;
    destructor Destroy; override;
    procedure StartMonitoring;
    procedure StopMonitoring;
    property OnMouseClick: TMouseClickEvent read OnMouseClickEvent write OnMouseClickEvent;
    property Active: boolean read IsActive write IsActive;
  end;

implementation

{ TGlobalMouseListener }

constructor TGlobalMouseListener.Create;
begin
  inherited Create;
  IsActive := False;
  OnMouseClickEvent := nil;
end;

destructor TGlobalMouseListener.Destroy;
begin
  StopMonitoring;
  inherited Destroy;
end;

procedure TGlobalMouseListener.StartMonitoring;
begin
  if not IsActive then
    DoStartMonitoring;
end;

procedure TGlobalMouseListener.StopMonitoring;
begin
  if IsActive then
    DoStopMonitoring;
end;

end.
