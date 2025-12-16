{ GlobalMouseMonitorIntf

  Copyright (C) 2025 LazHIDControl Contributors

  This source is free software; you can redistribute it and/or modify it under the terms of the
  GNU General Public License as published by the Free Software Foundation; either version 2 of the
  License, or (at your option) any later version.

  This code is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without
  even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
  General Public License for more details.

  A copy of the GNU General Public License is available on the World Wide Web at
  <http://www.gnu.org/copyleft/gpl.html>. You can also obtain it by writing to the Free Software
  Foundation, Inc., 51 Franklin Street - Fifth Floor, Boston, MA 02110-1335, USA.
}
unit GlobalMouseMonitorIntf;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Controls;

type
  TMouseClickEvent = procedure(Button: TMouseButton; X, Y: Integer) of object;

  { TGlobalMouseListener - Abstract base class for system-wide mouse monitoring }
  TGlobalMouseListener = class
  protected
    OnMouseClickEvent: TMouseClickEvent;
    IsActive: Boolean;
    procedure DoStartMonitoring; virtual; abstract;
    procedure DoStopMonitoring; virtual; abstract;
  public
    constructor Create; virtual;
    destructor Destroy; override;
    procedure StartMonitoring;
    procedure StopMonitoring;
    property OnMouseClick: TMouseClickEvent read OnMouseClickEvent write OnMouseClickEvent;
    property Active: Boolean read IsActive write IsActive;
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
