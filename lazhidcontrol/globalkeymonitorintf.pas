{ GlobalKeyMonitorIntf

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
unit GlobalKeyMonitorIntf;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LCLType;

type
  TKeyEvent = procedure(Key: Word; Shift: TShiftState) of object;

  { TGlobalKeyListener - Abstract base class for system-wide key monitoring }
  TGlobalKeyListener = class
  protected
    OnKeyPressEvent: TKeyEvent;
    IsActive: Boolean;
    procedure DoStartMonitoring; virtual; abstract;
    procedure DoStopMonitoring; virtual; abstract;
  public
    constructor Create; virtual;
    destructor Destroy; override;
    procedure StartMonitoring;
    procedure StopMonitoring;
    property OnKeyPress: TKeyEvent read OnKeyPressEvent write OnKeyPressEvent;
    property Active: Boolean read IsActive write IsActive;
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
