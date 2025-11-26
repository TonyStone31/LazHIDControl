{ HotkeyInputIntf

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
unit HotkeyInputIntf;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LCLType;

type
  TKeyNotifyEvent = procedure(Sender: TObject; Key: Word; Shift: TShiftState) of object;

  { THotkey - Abstract base class for global hotkey registration }
  THotkey = class
  protected
    KeyCode: Word;
    ShiftState: TShiftState;
    OnHotkeyEvent: TKeyNotifyEvent;
    IsRegistered: Boolean;
    procedure DoRegister; virtual; abstract;
    procedure DoUnregister; virtual; abstract;
  public
    constructor Create(AKey: Word; AShift: TShiftState; AOnHotkey: TKeyNotifyEvent); virtual;
    destructor Destroy; override;
    procedure Register;
    procedure Unregister;
    property Key: Word read KeyCode;
    property Shift: TShiftState read ShiftState;
    property Registered: Boolean read IsRegistered write IsRegistered;
  end;

implementation

{ THotkey }

constructor THotkey.Create(AKey: Word; AShift: TShiftState; AOnHotkey: TKeyNotifyEvent);
begin
  inherited Create;
  KeyCode := AKey;
  ShiftState := AShift;
  OnHotkeyEvent := AOnHotkey;
  IsRegistered := False;
end;

destructor THotkey.Destroy;
begin
  Unregister;
  inherited Destroy;
end;

procedure THotkey.Register;
begin
  if not IsRegistered then
    DoRegister;
end;

procedure THotkey.Unregister;
begin
  if IsRegistered then
    DoUnregister;
end;

end.
