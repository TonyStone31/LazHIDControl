{ CocoaHotkeyInput

  Copyright (C) 2025 LazHIDControl Contributors

  macOS/Cocoa implementation of global hotkey registration.

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
unit CocoaHotkeyInput;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LCLType, HotkeyInputIntf;

type
  { TCocoaHotkey - Carbon's RegisterEventHotKey, which is still how macOS
    hands out a system-wide shortcut; it needs no Accessibility permission }
  TCocoaHotkey = class(THotkey)
  private
    HotKeyRef: Pointer;
    ID: longword;
  protected
    procedure DoRegister; override;
    procedure DoUnregister; override;
  end;

function InitializeHotkey(AKey: word; AShift: TShiftState; AOnHotkey: TKeyNotifyEvent): THotkey;

implementation

uses
  HIDCocoaKeyInput;

  {$linkframework Carbon}

type
  TEventHotKeyID = record
    signature, id: longword;
  end;

  TEventTypeSpec = record
    eventClass, eventKind: longword;
  end;

  TEventHandler = function(HandlerCall, Event, UserData: Pointer): longint; cdecl;

const
  kEventClassKeyboard = $6B657962;      // 'keyb'
  kEventHotKeyPressed = 5;
  kEventParamDirectObject = $2D2D2D2D;  // '----'
  typeEventHotKeyID = $686B6964;        // 'hkid'
  eventNotHandledErr = -9874;
  controlKey = 1 shl 12;
  optionKey = 1 shl 11;
  shiftKey = 1 shl 9;
  Signature = $4C484944;                // 'LHID'

function GetApplicationEventTarget: Pointer; cdecl; external;
function InstallEventHandler(Target: Pointer; Handler: TEventHandler; NumTypes: longword; EventTypes, UserData: Pointer; out HandlerRef: Pointer): longint; cdecl; external;
function RemoveEventHandler(HandlerRef: Pointer): longint; cdecl; external;
function RegisterEventHotKey(KeyCode, Modifiers: longword; HotKeyID: TEventHotKeyID; Target: Pointer; Options: longword; out HotKeyRef: Pointer): longint; cdecl; external;
function UnregisterEventHotKey(HotKeyRef: Pointer): longint; cdecl; external;
function GetEventParameter(Event: Pointer; Name, DesiredType: longword; ActualType: Pointer; BufferSize: longword; ActualSize, Data: Pointer): longint; cdecl; external;

var
  Hotkeys: TList = nil;      // registered TCocoaHotkey; nil while there are none
  Handler: Pointer = nil;    // one event handler serves them all
  LastID: longword = 0;

function HotkeyPressed(HandlerCall, Event, UserData: Pointer): longint; cdecl;
var
  Pressed: TEventHotKeyID;
  I: integer;
  Hotkey: TCocoaHotkey;
begin
  Result := eventNotHandledErr;
  if Hotkeys = nil then Exit;
  if GetEventParameter(Event, kEventParamDirectObject, typeEventHotKeyID, nil, SizeOf(Pressed), nil, @Pressed) <> 0 then Exit;
  for I := 0 to Hotkeys.Count - 1 do
  begin
    Hotkey := TCocoaHotkey(Hotkeys[I]);
    if Hotkey.ID = Pressed.id then
    begin
      if Assigned(Hotkey.OnHotkeyEvent) then Hotkey.OnHotkeyEvent(Hotkey, Hotkey.KeyCode, Hotkey.ShiftState);
      Exit(0);
    end;
  end;
end;

procedure DropHandlerIfUnused;
begin
  if (Hotkeys = nil) or (Hotkeys.Count > 0) then Exit;
  if Handler <> nil then RemoveEventHandler(Handler);
  Handler := nil;
  FreeAndNil(Hotkeys);
end;

function InitializeHotkey(AKey: word; AShift: TShiftState; AOnHotkey: TKeyNotifyEvent): THotkey;
begin
  Result := TCocoaHotkey.Create(AKey, AShift, AOnHotkey);
end;

{ TCocoaHotkey }

procedure TCocoaHotkey.DoRegister;
var
  EventType: TEventTypeSpec;
  HotKeyID: TEventHotKeyID;
  Modifiers: longword;
  Status: longint;
begin
  if VKToMacCode(KeyCode) = NoMacCode then
    raise Exception.CreateFmt('Key %d cannot be a hotkey on macOS.', [KeyCode]);

  if Hotkeys = nil then
  begin
    Hotkeys := TList.Create;
    EventType.eventClass := kEventClassKeyboard;
    EventType.eventKind := kEventHotKeyPressed;
    Status := InstallEventHandler(GetApplicationEventTarget, @HotkeyPressed, 1, @EventType, nil, Handler);
    if Status <> 0 then
    begin
      DropHandlerIfUnused;
      raise Exception.CreateFmt('Could not install the macOS hotkey handler (%d).', [Status]);
    end;
  end;

  Modifiers := 0;
  if ssCtrl in ShiftState then Modifiers := Modifiers or controlKey;
  if ssAlt in ShiftState then Modifiers := Modifiers or optionKey;
  if ssShift in ShiftState then Modifiers := Modifiers or shiftKey;
  Inc(LastID);
  ID := LastID;
  HotKeyID.signature := Signature;
  HotKeyID.id := ID;
  Status := RegisterEventHotKey(VKToMacCode(KeyCode), Modifiers, HotKeyID, GetApplicationEventTarget, 0, HotKeyRef);
  if Status <> 0 then
  begin
    DropHandlerIfUnused;
    raise Exception.CreateFmt('Could not register the macOS hotkey (%d).', [Status]);
  end;
  Hotkeys.Add(Self);
  Registered := True;
end;

procedure TCocoaHotkey.DoUnregister;
begin
  UnregisterEventHotKey(HotKeyRef);
  HotKeyRef := nil;
  Registered := False;
  Hotkeys.Remove(Self);
  DropHandlerIfUnused;
end;

end.
