unit CocoaHotkeyInput;

{$mode objfpc}{$H+}
// Copyright (c) 2026 Tony Stone. 0BSD.

{ Hotkeys on macOS through the Carbon event manager's RegisterEventHotKey,
  which still hands out system-wide shortcuts and needs no Accessibility
  permission.  One application event handler serves them all. }

interface

uses
  Classes, HotkeyInputIntf;

type
  TCocoaHotkey = class(THotkey)
  private
    Id: longword;
    Ref: Pointer;
  protected
    procedure DoRegister; override;
    procedure DoUnregister; override;
  end;

implementation

uses
  SysUtils, HIDCocoaKeyInput;

  {$linkframework Carbon}

type
  OSStatus = longint;

  TEventHotKeyID = record
    Signature: longword;
    Id: longword;
  end;

  TEventTypeSpec = record
    EventClass: longword;
    EventKind: longword;
  end;

  TEventHandlerProc = function(CallRef, Event, UserData: Pointer): OSStatus; cdecl;

function GetApplicationEventTarget: Pointer; cdecl; external;
function InstallEventHandler(Target: Pointer; Handler: TEventHandlerProc; NumTypes: PtrUInt; List: Pointer; UserData: Pointer; OutRef: Pointer): OSStatus; cdecl; external;
function GetEventParameter(Event: Pointer; Name, DesiredType: longword; ActualType: Pointer; BufferSize: PtrUInt; ActualSize: Pointer; Data: Pointer): OSStatus; cdecl; external;
function RegisterEventHotKey(Code, Modifiers: longword; HotKeyID: TEventHotKeyID; Target: Pointer; Options: longword; out HotKeyRef: Pointer): OSStatus; cdecl; external;
function UnregisterEventHotKey(HotKeyRef: Pointer): OSStatus; cdecl; external;

const
  NoErr = 0;
  EventNotHandledErr = -9874;
  EventHotKeyExistsErr = -9878;
  EventClassKeyboard = $6B657962;    // 'keyb'
  EventHotKeyPressed = 5;
  ParamDirectObject = $2D2D2D2D;     // '----'
  TypeEventHotKeyID = $686B6964;     // 'hkid'
  ShiftKeyBit = $0200;
  OptionKeyBit = $0800;
  ControlKeyBit = $1000;
  // marks our hotkeys among any others the application registers
  OurSignature = $4C7A484B;          // 'LzHK'

var
  Hotkeys: TFPList;
  HandlerInstalled: boolean;
  LastId: longword;

function FindHotkey(AId: longword): TCocoaHotkey;
var
  I: integer;
begin
  Result := nil;
  if Hotkeys <> nil then
    for I := 0 to Hotkeys.Count - 1 do
      if TCocoaHotkey(Hotkeys[I]).Id = AId then Exit(TCocoaHotkey(Hotkeys[I]));
end;

function HotkeyEvent(CallRef, Event, UserData: Pointer): OSStatus; cdecl;
var
  HotKeyID: TEventHotKeyID;
  H: TCocoaHotkey;
begin
  Result := EventNotHandledErr;
  HotKeyID := Default(TEventHotKeyID);
  if GetEventParameter(Event, ParamDirectObject, TypeEventHotKeyID, nil, SizeOf(HotKeyID), nil, @HotKeyID) <> NoErr then Exit;
  if HotKeyID.Signature <> OurSignature then Exit;
  H := FindHotkey(HotKeyID.Id);
  if H = nil then Exit;
  H.PressedLater;
  Result := NoErr;
end;

procedure TCocoaHotkey.DoRegister;
var
  Code: word;
  Mods: longword;
  HotKeyID: TEventHotKeyID;
  Spec: TEventTypeSpec;
  Status: OSStatus;
  I: integer;
begin
  if GetCurrentThreadId <> MainThreadID then raise EHotkeyError.Create('Hotkeys must be registered from the main thread.');
  if Shift - [ssCtrl, ssAlt, ssShift] <> [] then raise EHotkeyError.Create('Only Ctrl, Alt and Shift can be part of a hotkey.');
  Code := VKToMacCode(Key);
  if Code = NoMacCode then raise EHotkeyError.CreateFmt('Key %d does not exist on a Mac keyboard.', [Key]);
  // refused here, so the reason is the same whatever macOS would answer
  for I := 0 to Hotkeys.Count - 1 do
    if (TCocoaHotkey(Hotkeys[I]).Key = Key) and (TCocoaHotkey(Hotkeys[I]).Shift = Shift) then
      raise EHotkeyError.Create('This combination is already registered in this program.');

  if not HandlerInstalled then
  begin
    Spec.EventClass := EventClassKeyboard;
    Spec.EventKind := EventHotKeyPressed;
    Status := InstallEventHandler(GetApplicationEventTarget, @HotkeyEvent, 1, @Spec, nil, nil);
    if Status <> NoErr then raise EHotkeyError.CreateFmt('The hotkey event handler could not be installed (status %d).', [Status]);
    HandlerInstalled := True;
  end;

  Mods := 0;
  if ssCtrl in Shift then Mods := Mods or ControlKeyBit;
  if ssAlt in Shift then Mods := Mods or OptionKeyBit;
  if ssShift in Shift then Mods := Mods or ShiftKeyBit;
  Inc(LastId);
  Id := LastId;
  HotKeyID.Signature := OurSignature;
  HotKeyID.Id := Id;
  Ref := nil;
  Status := RegisterEventHotKey(Code, Mods, HotKeyID, GetApplicationEventTarget, 0, Ref);
  if Status = EventHotKeyExistsErr then raise EHotkeyError.CreateFmt('This combination is already taken (status %d).', [Status]);
  if Status <> NoErr then raise EHotkeyError.CreateFmt('macOS refused this combination (status %d).', [Status]);
  Hotkeys.Add(Self);
  Registered := True;
end;

procedure TCocoaHotkey.DoUnregister;
begin
  if Hotkeys = nil then Exit;
  Hotkeys.Remove(Self);
  if Ref <> nil then UnregisterEventHotKey(Ref);
  Ref := nil;
end;

initialization
  Hotkeys := TFPList.Create;

finalization
  FreeAndNil(Hotkeys);
end.
