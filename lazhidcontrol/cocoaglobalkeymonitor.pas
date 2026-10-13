{ CocoaGlobalKeyMonitor

  Copyright (C) 2025 LazHIDControl Contributors

  macOS/Cocoa implementation of global key monitoring .

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
unit CocoaGlobalKeyMonitor;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LCLType, MacOSAll, GlobalKeyMonitorIntf;

type
  { Every key press on the system, through a Core Graphics event tap.  macOS
    hands key presses only to a program the user has allowed under Privacy &
    Security > Input Monitoring; until then Active stays False. }
  TCocoaGlobalKeyListener = class(TGlobalKeyListener)
  private
    Tap: CFMachPortRef;
    Source: CFRunLoopSourceRef;
  protected
    procedure DoStartMonitoring; override;
    procedure DoStopMonitoring; override;
  end;

function InitializeGlobalKeyListener: TGlobalKeyListener;

implementation

uses
  CocoaEventTap, HIDCocoaKeyInput;

function InitializeGlobalKeyListener: TGlobalKeyListener;
begin
  Result := TCocoaGlobalKeyListener.Create;
end;

function MacCodeToVK(Code: word): word;
var
  Key: word;
begin
  for Key := 1 to 255 do
    if VKToMacCode(Key) = Code then Exit(Key);
  Result := 0;
end;

function KeyPressed(Proxy: CGEventTapProxy; Kind: CGEventType; Event: CGEventRef; Info: UnivPtr): CGEventRef; mwpascal;
var
  Listener: TCocoaGlobalKeyListener;
  Flags: CGEventFlags;
  Shift: TShiftState;
begin
  Result := Event;
  Listener := TCocoaGlobalKeyListener(Info);
  // macOS switches off a tap that was slow to answer; switch it back on
  if Kind = kCGEventTapDisabledByTimeout then CGEventTapEnable(Listener.Tap, 1);
  if (Kind <> kCGEventKeyDown) or not Assigned(Listener.OnKeyPressEvent) then Exit;
  Flags := CGEventGetFlags(Event);
  Shift := [];
  if Flags and kCGEventFlagMaskShift <> 0 then Include(Shift, ssShift);
  if Flags and kCGEventFlagMaskControl <> 0 then Include(Shift, ssCtrl);
  if Flags and kCGEventFlagMaskAlternate <> 0 then Include(Shift, ssAlt);
  if Flags and kCGEventFlagMaskCommand <> 0 then Include(Shift, ssMeta);
  Listener.OnKeyPressEvent(MacCodeToVK(CGEventGetIntegerValueField(Event, kCGKeyboardEventKeycode)), Shift);
end;

procedure TCocoaGlobalKeyListener.DoStartMonitoring;
begin
  Active := StartEventTap(CGEventMask(1) shl kCGEventKeyDown, @KeyPressed, Self, Tap, Source);
end;

procedure TCocoaGlobalKeyListener.DoStopMonitoring;
begin
  StopEventTap(Tap, Source);
  Active := False;
end;

end.
