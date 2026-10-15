unit HIDMouseAndKeyInput;

{$mode objfpc}{$H+}
// Copyright (c) 2026 Tony Stone. 0BSD.

{ The two objects that send keys and mouse events, made for the system the
  program runs on. }

{ The input units began as Tom Gregorovic's MouseAndKeyInput (2008), with
  Cocoa input by Sammarco Francesco.  They were written new in 2026 from a
  specification so the package could be offered under a less restrictive
  license. }

interface

uses
  HIDKeyInputIntf, HIDMouseInputIntf;

var
  MouseInput: TMouseInput;
  KeyInput: TKeyInput;

implementation

uses
  SysUtils
  {$if defined(windows)}
  , HIDWinKeyInput, HIDWinMouseInput
  {$elseif defined(darwin)}
  , HIDCocoaKeyInput, HIDCocoaMouseInput
  {$elseif defined(linux)}
  , HIDXKeyInput, HIDXMouseInput, WaylandMouseAndKeyInput
  {$elseif defined(unix)}
  , HIDXKeyInput, HIDXMouseInput
  {$endif};

procedure CreateInputs;
begin
  {$if defined(windows)}
  MouseInput := InitializeWinMouseInput;
  KeyInput := InitializeWinKeyInput;
  {$elseif defined(darwin)}
  MouseInput := InitializeCocoaMouseInput;
  KeyInput := InitializeCocoaKeyInput;
  {$elseif defined(linux)}
  if GetEnvironmentVariable('WAYLAND_DISPLAY') <> '' then
  begin
    MouseInput := InitializeWaylandMouseInput;
    KeyInput := InitializeWaylandKeyInput;
  end
  else
  begin
    MouseInput := InitializeXMouseInput;
    KeyInput := InitializeXKeyInput;
  end;
  {$elseif defined(unix)}
  MouseInput := InitializeXMouseInput;
  KeyInput := InitializeXKeyInput;
  {$endif}
end;

initialization
  CreateInputs;

finalization
  FreeAndNil(MouseInput);
  FreeAndNil(KeyInput);
end.
