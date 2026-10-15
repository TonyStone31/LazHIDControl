// Copyright (c) 2026 Tony Stone. 0BSD.
unit CocoaEventTap;

{$mode objfpc}{$H+}

interface

uses
  MacOSAll;

const
  // event kinds and modifier flags that FPC's Core Graphics bindings leave out
  kCGEventLeftMouseDown = 1;
  kCGEventLeftMouseUp = 2;
  kCGEventRightMouseDown = 3;
  kCGEventRightMouseUp = 4;
  kCGEventMouseMoved = 5;
  kCGEventKeyDown = 10;
  kCGEventOtherMouseDown = 25;
  kCGEventOtherMouseUp = 26;
  kCGEventTapDisabledByTimeout = $FFFFFFFE;
  kCGEventFlagMaskAlphaShift = $00010000;
  kCGEventFlagMaskShift = $00020000;
  kCGEventFlagMaskControl = $00040000;
  kCGEventFlagMaskAlternate = $00080000;
  kCGEventFlagMaskCommand = $00100000;

{ A listen-only Core Graphics event tap on the main run loop.  Callback is
  called on the main thread for every event in Mask, wherever on the system
  it happens.  False when macOS will not make the tap: for key presses the
  user has to allow the program under Privacy & Security > Input Monitoring. }
function StartEventTap(Mask: CGEventMask; Callback: CGEventTapCallBack; Info: Pointer; out Tap: CFMachPortRef; out Source: CFRunLoopSourceRef): boolean;
procedure StopEventTap(var Tap: CFMachPortRef; var Source: CFRunLoopSourceRef);

implementation

function StartEventTap(Mask: CGEventMask; Callback: CGEventTapCallBack; Info: Pointer; out Tap: CFMachPortRef; out Source: CFRunLoopSourceRef): boolean;
begin
  Source := nil;
  Tap := CGEventTapCreate(kCGSessionEventTap, kCGHeadInsertEventTap, kCGEventTapOptionListenOnly, Mask, Callback, Info);
  Result := Tap <> nil;
  if not Result then Exit;
  Source := CFMachPortCreateRunLoopSource(nil, Tap, 0);
  CFRunLoopAddSource(CFRunLoopGetMain, Source, kCFRunLoopCommonModes);
  CGEventTapEnable(Tap, 1);
end;

procedure StopEventTap(var Tap: CFMachPortRef; var Source: CFRunLoopSourceRef);
begin
  if Tap = nil then Exit;
  CGEventTapEnable(Tap, 0);
  CFRunLoopRemoveSource(CFRunLoopGetMain, Source, kCFRunLoopCommonModes);
  CFRelease(Source);
  CFRelease(Tap);
  Source := nil;
  Tap := nil;
end;

end.
