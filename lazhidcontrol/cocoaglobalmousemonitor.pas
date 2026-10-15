// Copyright (c) 2026 Tony Stone. 0BSD.
unit CocoaGlobalMouseMonitor;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Controls, MacOSAll, GlobalMouseMonitorIntf;

type
  { Every mouse click on the system, through a Core Graphics event tap. }
  TCocoaGlobalMouseListener = class(TGlobalMouseListener)
  private
    Tap: CFMachPortRef;
    Source: CFRunLoopSourceRef;
  protected
    procedure DoStartMonitoring; override;
    procedure DoStopMonitoring; override;
  end;

function InitializeGlobalMouseListener: TGlobalMouseListener;

implementation

uses
  CocoaEventTap;

function InitializeGlobalMouseListener: TGlobalMouseListener;
begin
  Result := TCocoaGlobalMouseListener.Create;
end;

function Clicked(Proxy: CGEventTapProxy; Kind: CGEventType; Event: CGEventRef; Info: UnivPtr): CGEventRef; mwpascal;
var
  Listener: TCocoaGlobalMouseListener;
  Button: TMouseButton;
  Where: CGPoint;
begin
  Result := Event;
  Listener := TCocoaGlobalMouseListener(Info);
  if Kind = kCGEventTapDisabledByTimeout then CGEventTapEnable(Listener.Tap, 1);
  if not Assigned(Listener.OnMouseClickEvent) then Exit;
  case Kind of
    kCGEventLeftMouseDown: Button := mbLeft;
    kCGEventRightMouseDown: Button := mbRight;
    kCGEventOtherMouseDown:
      // button 2 is the wheel; the rest are the side buttons
      case CGEventGetIntegerValueField(Event, kCGMouseEventButtonNumber) of
        2: Button := mbMiddle;
        3: Button := mbExtra1;
        else
          Button := mbExtra2;
      end;
    else
      Exit;
  end;
  Where := CGEventGetLocation(Event);
  Listener.OnMouseClickEvent(Button, Round(Where.x), Round(Where.y));
end;

procedure TCocoaGlobalMouseListener.DoStartMonitoring;
begin
  Active := StartEventTap((CGEventMask(1) shl kCGEventLeftMouseDown) or (CGEventMask(1) shl kCGEventRightMouseDown) or
    (CGEventMask(1) shl kCGEventOtherMouseDown), @Clicked, Self, Tap, Source);
end;

procedure TCocoaGlobalMouseListener.DoStopMonitoring;
begin
  StopEventTap(Tap, Source);
  Active := False;
end;

end.
