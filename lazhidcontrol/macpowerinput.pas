unit MacPowerInput;
{$mode objfpc}{$H+}
{$modeswitch objectivec1}
// Copyright (c) 2026 Tony Stone. GPL-2.0-or-later; see COPYING.GPL2.
interface
uses CocoaAll, PowerInput;
type
  TMacPowerInput = class;
  TSleepWatcher = objcclass(NSObject)
  public
    Owner: TMacPowerInput;
    procedure willSleep(note: NSNotification); message 'willSleep:';
    procedure didWake(note: NSNotification); message 'didWake:';
  end;
  TMacPowerInput = class(TPowerInput)
  private
    Watcher: TSleepWatcher;
    Previous: TPowerStatus;
  public
    destructor Destroy; override;
    function Start: Boolean; override;
    procedure Stop; override;
    function Poll(out Events: TPowerEvents): Boolean; override;
    function ReadStatus(out Status: TPowerStatus): Boolean; override;
  end;
implementation
{$linkframework IOKit}
// The IOPS results are CF objects, toll-free bridged to their NS counterparts.
function IOPSCopyPowerSourcesInfo: Pointer; cdecl; external;
function IOPSCopyPowerSourcesList(blob: Pointer): Pointer; cdecl; external;
function IOPSGetPowerSourceDescription(blob, source: Pointer): Pointer; cdecl; external;
procedure CFRelease(cf: Pointer); cdecl; external;

procedure TSleepWatcher.willSleep(note: NSNotification);
begin if Owner <> nil then Owner.AddEvent(peSuspend); end;
procedure TSleepWatcher.didWake(note: NSNotification);
begin if Owner <> nil then Owner.AddEvent(peResume); end;

function TMacPowerInput.Start: Boolean;
var NC: NSNotificationCenter;
begin
  Watcher := TSleepWatcher.alloc.init;
  Watcher.Owner := Self;
  NC := NSWorkspace.sharedWorkspace.notificationCenter;
  NC.addObserver_selector_name_object(Watcher, objcselector('willSleep:'),
    NSWorkspaceWillSleepNotification, nil);
  NC.addObserver_selector_name_object(Watcher, objcselector('didWake:'),
    NSWorkspaceDidWakeNotification, nil);
  Result := ReadStatus(Previous);
end;
procedure TMacPowerInput.Stop;
begin
  if Watcher <> nil then begin
    NSWorkspace.sharedWorkspace.notificationCenter.removeObserver(Watcher);
    Watcher.Owner := nil;
    Watcher.release;
    Watcher := nil;
  end;
  Pending := nil;
end;
destructor TMacPowerInput.Destroy;
begin Stop; inherited Destroy; end;
function TMacPowerInput.Poll(out Events: TPowerEvents): Boolean;
var Current: TPowerStatus;
begin
  // Status is diffed per poll, like the Windows backend, so percentage
  // changes become peChanged without an extra notification channel.
  Result := ReadStatus(Current);
  if not Result then begin Events := nil; Exit; end;
  if (Current.Source <> Previous.Source) or (Current.BatteryPercent <> Previous.BatteryPercent) or
    (Current.Charging <> Previous.Charging) then AddEvent(peChanged);
  Previous := Current;
  inherited Poll(Events);
end;
function TMacPowerInput.ReadStatus(out Status: TPowerStatus): Boolean;
var Blob, ListRef: Pointer; List: NSArray; Desc: NSDictionary;
  I: Integer; State: NSString; Current, Max, Charging: NSNumber;
begin
  Status := Default(TPowerStatus);
  Status.Source := psUnknown;
  Status.BatteryPercent := -1;
  Blob := IOPSCopyPowerSourcesInfo;
  if Blob = nil then begin ErrorText := 'macOS could not query power sources.'; Exit(False); end;
  ListRef := IOPSCopyPowerSourcesList(Blob);
  try
    List := NSArray(ListRef);
    // A machine with no listed sources is a desktop on mains power.
    if (List = nil) or (List.count = 0) then Status.Source := psAC;
    for I := 0 to Integer(List.count) - 1 do begin
      Desc := NSDictionary(IOPSGetPowerSourceDescription(Blob, List.objectAtIndex(I)));
      if Desc = nil then Continue;
      State := NSString(Desc.objectForKey(NSSTR('Power Source State')));
      if (State <> nil) and State.isEqualToString(NSSTR('Battery Power')) then
        Status.Source := psBattery
      else Status.Source := psAC;
      Current := NSNumber(Desc.objectForKey(NSSTR('Current Capacity')));
      Max := NSNumber(Desc.objectForKey(NSSTR('Max Capacity')));
      if (Current <> nil) and (Max <> nil) and (Max.intValue > 0) then
        Status.BatteryPercent := (Current.intValue * 100) div Max.intValue;
      Charging := NSNumber(Desc.objectForKey(NSSTR('Is Charging')));
      Status.Charging := (Charging <> nil) and Charging.boolValue;
    end;
    Result := True;
  finally
    if ListRef <> nil then CFRelease(ListRef);
    CFRelease(Blob);
  end;
end;
end.
