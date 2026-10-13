program MacOSSmoke;
{$mode objfpc}{$H+}{$codepage utf8}
// Copyright (c) 2026 Tony Stone. GPL-2.0-or-later.
uses
  Interfaces,
  Forms,
  Classes,
  SysUtils,
  Types,
  Menus,
  LCLType,
  Clipbrd,
  HIDClipboard,
  HIDPower,
  PowerInput,
  HIDSystemIdle,
  HIDWindow,
  HIDHotkey,
  HIDScreenshot,
  HIDMouseAndKeyInput,
  WindowManagerIntf,
  GlobalKeyMonitor,
  GlobalMouseMonitor;

var
  Checks, Failures, Skipped: integer;

type
  TEvents = class
    Changes: integer;
    HotkeyFired: integer;
    procedure Changed(Sender: TObject);
    procedure HotkeyPressed(Sender: TObject);
  end;

  procedure TEvents.Changed(Sender: TObject);
  begin
    Inc(Changes);
  end;

  procedure TEvents.HotkeyPressed(Sender: TObject);
  begin
    Inc(HotkeyFired);
  end;

  procedure Check(Condition: boolean; const Message: string);
  begin
    Inc(Checks);
    if Condition then WriteLn('PASS: ', Message)
    else
    begin
      Inc(Failures);
      WriteLn('FAIL: ', Message);
    end;
  end;

  procedure Skip(const Message: string);
  begin
    Inc(Skipped);
    WriteLn('SKIP: ', Message);
  end;

  procedure ClipboardChecks;
  const
    Text = 'LazHID Cocoa clipboard: é 漢 😀';
  var
    Monitor: THIDClipboardMonitor;
    Events: TEvents;
  begin
    Monitor := THIDClipboardMonitor.Create(nil);
    Events := TEvents.Create;
    try
      Monitor.OnChanged := @Events.Changed;
      Monitor.Enabled := True;
      Check(Monitor.Enabled and (Monitor.LastError = ''), 'clipboard monitor starts');
      Check(Events.Changes = 0, 'clipboard baseline does not emit a change');
      Clipboard.AsText := Text;
      Check(Monitor.Check, 'clipboard change can be polled');
      Check(Events.Changes = 1, 'clipboard write emits one change');
      Check(Clipboard.AsText = Text, 'clipboard preserves Unicode text');
      Check(Monitor.Check and (Events.Changes = 1), 'unchanged clipboard emits no duplicate');
      Clipboard.AsText := Text + ' second';
      Check(Monitor.Check and (Events.Changes = 2), 'second clipboard write is detected');
      Monitor.Enabled := False;
      Check(not Monitor.Enabled, 'clipboard monitor stops');
    finally
      Monitor.Free;
      Events.Free;
    end;
  end;

  procedure PowerChecks;
  var
    Monitor: THIDPowerMonitor;
  begin
    Monitor := THIDPowerMonitor.Create(nil);
    try
      Monitor.Enabled := True;
      Check(Monitor.Enabled and (Monitor.LastError = ''), 'power monitor starts');
      Check(Monitor.Refresh, 'power status refresh succeeds');
      Check(Monitor.PowerSource in [psAC, psBattery], 'power source is known');
      Check((Monitor.BatteryPercent >= -1) and (Monitor.BatteryPercent <= 100), 'battery status is valid');
      WriteLn('Power source ordinal: ', Ord(Monitor.PowerSource), '; battery: ', Monitor.BatteryPercent);
      Check(Monitor.Check, 'power notification queue can be polled');
      Monitor.Enabled := False;
      Check(not Monitor.Enabled, 'power monitor stops');
      Monitor.Enabled := True;
      Check(Monitor.Enabled and Monitor.Check, 'power monitor can restart');
      Skip('battery changes and sleep/wake delivery require hardware/session events');
    finally
      Monitor.Free;
    end;
  end;

  procedure IdleChecks;
  var
    Monitor: THIDSystemIdle;
  begin
    Monitor := THIDSystemIdle.Create(nil);
    try
      Check(Monitor.Check, 'system idle query succeeds');
      Check(Monitor.LastError = '', 'system idle query has no error');
      WriteLn('Idle milliseconds: ', Monitor.IdleMilliseconds);
      Monitor.Enabled := True;
      Check(Monitor.Enabled and Monitor.Check, 'idle monitoring starts and polls');
      Monitor.Enabled := False;
      Check(not Monitor.Enabled, 'idle monitoring stops');
      Skip('idle/active transitions require real session input');
    finally
      Monitor.Free;
    end;
  end;

  { Windows, pictures, hotkeys and the monitors.  What macOS only does for a
    program the user has allowed is reported, not counted as a failure. }
  procedure DesktopChecks;
  var
    Window: THIDWindow;
    Hotkey: THIDHotkey;
    Shot: THIDScreenshot;
    Info: TWindowInfo;
    Keys: TGlobalKeyListener;
    Clicks: TGlobalMouseListener;
    Rejected: boolean;
  begin
    Window := THIDWindow.Create(nil);
    Hotkey := THIDHotkey.Create(nil);
    Shot := THIDScreenshot.Create(nil);
    Keys := CreateGlobalKeyListener;
    Clicks := CreateGlobalMouseListener;
    try
      if Window.Find and Window.GetInfo(Info) then
      begin
        Check(Info.Rect.Width > 0, 'the front window has a size');
        Check(Info.ProcessID <> 0, 'the front window has a process');
        WriteLn('Front window: "', Info.Title, '" of ', Info.ClassName, ' at ', Info.Rect.Left, ',', Info.Rect.Top, ' ',
          Info.Rect.Width, 'x', Info.Rect.Height);
        if Window.MoveTo(Info.Rect.Left + 20, Info.Rect.Top + 20) then
        begin
          Check(Window.GetInfo(Info), 'the moved window is still found');
          Check(Window.MoveTo(Info.Rect.Left - 20, Info.Rect.Top - 20), 'the window moves back');
        end
        else
          Skip('moving a window needs Accessibility permission: ' + Window.LastError);
      end
      else
        Skip('no window on the screen to look at: ' + Window.LastError);

      Hotkey.ShortCut := ShortCut(VK_F9, [ssCtrl, ssShift]);
      Check(Hotkey.RegisterHotkey and Hotkey.Registered, 'global hotkey registers');
      Hotkey.UnregisterHotkey;
      Check(not Hotkey.Registered, 'global hotkey unregisters');

      Check(Shot.Capture and (Shot.Bitmap.Width > 0) and (Shot.Bitmap.Height > 0), 'desktop capture has pixels');
      WriteLn('Desktop capture: ', Shot.Bitmap.Width, 'x', Shot.Bitmap.Height);

      Clicks.StartMonitoring;
      Check(Clicks.Active, 'the mouse monitor starts');
      Clicks.StopMonitoring;
      Keys.StartMonitoring;
      if Keys.Active then Check(True, 'the key monitor starts')
      else
        Skip('the key monitor needs Input Monitoring permission');
      Keys.StopMonitoring;

      Rejected := False;
      try
        KeyInput.Down(VK_APPS);
      except
        on E: Exception do Rejected := True;
      end;
      Check(Rejected, 'a key a Mac does not have is refused');
      WriteLn('Caps Lock is ', BoolToStr(KeyInput.GetCapsLockState, 'on', 'off'));
      Skip('battery changes, sleep/wake and typing into another program are for the demo, by hand');
    finally
      Clicks.Free;
      Keys.Free;
      Shot.Free;
      Hotkey.Free;
      Window.Free;
    end;
  end;

var
  Hotkey: THIDHotkey;
  Events: TEvents;
  Started: QWord;
begin
  Application.Initialize;
  if (ParamCount > 0) and (ParamStr(1) = '--hotkey-interactive') then
  begin
    Hotkey := THIDHotkey.Create(nil);
    Events := TEvents.Create;
    try
      Hotkey.OnHotkey := @Events.HotkeyPressed;
      Hotkey.ShortCut := ShortCut(VK_F9, [ssCtrl, ssShift]);
      Check(Hotkey.RegisterHotkey, 'interactive hotkey registers: ' + Hotkey.LastError);
      Started := GetTickCount64;
      while (GetTickCount64 - Started < 15000) and (Events.HotkeyFired = 0) do
      begin
        Application.ProcessMessages;
        Sleep(20);
      end;
      Check(Events.HotkeyFired = 1, 'Ctrl+Shift+F9 dispatches to the callback');
      Hotkey.UnregisterHotkey;
    finally
      Hotkey.Free;
      Events.Free;
    end;
    Halt(Ord(Failures > 0));
  end;
  try
    ClipboardChecks;
    PowerChecks;
    IdleChecks;
    DesktopChecks;
  except
    on E: Exception do
    begin
      Inc(Failures);
      WriteLn('FAIL: ', E.ClassName, ': ', E.Message);
    end;
  end;
  WriteLn(Checks, ' checks; ', Failures, ' failures; ', Skipped, ' limitations/skips.');
  if Failures > 0 then Halt(1);
end.
