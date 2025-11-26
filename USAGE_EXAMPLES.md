# LazHIDControl Usage Examples

## Basic Mouse and Keyboard Automation

```pascal
uses
  LazHIDControl; // This brings in MouseAndKeyInput, HotkeyInput, GlobalKeyMonitor

// Simulate typing
KeyInput.PressString('Hello World!');

// Press individual keys
KeyInput.Press(VK_RETURN);
KeyInput.Down(VK_CONTROL);
KeyInput.Press(VK_C);
KeyInput.Up(VK_CONTROL);

// Move mouse to screen coordinates
MouseInput.Move([], 100, 100, 500); // x, y, duration in ms

// Click at current position
MouseInput.Click(mbLeft, []);

// Click at specific coordinates
MouseInput.Click(mbLeft, [], 200, 200);

// Drag mouse
MouseInput.Down(mbLeft, []);
MouseInput.Move([], 300, 300, 1000);
MouseInput.Up(mbLeft, []);
```

## Global Hotkey Registration

```pascal
uses
  LazHIDControl, LCLType;

var
  MyHotkey: THotkey;

// Register Ctrl+Shift+F9
MyHotkey := THotkey.Create(VK_F9, [ssCtrl, ssShift], @OnHotkeyPressed);
MyHotkey.Register;

// Check if registration succeeded
if MyHotkey.Registered then
  ShowMessage('Hotkey registered!')
else
  ShowMessage('Hotkey not available on this platform');

// Handler
procedure TForm1.OnHotkeyPressed(Sender: TObject; Key: Word; Shift: TShiftState);
begin
  // IMPORTANT: Wait for keys to be released
  Sleep(200);

  // Now safe to simulate input
  KeyInput.PressString('Hotkey triggered!');
end;

// Cleanup
MyHotkey.Unregister;
MyHotkey.Free;
```

## Global Key Monitoring (for testing)

```pascal
uses
  LazHIDControl;

var
  KeyListener: TGlobalKeyListener;

// Create and start monitoring
KeyListener := TGlobalKeyListener.Create;
KeyListener.OnKeyPress := @OnKeyPressed;
KeyListener.StartMonitoring;

// Check if active
if KeyListener.Active then
  ShowMessage('Monitoring system-wide key presses')
else
  ShowMessage('Not supported on this platform');

// Handler
procedure TForm1.OnKeyPressed(Key: Word; Shift: TShiftState);
begin
  // Display key info
  StatusBar1.Panels[0].Text := Format('Key: %d', [Key]);
end;

// Cleanup
KeyListener.StopMonitoring;
KeyListener.Free;
```

## Platform Support

| Feature | Windows | X11 (Linux) | Wayland | macOS |
|---------|---------|-------------|---------|-------|
| Mouse/Keyboard Automation | ✅ | ✅ | ✅* | ⚠️ Untested |
| Global Hotkeys | ✅ | ✅ | ✅** | ❌ |
| Global Key Monitoring | ✅ | ✅ | ❌ | ❌ |

\* Wayland requires elevated permissions or proper user access to input devices
\** Wayland hotkeys work on GNOME 45+ and KDE Plasma 5/6 only (via portal API)

## See Also

- `HOTKEY_USAGE.md` - Detailed hotkey registration documentation
- `README.md` - Project overview and goals
