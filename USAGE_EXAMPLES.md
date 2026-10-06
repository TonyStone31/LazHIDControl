# LazHIDControl Usage Examples

## Basic Mouse and Keyboard Automation

```pascal
uses
  MouseAndKeyInput, LCLType;

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
  HotkeyInput, MouseAndKeyInput, LCLType;

var
  MyHotkey: THotkey;

// Register Ctrl+Shift+F9
MyHotkey := CreateHotkey(VK_F9, [ssCtrl, ssShift], @OnHotkeyPressed);
MyHotkey.Register;

// Check if registration succeeded
if MyHotkey.Registered then
  ShowMessage('Hotkey registered!')
else
  ShowMessage('Hotkey not available on this platform');

// Handler
procedure TForm1.OnHotkeyPressed(Sender: TObject; Key: Word; Shift: TShiftState);
begin
  // A delay does not guarantee that the hotkey modifiers were released.
  // Arrange replay after release, as in the main demo.
  Sleep(200);

  // Simulate input
  KeyInput.PressString('Hotkey triggered!');
end;

// Cleanup
MyHotkey.Unregister;
MyHotkey.Free;
```

## Platform support

See the [platform status and Wayland limitations](README.md#platform-status)
in the README. The Wayland input implementation is unfinished; device
permissions alone do not make it functional.

## See Also

- `lazhidcontrol/HOTKEY_USAGE.md` - Detailed hotkey registration documentation
- `README.md` - Project overview and goals
