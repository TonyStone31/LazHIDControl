# Global Hotkey Registration

The LazHIDControl package includes cross-platform global hotkey registration support through the `HotkeyInput` unit.

## Platforms Supported

- **Windows**: Fully supported using low-level keyboard hooks
- **X11 (Linux/Unix)**: Fully supported using XGrabKey
- **Wayland**: Supported on GNOME 45+ and KDE Plasma 5/6 via org.freedesktop.portal.GlobalShortcuts
- **macOS**: Not currently implemented

### Wayland Compositor Support

The Wayland implementation uses the GlobalShortcuts portal via D-Bus:

- ✅ **GNOME 45+**: Full support
- ✅ **KDE Plasma 5/6**: Full support
- ❌ **Sway/wlroots**: Not supported (portal not implemented)
- ❌ **Hyprland**: Not supported (portal not implemented)

**Note**: Wayland hotkeys require user permission dialogs and cannot override compositor-defined hotkeys. This is a security feature of Wayland.

## Basic Usage

```pascal
uses
  HotkeyInput, MouseAndKeyInput, LCLType;

var
  MyHotkey: THotkey;

procedure TForm1.FormCreate(Sender: TObject);
begin
  // Create and register a hotkey (Ctrl+Shift+F9)
  MyHotkey := THotkey.Create(VK_F9, [ssCtrl, ssShift], @MyHotkeyHandler);
  MyHotkey.Register;

  // Check if registration was successful
  if not MyHotkey.Registered then
    ShowMessage('Hotkey registration failed - may not be supported on this platform');
end;

procedure TForm1.FormDestroy(Sender: TObject);
begin
  // Always unregister and free hotkeys
  if Assigned(MyHotkey) then
  begin
    MyHotkey.Unregister;
    MyHotkey.Free;
  end;
end;

procedure TForm1.MyHotkeyHandler(Sender: TObject; Key: Word; Shift: TShiftState);
begin
  // IMPORTANT: Wait for keys to be released before simulating input
  // Otherwise the modifier keys (Ctrl, Shift, etc.) will interfere
  Sleep(200);

  // Now safe to simulate keyboard input
  KeyInput.PressString('Text typed by hotkey!');
end;
```

## Important Notes

### Key Release Timing

When using hotkeys to trigger keyboard simulation, **always add a delay** at the start of your hotkey handler:

```pascal
procedure TForm1.MyHotkeyHandler(Sender: TObject; Key: Word; Shift: TShiftState);
begin
  // Wait for the hotkey combination to be released
  Sleep(200);

  // Now type text - modifiers won't interfere
  KeyInput.PressString('Your text here');
end;
```

Without this delay, the Ctrl/Shift/Alt keys from your hotkey combination may still be pressed when you start simulating keystrokes, causing unexpected behavior.

### Platform Detection

Check the `Registered` property after calling `Register()` to detect if hotkey registration succeeded:

```pascal
MyHotkey.Register;
if MyHotkey.Registered then
  StatusBar1.Panels[0].Text := 'Hotkey active'
else
  StatusBar1.Panels[0].Text := 'Hotkey not available on this platform';
```

### Supported Key Codes

The hotkey system supports:
- Function keys (VK_F1 through VK_F12)
- Letters (VK_A through VK_Z)
- Numbers (VK_0 through VK_9)
- Common keys (VK_SPACE, VK_RETURN, VK_ESCAPE, VK_TAB)

### Modifiers

Supported shift states:
- `ssCtrl` - Control key
- `ssShift` - Shift key
- `ssAlt` - Alt key
- Combinations: `[ssCtrl, ssShift]`, `[ssCtrl, ssAlt]`, etc.

## Common Use Cases

### Text Expansion
```pascal
procedure TForm1.TextExpandHotkey(Sender: TObject; Key: Word; Shift: TShiftState);
begin
  Sleep(200);
  KeyInput.PressString('your@email.com');
end;
```

### Template Insertion
```pascal
procedure TForm1.TemplateHotkey(Sender: TObject; Key: Word; Shift: TShiftState);
begin
  Sleep(200);
  KeyInput.PressString('Dear Sir/Madam,' + LineEnding + LineEnding);
end;
```

### Automation Trigger
```pascal
procedure TForm1.AutomationHotkey(Sender: TObject; Key: Word; Shift: TShiftState);
begin
  Sleep(200);
  // Perform complex mouse and keyboard automation
  MouseInput.Move([], 100, 100, 500);
  MouseInput.Click(mbLeft, []);
  KeyInput.PressString('Automated task');
end;
```

## Attribution

The hotkey implementation is adapted from the [Codebot Pascal Library](http://cross.codebot.org) by Codebot.

## See Also

- Demo application in `project1.lpr` for complete working example
- `MouseAndKeyInput` unit for keyboard and mouse simulation
- `KeyInputIntf` and `MouseInputIntf` for low-level interfaces
