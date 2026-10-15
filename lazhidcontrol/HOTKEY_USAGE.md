# Global Hotkey Registration

The LazHIDControl package includes cross-platform global hotkey registration support through the `HotkeyInput` unit.

## Platforms Supported

- **Windows**: Implemented using RegisterHotKey
- **X11 (Linux/Unix)**: XGrabKey on the package's own X connection, read on the main thread; no GTK hook
- **Wayland**: Not implemented. The GlobalShortcuts portal refuses a program that has no installed desktop entry ("an app id is required")
- **macOS**: Implemented using Carbon's RegisterEventHotKey (Ctrl, Alt/Option, Shift)

### Wayland status

`TWaylandHotkey` registers nothing and leaves `Registered` false. The
GlobalShortcuts portal it should talk to exists on KDE Plasma, GNOME 48 and
later, and Hyprland; that work is not done.

## Basic Usage

```pascal
uses
  HotkeyInput, HIDMouseAndKeyInput, LCLType;

var
  MyHotkey: THotkey;

procedure TForm1.FormCreate(Sender: TObject);
begin
  // Create and register a hotkey (Ctrl+Shift+F9)
  MyHotkey := CreateHotkey(VK_F9, [ssCtrl, ssShift], @MyHotkeyHandler);
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

  // Replay should wait for the hotkey modifiers to be released.
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

`Register` raises `EHotkeyError` when the system refuses the combination;
the message says why (another program holds it, the key is not on this
keyboard, a modifier other than Ctrl, Alt or Shift). Where there is no
backend at all, as on Wayland, it raises nothing and `Registered` stays
false:

```pascal
try
  MyHotkey.Register;
except
  on E: EHotkeyError do
    StatusBar1.Panels[0].Text := E.Message;
end;
if MyHotkey.Registered then
  StatusBar1.Panels[0].Text := 'Hotkey active';
```

`THIDHotkey` catches the exception and reports it through `OnError`.

### Supported Key Codes

Any key the keyboard has: letters, digits, F1 to F24, the number pad, the
navigation keys and the punctuation keys. Register hotkeys from the main
thread; handlers run there too, queued to the message loop.

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

The hotkey units began as an adaptation of the [Codebot Pascal Library](http://cross.codebot.org)
by Anthony Walter. They were written new in October 2026 from a specification so
the package could be offered under a less restrictive license.

## See Also

- The demo in `../examples/automation/AutomationDemo.lpi`, which registers two hotkeys
- `HIDMouseAndKeyInput` unit for keyboard and mouse simulation
- `HIDKeyInputIntf` and `HIDMouseInputIntf` for low-level interfaces
