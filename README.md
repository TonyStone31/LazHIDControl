# LazHIDControl

Desktop automation for Lazarus / Free Pascal: type, click, scroll, register
global hotkeys, find and control other programs' windows, take screenshots,
and watch the clipboard, the power supply, and session idle time. Eight
nonvisual components to drop on a form, the library underneath them, and a
command-line tool that drives any program from a script.

Three pieces, three directories:

| | |
|---|---|
| [`hidctl/`](hidctl/) | The command-line automation tool. Does the work people reach for xdotool to do, in one binary, on an API you can also call from Pascal. |
| [`lazhidcontrol/`](lazhidcontrol/) | The package: components, input backends, window management. |
| [`examples/`](examples/) | Two demos: the component workout, and the original library demo. |

## The components

| Component | What it does |
|---|---|
| `THIDHotkey` | Registers a global shortcut, configured in the Object Inspector; fires `OnHotkey` from any application. |
| `THIDKeyboard` | Types text - Unicode included - and sends keys with modifiers to the focused application. |
| `THIDMouse` | Moves, clicks, and scrolls; coordinates can be screen, control, or window relative. |
| `THIDWindow` | Finds windows by title, class, or process; waits for them, activates, moves, resizes. |
| `THIDScreenshot` | Captures the desktop, a monitor, a window, a rectangle, or a drag selection. |
| `THIDClipboardMonitor` | Fires `OnChanged` when anything lands on the clipboard, without reading it. |
| `THIDPowerMonitor` | Sleep and wake notifications, power source, battery percentage, charging. |
| `THIDSystemIdle` | Fires `OnIdle` and `OnActive` as session input stops and resumes. |

Every component has palette icons, Object Inspector hints on every property,
an `OnError` event instead of surprise dialogs, and does nothing in the
designer or at form load beyond what you enabled.

[MANUAL.md](MANUAL.md) covers each component in detail, the library beneath
them, and the per-platform notes.

## Install

Open `lazhidcontrol/lazhidcontrol.lpk` in Lazarus, **Compile**, then
**Use → Install** to put the components on the palette. For library-only use,
**Use → Add to Project** is enough. From a shell:

```sh
lazbuild --add-package-link lazhidcontrol/lazhidcontrol.lpk
lazbuild examples/automation/AutomationDemo.lpi
lazbuild hidctl/hidctl.lpi
```

Dependencies are `LCL` and `FCL`. Linux also wants the X11, Xtst and D-Bus
libraries that every desktop already has.

## A taste

```pascal
// Drop a THIDWindow and a THIDMouse on a form, or create them in code:
if Window.Find then            // WindowTitle set in the Object Inspector
begin
  Window.Activate;
  Mouse.RelativeWindow := Window;
  Mouse.MoveTo(200, 150);      // 200,150 inside that window, wherever it is
  Mouse.Click;
end;
```

Or skip the components and use the library directly:

```pascal
uses HIDMouseAndKeyInput, LCLType;
KeyInput.PressString('Hello World!');
KeyInput.Press(VK_RETURN);
MouseInput.Move([], 100, 100, 500);  // walk there over half a second
MouseInput.Click(mbLeft, []);
```

## hidctl

Everything above, from a shell - on Windows as well as Linux. It answers to
xdotool's command names, chained on one line the way xdotool takes them:

```sh
hidctl search --name "Text Editor" windowactivate key ctrl+a type "Hello" key Return
```

It also runs scripts, one command per line from a file or standard input, in
a vocabulary of its own that works in coordinates relative to the target
window - so a test does not break because a window opened two pixels lower
today.

```sh
./hidctl/hidctl --window "Some App" - <<EOF
waitfor 5000 Some App
at 300 200
click
type Hello from a script
shot /tmp/proof.png
power
EOF
```

It covers the pointer, the keyboard, window find/wait/activate/geometry,
screenshots of the window, the desktop, or an area, the clipboard, idle time,
and power status. `hidctl --help` prints every command;
[hidctl/README.md](hidctl/README.md) says which of xdotool's it covers. We use it, against a nested X server, to drive the GUI tests of our
other projects.

## What is tested, what is not

| Platform | Status | Notes |
|---|---|---|
| Linux / X11 (gtk2, gtk3) | **Tested.** | All eight components and hidctl are exercised in a nested X server on every change: typing with Unicode, clicking, capture, window control, clipboard, idle, power. Typing is checked on US, German, French, Spanish, and Russian keyboard layouts and with Caps Lock on. Qt builds compile; hotkeys and clipboard monitoring need GTK for now. |
| Windows | **Tested on Windows 10 (22H2) in a virtual machine.** | hidctl and both demos pass there: typing the full Unicode sheet (also with Caps Lock and NumLock on), navigation keys, clicking, window find/move/size/minimize with Unicode titles, window and desktop capture, the global hotkey from another application, clipboard monitoring, idle, and power status. Not covered: suspend/resume events, more than one monitor, Windows 11. |
| Linux / Wayland | **Partly there, untested on a real compositor.** | Screenshots go through the desktop's screenshot portal (tested against that portal on an X11 session; capturing one window by title is not possible). Idle (GNOME and KDE services), power (logind/UPower), and clipboard monitoring ride D-Bus and GTK, not X. Keyboard and mouse use the kernel's uinput: written, compiled, **never run** - it needs write access to `/dev/uinput`, types as a US layout, and cannot type beyond ASCII. Global hotkeys (GlobalShortcuts portal) are not written. Finding and moving other applications' windows is the one thing Wayland itself rules out. |
| macOS / Cocoa | **Partly there. Tested on macOS Sequoia 15 (Intel) in a virtual machine.** | Built with the released Lazarus 4.8 and FPC 3.2.4; both demos and hidctl run. Working and tested: global hotkeys (registered, then fired by a real Ctrl+Shift+F9), clipboard monitoring with Unicode text, idle time, power status, and hidctl's idle, power, display, and clipboard commands. Sending keys and mouse moves is written but has not been run against another application - it needs Accessibility permission. Not there yet: typing beyond the keys on the keyboard, the scroll wheel, window control, screenshots. Battery changes and sleep/wake cannot be tried in a virtual machine. `sh tests/run_macos.sh` runs the checks. |

**Wayland and macOS developers: we want your feedback.** If you can run the
automation demo on a real Wayland desktop or a Mac - or can tell us what the
portal and Cocoa backends should look like - open an
[issue](https://github.com/TonyStone31/LazHIDControl/issues). Those two
columns improve only with reports from machines we do not have.

The automation demo and the library demo on macOS:

![The automation demo on macOS](images/macos/automation-demo.png)

![The library demo on macOS, its hotkey registered](images/macos/hid-demo-keyboard.png)

## License and credits

**GPL-2.0-or-later**; [COPYING.GPL2](COPYING.GPL2) holds version 2 and
[LICENSE](LICENSE) version 3, an allowed later version.

Original input code by Tom Gregorovic, Cocoa input by Sammarco Francesco,
hotkey code adapted from Codebot. Components, window management, hidctl, and
everything since: Tony Stone. Source notices remain in place.
