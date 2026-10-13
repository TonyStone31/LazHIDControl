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
| [`examples/`](examples/) | The demo: every component at work, a tab for each job. |

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

Everything above, from a shell - on Linux, Windows and macOS. It answers to
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

| | Linux / X11 | Windows | macOS | Linux / Wayland |
|---|---|---|---|---|
| Tested on | Mint, in a nested X server; GTK2 and GTK3 | Windows 10, in a virtual machine | macOS 15 on Intel, in a virtual machine; Lazarus 4.8, FPC 3.2.4 | KDE Plasma 6 on Ubuntu 26.04, in a virtual machine; GTK3 |
| Typing, Unicode included | ✅ on US, German, French, Spanish and Russian layouts | ✅ | ✅ | ASCII only, as a US keyboard types it |
| Keys with Ctrl, Alt, Shift | ✅ | ✅ | ✅ | ✅ |
| Mouse: move, click, scroll | ✅ | ✅ | ✅ | ✅ in screen coordinates |
| Global hotkeys | ✅ | ✅ | ✅ | not written; the portal serves only installed applications |
| Windows: find, activate, move, size, minimize | ✅ | ✅ | ✅ | not possible on Wayland |
| Screenshots | ✅ | ✅ | ✅ | ✅ through the desktop's portal; it asks the user once |
| Clipboard monitoring | ✅ | ✅ | ✅ | ✅ |
| Idle time | ✅ | ✅ | ✅ | not on KDE; GNOME untested |
| Power status | ✅ | ✅ | ✅ | ✅ |
| Sleep and wake events | ✅ | not tried | not tried | not tried |
| Watching keys and clicks system-wide, for recording a task to replay | ✅ | ✅ | ✅ | not possible on Wayland |
| hidctl | ✅ | ✅ | ✅ | ✅ for what the rows above allow |

Qt builds compile; hotkeys and clipboard monitoring need GTK for now.

Watching keys and clicks is not one of the palette components. It is the
`GlobalKeyMonitor` and `GlobalMouseMonitor` units, used from code; the demo
shows how, behind a checkbox that starts switched off.

**On a Mac the user has to allow it.** macOS asks before a program may do
any of this to other programs, under System Settings > Privacy & Security:
*Accessibility* for typing, the mouse and moving windows, *Input Monitoring*
for watching keys, *Screen Recording* for screenshots that show other
programs' windows. Hotkeys, the clipboard, idle time and power need nothing.
Without a permission the call fails or comes back empty; nothing crashes.
One Mac difference: a minimized window is not in the window list, so keep
its handle if you mean to bring it back.

**On Wayland** keyboard and mouse go through the kernel's uinput, which
needs one udev rule to let the logged-in user write to `/dev/uinput`
([the manual](MANUAL.md#wayland-in-detail) has it). A virtual keyboard
presses keys, not characters, so text arrives as a US layout would type it
and anything beyond ASCII is skipped. Wayland does not tell a program where
windows are, its own included, so the mouse works in screen coordinates only
and other programs' windows are out of reach. Screenshots come from the
desktop's portal. GNOME and other desktops have not been tried;
[tell us how it goes](https://github.com/TonyStone31/LazHIDControl/issues).

**Still wanted from a real Mac:** Apple Silicon, battery changes, and
sleep/wake events - a virtual machine has none of them. `sh tests/run_macos.sh`
runs the checks.

The demo on macOS, listing the Mac's windows and showing its typing test sheet:

![The demo's Windows tab on macOS](images/macos/demo-windows.png)

![The demo's Typing tab on macOS](images/macos/demo-typing.png)

## License and credits

**GPL-2.0-or-later**; [COPYING.GPL2](COPYING.GPL2) holds version 2 and
[LICENSE](LICENSE) version 3, an allowed later version.

Original input code by Tom Gregorovic, Cocoa input by Sammarco Francesco,
hotkey code adapted from Codebot. Components, window management, hidctl, and
everything since: Tony Stone. Source notices remain in place.
