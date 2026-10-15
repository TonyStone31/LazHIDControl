# LazHIDControl

Desktop automation for Lazarus / Free Pascal: type, click, scroll, register
global hotkeys, find and control other programs' windows, take screenshots,
and watch the clipboard, the power supply, and session idle time. Ten
nonvisual components to drop on a form, the library underneath them, and a
command-line tool that drives any program from a script.

The repository has three parts:

| | |
|---|---|
| [`hidctl/`](hidctl/) | The command-line automation tool. Covers most of what xdotool does, in one binary, using the same API you can call from Pascal. |
| [`lazhidcontrol/`](lazhidcontrol/) | The package: components, input backends, window management. |
| [`examples/`](examples/) | The demo program, with a tab for each area. |

## The components

| Component | What it does |
|---|---|
| `THIDHotkey` | Registers a global shortcut, configured in the Object Inspector; fires `OnHotkey` from any application. |
| `THIDKeyboard` | Types text - Unicode included - and sends keys with modifiers to the focused application. |
| `THIDMouse` | Moves, clicks, and scrolls; coordinates can be screen, control, or window relative. |
| `THIDWindow` | Finds windows by title, class, or process; waits for them, activates, moves, resizes. |
| `THIDScreenshot` | Captures the desktop, a monitor, a window, a rectangle, or a drag selection. |
| `THIDClipboardMonitor` | Fires `OnChanged` when the clipboard content changes, without reading it. |
| `THIDPowerMonitor` | Sleep and wake notifications, power source, battery percentage, charging. |
| `THIDSystemIdle` | Fires `OnIdle` and `OnActive` as session input stops and resumes. |
| `THIDInputMonitor` | Reports key presses and mouse clicks anywhere on the system, for recording a task to play back. |
| `THIDPermission` | Explains which permissions this desktop needs and requests them: the udev rule on Wayland through the system's password dialog, the Privacy & Security prompts on macOS. |

Every component has palette icons and Object Inspector hints on every
property, reports errors through `OnError` rather than dialogs, and does
nothing in the designer or at form load beyond what you enabled.

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

Dependencies are `LCL` and `FCL`. Linux also needs the X11, Xtst and D-Bus
libraries, which every desktop has.

## Example

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
MouseInput.Move([], 100, 100, 500);  // move there over 500 ms
MouseInput.Click(mbLeft, []);
```

## hidctl

The same functions from a shell, on Linux, Windows and macOS. It accepts
xdotool's command names, chained on one line as xdotool does:

```sh
hidctl search --name "Text Editor" windowactivate key ctrl+a type "Hello" key Return
```

It also runs scripts, one command per line from a file or standard input,
with its own commands that use coordinates relative to the target window, so
a script still works when the window opens somewhere else.

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
[hidctl/README.md](hidctl/README.md) lists which xdotool commands it supports. We use it, against a nested X server, to drive the GUI tests of our
other projects.

## What is tested, what is not

| | Linux / X11 | Windows | macOS | Linux / Wayland |
|---|---|---|---|---|
| Tested on | Mint, in a nested X server; GTK2 and GTK3 | Windows 10, in a virtual machine | macOS 15 on Intel, in a virtual machine; Lazarus 4.8, FPC 3.2.4 | KDE Plasma 6 on Ubuntu 26.04, in a virtual machine; GTK3 |
| Typing, Unicode included | ✅ on US, German, French, Spanish and Russian layouts | ✅ | ✅ | ✅ on KDE; the desktop asks the user once when the text goes beyond ASCII |
| Keys with Ctrl, Alt, Shift | ✅ | ✅ | ✅ | ✅ |
| Mouse: move, click, scroll | ✅ | ✅ | ✅ | ✅ in screen coordinates |
| Back and forward mouse buttons | ✅ sent; a GTK3 Lazarus program does not receive them, a fault in the LCL | ✅ | not passed on in the virtual machine | not tried |
| Global hotkeys | ✅ | ✅ | ✅ | not written; the portal serves only installed applications |
| Windows: find, activate, move, size, minimize | ✅ | ✅ | ✅ | not possible on Wayland |
| Screenshots | ✅ | ✅ | ✅ | ✅ through the desktop's portal; it asks the user once |
| Clipboard monitoring | ✅ | ✅ | ✅ | ✅ |
| Idle time | ✅ | ✅ | ✅ | ✅ on KDE; GNOME untested |
| Power status | ✅ | ✅ | ✅ | ✅ |
| Sleep and wake events | ✅ | not tried | not tried | not tried |
| Watching keys and clicks system-wide, for recording a task to replay | ✅ | ✅ | ✅ | ✅ keys and buttons from the kernel's input devices, once `THIDPermission` has installed its rule; no click positions |
| hidctl | ✅ | ✅ | ✅ | ✅ for what the rows above allow |

Qt builds compile. Clipboard monitoring needs GTK. The X11 hotkey backend no
longer does, but `THIDHotkey` enables it only on GTK2 and GTK3 until a Qt
build has been tested.

`tests/inputcheck` types into its own window and clicks, drags and scrolls on
its own controls, then checks what arrived (29 checks); `tests/hotkeycheck`
registers global hotkeys and presses them (23). Both take over the keyboard
and mouse, so run them in a nested X server or a virtual machine.

**On a Mac the user has to allow it.** macOS asks before a program may do
any of this to other programs, under System Settings > Privacy & Security:
*Accessibility* for typing, the mouse and moving windows, *Input Monitoring*
for watching keys, *Screen Recording* for screenshots that show other
programs' windows. Hotkeys, the clipboard, idle time and power need nothing.
Without a permission the call fails or comes back empty; nothing crashes.
One Mac difference: a minimized window is not in the window list, so keep
its handle if you mean to bring it back.

**On Wayland the user grants access once**, through a prompt the package
starts. Which prompt depends on what the program does:

- *Typing and the mouse only:* the desktop's own "may this program control
  input devices" question, through its RemoteDesktop portal, the first time
  input is sent. No password, no system file, and the desktop remembers the
  answer for that program. `THIDKeyboard.PermissionNote` is shown just
  before it, to say why. Desktops without that portal - the wlroots ones,
  such as Sway - take the password route below instead.
- *Watching keys and clicks as well:* there is no portal for watching, so
  that takes a small udev rule, and the same password installs a second
  rule that lets typing and the mouse go through the kernel's virtual
  devices, so the portal prompt is not used. Drop
  `THIDPermission` on the form with both needs and call `Ask`: it shows the
  user what is being allowed and the exact files, then the system's own
  password dialog installs them. The manual has the rules for anyone who
  would rather write them by hand.

One exception: a virtual keyboard presses keys, not characters, so text
beyond ASCII still goes through the desktop's portal prompt the first time. Set `THIDKeyboard.WaylandUnicode` off and it never does;
`TypeText` refuses such text instead.

Text sent through the virtual keyboard is typed with a US layout. Wayland
does not tell a program where windows are, its own included, so the mouse
works in screen coordinates only and other programs' windows cannot be
targeted. Screenshots come from the desktop's portal. KDE Plasma 6 is
what was tested; GNOME and other desktops have not been, and
[reports are welcome](https://github.com/TonyStone31/LazHIDControl/issues).

**Still wanted on Wayland: where the pointer is.** No Wayland desktop tells
a program that, so `THIDInputMonitor` does not watch clicks there - its
`OnError` says why - and a recorder cannot say where anything was clicked. Per-desktop workarounds exist, and this package
does not use them: it has to work the same everywhere with nothing extra to
install. The only option that does is the desktop's screen-sharing portal,
which can report the cursor alongside the screen - [TODO.md](TODO.md) has
the plan, the costs and a first step, for anyone who wants to take it on.

**Still wanted from a real Mac:** Apple Silicon, battery changes, and
sleep/wake events - a virtual machine has none of them. `sh tests/run_macos.sh`
runs the checks.

The demo on macOS, listing the Mac's windows and showing its typing test sheet:

![The demo's Windows tab on macOS](images/macos/demo-windows.png)

![The demo's Typing tab on macOS](images/macos/demo-typing.png)

## Where it started

LazHIDControl began from LazMouseAndKeyInput, the package Tom Gregorovic
wrote in 2008 that ships with Lazarus in `components/mouseandkeyinput`,
with its Cocoa units by Sammarco Francesco. Global hotkeys adapted from the
Codebot library by Anthony Walter were added to it. Much of what this
package knows about sending input on each system was learned from reading
their code.

It has come a long way since. What began as typing and clicking is now ten
components, window control, screenshots, clipboard, power and idle
monitoring, watching keys and clicks system-wide, Wayland through the
desktop's portals and the kernel's input devices, and hidctl.

In October 2026 the last of the borrowed code was replaced. The keyboard,
mouse and hotkey units were written new from a written description of what
they must do, by a writer who had not seen the earlier code. That was not
because the old code was bad - it worked, and this package rests on what it
taught. It was because that code was under the GPL, and this package is
meant to be usable in any program. On the package's own tests the new units
do at least as well as the old ones on every platform.

Components, window management, hidctl, and everything since: Tony Stone.

## License

[0BSD](LICENSE): use it in anything, open or closed, with or without
credit. Releases before October 2026 were GPL-2.0-or-later, and copies of
those stay under it.
