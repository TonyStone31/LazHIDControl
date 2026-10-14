# The LazHIDControl manual

[Back to the overview](README.md)

Install `lazhidcontrol/lazhidcontrol.lpk` with **Use → Install**, then rebuild
and restart Lazarus. Rebuild the IDE after component updates too, so its
installed properties match your forms. The existing library API remains available.

## The library beneath the components

Everything the components do is also callable directly, with no form involved:

```pascal
uses HIDMouseAndKeyInput, LCLType;

KeyInput.PressString('Hello World!');
KeyInput.Press(VK_RETURN);
KeyInput.Down(VK_CONTROL);           // paired down/up for chords
KeyInput.Press(VK_C);
KeyInput.Up(VK_CONTROL);

MouseInput.Move([], 100, 100, 500);  // x, y, optionally milliseconds of travel
MouseInput.Click(mbLeft, []);
MouseInput.Down(mbLeft, []);         // a drag, spelled out
MouseInput.Move([], 300, 300, 1000);
MouseInput.Up(mbLeft, []);
```

Hotkeys without the component go through `HotkeyInput`:

```pascal
uses HotkeyInput, LCLType;

MyHotkey := CreateHotkey(VK_F9, [ssCtrl, ssShift], @OnHotkeyPressed);
MyHotkey.Register;
if not MyHotkey.Registered then ... // taken, or no backend here
...
MyHotkey.Free;                      // releases the binding
```

[lazhidcontrol/HOTKEY_USAGE.md](lazhidcontrol/HOTKEY_USAGE.md) goes deeper on
the hotkey API. Input goes to the focused application or the current pointer
position; nothing selects a target application for you - find and activate
one with `THIDWindow` or the window manager API first.

## THIDHotkey

Drop **THIDHotkey** on a form, choose **ShortCut** in the Object Inspector,
set **Enabled = True**, and assign **OnHotkey**. One component handles one
shortcut; drop another for a second shortcut. To record a combination, click
the **...** button beside **ShortCut**, press the keys in the capture box, and
click **OK**. Lazarus supplies this editor; no global capture is involved.
Settings are saved in the `.lfm`.

- `Registered` reports whether the OS accepted the shortcut.
- `OnError` and `LastError` explain registration failure without showing a dialog.
- Set `Enabled = False` to release it; changing `ShortCut` replaces its binding.
- `RegisterHotkey` / `UnregisterHotkey` are available for explicit control.

The component owns its binding and releases it when destroyed. Constructors
are passive and the designer never grabs keys. Published properties and events
include source comments for the Object Inspector's hints and information box.
Windows, GTK2/GTK3 Linux/X11, and macOS register global shortcuts (macOS
through Carbon's hotkey call, which needs no Accessibility permission). Wayland
hotkeys remain unfinished; unsupported registration reports failure.

The event runs on the GUI thread. Hotkey modifiers may still be held: if you
replay typing, wait for their release first. The automation demo's hotkey
simply presses its own Capture button.

## THIDKeyboard and THIDMouse

Both components delegate to the existing input backends. They perform actions
only when called, not when the form loads. The keyboard uses the currently
focused application; mouse clicks and scrolling use the current pointer position.

```pascal
HIDKeyboard1.TypeText('Hello');
HIDKeyboard1.PressKey(VK_RETURN);
HIDMouse1.MoveTo(100, 100);
HIDMouse1.Click(mbLeft);
```

`TypingInterval` adds milliseconds between characters. `Modifiers` applies
Ctrl, Alt, and Shift to `PressKey`; `TypeText` sends literal text. Existing
UTF-8 handling and line-ending normalization stay in the keyboard backend.
Text entry does not automatically focus a target: the demo explicitly focuses
its output edit first. The demo's countdown button types after a chosen delay
into whatever then holds the focus - the way to hand text to an application
that takes typing but not a paste - and its sample text exercises the Unicode
path, currency to emoji.

`MovementDuration` controls mouse movement timing. Set `RelativeTo` to a form
or control to make `MoveTo` use its client coordinates. For external windows,
set `RelativeWindow` instead. Leave both blank for screen coordinates. `Modifiers` applies Ctrl, Alt, and Shift during mouse actions.
`DoubleClick` is available too; `Scroll` uses positive steps for up, negative for down.

`KeyDown`/`KeyUp` and `ButtonDown`/`ButtonUp` are explicit paired operations for
custom gestures. Pair them in your code, using `try/finally` when needed. The
components do not release arbitrary held keys or buttons on destruction.
Input methods are synchronous; movement and typing can process GUI messages.
They raise an exception for unsupported input or designer calls.

## THIDScreenshot

Choose `Source` in the Object Inspector, then call `Capture`. It returns
`True` on success, with the result in the component-owned `Bitmap`.
`OnCaptured` lets a form update its preview; `OnError` and `LastError` report failure.

| Source | Configuration |
|---|---|
| `hcsDesktop` | Entire desktop across all monitors. |
| `hcsMonitor` | `MonitorIndex = -1` selects the primary monitor; otherwise use a zero-based `Screen.Monitors` index. |
| `hcsWindow` | `WindowTitle` finds a window using the existing window manager. Blank means the active window. |
| `hcsRegion` | `RegionLeft`, `RegionTop`, `RegionWidth`, and `RegionHeight` define a rectangle in screen coordinates. |
| `hcsSelection` | Shows a frozen desktop image for the user to drag a rectangle. Escape or right-click cancels. |

Negative coordinates work for monitors to the left or above the primary;
rectangles are clipped to the desktop. A missing window or invalid monitor
reports failure rather than capturing the wrong target. Window capture reads
the screen, so the target window is **brought to the front first** and given
a moment to paint; minimized or offscreen targets cannot be captured, and
hidden window contents are not reconstructed.

```pascal
if HIDScreenshot1.Capture then
  Image1.Picture.Assign(HIDScreenshot1.Bitmap);
```

The selection is cropped from the frozen image, so its outline and instructions
are not included. Cancel returns `False` with an empty `LastError` and does not
raise `OnError`. `CaptureArea(TRect)` and `CaptureWindow(TWindowHandle)` are
also available for runtime targets. Copy `Bitmap` if retaining a capture after
another capture or after freeing the component. Saving belongs to your app;
the demo shows saving an opaque PNG with the standard LCL image class.

## In the demo

[AutomationDemo.lpi](examples/automation/AutomationDemo.lpi) exercises this:
typing into its own edit, moving to and clicking a sample button, capture
source selection, image preview, and PNG saving. Ctrl+Shift+F9 captures using
the current settings. Rectangle mode uses the screenshot properties saved in
the `.lfm`.

The demos share Ctrl+Shift+F9; run one at a time or change its `ShortCut`.

Published properties and events have source comments for Object Inspector help.
The components and their GUI methods are intended for the main GUI thread.
Windows and Linux/X11 use the existing backends. Hotkeys use GTK2/GTK3 on X11.
Linux/X11 is tested in a nested X server; Windows was tested on Windows 10 in
a virtual machine, with hidctl and the demo.
macOS was tested on macOS Sequoia 15 in a virtual machine: typing, the mouse,
window control, screenshots, hotkeys, and the monitors all work there, once
the user has allowed the program under Privacy & Security - Accessibility for
input and windows, Screen Recording for screenshots of other programs, Input
Monitoring for watching keys. Wayland support is partial - see "Wayland, in
detail". Components report unavailable operations rather than simulating
success.

A Wayland session has no window backend and does not fall back to X11 window
control. On macOS a window's handle is its Core Graphics window number, its
"class" is the application's name, and a minimized window is not listed -
keep its handle to restore it.

## THIDWindow

Set **WindowTitle**, **WindowClass**, and/or **ProcessID** in the Object Inspector.
All supplied filters must match. Blank text and process ID zero accept any value.
**ExactMatch** selects whole-value or partial matching; both ignore case.
**VisibleOnly** defaults to True. Nothing searches or moves windows at startup.

Call `Find` for the first match; `OnFound` receives the component and `Handle`
identifies the result. `FindAll` returns a caller-owned `TList` of handles.
No match returns False with an empty `LastError`; unsupported operations report
`OnError` and `LastError`.

`WaitForWindow` first searches immediately, then checks every 100 milliseconds
using an LCL timer until **WaitTimeout** expires (default 5000 milliseconds).
True means a match was found or a wait started. Handle the result in **OnFound**
or **OnTimeout**; `CancelWait` stops silently. A console program can use `Find`
and its own loop instead of the GUI timer. Search order is determined by the OS.

After finding a window, call `Activate`, `MoveTo`, `Resize`, `Minimize`, `Restore`,
or `GetInfo`. Activation success means the backend accepted the request; focus
restrictions and the window manager can still prevent actual activation. Confirm
with `WindowMgr.GetActiveWindow` before sending input. `THIDMouse.MoveToWindow`
uses coordinates relative to the target's reported screen rectangle, including
any borders reported by the backend; it ignores the mouse's `RelativeTo` control.

The automation demo's **Find and activate window** button uses the title box.
X11 uses the window manager's client list, falling back to direct root children
on bare X servers. macOS uses the Core Graphics window list and the
Accessibility API; native Wayland has no window backend.

## THIDSystemIdle

Drop the component on a form, set **IdleThreshold** in milliseconds (default
60000), assign **OnIdle** and **OnActive**, then set **Enabled** to True.
**PollInterval** defaults to 1000 milliseconds. Both interval and threshold must
be positive. Monitoring starts after loading and never runs in the designer.

Events report state changes, rather than firing every poll. If the first check
finds the session already idle, `OnIdle` fires immediately. `OnActive` fires only
when activity follows idle. Both run on the GUI thread during timer checks.
`IsIdle` and `IdleMilliseconds` hold the latest successful result.

Call `Check` directly for manual polling, including console applications without
a GUI timer; the same events fire on the calling thread. Query failure returns
False, clears the reported state, disables monitoring, and reports **OnError**
and `LastError`. Re-enable to retry. Disabling the component does not fire events.

Windows uses the current session's `GetLastInputInfo`. Linux/X11 queries the
screen-saver extension using `libXss.so.1` (an optional runtime dependency, loaded
only when checking). macOS uses FPC's Core Graphics binding; the idle query
is tested there, the idle/active transitions are not. Backend selection is
automatic: X11 sessions use the X11 query. Wayland sessions, and X11 sessions
whose query fails, ask GNOME's Mutter IdleMonitor, then the compositor itself
through the `ext-idle-notify` protocol (KWin and the wlroots compositors have
it; `libwayland-client.so.0` is loaded for it when needed), then the
freedesktop ScreenSaver service. Tested on KDE Plasma 6; GNOME on Wayland
still needs testing. No XWayland idle fallback is used.

A compositor does not say how long the session has been idle, only that a
time asked for in advance has gone by. So through `ext-idle-notify` the
count starts once a second has passed without input, it can run up to one
**PollInterval** behind, and nothing is known from before the first check.

OS-reported activity
may include synthetic input. This detects session inactivity, not application
inactivity or whether an application is busy.

The automation demo has a checkbox for a five-second threshold. Leave the mouse
and keyboard alone, then move or type to see the two events.

## THIDInputMonitor

Reports every key press and mouse click on the system, not only the ones in
your own windows. It is there for recording what somebody does so it can be
played back with `THIDKeyboard` and `THIDMouse`, and for seeing what a key
really sends.

Set **Enabled** to True in code, or in the Object Inspector to start with the
form. **OnKeyPress** gives the LCL virtual key and the Ctrl, Alt, Shift and
Meta held with it; **OnMouseClick** gives the button and where on the screen.
Both run on the main thread. **WatchKeys** and **WatchClicks** choose what is
watched; set them before enabling. If one of the two cannot start, **OnError**
says which, and `WatchingKeys` and `WatchingClicks` tell what is running.

Windows uses low-level hooks. Linux/X11 uses XInput2 raw events (`libXi.so.6`),
which keep coming while a menu is open. macOS uses an event tap, and no key
arrives until the user allows the program under Privacy & Security > Input
Monitoring. On Wayland the compositor shows a program nothing but its own
input, so the component reads the kernel's input devices instead -
`/dev/input/event*`, which takes the second line of the udev rule in
"Wayland, in detail". That gives every key with its modifiers and every
mouse button, from any number of keyboards and mice, plugged in whenever.
Two things the kernel does not know: the layout, so letters are reported as
a US keyboard has them; and where the pointer is, so `OnMouseClick` arrives
with the button and -1, -1. Without the rule the component says what it
needs.

It sees everything typed, passwords included. Say so in your program when
it is running.

The demo's status bar is this component, behind a checkbox that starts off.

## THIDPowerMonitor

Set **Enabled** to True and assign **OnSuspend**, **OnResume**, and
**OnPowerChanged**. **PollInterval** controls queue processing (default 250
milliseconds). Nothing starts in the designer. No compositor setting is needed.

Read `PowerSource` (`psUnknown`, `psAC`, `psBattery`), `BatteryPercent` (0–100,
-1 for absent/unavailable), and `Charging`. Initial status is available after
enabling without firing a change event. `Refresh` requests fresh status;
`OnPowerChanged` fires only when these values change. Wake events refresh status
before calling `OnResume`. A failure disables monitoring and reports **OnError**
and `LastError`; re-enable to retry.

Windows uses a hidden LCL notification window and `GetSystemPowerStatus`.
Linux uses logind sleep/wake signals and optional UPower status on the system
D-Bus. With logind but no UPower, sleep events still work and status remains
unknown. Both X11 and Wayland use these same Linux services. macOS listens for
`NSWorkspace` sleep/wake notifications - which also cover waking from
hibernation - and reads battery status from IOKit power sources. Power
status is tested on a Mac; battery changes and sleep/wake need real hardware
and are not.

Events run when the application processes them. `OnSuspend` is informational:
it does not delay sleep or guarantee your handler finishes before suspension.
A queued sleep notification may be delivered after wake if the application did
not process it beforehand. Keep event handlers short. No sleep, shutdown, or
inhibitor commands are issued by this component.

The demo checkbox starts monitoring; **Refresh power status** queries manually.
Try disconnecting/reconnecting laptop power, then sleep and wake using your OS.
The log displays sleep/wake notifications. On desktops, no battery reading is
normal. For code-created components, initialize the LCL widgetset and process
its message loop; `Check` can process queued power events manually.

## THIDClipboardMonitor

Enable the component and assign **OnChanged**. It notices clipboard changes
without reading or retaining contents. In your handler, use standard LCL
`Clipboard.HasFormat`, `Clipboard.AsText`, or `Clipboard.GetFormat` as needed.
Enabling establishes a baseline; existing clipboard contents do not fire an
initial event. **PollInterval** defaults to 250 milliseconds, and multiple
changes between checks may produce one event. `Check` supports manual checking.

Windows uses its clipboard sequence counter. GTK2 and GTK3 use the clipboard
owner-change signal; macOS uses `NSPasteboard.changeCount`, tested with
Unicode text.
GTK3 on Wayland is subject to the clipboard visibility provided by GTK and the
compositor; unrestricted background clipboard access is not promised. Other
widgetsets currently report unsupported. This monitors the ordinary clipboard,
not X11 primary-selection text. Identical copied contents can still trigger an
event. **OnError** reports failures and disables monitoring.

In the demo, enable clipboard monitoring and press **Copy demo text**, or copy
text/images from another application. After capturing a screenshot, **Copy
captured image to clipboard** demonstrates non-text clipboard changes.
The label counts notifications; the text
box displays copied text when available.

## Window-relative pointer movement

Set **THIDMouse.RelativeWindow** to a **THIDWindow** component in the Object
Inspector. Call that window's `Find`, then `Mouse.MoveTo(X, Y)`. The window
reference takes precedence over **RelativeTo**. Coordinates use its reported
screen rectangle, including native borders; they are not external child-control
coordinates. The demo has a separate configured mouse component for this, and
its **Move pointer to window offset X, Y** button moves without clicking.

## The demos

One program covers the lot:
**[examples/automation/AutomationDemo.lpi](examples/automation/AutomationDemo.lpi)**.
Every component sits on its form in the designer, a tab for each job:

- **Typing** - a sheet of Unicode text typed into the box beside it and
  compared, a countdown that types it into whatever then has the focus, and
  Ctrl+Shift+F9 to type it into another program from anywhere. Handy for the
  places that will not take a paste.
- **Mouse** - move and click relative to a control, and the scroll wheel.
- **Windows** - list other programs' windows; activate, minimize, restore,
  close; see what is under the pointer; aim the pointer inside one.
- **Screenshot** - every capture mode, with preview, save and copy;
  Ctrl+Shift+F10 captures from anywhere.
- **Idle, power, clipboard** - the three monitors, live.

The status bar shows the pointer and, once you tick the box above it, the
last click and the last key anywhere on the system - `THIDInputMonitor`.

Try the input demos in a nested X server (`Xephyr :9 & DISPLAY=:9 ...`) so a
misaimed script cannot click on your real desktop. That is also how we drive
them with [hidctl](hidctl/README.md).

## Wayland, in detail

**Screenshots** come from the desktop's `org.freedesktop.portal.Screenshot`.
The portal hands over the whole desktop and `THIDScreenshot` cuts the monitor,
region, or dragged selection out of it. The desktop may ask the user for
permission the first time. Capturing a window by title is not possible: a
Wayland program cannot learn where another program's window is.

**Keyboard and mouse** go through the kernel's
[`uinput`](https://docs.kernel.org/input/uinput.html), since there is no XTest
to ask. `waylandmouseandkeyinput.pas` creates a virtual keyboard and a virtual
absolute pointer that any compositor reads like hardware. Tested on KDE Plasma
6: typing, keys with modifiers, moving, clicking and the scroll wheel. It
needs write access to `/dev/uinput`, which belongs to root by default. One
udev rule gives it to whoever is logged in at the machine:

```
# /etc/udev/rules.d/60-lazhidcontrol.rules
KERNEL=="uinput", SUBSYSTEM=="misc", TAG+="uaccess", OPTIONS+="static_node=uinput"
SUBSYSTEM=="input", KERNEL=="event*", TAG+="uaccess"
```

The first line is for typing and the mouse, the second for
`THIDInputMonitor`, which reads what the keyboard and mouse do. Reload with
`sudo udevadm control --reload && sudo udevadm trigger`. Be clear with
whoever installs it about what it is: the access goes to the person sitting
at the machine, for every program they run, not to one program - Linux has
no narrower grant for a device outside a Flatpak. Deleting the file and
reloading takes it back from then on; a program that already has a device
open keeps it until it exits. Without access, `THIDKeyboard` and `THIDMouse`
raise an error that says so. A virtual
keyboard presses keys, not characters: text is typed as a US layout would
type it, and the lock keys cannot be read.

**Unicode text** is typed through the desktop's
`org.freedesktop.portal.RemoteDesktop`, whose `NotifyKeyboardKeysym` takes
any character and does not care about the layout. The desktop asks the user
to allow it - on KDE, "Control input devices" with "Allow restoring on
future sessions" ticked - and gives back a token that stands for the
answer. The token is kept in the application's configuration directory
(`GetAppConfigDir`, file `wayland-typing.token`), so the question comes once
per application, not once per run; delete the file to be asked again. The
portal is left alone until `TypeText` meets a character the virtual keyboard
cannot press; from then on all typed text goes that way, which keeps it in
order and right on any layout. If the user says no, or the desktop has no
such portal, characters beyond ASCII are skipped as before. Tested on KDE
Plasma 6 with accents, Greek, Cyrillic, Chinese and emoji, asked once across
runs; GNOME has the same portal and has not been tried.

The pointer goes where it is told in screen coordinates. A `THIDMouse` with
`RelativeTo` or `RelativeWindow` set raises an error on Wayland instead of
guessing: a Wayland program is not told where its own windows are on the
screen, so there is nothing to measure from.

**Global hotkeys** want the `GlobalShortcuts` desktop portal (KDE Plasma,
GNOME 48 and later, Hyprland); that unit is still a stub and registration
fails with a message. It will take more than code in a unit: asked by an
ordinary program, KDE's portal answers "an app id is required". The program
has to give its id to `org.freedesktop.host.portal.Registry` before any
other portal call, and that id has to be the name of an installed `.desktop`
file. With both in place KDE shows the user the shortcut and it fires -
tried on Plasma 6 with a script - so the unit can be written for programs
that install a desktop entry. **Window management** has no cross-compositor protocol
to use, so `THIDWindow` reports that it is unavailable. Power status and
clipboard monitoring work as described in their sections. Idle time comes
from the compositor on KDE; the `THIDSystemIdle` section has the detail.
`THIDInputMonitor` reads the kernel's input devices, as its section says.

A GTK2 program on a Wayland desktop runs through XWayland, the X server
Wayland sessions carry for older programs. X calls made there reach only
other XWayland programs - native Wayland windows are invisible to them - which
is why this package does not fall back to its X11 backends on Wayland.

## Possible future work

- Session lock/unlock notifications.
- Hotkeys on Wayland through the GlobalShortcuts portal, for programs with an installed desktop entry.
- macOS on Apple Silicon, and its battery and sleep/wake events, on a real Mac.

These are future options, not implemented features or release commitments.
