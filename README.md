# LazHIDControl

Mouse and keyboard automation for Lazarus / Free Pascal, with global hotkeys
and window management. The working backends target Windows and Linux/X11.
Wayland support is unfinished; macOS is not a supported target for this release.

Package version: **1.0.0.0**. Use the library directly or install its eight
nonvisual components on the **LazHIDControl** palette.

## Install and build

Open `lazhidcontrol/lazhidcontrol.lpk` in Lazarus, click **Compile**, then
**Use → Install** to add the components to the palette and rebuild Lazarus.
For library-only use, choose **Use → Add to Project** instead.
Open `examples/hotkey/HotkeyDemo.lpi` for the small component demo, or
`HIDControlDemo.lpi` for the existing automation demo.
Keep the repository layout intact; the package belongs in its subdirectory.

From the repository root, with Lazarus tools on your PATH:

```sh
lazbuild --add-package-link lazhidcontrol/lazhidcontrol.lpk
lazbuild --ws=gtk3 HIDControlDemo.lpi
lazbuild --ws=gtk3 lazhidcontrol/example/project1.lpi
lazbuild --ws=gtk3 hidctl/hidctl.lpi
```

Package dependencies are `LCL` and `FCL` (declared FCL minimum: 1.0.0.0).
GTK3 is the Linux target for the component demos. Linux builds also need the FPC X11 and D-Bus units and linkable X11, Xtst and
D-Bus libraries, plus the usual libraries for your LCL widgetset. The demo's
X11 event support also loads `libXi` at runtime.

The package and all three projects compiled on Linux x86-64 with GTK3,
Lazarus 4.99 and FPC 3.3.1 during packaging review. That is a build check;
it does not establish runtime support on every platform or older IDE versions.
See [OPM.md](OPM.md) for submission notes.

## Drop-on-form components

| Component | Quick setup |
|---|---|
| `THIDHotkey` | Record a shortcut in the Object Inspector, enable it, and assign `OnHotkey`. |
| `THIDKeyboard` | Set typing timing or modifiers, then call `TypeText` or `PressKey`. |
| `THIDMouse` | Configure movement timing and a relative control or external window, then move, click, or scroll. |
| `THIDPowerMonitor` | Enable sleep/wake events and watch power source, battery level, and charging. |
| `THIDClipboardMonitor` | Enable clipboard-change events; use LCL to read the copied data. |
| `THIDIdle` | Set an inactivity threshold and handle `OnIdle` / `OnActive`. |
| `THIDWindow` | Find a window by title, class, or process; wait for it, activate, move, or resize. |
| `THIDScreenshot` | Choose desktop, monitor, window, rectangle, or drag selection; call `Capture`. |

[Component guide and Object Inspector settings](COMPONENTS.md) ·
[Small hotkey demo](examples/hotkey/HotkeyDemo.lpi) ·
[Automation and capture demo](examples/automation/AutomationDemo.lpi)

## Using it

Use `MouseAndKeyInput` for the shared `KeyInput` and `MouseInput` objects.
For example, inside a form event handler:

```pascal
// uses MouseAndKeyInput, Controls, LCLType;
KeyInput.PressString('Hello World!');
KeyInput.Press(VK_RETURN);
MouseInput.Move([], 100, 100, 500); // screen coordinates, duration in ms
MouseInput.Click(mbLeft, []);
```

Input goes to the focused application or current pointer location. These
calls do not select a target application for you.

[USAGE_EXAMPLES.md](USAGE_EXAMPLES.md) covers mouse/keyboard calls and hotkey
registration. [HOTKEY_USAGE.md](lazhidcontrol/HOTKEY_USAGE.md) describes the
hotkey API. Check `Registered` after registering a hotkey; the combination
may already be in use.

## Examples and manual tests

There is no automated test suite in this repository. The included projects
are examples and manual checks:

- **`HIDControlDemo.lpi`**: keyboard replay, mouse movement/drawing, and window
  inspection. The keyboard tab compares the source and destination text.
  On startup the demo attempts to register **Ctrl+Shift+F9** to replay the
  source text into the focused application. Use a disposable editor window
  when trying it. The mouse demo moves and clicks the pointer. Window Spy
  exercises window lookup, focus, geometry and screenshots. The stay-on-top
  form is a manual window behavior check.
- **`lazhidcontrol/example/project1.lpi`**: a smaller example with buttons
  that click grid cells, move/double-click, type `HELLO` into an edit control,
  and scroll the grid. Useful for checking the basic input API without the
  larger demo.
- **`hidctl/hidctl.lpi`**: command-line mouse, keyboard and window automation,
  including screenshots. It runs a script against a selected window. See
  [hidctl/README.md](hidctl/README.md) for the script commands.

After building the CLI, run it from the repository root:

```sh
./hidctl/hidctl --window "Some App" script.txt
./hidctl/hidctl --window "Some App" -  # script from standard input
```

Try input tests in a separate desktop session or nested X server where possible;
mouse movement and typing affect whichever application receives the events.
Compilation alone does not verify those interactions.

## Platform status

| Platform | Mouse / keyboard input | Hotkeys | Window management |
| --- | --- | --- | --- |
| Windows | Windows API backend | `RegisterHotKey` | Windows API backend |
| Linux / X11 | XTest backend | `XGrabKey` | X11 backend |
| Linux / Wayland | Unfinished `uinput` backend | Portal stub; registration fails | No native backend |
| macOS | Existing Carbon/Cocoa code; unverified dispatch | Stub | No backend |

Windows and X11 implementations are present, but Windows was not tested during
this review. macOS needs implementation work and testing before claiming support.

## Wayland: what is there now

The intended approach is to emulate a keyboard and mouse using Linux
[`uinput`](https://docs.kernel.org/input/uinput.html). A program creates virtual
input devices through `/dev/uinput`; the compositor can then receive their events
as device input. This differs from sending XTest events to an X server.

The current `waylandmouseandkeyinput.pas` contains event-writing code, but does
not open `/dev/uinput`, configure device capabilities, or create/destroy the
virtual devices. Its keyboard and mouse file descriptors are never assigned.
The event value field, key-code translation and relative mouse movement also
need correction before this backend can be considered usable. Giving the app
permissions alone does not complete it. Earlier Wayland testing may have used
a different revision or an X11 path; this checkout does not establish which.

On Linux the input factory chooses this backend whenever `WAYLAND_DISPLAY` is
set, even if the Lazarus application itself runs under Xwayland. An Xwayland
window therefore does not automatically make this package's input work. The
hotkey portal implementation is also a stub, and window management remains X11
only; it cannot manage native Wayland windows.

The setup remembered from earlier testing was likely a **group membership and
udev rule**, rather than creating a new login user. A finished `uinput` backend
would need permission to access `/dev/uinput`. A dedicated group restricted to
that device is preferable to joining the general `input` group, which can also
grant access to physical input events. Do not treat running the demo as root or
changing device permissions as a fix for the missing implementation.

## License and authors

Source notices specify **GPL-2.0-or-later**. [COPYING.GPL2](COPYING.GPL2) contains
GPL version 2; [LICENSE](LICENSE) contains GPL version 3, an allowed later version.
These retained input implementations are not covered by LazInk's 0BSD license.

Original input code: Tom Gregorovic. Cocoa input: Sammarco Francesco.
Hotkey implementation adapted from Codebot. Package extensions: Tony Stone.
The corresponding source notices remain in place.

Bugs and testing results: [GitHub issues](https://github.com/TonyStone31/LazHIDControl/issues).
