# To do

Open work, for anyone who wants to pick it up. Ask in an issue first if it
is big, so two people do not build the same thing.

## Where the pointer is, on Wayland

**The problem.** A Wayland desktop never tells a program where the pointer
is, and no permission the user can grant changes that. The kernel only
reports that the mouse moved by so much; the desktop alone knows where it
ended up. So `THIDInputMonitor` does not watch clicks there - `OnError`
says why - and a recorder cannot capture where anything was clicked.
Playing clicks back is fine - that already works to the pixel.

**What other tools do.** Mostly nothing: ydotool, dotool and wtype only play
back; xdotool and its relatives are X11 tools. The rest ask one desktop its
own way - a KWin script (kdotool), Hyprland's and Sway's own commands, a
GNOME Shell extension - and break on the next desktop. That is the road this
package does not take: everything here has to work across desktops and
platforms, with nothing for the user to install.

**The approach.** The desktop's own `org.freedesktop.portal.ScreenCast`, the
one screen recorders use, can send the cursor position alongside the screen
as metadata (`cursor_mode` 4, Metadata). KDE Plasma 6 offers it
(`AvailableCursorModes` = 7 on the test machine) and GNOME has for years.
Opened in the same session as the RemoteDesktop portal the package already
uses, one session would give:

- the pointer position, live, so clicks are recorded with coordinates;
- absolute pointer moves, so the portal road no longer needs to push the
  pointer into a corner before each move;
- everything the session gives now - keys, clicks, any character.

The user is asked once, and the answer can be remembered the way today's is.

**What it costs.**

- The question becomes "share your screen", with a monitor to pick, and the
  desktop shows its screen-sharing indicator while it runs. For a recorder
  that is honest: it is watching.
- The position arrives over PipeWire. `libpipewire-0.3` is part of every
  current Linux desktop, so it fits the package's rule - the system's own
  libraries only, loaded at run time like `libwayland-client`, nothing for a
  user to hunt down. But much of its API is C inline helpers (SPA pods) that
  would have to be written in Pascal; expect it to be the largest unit in
  the package.
- Positions come per monitor and need turning into desktop coordinates.

**First step.** A throwaway probe, not package code: open the combined
session, connect to the stream, print cursor positions as the mouse moves.
If KDE and GNOME both deliver, design the unit; if not, this note says why.

Ruled out: adding up the kernel's mouse movements (pointer acceleration and
screen edges make it drift); the InputCapture portal (it takes the pointer
away from the desktop rather than watching it); anything that needs one
particular desktop.

## Smaller

- The portal road's corner trick assumes the screens' tops line up; more
  than one screen has not been tried.
- GNOME has the same portals as KDE and has not been tried at all.
- Global hotkeys on Wayland: the GlobalShortcuts portal; the manual's
  "Wayland, in detail" has what KDE needs before it will answer.
- Hotkeys in Qt builds: the X11 backend no longer needs GTK; run
  `tests/hotkeycheck` on a Qt5 build, then widen `HOTKEYBACKEND` in
  `hidhotkey.pas`.
- macOS keyboard checks: `tests/inputcheck` skips them over SSH. Run it as an
  application with the Accessibility permission.
- The back and forward buttons in GTK3 programs wait on a fix in the LCL's
  GTK3 widgetset.
