# hidctl - LazHIDControl from the command line

Everything the package can do to a mouse, a keyboard and a window, reachable
from a shell script, a Makefile, a test runner, or any language that can
write lines of text.  One binary, no dependency on `xdotool`, `wmctrl`,
`xwininfo` or ImageMagick - and the same binary builds for Windows.

    lazbuild hidctl/hidctl.lpi
    hidctl --help                          # every command, on one screen

## In place of xdotool

Commands chain on one line, under xdotool's names:

    hidctl search --name "Text Editor" windowactivate key ctrl+a type "Hello" key Return
    hidctl getactivewindow getwindowgeometry
    hidctl mousemove 400 300 click 1 sleep 0.5 type --delay 20 "typed slowly"

These work as they do in xdotool: `search` (`--name`, `--class`, `--pid`),
`getactivewindow`, `getwindowname`, `getwindowpid`, `getwindowgeometry`,
`windowactivate`, `windowfocus`, `windowraise`, `windowmove`, `windowsize`,
`windowminimize`, `windowclose`, `windowmap`, `windowunmap`, `mousemove`,
`mousemove_relative`, `click`, `mousedown`, `mouseup`, `getmouselocation`,
`getdisplaygeometry`, `key`, `keydown`, `keyup`, `type` (`--delay`), `sleep`,
`get_num_desktops`, `get_desktop`, `set_desktop`.

Where it differs: `search` matches plain text anywhere in the title, any case,
not a regular expression; `--window`, `--clearmodifiers`, `--sync` and
`--onlyvisible` are accepted and ignored; anything not in that list is not
implemented.  Where it does better: `type` gets the text right on any
keyboard layout, including with a second layout switched in, and it runs on
Windows.

## Scripts

    hidctl --window "Some App" script.txt
    hidctl --window "Some App" -           # reads standard input

The second form is the point of it.  Work the geometry out in whatever is
convenient, and let this do the part that wants a typed API and a real window
manager:

```bash
#!/usr/bin/env bash
# a wave, drawn with the button held down
{
  echo "dwell 8"
  echo "at 300 400"
  echo "press"
  # bash only does whole numbers, so awk works the curve out
  awk 'BEGIN { for (i = 0; i < 140; i++)
                 printf "at %d %d\n", 300 + i*6, 400 + int(90*sin(i/12)) }'
  echo "release"
  echo "shot /tmp/wave.png"
} | hidctl --window "Some App" -
```

Coordinates are relative to the target window's client area, so a window that
opens two pixels lower than last time does not move every click in the test.

### Moving rather than jumping

`at x y` puts the pointer there.  `at x y 400` **walks** it there over four
hundred milliseconds, in twenty millisecond steps, using the package's timed
`TMouseInput.Move`.  `drag` takes the same trailing number.

Use it whenever something is watching:

* **recording a demonstration** - a pointer that teleports shows nothing
  about where the hand went;
* **hovers** - an application that only opens a tooltip after the pointer has
  been still for a moment never sees a jump arrive;
* **anything that tracks the pointer** as it moves rather than sampling it.

## Script commands

One per line.  Blank lines and `#` comments are ignored.  The xdotool names
above work in a script too.

| | |
| --- | --- |
| `find <text>` | the window whose title contains this |
| `waitfor <ms> <text>` | the same, but keep looking for up to that long |
| `title` | print the window's title |
| `activate` | bring it to the front |
| `geometry x y w h` | move and resize it |
| `minimize` / `restore` | put it away, bring it back |
| `at x y` | the pointer, in the window's coordinates |
| `at x y <ms>` | walk it there over that many milliseconds instead of jumping |
| `dwell <ms>` | how long to rest after each move (default 120) |
| `click [l\|r\|m]` | press and release where the pointer is |
| `dblclick [l\|r\|m]` | |
| `press` / `release [l\|r\|m]` | hold a button down, let it go |
| `drag x1 y1 x2 y2 [l\|r\|m] [ms]` | press, move, release - over that many milliseconds if given |
| `scroll up\|down [n]` | |
| `key <name>` | `return` `escape` `tab` `space` `back` `delete` `up` `down` `left` `right` `pgup` `pgdn` `home` `end` `f1`..`f12`, one letter or digit, or a combination like `ctrl+shift+t`; `hidctl --help` has the whole list |
| `hold <name>` / `let <name>` | `ctrl` `shift` `alt` |
| `type <text>` | typed as if on the keyboard |
| `wait <ms>` | |
| `shot <file>` | a PNG of the window |
| `shotdesk <file>` | a PNG of the whole desktop |
| `shotarea x y w h <file>` | a PNG of that part of the window |
| `clipset <text>` | put text on the clipboard (held while hidctl runs, unless a clipboard manager keeps it) |
| `clipget` | print the clipboard's text |
| `idle` | print the session's idle time in milliseconds |
| `power` | print power source, battery percentage and charging |

Exits non-zero if a command fails or the window is not found.

A script can watch the machine as well as drive it: `idle` and `power` read
the same backends as the `THIDSystemIdle` and `THIDPowerMonitor` components,
and the desktop and area shots go through `THIDScreenshot`.

## Testing without taking the machine over

Run the program and `hidctl` against a nested X server and neither can steal
the pointer from whoever is sitting at the desk:

```sh
Xephyr :9 -screen 1500x850 &
DISPLAY=:9 openbox &            # something has to honour a move or a resize
DISPLAY=:9 ./someapp &
DISPLAY=:9 hidctl --window "Some App" script.txt
```

## What it cannot do

Nothing reads the screen back, so a script cannot assert - it takes a picture
and a person looks at it.  Either OCR or, far better, a way to ask the program
under test what it thinks its state is.
