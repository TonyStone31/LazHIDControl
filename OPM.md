# LazHIDControl OPM maintainer notes

- Package: `LazHIDControl`, runtime (the `.lpk` omits Type, whose default is
  runtime); 31 listed units and the generated `lazhidcontrol.pas` entry point.
- Package file: `lazhidcontrol/lazhidcontrol.lpk`.
- Version: **1.0.0.0**; unchanged for submission.
- Maintainer: Tony Stone; original input code by Tom Gregorovic, Cocoa input
  by Sammarco Francesco, hotkey code adapted from Codebot. Source credits remain.
- Homepage: https://github.com/TonyStone31/LazHIDControl
- Description: Mouse/keyboard automation, global hotkeys, global input monitoring
  and window management for Windows and X11, with an unfinished Wayland input backend.
- License: **GPL-2.0-or-later** in package source notices; full GPL v2 text in
  `COPYING.GPL2`. Existing `LICENSE` contains GPL v3 (a permitted later version).
- Lazarus dependencies: `LCL`, `FCL` with declared minimum **1.0.0.0** for FCL.
- External dependencies on Linux: FPC X11/D-Bus units, linkable X11, Xtst and
  D-Bus libraries, `libXi` for global monitoring; usual LCL/widgetset libraries.
  The unfinished Wayland backend targets uinput; access permissions alone
  do not make it functional.
- Examples: `HIDControlDemo.lpi`, `lazhidcontrol/example/project1.lpi`, and
  `hidctl/hidctl.lpi`; all require `LazHIDControl`.
- Verified here: Linux x86-64, GTK3, Lazarus **4.99** (trunk), FPC **3.3.1**.
  Windows support is reported in the README but was not tested in this review.
  GTK2/Qt and release compiler/IDE versions were not verified here.
  No minimum Lazarus/FPC version has been established.

## Known limitations

Wayland input does not open `/dev/uinput` or create virtual devices; event
layout, key translation and mouse coordinates also need correction.
Wayland hotkey registration and its portal message loop are stubs: registration
remains false. Global monitoring and window management use X11 and do not cover
native Wayland windows. macOS is not a supported submission target: dispatch is
incomplete, hotkey/key-monitor code is stubbed, and global mouse monitoring and
window management return nil. Platform behavior was preserved, not repaired.

Several public unit names overlap Lazarus's bundled input implementation.
Avoid adding both implementations to one project; this package has no dependency
on that other package. It is a runtime library, so use **Compile → Use → Add to
Project**, or register a package link; no IDE palette installation is needed.

## Release contents and checks

Select the **repository root** for OPM, preserving the package subdirectory.
OPM stores `lazhidcontrol` as the relative package path; moving the `.lpk` to the
root is unnecessary. Keep both license texts at the archive root, all package
Pascal units, root documentation, `lazhidcontrol/HOTKEY_USAGE.md`, and the complete
root demo, small example and `hidctl` sources/forms/projects. Keep `project1.ico`.
The Lazarus-generated `lazhidcontrol.pas` entry point is useful source.

Exclude `lib` directories, backups, sessions, generated project `.res` files,
compiled programs (`HIDControlDemo`, `hidctl/hidctl`, and example `project1`),
local `.claude` settings, local build scripts and local patch files from the
staging copy. No tracked build artifacts were found. Do not remove useful
repository files merely to reduce the release archive.

Package/project XML and file paths were checked. Package, main demo, small
example and CLI builds passed from repository-file-only temporary copies with
fresh Lazarus configuration on GTK3. Existing compiler warnings remain unchanged.
No input automation was run against the user's desktop. Windows/macOS/Wayland
runtime testing and the OPM archive-generation UI remain untested.

## Maintenance and compatibility

The [OPM maintainer discussion](https://forum.lazarus.freepascal.org/index.php/topic,75057.msg591411/topicseen.html#new)
explains that one package catalog serves multiple Lazarus versions. Keep the
GitHub issue tracker monitored and verify current release, fixes and trunk
compatibility as they change. A trunk build alone does not establish release
compatibility. Report the configurations actually tested in the submission.

## Generate the OPM submission in Lazarus

1. Prepare a source-only staging copy of this repository, including the pending
   packaging changes. Preserve the repository directory name and relative layout.
   Keep useful development files in Git; omit them only from this release copy.
2. Open **Package → Online Package Manager → Create → Create repository package**.
   Select the staging repository root as the package directory, not the directory
   above it. OPM scans subdirectories and records the package's relative path.
3. Select the repository node: enter its display name, GitHub homepage, an
   appropriate category, description, and external dependencies from these notes.
   Leave download/update URLs blank until actual release URLs exist.
4. Select the `.lpk` node: check imported name, version, author, description,
   license and dependencies. Set Lazarus/FPC compatibility and widgetsets to
   configurations actually verified; review OPM's defaults rather than accepting
   untested platforms or compiler versions.
5. Open **Options** in the creation dialog and review excluded files/folders.
   Exclude build output (`lib`, `units`, `compiled`), backups, VCS directories,
   local settings, sessions, and compiled binaries. Filters in the inspected OPM
   implementation match extensions/folder names; do not rely on them to exclude
   named extensionless executables. A source-only staging copy avoids that issue.
6. Click **Create**, choose an output directory outside the source/staging tree,
   and retain the generated ZIP and repository JSON. Leave **Create JSON for
   updates** unchecked for this initial submission. Do not click **Submit** yet.
7. Inspect the ZIP: ensure the `.lpk`, all units/includes/resources, licenses,
   documentation and demo assets survived. Extract into a fresh directory, open
   the package in Lazarus, compile it, and build the included examples again.
   For a design-time package, also install it and rebuild the IDE.
8. When ready, email the generated ZIP and JSON (or stable download links) to
   `opm@lazarus-ide.org`, identifying the maintainer and tested configurations.

No repository JSON is checked in: the official generator derives archive size,
hash, date, base directory and relative package path from the final ZIP. It belongs
with that release artifact. An update JSON is a separate optional mechanism and
needs real hosted URLs; no format or URL has been invented here.

Workflow verified against the installed Lazarus 4.99 sources:
`components/onlinepackagemanager/opkman_createrepositorypackagefrm.pas`,
`opkman_common.pas`, `opkman_const.pas`, and `opkman_mainfrm.lfm`.
[Official source](https://gitlab.com/freepascal.org/lazarus/lazarus/-/tree/main/components/onlinepackagemanager)
contains the generator and serialization format.
