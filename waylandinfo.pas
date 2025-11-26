unit WaylandInfo;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, ExtCtrls,
  LCLType;

type

  { TfrmWaylandInfo }

  TfrmWaylandInfo = class(TForm)
    btnClose: TButton;
    lblTitle: TLabel;
    memoInfo: TMemo;
    pnlBottom: TPanel;
    pnlTop: TPanel;
    procedure btnCloseClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
  private
    function DetectDisplayServer: string;
  public

  end;

var
  frmWaylandInfo: TfrmWaylandInfo;

implementation

{$R *.lfm}

{ TfrmWaylandInfo }

procedure TfrmWaylandInfo.FormCreate(Sender: TObject);
begin
  Caption := 'Wayland Implementation Info';
  Position := poScreenCenter;
  Width := 700;
  Height := 500;
end;

procedure TfrmWaylandInfo.FormShow(Sender: TObject);
var
  displayServer: string;
begin
  displayServer := DetectDisplayServer;

  memoInfo.Clear;
  memoInfo.Lines.Add('═══════════════════════════════════════════════════════════');
  memoInfo.Lines.Add('   WAYLAND SUPPORT IN LAZARUS MOUSE AND KEY INPUT PACKAGE');
  memoInfo.Lines.Add('═══════════════════════════════════════════════════════════');
  memoInfo.Lines.Add('');
  memoInfo.Lines.Add('Current Display Server: ' + displayServer);
  memoInfo.Lines.Add('');
  memoInfo.Lines.Add('───────────────────────────────────────────────────────────');
  memoInfo.Lines.Add('IMPLEMENTATION APPROACH');
  memoInfo.Lines.Add('───────────────────────────────────────────────────────────');
  memoInfo.Lines.Add('');
  memoInfo.Lines.Add('Unlike X11 which provides XTest extension for input simulation,');
  memoInfo.Lines.Add('Wayland follows a security-focused design that does NOT allow');
  memoInfo.Lines.Add('applications to arbitrarily inject input events.');
  memoInfo.Lines.Add('');
  memoInfo.Lines.Add('Our solution uses the Linux kernel''s uinput interface to create');
  memoInfo.Lines.Add('virtual input devices. This approach:');
  memoInfo.Lines.Add('');
  memoInfo.Lines.Add('  • Works at the kernel level (below Wayland compositor)');
  memoInfo.Lines.Add('  • Creates virtual keyboard and mouse devices');
  memoInfo.Lines.Add('  • Events appear as real hardware input to all applications');
  memoInfo.Lines.Add('  • Compatible with any Wayland compositor (GNOME, KDE, etc.)');
  memoInfo.Lines.Add('');
  memoInfo.Lines.Add('───────────────────────────────────────────────────────────');
  memoInfo.Lines.Add('PERMISSIONS REQUIRED');
  memoInfo.Lines.Add('───────────────────────────────────────────────────────────');
  memoInfo.Lines.Add('');
  memoInfo.Lines.Add('Access to /dev/uinput requires elevated permissions:');
  memoInfo.Lines.Add('');
  memoInfo.Lines.Add('Option 1: Run as root (NOT RECOMMENDED for regular use)');
  memoInfo.Lines.Add('  $ sudo ./your_application');
  memoInfo.Lines.Add('');
  memoInfo.Lines.Add('Option 2: Add user to input group (RECOMMENDED)');
  memoInfo.Lines.Add('  $ sudo usermod -a -G input $USER');
  memoInfo.Lines.Add('  $ sudo chmod 660 /dev/uinput');
  memoInfo.Lines.Add('  (Log out and back in for changes to take effect)');
  memoInfo.Lines.Add('');
  memoInfo.Lines.Add('Option 3: Create udev rule (BEST for permanent setup)');
  memoInfo.Lines.Add('  Create /etc/udev/rules.d/99-uinput.rules with:');
  memoInfo.Lines.Add('  KERNEL=="uinput", MODE="0660", GROUP="input"');
  memoInfo.Lines.Add('  Then reload: sudo udevadm control --reload-rules');
  memoInfo.Lines.Add('');
  memoInfo.Lines.Add('───────────────────────────────────────────────────────────');
  memoInfo.Lines.Add('CURRENT LIMITATIONS');
  memoInfo.Lines.Add('───────────────────────────────────────────────────────────');
  memoInfo.Lines.Add('');
  memoInfo.Lines.Add('  • Requires permissions setup (security trade-off)');
  memoInfo.Lines.Add('  • Mouse movement is RELATIVE, not absolute positioning');
  memoInfo.Lines.Add('  • Some advanced features still under development');
  memoInfo.Lines.Add('  • Keyboard layout handling may vary by compositor');
  memoInfo.Lines.Add('');
  memoInfo.Lines.Add('───────────────────────────────────────────────────────────');
  memoInfo.Lines.Add('TECHNICAL DETAILS');
  memoInfo.Lines.Add('───────────────────────────────────────────────────────────');
  memoInfo.Lines.Add('');
  memoInfo.Lines.Add('Implementation file: waylandmouseandkeyinput.pas');
  memoInfo.Lines.Add('Interface: Linux uinput (/dev/uinput)');
  memoInfo.Lines.Add('Event types: EV_KEY, EV_REL, EV_SYN');
  memoInfo.Lines.Add('');
  memoInfo.Lines.Add('The implementation creates virtual devices using kernel input');
  memoInfo.Lines.Add('event structures and ioctl() calls. Events are written directly');
  memoInfo.Lines.Add('to the uinput file descriptor, bypassing the Wayland compositor''s');
  memoInfo.Lines.Add('input restrictions.');
  memoInfo.Lines.Add('');
  memoInfo.Lines.Add('───────────────────────────────────────────────────────────');
  memoInfo.Lines.Add('CROSS-PLATFORM COMPATIBILITY');
  memoInfo.Lines.Add('───────────────────────────────────────────────────────────');
  memoInfo.Lines.Add('');
  memoInfo.Lines.Add('This package automatically detects and uses:');
  memoInfo.Lines.Add('  • X11 (XTest extension) - for X Window System');
  memoInfo.Lines.Add('  • Wayland (uinput) - for Wayland compositors');
  memoInfo.Lines.Add('  • Windows API - for Microsoft Windows');
  memoInfo.Lines.Add('  • Carbon/Cocoa - for macOS (community contributions welcome)');
  memoInfo.Lines.Add('');
  memoInfo.Lines.Add('───────────────────────────────────────────────────────────');
  memoInfo.Lines.Add('');
  memoInfo.Lines.Add('For more information and updates, visit the project repository.');
  memoInfo.Lines.Add('Contributions and testing feedback are always welcome!');
  memoInfo.Lines.Add('');
end;

function TfrmWaylandInfo.DetectDisplayServer: string;
var
  waylandDisplay, x11Display: string;
begin
  waylandDisplay := GetEnvironmentVariable('WAYLAND_DISPLAY');
  x11Display := GetEnvironmentVariable('DISPLAY');

  if waylandDisplay <> '' then
    Result := 'Wayland (' + waylandDisplay + ')'
  else if x11Display <> '' then
    Result := 'X11 (' + x11Display + ')'
  else
    Result := 'Unknown (no display server detected)';
end;

procedure TfrmWaylandInfo.btnCloseClick(Sender: TObject);
begin
  Close;
end;

end.
