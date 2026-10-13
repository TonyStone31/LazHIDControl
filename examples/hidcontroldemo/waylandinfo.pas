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
  memoInfo.Lines.Add('Current display environment: ' + displayServer);
  memoInfo.Lines.Add('Wayland support in LazHIDControl');
  memoInfo.Lines.Add('');
  memoInfo.Lines.Add('Keyboard and mouse: a virtual keyboard and pointer made through Linux uinput.');
  memoInfo.Lines.Add('It needs write access to /dev/uinput - MANUAL.md has the udev rule - and has');
  memoInfo.Lines.Add('not been run on a real Wayland desktop yet. Text is typed as a US layout');
  memoInfo.Lines.Add('would type it, and characters beyond ASCII are skipped.');
  memoInfo.Lines.Add('Do not run the demo as root as a workaround.');
  memoInfo.Lines.Add('');
  memoInfo.Lines.Add('Screenshots: through the desktop''s screenshot portal.');
  memoInfo.Lines.Add('Idle, power and clipboard monitoring: through D-Bus and GTK.');
  memoInfo.Lines.Add('Global hotkeys: not written yet (GlobalShortcuts portal).');
  memoInfo.Lines.Add('Other applications'' windows: Wayland does not allow it.');
  memoInfo.Lines.Add('');
  memoInfo.Lines.Add('This backend is chosen whenever WAYLAND_DISPLAY is set,');
  memoInfo.Lines.Add('including when the program itself runs under XWayland.');

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
