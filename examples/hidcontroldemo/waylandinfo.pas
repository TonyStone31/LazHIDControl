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
  memoInfo.Lines.Add('The intended backend emulates a keyboard and mouse through Linux uinput.');
  memoInfo.Lines.Add('This checkout does not open /dev/uinput or create the virtual devices.');
  memoInfo.Lines.Add('Event layout, key translation and mouse coordinates also need work.');
  memoInfo.Lines.Add('Changing permissions alone will not make this backend functional.');
  memoInfo.Lines.Add('');
  memoInfo.Lines.Add('The input factory selects it when WAYLAND_DISPLAY is set,');
  memoInfo.Lines.Add('including when this application runs under Xwayland.');
  memoInfo.Lines.Add('Hotkey portal support is a stub. Window management remains X11 only.');
  memoInfo.Lines.Add('');
  memoInfo.Lines.Add('For a completed backend, device access should be restricted to /dev/uinput.');
  memoInfo.Lines.Add('The general input group may also allow reading physical input events.');
  memoInfo.Lines.Add('Do not run the demo as root as a workaround.');
  memoInfo.Lines.Add('');
  memoInfo.Lines.Add('See README.md for the current implementation limits.');

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
