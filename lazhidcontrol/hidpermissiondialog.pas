unit HIDPermissionDialog;

{$mode objfpc}{$H+}
// Copyright (c) 2026 Tony Stone. 0BSD.

{ The window THIDPermission.Ask shows: what is being allowed, the exact
  files it installs where there are any, and Allow or Not now. }

interface

uses Classes, SysUtils, Forms, Controls, StdCtrls, ExtCtrls;

type
  THIDPermissionDialog = class(TForm)
    AllowButton: TButton;
    LaterButton: TButton;
    Buttons: TPanel;
    DetailsMemo: TMemo;
    Explain: TLabel;
  public
    procedure Prepare(const Explanation, Details: string);
  end;

implementation

{$R *.lfm}

procedure THIDPermissionDialog.Prepare(const Explanation, Details: string);
begin
  Explain.Caption := Explanation;
  DetailsMemo.Text := Details;
  DetailsMemo.Visible := Details <> '';
  {$if defined(linux)}
  AllowButton.Caption := 'Allow - asks for the administrator password';
  {$elseif defined(darwin)}
  AllowButton.Caption := 'Ask macOS';
  {$endif}
end;

end.
