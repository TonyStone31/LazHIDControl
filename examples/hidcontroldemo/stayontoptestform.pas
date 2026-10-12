unit StayOnTopTestForm;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls;

type
  TfrmStayOnTopTest = class(TForm)
    lblInfo: TLabel;
    chkStayOnTop: TCheckBox;
    procedure chkStayOnTopChange(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  end;

var
  frmStayOnTopTest: TfrmStayOnTopTest;

implementation

{$R *.lfm}

procedure TfrmStayOnTopTest.FormCreate(Sender: TObject);
begin
  chkStayOnTop.Checked := FormStyle = fsStayOnTop;
end;

procedure TfrmStayOnTopTest.chkStayOnTopChange(Sender: TObject);
begin
  if chkStayOnTop.Checked then
    FormStyle := fsStayOnTop
  else
    FormStyle := fsNormal;
end;

end.
