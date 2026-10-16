unit ScriptCheckMain;

{$mode objfpc}{$H+}
// Copyright (c) 2026 Tony Stone. 0BSD.

{ The window check.hid drives.  It writes down what reached it - text,
  clicks and where they landed, keys, the wheel - and when Done is clicked
  saves that to the file named on the command line and closes.  The runner
  compares the file with expected.txt, so the same script is checked the
  same way on every system. }

interface

uses
  Classes, SysUtils, Forms, Controls, StdCtrls, ExtCtrls;

type
  TCheckForm = class(TForm)
    NameEdit: TEdit;
    ReplaceEdit: TEdit;
    TickBox: TCheckBox;
    CountButton: TButton;
    Choices: TListBox;
    DoneButton: TButton;
    Pad: TPanel;
    procedure CountButtonClick(Sender: TObject);
    procedure DoneButtonClick(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: word; Shift: TShiftState);
    procedure PadDblClick(Sender: TObject);
    procedure PadMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: integer);
    procedure PadMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: integer);
    procedure PadMouseWheel(Sender: TObject; Shift: TShiftState; WheelDelta: integer; MousePos: TPoint; var Handled: boolean);
  private
    Count: integer;
    DownAt: TPoint;
    LeftClick, RightClick, DoubleClick, Drag, Shortcut: string;
    WheelUp, WheelDown: integer;
    function OnForm(X, Y: integer): string;
  end;

var
  CheckForm: TCheckForm;

implementation

{$R *.lfm}

uses
  LCLType;

// A point on the pad, counted from the form's corner in the designer's
// pixels, as the script counts it, however much the LCL grew the form.
function TCheckForm.OnForm(X, Y: integer): string;
begin
  Result := IntToStr(ScaleFormTo96(Pad.Left + X)) + ',' + IntToStr(ScaleFormTo96(Pad.Top + Y));
end;

procedure TCheckForm.CountButtonClick(Sender: TObject);
begin
  Inc(Count);
  CountButton.Caption := 'Count ' + IntToStr(Count);
end;

procedure TCheckForm.FormKeyDown(Sender: TObject; var Key: word; Shift: TShiftState);
begin
  if (Key = VK_K) and (ssCtrl in Shift) and (ssShift in Shift) then
    Shortcut := 'ctrl+shift+k';
end;

procedure TCheckForm.PadMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: integer);
begin
  DownAt := Point(X, Y);
end;

procedure TCheckForm.PadMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: integer);
begin
  if (Abs(X - DownAt.X) > 5) or (Abs(Y - DownAt.Y) > 5) then
    Drag := OnForm(DownAt.X, DownAt.Y) + ' to ' + OnForm(X, Y)
  else if (Button = mbRight) and (RightClick = '') then
    RightClick := OnForm(X, Y)
  else if (Button = mbLeft) and (LeftClick = '') then
    LeftClick := OnForm(X, Y);
end;

procedure TCheckForm.PadDblClick(Sender: TObject);
var
  P: TPoint;
begin
  P := Pad.ScreenToClient(Mouse.CursorPos);
  DoubleClick := OnForm(P.X, P.Y);
end;

procedure TCheckForm.PadMouseWheel(Sender: TObject; Shift: TShiftState; WheelDelta: integer; MousePos: TPoint; var Handled: boolean);
begin
  if WheelDelta > 0 then Inc(WheelUp)
  else if WheelDelta < 0 then Inc(WheelDown);
  Handled := True;
end;

procedure TCheckForm.DoneButtonClick(Sender: TObject);
var
  Lines: TStringList;
  Choice: string;
begin
  Choice := '';
  if Choices.ItemIndex >= 0 then Choice := Choices.Items[Choices.ItemIndex];
  Lines := TStringList.Create;
  try
    Lines.Add('name: ' + NameEdit.Text);
    Lines.Add('replace: ' + ReplaceEdit.Text);
    Lines.Add('tick: ' + BoolToStr(TickBox.Checked, 'checked', 'clear'));
    Lines.Add('count: ' + IntToStr(Count));
    Lines.Add('list: ' + Choice);
    Lines.Add('shortcut: ' + Shortcut);
    Lines.Add('left click: ' + LeftClick);
    Lines.Add('right click: ' + RightClick);
    Lines.Add('double click: ' + DoubleClick);
    Lines.Add('drag: ' + Drag);
    Lines.Add('wheel: up ' + IntToStr(WheelUp) + ', down ' + IntToStr(WheelDown));
    if ParamCount > 0 then Lines.SaveToFile(ParamStr(1))
    else
      Lines.SaveToFile('scriptcheck-result.txt');
  finally
    Lines.Free;
  end;
  Close;
end;

end.
