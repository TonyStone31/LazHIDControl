program HotkeyDemo;
{$mode objfpc}{$H+}
{$ifdef windows}{$apptype gui}{$endif}
uses Interfaces, Forms, HotkeyDemoMain;
begin
  RequireDerivedFormResource := True;
  Application.Initialize;
  Application.CreateForm(THotkeyDemoForm, HotkeyDemoForm);
  Application.Run;
end.
