program AutomationDemo;
{$mode objfpc}{$H+}
{$ifdef windows}{$apptype gui}{$endif}
uses Interfaces, Forms, AutomationDemoMain;
begin
  RequireDerivedFormResource := True;
  Application.Initialize;
  Application.CreateForm(TAutomationForm, AutomationForm);
  Application.Run;
end.
