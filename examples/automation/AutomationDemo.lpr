program AutomationDemo;
{$mode objfpc}{$H+}
{$ifdef windows}{$apptype gui}{$endif}

uses
  // the key and mouse monitors listen on a thread
  {$ifdef unix}cthreads,{$endif}
  Interfaces,
  Forms,
  AutomationDemoMain;

begin
  RequireDerivedFormResource := True;
  Application.Initialize;
  Application.CreateForm(TAutomationForm, AutomationForm);
  Application.Run;
end.
