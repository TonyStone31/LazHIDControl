program scriptcheck;
// Copyright (c) 2026 Tony Stone. 0BSD.

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}cthreads,{$ENDIF}
  Interfaces,
  Forms,
  ScriptCheckMain;

begin
  RequireDerivedFormResource := True;
  Application.Scaled := True;
  Application.Initialize;
  Application.CreateForm(TCheckForm, CheckForm);
  Application.Run;
end.
