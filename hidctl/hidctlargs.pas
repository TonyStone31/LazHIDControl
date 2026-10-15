unit hidctlargs;

{$mode objfpc}{$H+}
// Copyright (c) 2026 Tony Stone. 0BSD.

{ The command line as it was typed.  GTK takes --name, --class and --sync
  out of it while the widgetset starts, and xdotool's search wants them; so
  this unit is used before Interfaces and keeps a copy. }

interface

var
  Args: array of string;

implementation

var
  I: integer;

initialization
  SetLength(Args, ParamCount + 1);
  for I := 0 to ParamCount do
    Args[I] := ParamStr(I);
end.
