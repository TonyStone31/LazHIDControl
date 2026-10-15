// Copyright (c) 2026 Tony Stone. 0BSD.
unit WaylandWindowManager;

{$mode objfpc}{$H+}

interface

uses WindowManagerIntf;

// Returns nil until this backend is implemented.
function InitializeWindowManager: TWindowManager;

implementation

function InitializeWindowManager: TWindowManager;
begin
  // TODO: Implement supported window operations through compositor protocols.
  Result := nil;
end;

end.
