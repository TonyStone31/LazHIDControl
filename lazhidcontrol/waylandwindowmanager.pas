{ Copyright (C) 2026 Tony Stone. GPL-2.0-or-later. }
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
