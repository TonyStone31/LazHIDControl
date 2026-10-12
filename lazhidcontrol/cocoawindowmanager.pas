{ Copyright (C) 2026 Tony Stone. GPL-2.0-or-later. }
unit CocoaWindowManager;

{$mode objfpc}{$H+}

interface

uses WindowManagerIntf;

// Returns nil until this backend is implemented.
function InitializeWindowManager: TWindowManager;

implementation

function InitializeWindowManager: TWindowManager;
begin
  // TODO: Implement external-window queries and control using macOS APIs.
  Result := nil;
end;

end.
