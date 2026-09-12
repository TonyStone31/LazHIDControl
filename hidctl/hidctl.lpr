program hidctl;

{ hidctl - work a program the way a person does, from a script.

  The command line half of LazHIDControl: everything the package can do to a
  mouse, a keyboard and a window, reachable from a shell script, a Makefile,
  a test runner or any language that can write lines of text.  This is the
  xdotool replacement the package set out to be, in one binary with no
  dependency on X tooling.

  It reads a script - one command a line - and works in coordinates relative
  to the target window's client area, because a window that opens two pixels
  lower than last time should not move every click in the test.

      hidctl --window "Some App" script.txt
      hidctl --window "Some App" -            reads standard input

  That last one is the point of it.  Anything that can print lines can drive
  a program: work the geometry out wherever it is convenient, and let this do
  the part that wants a typed API and a real window manager.

      python3 -c 'import math
      print("at 300 400"); print("press")
      for i in range(70):
          print("at %d %d" % (300+i*12, 400+int(90*math.sin(i/6))))
      print("release")' | hidctl --window "Some App" -

  Commands, one per line; blank lines and # comments ignored:

      find <text>        the window whose title contains this
      title              print the window's title
      activate           bring it to the front
      geometry x y w h   move and resize it
      at x y             the pointer, in the window's coordinates
      dwell <ms>         how long to rest after each move (default 120)
      click [l|r|m]      press and release where the pointer is
      dblclick [l|r|m]
      press [l|r|m]      and hold
      release [l|r|m]
      drag x1 y1 x2 y2   press, move in steps, release
      scroll up|down [n]
      key <name>         return escape tab space back delete
                         up down left right pgup pgdn home end
                         f1..f12, or a single character
      hold <name>        ctrl shift alt, held down
      let <name>         and let go
      type <text>        typed as if on the keyboard
      wait <ms>
      shot <file>        a PNG of the window
      echo <text>

  Exits non-zero if a command fails or the window is not found.
}

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}cthreads,{$ENDIF}
  Interfaces, Classes, SysUtils, Math, Controls, Forms, LCLType,
  MouseAndKeyInput, WindowManager, WindowManagerIntf;

var
  Win: TWindowHandle = 0;
  Client: TRect;
  Failed: Boolean = False;
  { How long to rest after moving the pointer.  Long enough by default that a
    program with a hover of its own keeps up; a generated path of a hundred
    points wants it far shorter, and says so. }
  Dwell: Integer = 120;

{ The client area in screen coordinates.  Every at/drag is relative to this,
  so a script does not care where the window landed. }
procedure Refresh;
begin
  if Win = 0 then Exit;
  Client := WindowMgr.GetWindowRect(Win);
end;

function Btn(const S: string): TMouseButton;
begin
  if (S = 'r') or (S = 'right') then Result := mbRight
  else if (S = 'm') or (S = 'middle') then Result := mbMiddle
  else Result := mbLeft;
end;

{ Names a script can use for a key.  Anything not listed is taken as a single
  character and pressed as itself, which covers letters and digits. }
function KeyOf(S: string): Word;
begin
  S := LowerCase(S);
  if S = 'return' then Exit(VK_RETURN);
  if S = 'enter' then Exit(VK_RETURN);
  if S = 'escape' then Exit(VK_ESCAPE);
  if S = 'esc' then Exit(VK_ESCAPE);
  if S = 'tab' then Exit(VK_TAB);
  if S = 'space' then Exit(VK_SPACE);
  if S = 'back' then Exit(VK_BACK);
  if S = 'delete' then Exit(VK_DELETE);
  if S = 'up' then Exit(VK_UP);
  if S = 'down' then Exit(VK_DOWN);
  if S = 'left' then Exit(VK_LEFT);
  if S = 'right' then Exit(VK_RIGHT);
  if S = 'pgup' then Exit(VK_PRIOR);
  if S = 'pgdn' then Exit(VK_NEXT);
  if S = 'home' then Exit(VK_HOME);
  if S = 'end' then Exit(VK_END);
  if S = 'ctrl' then Exit(VK_CONTROL);
  if S = 'shift' then Exit(VK_SHIFT);
  if S = 'alt' then Exit(VK_MENU);
  if S = 'slash' then Exit(VK_DIVIDE);
  if (Length(S) = 2) and (S[1] = 'f') then Exit(VK_F1 + Ord(S[2]) - Ord('1'));
  if (Length(S) = 3) and (S[1] = 'f') then Exit(VK_F10 + Ord(S[3]) - Ord('0'));
  if Length(S) = 1 then Exit(Ord(UpCase(S[1])));
  Result := 0;
end;

{ A picture of the window.  The capture itself lives in LazHIDControl, beside
  the rest of the window management, because grabbing a window is the same
  problem on every platform as finding or moving one. }
procedure Shot(const FileName: string);
begin
  Refresh;
  if WindowMgr.CaptureWindowToFile(Win, FileName) then
    WriteLn('shot ', FileName, ' ',
      Client.Right - Client.Left, 'x', Client.Bottom - Client.Top)
  else
  begin
    WriteLn('could not capture the window');
    Failed := True;
  end;
end;

procedure Settle(Ms: Integer);
var
  Until_: QWord;
begin
  Until_ := GetTickCount64 + QWord(Max(0, Ms));
  while GetTickCount64 < Until_ do
  begin
    Application.ProcessMessages;
    Sleep(5);
  end;
end;

procedure Drag(X1, Y1, X2, Y2: Integer; B: TMouseButton);
var
  I: Integer;
begin
  MouseInput.Move([], Client.Left + X1, Client.Top + Y1);
  Settle(80);
  MouseInput.Down(B, []);
  Settle(60);
  for I := 1 to 24 do
  begin
    MouseInput.Move([], Client.Left + X1 + (X2 - X1) * I div 24,
                        Client.Top + Y1 + (Y2 - Y1) * I div 24);
    Settle(12);
  end;
  MouseInput.Up(B, []);
  Settle(80);
end;

procedure Run(const Line: string);
var
  P: TStringList;
  Cmd, Rest: string;
  K: Word;
  Steps: Integer;
begin
  if (Trim(Line) = '') or (Copy(Trim(Line), 1, 1) = '#') then Exit;

  P := TStringList.Create;
  try
    P.Delimiter := ' ';
    P.StrictDelimiter := True;
    P.DelimitedText := Trim(Line);
    if P.Count = 0 then Exit;
    Cmd := LowerCase(P[0]);
    { everything after the first word, spaces kept - type and echo want it }
    Rest := Trim(Copy(Trim(Line), Length(P[0]) + 1, MaxInt));

    if Cmd = 'find' then
    begin
      Win := WindowMgr.FindWindow(Rest);
      if Win = 0 then
      begin
        WriteLn('no window titled "', Rest, '"');
        Failed := True;
      end
      else
      begin
        Refresh;
        WriteLn('found "', WindowMgr.GetWindowTitle(Win), '" at ',
          Client.Left, ',', Client.Top, ' ',
          Client.Right - Client.Left, 'x', Client.Bottom - Client.Top);
      end;
    end
    else if Cmd = 'title' then
      WriteLn('title ', WindowMgr.GetWindowTitle(Win))
    else if Cmd = 'activate' then
    begin
      WindowMgr.ActivateWindow(Win);
      Settle(250);
      Refresh;
    end
    else if Cmd = 'geometry' then
    begin
      WindowMgr.MoveResizeWindow(Win, StrToInt(P[1]), StrToInt(P[2]),
        StrToInt(P[3]), StrToInt(P[4]));
      Settle(400);
      Refresh;
    end
    else if Cmd = 'at' then
    begin
      MouseInput.Move([], Client.Left + StrToInt(P[1]), Client.Top + StrToInt(P[2]));
      Settle(Dwell);
    end
    else if Cmd = 'dwell' then
      Dwell := Max(0, StrToInt(P[1]))
    else if Cmd = 'scroll' then
    begin
      K := 1;
      if P.Count > 2 then K := Max(1, StrToInt(P[2]));
      for Steps := 1 to K do
      begin
        if (P.Count > 1) and (LowerCase(P[1]) = 'down') then
          MouseInput.ScrollDown([])
        else
          MouseInput.ScrollUp([]);
        Settle(60);
      end;
    end
    else if Cmd = 'click' then
    begin
      if P.Count > 1 then MouseInput.Click(Btn(P[1]), []) else MouseInput.Click(mbLeft, []);
      Settle(150);
    end
    else if Cmd = 'dblclick' then
    begin
      if P.Count > 1 then MouseInput.DblClick(Btn(P[1]), []) else MouseInput.DblClick(mbLeft, []);
      Settle(150);
    end
    else if Cmd = 'press' then
    begin
      if P.Count > 1 then MouseInput.Down(Btn(P[1]), []) else MouseInput.Down(mbLeft, []);
      Settle(80);
    end
    else if Cmd = 'release' then
    begin
      if P.Count > 1 then MouseInput.Up(Btn(P[1]), []) else MouseInput.Up(mbLeft, []);
      Settle(80);
    end
    else if Cmd = 'drag' then
    begin
      if P.Count > 5 then
        Drag(StrToInt(P[1]), StrToInt(P[2]), StrToInt(P[3]), StrToInt(P[4]), Btn(P[5]))
      else
        Drag(StrToInt(P[1]), StrToInt(P[2]), StrToInt(P[3]), StrToInt(P[4]), mbLeft);
    end
    else if Cmd = 'key' then
    begin
      K := KeyOf(P[1]);
      if K = 0 then WriteLn('unknown key ', P[1]) else KeyInput.Press(K);
      Settle(90);
    end
    else if Cmd = 'hold' then
    begin
      K := KeyOf(P[1]);
      if K <> 0 then KeyInput.Down(K);
      Settle(60);
    end
    else if Cmd = 'let' then
    begin
      K := KeyOf(P[1]);
      if K <> 0 then KeyInput.Up(K);
      Settle(60);
    end
    else if Cmd = 'type' then
    begin
      KeyInput.PressString(Rest);
      Settle(150);
    end
    else if Cmd = 'wait' then
      Settle(StrToInt(P[1]))
    else if Cmd = 'shot' then
      Shot(Rest)
    else if Cmd = 'echo' then
      WriteLn(Rest)
    else
    begin
      WriteLn('unknown command: ', Cmd);
      Failed := True;
    end;
  finally
    P.Free;
  end;
end;

var
  Script: TStringList;
  I: Integer;
  Arg, ScriptFile, WantTitle: string;
begin
  Application.Initialize;
  EnsureWindowManager;
  ScriptFile := '';
  WantTitle := '';
  I := 1;
  while I <= ParamCount do
  begin
    Arg := ParamStr(I);
    if (Arg = '--window') and (I < ParamCount) then
    begin
      Inc(I);
      WantTitle := ParamStr(I);
    end
    else
      ScriptFile := Arg;
    Inc(I);
  end;

  if ScriptFile = '' then
  begin
    WriteLn('hsdrive [--window <title>] <script>   (- for standard input)');
    Halt(2);
  end;

  Script := TStringList.Create;
  try
    if ScriptFile = '-' then
    begin
      while not EOF(Input) do
      begin
        ReadLn(Arg);
        Script.Add(Arg);
      end;
    end
    else
      Script.LoadFromFile(ScriptFile);

    if WantTitle <> '' then Run('find ' + WantTitle);
    for I := 0 to Script.Count - 1 do
    begin
      if Failed then Break;
      Run(Script[I]);
    end;
  finally
    Script.Free;
  end;
  if Failed then Halt(1);
end.
