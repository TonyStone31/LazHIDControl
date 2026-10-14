program hidctl;

(* hidctl - work a program the way a person does, from a shell.

  The command line half of LazHIDControl: the mouse, the keyboard, windows,
  screenshots and the clipboard, in one binary that runs on Windows as well
  as Linux.  It answers to xdotool's command names where it has the command,
  and to a short vocabulary of its own that works in coordinates relative to
  the target window.  "hidctl --help" prints all of it.
*)

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}cthreads,{$ENDIF}
  hidctlargs,
  Interfaces,
  Classes,
  SysUtils,
  Math,
  Controls,
  Forms,
  LCLType,
  Graphics,
  Clipbrd,
  LazUTF8,
  HIDMouseAndKeyInput,
  WindowManager,
  WindowManagerIntf,
  HIDScreenshot,
  IdleInput,
  PowerInput,
  HIDPermission;

const
  Help: array[0..78] of string = (
    'hidctl - drive the mouse, keyboard and windows from a shell',
    '',
    '  hidctl [--window <title>] <script>     run a script, one command a line',
    '  hidctl [--window <title>] -            the same, from standard input',
    '  hidctl [--window <title>] <command> [args] [<command> [args]] ...',
    '',
    'The third form chains commands on one line the way xdotool does:',
    '',
    '  hidctl search --name "Text Editor" windowactivate key ctrl+a type "hello"',
    '',
    'A window found by --window, find, waitfor, search or getactivewindow stays',
    'the current window for every command after it.  Exits non-zero if a',
    'command fails.  In a script, blank lines and # comments are ignored.',
    '',
    'xdotool commands (screen coordinates; [win] is an id, default the current window)',
    '',
    '  search [--name|--class|--pid] <text>   print the ids of matching windows;',
    '                               text matches any part, any case',
    '  getactivewindow              print the active window''s id',
    '  getwindowname [win]          print its title',
    '  getwindowpid [win]           print its process id',
    '  getwindowgeometry [win]      print its position and size',
    '  windowactivate [win]         bring it to the front',
    '  windowfocus [win]  windowraise [win]',
    '  windowmove [win] x y         windowsize [win] w h',
    '  windowminimize [win]         windowclose [win]',
    '  windowmap [win]              windowunmap [win]',
    '  mousemove x y                mousemove_relative dx dy',
    '  click <1|2|3|4|5>            left, middle, right, wheel up, wheel down',
    '  mousedown <1|2|3>            mouseup <1|2|3>',
    '  getmouselocation             getdisplaygeometry',
    '  key <key> [<key> ...]        a key or a combination: Return, ctrl+c, alt+F4',
    '  keydown <key>                keyup <key>',
    '  type [--delay <ms>] <text>   typed as if on the keyboard, any Unicode',
    '  sleep <seconds>',
    '  get_num_desktops  get_desktop  set_desktop <n>',
    '',
    'hidctl commands (coordinates inside the current window)',
    '',
    '  find <text>                  the window whose title contains this',
    '  waitfor <ms> <text>          the same, but keep looking that long',
    '  title                        print the window''s title',
    '  activate                     bring it to the front',
    '  geometry x y w h             move and resize it',
    '  minimize                     restore',
    '  at x y [ms]                  move the pointer; with ms, walk it there',
    '  dwell <ms>                   rest after each "at" (default 120)',
    '  click [l|r|m]                dblclick [l|r|m]',
    '  press [l|r|m]                release [l|r|m]',
    '  drag x1 y1 x2 y2 [l|r|m] [ms]',
    '  scroll up|down [n]',
    '  hold <key>                   let <key>',
    '  wait <ms>',
    '  shot <file.png>              a picture of the window',
    '  shotarea x y w h <file.png>  a picture of part of it',
    '  shotdesk <file.png>          a picture of the whole desktop',
    '  clipset <text>               clipget',
    '  idle                         print the session''s idle time in ms',
    '  power                        print power source, battery, charging',
    '  permissions                  what this desktop has allowed: typing, watching',
    '  grant                        ask the desktop to allow them; revoke takes it back',
    '  echo <text>',
    '',
    'Keys',
    '',
    '  return enter escape esc tab space backspace back delete insert',
    '  up down left right home end pgup page_up prior pgdn page_down next',
    '  f1..f12 ctrl shift alt capslock numlock',
    '  minus equal comma period semicolon apostrophe grave backslash',
    '  bracketleft bracketright slash (the keypad one)',
    '  or one letter or digit.  Names ignore case.',
    '',
    'Not xdotool: search takes plain text, not a regular expression; of its',
    'options only those shown do anything (--window, --clearmodifiers, --sync',
    'and --onlyvisible are accepted and ignored); commands not listed are not',
    'implemented.  On Wayland the keyboard and mouse need write access to',
    '/dev/uinput and the window commands are unavailable.',
    '',
    'https://github.com/TonyStone31/LazHIDControl');

  KeyNames: array[0..38] of record
      Name: string;
      Key: word;
      end
  = (
    (Name: 'return'; Key: VK_RETURN), (Name: 'enter'; Key: VK_RETURN),
    (Name: 'escape'; Key: VK_ESCAPE), (Name: 'esc'; Key: VK_ESCAPE),
    (Name: 'tab'; Key: VK_TAB), (Name: 'space'; Key: VK_SPACE),
    (Name: 'back'; Key: VK_BACK), (Name: 'backspace'; Key: VK_BACK),
    (Name: 'delete'; Key: VK_DELETE), (Name: 'insert'; Key: VK_INSERT),
    (Name: 'up'; Key: VK_UP), (Name: 'down'; Key: VK_DOWN),
    (Name: 'left'; Key: VK_LEFT), (Name: 'right'; Key: VK_RIGHT),
    (Name: 'home'; Key: VK_HOME), (Name: 'end'; Key: VK_END),
    (Name: 'pgup'; Key: VK_PRIOR), (Name: 'page_up'; Key: VK_PRIOR), (Name: 'prior'; Key: VK_PRIOR),
    (Name: 'pgdn'; Key: VK_NEXT), (Name: 'page_down'; Key: VK_NEXT), (Name: 'next'; Key: VK_NEXT),
    (Name: 'ctrl'; Key: VK_CONTROL), (Name: 'control'; Key: VK_CONTROL),
    (Name: 'shift'; Key: VK_SHIFT), (Name: 'alt'; Key: VK_MENU),
    (Name: 'capslock'; Key: VK_CAPITAL), (Name: 'numlock'; Key: VK_NUMLOCK),
    (Name: 'slash'; Key: VK_DIVIDE), (Name: 'minus'; Key: VK_LCL_MINUS),
    (Name: 'equal'; Key: VK_LCL_EQUAL), (Name: 'comma'; Key: VK_LCL_COMMA),
    (Name: 'period'; Key: VK_LCL_POINT), (Name: 'semicolon'; Key: VK_LCL_SEMI_COMMA),
    (Name: 'apostrophe'; Key: VK_LCL_QUOTE), (Name: 'grave'; Key: VK_LCL_TILDE),
    (Name: 'backslash'; Key: VK_LCL_BACKSLASH),
    (Name: 'bracketleft'; Key: VK_LCL_OPEN_BRACKET), (Name: 'bracketright'; Key: VK_LCL_CLOSE_BRACKET));

  { Every command word, so a chain on the command line knows where one
    command's arguments stop and the next command starts. }
  Commands = ' find waitfor title activate geometry minimize restore at dwell click dblclick press release drag scroll key hold let type ' +
    'wait shot shotdesk shotarea clipset clipget idle power permissions grant revoke echo search getactivewindow getwindowfocus getwindowname getwindowpid ' +
    'getwindowgeometry windowactivate windowfocus windowraise windowmove windowsize windowminimize windowclose windowmap windowunmap ' +
    'mousemove mousemove_relative mousedown mouseup getmouselocation getdisplaygeometry keydown keyup sleep get_num_desktops ' + 'get_desktop set_desktop ';

  Options = ' --name --class --pid --delay --repeat --window --clearmodifiers --sync --onlyvisible ';

var
  Win: TWindowHandle = 0;
  Client: TRect;
  Failed: boolean = False;
  { How long to rest after moving the pointer.  Long enough by default that a
    program with a hover of its own keeps up; a generated path of a hundred
    points wants it far shorter, and says so. }
  Dwell: integer = 120;

{ The window in screen coordinates.  Every at/drag is relative to this, so a
  script does not care where the window landed. }
  procedure Refresh;
  begin
    if Win = 0 then Exit;
    Client := WindowMgr.GetWindowRect(Win);
  end;

  function Btn(const S: string): TMouseButton;
  begin
    if (S = 'r') or (S = 'right') or (S = '3') then Result := mbRight
    else if (S = 'm') or (S = 'middle') or (S = '2') then Result := mbMiddle
    else
      Result := mbLeft;
  end;

  function KeyOf(S: string): word;
  var
    I: integer;
  begin
    S := LowerCase(S);
    for I := Low(KeyNames) to High(KeyNames) do
      if KeyNames[I].Name = S then Exit(KeyNames[I].Key);
    if (Length(S) = 2) and (S[1] = 'f') then Exit(VK_F1 + Ord(S[2]) - Ord('1'));
    if (Length(S) = 3) and (S[1] = 'f') then Exit(VK_F10 + Ord(S[3]) - Ord('0'));
    if (Length(S) = 1) and (S[1] in ['a'..'z', '0'..'9']) then Exit(Ord(UpCase(S[1])));
    Result := 0;
  end;

  procedure Settle(Ms: integer);
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

  { "ctrl+shift+t" holds the first keys while it presses the last. }
  procedure PressCombo(const Combo: string);
  var
    Parts: TStringList;
    I: integer;
  begin
    Parts := TStringList.Create;
    try
      Parts.Delimiter := '+';
      Parts.StrictDelimiter := True;
      Parts.DelimitedText := Combo;
      for I := 0 to Parts.Count - 1 do
        if KeyOf(Parts[I]) = 0 then
        begin
          WriteLn('unknown key ', Combo);
          Failed := True;
          Exit;
        end;
      for I := 0 to Parts.Count - 2 do
        KeyInput.Down(KeyOf(Parts[I]));
      KeyInput.Press(KeyOf(Parts[Parts.Count - 1]));
      for I := Parts.Count - 2 downto 0 do
        KeyInput.Up(KeyOf(Parts[I]));
    finally
      Parts.Free;
    end;
  end;

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

  { Report the window just found, which every way of finding one wants to do. }
  procedure Announce;
  begin
    Refresh;
    WriteLn('found "', WindowMgr.GetWindowTitle(Win), '" at ',
      Client.Left, ',', Client.Top, ' ',
      Client.Right - Client.Left, 'x', Client.Bottom - Client.Top);
  end;

  procedure Find(const Title: string; Ms: integer);
  var
    Deadline: QWord;
  begin
    Deadline := GetTickCount64 + QWord(Max(0, Ms));
    repeat
      Win := WindowMgr.FindWindow(Title);
      if (Win <> 0) or (GetTickCount64 >= Deadline) then Break;
      Settle(100);
    until False;
    if Win <> 0 then Announce
    else
    begin
      if Ms > 0 then WriteLn('no window titled "', Title, '" after ', Ms, ' ms')
      else
        WriteLn('no window titled "', Title, '"');
      Failed := True;
    end;
  end;

  { xdotool's search: every match is printed, the first becomes the current
    window.  Kind is name, class or pid. }
  procedure Search(const Kind, Text: string);
  var
    List: TList;
    Item: Pointer;
    Info: TWindowInfo;
    Hit: boolean;
  begin
    Win := 0;
    List := WindowMgr.EnumerateWindows;
    try
      for Item in List do
        if WindowMgr.GetWindowInfo(TWindowHandle(Item), Info) then
        begin
          if Kind = 'pid' then Hit := Info.ProcessID = StrToDWord(Text)
          else if Kind = 'class' then Hit := Pos(UTF8LowerCase(Text), UTF8LowerCase(Info.ClassName)) > 0
          else
            Hit := Pos(UTF8LowerCase(Text), UTF8LowerCase(Info.Title)) > 0;
          if not Hit then Continue;
          WriteLn(Info.Handle);
          if Win = 0 then Win := Info.Handle;
        end;
    finally
      List.Free;
    end;
    Failed := Win = 0;
    Refresh;
  end;

{ The desktop, or a piece of the window, through the package's own
  THIDScreenshot - the same road the dropped component takes. }
  procedure AreaShot(const FileName: string; const Area: TRect; Whole: boolean);
  var
    S: THIDScreenshot;
    Png: TPortableNetworkGraphic;
    Ok: boolean;
  begin
    S := THIDScreenshot.Create(nil);
    try
      if Whole then Ok := S.Capture
      else
        Ok := S.CaptureArea(Area);
      if not Ok then
      begin
        WriteLn('could not capture: ', S.LastError);
        Failed := True;
        Exit;
      end;
      Png := TPortableNetworkGraphic.Create;
      try
        Png.Assign(S.Bitmap);
        Png.SaveToFile(FileName);
      finally
        Png.Free;
      end;
      WriteLn('shot ', FileName, ' ', S.Bitmap.Width, 'x', S.Bitmap.Height);
    finally
      S.Free;
    end;
  end;

  procedure PowerLine;
  const
    Names: array[TPowerSource] of string = ('unknown source', 'ac', 'battery');
  var
    P: TPowerInput;
    St: TPowerStatus;
    Line: string;
  begin
    P := CreatePowerInput;
    if (P = nil) or not P.Start or not P.ReadStatus(St) then
    begin
      if P = nil then WriteLn('power is not implemented on this platform')
      else
        WriteLn('power: ', P.ErrorText);
      Failed := True;
    end
    else
    begin
      Line := 'power ' + Names[St.Source];
      if St.BatteryPercent >= 0 then Line := Line + ' ' + IntToStr(St.BatteryPercent) + '%'
      else
        Line := Line + ' no battery';
      if St.Charging then Line := Line + ' charging';
      WriteLn(Line);
    end;
    P.Free;
  end;

{ A drag, optionally over a given number of milliseconds.

  Ms = 0 is twenty-four even steps with a short rest between them, which is
  enough for an application to see a drag and quick enough not to hold a
  test up.  Anything else walks the pointer for exactly that long, for when
  somebody is going to watch it. }
  procedure Drag(X1, Y1, X2, Y2: integer; B: TMouseButton; Ms: integer = 0);
  var
    I: integer;
  begin
    MouseInput.Move([], Client.Left + X1, Client.Top + Y1);
    Settle(80);
    MouseInput.Down(B, []);
    Settle(60);
    if Ms > 0 then
      MouseInput.Move([], Client.Left + X2, Client.Top + Y2, Ms)
    else
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
    Cmd, Rest, Msg, Kind: string;
    K: word;
    I, Ms, Delay, Times: integer;
    Btm: TMouseButton;
    Q: QWord;
    Perm: THIDPermission;
    Seconds: double;

    { A leading window id on the xdotool window commands: "windowmove 123 10 20"
      against "windowmove 10 20".  Extra is how many arguments follow it. }
    procedure TakeWindow(Extra: integer);
    begin
      if P.Count - 1 <= Extra then Exit;
      if P[1][1] <> '%' then Win := StrToQWord(P[1]);
      P.Delete(1);
      Refresh;
    end;

  begin
    if (Trim(Line) = '') or (Copy(Trim(Line), 1, 1) = '#') then Exit;

    P := TStringList.Create;
    try
      P.Delimiter := ' ';
      P.StrictDelimiter := True;
      P.DelimitedText := Trim(Line);
      Cmd := LowerCase(P[0]);
      { everything after the first word, spaces kept - type and echo want it }
      Rest := Trim(Copy(Trim(Line), Length(P[0]) + 1, MaxInt));

      { xdotool's options: the few that mean something here are kept, the
        rest are dropped so its command lines still run }
      Delay := 0;
      Times := 1;
      Kind := 'name';
      while (P.Count > 1) and (Pos(' ' + P[1] + ' ', Options) > 0) do
      begin
        Msg := Copy(P[1], 3, MaxInt);
        Rest := Trim(Copy(Rest, Length(P[1]) + 1, MaxInt));
        P.Delete(1);
        if (Msg = 'name') or (Msg = 'class') or (Msg = 'pid') then Kind := Msg
        else if ((Msg = 'delay') or (Msg = 'repeat') or (Msg = 'window')) and (P.Count > 1) then
        begin
          if Msg = 'delay' then Delay := StrToInt(P[1]);
          if Msg = 'repeat' then Times := StrToInt(P[1]);
          Rest := Trim(Copy(Rest, Length(P[1]) + 1, MaxInt));
          P.Delete(1);
        end;
      end;

      // the window commands, which a Wayland session has no backend for
      if (WindowMgr = nil) and (Pos(' ' + Cmd + ' ', ' find waitfor title activate geometry at search getactivewindow getwindowfocus getwindowname getwindowpid getwindowgeometry '
        + 'windowactivate windowfocus windowraise windowmove windowsize windowminimize windowclose windowmap windowunmap minimize restore get_num_desktops get_desktop set_desktop ') > 0) then
      begin
        WriteLn(Cmd, ': there is no window control on this desktop');
        Failed := True;
      end
      else if Cmd = 'find' then
        Find(Rest, 0)
      else if Cmd = 'waitfor' then
        Find(Trim(Copy(Rest, Length(P[1]) + 1, MaxInt)), StrToInt(P[1]))
      else if Cmd = 'search' then
        Search(Kind, Rest)
      else if (Cmd = 'getactivewindow') or (Cmd = 'getwindowfocus') then
      begin
        Win := WindowMgr.GetActiveWindow;
        Refresh;
        WriteLn(Win);
      end
      else if Cmd = 'title' then
        WriteLn('title ', WindowMgr.GetWindowTitle(Win))
      else if Cmd = 'getwindowname' then
      begin
        TakeWindow(0);
        WriteLn(WindowMgr.GetWindowTitle(Win));
      end
      else if Cmd = 'getwindowpid' then
      begin
        TakeWindow(0);
        WriteLn(WindowMgr.GetWindowPID(Win));
      end
      else if Cmd = 'getwindowgeometry' then
      begin
        TakeWindow(0);
        Refresh;
        WriteLn('Window ', Win);
        WriteLn('  Position: ', Client.Left, ',', Client.Top, ' (screen: 0)');
        WriteLn('  Geometry: ', Client.Right - Client.Left, 'x', Client.Bottom - Client.Top);
      end
      else if (Cmd = 'activate') or (Cmd = 'windowactivate') then
      begin
        TakeWindow(0);
        WindowMgr.ActivateWindow(Win);
        Settle(250);
        Refresh;
      end
      else if Cmd = 'windowfocus' then
      begin
        TakeWindow(0);
        WindowMgr.FocusWindow(Win);
      end
      else if Cmd = 'windowraise' then
      begin
        TakeWindow(0);
        WindowMgr.RaiseWindow(Win);
      end
      else if Cmd = 'geometry' then
      begin
        WindowMgr.MoveResizeWindow(Win, StrToInt(P[1]), StrToInt(P[2]),
          StrToInt(P[3]), StrToInt(P[4]));
        Settle(400);
        Refresh;
      end
      else if Cmd = 'windowmove' then
      begin
        TakeWindow(2);
        WindowMgr.MoveWindow(Win, StrToInt(P[1]), StrToInt(P[2]));
        Settle(250);
        Refresh;
      end
      else if Cmd = 'windowsize' then
      begin
        TakeWindow(2);
        WindowMgr.ResizeWindow(Win, StrToInt(P[1]), StrToInt(P[2]));
        Settle(250);
        Refresh;
      end
      else if (Cmd = 'minimize') or (Cmd = 'windowminimize') then
      begin
        TakeWindow(0);
        WindowMgr.MinimizeWindow(Win);
        Settle(250);
      end
      else if Cmd = 'restore' then
      begin
        WindowMgr.RestoreWindow(Win);
        Settle(250);
        Refresh;
      end
      else if Cmd = 'windowclose' then
      begin
        TakeWindow(0);
        WindowMgr.CloseWindow(Win);
      end
      else if Cmd = 'windowmap' then
      begin
        TakeWindow(0);
        WindowMgr.ShowWindow(Win);
      end
      else if Cmd = 'windowunmap' then
      begin
        TakeWindow(0);
        WindowMgr.HideWindow(Win);
      end
      else if Cmd = 'at' then
      begin
        { "at x y" jumps; "at x y 400" walks there over 400 ms, for a
          recording, or a hover that only starts once the pointer has
          arrived the slow way }
        if P.Count > 3 then
          MouseInput.Move([], Client.Left + StrToInt(P[1]),
            Client.Top + StrToInt(P[2]), Max(0, StrToInt(P[3])))
        else
          MouseInput.Move([], Client.Left + StrToInt(P[1]), Client.Top + StrToInt(P[2]));
        Settle(Dwell);
      end
      else if Cmd = 'mousemove' then
        MouseInput.Move([], StrToInt(P[1]), StrToInt(P[2]))
      else if Cmd = 'mousemove_relative' then
        MouseInput.MoveBy([], StrToInt(P[1]), StrToInt(P[2]))
      else if Cmd = 'getmouselocation' then
        WriteLn('x:', Mouse.CursorPos.X, ' y:', Mouse.CursorPos.Y, ' screen:0 window:',
          GetWindowAtPoint(Mouse.CursorPos.X, Mouse.CursorPos.Y))
      else if Cmd = 'getdisplaygeometry' then
        WriteLn(Screen.DesktopWidth, ' ', Screen.DesktopHeight)
      else if Cmd = 'dwell' then
        Dwell := Max(0, StrToInt(P[1]))
      else if Cmd = 'scroll' then
      begin
        if P.Count > 2 then Times := Max(1, StrToInt(P[2]));
        for I := 1 to Times do
        begin
          if (P.Count > 1) and (LowerCase(P[1]) = 'down') then
            MouseInput.ScrollDown([])
          else
            MouseInput.ScrollUp([]);
          Settle(60);
        end;
      end
      else if Cmd = 'click' then
        for I := 1 to Times do
        begin
          { xdotool's buttons 4 and 5 are the wheel }
          if (P.Count > 1) and (P[1] = '4') then MouseInput.ScrollUp([])
          else if (P.Count > 1) and (P[1] = '5') then MouseInput.ScrollDown([])
          else if P.Count > 1 then MouseInput.Click(Btn(P[1]), [])
          else
            MouseInput.Click(mbLeft, []);
          Settle(150);
        end
      else if Cmd = 'dblclick' then
      begin
        if P.Count > 1 then MouseInput.DblClick(Btn(P[1]), [])
        else
          MouseInput.DblClick(mbLeft, []);
        Settle(150);
      end
      else if (Cmd = 'press') or (Cmd = 'mousedown') then
      begin
        if P.Count > 1 then MouseInput.Down(Btn(P[1]), [])
        else
          MouseInput.Down(mbLeft, []);
        Settle(80);
      end
      else if (Cmd = 'release') or (Cmd = 'mouseup') then
      begin
        if P.Count > 1 then MouseInput.Up(Btn(P[1]), [])
        else
          MouseInput.Up(mbLeft, []);
        Settle(80);
      end
      else if Cmd = 'drag' then
      begin
      { "drag x1 y1 x2 y2", then optionally a button, then optionally how
        many milliseconds it should take.  The two are told apart by whether
        it is a number - buttons are l, r and m - so either can be left out
        and neither has to be remembered in order. }
        Btm := mbLeft;
        Ms := 0;
        if (P.Count > 5) and not TryStrToInt(P[5], Ms) then
        begin
          Btm := Btn(P[5]);
          Ms := 0;
          if P.Count > 6 then TryStrToInt(P[6], Ms);
        end;
        Drag(StrToInt(P[1]), StrToInt(P[2]), StrToInt(P[3]), StrToInt(P[4]), Btm, Ms);
      end
      else if Cmd = 'key' then
        for I := 1 to P.Count - 1 do
        begin
          PressCombo(P[I]);
          Settle(90);
        end
      else if (Cmd = 'hold') or (Cmd = 'keydown') then
      begin
        K := KeyOf(P[1]);
        if K <> 0 then KeyInput.Down(K);
        Settle(60);
      end
      else if (Cmd = 'let') or (Cmd = 'keyup') then
      begin
        K := KeyOf(P[1]);
        if K <> 0 then KeyInput.Up(K);
        Settle(60);
      end
      else if Cmd = 'type' then
      begin
        KeyInput.PressString(Rest, Delay);
        Settle(150);
      end
      else if Cmd = 'wait' then
        Settle(StrToInt(P[1]))
      else if Cmd = 'sleep' then
      begin
        Val(P[1], Seconds, I);
        Settle(Round(Seconds * 1000));
      end
      else if Cmd = 'shot' then
        Shot(Rest)
      else if Cmd = 'shotdesk' then
        AreaShot(Rest, Rect(0, 0, 0, 0), True)
      else if Cmd = 'shotarea' then
      begin
        Refresh;
        AreaShot(P[5], Rect(Client.Left + StrToInt(P[1]), Client.Top + StrToInt(P[2]), Client.Left + StrToInt(P[1]) + StrToInt(P[3]),
          Client.Top + StrToInt(P[2]) + StrToInt(P[4])), False);
      end
      else if Cmd = 'clipset' then
      begin
        { held only while hidctl runs, unless a clipboard manager keeps it }
        Clipboard.AsText := Rest;
        Settle(100);
      end
      else if Cmd = 'clipget' then
        WriteLn('clip ', Clipboard.AsText)
      else if (Cmd = 'permissions') or (Cmd = 'grant') or (Cmd = 'revoke') then
      begin
        Perm := THIDPermission.Create(nil);
        try
          Perm.Needs := [hnInput, hnWatching];
          if Cmd = 'grant' then Failed := not Perm.Grant
          else if Cmd = 'revoke' then Failed := not Perm.Revoke;
          if Failed then WriteLn(Cmd, ': ', Perm.LastError)
          else
          begin
            WriteLn('typing ', BoolToStr(hnInput in Perm.Missing, 'missing', 'allowed'));
            WriteLn('watching ', BoolToStr(hnWatching in Perm.Missing, 'missing', 'allowed'));
          end;
        finally
          Perm.Free;
        end;
      end
      else if Cmd = 'idle' then
      begin
        if QuerySystemIdle(Q, Msg) then
          WriteLn('idle ', Q)
        else
        begin
          WriteLn('idle: ', Msg);
          Failed := True;
        end;
      end
      else if Cmd = 'power' then
        PowerLine
      else if Cmd = 'get_num_desktops' then
        WriteLn(WindowMgr.GetDesktopCount)
      else if Cmd = 'get_desktop' then
        WriteLn(WindowMgr.GetCurrentDesktop)
      else if Cmd = 'set_desktop' then
        WindowMgr.SetCurrentDesktop(StrToInt(P[1]))
      else if Cmd = 'echo' then
        WriteLn(Rest)
      else
      begin
        WriteLn('unknown command: ', Cmd, '   (hidctl --help lists them)');
        Failed := True;
      end;
    finally
      P.Free;
    end;
  end;

  function IsCommand(const S: string): boolean;
  begin
    Result := Pos(' ' + LowerCase(S) + ' ', Commands) > 0;
  end;

  { "mousemove 10 20 click 1 type hello" becomes three script lines.  A
    command's arguments run to the next command word - except the ones that
    take free text, which take a fixed count so the text can say anything. }
  procedure Chain(First: integer; Script: TStrings);
  var
    I, Fixed: integer;
    Line, Arg: string;
  begin
    I := First;
    while I <= High(Args) do
    begin
      Line := Args[I];
      Inc(I);
      case LowerCase(Line) of
        'type', 'echo', 'clipset', 'find', 'shot', 'shotdesk': Fixed := 1;
        'waitfor': Fixed := 2;
        'shotarea': Fixed := 5;
        else
          Fixed := -1;
      end;
      while (I <= High(Args)) and (Fixed <> 0) do
      begin
        Arg := Args[I];
        if (Fixed < 0) and IsCommand(Arg) then Break;
        Line := Line + ' ' + Arg;
        Inc(I);
        if ((Arg = '--delay') or (Arg = '--repeat') or (Arg = '--window')) and (I <= High(Args)) then
        begin
          Line := Line + ' ' + Args[I];
          Inc(I);
        end
        else if Pos(' ' + Arg + ' ', Options) = 0 then
          Dec(Fixed);
      end;
      Script.Add(Line);
    end;
  end;

var
  Script: TStringList;
  I: integer;
  Arg, WantTitle: string;
begin
  I := 1;
  WantTitle := '';
  if (Length(Args) > 2) and (Args[1] = '--window') then
  begin
    WantTitle := Args[2];
    I := 3;
  end;
  if (I > High(Args)) or (Args[I] = '--help') or (Args[I] = '-h') or (Args[I] = 'help') then
  begin
    for Arg in Help do
      WriteLn(Arg);
    Halt(Ord(Length(Args) = 1) * 2);
  end;
  Arg := Args[I];

  Script := TStringList.Create;
  try
    if Arg = '-' then
      while not EOF(Input) do
      begin
        ReadLn(Arg);
        Script.Add(Arg);
      end
    else if FileExists(Arg) then
      Script.LoadFromFile(Arg)
    else if IsCommand(Arg) then
      Chain(I, Script)
    else
    begin
      WriteLn('"', Arg, '" is neither a script file nor a command; hidctl --help lists the commands');
      Halt(2);
    end;

    Application.Initialize;
    EnsureWindowManager;
    if WantTitle <> '' then Run('find ' + WantTitle);
    for I := 0 to Script.Count - 1 do
    begin
      if Failed then Break;
      try
        Run(Script[I]);
      except
        on E: Exception do
        begin
          WriteLn('bad command "', Script[I], '": ', E.Message);
          Failed := True;
        end;
      end;
    end;
  finally
    Script.Free;
  end;
  if Failed then Halt(1);
end.
