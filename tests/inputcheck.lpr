program InputCheck;
{$mode objfpc}{$H+}{$codepage utf8}
// Copyright (c) 2026 Tony Stone. 0BSD.

{ Types into its own memo and clicks, drags and scrolls on its own controls
  through MouseInput and KeyInput, and checks what the controls received.
  Exit code is the number of failures.  --ascii skips text beyond ASCII. }

uses
  {$ifdef unix}cthreads,{$endif}
  Interfaces,
  Forms,
  Controls,
  StdCtrls,
  ExtCtrls,
  Classes,
  SysUtils,
  Types,
  LCLType,
  HIDMouseAndKeyInput;

type
  TRec = class
    Clicks, DblClicks, Wheels, WheelSum: integer;
    Downs, Ups, Moves: TStringList;
    KeysDown, KeysUp: TStringList;
    procedure ButtonClick(Sender: TObject);
    procedure PadDblClick(Sender: TObject);
    procedure PadDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: integer);
    procedure PadUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: integer);
    procedure PadMove(Sender: TObject; Shift: TShiftState; X, Y: integer);
    procedure PadWheel(Sender: TObject; Shift: TShiftState; WheelDelta: integer; MousePos: TPoint; var Handled: boolean);
    procedure FormKeyDown(Sender: TObject; var Key: word; Shift: TShiftState);
    procedure FormKeyUp(Sender: TObject; var Key: word; Shift: TShiftState);
    procedure Clear;
  end;

var
  Form: TForm;
  Memo: TMemo;
  Button: TButton;
  Pad: TPanel;
  Rec: TRec;
  Checks, Failures, Skipped: integer;
  AsciiOnly: boolean;

const
  ButtonNames: array[TMouseButton] of string = ('left', 'right', 'middle', 'extra1', 'extra2');

  procedure TRec.ButtonClick(Sender: TObject);
  begin
    Inc(Clicks);
  end;

  procedure TRec.PadDblClick(Sender: TObject);
  begin
    Inc(DblClicks);
  end;

  function ShiftText(Shift: TShiftState): string;
  begin
    Result := '';
    if ssShift in Shift then Result := Result + '+shift';
    if ssCtrl in Shift then Result := Result + '+ctrl';
    if ssAlt in Shift then Result := Result + '+alt';
    if ssLeft in Shift then Result := Result + '+held';
  end;

  procedure TRec.PadDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: integer);
  begin
    Downs.Add(Format('%s %d,%d%s', [ButtonNames[Button], X, Y, ShiftText(Shift - [ssLeft, ssRight, ssMiddle, ssDouble])]));
  end;

  procedure TRec.PadUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: integer);
  begin
    Ups.Add(Format('%s %d,%d', [ButtonNames[Button], X, Y]));
  end;

  procedure TRec.PadMove(Sender: TObject; Shift: TShiftState; X, Y: integer);
  begin
    Moves.Add(Format('%d,%d%s', [X, Y, ShiftText(Shift)]));
  end;

  procedure TRec.PadWheel(Sender: TObject; Shift: TShiftState; WheelDelta: integer; MousePos: TPoint; var Handled: boolean);
  begin
    Inc(Wheels);
    Inc(WheelSum, WheelDelta);
    Handled := True;
  end;

  procedure TRec.FormKeyDown(Sender: TObject; var Key: word; Shift: TShiftState);
  begin
    KeysDown.Add(IntToStr(Key));
  end;

  procedure TRec.FormKeyUp(Sender: TObject; var Key: word; Shift: TShiftState);
  begin
    KeysUp.Add(IntToStr(Key));
  end;

  procedure TRec.Clear;
  begin
    Clicks := 0;
    DblClicks := 0;
    Wheels := 0;
    WheelSum := 0;
    Downs.Clear;
    Ups.Clear;
    Moves.Clear;
    KeysDown.Clear;
    KeysUp.Clear;
  end;

  procedure Settle(Ms: integer = 300);
  var
    Deadline: QWord;
  begin
    Deadline := GetTickCount64 + Ms;
    repeat
      Application.ProcessMessages;
      Sleep(5);
    until GetTickCount64 >= Deadline;
  end;

  procedure Check(Condition: boolean; const What, Got: string);
  begin
    Inc(Checks);
    if Condition then WriteLn('ok   ', What)
    else
    begin
      Inc(Failures);
      WriteLn('FAIL ', What, ': got ', Got);
    end;
  end;

  procedure Skip(const What, Why: string);
  begin
    Inc(Skipped);
    WriteLn('skip ', What, ': ', Why);
  end;

  // The memo's text with line ends as LF, whatever the system writes.
  function MemoText: string;
  begin
    Result := StringReplace(Memo.Text, #13#10, #10, [rfReplaceAll]);
    Result := StringReplace(Result, #13, #10, [rfReplaceAll]);
    if (Result <> '') and (Result[Length(Result)] = #10) then SetLength(Result, Length(Result) - 1);
  end;

  procedure FreshMemo;
  begin
    Memo.Clear;
    Memo.SetFocus;
    Settle(150);
  end;

  procedure Typed(const What, Text, Wanted: string);
  begin
    FreshMemo;
    KeyInput.PressString(Text);
    Settle(400);
    Check(MemoText = Wanted, What, '"' + MemoText + '"');
  end;

  function PadPoint(X, Y: integer): TPoint;
  begin
    Result := Pad.ClientToScreen(Point(X, Y));
  end;

  procedure KeyboardChecks;
  var
    HadCaps, HadNum: boolean;
  begin
    Typed('letters and digits', 'Hello World 0123456789', 'Hello World 0123456789');
    Typed('every US punctuation mark', '`~!@#$%^&*()-_=+[{]}\|;:''",<.>/?',
      '`~!@#$%^&*()-_=+[{]}\|;:''",<.>/?');
    Typed('tab, and CR, LF and CRLF as one Enter each', 'a'#9'b'#13'c'#10'd'#13#10'e',
      'a'#9'b'#10'c'#10'd'#10'e');
    if AsciiOnly then Skip('text beyond ASCII', '--ascii')
    else
    begin
      Typed('Latin-1 and Latin Extended', 'café ñ ü ß Œ ł', 'café ñ ü ß Œ ł');
      Typed('Cyrillic, Greek and CJK', 'Жук λ 漢字', 'Жук λ 漢字');
      Typed('beyond 16 bits', 'a😀b', 'a😀b');
    end;

    // a hotkey's Shift may still be down when typing starts
    FreshMemo;
    KeyInput.Down(VK_SHIFT);
    KeyInput.PressString('abc');
    KeyInput.Up(VK_SHIFT);
    Settle(400);
    Check(MemoText = 'abc', 'typing lets go of a held Shift first', '"' + MemoText + '"');

    // Caps Lock on: text still comes out as written
    HadCaps := KeyInput.GetCapsLockState;
    if not HadCaps then KeyInput.ToggleCapsLock;
    Settle(300);
    Check(KeyInput.GetCapsLockState, 'Caps Lock reads as on after ToggleCapsLock', 'off');
    if AsciiOnly then Typed('with Caps Lock on', 'aBc Xy 1!', 'aBc Xy 1!')
    else
      Typed('with Caps Lock on', 'aBc Xy 1! é', 'aBc Xy 1! é');
    KeyInput.ToggleCapsLock;
    Settle(300);
    Check(not KeyInput.GetCapsLockState, 'Caps Lock reads as off after toggling again', 'on');
    if HadCaps then KeyInput.ToggleCapsLock;

    // modifiers held around a key
    FreshMemo;
    KeyInput.PressString('select me');
    Settle(200);
    KeyInput.Apply([ssCtrl]);
    KeyInput.Press(VK_A);
    KeyInput.Unapply([ssCtrl]);
    Settle(300);
    Check(Memo.SelLength = Length('select me'), 'Ctrl+A through Apply and Press', IntToStr(Memo.SelLength));

    // arrows must stay arrows with Num Lock on (Windows' extended keys)
    {$ifndef darwin}
    HadNum := KeyInput.GetNumLockState;
    if not HadNum then KeyInput.ToggleNumLock;
    Settle(200);
    {$else}
    HadNum := True;
    {$endif}
    FreshMemo;
    KeyInput.PressString('abc');
    KeyInput.Press(VK_LEFT);
    KeyInput.Press(VK_LEFT);
    KeyInput.PressString('X');
    KeyInput.Press(VK_END);
    KeyInput.Press(VK_BACK);
    Settle(400);
    Check(MemoText = 'aXb', 'Left, End and Backspace with Num Lock on', '"' + MemoText + '"');
    if not HadNum then KeyInput.ToggleNumLock;

    // Down and Up are separate events
    Rec.Clear;
    KeyInput.Down(VK_F5);
    Settle(200);
    Check((Rec.KeysDown.IndexOf(IntToStr(VK_F5)) >= 0) and (Rec.KeysUp.IndexOf(IntToStr(VK_F5)) < 0),
      'Down sends only the press', 'down [' + Rec.KeysDown.CommaText + '] up [' + Rec.KeysUp.CommaText + ']');
    KeyInput.Up(VK_F5);
    Settle(200);
    Check(Rec.KeysUp.IndexOf(IntToStr(VK_F5)) >= 0, 'Up sends the release', '[' + Rec.KeysUp.CommaText + ']');

    FreshMemo;
    KeyInput.PressString('slow', 60);
    Settle(400);
    Check(MemoText = 'slow', 'PressString with an interval', '"' + MemoText + '"');
  end;

  procedure MouseChecks;
  var
    P, Target: TPoint;
    T0: QWord;
  begin
    Target := PadPoint(40, 30);
    MouseInput.Move([], Target.X, Target.Y);
    Settle(200);
    P := Mouse.CursorPos;
    Check((Abs(P.X - Target.X) <= 1) and (Abs(P.Y - Target.Y) <= 1), 'Move to a screen point',
      Format('%d,%d for %d,%d', [P.X, P.Y, Target.X, Target.Y]));

    MouseInput.MoveBy([], 25, -10);
    Settle(200);
    P := Mouse.CursorPos;
    Check((Abs(P.X - Target.X - 25) <= 1) and (Abs(P.Y - Target.Y + 10) <= 1), 'MoveBy',
      Format('%d,%d', [P.X - Target.X, P.Y - Target.Y]));

    Target := PadPoint(150, 90);
    T0 := GetTickCount64;
    MouseInput.Move([], Pad, 150, 90, 300);
    Settle(200);
    P := Mouse.CursorPos;
    Check((Abs(P.X - Target.X) <= 1) and (Abs(P.Y - Target.Y) <= 1) and (GetTickCount64 - T0 >= 250),
      'Move over a duration ends on the point, in about that time',
      Format('%d,%d after %d ms', [P.X - Target.X, P.Y - Target.Y, GetTickCount64 - T0]));

    // far enough apart in time not to make a double click
    Settle(700);
    Rec.Clear;
    MouseInput.Click(mbLeft, [], Button, Button.Width div 2, Button.Height div 2);
    Settle(400);
    Check(Rec.Clicks = 1, 'Click on a button', IntToStr(Rec.Clicks));

    // far enough apart in time not to make a double click
    Settle(700);
    Rec.Clear;
    MouseInput.Click(mbRight, [], Pad, 30, 20);
    Settle(300);
    Check((Rec.Downs.Count = 1) and (Rec.Downs[0] = 'right 30,20') and (Rec.Ups.Count = 1),
      'right click lands where it was sent', '[' + Rec.Downs.CommaText + ']');

    // far enough apart in time not to make a double click
    Settle(700);
    Rec.Clear;
    MouseInput.Click(mbMiddle, [], Pad, 31, 21);
    Settle(300);
    Check((Rec.Downs.Count = 1) and (Rec.Downs[0] = 'middle 31,21'), 'middle click', '[' + Rec.Downs.CommaText + ']');

    // far enough apart in time not to make a double click
    Settle(700);
    Rec.Clear;
    MouseInput.Click(mbExtra1, [], Pad, 32, 22);
    Settle(300);
    Check((Rec.Downs.Count = 1) and (Rec.Downs[0] = 'extra1 32,22') and (Rec.Wheels = 0),
      'first extra button (back)', '[' + Rec.Downs.CommaText + '] wheels ' + IntToStr(Rec.Wheels));

    // far enough apart in time not to make a double click
    Settle(700);
    Rec.Clear;
    MouseInput.Click(mbExtra2, [], Pad, 33, 23);
    Settle(300);
    Check((Rec.Downs.Count = 1) and (Rec.Downs[0] = 'extra2 33,23') and (Rec.Wheels = 0),
      'second extra button (forward)', '[' + Rec.Downs.CommaText + '] wheels ' + IntToStr(Rec.Wheels));

    // far enough apart in time not to make a double click
    Settle(700);
    Rec.Clear;
    MouseInput.Click(mbLeft, [ssShift], Pad, 40, 25);
    Settle(300);
    Check((Rec.Downs.Count = 1) and (Rec.Downs[0] = 'left 40,25+shift'), 'Shift+click carries Shift',
      '[' + Rec.Downs.CommaText + ']');

    // far enough apart in time not to make a double click
    Settle(700);
    Rec.Clear;
    MouseInput.Click(mbLeft, [ssCtrl], Pad, 41, 26);
    Settle(300);
    Check((Rec.Downs.Count = 1) and (Rec.Downs[0] = 'left 41,26+ctrl'), 'Ctrl+click carries Ctrl',
      '[' + Rec.Downs.CommaText + ']');

    Rec.Clear;
    MouseInput.DblClick(mbLeft, [], Pad, 50, 30);
    Settle(400);
    Check(Rec.DblClicks = 1, 'DblClick', IntToStr(Rec.DblClicks));

    Settle(600);
    Rec.Clear;
    MouseInput.ScrollUp([], Pad, 60, 40);
    Settle(300);
    Check((Rec.Wheels = 1) and (Rec.WheelSum > 0), 'ScrollUp is one notch up', Format('%d notches, %d', [Rec.Wheels, Rec.WheelSum]));

    Rec.Clear;
    MouseInput.ScrollDown([], Pad, 60, 40);
    Settle(300);
    Check((Rec.Wheels = 1) and (Rec.WheelSum < 0), 'ScrollDown is one notch down', Format('%d notches, %d', [Rec.Wheels, Rec.WheelSum]));

    Rec.Clear;
    MouseInput.Down(mbLeft, [], Pad, 10, 10);
    Settle(100);
    MouseInput.Move([], Pad, 60, 40, 150);
    Settle(100);
    MouseInput.Up(mbLeft, [], Pad, 60, 40);
    Settle(300);
    Check((Rec.Downs.Count = 1) and (Rec.Downs[0] = 'left 10,10') and (Rec.Ups.Count = 1) and (Rec.Ups[0] = 'left 60,40') and
      (Pos('+held', Rec.Moves.CommaText) > 0), 'drag: down, moves with the button held, up at the end',
      'down [' + Rec.Downs.CommaText + '] up [' + Rec.Ups.CommaText + ']');
  end;

var
  KeyboardReaches: boolean;
begin
  AsciiOnly := (ParamCount > 0) and (ParamStr(1) = '--ascii');
  Application.Initialize;
  Rec := TRec.Create;
  Rec.Downs := TStringList.Create;
  Rec.Ups := TStringList.Create;
  Rec.Moves := TStringList.Create;
  Rec.KeysDown := TStringList.Create;
  Rec.KeysUp := TStringList.Create;

  Form := TForm.CreateNew(nil);
  Form.Caption := 'inputcheck';
  Form.SetBounds(60, 60, 520, 380);
  Form.KeyPreview := True;
  Form.OnKeyDown := @Rec.FormKeyDown;
  Form.OnKeyUp := @Rec.FormKeyUp;
  Memo := TMemo.Create(Form);
  Memo.Parent := Form;
  Memo.SetBounds(10, 10, 500, 120);
  Memo.WantTabs := True;
  Memo.WantReturns := True;
  Button := TButton.Create(Form);
  Button.Parent := Form;
  Button.SetBounds(10, 140, 120, 32);
  Button.Caption := 'Press';
  Button.OnClick := @Rec.ButtonClick;
  Pad := TPanel.Create(Form);
  Pad.Parent := Form;
  Pad.SetBounds(10, 180, 500, 190);
  Pad.Caption := '';
  Pad.OnMouseDown := @Rec.PadDown;
  Pad.OnMouseUp := @Rec.PadUp;
  Pad.OnMouseMove := @Rec.PadMove;
  Pad.OnMouseWheel := @Rec.PadWheel;
  Pad.OnDblClick := @Rec.PadDblClick;
  Form.Show;
  Settle(800);
  // the pointer over the form gives it focus where nothing else does
  MouseInput.Click(mbLeft, [], Memo, 20, 20);
  Settle(400);

  // A program started over SSH on a Mac never becomes the active one.
  Rec.Clear;
  KeyInput.Press(VK_F6);
  Settle(300);
  KeyboardReaches := Rec.KeysDown.IndexOf(IntToStr(VK_F6)) >= 0;
  if KeyboardReaches then KeyboardChecks
  {$ifdef darwin}
  else
    Skip('keyboard', 'keys do not reach this window; on a Mac run it from the desktop')
  {$else}
  else
    Check(False, 'a key reaches the window', 'nothing')
  {$endif}
  ;
  MouseChecks;

  Form.Free;
  WriteLn(Checks, ' checks, ', Failures, ' failed, ', Skipped, ' skipped');
  ExitCode := Failures;
end.
