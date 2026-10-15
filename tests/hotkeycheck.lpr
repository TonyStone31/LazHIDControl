program HotkeyCheck;
{$mode objfpc}{$H+}
// Copyright (c) 2026 Tony Stone. 0BSD.

{ Registers global hotkeys through THIDHotkey and CreateHotkey, presses them
  with KeyInput, and checks what fired and what reached the focused window.
  Exit code is the number of failures.  Run with --hold it only holds
  Ctrl+Alt+F8 for a few seconds, as the other program in the conflict check. }

uses
  {$ifdef unix}cthreads,{$endif}
  Interfaces,
  Forms,
  Controls,
  StdCtrls,
  Classes,
  SysUtils,
  Menus,
  LCLType,
  Process,
  HIDHotkey,
  HotkeyInput,
  HIDMouseAndKeyInput;

type
  TRec = class
    Hits: array[1..8] of integer;
    KeysDown: TStringList;
    SelfStop: THIDHotkey;
    procedure Hit1(Sender: TObject);
    procedure Hit2(Sender: TObject);
    procedure Hit3(Sender: TObject);
    procedure Hit4(Sender: TObject);
    procedure Hit5(Sender: TObject);
    procedure Hit6(Sender: TObject);
    procedure Hit7(Sender: TObject);
    procedure RawHit(Sender: TObject; Key: word; Shift: TShiftState);
    procedure FormKeyDown(Sender: TObject; var Key: word; Shift: TShiftState);
  end;

var
  Form: TForm;
  Memo: TMemo;
  Rec: TRec;
  Checks, Failures, Skipped: integer;
  KeyboardReaches: boolean;

  procedure TRec.Hit1(Sender: TObject);
  begin
    Inc(Hits[1]);
  end;

  procedure TRec.Hit2(Sender: TObject);
  begin
    Inc(Hits[2]);
  end;

  procedure TRec.Hit3(Sender: TObject);
  begin
    Inc(Hits[3]);
  end;

  procedure TRec.Hit4(Sender: TObject);
  begin
    Inc(Hits[4]);
  end;

  // unregisters itself from inside its own handler
  procedure TRec.Hit5(Sender: TObject);
  begin
    Inc(Hits[5]);
    SelfStop.Enabled := False;
  end;

  procedure TRec.Hit6(Sender: TObject);
  begin
    Inc(Hits[6]);
  end;

  procedure TRec.Hit7(Sender: TObject);
  begin
    Inc(Hits[7]);
  end;

  procedure TRec.RawHit(Sender: TObject; Key: word; Shift: TShiftState);
  begin
    Inc(Hits[8]);
  end;

  procedure TRec.FormKeyDown(Sender: TObject; var Key: word; Shift: TShiftState);
  begin
    KeysDown.Add(IntToStr(Key));
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
    Flush(Output);
  end;

  procedure Skip(const What, Why: string);
  begin
    Inc(Skipped);
    WriteLn('skip ', What, ': ', Why);
    Flush(Output);
  end;

  procedure Combo(Shift: TShiftState; Key: word);
  begin
    Rec.KeysDown.Clear;
    // a key the backend cannot send must not leave the modifiers held
    try
      KeyInput.Apply(Shift);
      try
        KeyInput.Press(Key);
      finally
        KeyInput.Unapply(Shift);
      end;
    except
      on E: Exception do WriteLn('note: pressing key ', Key, ' failed: ', E.Message);
    end;
    Settle(400);
  end;

  function NewHotkey(Key: word; Shift: TShiftState; Handler: TNotifyEvent): THIDHotkey;
  begin
    Result := THIDHotkey.Create(Form);
    Result.ShortCut := ShortCut(Key, Shift);
    Result.OnHotkey := Handler;
    Result.Enabled := True;
  end;

  procedure Hold;
  var
    Held: THotkey;
  begin
    Held := CreateHotkey(VK_F8, [ssCtrl, ssAlt], nil);
    Held.Register;
    if Held.Registered then WriteLn('holding')
    else
      WriteLn('could not hold');
    Flush(Output);
    Settle(6000);
    Held.Free;
  end;

var
  HK1, HK2, HK3, HK4, HK6, HK7: THIDHotkey;
  Raw: THotkey;
  Holder: TProcess;
  Line: string;
  Buf: array[0..255] of char;
  N: integer;
  Deadline: QWord;
  HadCaps, HadNum: boolean;
begin
  Application.Initialize;
  if (ParamCount > 0) and (ParamStr(1) = '--hold') then
  begin
    Hold;
    Exit;
  end;
  Rec := TRec.Create;
  Rec.KeysDown := TStringList.Create;

  Form := TForm.CreateNew(nil);
  Form.Caption := 'hotkeycheck';
  Form.SetBounds(80, 80, 400, 200);
  Form.KeyPreview := True;
  Form.OnKeyDown := @Rec.FormKeyDown;
  Memo := TMemo.Create(Form);
  Memo.Parent := Form;
  Memo.Align := alClient;
  Form.Show;
  Settle(800);
  MouseInput.Click(mbLeft, [], Memo, 20, 20);
  Settle(400);

  Rec.KeysDown.Clear;
  KeyInput.Press(VK_F6);
  Settle(300);
  KeyboardReaches := Rec.KeysDown.IndexOf(IntToStr(VK_F6)) >= 0;
  if not KeyboardReaches then
    WriteLn('note: keys do not reach this window, so checks of what the window sees are skipped');

  HK1 := NewHotkey(VK_F9, [ssCtrl, ssShift], @Rec.Hit1);
  Check(HK1.Registered, 'Ctrl+Shift+F9 registers', HK1.LastError);
  Combo([ssCtrl, ssShift], VK_F9);
  Check(Rec.Hits[1] = 1, 'pressing it runs OnHotkey once', IntToStr(Rec.Hits[1]));
  if KeyboardReaches then
    Check(Rec.KeysDown.IndexOf(IntToStr(VK_F9)) < 0, 'the window does not see the hotkey''s key', '[' + Rec.KeysDown.CommaText + ']')
  else
    Skip('the window does not see the hotkey''s key', 'keys do not reach this window');

  Combo([], VK_F9);
  Check(Rec.Hits[1] = 1, 'F9 alone does not run it', IntToStr(Rec.Hits[1]));
  if KeyboardReaches then
    Check(Rec.KeysDown.IndexOf(IntToStr(VK_F9)) >= 0, 'F9 alone reaches the window', '[' + Rec.KeysDown.CommaText + ']')
  else
    Skip('F9 alone reaches the window', 'keys do not reach this window');
  Combo([ssCtrl], VK_F9);
  Check(Rec.Hits[1] = 1, 'Ctrl+F9 does not run it', IntToStr(Rec.Hits[1]));

  HadCaps := KeyInput.GetCapsLockState;
  if not HadCaps then KeyInput.ToggleCapsLock;
  Settle(200);
  Combo([ssCtrl, ssShift], VK_F9);
  Check(Rec.Hits[1] = 2, 'it still runs with Caps Lock on', IntToStr(Rec.Hits[1]));
  if not HadCaps then KeyInput.ToggleCapsLock;
  {$ifndef darwin}
  HadNum := KeyInput.GetNumLockState;
  KeyInput.ToggleNumLock;
  Settle(200);
  Combo([ssCtrl, ssShift], VK_F9);
  Check(Rec.Hits[1] = 3, 'it still runs with Num Lock flipped', IntToStr(Rec.Hits[1]));
  if KeyInput.GetNumLockState <> HadNum then KeyInput.ToggleNumLock;
  {$else}
  Rec.Hits[1] := 3;
  {$endif}
  Settle(200);

  HK2 := NewHotkey(VK_F9, [ssCtrl, ssShift], @Rec.Hit2);
  Check(not HK2.Registered and (HK2.LastError <> ''), 'the same combination twice is refused, with a reason',
    BoolToStr(HK2.Registered, True) + ' "' + HK2.LastError + '"');
  Combo([ssCtrl, ssShift], VK_F9);
  Check((Rec.Hits[1] = 4) and (Rec.Hits[2] = 0), 'the first one keeps it', Format('%d and %d', [Rec.Hits[1], Rec.Hits[2]]));

  HK3 := NewHotkey(VK_K, [ssCtrl, ssAlt], @Rec.Hit3);
  Combo([ssCtrl, ssAlt], VK_K);
  Check(HK3.Registered and (Rec.Hits[3] = 1), 'a letter: Ctrl+Alt+K', HK3.LastError + ' ' + IntToStr(Rec.Hits[3]));
  HK4 := NewHotkey(VK_5, [ssCtrl, ssAlt], @Rec.Hit4);
  Combo([ssCtrl, ssAlt], VK_5);
  Check(HK4.Registered and (Rec.Hits[4] = 1), 'a digit: Ctrl+Alt+5', HK4.LastError + ' ' + IntToStr(Rec.Hits[4]));
  HK6 := NewHotkey(VK_HOME, [ssCtrl, ssAlt], @Rec.Hit6);
  Combo([ssCtrl, ssAlt], VK_HOME);
  Check(HK6.Registered and (Rec.Hits[6] = 1), 'a navigation key: Ctrl+Alt+Home', HK6.LastError + ' ' + IntToStr(Rec.Hits[6]));
  HK7 := NewHotkey(VK_F13, [ssCtrl, ssShift], @Rec.Hit7);
  Combo([ssCtrl, ssShift], VK_F13);
  Check(HK7.Registered and (Rec.Hits[7] = 1), 'past F12: Ctrl+Shift+F13', HK7.LastError + ' ' + IntToStr(Rec.Hits[7]));

  HK1.Enabled := False;
  Check(not HK1.Registered, 'Enabled := False unregisters', 'still registered');
  Combo([ssCtrl, ssShift], VK_F9);
  Check(Rec.Hits[1] = 4, 'an unregistered hotkey does not run', IntToStr(Rec.Hits[1]));
  if KeyboardReaches then
    Check(Rec.KeysDown.IndexOf(IntToStr(VK_F9)) >= 0, 'and its key reaches the window again', '[' + Rec.KeysDown.CommaText + ']')
  else
    Skip('and its key reaches the window again', 'keys do not reach this window');
  HK1.Enabled := True;
  Combo([ssCtrl, ssShift], VK_F9);
  Check(HK1.Registered and (Rec.Hits[1] = 5), 'registered again, it runs again', IntToStr(Rec.Hits[1]));

  // F7, not F10: Shift+F10 opens a context menu once it is no longer a hotkey
  Rec.SelfStop := NewHotkey(VK_F7, [ssCtrl, ssShift], @Rec.Hit5);
  Combo([ssCtrl, ssShift], VK_F7);
  Combo([ssCtrl, ssShift], VK_F7);
  Check((Rec.Hits[5] = 1) and not Rec.SelfStop.Registered, 'a hotkey can unregister itself from its own handler',
    IntToStr(Rec.Hits[5]));

  Raw := CreateHotkey(VK_F11, [ssCtrl, ssShift], @Rec.RawHit);
  Raw.Register;
  Combo([ssCtrl, ssShift], VK_F11);
  Check(Raw.Registered and (Raw.Key = VK_F11) and (Raw.Shift = [ssCtrl, ssShift]) and (Rec.Hits[8] = 1),
    'CreateHotkey without the component', IntToStr(Rec.Hits[8]));
  Raw.Free;
  Combo([ssCtrl, ssShift], VK_F11);
  Check(Rec.Hits[8] = 1, 'freeing it unregisters', IntToStr(Rec.Hits[8]));

  // another program holding a combination; macOS gives it to both
  {$ifdef darwin}
  Skip('a combination another program holds is refused', 'macOS lets two programs register the same one');
  {$else}
  Holder := TProcess.Create(nil);
  Holder.Executable := ParamStr(0);
  Holder.Parameters.Add('--hold');
  Holder.Options := [poUsePipes];
  Holder.Execute;
  Line := '';
  Deadline := GetTickCount64 + 8000;
  while (Pos(LineEnding, Line) = 0) and (GetTickCount64 < Deadline) do
  begin
    if Holder.Output.NumBytesAvailable > 0 then
    begin
      N := Holder.Output.Read(Buf, SizeOf(Buf));
      Line := Line + Copy(Buf, 1, N);
    end;
    Settle(50);
  end;
  if Pos('holding', Line) = 1 then
  begin
    HK2.ShortCut := ShortCut(VK_F8, [ssCtrl, ssAlt]);
    HK2.Enabled := True;
    Check(not HK2.Registered and (HK2.LastError <> ''), 'a combination another program holds is refused',
      BoolToStr(HK2.Registered, True));
    Holder.WaitOnExit;
    Settle(300);
    HK2.Enabled := True;
    Check(HK2.Registered, 'and is free once that program is gone', HK2.LastError);
  end
  else
    Check(False, 'the other program holds Ctrl+Alt+F8', '"' + Trim(Line) + '"');
  Holder.Free;
  {$endif}

  Form.Free;
  WriteLn(Checks, ' checks, ', Failures, ' failed, ', Skipped, ' skipped');
  ExitCode := Failures;
end.
