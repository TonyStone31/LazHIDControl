unit HIDKeyInputIntf;

{$mode objfpc}{$H+}
// Copyright (c) 2026 Tony Stone. 0BSD.

{ The keyboard as the rest of the package sees it.  A key is a place on the
  keyboard, named by its LCL virtual key; what character it gives is up to
  the user's layout.  PressString types characters, whatever the layout. }

interface

uses
  Classes, SysUtils, LCLType;

type
  TKeyInput = class
  private
    FTyping: boolean;
    FCapsAtStart: boolean;
    procedure ReleaseHeldKeys;
  protected
    procedure DoDown(Key: word); virtual; abstract;
    procedure DoUp(Key: word); virtual; abstract;
    // Called after PressString has typed its last character, to put back
    // anything the backend changed for the length of the string.
    procedure StringDone; virtual;
    // Waits Ms milliseconds while the application keeps processing messages.
    procedure Pause(Ms: cardinal);
    // True while PressString is running.
    property Typing: boolean read FTyping;
    // Whether Caps Lock was on when the running PressString began.
    property CapsLockAtStart: boolean read FCapsAtStart;
  public
    procedure Down(Key: word);
    procedure Up(Key: word);
    procedure Press(Key: word);
    procedure Apply(Shift: TShiftState);
    procedure Unapply(Shift: TShiftState);
    procedure PressString(StringValue: string; Interval: cardinal = 0);
    procedure PressASCIIChar(ch: char); virtual;
    procedure PressUnicodeChar(unicode: cardinal); virtual;
    function GetCapsLockState: boolean; virtual; abstract;
    function GetNumLockState: boolean; virtual; abstract;
    function GetScrollLockState: boolean; virtual; abstract;
    procedure ToggleCapsLock; virtual; abstract;
    procedure ToggleNumLock; virtual; abstract;
    procedure ToggleScrollLock; virtual; abstract;
  end;

implementation

uses
  Forms, LazUTF8;

procedure TKeyInput.StringDone;
begin
end;

procedure TKeyInput.Pause(Ms: cardinal);
var
  Deadline: QWord;
begin
  Deadline := GetTickCount64 + Ms;
  repeat
    Application.ProcessMessages;
    if GetTickCount64 < Deadline then Sleep(1);
  until GetTickCount64 >= Deadline;
end;

procedure TKeyInput.Down(Key: word);
begin
  DoDown(Key);
  Application.ProcessMessages;
end;

procedure TKeyInput.Up(Key: word);
begin
  DoUp(Key);
  Application.ProcessMessages;
end;

procedure TKeyInput.Press(Key: word);
begin
  Down(Key);
  Up(Key);
end;

type
  TModifierKey = record
    State: TShiftStateEnum;
    Key: word;
  end;

const
  { pressed in this order, let go in the reverse }
  ModifierKeys: array[0..2] of TModifierKey = (
    (State: ssCtrl; Key: VK_CONTROL),
    (State: ssAlt; Key: VK_MENU),
    (State: ssShift; Key: VK_SHIFT));

procedure TKeyInput.Apply(Shift: TShiftState);
var
  I: integer;
begin
  for I := 0 to High(ModifierKeys) do
    if ModifierKeys[I].State in Shift then Down(ModifierKeys[I].Key);
end;

procedure TKeyInput.Unapply(Shift: TShiftState);
var
  I: integer;
begin
  for I := High(ModifierKeys) downto 0 do
    if ModifierKeys[I].State in Shift then Up(ModifierKeys[I].Key);
end;

// Typing is often started by a hotkey whose keys are still held down.
procedure TKeyInput.ReleaseHeldKeys;
const
  Modifiers: array[0..5] of word = (VK_SHIFT, VK_RSHIFT, VK_CONTROL, VK_RCONTROL, VK_MENU, VK_RMENU);
var
  Key: word;
begin
  for Key in Modifiers do DoUp(Key);
  for Key := VK_A to VK_Z do DoUp(Key);
  for Key := VK_0 to VK_9 do DoUp(Key);
  for Key := VK_F1 to VK_F12 do DoUp(Key);
  Application.ProcessMessages;
end;

procedure TKeyInput.PressString(StringValue: string; Interval: cardinal);
var
  P, Stop: PChar;
  Len: integer;
  Code: cardinal;
begin
  if StringValue = '' then Exit;
  ReleaseHeldKeys;
  FCapsAtStart := GetCapsLockState;
  FTyping := True;
  try
    P := PChar(StringValue);
    Stop := P + Length(StringValue);
    while P < Stop do
    begin
      if (Interval > 0) and (P > PChar(StringValue)) then Pause(Interval);
      case P^ of
        #13:
        begin
          Press(VK_RETURN);
          Inc(P);
          if (P < Stop) and (P^ = #10) then Inc(P);
        end;
        #10:
        begin
          Press(VK_RETURN);
          Inc(P);
        end;
        #9:
        begin
          Press(VK_TAB);
          Inc(P);
        end;
        #0..#8, #11..#12, #14..#127:
        begin
          PressASCIIChar(P^);
          Inc(P);
        end;
        else
        begin
          Code := UTF8CodepointToUnicode(P, Len);
          if Len < 1 then Len := 1;
          PressUnicodeChar(Code);
          Inc(P, Len);
        end;
      end;
    end;
  finally
    FTyping := False;
    StringDone;
  end;
end;

// The character as a US keyboard types it, the way a backend that cannot
// see the user's layout has to.
procedure TKeyInput.PressASCIIChar(ch: char);
const
  Plain = '`-=[]\;'',./';
  Shifted = '~_+{}|:"<>?';
  PunctuationKeys: array[1..11] of word = (VK_LCL_TILDE, VK_LCL_MINUS, VK_LCL_EQUAL,
    VK_LCL_OPEN_BRACKET, VK_LCL_CLOSE_BRACKET, VK_LCL_BACKSLASH, VK_LCL_SEMI_COMMA,
    VK_LCL_QUOTE, VK_LCL_COMMA, VK_LCL_POINT, VK_LCL_SLASH);
  // the shifted digits, from 0 to 9
  OverDigits = ')!@#$%^&*(';
var
  Key: word;
  NeedShift: boolean;
  I: integer;
begin
  Key := 0;
  NeedShift := False;
  case ch of
    'a'..'z':
    begin
      Key := VK_A + Ord(ch) - Ord('a');
      NeedShift := GetCapsLockState;
    end;
    'A'..'Z':
    begin
      Key := VK_A + Ord(ch) - Ord('A');
      NeedShift := not GetCapsLockState;
    end;
    '0'..'9': Key := VK_0 + Ord(ch) - Ord('0');
    ' ': Key := VK_SPACE;
    #8: Key := VK_BACK;
    #9: Key := VK_TAB;
    #10, #13: Key := VK_RETURN;
    #27: Key := VK_ESCAPE;
    else
    begin
      I := Pos(ch, Plain);
      if I > 0 then Key := PunctuationKeys[I]
      else
      begin
        I := Pos(ch, Shifted);
        if I > 0 then
        begin
          Key := PunctuationKeys[I];
          NeedShift := True;
        end
        else
        begin
          I := Pos(ch, OverDigits);
          if I > 0 then
          begin
            Key := VK_0 + I - 1;
            NeedShift := True;
          end;
        end;
      end;
    end;
  end;
  if Key = 0 then Exit;
  if NeedShift then Down(VK_SHIFT);
  try
    Press(Key);
  finally
    if NeedShift then Up(VK_SHIFT);
  end;
end;

// Nothing to do here: a key cannot type what the layout lacks.  Backends
// that can send any character override this.
procedure TKeyInput.PressUnicodeChar(unicode: cardinal);
begin
end;

end.
