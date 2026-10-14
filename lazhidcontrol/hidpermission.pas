unit HIDPermission;

{$mode objfpc}{$H+}
// Copyright (c) 2026 Tony Stone. GPL-2.0-or-later; see COPYING.GPL2.

{ What this desktop wants allowed before the program may type, click, watch
  the keyboard or capture the screen - and getting it allowed, with the user
  told plainly what they are allowing.

  Linux under Wayland: a udev rule, one small file under /etc/udev/rules.d,
  installed through pkexec so the system asks for the administrator password
  itself.  macOS: the Privacy & Security panes, through the system's own
  prompts.  Linux under X11, and Windows: nothing to do. }

interface

uses Classes, SysUtils;

type
  // Typing and the mouse; watching keys and clicks; screenshots of other programs.
  THIDNeed = (hnInput, hnWatching, hnCapture);
  THIDNeeds = set of THIDNeed;

  THIDPermissionErrorEvent = procedure(Sender: TObject; const Message: string) of object;

  THIDPermission = class(TComponent)
  private
    Wanted: THIDNeeds;
    ErrorText: string;
    ErrorHandler: THIDPermissionErrorEvent;
    function Fail(const Message: string): boolean;
  public
    constructor Create(AOwner: TComponent); override;
    // Of Needs, what this desktop has not allowed yet. Empty means go ahead.
    function Missing: THIDNeeds;
    function Granted: boolean;
    // The plain words for the user: what is being allowed, to whom, how to take it back.
    function Explanation: string;
    // The exact files and lines a Grant installs, for showing; empty where there are none.
    function Details: string;
    // The desktop's own way of allowing it. True when nothing is missing afterwards.
    function Grant: boolean;
    // Linux: removes the rule files. macOS: opens the pane for the user to untick.
    function Revoke: boolean;
    // The package's dialog - Explanation, Details, Allow or Not now - then Grant.
    function Ask: boolean;
    property LastError: string read ErrorText;
  published
    // What the program does, so the user is asked for that and nothing more.
    // hnInput is THIDKeyboard and THIDMouse, hnWatching is THIDInputMonitor,
    // hnCapture is THIDScreenshot of other programs' windows (macOS only;
    // Wayland's portal asks by itself).
    property Needs: THIDNeeds read Wanted write Wanted default [hnInput];
    // Granting failed - the password was not given, pkexec is missing, or the
    // desktop said no. The message stays in LastError; no dialog is shown.
    property OnError: THIDPermissionErrorEvent read ErrorHandler write ErrorHandler;
  end;

implementation

uses Forms, Controls, Process, HIDPermissionDialog
  {$if defined(linux)}, BaseUnix{$elseif defined(darwin)}, MacOSAll{$endif};

{$ifdef linux}
const
  RulesDir = '/etc/udev/rules.d/';

function OnWayland: boolean;
begin
  Result := GetEnvironmentVariable('WAYLAND_DISPLAY') <> '';
end;

// The rule file each need lives in, and its one line.
function RuleFile(Need: THIDNeed): string;
begin
  case Need of
    hnInput: Result := RulesDir + '60-lazhidcontrol-uinput.rules';
    hnWatching: Result := RulesDir + '60-lazhidcontrol-input.rules';
    else
      Result := '';
  end;
end;

function RuleLine(Need: THIDNeed): string;
begin
  case Need of
    hnInput: Result := 'KERNEL=="uinput", SUBSYSTEM=="misc", TAG+="uaccess", OPTIONS+="static_node=uinput"';
    hnWatching: Result := 'SUBSYSTEM=="input", KERNEL=="event*", TAG+="uaccess"';
    else
      Result := '';
  end;
end;

// Every input device, since the rule covers them all.
function EventsReadable: boolean;
var
  Found: TSearchRec;
begin
  Result := True;
  if FindFirst('/dev/input/event*', faAnyFile, Found) = 0 then
  begin
    repeat
      if fpAccess('/dev/input/' + Found.Name, R_OK) <> 0 then Result := False;
    until FindNext(Found) <> 0;
    FindClose(Found);
  end;
end;

{ pkexec runs the script as root after the desktop's own password dialog.
  126 is the user closing that dialog; 127 is not being allowed to, which
  is also what a program started outside the desktop session gets. }
function RunAsRoot(const Script: string; out Why: string): boolean;
var
  P: TProcess;
  Err: TStringList;
begin
  Why := '';
  P := TProcess.Create(nil);
  Err := TStringList.Create;
  try
    P.Executable := 'pkexec';
    P.Parameters.Add('sh');
    P.Parameters.Add('-c');
    P.Parameters.Add(Script);
    P.Options := [poUsePipes];
    try
      P.Execute;
    except
      on E: Exception do
      begin
        Why := 'pkexec could not be run: ' + E.Message;
        Exit(False);
      end;
    end;
    // the password dialog can stay up a while; the program's windows stay alive meanwhile
    while P.Running do
    begin
      Application.ProcessMessages;
      Sleep(50);
    end;
    Err.LoadFromStream(P.Stderr);
    case P.ExitStatus of
      0: Result := True;
      126: Why := 'The administrator password was not given.';
      127: Why := 'The system did not allow it: this account is not an administrator, or no password dialog is available in this session.';
      else
        Why := Trim(Err.Text);
    end;
    Result := P.ExitStatus = 0;
  finally
    Err.Free;
    P.Free;
  end;
end;

// udev reads the rules again and applies them to the devices already there;
// settle waits until it has.
const
  Reload = 'udevadm control --reload' + LineEnding + 'udevadm trigger --subsystem-match=input --subsystem-match=misc' + LineEnding + 'udevadm settle' + LineEnding;
{$endif}

{$ifdef darwin}
{$linkframework ApplicationServices}
function AXIsProcessTrustedWithOptions(Options: CFDictionaryRef): boolean; cdecl; external name 'AXIsProcessTrustedWithOptions';
function CGPreflightListenEventAccess: boolean; cdecl; external name 'CGPreflightListenEventAccess';
function CGRequestListenEventAccess: boolean; cdecl; external name 'CGRequestListenEventAccess';
function CGPreflightScreenCaptureAccess: boolean; cdecl; external name 'CGPreflightScreenCaptureAccess';
function CGRequestScreenCaptureAccess: boolean; cdecl; external name 'CGRequestScreenCaptureAccess';

// The system's Accessibility prompt, which also lists the program in the pane.
function AskAccessibility: boolean;
var
  Key: CFStringRef;
  Value: CFBooleanRef;
  Options: CFDictionaryRef;
begin
  Key := CFStringCreateWithCString(nil, 'AXTrustedCheckOptionPrompt', kCFStringEncodingUTF8);
  Value := kCFBooleanTrue;
  Options := CFDictionaryCreate(nil, UnivPtrPtr(@Key), UnivPtrPtr(@Value), 1, @kCFTypeDictionaryKeyCallBacks, @kCFTypeDictionaryValueCallBacks);
  Result := AXIsProcessTrustedWithOptions(Options);
  CFRelease(Options);
  CFRelease(Key);
end;

procedure OpenPrivacyPane(const Pane: string);
begin
  with TProcess.Create(nil) do
    try
      Executable := '/usr/bin/open';
      Parameters.Add('x-apple.systempreferences:com.apple.preference.security?' + Pane);
      Execute;
    finally
      Free;
    end;
end;
{$endif}

constructor THIDPermission.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  Wanted := [hnInput];
end;

function THIDPermission.Fail(const Message: string): boolean;
begin
  ErrorText := Message;
  if Assigned(ErrorHandler) then ErrorHandler(Self, Message);
  Result := False;
end;

function THIDPermission.Missing: THIDNeeds;
begin
  Result := [];
  {$if defined(linux)}
  if not OnWayland then Exit;
  if (hnInput in Needs) and (fpAccess('/dev/uinput', W_OK) <> 0) then Include(Result, hnInput);
  if (hnWatching in Needs) and not EventsReadable then Include(Result, hnWatching);
  {$elseif defined(darwin)}
  if (hnInput in Needs) and not AXIsProcessTrusted then Include(Result, hnInput);
  if (hnWatching in Needs) and not CGPreflightListenEventAccess then Include(Result, hnWatching);
  if (hnCapture in Needs) and not CGPreflightScreenCaptureAccess then Include(Result, hnCapture);
  {$endif}
end;

function THIDPermission.Granted: boolean;
begin
  Result := Missing = [];
end;

// What is wanted, in words, for the sentences below.
function Doing(Needs: THIDNeeds): string;
var
  Parts: TStringList;
begin
  Parts := TStringList.Create;
  try
    if hnInput in Needs then Parts.Add('type and use the mouse in other programs');
    if hnWatching in Needs then Parts.Add('see the keys and mouse buttons pressed anywhere');
    if hnCapture in Needs then Parts.Add('take pictures of other programs'' windows');
    case Parts.Count of
      0: Result := '';
      1: Result := Parts[0];
      2: Result := Parts[0] + ' and ' + Parts[1];
      else
        Result := Parts[0] + ', ' + Parts[1] + ' and ' + Parts[2];
    end;
  finally
    Parts.Free;
  end;
end;

function THIDPermission.Explanation: string;
var
  Want: THIDNeeds;
begin
  Want := Missing;
  if Want = [] then Exit('Nothing more is needed on this computer.');
  Result := Format('To %s, %s needs permission that this desktop does not give out on its own.', [Doing(Want), Application.Title]);
  {$if defined(linux)}
  Result := Result + LineEnding + LineEnding + 'Allowing it installs the small system file below, which lets whoever is sitting at this computer - ' +
    'and every program they run - do the same. The system asks for the administrator password once, and the permission holds from then on. ' +
    'To take it back, delete the file; that counts from the next login.';
  {$elseif defined(darwin)}
  Result := Result + LineEnding + LineEnding + 'macOS will ask, and the answer is kept in System Settings > Privacy & Security, where it can be taken back at any time.';
  if Want * [hnWatching, hnCapture] <> [] then Result := Result + ' Input Monitoring and Screen Recording count from the next start of the program.';
  {$endif}
end;

function THIDPermission.Details: string;
  {$ifdef linux}
var
  Need: THIDNeed;
  {$endif}
begin
  Result := '';
  {$ifdef linux}
  for Need in Missing do
    if RuleFile(Need) <> '' then
      Result := Result + RuleFile(Need) + LineEnding + '  ' + RuleLine(Need) + LineEnding;
  {$endif}
end;

function THIDPermission.Grant: boolean;
  {$ifdef linux}
var
  Need: THIDNeed;
  Tries: integer;
  Script, Why: string;
  {$endif}
begin
  ErrorText := '';
  if Granted then Exit(True);
  {$if defined(linux)}
  Script := 'set -e' + LineEnding + 'umask 022' + LineEnding;
  for Need in Missing do
    if RuleFile(Need) <> '' then
      Script := Script + 'printf ''%s\n'' ''' + RuleLine(Need) + ''' > ' + RuleFile(Need) + LineEnding;
  if not RunAsRoot(Script + Reload, Why) then Exit(Fail(Why));
  // the session's access arrives a moment after udev is done
  for Tries := 1 to 20 do
  begin
    if Granted then Break;
    Sleep(100);
  end;
  if not Granted then Exit(Fail('The rule is installed, but the devices are still not open to this session. Logging out and in applies it.'));
  Result := True;
  {$elseif defined(darwin)}
  if hnInput in Missing then AskAccessibility;
  if hnWatching in Missing then CGRequestListenEventAccess;
  if hnCapture in Missing then CGRequestScreenCaptureAccess;
  Result := Granted;
  {$else}
  Result := True;
  {$endif}
end;

function THIDPermission.Revoke: boolean;
  {$ifdef linux}
var
  Need: THIDNeed;
  Script, Why: string;
  {$endif}
begin
  ErrorText := '';
  {$if defined(linux)}
  Script := 'set -e' + LineEnding;
  for Need in Needs do
    if RuleFile(Need) <> '' then Script := Script + 'rm -f ' + RuleFile(Need) + LineEnding;
  Result := RunAsRoot(Script + Reload, Why);
  if not Result then Fail(Why);
  {$elseif defined(darwin)}
  if hnCapture in Needs then OpenPrivacyPane('Privacy_ScreenCapture')
  else if hnWatching in Needs then OpenPrivacyPane('Privacy_ListenEvent')
  else
    OpenPrivacyPane('Privacy_Accessibility');
  Result := True;
  {$else}
  Result := True;
  {$endif}
end;

function THIDPermission.Ask: boolean;
begin
  if Granted then Exit(True);
  Result := False;
  with THIDPermissionDialog.Create(nil) do
  try
    Prepare(Explanation, Details);
    if ShowModal = mrOk then Result := Grant;
  finally
    Free;
  end;
end;

end.
