{ XGlobalKeyMonitor

  Copyright (c) 2025-2026 Tony Stone. 0BSD.

  X11 implementation of global key monitoring using XInput2.
}
unit XGlobalKeyMonitor;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LCLType, DynLibs, X, XLib, KeySym, ctypes,
  GlobalKeyMonitorIntf;

type
  { TXGlobalKeyListener }
  TXGlobalKeyListener = class(TGlobalKeyListener)
  private
    Display: PDisplay;
    RootWindow: TWindow;
    MonitorThread: TThread;
    XI2Opcode: cint;
  protected
    procedure DoStartMonitoring; override;
    procedure DoStopMonitoring; override;
  public
    constructor Create; override;
    destructor Destroy; override;
  end;

function InitializeGlobalKeyListener: TGlobalKeyListener;

implementation

// XInput2 Constants
const
  XI_RawKeyPress = 13;
  XI_RawKeyRelease = 14;
  XIAllMasterDevices = 1;

type
  PXIEventMask = ^TXIEventMask;

  TXIEventMask = record
    deviceid: cint;
    mask_len: cint;
    mask: pbyte;
  end;

  // Simplified XIRawEvent structure - we only need the detail field
  PXIRawEvent = ^TXIRawEvent;

  TXIRawEvent = record
    _type: cint;
    serial: culong;
    send_event: longbool;
    display: PDisplay;
    extension: cint;
    evtype: cint;
    time: TTime;
    deviceid: cint;
    sourceid: cint;
    detail: cint;  // This is the keycode!
    flags: cint;
    // ... rest of structure omitted, we only need detail
  end;

  // XInput2 functions - we'll load them dynamically
var
  XISelectEvents: function(dpy: PDisplay; win: TWindow; masks: PXIEventMask; num_masks: cint): cint; cdecl = nil;
  LibXI2: TLibHandle = NilHandle;

type
  { TKeyMonitorThread }
  TKeyMonitorThread = class(TThread)
  private
    Listener: TXGlobalKeyListener;
    DetectedKey: word;
    DetectedShift: TShiftState;
    // the modifiers are kept from the raw stream: a raw event carries none,
    // and asking the server afterwards races the release
    Held: TShiftState;
    procedure NotifyKeyPress;
  public
    constructor Create(AListener: TXGlobalKeyListener);
    procedure Execute; override;
  end;

// Convert X KeySym to LCL VK code
function SymToKey(Sym: TKeySym): word;
begin
  case Sym of
    XK_TAB: Result := VK_TAB;
    XK_RETURN: Result := VK_RETURN;
    XK_ESCAPE: Result := VK_ESCAPE;
    XK_SPACE: Result := VK_SPACE;
    XK_BACKSPACE: Result := VK_BACK;
    XK_DELETE: Result := VK_DELETE;
    XK_HOME: Result := VK_HOME;
    XK_END: Result := VK_END;
    XK_PRIOR: Result := VK_PRIOR;
    XK_NEXT: Result := VK_NEXT;
    XK_LEFT: Result := VK_LEFT;
    XK_UP: Result := VK_UP;
    XK_RIGHT: Result := VK_RIGHT;
    XK_DOWN: Result := VK_DOWN;
    XK_INSERT: Result := VK_INSERT;
    XK_F1..XK_F12: Result := VK_F1 + word(Sym - XK_F1);
    XK_SHIFT_L, XK_SHIFT_R: Result := VK_SHIFT;
    XK_CONTROL_L, XK_CONTROL_R: Result := VK_CONTROL;
    XK_ALT_L, XK_ALT_R: Result := VK_MENU;
    else
      // For printable ASCII characters (letters, numbers, symbols), convert to uppercase VK code
      if (Sym >= $20) and (Sym <= $7E) then
      begin
        // If lowercase letter, convert to uppercase for VK code
        if (Sym >= Ord('a')) and (Sym <= Ord('z')) then
          Result := Sym - 32
        else
          Result := Sym;  // Numbers, uppercase letters, and symbols stay as-is
      end
      else
        Result := Sym and $FF;  // Fallback for other keysyms
  end;
end;

function ModToShift(Modifiers: cardinal): TShiftState;
begin
  Result := [];
  if (ShiftMask and Modifiers) > 0 then
    Include(Result, ssShift);
  if (Mod1Mask and Modifiers) > 0 then
    Include(Result, ssAlt);
  if (ControlMask and Modifiers) > 0 then
    Include(Result, ssCtrl);
end;

constructor TKeyMonitorThread.Create(AListener: TXGlobalKeyListener);
begin
  inherited Create(False);
  FreeOnTerminate := False;
  Listener := AListener;
end;

procedure TKeyMonitorThread.NotifyKeyPress;
begin
  if Assigned(Listener.OnKeyPress) then
    Listener.OnKeyPress(DetectedKey, DetectedShift);
end;

procedure TKeyMonitorThread.Execute;
var
  Event: TXEvent;
  Sym: TKeySym;
  RawEvent: PXIRawEvent;
  keycode: TKeyCode;
  keysyms_per_keycode: cint;
  keysym_map: PKeySym;
  Modifier: TShiftStateEnum;
begin
  while not Terminated do
  begin
    if XPending(Listener.Display) > 0 then
    begin
      XNextEvent(Listener.Display, @Event);

      // Check for GenericEvent (XInput2 events)
      if Event._type = GenericEvent then
      begin
        // Get the event data
        if XGetEventData(Listener.Display, @Event.xcookie) then
        begin
          if (Event.xcookie.extension = Listener.XI2Opcode) and (Event.xcookie.evtype in [XI_RawKeyPress, XI_RawKeyRelease]) then
          begin
            // Cast the data to XIRawEvent structure
            RawEvent := PXIRawEvent(Event.xcookie.Data);

            // Extract keycode from detail field
            keycode := RawEvent^.detail;

            // Get the keysym mapping for this keycode
            keysym_map := XGetKeyboardMapping(Listener.Display, keycode, 1, @keysyms_per_keycode);
            if keysym_map <> nil then
            begin
              // Get the first keysym (unshifted)
              Sym := keysym_map^;
              XFree(keysym_map);

              case Sym of
                XK_SHIFT_L, XK_SHIFT_R: Modifier := ssShift;
                XK_CONTROL_L, XK_CONTROL_R: Modifier := ssCtrl;
                XK_ALT_L, XK_ALT_R: Modifier := ssAlt;
                XK_SUPER_L, XK_SUPER_R: Modifier := ssMeta;
                else
                  Modifier := ssDouble;
              end;
              if Modifier <> ssDouble then
                if Event.xcookie.evtype = XI_RawKeyPress then Include(Held, Modifier)
                else
                  Exclude(Held, Modifier);
              if Event.xcookie.evtype = XI_RawKeyPress then
              begin
                DetectedKey := SymToKey(Sym);
                DetectedShift := Held;
                Synchronize(@NotifyKeyPress);
              end;
            end;
          end;
          XFreeEventData(Listener.Display, @Event.xcookie);
        end;
      end;
    end
    else
      Sleep(10);
  end;
end;

function LoadXI2Library: boolean;
begin
  Result := False;
  LibXI2 := LoadLibrary('libXi.so.6');
  if LibXI2 = NilHandle then
    LibXI2 := LoadLibrary('libXi.so');
  if LibXI2 = NilHandle then
    Exit;

  Pointer(XISelectEvents) := GetProcedureAddress(LibXI2, 'XISelectEvents');
  Result := Assigned(XISelectEvents);
end;

function InitializeGlobalKeyListener: TGlobalKeyListener;
begin
  Result := TXGlobalKeyListener.Create;
end;

{ TXGlobalKeyListener }

constructor TXGlobalKeyListener.Create;
var
  event_base, error_base: cint;
begin
  inherited Create;
  MonitorThread := nil;
  XI2Opcode := 0;

  if not LoadXI2Library then
    Exit;

  Display := XOpenDisplay(nil);
  if Display = nil then
    Exit;

  RootWindow := XDefaultRootWindow(Display);

  // Query XInput2 extension
  if XQueryExtension(Display, 'XInputExtension', @XI2Opcode, @event_base, @error_base) = False then
    Exit;
end;

destructor TXGlobalKeyListener.Destroy;
begin
  StopMonitoring;
  if Display <> nil then
    XCloseDisplay(Display);
  if LibXI2 <> NilHandle then
    FreeLibrary(LibXI2);
  inherited Destroy;
end;

procedure TXGlobalKeyListener.DoStartMonitoring;
var
  mask: array[0..3] of byte;
  evmask: TXIEventMask;
begin
  if Active or (Display = nil) or not Assigned(XISelectEvents) then
    Exit;

  // presses to report, releases to keep the modifiers right
  FillChar(mask, SizeOf(mask), 0);
  mask[XI_RawKeyPress shr 3] := (1 shl (XI_RawKeyPress and 7)) or (1 shl (XI_RawKeyRelease and 7));

  evmask.deviceid := XIAllMasterDevices;
  evmask.mask_len := SizeOf(mask);
  evmask.mask := @mask[0];

  XISelectEvents(Display, RootWindow, @evmask, 1);

  // Start monitoring thread
  MonitorThread := TKeyMonitorThread.Create(Self);
  Active := True;
end;

procedure TXGlobalKeyListener.DoStopMonitoring;
var
  evmask: TXIEventMask;
begin
  if not Active then
    Exit;

  if Assigned(MonitorThread) then
  begin
    MonitorThread.Terminate;
    MonitorThread.WaitFor;
    MonitorThread.Free;
    MonitorThread := nil;
  end;

  if (Display <> nil) and Assigned(XISelectEvents) then
  begin
    evmask.deviceid := XIAllMasterDevices;
    evmask.mask_len := 0;
    evmask.mask := nil;
    XISelectEvents(Display, RootWindow, @evmask, 1);
  end;

  Active := False;
end;

end.
