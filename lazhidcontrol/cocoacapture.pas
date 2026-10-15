// Copyright (c) 2026 Tony Stone. 0BSD.
unit CocoaCapture;

{$mode objfpc}{$H+}

interface

uses
  Classes, Types, Graphics, MacOSAll;

{ Pictures of the screen on macOS.  Core Graphics lets a program see other
  programs' windows only once the user has allowed it under Privacy &
  Security > Screen Recording.  Without that the picture still comes back,
  with the desktop and the program's own windows in it and nothing else. }
function CaptureScreenArea(const Area: TRect; Destination: TBitmap): boolean;
function CaptureWindowImage(Window: CGWindowID; Destination: TBitmap): boolean;

implementation

{ Through a PNG in memory, which is the one format both sides read.  Takes
  over the image and releases it. }
function ImageToBitmap(Image: CGImageRef; Destination: TBitmap): boolean;
var
  Data: CFMutableDataRef;
  Target: CGImageDestinationRef;
  Stream: TMemoryStream;
  Png: TPortableNetworkGraphic;
begin
  Result := False;
  if Image = nil then Exit;
  Data := CFDataCreateMutable(nil, 0);
  Target := CGImageDestinationCreateWithData(Data, CFSTR('public.png'), 1, nil);
  Stream := TMemoryStream.Create;
  Png := TPortableNetworkGraphic.Create;
  try
    CGImageDestinationAddImage(Target, Image, nil);
    if CGImageDestinationFinalize(Target) = 0 then Exit;
    Stream.WriteBuffer(CFDataGetBytePtr(Data)^, CFDataGetLength(Data));
    Stream.Position := 0;
    Png.LoadFromStream(Stream);
    Destination.Assign(Png);
    Result := True;
  finally
    Png.Free;
    Stream.Free;
    CFRelease(Target);
    CFRelease(Data);
    CGImageRelease(Image);
  end;
end;

function CaptureScreenArea(const Area: TRect; Destination: TBitmap): boolean;
begin
  Result := ImageToBitmap(CGWindowListCreateImage(CGRectMake(Area.Left, Area.Top, Area.Right - Area.Left, Area.Bottom - Area.Top),
    kCGWindowListOptionOnScreenOnly, kCGNullWindowID, kCGWindowImageDefault), Destination);
end;

function CaptureWindowImage(Window: CGWindowID; Destination: TBitmap): boolean;
begin
  // the window alone, without its shadow, even if something covers it
  Result := ImageToBitmap(CGWindowListCreateImage(CGRectNull, kCGWindowListOptionIncludingWindow, Window, kCGWindowImageBoundsIgnoreFraming), Destination);
end;

end.
