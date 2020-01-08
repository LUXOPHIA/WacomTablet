unit PaintWindow;

{
  Tablet Enable PaintBox Sample

  by LI Qingrui
  emailto: qrli@hotmail.com

  This file is supplied "AS IS", without warranty of any kind.
  Feel free to use and modify for any purpose.
  Enjoy yourself.
}

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms,
  WinTab32, Math;

type
  TPenEvent = procedure(sender: TObject; X, Y, P: single; key: boolean) of object;

  TPaintWindow = class(TCustomControl)
  private
    FTablet: HCTX;
    FMaxNPressure: integer;
    FBitmap: TBitmap;
    FScale: integer;
    FOnPenMove: TPenEvent;
    FOnPenUp: TPenEvent;
    FOnPenDown: TPenEvent;
    FIsPenDown: boolean;
    procedure InitializeTablet;
    procedure FinalizeTablet;
    procedure UpdateSize;
    procedure WMEraseBkgnd(var Message: TWmEraseBkgnd); message WM_ERASEBKGND;
    procedure SetScale(const Value: integer);
    procedure SetTabletContextActive(const Value: boolean);
  protected
    procedure Paint; override;
    procedure CreateWnd; override;
    procedure DestroyWnd; override;
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
  public
    constructor Create(o: TComponent); override;
    destructor Destroy; override;
    procedure SetDimension(w, h: integer);
    procedure UpdateRect(const r: TRect);
    procedure FlushPackets;
    property Bitmap: TBitmap read FBitmap;
    property TabletContextActive: boolean write SetTabletContextActive;
    property MouseCapture;
    property IsPenDown: boolean read FIsPenDown;
  published
    property Scale: integer read FScale write SetScale default 100;
    property Align;
    property Anchors;
    property PopupMenu;
    property Visible;
    property OnPenDown: TPenEvent read FOnPenDown write FOnPenDown;
    property OnPenMove: TPenEvent read FOnPenMove write FOnPenMove;
    property OnPenUp: TPenEvent read FOnPenUp write FOnPenUp;
    property OnClick;
    property OnContextPopup;
    property OnDblClick;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
  end;

procedure Register;

implementation

procedure Register;
begin
  RegisterComponents('Excite', [TPaintWindow]);
end;

{ TPaintWindow }

constructor TPaintWindow.Create(o: TComponent);
begin
  inherited;
  ControlStyle := ControlStyle + [csOpaque];
  FBitmap := TBitmap.Create;
  FBitmap.PixelFormat := pf32bit;
  FScale := 100;
end;

procedure TPaintWindow.CreateWnd;
begin
  inherited;
  if not (csDesigning in ComponentState) then
    InitializeTablet;
end;

destructor TPaintWindow.Destroy;
begin
  FBitmap.Free;
  if not (csDesigning in ComponentState) then
    FinalizeTablet;
  inherited;
end;

procedure TPaintWindow.DestroyWnd;
begin
  if not (csDesigning in ComponentState) then
    FinalizeTablet;
  inherited;
end;

procedure TPaintWindow.FinalizeTablet;
begin
  if FTablet <> 0 then
  begin
    WTClose(FTablet);
    FTablet := 0;
  end;
end;

procedure TPaintWindow.InitializeTablet;
var
  lc: LOGCONTEXT;
  npAxis: AXIS;
begin
  if not IsWinTab32Available then Exit;
  // get default
  WTInfo(WTI_DEFSYSCTX, 0, @lc);
  // modify the digitizing region
  StrCopy(lc.lcName, PChar('PaintWindow '+IntToHex(HInstance, 8)));
  lc.lcOptions := lc.lcOptions or CXO_SYSTEM;
  lc.lcMsgBase := WT_DEFBASE;
  lc.lcPktData := PACKETDATA;
  lc.lcPktMode := PACKETMODE;
  lc.lcMoveMask := PACKETDATA;
  lc.lcBtnUpMask := lc.lcBtnDnMask;
  lc.lcOutExtX := lc.lcOutExtX * 10;
  lc.lcOutExtY := lc.lcOutExtY * 10;

  FTablet := WTOpen(Handle, lc, TRUE);
  WTInfo(WTI_DEVICES + lc.lcDevice, DVC_NPRESSURE, @npAxis);
  FMaxNPressure := npAxis.axMax;
end;

procedure TPaintWindow.MouseMove(Shift: TShiftState; X, Y: Integer);
var
  buf: array[0..31] of PACKET;
  org: TPoint;
  n, scrH: integer;

  procedure ProcessPackets;
  var
    i: integer;
    x, y, p: single;
  begin
    for i := 0 to n-1 do
    begin
      if buf[i].pkNormalPressure > 0 then
        if i=0 then;
      x := (buf[i].pkX / 10 - org.x) * 100/FScale;
      y := ((scrH*10 - buf[i].pkY) / 10 - org.y) * 100/FScale;
      if not IsPenDown then p := 0
      else if FMaxNPressure = 0 then p := 1
      else begin
        p := EnsureRange(buf[i].pkNormalPressure / FMaxNPressure, 0.0, 1.0);
      end;
      if i = n-1 then FOnPenMove(self, x, y, p, true)
      else FOnPenMove(self, x, y, p, false);
    end;
  end;

  procedure ProcessMouse;
  begin
    if ssLeft in Shift then FOnPenMove(self, X+0.5, Y+0.5, 1, true)
    else FOnPenMove(self, X+0.5, Y+0.5, 0, true);
  end;

begin
  inherited;
  if not Assigned(FOnPenMove) then Exit;
  if IsWinTab32Available then
  begin
    org := ClientToScreen(Point(0, 0));
    scrH := Screen.Height;
    n := WTPacketsGet(FTablet, 32, @buf);
    if n <= 0 then ProcessMouse
    else repeat
      ProcessPackets;
      n := WTPacketsGet(FTablet, 32, @buf);
    until n = 0;
  end
  else
    ProcessMouse;
end;

procedure TPaintWindow.MouseDown(Button: TMouseButton; Shift: TShiftState;
  X, Y: Integer);
begin
  inherited;
  if Button = mbLeft then
  begin
    FIsPenDown := true;
    FlushPackets;
  end;
  if Assigned(FOnPenDown) and (mbLeft = Button) then
  begin
    FOnPenDown(self, X+0.5, Y+0.5, 1, true);
  end;
end;

procedure TPaintWindow.MouseUp(Button: TMouseButton; Shift: TShiftState; X,
  Y: Integer);
begin
  inherited;
  if Button = mbLeft then
  begin
    FIsPenDown := false;
  end;
  if Assigned(FOnPenUp) and (mbLeft = Button) then
    FOnPenUp(self, X+0.5, Y+0.5, 0, false);
end;

procedure TPaintWindow.Paint;
begin
  if FBitmap.Height = 0 then canvas.FillRect(ClientRect)
  else canvas.StretchDraw(ClientRect, FBitmap);
end;

procedure TPaintWindow.SetTabletContextActive(const Value: boolean);
begin
  if FTablet <> 0 then
    if value then
    begin
      WTEnable(FTablet, true);
      WTOverlap(FTablet, true);
    end
    else begin
      WTOverlap(FTablet, false);
      WTEnable(FTablet, false);
    end;
end;

procedure TPaintWindow.SetDimension(w, h: integer);
begin
  FBitmap.Width := w;
  FBitmap.Height := h;
  UpdateSize;
end;

procedure TPaintWindow.SetScale(const Value: integer);
begin
  if value < 25 then Exit;
  FScale := Value;
  UpdateSize;
end;

procedure TPaintWindow.UpdateSize;
begin
  if not FBitmap.Empty then
  begin
    Width := FBitmap.Width * FScale div 100;
    Height := FBitmap.Height * FScale div 100;
    Repaint;
  end;
end;

procedure TPaintWindow.WMEraseBkgnd(var Message: TWmEraseBkgnd);
begin
  Message.result := 1;
end;

procedure TPaintWindow.UpdateRect(const r: TRect);
begin
  if IsRectEmpty(r) then Exit;
  with r do
    StretchBlt(canvas.Handle, left * 100 div FScale, top * 100 div FScale,
      (right - left) * 100 div FScale, (bottom - top) * 100 div FScale,
      FBitmap.canvas.handle, left, top, right - left, bottom - top, SRCCOPY);
end;

procedure TPaintWindow.FlushPackets;
begin
  if IsWinTab32Available then WTPacketsGet(FTablet, 0, nil);
end;

end.
