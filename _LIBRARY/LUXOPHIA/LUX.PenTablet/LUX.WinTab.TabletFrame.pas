unit LUX.WinTab.TabletFrame;

interface //####################################################################

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants, 
  FMX.Types, FMX.Graphics, FMX.Controls, FMX.Forms, FMX.Dialogs, FMX.StdCtrls,
  LUX.WinTab;

type
  TTabletFrame = class( TFrame )
  private
    { private 宣言 }
  protected
    _Tablet   :TPenTablet;
    _Image    :TBitmap;
    _Packets  :TArray<TTabletPacket>;
    _PacketsN :Integer;
    _DrawArea :TRectF;
    _Timer    :TTimer;
    ///// メソッド
    procedure Paint; override;
    procedure Resize; override;
    procedure CalcDrawArea;
    function TabToScr( const X_,Y_:Integer ) :TPointF;
    procedure DrawFrame(Sender: TObject);
  public
    { public 宣言 }
    constructor Create( Owner_:TComponent ); override;
    destructor Destroy; override;
    ///// プロパティ
    property Tablet :TPenTablet read _Tablet;
  end;

implementation //###############################################################

{$R *.fmx}

uses System.Math.Vectors,
     FMX.Platform,
     LUX.FMX.Pratform;

procedure TTabletFrame.Paint;
var
   P :TTabletPacket;
   C :TPointF;
   S :Single;
begin
     inherited;

     with Canvas do
     begin
          DrawBitmap( _Image, _Image.BoundsF, LocalRect, 1 );

          if _PacketsN > 0 then
          begin
               P := _Packets[ _PacketsN-1 ];

               C := TabToScr( P.X, P.Y );

               S := 10;

               with Stroke do
               begin
                    Kind      := TBrushKind.Solid;
                    Thickness := 2;

                    case P.Status of
                      $00: Color := TAlphaColors.Red ;  // ペン
                      $10: Color := TAlphaColors.Blue;  // 消しゴム
                    end;
               end;

               DrawEllipse( TRectF.Create( C.X-S, C.Y-S, C.X+S, C.Y+S ), 1 );
          end;
     end;
end;

procedure TTabletFrame.Resize;
begin
     inherited;

     if Assigned( _Tablet ) then CalcDrawArea;

     if Assigned( _Image ) then _Image.SetSize( Round( GetDisplayScale * Width  ),
                                                Round( GetDisplayScale * Height ) );
end;

procedure TTabletFrame.CalcDrawArea;
var
   TW, TH :Integer;
   CW, CH, AW, AH :Single;
begin
     TW := _Tablet.PosMaxX - _Tablet.PosMinX + 1;  CW := Width;
     TH := _Tablet.PosMaxY - _Tablet.PosMinY + 1;  CH := Height;

     if TW * CH <= TH * CW then
     begin
          AW := TW          ;
          AH := TW / CW * CH;

          _DrawArea.Left := _Tablet.PosMinX;
          _DrawArea.Top  := _Tablet.PosMinY + ( TH - AH ) / 2;
     end
     else
     begin
          AW := TH / CH * CW;
          AH := TH          ;

          _DrawArea.Left := _Tablet.PosMinX + ( TW - AW ) / 2;
          _DrawArea.Top  := _Tablet.PosMinY;
     end;

     _DrawArea.Width  := AW;
     _DrawArea.Height := AH;
end;

function TTabletFrame.TabToScr( const X_,Y_:Integer ) :TPointF;
begin
     Result.X :=       ( X_ - _DrawArea.Left ) / _DrawArea.Width    * Width ;
     Result.Y := ( 1 - ( Y_ - _DrawArea.Top  ) / _DrawArea.Height ) * Height;
end;

procedure TTabletFrame.DrawFrame(Sender: TObject);
var
   I :Integer;
   P :TTabletPacket;
   C :TPointF;
   S :Single;
begin
     _PacketsN := _Tablet.GetPakets( _Packets );  //すべての未処理パケットを取得

     if _PacketsN > 0 then  //未処理パケットがある場合
     begin
          with _Image.Canvas do
          begin
               BeginScene;  //描画開始

               Fill.Kind  := TBrushKind.Solid;

               for I := 0 to _PacketsN-1 do
               begin
                    P := _Packets[ I ];  //単一のパケット

                    if P.Buttons = 1 then  //ペン先が押された場合
                    begin
                         case P.Status of
                           $00: Fill.Color := TAlphaColors.Black;  // ペン
                           $10: Fill.Color := TAlphaColors.White;  // 消しゴム
                         end;

                         C := TabToScr( P.X, P.Y );

                         S := 20 * ( P.NormalPressure / _Tablet.PreMax );

                         FillEllipse( TRectF.Create( C.X-S, C.Y-S, C.X+S, C.Y+S ), 0.75 );  //円を描画
                    end;
               end;

               EndScene;  //描画終了
          end;
     end;

     Repaint;
end;

constructor TTabletFrame.Create( Owner_:TComponent );
begin
     inherited;

     _Tablet := TPenTablet.Create;

     SetLength( _Packets, _Tablet.QueueSize );

     CalcDrawArea;

     _Image := TBitmap.Create;

     with _Image do
     begin
          BitmapScale := GetDisplayScale;

          SetSize( Round( GetDisplayScale * Width  ),
                   Round( GetDisplayScale * Height ) );
     end;

     _Timer := TTimer.Create( Self );
     _Timer.Interval := 10;
     _Timer.OnTimer  := DrawFrame;
end;

destructor TTabletFrame.Destroy;
begin
     _Image.Free;

     _Tablet.Free;

     inherited;
end;

end. //#########################################################################
