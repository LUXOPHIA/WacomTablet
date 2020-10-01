unit LUX.WinTab.TabletFrame;

interface //#################################################################### ■

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Graphics, FMX.Controls, FMX.Forms, FMX.Dialogs, FMX.StdCtrls,
  LUX, LUX.WinTab;

type //$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$【型】

     //$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$【クラス】

     //%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% TTabletFrame

     TTabletFrame = class( TFrame )
     private
     protected
       _Tablet   :TPenTablet;
       _Image    :TBitmap;
       _Packets  :TArray<TTabletPacket>;
       _PacketsN :Integer;
       _DrawArea :TRectF;
       _Timer    :TTimer;
       _Brush    :TBitmap;
       ///// メソッド
       procedure Paint; override;
       procedure Resize; override;
       procedure CalcDrawArea;
       function TabToScr( const X_,Y_:Integer ) :TPointF;
       procedure DrawFrame(Sender: TObject);
     public
       constructor Create( Owner_:TComponent ); override;
       destructor Destroy; override;
       ///// プロパティ
       property Tablet :TPenTablet read _Tablet;
       property Brush  :TBitmap    read _Brush ;
     end;

implementation //############################################################### ■

{$R *.fmx}

uses System.Math, System.Math.Vectors,
     FMX.Platform,
     LUX.FMX.Pratform;

//$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$【クラス】

//%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% TTabletFrame

//&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&& private

//&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&& protected

procedure TTabletFrame.Paint;
const
     S = 10;
     R :TRectF = ( Left:-S; Top:-S; Right:+S; Bottom:+S );
var
   P :TTabletPacket;
   Pos :TPointF;
   Azi, Alt :Single;
begin
     inherited;

     with Canvas do
     begin
          DrawBitmap( _Image, _Image.BoundsF, LocalRect, 1 );

          if _PacketsN > 0 then
          begin
               P := _Packets[ _PacketsN-1 ];

               Pos := TabToScr( P.X, P.Y );
               Azi := P.Orientation.orAzimuth  / _Tablet.AziMax * Pi2;
               Alt := P.Orientation.orAltitude / _Tablet.AltMax * P2i;

               SetMatrix( TMatrix.CreateScaling( 1, 1 / Sin( Alt ) )
                        * TMatrix.CreateRotation( Azi )
                        * TMatrix.CreateTranslation( Pos.X, Pos.Y )
                        * Matrix );

               with Stroke do
               begin
                    Kind      := TBrushKind.Solid;
                    Thickness := 1;

                    case P.Status of
                      $00: Color := TAlphaColors.Red ;  // ペン
                      $10: Color := TAlphaColors.Blue;  // 消しゴム
                    end;
               end;

               DrawEllipse( R, 1 );
          end;
     end;
end;

//------------------------------------------------------------------------------

procedure TTabletFrame.Resize;
begin
     inherited;

     if Assigned( _Tablet ) then CalcDrawArea;

     if Assigned( _Image ) then _Image.SetSize( Round( GetDisplayScale * Width  ),
                                                Round( GetDisplayScale * Height ) );
end;

//------------------------------------------------------------------------------

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

//------------------------------------------------------------------------------

function TTabletFrame.TabToScr( const X_,Y_:Integer ) :TPointF;
begin
     Result.X :=       ( X_ - _DrawArea.Left ) / _DrawArea.Width    * Width ;
     Result.Y := ( 1 - ( Y_ - _DrawArea.Top  ) / _DrawArea.Height ) * Height;
end;

//------------------------------------------------------------------------------

procedure TTabletFrame.DrawFrame(Sender: TObject);
const
     S = 20;
     R :TRectF = ( Left:-S; Top:-S; Right:+S; Bottom:+S );
var
   M :TMatrix;
   I :Integer;
   P :TTabletPacket;
   Pos :TPointF;
   Pre, Azi, Alt :Single;
begin
     _PacketsN := _Tablet.GetPakets( _Packets );

     if _PacketsN > 0 then
     begin
          with _Image.Canvas do
          begin
               BeginScene;

               M := Matrix;

               for I := 0 to _PacketsN-1 do
               begin
                    P := _Packets[ I ];

                    if P.Buttons = 1 then
                    begin
                         Pos := TabToScr( P.X, P.Y );
                         Pre := P.NormalPressure / _Tablet.PreMax;
                         Azi := P.Orientation.orAzimuth / _Tablet.AziMax * Pi2;
                         Alt := P.Orientation.orAltitude / _Tablet.AltMax * P2i;

                         SetMatrix( TMatrix.CreateRotation( -Azi )
                                  * TMatrix.CreateScaling( Pre, Pre / Sin( Alt ) )
                                  * TMatrix.CreateRotation( +Azi )
                                  * TMatrix.CreateTranslation( Pos.X, Pos.Y )
                                  * M );

                         case P.Status of
                         $00: begin
                                   DrawBitmap( _Brush, _Brush.BoundsF, R, 0.75 );
                              end;
                         $10: begin
                                   Fill.Kind  := TBrushKind.Solid;
                                   Fill.Color := TAlphaColors.White;

                                   FillEllipse( R, 0.5 );
                              end;
                         end;
                    end;
               end;

               EndScene;
          end;
     end;

     Repaint;
end;

//&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&& public

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
     _Timer.Interval{ms/f} := 1000{ms/s} div 100{f/s};
     _Timer.OnTimer  := DrawFrame;

     _Brush := TBitmap.Create( 64, 64 );

     with _Brush.Canvas do
     begin
          BeginScene;

          Fill.Kind  := TBrushKind.Solid;
          Fill.Color := TAlphaColors.Black;

          FillEllipse( _Brush.BoundsF, 1 );

          EndScene;
     end;
end;

destructor TTabletFrame.Destroy;
begin
     _Brush.Free;

     _Image.Free;

     _Tablet.Free;

     inherited;
end;

end. //######################################################################### ■
