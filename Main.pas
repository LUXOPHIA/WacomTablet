unit Main;

interface //####################################################################

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs,
  FMX.Memo.Types, FMX.Controls.Presentation, FMX.ScrollBox, FMX.Memo, FMX.TabControl, FMX.Objects,
  LUX.WinTab;

type
  TForm1 = class(TForm)
    TabControl1: TTabControl;
      TabItem1: TTabItem;
        Image1: TImage;
      TabItem2: TTabItem;
        Memo1: TMemo;
    Timer1: TTimer;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure Timer1Timer(Sender: TObject);
  private
    { private 宣言 }
  public
    { public 宣言 }
    _PenTablet :TPenTablet;
  end;

var
  Form1: TForm1;

implementation //###############################################################

{$R *.fmx}

uses System.Math.Vectors;

//%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% private

//%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% public

//%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

procedure TForm1.FormCreate(Sender: TObject);
begin
     _PenTablet := TPenTablet.Create;

     with _PenTablet do
     begin
          with Memo1.Lines do
          begin
               Add( 'PosX = ' + PosMinX.ToString + ' ～ ' + PosMaxX.ToString );
               Add( 'PosY = ' + PosMinY.ToString + ' ～ ' + PosMaxY.ToString );

               Add( 'ResX = ' + ResX.ToString + ' ( ' + UniX.ToString + ' )' );
               Add( 'ResY = ' + ResY.ToString + ' ( ' + UniY.ToString + ' )' );

               Add( 'Pre  = ' + PreMin.ToString + ' ～ ' + PreMax.ToString );

               Add( 'Whe  = ' + WheMin.ToString + ' ～ ' + WheMax.ToString );

               Add( 'Azi  = ' + AziMin.ToString + ' ～ ' + AziMax.ToString );
               Add( 'Alt  = ' + AltMin.ToString + ' ～ ' + AltMax.ToString );
               Add( 'Twi  = ' + TwiMin.ToString + ' ～ ' + TwiMax.ToString );

          end;

          with Image1.Bitmap do
          begin
               SetSize( ( PosMaxX + 1 ) div 10,
                        ( PosMaxY + 1 ) div 10 );

               Clear( TAlphaColors.Lightgray );
          end;
     end;
end;

procedure TForm1.FormDestroy(Sender: TObject);
begin
     _PenTablet.Free;
end;

////////////////////////////////////////////////////////////////////////////////

procedure TForm1.Timer1Timer(Sender: TObject);
var
   PsN, I :Integer;
   Ps :array[ 0..16-1 ] of TTabletPacket;
   P :TTabletPacket;
   C :TPointF;
   S :Single;
begin
     PsN := _PenTablet.GetPakets( Ps );  //すべての未処理パケットを取得

     if PsN > 0 then  //未処理パケットがある場合
     begin
          with Image1.Bitmap.Canvas do
          begin
               BeginScene;  //描画開始

               SetMatrix( TMatrix.CreateScaling( 1 / Scale, 1 / Scale ) );  //ディスプレイスケーリングを無効化

               with Fill do
               begin
                    Kind  := TBrushKind.Solid;
                    Color := TAlphaColors.Black;  //図形の色を設定
               end;

               for I := 0 to PsN-1 do
               begin
                    P := Ps[ I ];  //単一のパケット

                    Assert( P.Status = 0, P.Status.ToHexString );  //パケットのステータスが正常

                    if P.Buttons = 1 then  //ペン先が押された場合
                    begin
                         C := TPointF.Create( P.X / 10, ( _PenTablet.PosMaxY - P.Y ) / 10 );

                         S := 100 * ( P.NormalPressure / _PenTablet.PreMax );

                         FillEllipse( TRectF.Create( C.X-S, C.Y-S, C.X+S, C.Y+S ), 0.75 );  //円を描画
                    end;
               end;

               EndScene;  //描画終了
          end;
     end;
end;

end. //#########################################################################
