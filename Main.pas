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
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
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
     _PenTablet := TPenTablet.Create( Self );

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

     _PenTablet.OnPacket := procedure( const Packet_:TTabletPacket )
     var
        P :TPointF;
        S :Single;
     begin
          with Image1.Bitmap.Canvas do
          begin
               BeginScene;

               SetMatrix( TMatrix.CreateScaling( 1 / Scale, 1 / Scale ) );

               with Fill do
               begin
                    Kind  := TBrushKind.Solid;
                    Color := TAlphaColors.Black;
               end;

               P := TPointF.Create( Packet_.X / 10, ( _PenTablet.PosMaxY - Packet_.Y ) / 10 );

               S := 100 * ( Packet_.NormalPressure / _PenTablet.PreMax );

               FillEllipse( TRectF.Create( P.X-S, P.Y-S, P.X+S, P.Y+S ), 1 );

               EndScene;
          end;
     end;
end;

procedure TForm1.FormDestroy(Sender: TObject);
begin
     _PenTablet.Free;
end;

end. //#########################################################################
