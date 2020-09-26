unit Main;

interface //####################################################################

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs,
  FMX.Controls.Presentation, FMX.ScrollBox, FMX.Memo, FMX.Memo.Types,
  LUX.WinTab;

type
  TForm1 = class(TForm)
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

//%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% private

//%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% public

//%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

procedure TForm1.FormCreate(Sender: TObject);
begin
     _PenTablet := TPenTablet.Create( Self );

     with Memo1.Lines do
     begin
          with _PenTablet do
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
     end;

     _PenTablet.OnPacket := procedure( const Packet_:TTabletPacket )
     begin
          with Packet_ do Memo1.Lines.Add( X             .ToString
                                  + ', ' + Y             .ToString
                                  + ', ' + NormalPressure.ToString );
     end;
end;

procedure TForm1.FormDestroy(Sender: TObject);
begin
     _PenTablet.Free;
end;

end. //#########################################################################
