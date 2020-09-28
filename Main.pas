unit Main;

interface //####################################################################

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs,
  FMX.Memo.Types, FMX.Controls.Presentation, FMX.ScrollBox, FMX.Memo, FMX.TabControl, FMX.Objects,
  LUX.WinTab.TabletFrame;

type
  TForm1 = class(TForm)
    TabControl1: TTabControl;
      TabItem1: TTabItem;
        TabletFrame1: TTabletFrame;
      TabItem2: TTabItem;
        Memo1: TMemo;
    procedure FormCreate(Sender: TObject);
  private
    { private 宣言 }
  public
    { public 宣言 }
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
     with TabletFrame1.Tablet do
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
     end;
end;

////////////////////////////////////////////////////////////////////////////////

end. //#########################################################################
