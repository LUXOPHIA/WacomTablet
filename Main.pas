unit Main;

interface //####################################################################

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs,
  FMX.Controls.Presentation, FMX.ScrollBox, FMX.Memo,
  WinTab32;

type
  TForm1 = class(TForm)
    Memo1: TMemo;
    Timer1: TTimer;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure Timer1Timer(Sender: TObject);
  private
    { private 宣言 }
  public
    { public 宣言 }
    FTablet :HCTX;
    /////
    procedure BeginTablet;
    procedure EndTablet;
  end;

var
  Form1: TForm1;

implementation //###############################################################

{$R *.fmx}

uses FMX.Platform.Win;

//%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% public

procedure TForm1.BeginTablet;
var
   lc :LOGCONTEXT;
   npAxis :AXIS;
begin
     if not IsWinTab32Available then Exit;
     // get default
     WTInfo( WTI_DEFSYSCTX, 0, @lc );
     // modify the digitizing region
     StrCopy( lc.lcName, PChar( 'PaintWindow '+IntToHex( HInstance, 8 ) ) );
     lc.lcOptions   := lc.lcOptions or CXO_SYSTEM;
     lc.lcMsgBase   := WT_DEFBASE;
     lc.lcPktData   := PACKETDATA;
     lc.lcPktMode   := PACKETMODE;
     lc.lcMoveMask  := PACKETDATA;
     lc.lcBtnUpMask := lc.lcBtnDnMask;
     lc.lcOutExtX   := lc.lcOutExtX * 10;
     lc.lcOutExtY   := lc.lcOutExtY * 10;

     CreateHandle;

     FTablet := WTOpen( WindowHandleToPlatform( Handle ).Wnd, lc, True );

     Assert( FTablet > 0, 'FTablet = 0' );

     WTInfo( WTI_DEVICES + lc.lcDevice, DVC_NPRESSURE, @npAxis );

     Memo1.Lines.Add( 'npAxis.axMin        = ' + npAxis.axMin       .ToString );
     Memo1.Lines.Add( 'npAxis.axMax        = ' + npAxis.axMax       .ToString );
     Memo1.Lines.Add( 'npAxis.axUnits      = ' + npAxis.axUnits     .ToString );
     Memo1.Lines.Add( 'npAxis.axResolution = ' + npAxis.axResolution.ToString );
end;

procedure TForm1.EndTablet;
begin
     if FTablet <> 0 then
     begin
          WTClose( FTablet );

          FTablet := 0;
     end;
end;

////////////////////////////////////////////////////////////////////////////////

procedure TForm1.FormCreate(Sender: TObject);
begin
     BeginTablet;
end;

procedure TForm1.FormDestroy(Sender: TObject);
begin
     EndTablet;
end;

////////////////////////////////////////////////////////////////////////////////

procedure TForm1.Timer1Timer(Sender: TObject);
var
   PsN, I :Integer;
   Ps :array[ 0..31 ] of PACKET;
begin
     PsN := WTPacketsGet( FTablet, 32, @Ps );

     if PsN > 0 then
     begin
          Memo1.Lines.Clear;

          for I := 0 to PsN-1 do
          begin
               with Ps[ I ] do Memo1.Lines.Add( pkX.ToString
                                       + ', ' + pkY.ToString
                                       + ', ' + pkNormalPressure.ToString );
          end;
     end;
end;

end. //#########################################################################

