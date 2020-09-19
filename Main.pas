unit Main;

interface //####################################################################

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs,
  FMX.Controls.Presentation, FMX.ScrollBox, FMX.Memo, FMX.Memo.Types,
  WINTAB, Core;

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
    _Tablet  :HCTX;
    _PosMinX :Integer;
    _PosMinY :Integer;
    _PosMaxX :Integer;
    _PosMaxY :Integer;
    _ResX    :Integer;
    _ResY    :Integer;
    _UniX    :Integer;
    _UniY    :Integer;
    _PreMin  :Integer;
    _PreMax  :Integer;
    _WheMin  :Integer;
    _WheMax  :Integer;
    _AziMin  :Integer;
    _AziMax  :Integer;
    _AltMin  :Integer;
    _AltMax  :Integer;
    _TwiMin  :Integer;
    _TwiMax  :Integer;
    ///// メソッド
    procedure TabletInfo;
    procedure BeginTablet;
    procedure EndTablet;
  end;

var
  Form1: TForm1;

implementation //###############################################################

{$R *.fmx}

uses FMX.Platform.Win, Winapi.Windows;

//%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% private

//%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% public

procedure TForm1.TabletInfo;
var
   A :AXIS;
   A3 :array [ 1..3 ] of AXIS;
begin
     WTInfo( WTI_DEVICES, DVC_X, @A );
     _PosMinX :=         A.axMin;                  // Ｘ座標の最小値
     _PosMaxX :=         A.axMax;                  // Ｘ座標の最大値
     _UniX    :=         A.axUnits;                // Ｘ座標の単位
     _ResX    := HIWORD( A.axResolution );         // Ｘ座標の分解能（line/inch）

     WTInfo( WTI_DEVICES, DVC_Y, @A );
     _PosMinY :=         A.axMin;                  // Ｙ座標の最小値
     _PosMaxY :=         A.axMax;                  // Ｙ座標の最大値
     _UniY    :=         A.axUnits;                // Ｙ座標の単位
     _ResY    := HIWORD( A.axResolution );         // Ｙ座標の分解能（line/inch）

     WTInfo( WTI_DEVICES, DVC_NPRESSURE, @A );
     _PreMin := A.axMin;                          // 筆圧の最小値
     _PreMax := A.axMax;                          // 筆圧の最大値

     WTInfo( WTI_DEVICES, DVC_TPRESSURE, @A );
     _WheMin := A.axMin;                          // ホイールの最小値
     _WheMax := A.axMax;                          // ホイールの最大値

     WTInfo( WTI_DEVICES, DVC_ORIENTATION, @A3 );
     _AziMin := A3[ 1 ].axMin;                    // ペンの方位の最小値
     _AziMax := A3[ 1 ].axMax;                    // ペンの方位の最大値
     _AltMin := A3[ 2 ].axMin;                    // ペンの傾きの最小値
     _AltMax := A3[ 2 ].axMax;                    // ペンの傾きの最大値
     _TwiMin := A3[ 3 ].axMin;                    // ペンの捩れの最小値
     _TwiMax := A3[ 3 ].axMax;                    // ペンの捩れの最大値
end;

//------------------------------------------------------------------------------

procedure TForm1.BeginTablet;
var
   C :LOGCONTEXT;
begin
     WTInfo( WTI_DEFSYSCTX, 0, @C );

     with C do
     begin
          StrCopy( lcName, PChar( 'WacomTablet ' + IntToHex( HInstance, 8 ) ) );

          lcOptions   := lcOptions or CXO_SYSTEM;
          lcMsgBase   := WT_DEFBASE;
          lcPktData   := PACKETDATA;
          lcPktMode   := PACKETMODE;
          lcMoveMask  := PACKETDATA;
          lcBtnUpMask := lcBtnDnMask;

          lcInOrgX    := _PosMinX;  // 入力Ｘ座標の最小値
          lcInOrgY    := _PosMinY;  // 入力Ｙ座標の最小値
          lcInExtX    := _PosMaxX;  // 入力Ｘ座標の最大値
          lcInExtY    := _PosMaxY;  // 入力Ｙ座標の最大値
          lcOutOrgX   := _PosMinX;  // ウィンドウＸ座標の最小値
          lcOutOrgY   := _PosMinY;  // ウィンドウＹ座標の最小値
          lcOutExtX   := _PosMaxX;  // ウィンドウＸ座標の最大値
          lcOutExtY   := _PosMaxY;  // ウィンドウＹ座標の最大値
     end;

     _Tablet := WTOpen( FormToHWND( Self ), @C, True );  // Wintab の初期化

     Assert( _Tablet > 0, '_Tablet = 0' );
end;

procedure TForm1.EndTablet;
begin
     WTClose( _Tablet );
end;

////////////////////////////////////////////////////////////////////////////////

procedure TForm1.FormCreate(Sender: TObject);
begin
     TabletInfo;

     with Memo1.Lines do
     begin
          Add( 'PosX = ' + _PosMinX.ToString + ' ～ ' + _PosMaxX.ToString );
          Add( 'PosY = ' + _PosMinY.ToString + ' ～ ' + _PosMaxY.ToString );

          Add( 'ResX = ' + _ResX.ToString + ' ( ' + _UniX.ToString + ' )' );
          Add( 'ResY = ' + _ResY.ToString + ' ( ' + _UniY.ToString + ' )' );

          Add( 'Pre = ' + _PreMin.ToString + ' ～ ' + _PreMax.ToString );

          Add( 'Whe = ' + _WheMin.ToString + ' ～ ' + _WheMax.ToString );

          Add( 'Azi = ' + _AziMin.ToString + ' ～ ' + _AziMax.ToString );
          Add( 'Alt = ' + _AltMin.ToString + ' ～ ' + _AltMax.ToString );
          Add( 'Twi = ' + _TwiMin.ToString + ' ～ ' + _TwiMax.ToString );
     end;

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
   Ps :array[ 0..32-1 ] of PACKET;
begin
     PsN := WTPacketsGet( _Tablet, 32, @Ps );

     if PsN > 0 then
     begin
          Memo1.Lines.Clear;

          for I := 0 to PsN-1 do
          begin
               with Ps[ I ] do Memo1.Lines.Add( pkX             .ToString
                                       + ', ' + pkY             .ToString
                                       + ', ' + pkNormalPressure.ToString );
          end;
     end;
end;

end. //#########################################################################

