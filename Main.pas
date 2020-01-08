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
    _Tablet :HCTX;
  protected
    ///// メソッド
    procedure CreateHandle; override;
    procedure DestroyHandle; override;
  public
    { public 宣言 }
    _PosXMin :Int32;
    _PosXMax :Int32;
    _UniX    :UInt32;
    _ResX    :Int32;
    _PosYMin :Int32;
    _PosYMax :Int32;
    _UniY    :UInt32;
    _ResY    :Int32;
    _PresMin :Int32;
    _PresMax :Int32;
    _WheeMin :Int32;
    _WheeMax :Int32;
    _AzimMin :Int32;
    _AzimMax :Int32;
    _AltiMin :Int32;
    _AltiMax :Int32;
    _TwisMin :Int32;
    _TwisMax :Int32;
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

//%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% protected

procedure TForm1.CreateHandle;
begin
     inherited;

     BeginTablet;
end;

procedure TForm1.DestroyHandle;
begin
     EndTablet;

     inherited;
end;

//%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% public

procedure TForm1.TabletInfo;
var
   A :AXIS;
   A3 :array [ 1..3 ] of AXIS;
begin
     WTInfo( WTI_DEVICES, DVC_X, @A );
     _PosXMin :=         A.axMin;                  // Ｘ座標の最小値
     _PosXMax :=         A.axMax;                  // Ｘ座標の最大値
     _UniX    :=         A.axUnits;                // Ｘ座標の単位
     _ResX    := HIWORD( A.axResolution );         // Ｘ座標の分解能（line/inch）

     WTInfo( WTI_DEVICES, DVC_Y, @A );
     _PosYMin :=         A.axMin;                  // Ｙ座標の最小値
     _PosYMax :=         A.axMax;                  // Ｙ座標の最大値
     _UniY    :=         A.axUnits;                // Ｙ座標の単位
     _ResY    := HIWORD( A.axResolution );         // Ｙ座標の分解能（line/inch）

     WTInfo( WTI_DEVICES, DVC_NPRESSURE, @A );
     _PresMin := A.axMin;                          // 筆圧の最小値
     _PresMax := A.axMax;                          // 筆圧の最大値

     WTInfo( WTI_DEVICES, DVC_TPRESSURE, @A );
     _WheeMin := A.axMin;                          // ホイールの最小値
     _WheeMax := A.axMax;                          // ホイールの最大値

     WTInfo( WTI_DEVICES, DVC_ORIENTATION, @A3 );
     _AzimMin := A3[ 1 ].axMin;                    // ペンの傾き方向の最小値
     _AzimMax := A3[ 1 ].axMax;                    // ペンの傾き方向の最大値
     _AltiMin := A3[ 2 ].axMin;                    // ペンの傾きの最小値
     _AltiMax := A3[ 2 ].axMax;                    // ペンの傾きの最大値
     _TwisMin := A3[ 3 ].axMin;                    // ペンの捩れの最小値
     _TwisMax := A3[ 3 ].axMax;                    // ペンの捩れの最大値
end;

//------------------------------------------------------------------------------

procedure TForm1.BeginTablet;
var
   C :LOGCONTEXT;
begin
     WTInfo( WTI_DEFCONTEXT, 0, @C );  // デジタイジングコンテキスト
     //WTInfo( WTI_DEFSYSCTX, 0, @C );  // システムコンテキスト

     with C do
     begin
          StrCopy( lcName, PChar( 'WacomTablet ' + IntToHex( HInstance, 8 ) ) );

          lcOptions   := lcOptions or CXO_MESSAGES or CXO_SYSTEM;  // デジタイジングコンテキストに必要

          lcMsgBase   := WT_DEFBASE;
          lcPktData   := PACKETDATA;
          lcPktMode   := PACKETMODE;
          lcMoveMask  := PACKETDATA;
          lcBtnUpMask := lcBtnDnMask;

          lcInOrgX    := _PosXMin;  // 入力Ｘ座標の最小値
          lcInOrgY    := _PosYMin;  // 入力Ｙ座標の最小値
          lcInExtX    := _PosXMax;  // 入力Ｘ座標の最大値
          lcInExtY    := _PosYMax;  // 入力Ｙ座標の最大値
          lcOutOrgX   := _PosXMin;  // ウィンドウＸ座標の最小値
          lcOutOrgY   := _PosYMin;  // ウィンドウＹ座標の最小値
          lcOutExtX   := _PosXMax;  // ウィンドウＸ座標の最大値
          lcOutExtY   := _PosYMax;  // ウィンドウＹ座標の最大値
     end;

     _Tablet := WTOpen( FormToHWND( Self ), C, True );  // Wintab の初期化

     Assert( _Tablet > 0, '_Tablet = 0' );
end;

procedure TForm1.EndTablet;
begin
     WTClose( _Tablet );
end;

////////////////////////////////////////////////////////////////////////////////

procedure TForm1.FormCreate(Sender: TObject);
begin
     Assert( IsWinTab32Available );

     TabletInfo;

     with Memo1.Lines do
     begin
          Add( 'PosX = ' + _PosXMin.ToString + ' ～ ' + _PosXMax.ToString );
          Add( 'PosY = ' + _PosYMin.ToString + ' ～ ' + _PosYMax.ToString );

          Add( 'ResX = ' + _ResX.ToString + ' ( ' + _UniX.ToString + ' )' );
          Add( 'ResY = ' + _ResY.ToString + ' ( ' + _UniY.ToString + ' )' );

          Add( 'Pres = ' + _PresMin.ToString + ' ～ ' + _PresMax.ToString );

          Add( 'Whee = ' + _WheeMin.ToString + ' ～ ' + _WheeMax.ToString );

          Add( 'Azim = ' + _AzimMin.ToString + ' ～ ' + _AzimMax.ToString );
          Add( 'Alti = ' + _AltiMin.ToString + ' ～ ' + _AltiMax.ToString );
          Add( 'Twis = ' + _TwisMin.ToString + ' ～ ' + _TwisMax.ToString );
     end;
end;

procedure TForm1.FormDestroy(Sender: TObject);
begin
     /////
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

