unit LUX.WinTab;

interface //#################################################################### ■

uses FMX.Forms,
     Winapi.Windows,
     WINTAB;

type //$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$【型】

     //$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$【レコード】

     //%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% TTabletPacket

     TTabletPacket = packed record
     { Context         :HCTX;         // PK_CONTEXT       }
       Status          :UINT;         // PK_STATUS
     { Time            :DWORD;        // PK_TIME          }
     { Changed         :WTPKT;        // PK_CHANGED       }
     { SerialNumber    :UINT;         // PK_SERIAL_NUMBER }
       Cursor          :UINT;         // PK_CURSOR
       Buttons         :DWORD;        // PK_BUTTONS
       X               :LONG;         // PK_X
       Y               :LONG;         // PK_Y
       Z               :LONG;         // PK_Z
       NormalPressure  :UINT;         // PK_NORMAL_PRESSURE
       TangentPressure :UINT;         // PK_TANGENT_PRESSURE
       Orientation     :ORIENTATION;  // PK_ORIENTATION
     end;

     //$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$【クラス】

     //%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% TPenTablet

     TPacketEvent = reference to procedure( const Packet_:TTabletPacket );

     TPenTablet = class
     private
     protected
       _Form    :TCommonCustomForm;
       _Handle  :HCTX;
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
       ///// イベント
       _OnPacket :TPacketEvent;
       ///// アクセス
       function GetOnPacket :TPacketEvent;
       procedure SetOnPacket( const OnPacket_:TPacketEvent );
       ///// メソッド
       procedure GetInfos;
       procedure BeginTablet( const Options_:UINT = 0 );
       procedure EndTablet;
       procedure OnMessage( const MSG_:TMsg );
     public
       constructor Create( const Form_:TCommonCustomForm ); overload;
       destructor Destroy; override;
       ///// プロパティ
       property Form    :TCommonCustomForm read _Form   ;
       property Handle  :HCTX              read _Handle ;
       property PosMinX :Integer           read _PosMinX;
       property PosMinY :Integer           read _PosMinY;
       property PosMaxX :Integer           read _PosMaxX;
       property PosMaxY :Integer           read _PosMaxY;
       property ResX    :Integer           read _ResX   ;
       property ResY    :Integer           read _ResY   ;
       property UniX    :Integer           read _UniX   ;
       property UniY    :Integer           read _UniY   ;
       property PreMin  :Integer           read _PreMin ;
       property PreMax  :Integer           read _PreMax ;
       property WheMin  :Integer           read _WheMin ;
       property WheMax  :Integer           read _WheMax ;
       property AziMin  :Integer           read _AziMin ;
       property AziMax  :Integer           read _AziMax ;
       property AltMin  :Integer           read _AltMin ;
       property AltMax  :Integer           read _AltMax ;
       property TwiMin  :Integer           read _TwiMin ;
       property TwiMax  :Integer           read _TwiMax ;
       ///// イベント
       property OnPacket :TPacketEvent read GetOnPacket write SetOnPacket;
     end;

const //$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$【定数】

      PACKETDATA = PK_STATUS
                or PK_CURSOR
                or PK_BUTTONS
                or PK_X
                or PK_Y
                or PK_Z
                or PK_NORMAL_PRESSURE
                or PK_TANGENT_PRESSURE
                or PK_ORIENTATION;

      PACKETMODE = 0;

//var //$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$【変数】

//$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$【ルーチン】

implementation //############################################################### ■

uses FMX.Platform.Win,
     LUX.FMX.Messaging.Win;

//$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$【レコード】

//$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$【クラス】

//%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% TPenTablet

//&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&& private

//&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&& protected

/////////////////////////////////////////////////////////////////////// アクセス

function TPenTablet.GetOnPacket :TPacketEvent;
begin
     Result := _OnPacket;
end;

procedure TPenTablet.SetOnPacket( const OnPacket_:TPacketEvent );
begin
     if Assigned( _OnPacket ) then TMessageService.EventList.Remove( WT_PACKET );

     EndTablet;

     _OnPacket := OnPacket_;

     if Assigned( _OnPacket ) then
     begin
          BeginTablet( CXO_MESSAGES );

          TMessageService.EventList.Add( WT_PACKET, OnMessage );
     end
     else BeginTablet;
end;

/////////////////////////////////////////////////////////////////////// メソッド

procedure TPenTablet.GetInfos;
var
   A :AXIS;
   A3 :array [ 1..3 ] of AXIS;
begin
     WTInfo( WTI_DEVICES, DVC_X, @A );
     _PosMinX :=         A.axMin;
     _PosMaxX :=         A.axMax;
     _UniX    :=         A.axUnits;
     _ResX    := HIWORD( A.axResolution );

     WTInfo( WTI_DEVICES, DVC_Y, @A );
     _PosMinY :=         A.axMin;
     _PosMaxY :=         A.axMax;
     _UniY    :=         A.axUnits;
     _ResY    := HIWORD( A.axResolution );

     WTInfo( WTI_DEVICES, DVC_NPRESSURE, @A );
     _PreMin := A.axMin;
     _PreMax := A.axMax;

     WTInfo( WTI_DEVICES, DVC_TPRESSURE, @A );
     _WheMin := A.axMin;
     _WheMax := A.axMax;

     WTInfo( WTI_DEVICES, DVC_ORIENTATION, @A3 );
     _AziMin := A3[ 1 ].axMin;
     _AziMax := A3[ 1 ].axMax;
     _AltMin := A3[ 2 ].axMin;
     _AltMax := A3[ 2 ].axMax;
     _TwiMin := A3[ 3 ].axMin;
     _TwiMax := A3[ 3 ].axMax;
end;

//------------------------------------------------------------------------------

procedure TPenTablet.BeginTablet( const Options_:UINT = 0 );
var
   C :LOGCONTEXT;
begin
     WTInfo( WTI_DEFCONTEXT, 0, @C );

     with C do
     begin
          lcOptions   := lcOptions or Options_;
          lcMsgBase   := WT_DEFBASE;
          lcPktData   := PACKETDATA;
          lcPktMode   := PACKETMODE;
          lcMoveMask  := PACKETDATA;
          lcBtnUpMask := lcBtnDnMask;

          lcInOrgX    := _PosMinX;
          lcInOrgY    := _PosMinY;
          lcInExtX    := _PosMaxX;
          lcInExtY    := _PosMaxY;
          lcOutOrgX   := _PosMinX;
          lcOutOrgY   := _PosMinY;
          lcOutExtX   := _PosMaxX;
          lcOutExtY   := _PosMaxY;
     end;

     _Handle := WTOpen( FormToHWND( _Form ), @C, True );

     Assert( _Handle > 0, '_Tablet = 0' );
end;

procedure TPenTablet.EndTablet;
begin
     WTClose( _Handle );
end;

//------------------------------------------------------------------------------

procedure TPenTablet.OnMessage( const MSG_:TMsg );
var
   P :TTabletPacket;
begin
     WTPacket( MSG_.lParam, MSG_.wParam, @P );

     if Assigned( _OnPacket ) then _OnPacket( P );
end;

//&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&& public

constructor TPenTablet.Create( const Form_:TCommonCustomForm );
begin
     inherited Create;

     GetInfos;

     _Form := Form_;

     BeginTablet;
end;

destructor TPenTablet.Destroy;
begin
     EndTablet;

     inherited;
end;

//$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$【ルーチン】

//############################################################################## □

initialization //$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$ 初期化

finalization //$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$ 最終化

end. //######################################################################### ■