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

     TPenTablet = class
     private
     protected
       _Form    :TCommonCustomForm;
       _Context :LOGCONTEXT;
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
       ///// アクセス
       function GetQueueSize :Integer;
       procedure SetQueueSize( const QueueSize_:Integer );
       ///// メソッド
       procedure GetInfos;
       procedure GetDefContext;
       procedure ApplyContext;
       procedure OptionAdd( const Option_:UINT );
       procedure OptionDel( const Option_:UINT );
     public
       constructor Create;
       destructor Destroy; override;
       ///// プロパティ
       property Handle    :HCTX    read   _Handle                      ;
       property PosMinX   :Integer read   _PosMinX                     ;
       property PosMinY   :Integer read   _PosMinY                     ;
       property PosMaxX   :Integer read   _PosMaxX                     ;
       property PosMaxY   :Integer read   _PosMaxY                     ;
       property ResX      :Integer read   _ResX                        ;
       property ResY      :Integer read   _ResY                        ;
       property UniX      :Integer read   _UniX                        ;
       property UniY      :Integer read   _UniY                        ;
       property PreMin    :Integer read   _PreMin                      ;
       property PreMax    :Integer read   _PreMax                      ;
       property WheMin    :Integer read   _WheMin                      ;
       property WheMax    :Integer read   _WheMax                      ;
       property AziMin    :Integer read   _AziMin                      ;
       property AziMax    :Integer read   _AziMax                      ;
       property AltMin    :Integer read   _AltMin                      ;
       property AltMax    :Integer read   _AltMax                      ;
       property TwiMin    :Integer read   _TwiMin                      ;
       property TwiMax    :Integer read   _TwiMax                      ;
       property QueueSize :Integer read GetQueueSize write SetQueueSize;
       ///// メソッド
       function GetPakets( var Packets_:array of TTabletPacket ) :Integer;
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

uses FMX.Platform.Win;

//$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$【レコード】

//$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$【クラス】

//%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% TPenTablet

//&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&& private

//&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&& protected

/////////////////////////////////////////////////////////////////////// アクセス

function TPenTablet.GetQueueSize :Integer;
begin
     Result := WTQueueSizeGet( _Handle );
end;

procedure TPenTablet.SetQueueSize( const QueueSize_:Integer );
begin
     WTQueueSizeSet( _Handle, QueueSize_ );
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

procedure TPenTablet.GetDefContext;
begin
     WTInfo( WTI_DEFCONTEXT, 0, @_Context );

     with _Context do
     begin
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
end;

procedure TPenTablet.ApplyContext;
begin
     WTSet( _Handle, @_Context );
end;

procedure TPenTablet.OptionAdd( const Option_:UINT );
begin
     with _Context do lcOptions := lcOptions or Option_;

     ApplyContext;
end;

procedure TPenTablet.OptionDel( const Option_:UINT );
begin
     with _Context do lcOptions := lcOptions and not Option_;

     ApplyContext;
end;

//&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&& public

constructor TPenTablet.Create;
begin
     inherited;

     GetInfos;

     GetDefContext;

     _Form := TCommonCustomForm.CreateNew( nil );

     _Handle := WTOpen( FormToHWND( _Form ), @_Context, True );

     Assert( _Handle > 0, '_Tablet = 0' );
end;

destructor TPenTablet.Destroy;
begin
     WTClose( _Handle );

     _Form.Free;

     inherited;
end;

/////////////////////////////////////////////////////////////////////// メソッド

function TPenTablet.GetPakets( var Packets_:array of TTabletPacket ) :Integer;
begin
     Result := WTPacketsGet( _Handle, Length( Packets_ ), @Packets_ );
end;

//$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$【ルーチン】

//############################################################################## □

initialization //$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$ 初期化

finalization //$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$ 最終化

end. //######################################################################### ■