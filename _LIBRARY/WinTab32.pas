unit WinTab32;

interface //#################################################################### ■

uses Windows, Messages;

const //$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$【定数】

      DLLNAME = 'Wintab32.dll';

      (* Messages *)
      WT_DEFBASE    = $7FF0;
      WT_MAXOFFSET  = $F;
      WT_PACKET     = WT_DEFBASE + 0;
      WT_CTXOPEN    = WT_DEFBASE + 1;
      WT_CTXCLOSE   = WT_DEFBASE + 2;
      WT_CTXUPDATE  = WT_DEFBASE + 3;
      WT_CTXOVERLAP = WT_DEFBASE + 4;
      WT_PROXIMITY  = WT_DEFBASE + 5;
      WT_INFOCHANGE = WT_DEFBASE + 6;
      WT_CSRCHANGE  = WT_DEFBASE + 7;  (* 1.1 *)
      WT_PACKETEXT	= WT_DEFBASE + 8;  (* 1.4 *)
      WT_MAX        = WT_DEFBASE + WT_MAXOFFSET;

      (* WTPKT bits *)
      PK_CONTEXT            = $0001;  (* reporting context *)
      PK_STATUS             = $0002;  (* status bits *)
      PK_TIME               = $0004;  (* time stamp *)
      PK_CHANGED            = $0008;  (* change bit vector *)
      PK_SERIAL_NUMBER      = $0010;  (* packet serial number *)
      PK_CURSOR             = $0020;  (* reporting cursor *)
      PK_BUTTONS            = $0040;  (* button information *)
      PK_X                  = $0080;  (* x axis *)
      PK_Y                  = $0100;  (* y axis *)
      PK_Z                  = $0200;  (* z axis *)
      PK_NORMAL_PRESSURE    = $0400;  (* normal or tip pressure *)
      PK_TANGENT_PRESSURE   = $0800;  (* tangential or barrel pressure *)
      PK_ORIENTATION        = $1000;  (* orientation info: tilts *)
      PK_ROTATION           = $2000;  (* rotation info; 1.1 *)

      LCNAMELEN = 40;

	 (* context option values *)
      CXO_SYSTEM	      = $0001;
      CXO_PEN	           = $0002;
      CXO_MESSAGES	      = $0004;
      CXO_MARGIN	      = $8000;
      CXO_MGNINSIDE	      = $4000;
      CXO_CSRMESSAGES     = $0008;  (* 1.1 *)

      (* context status values *)
      CXS_DISABLED	      = $0001;
      CXS_OBSCURED	      = $0002;
      CXS_ONTOP	      = $0004;

      (* context lock values *)
      CXL_INSIZE	      = $0001;
      CXL_INASPECT	      = $0002;
      CXL_SENSITIVITY     = $0004;
      CXL_MARGIN	      = $0008;
      CXL_SYSOUT	      = $0010;

      (* info categories *)

      WTI_DEFCONTEXT      = 3;
      WTI_DEFSYSCTX	      = 4;
      WTI_DDCTXS	      = 400;  (* 1.1 *)
      WTI_DSCTXS	      = 500;  (* 1.1 *)
      CTX_NAME	           = 1;
      CTX_OPTIONS	      = 2;
      CTX_STATUS	      = 3;
      CTX_LOCKS	      = 4;
      CTX_MSGBASE	      = 5;
      CTX_DEVICE	      = 6;
      CTX_PKTRATE	      = 7;
      CTX_PKTDATA	      = 8;
      CTX_PKTMODE	      = 9;
      CTX_MOVEMASK	      = 10;
      CTX_BTNDNMASK	      = 11;
      CTX_BTNUPMASK	      = 12;
      CTX_INORGX	      = 13;
      CTX_INORGY	      = 14;
      CTX_INORGZ	      = 15;
      CTX_INEXTX	      = 16;
      CTX_INEXTY	      = 17;
      CTX_INEXTZ	      = 18;
      CTX_OUTORGX	      = 19;
      CTX_OUTORGY	      = 20;
      CTX_OUTORGZ	      = 21;
      CTX_OUTEXTX	      = 22;
      CTX_OUTEXTY	      = 23;
      CTX_OUTEXTZ	      = 24;
      CTX_SENSX	      = 25;
      CTX_SENSY	      = 26;
      CTX_SENSZ	      = 27;
      CTX_SYSMODE	      = 28;
      CTX_SYSORGX	      = 29;
      CTX_SYSORGY	      = 30;
      CTX_SYSEXTX	      = 31;
      CTX_SYSEXTY	      = 32;
      CTX_SYSSENSX	      = 33;
      CTX_SYSSENSY	      = 34;
      CTX_MAX	           = 34;

      WTI_DEVICES	      = 100;
      DVC_NAME	           = 1;
      DVC_HARDWARE	      = 2;
      DVC_NCSRTYPES	      = 3;
      DVC_FIRSTCSR	      = 4;
      DVC_PKTRATE	      = 5;
      DVC_PKTDATA	      = 6;
      DVC_PKTMODE	      = 7;
      DVC_CSRDATA	      = 8;
      DVC_XMARGIN	      = 9;
      DVC_YMARGIN	      = 10;
      DVC_ZMARGIN	      = 11;
      DVC_X		      = 12;
      DVC_Y		      = 13;
      DVC_Z		      = 14;
      DVC_NPRESSURE	      = 15;
      DVC_TPRESSURE	      = 16;
      DVC_ORIENTATION     = 17;
      DVC_ROTATION	      = 18;  (* 1.1 *)
      DVC_PNPID	      = 19;  (* 1.1 *)
      DVC_MAX	           = 19;

type //$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$【型】

     HCTX  = THandle;

     WTPKT = Longword;

     FIX32 = Longword;

     //$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$【レコード】

     //%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% LOGCONTEXTA

     LOGCONTEXTA = packed record
       lcName      :array[ 0..LCNAMELEN-1 ] of AnsiChar;
       lcOptions   :UINT;
       lcStatus    :UINT;
       lcLocks     :UINT;
       lcMsgBase   :UINT;
       lcDevice    :UINT;
       lcPktRate   :UINT;
       lcPktData   :WTPKT;
       lcPktMode   :WTPKT;
       lcMoveMask  :WTPKT;
       lcBtnDnMask :DWORD;
       lcBtnUpMask :DWORD;
       lcInOrgX    :LONG;
       lcInOrgY    :LONG;
       lcInOrgZ    :LONG;
       lcInExtX    :LONG;
       lcInExtY    :LONG;
       lcInExtZ    :LONG;
       lcOutOrgX   :LONG;
       lcOutOrgY   :LONG;
       lcOutOrgZ   :LONG;
       lcOutExtX   :LONG;
       lcOutExtY   :LONG;
       lcOutExtZ   :LONG;
       lcSensX     :FIX32;
       lcSensY     :FIX32;
       lcSensZ     :FIX32;
       lcSysMode   :BOOL;
       lcSysOrgX   :Integer;
       lcSysOrgY   :Integer;
       lcSysExtX   :Integer;
       lcSysExtY   :Integer;
       lcSysSensX  :FIX32;
       lcSysSensY  :FIX32;
     end;

     //%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% LOGCONTEXTW

     LOGCONTEXTW = packed record
       lcName      :array[ 0..LCNAMELEN-1 ] of Char;
       lcOptions   :UINT;
       lcStatus    :UINT;
       lcLocks     :UINT;
       lcMsgBase   :UINT;
       lcDevice    :UINT;
       lcPktRate   :UINT;
       lcPktData   :WTPKT;
       lcPktMode   :WTPKT;
       lcMoveMask  :WTPKT;
       lcBtnDnMask :DWORD;
       lcBtnUpMask :DWORD;
       lcInOrgX    :LONG;
       lcInOrgY    :LONG;
       lcInOrgZ    :LONG;
       lcInExtX    :LONG;
       lcInExtY    :LONG;
       lcInExtZ    :LONG;
       lcOutOrgX   :LONG;
       lcOutOrgY   :LONG;
       lcOutOrgZ   :LONG;
       lcOutExtX   :LONG;
       lcOutExtY   :LONG;
       lcOutExtZ   :LONG;
       lcSensX     :FIX32;
       lcSensY     :FIX32;
       lcSensZ     :FIX32;
       lcSysMode   :BOOL;
       lcSysOrgX   :Integer;
       lcSysOrgY   :Integer;
       lcSysExtX   :Integer;
       lcSysExtY   :Integer;
       lcSysSensX  :FIX32;
       lcSysSensY  :FIX32;
     end;

     //%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% AXIS

     AXIS = packed record
       axMin        :LONG;
       axMax        :LONG;
       axUnits      :UINT;
       axResolution :FIX32;
     end;

     //%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% PACKET

     PACKET = packed record
//     pkContext         :HCTX;        // PK_CONTEXT
//     pkStatus          :Cardinal;    // PK_STATUS
//     pkTime            :Longword;    // PK_TIME
//     pkChanged         :WTPKT;       // PK_CHANGED
//     pkSerialNumber    :Cardinal;    // PK_SERIAL_NUMBER
//     pkCursor          :Cardinal;    // PK_CURSOR
//     pkButtons         :Longword;    // PK_BUTTONS
       pkX               :LongInt;     // PK_X
       pkY               :LongInt;     // PK_Y
//     pkZ               :LongInt;     // PK_Z
       pkNormalPressure  :Integer;     // PK_NORMAL_PRESSURE
//     pkTangentPressure :Integer;     // PK_TANGENT_PRESSURE
//     pkOrientation     :ORIENTATION; // PK_ORIENTATION
//     pkRotation        :ROTATION;    // PK_ROTATION  Ver 1.1
     end;

const //$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$【定数】

      // this constant is used to define PACKET record
      PACKETDATA = PK_X or PK_Y or PK_NORMAL_PRESSURE;

      // this constant is used to define PACKET record
      PACKETMODE = 0; //This means all values are reported "absoulte"

//var //$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$【変数】

//$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$【ルーチン】

function WTInfoA( wCategory,nIndex:Cardinal; lpOutput:Pointer ) :Cardinal; stdcall; external DLLNAME;
function WTInfoW( wCategory,nIndex:Cardinal; lpOutput:Pointer ) :Cardinal; stdcall; external DLLNAME;

function WTOpenA( hw:HWND; var lc:LOGCONTEXTA; fEnable:BOOL ) :HCTX; stdcall; external DLLNAME;
function WTOpenW( hw:HWND; var lc:LOGCONTEXTW; fEnable:BOOL ) :HCTX; stdcall; external DLLNAME;

function WTClose( hc:HCTX ) :LongBool; stdcall; external DLLNAME;

function WTPacketsGet( hc:HCTX; cMaxPackets:Integer; lpPkts:Pointer ) :Integer; stdcall; external DLLNAME;

function WTPacket( hc:HCTX; wSerial:Cardinal; lpPkts:Pointer ) :LongBool; stdcall; external DLLNAME;

implementation //############################################################### ■

//$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$【レコード】

//%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% {RECORD}

//&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&& private

//&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&& public

//$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$【クラス】

//%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% {CLASS}

//&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&& private

//&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&& protected

//&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&& public

//$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$【ルーチン】

//############################################################################## □

initialization //$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$ 初期化

finalization //$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$ 最終化

end. //######################################################################### ■