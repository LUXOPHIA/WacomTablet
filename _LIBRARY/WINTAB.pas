(* -------------------------------- wintab.h -------------------------------- *)
(* Combined 16 & 32-bit version. *)

(*------------------------------------------------------------------------------
The text and information contained in this file may be freely used,
copied, or distributed without compensation or licensing restrictions.

This file is Copyright (c) Wacom Company, Ltd. 2010 All Rights Reserved
with portions copyright 1991-1998 by LCS/Telegraphics.
------------------------------------------------------------------------------*)

unit WINTAB;

{$IFNDEF _INC_WINTAB } (* prevent multiple includes *)
{$DEFINE _INC_WINTAB }

{$IFDEF __cplusplus }
//extern "C" {
{$ENDIF} (* __cplusplus *)

interface //#################################################################### ■

uses Winapi.Windows;

(* -------------------------------------------------------------------------- *)
(* Messages *)
{$IFNDEF NOWTMESSAGES }

    const WT_DEFBASE       = $7FF0;
    const WT_MAXOFFSET     = $F;

    const WT_PACKET        = WT_DEFBASE + 0;
    const WT_CTXOPEN       = WT_DEFBASE + 1;
    const WT_CTXCLOSE      = WT_DEFBASE + 2;
    const WT_CTXUPDATE     = WT_DEFBASE + 3;
    const WT_CTXOVERLAP    = WT_DEFBASE + 4;
    const WT_PROXIMITY     = WT_DEFBASE + 5;
    const WT_INFOCHANGE    = WT_DEFBASE + 6;
    const WT_CSRCHANGE     = WT_DEFBASE + 7;             (* 1.1 *)
    const WT_PACKETEXT     = WT_DEFBASE + 8;             (* 1.4 *)
    const WT_MAX           = WT_DEFBASE + WT_MAXOFFSET;

{$ENDIF}

(* -------------------------------------------------------------------------- *)
(* -------------------------------------------------------------------------- *)
(* Data Types *)

(* -------------------------------------------------------------------------- *)
(* COMMON DATA DEFS *)

type HMGR    = type THandle;  (* manager handle *)
type HCTX    = type THandle;  (* context handle *)
type HWTHOOK = type THandle;  (* hook handle *)

type WTPKT = DWORD;  (* packet mask *)


{$IFNDEF NOWTPKT }

    (* WTPKT bits *)
    const PK_CONTEXT             = $0001;  (* reporting context *)
    const PK_STATUS              = $0002;  (* status bits *)
    const PK_TIME                = $0004;  (* time stamp *)
    const PK_CHANGED             = $0008;  (* change bit vector *)
    const PK_SERIAL_NUMBER       = $0010;  (* packet serial number *)
    const PK_CURSOR              = $0020;  (* reporting cursor *)
    const PK_BUTTONS             = $0040;  (* button information *)
    const PK_X                   = $0080;  (* x axis *)
    const PK_Y                   = $0100;  (* y axis *)
    const PK_Z                   = $0200;  (* z axis *)
    const PK_NORMAL_PRESSURE     = $0400;  (* normal or tip pressure *)
    const PK_TANGENT_PRESSURE    = $0800;  (* tangential or barrel pressure *)
    const PK_ORIENTATION         = $1000;  (* orientation info: tilts *)
    const PK_ROTATION            = $2000;  (* rotation info; 1.1 *)

{$ENDIF}

type FIX32 = DWORD;  (* fixed-point arithmetic type *)

//{$IFNDEF NOFIX32 }
//    #define INT(x)     HIWORD(x)
//    #define FRAC(x)    LOWORD(x)
//
//    #define CASTFIX32(x)    ((FIX32)((x)*65536L))
//
//    #define ROUND(x)    (INT(x) + (FRAC(x) > (WORD)$8000))
//
//    #define FIX_MUL(c, a, b)                         \
//        (c = (((DWORD)FRAC(a) * FRAC(b)) >> 16) +    \
//            (DWORD)INT(a) * FRAC(b) +                \
//            (DWORD)INT(b) * FRAC(a) +                \
//            ((DWORD)INT(a) * INT(b) << 16))
//
//    {$IFDEF _WINDLL }
//        #define FIX_DIV_SC static
//    {$ELSE}
//        #define FIX_DIV_SC
//    {$ENDIF}
//
//    #define FIX_DIV(c, a, b)                      \
//        {                                         \
//            FIX_DIV_SC DWORD temp, rem, btemp;    \
//                                                  \
//            (* fraction done bytewise *)          \
//            temp = ((a / b) << 16);               \
//            rem = a % b;                          \
//            btemp = b;                            \
//            if (INT(btemp) < 256) {               \
//                rem <<= 8;                        \
//            }                                     \
//            else {                                \
//                btemp >>= 8;                      \
//            }                                     \
//            temp += ((rem / btemp) << 8);         \
//            rem %= btemp;                         \
//            rem <<= 8;                            \
//            temp += rem / btemp;                  \
//            c = temp;                             \
//        }
//{$ENDIF}

(* -------------------------------------------------------------------------- *)
(* INFO DATA DEFS *)

{$IFNDEF NOWTINFO }

{$IFNDEF NOWTAXIS }

type AXIS = record
       axMin         :LONG;
       axMax         :LONG;
       axUnits       :UINT;
       axResolution  :FIX32;
     end;
    PAXIS = ^AXIS;
   NPAXIS = PAXIS;
   LPAXIS = PAXIS;

    (* unit specifiers *)
    const TU_NONE           = 0;
    const TU_INCHES         = 1;
    const TU_CENTIMETERS    = 2;
    const TU_CIRCLE         = 3;

{$ENDIF}

{$IFNDEF NOWTSYSBUTTONS }

(* system button assignment values *)
const SBN_NONE          = $00;
const SBN_LCLICK        = $01;
const SBN_LDBLCLICK     = $02;
const SBN_LDRAG         = $03;
const SBN_RCLICK        = $04;
const SBN_RDBLCLICK     = $05;
const SBN_RDRAG         = $06;
const SBN_MCLICK        = $07;
const SBN_MDBLCLICK     = $08;
const SBN_MDRAG         = $09;
(* for Pen Windows *)
const SBN_PTCLICK       = $10;
const SBN_PTDBLCLICK    = $20;
const SBN_PTDRAG        = $30;
const SBN_PNCLICK       = $40;
const SBN_PNDBLCLICK    = $50;
const SBN_PNDRAG        = $60;
const SBN_P1CLICK       = $70;
const SBN_P1DBLCLICK    = $80;
const SBN_P1DRAG        = $90;
const SBN_P2CLICK       = $A0;
const SBN_P2DBLCLICK    = $B0;
const SBN_P2DRAG        = $C0;
const SBN_P3CLICK       = $D0;
const SBN_P3DBLCLICK    = $E0;
const SBN_P3DRAG        = $F0;

{$ENDIF}

{$IFNDEF NOWTCAPABILITIES }

(* hardware capabilities *)
const HWC_INTEGRATED        = $0001;
const HWC_TOUCH             = $0002;
const HWC_HARDPROX          = $0004;
const HWC_PHYSID_CURSORS    = $0008;  (* 1.1 *)
{$ENDIF}

{$IFNDEF NOWTIFC }

{$IFNDEF NOWTCURSORS }

(* cursor capabilities *)
const CRC_MULTIMODE    = $0001;  (* 1.1 *)
const CRC_AGGREGATE    = $0002;  (* 1.1 *)
const CRC_INVERT       = $0004;  (* 1.1 *)

{$ENDIF}

(* info categories *)
const WTI_INTERFACE          = 1;
    const IFC_WINTABID       = 1;
    const IFC_SPECVERSION    = 2;
    const IFC_IMPLVERSION    = 3;
    const IFC_NDEVICES       = 4;
    const IFC_NCURSORS       = 5;
    const IFC_NCONTEXTS      = 6;
    const IFC_CTXOPTIONS     = 7;
    const IFC_CTXSAVESIZE    = 8;
    const IFC_NEXTENSIONS    = 9;
    const IFC_NMANAGERS      = 10;
    const IFC_MAX            = 10;


{$ENDIF}

{$IFNDEF NOWTSTATUS }

const WTI_STATUS           = 2;
    const STA_CONTEXTS     = 1;
    const STA_SYSCTXS      = 2;
    const STA_PKTRATE      = 3;
    const STA_PKTDATA      = 4;
    const STA_MANAGERS     = 5;
    const STA_SYSTEM       = 6;
    const STA_BUTTONUSE    = 7;
    const STA_SYSBTNUSE    = 8;
    const STA_MAX          = 8;

{$ENDIF}

{$IFNDEF NOWTDEFCONTEXT }

const WTI_DEFCONTEXT       = 3;
const WTI_DEFSYSCTX        = 4;
const WTI_DDCTXS           = 400;  (* 1.1 *)
const WTI_DSCTXS           = 500;  (* 1.1 *)
    const CTX_NAME         = 1;
    const CTX_OPTIONS      = 2;
    const CTX_STATUS       = 3;
    const CTX_LOCKS        = 4;
    const CTX_MSGBASE      = 5;
    const CTX_DEVICE       = 6;
    const CTX_PKTRATE      = 7;
    const CTX_PKTDATA      = 8;
    const CTX_PKTMODE      = 9;
    const CTX_MOVEMASK     = 10;
    const CTX_BTNDNMASK    = 11;
    const CTX_BTNUPMASK    = 12;
    const CTX_INORGX       = 13;
    const CTX_INORGY       = 14;
    const CTX_INORGZ       = 15;
    const CTX_INEXTX       = 16;
    const CTX_INEXTY       = 17;
    const CTX_INEXTZ       = 18;
    const CTX_OUTORGX      = 19;
    const CTX_OUTORGY      = 20;
    const CTX_OUTORGZ      = 21;
    const CTX_OUTEXTX      = 22;
    const CTX_OUTEXTY      = 23;
    const CTX_OUTEXTZ      = 24;
    const CTX_SENSX        = 25;
    const CTX_SENSY        = 26;
    const CTX_SENSZ        = 27;
    const CTX_SYSMODE      = 28;
    const CTX_SYSORGX      = 29;
    const CTX_SYSORGY      = 30;
    const CTX_SYSEXTX      = 31;
    const CTX_SYSEXTY      = 32;
    const CTX_SYSSENSX     = 33;
    const CTX_SYSSENSY     = 34;
    const CTX_MAX          = 34;

{$ENDIF}

{$IFNDEF NOWTDEVICES }

const WTI_DEVICES            = 100;
    const DVC_NAME           = 1;
    const DVC_HARDWARE       = 2;
    const DVC_NCSRTYPES      = 3;
    const DVC_FIRSTCSR       = 4;
    const DVC_PKTRATE        = 5;
    const DVC_PKTDATA        = 6;
    const DVC_PKTMODE        = 7;
    const DVC_CSRDATA        = 8;
    const DVC_XMARGIN        = 9;
    const DVC_YMARGIN        = 10;
    const DVC_ZMARGIN        = 11;
    const DVC_X              = 12;
    const DVC_Y              = 13;
    const DVC_Z              = 14;
    const DVC_NPRESSURE      = 15;
    const DVC_TPRESSURE      = 16;
    const DVC_ORIENTATION    = 17;
    const DVC_ROTATION       = 18;   (* 1.1 *)
    const DVC_PNPID          = 19;   (* 1.1 *)
    const DVC_MAX            = 19;

{$ENDIF}

{$IFNDEF NOWTCURSORS }

const WTI_CURSORS             = 200;
    const CSR_NAME            = 1;
    const CSR_ACTIVE          = 2;
    const CSR_PKTDATA         = 3;
    const CSR_BUTTONS         = 4;
    const CSR_BUTTONBITS      = 5;
    const CSR_BTNNAMES        = 6;
    const CSR_BUTTONMAP       = 7;
    const CSR_SYSBTNMAP       = 8;
    const CSR_NPBUTTON        = 9;
    const CSR_NPBTNMARKS      = 10;
    const CSR_NPRESPONSE      = 11;
    const CSR_TPBUTTON        = 12;
    const CSR_TPBTNMARKS      = 13;
    const CSR_TPRESPONSE      = 14;
    const CSR_PHYSID          = 15;   (* 1.1 *)
    const CSR_MODE            = 16;   (* 1.1 *)
    const CSR_MINPKTDATA      = 17;   (* 1.1 *)
    const CSR_MINBUTTONS      = 18;   (* 1.1 *)
    const CSR_CAPABILITIES    = 19;   (* 1.1 *)
    const CSR_TYPE            = 20;   (* 1.2 *)
    const CSR_MAX             = 20;

{$ENDIF}

{$IFNDEF NOWTEXTENSIONS }

const WTI_EXTENSIONS        = 300;
    const EXT_NAME          = 1;
    const EXT_TAG           = 2;
    const EXT_MASK          = 3;
    const EXT_SIZE          = 4;
    const EXT_AXES          = 5;
    const EXT_DEFAULT       = 6;
    const EXT_DEFCONTEXT    = 7;
    const EXT_DEFSYSCTX     = 8;
    const EXT_CURSORS       = 9;
    const EXT_DEVICES       = 110;  (* Allow 100 cursors *)
    const EXT_MAX           = 210;  (* Allow 100 devices *)

{$ENDIF}

{$ENDIF}

(* -------------------------------------------------------------------------- *)
(* CONTEXT DATA DEFS *)

const LCNAMELEN     = 40;
const LC_NAMELEN    = 40;
{$IFDEF WIN32 }
type LOGCONTEXTA = record
       lcName       :array [ 0..LCNAMELEN-1 ] of AnsiChar;
       lcOptions    :UINT;
       lcStatus     :UINT;
       lcLocks      :UINT;
       lcMsgBase    :UINT;
       lcDevice     :UINT;
       lcPktRate    :UINT;
       lcPktData    :WTPKT;
       lcPktMode    :WTPKT;
       lcMoveMask   :WTPKT;
       lcBtnDnMask  :DWORD;
       lcBtnUpMask  :DWORD;
       lcInOrgX     :LONG;
       lcInOrgY     :LONG;
       lcInOrgZ     :LONG;
       lcInExtX     :LONG;
       lcInExtY     :LONG;
       lcInExtZ     :LONG;
       lcOutOrgX    :LONG;
       lcOutOrgY    :LONG;
       lcOutOrgZ    :LONG;
       lcOutExtX    :LONG;
       lcOutExtY    :LONG;
       lcOutExtZ    :LONG;
       lcSensX      :FIX32;
       lcSensY      :FIX32;
       lcSensZ      :FIX32;
       lcSysMode    :BOOL;
       lcSysOrgX    :Integer;
       lcSysOrgY    :Integer;
       lcSysExtX    :Integer;
       lcSysExtY    :Integer;
       lcSysSensX   :FIX32;
       lcSysSensY   :FIX32;
     end;
    PLOGCONTEXTA = ^LOGCONTEXTA;
   NPLOGCONTEXTA = PLOGCONTEXTA;
   LPLOGCONTEXTA = PLOGCONTEXTA;
type LOGCONTEXTW = record
       lcName       :array [ 0..LCNAMELEN-1 ] of WideChar;
       lcOptions    :UINT;
       lcStatus     :UINT;
       lcLocks      :UINT;
       lcMsgBase    :UINT;
       lcDevice     :UINT;
       lcPktRate    :UINT;
       lcPktData    :WTPKT;
       lcPktMode    :WTPKT;
       lcMoveMask   :WTPKT;
       lcBtnDnMask  :DWORD;
       lcBtnUpMask  :DWORD;
       lcInOrgX     :LONG;
       lcInOrgY     :LONG;
       lcInOrgZ     :LONG;
       lcInExtX     :LONG;
       lcInExtY     :LONG;
       lcInExtZ     :LONG;
       lcOutOrgX    :LONG;
       lcOutOrgY    :LONG;
       lcOutOrgZ    :LONG;
       lcOutExtX    :LONG;
       lcOutExtY    :LONG;
       lcOutExtZ    :LONG;
       lcSensX      :FIX32;
       lcSensY      :FIX32;
       lcSensZ      :FIX32;
       lcSysMode    :BOOL;
       lcSysOrgX    :Integer;
       lcSysOrgY    :Integer;
       lcSysExtX    :Integer;
       lcSysExtY    :Integer;
       lcSysSensX   :FIX32;
       lcSysSensY   :FIX32;
     end;
    PLOGCONTEXTW = ^LOGCONTEXTW;
   NPLOGCONTEXTW = PLOGCONTEXTW;
   LPLOGCONTEXTW = PLOGCONTEXTW;
{$IFDEF UNICODE }
type   LOGCONTEXT =   LOGCONTEXTW;
type  PLOGCONTEXT =  PLOGCONTEXTW;
type NPLOGCONTEXT = NPLOGCONTEXTW;
type LPLOGCONTEXT = LPLOGCONTEXTW;
{$ELSE}
type   LOGCONTEXT =   LOGCONTEXTA;
type  PLOGCONTEXT =  PLOGCONTEXTA;
type NPLOGCONTEXT = NPLOGCONTEXTA;
type LPLOGCONTEXT = LPLOGCONTEXTA;
{$ENDIF} (* UNICODE *)
{$ELSE} (* WIN32 *)
type LOGCONTEXT = record
       lcName       :array [ 0..LCNAMELEN-1 ] of Char;
       lcOptions    :UINT;
       lcStatus     :UINT;
       lcLocks      :UINT;
       lcMsgBase    :UINT;
       lcDevice     :UINT;
       lcPktRate    :UINT;
       lcPktData    :WTPKT;
       lcPktMode    :WTPKT;
       lcMoveMask   :WTPKT;
       lcBtnDnMask  :DWORD;
       lcBtnUpMask  :DWORD;
       lcInOrgX     :LONG;
       lcInOrgY     :LONG;
       lcInOrgZ     :LONG;
       lcInExtX     :LONG;
       lcInExtY     :LONG;
       lcInExtZ     :LONG;
       lcOutOrgX    :LONG;
       lcOutOrgY    :LONG;
       lcOutOrgZ    :LONG;
       lcOutExtX    :LONG;
       lcOutExtY    :LONG;
       lcOutExtZ    :LONG;
       lcSensX      :FIX32;
       lcSensY      :FIX32;
       lcSensZ      :FIX32;
       lcSysMode    :BOOL;
       lcSysOrgX    :Integer;
       lcSysOrgY    :Integer;
       lcSysExtX    :Integer;
       lcSysExtY    :Integer;
       lcSysSensX   :FIX32;
       lcSysSensY   :FIX32;
     end;
    PLOGCONTEXT = ^LOGCONTEXT;
   NPLOGCONTEXT = PLOGCONTEXT;
   LPLOGCONTEXT = PLOGCONTEXT;
{$ENDIF} (* WIN32 *)

    (* context option values *)
    const CXO_SYSTEM         = $0001;
    const CXO_PEN            = $0002;
    const CXO_MESSAGES       = $0004;
    const CXO_MARGIN         = $8000;
    const CXO_MGNINSIDE      = $4000;
    const CXO_CSRMESSAGES    = $0008;  (* 1.1 *)

    (* context status values *)
    const CXS_DISABLED       = $0001;
    const CXS_OBSCURED       = $0002;
    const CXS_ONTOP          = $0004;

    (* context lock values *)
    const CXL_INSIZE         = $0001;
    const CXL_INASPECT       = $0002;
    const CXL_SENSITIVITY    = $0004;
    const CXL_MARGIN         = $0008;
    const CXL_SYSOUT         = $0010;

(* -------------------------------------------------------------------------- *)
(* EVENT DATA DEFS *)

(* For packet structure definition, see pktdef.h *)

(* packet status values *)
const TPS_PROXIMITY    = $0001;
const TPS_QUEUE_ERR    = $0002;
const TPS_MARGIN       = $0004;
const TPS_GRAB         = $0008;
const TPS_INVERT       = $0010;  (* 1.1 *)

type ORIENTATION = record
       orAzimuth   :Integer;
       orAltitude  :Integer;
       orTwist     :Integer;
     end;
    PORIENTATION = ^ORIENTATION;
   NPORIENTATION = PORIENTATION;
   LPORIENTATION = PORIENTATION;

type ROTATION = record (* 1.1 *)
       roPitch  :Integer;
       roRoll   :Integer;
       roYaw    :Integer;
     end;
    PROTATION = ^ROTATION;
   NPROTATION = PROTATION;
   LPROTATION = PROTATION;
// grandfather in obsolete member names.
//#define rotPitch    roPitch
//#define rotRoll     roRoll
//#define rotYaw      roYaw


(* relative buttons *)
const TBN_NONE    = 0;
const TBN_UP      = 1;
const TBN_DOWN    = 2;

(* -------------------------------------------------------------------------- *)
(* DEVICE CONFIG CONSTANTS *)

{$IFNDEF NOWTDEVCFG }

const WTDC_NONE       = 0;
const WTDC_CANCEL     = 1;
const WTDC_OK         = 2;
const WTDC_RESTART    = 3;

{$ENDIF}

(* -------------------------------------------------------------------------- *)
(* HOOK CONSTANTS *)

{$IFNDEF NOWTHOOKS }

const WTH_PLAYBACK       = 1;
const WTH_RECORD         = 2;

const WTHC_GETLPLPFN     = -3;
const WTHC_LPLPFNNEXT    = -2;
const WTHC_LPFNNEXT      = -1;
const WTHC_ACTION        = 0;
const WTHC_GETNEXT       = 1;
const WTHC_SKIP          = 2;

{$ENDIF}

(* -------------------------------------------------------------------------- *)
(* PREFERENCE FUNCTION CONSTANTS *)

{$IFNDEF NOWTPREF }

const WTP_LPDEFAULT    = LPVOID(-1);
const WTP_DWDEFAULT    = DWORD(-1);

{$ENDIF}

(* -------------------------------------------------------------------------- *)
(* EXTENSION TAGS AND CONSTANTS *)

{$IFNDEF NOWTEXTENSIONS }

(* constants for use with pktdef.h *)
const PKEXT_ABSOLUTE    = 1;
const PKEXT_RELATIVE    = 2;

(* Extension tags. *)
const WTX_OBT           = 0;  (* Out of bounds tracking *)
const WTX_FKEYS         = 1;  (* Function keys *)
const WTX_TILT          = 2;  (* Raw Cartesian tilt; 1.1 *)
const WTX_CSRMASK       = 3;  (* select input by cursor type; 1.1 *)
const WTX_XBTNMASK      = 4;  (* Extended button mask; 1.1 *)
const WTX_EXPKEYS       = 5;  (* ExpressKeys; 1.3 - DEPRECATED: see WTX_EXPKEYS2 *)
const WTX_TOUCHSTRIP    = 6;  (* TouchStrips; 1.4 *)
const WTX_TOUCHRING     = 7;  (* TouchRings; 1.4 *)
const WTX_EXPKEYS2      = 8;  (* ExpressKeys; 1.4 *)

type XBTNMASK = record
       xBtnDnMask  :array [ 0..32-1 ] of BYTE;
       xBtnUpMask  :array [ 0..32-1 ] of BYTE;
     end;

type TILT = record  (* 1.1 *)
       tiltX  :Integer;
       tiltY  :Integer;
     end;

type EXTENSIONBASE = record  (* 1.4 *)
       nContext       :HCTX;
       nStatus        :UINT;
       nTime          :DWORD;
       nSerialNumber  :UINT;
     end;

type EXPKEYSDATA = record  (* 1.4 *)
       nTablet    :BYTE;
       nControl   :BYTE;
       nLocation  :BYTE;
       nReserved  :BYTE;
       nState     :DWORD;
     end;

type SLIDERDATA = record  (* 1.4 *)
       nTablet    :BYTE;
       nControl   :BYTE;
       nMode      :BYTE;
       nReserved  :BYTE;
       nPosition  :DWORD;
     end;

type EXTPROPERTY = record  (* 1.4 *)
       version        :BYTE;                        // Structure version, 0 for now
       tabletIndex    :BYTE;                        // 0-based index for tablet
       controlIndex   :BYTE;                        // 0-based index for control
       functionIndex  :BYTE;                        // 0-based index for control's sub-function
       propertyID     :WORD;                        // property ID
       reserved       :WORD;                        // DWORD-alignment filler
       dataSize       :DWORD;                       // number of bytes in data[] buffer
       data           :array [ 0..1-1 ] of BYTE;    // raw data
     end;

const TABLET_PROPERTY_CONTROLCOUNT     = 0;     // UINT32: number of physical controls on tablet
const TABLET_PROPERTY_FUNCCOUNT        = 1;     // UINT32: number of functions of control
const TABLET_PROPERTY_AVAILABLE        = 2;     // BOOL: control/mode is available for override
const TABLET_PROPERTY_MIN              = 3;     // UINT32: minimum value
const TABLET_PROPERTY_MAX              = 4;     // UINT32: maximum value
const TABLET_PROPERTY_OVERRIDE         = 5;     // BOOL: control is overridden
const TABLET_PROPERTY_OVERRIDE_NAME    = 6;     // UTF-8: Displayable name when control is overridden
const TABLET_PROPERTY_LOCATION         = 11;    // UINT32: Physical location of control (see TABLET_LOC_*)

const TABLET_LOC_LEFT          = 0;
const TABLET_LOC_RIGHT         = 1;
const TABLET_LOC_TOP           = 2;
const TABLET_LOC_BOTTOM        = 3;
const TABLET_LOC_TRANSDUCER    = 4;

{$ENDIF}

(* -------------------------------------------------------------------------- *)
(* -------------------------------------------------------------------------- *)
(* Functions *)

    {$IFNDEF API }
        {$IFNDEF WINAPI }
//            #define API    FAR PASCAL
        {$ELSE}
//            #define API    WINAPI
        {$ENDIF}
    {$ENDIF}

{$IFNDEF NOWTCALLBACKS }

    {$IFNDEF CALLBACK }
//    #define CALLBACK    FAR PASCAL
    {$ENDIF}

    {$IFNDEF NOWTMANAGERFXNS }
    (* callback function types *)
    type WTENUMPROC   = function ( _1:HCTX; _2:LPARAM ) :BOOL;  (* changed CALLBACK->WINAPI, 1.1 *)
    type WTCONFIGPROC = function ( _1:HCTX; _2:HWND ) :BOOL;
    type WTHOOKPROC   = function ( _1:Integer; _2:WPARAM; _3:LPARAM ) :LRESULT;
    type LPWTHOOKPROC = ^WTHOOKPROC;
    {$ENDIF}

{$ENDIF}


{$IFNDEF NOWTFUNCTIONS }

    {$IFNDEF NOWTBASICFXNS }
    (* BASIC FUNCTIONS *)
{$IFDEF WIN32 }
    UINT API WTInfoA(UINT, UINT, LPVOID);
    const ORD_WTInfoA    = 20;
    UINT API WTInfoW(UINT, UINT, LPVOID);
    const ORD_WTInfoW    = 1020;
    {$IFDEF UNICODE }
    #define WTInfo         WTInfoW
    const ORD_WTInfo     = ORD_WTInfoW;
    {$ELSE}
    #define WTInfo         WTInfoA
    const ORD_WTInfo     = ORD_WTInfoA;
    {$ENDIF} (* !UNICODE *)
{$ELSE}
    UINT API WTInfo(UINT, UINT, LPVOID);
    const ORD_WTInfo     = 20;
{$ENDIF}
{$IFDEF WIN32 }
    HCTX API WTOpenA(HWND, LPLOGCONTEXTA, BOOL);
    const ORD_WTOpenA    = 21;
    HCTX API WTOpenW(HWND, LPLOGCONTEXTW, BOOL);
    const ORD_WTOpenW    = 1021;
    {$IFDEF UNICODE }
    #define WTOpen         WTOpenW
    const ORD_WTOpen     = ORD_WTOpenW;
    {$ELSE}
    #define WTOpen         WTOpenA
    const ORD_WTOpen     = ORD_WTOpenA;
    {$ENDIF} (* !UNICODE *)
{$ELSE}
    HCTX API WTOpen(HWND, LPLOGCONTEXT, BOOL);
    const ORD_WTOpen     = 21;
{$ENDIF}
    BOOL API WTClose(HCTX);
    const ORD_WTClose         = 22;
    int API WTPacketsGet(HCTX, int, LPVOID);
    const ORD_WTPacketsGet    = 23;
    BOOL API WTPacket(HCTX, UINT, LPVOID);
    const ORD_WTPacket        = 24;
    {$ENDIF}

    {$IFNDEF NOWTVISIBILITYFXNS }
    (* VISIBILITY FUNCTIONS *)
    BOOL API WTEnable(HCTX, BOOL);
    const ORD_WTEnable     = 40;
    BOOL API WTOverlap(HCTX, BOOL);
    const ORD_WTOverlap    = 41;
    {$ENDIF}

    {$IFNDEF NOWTCTXEDITFXNS }
    (* CONTEXT EDITING FUNCTIONS *)
    BOOL API WTConfig(HCTX, HWND);
    const ORD_WTConfig    = 60;
{$IFDEF WIN32 }
    BOOL API WTGetA(HCTX, LPLOGCONTEXTA);
    const ORD_WTGetA    = 61;
    BOOL API WTGetW(HCTX, LPLOGCONTEXTW);
    const ORD_WTGetW    = 1061;
    {$IFDEF UNICODE }
    #define WTGet         WTGetW
    const ORD_WTGet     = ORD_WTGetW;
    {$ELSE}
    #define WTGet         WTGetA
    const ORD_WTGet     = ORD_WTGetA;
    {$ENDIF} (* !UNICODE *)
{$ELSE}
    BOOL API WTGet(HCTX, LPLOGCONTEXT);
    const ORD_WTGet     = 61;
{$ENDIF}
{$IFDEF WIN32 }
    BOOL API WTSetA(HCTX, LPLOGCONTEXTA);
    const ORD_WTSetA    = 62;
    BOOL API WTSetW(HCTX, LPLOGCONTEXTW);
    const ORD_WTSetW    = 1062;
    {$IFDEF UNICODE }
    #define WTSet         WTSetW
    const ORD_WTSet     = ORD_WTSetW;
    {$ELSE}
    #define WTSet         WTSetA
    const ORD_WTSet     = ORD_WTSetA;
    {$ENDIF} (* !UNICODE *)
{$ELSE}
    BOOL API WTSet(HCTX, LPLOGCONTEXT);
    const ORD_WTSet     = 62;
{$ENDIF}
    BOOL API WTExtGet(HCTX, UINT, LPVOID);
    const ORD_WTExtGet     = 63;
    BOOL API WTExtSet(HCTX, UINT, LPVOID);
    const ORD_WTExtSet     = 64;
    BOOL API WTSave(HCTX, LPVOID);
    const ORD_WTSave       = 65;
    HCTX API WTRestore(HWND, LPVOID, BOOL);
    const ORD_WTRestore    = 66;
    {$ENDIF}

    {$IFNDEF NOWTQUEUEFXNS }
    (* ADVANCED PACKET AND QUEUE FUNCTIONS *)
    int API WTPacketsPeek(HCTX, int, LPVOID);
    const ORD_WTPacketsPeek    = 80;
    int API WTDataGet(HCTX, UINT, UINT, int, LPVOID, LPINT);
    const ORD_WTDataGet        = 81;
    int API WTDataPeek(HCTX, UINT, UINT, int, LPVOID, LPINT);
    const ORD_WTDataPeek       = 82;
{$IFNDEF WIN32 }
(* OBSOLETE IN WIN32! *)
    DWORD API WTQueuePackets(HCTX);
    const ORD_WTQueuePackets    = 83;
{$ENDIF}
    int API WTQueueSizeGet(HCTX);
    const ORD_WTQueueSizeGet    = 84;
    BOOL API WTQueueSizeSet(HCTX, int);
    const ORD_WTQueueSizeSet    = 85;
    {$ENDIF}

    {$IFNDEF NOWTHMGRFXNS }
    (* MANAGER HANDLE FUNCTIONS *)
    HMGR API WTMgrOpen(HWND, UINT);
    const ORD_WTMgrOpen     = 100;
    BOOL API WTMgrClose(HMGR);
    const ORD_WTMgrClose    = 101;
    {$ENDIF}

    {$IFNDEF NOWTMGRCTXFXNS }
    (* MANAGER CONTEXT FUNCTIONS *)
    BOOL API WTMgrContextEnum(HMGR, WTENUMPROC, LPARAM);
    const ORD_WTMgrContextEnum     = 120;
    HWND API WTMgrContextOwner(HMGR, HCTX);
    const ORD_WTMgrContextOwner    = 121;
    HCTX API WTMgrDefContext(HMGR, BOOL);
    const ORD_WTMgrDefContext      = 122;
    HCTX API WTMgrDefContextEx(HMGR, UINT, BOOL); (* 1.1 *)
    const ORD_WTMgrDefContextEx    = 206;
    {$ENDIF}
    
    {$IFNDEF NOWTMGRCONFIGFXNS }
    (* MANAGER CONFIG BOX  FUNCTIONS *)
    UINT API WTMgrDeviceConfig(HMGR, UINT, HWND);
    const ORD_WTMgrDeviceConfig    = 140;
{$IFNDEF WIN32 }
(* OBSOLETE IN WIN32! *)
    BOOL API WTMgrConfigReplace(HMGR, BOOL, WTCONFIGPROC);
    const ORD_WTMgrConfigReplace    = 141;
{$ENDIF}
    {$ENDIF}

    {$IFNDEF NOWTMGRHOOKFXNS }
    (* MANAGER PACKET HOOK FUNCTIONS *)
{$IFNDEF WIN32 }
(* OBSOLETE IN WIN32! *)
    WTHOOKPROC API WTMgrPacketHook(HMGR, BOOL, int, WTHOOKPROC);
    const ORD_WTMgrPacketHook           = 160;
    LRESULT API WTMgrPacketHookDefProc(int, WPARAM, LPARAM, LPWTHOOKPROC);
    const ORD_WTMgrPacketHookDefProc    = 161;
{$ENDIF}
    {$ENDIF}

    {$IFNDEF NOWTMGRPREFFXNS }
    (* MANAGER PREFERENCE DATA FUNCTIONS *)
    BOOL API WTMgrExt(HMGR, UINT, LPVOID);
    const ORD_WTMgrExt                    = 180;
    BOOL API WTMgrCsrEnable(HMGR, UINT, BOOL);
    const ORD_WTMgrCsrEnable              = 181;
    BOOL API WTMgrCsrButtonMap(HMGR, UINT, LPBYTE, LPBYTE);
    const ORD_WTMgrCsrButtonMap           = 182;
    BOOL API WTMgrCsrPressureBtnMarks(HMGR, UINT, DWORD, DWORD);
    const ORD_WTMgrCsrPressureBtnMarks    = 183;
    BOOL API WTMgrCsrPressureResponse(HMGR, UINT, UINT FAR *, UINT FAR *);
    const ORD_WTMgrCsrPressureResponse    = 184;
    BOOL API WTMgrCsrExt(HMGR, UINT, UINT, LPVOID);
    const ORD_WTMgrCsrExt                 = 185;
    {$ENDIF}

(* Win32 replacements for non-portable functions. *)
    {$IFNDEF NOWTQUEUEFXNS }
    (* ADVANCED PACKET AND QUEUE FUNCTIONS *)
    BOOL API WTQueuePacketsEx(HCTX, UINT FAR *, UINT FAR *);
    const ORD_WTQueuePacketsEx    = 200;
    {$ENDIF}

    {$IFNDEF NOWTMGRCONFIGFXNS }
    (* MANAGER CONFIG BOX  FUNCTIONS *)
{$IFDEF WIN32 }
    BOOL API WTMgrConfigReplaceExA(HMGR, BOOL, LPSTR, LPSTR);
    const ORD_WTMgrConfigReplaceExA    = 202;
    BOOL API WTMgrConfigReplaceExW(HMGR, BOOL, LPWSTR, LPSTR);
    const ORD_WTMgrConfigReplaceExW    = 1202;
    {$IFDEF UNICODE }
    #define WTMgrConfigReplaceEx         WTMgrConfigReplaceExW
    const ORD_WTMgrConfigReplaceEx     = ORD_WTMgrConfigReplaceExW;
    {$ELSE}
    #define WTMgrConfigReplaceEx         WTMgrConfigReplaceExA
    const ORD_WTMgrConfigReplaceEx     = ORD_WTMgrConfigReplaceExA;
    {$ENDIF} (* !UNICODE *)
{$ELSE}
    BOOL API WTMgrConfigReplaceEx(HMGR, BOOL, LPSTR, LPSTR);
    const ORD_WTMgrConfigReplaceEx     = 202;
{$ENDIF}
    {$ENDIF}

    {$IFNDEF NOWTMGRHOOKFXNS }
    (* MANAGER PACKET HOOK FUNCTIONS *)
{$IFDEF WIN32 }
    HWTHOOK API WTMgrPacketHookExA(HMGR, int, LPSTR, LPSTR);
    const ORD_WTMgrPacketHookExA     = 203;
    HWTHOOK API WTMgrPacketHookExW(HMGR, int, LPWSTR, LPSTR);
    const ORD_WTMgrPacketHookExW     = 1203;
    {$IFDEF UNICODE }
    #define WTMgrPacketHookEx          WTMgrPacketHookExW
    const ORD_WTMgrPacketHookEx      = ORD_WTMgrPacketHookExW;
    {$ELSE}
    #define WTMgrPacketHookEx          WTMgrPacketHookExA
    const ORD_WTMgrPacketHookEx      = ORD_WTMgrPacketHookExA;
    {$ENDIF} (* !UNICODE *)
{$ELSE}
    HWTHOOK API WTMgrPacketHookEx(HMGR, int, LPSTR, LPSTR);
    const ORD_WTMgrPacketHookEx      = 203;
{$ENDIF}
    BOOL API WTMgrPacketUnhook(HWTHOOK);
    const ORD_WTMgrPacketUnhook      = 204;
    LRESULT API WTMgrPacketHookNext(HWTHOOK, int, WPARAM, LPARAM);
    const ORD_WTMgrPacketHookNext    = 205;
    {$ENDIF}

    {$IFNDEF NOWTMGRPREFFXNS }
    (* MANAGER PREFERENCE DATA FUNCTIONS *)
    BOOL API WTMgrCsrPressureBtnMarksEx(HMGR, UINT, UINT FAR *, UINT FAR *);
    const ORD_WTMgrCsrPressureBtnMarksEx    = 201;
    {$ENDIF}

{$ENDIF}

implementation //############################################################### ■

end. //######################################################################### ■

{$IFDEF __cplusplus }
}
{$ENDIF} (* __cplusplus *)

{$ENDIF} (* #define _INC_WINTAB *)
