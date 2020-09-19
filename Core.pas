unit Core;

interface //#################################################################### ■

uses WINTAB;

//const //$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$【定数】

type //$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$【型】

     //$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$【レコード】

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

implementation //############################################################### ■

//$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$【レコード】

//$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$【クラス】

//$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$【ルーチン】

//############################################################################## □

initialization //$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$ 初期化

finalization //$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$ 最終化

end. //######################################################################### ■