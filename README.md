# WacomTablet
ペンタブレット（特に Wacom 製）から、ペン先の座標や筆圧や傾きなどのストローク情報を取得する方法。  
※ Windows のみ対応。

![](https://github.com/LUXOPHIA/WacomTablet/raw/master/--------/_SCREENSHOT/WacomTablet.png)

----
## ■ 準備
「Wacom SDK」と検索すると、Wacom の提唱する新規格である「[WILL: Wacom Ink Layer Language](https://developer.wacom.com/ja-jp/products/will-sdk-for-ink)」関係の情報ばかりがヒットしてしまうが、Windows でのサンプルは C# 用のみ。ネイティブ言語で扱わせる気はなくなったらしい。

* [Wacom-Developer-Welcome-Page](https://github.com/Wacom-Developer/Wacom-Developer-Welcome-Page) @ GitHub

しかし従来から、Wacom 製以外のタブレットも包括した「Wintab」という統一規格が存在し、今も現役である。そして以下のサイトが、ほぼ唯一の情報源。

* [Wintab | WDN ワコムの技術情報提供サイト](http://wdnet.jp/library/windows/wintab)

特に複雑なライブラリのインクルードが必要なわけではなく、ペンタブレットのドライバをインストールすると、システムに `Wintab32.dll` という DLL がインストールされるので、その中の C-API を叩けばいい。ただ、ペンタブレットのメーカー毎にドライバが異なるので、複数のタブレットを共存させるには、以下のように複数の DLL を使い分ける必要がある。

* [一台のWindows PCで複数のペンタブレット・液晶ペンタブレットを共存させ、『Wintab API』でソフトごとに使い分ける方法](https://hel.hatenablog.com/entry/2018/08/05/103422)

`Wintab32.dll` 内の関数を列挙したヘッダファイル「[Wintab.h](https://github.com/LUXOPHIA/WacomTablet/blob/master/--------/WINTAB.H)」は、[以下のページからダウンロードできる](http://wdnet.jp/library/windows/notesfortabletawarepcdevelopers)と記載されているが、実際はリンクが切れており、[Wacom for Developers](https://developer.wacom.com/ja-jp) のページへ飛ばされるだけ。

* [Windows | WDN ワコムの技術情報提供サイト](http://wdnet.jp/library/windows)

実際は以下の、Wacom のレガシーなドキュメント置き場からしかダウンロードできない。

* [Developer Documentation](https://developer-docs-legacy.wacom.com/)
    * [Wintab / Feel-Touch API](https://developer-docs-legacy.wacom.com/pages/viewpage.action?pageId=10422351)
        * [Windows](https://developer-docs-legacy.wacom.com/display/DevDocs/Windows)
            * [Wintab Sample Code Downloads](https://developer-docs-legacy.wacom.com/display/DevDocs/Wintab+Sample+Code+Downloads)

そして、`Wintab.h` を Delphi 用へ翻訳したのが以下のユニットである。

* [WINTAB.pas](https://github.com/LUXOPHIA/LUX.PenTablet/blob/master/WINTAB.pas) @ [LUX.PenTablet](https://github.com/LUXOPHIA/LUX.PenTablet)

----
## ■ 実装
結論から言うと、以下の３つのユニットをインクルードすれば完結する。しかも、（未だ汎用性のない実装であるが）`LUX.WinTab.TabletFrame` ユニットの `TTabletFrame`フレームをフォームに貼るだけで、初期化から描画までが完結する。

* [LUX.PenTablet](https://github.com/LUXOPHIA/LUX.PenTablet)
    * [WINTAB.pas](https://github.com/LUXOPHIA/LUX.PenTablet/blob/master/WINTAB.pas)
    * [LUX.WinTab.pas](https://github.com/LUXOPHIA/LUX.PenTablet/blob/master/LUX.WinTab.pas)
    * [LUX.WinTab.TabletFrame.pas](https://github.com/LUXOPHIA/LUX.PenTablet/blob/master/LUX.WinTab.TabletFrame.pas)
        * [LUX.WinTab.TabletFrame.fmx](https://github.com/LUXOPHIA/LUX.PenTablet/blob/master/LUX.WinTab.TabletFrame.fmx)

なお標準では“黒丸”になっているが、必要であればペン先に用いる画像を読み込むこともできる。
```Delphi
TabletFrame1.Brush.LoadFromFile( '～.png' );
```

----
## ■ 解説

### ▼ TTabletPacket レコード
刻々と変化するペン先の情報「パケット」には、２Ｄ座標はもちろん、筆圧や傾きなど複数の情報が含まれる。
各タブレットメーカーの性能によって取得可能な情報は異なるが、「Wacom Intous Pro」に関しては**９項目**が有効である。
それらをまとめて取得するためにレコードを利用するが、その定義は開発者に委ねられている。
というのも、すべての項目の取得は強制されておらず、必要に応じてフィールドを取捨選択できるようになっているからである（フィールドの順番は厳守）。
```Delphi
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
```
ただし、独自に定義した `TTabletPacket` レコードに対応させて、初期化に用いる定数 `PACKETDATA` も定義しなくてはならない。
```Delphi
const PACKETDATA = PK_STATUS
                or PK_CURSOR
                or PK_BUTTONS
                or PK_X
                or PK_Y
                or PK_Z
                or PK_NORMAL_PRESSURE
                or PK_TANGENT_PRESSURE
                or PK_ORIENTATION;
```

### ▼ TPenTablet クラス
タブレットの初期化やデータの取得など、C-API を直接叩くルーチンをまとめたラッパークラスであり、クラスを生成するだけでペンタブレットの初期化が完了する。
内部的には、`WTOpen` / `WTClose` 関数によって、ペンタブレットの利用を開始し終了している。
`WTOpen` 関数での初期化に必要なのは、パケット仕様や描画範囲などを指定したコンテキストと、ウィンドウハンドルの２つである。
```Delphi
constructor TPenTablet.Create;
begin
     inherited;
     GetInfos;  //タブレット情報の取得
     GetDefContext;  //コンテキストの設定
     _Form := TCommonCustomForm.CreateNew( nil );  //内部的な不可視ウィンドウを生成
     _Handle := WTOpen( FormToHWND( _Form ), @_Context, True );  //ペンタブレットのハンドルを取得
end;

destructor TPenTablet.Destroy;
begin
     WTClose( _Handle );  //ペンタブレットのハンドルを廃棄
     _Form.Free;  //内部的な不可視ウィンドウを廃棄
     inherited;
end;
```
もっともウィンドウハンドルに関しては、アプリのメインフォームである必要はないので、内部的に不可視ウィンドウを生成して利用している。

#### ▽ 初期化
コンテキストを定義するレコードは、`WINTAB` ユニット内で定義されている。
```Delphi
type LOGCONTEXTA = record
       lcName       :array [ 0..LCNAMELEN-1 ] of AnsiChar;
       lcOptions    :UINT;
       lcStatus     :UINT;
       lcLocks      :UINT;
       lcMsgBase    :UINT;
       ～
     end;
```

`WTInfo` 関数によってデフォルトのコンテキストを取得した後、パケットの仕様や描画座標系の範囲を上書きする形で設定する。
```Delphi
procedure TPenTablet.GetDefContext;
begin
     WTInfo( WTI_DEFCONTEXT, 0, @_Context );    //デフォルトのデジタイジングコンキストを取得
   { WTInfo( WTI_DEFSYSCTX, 0, @_Context ); }    //デフォルトのシステムコンテキストを取得
     with _Context do
     begin
        { lcMine.lcOption := lcMine.lcOption or CXO_MESSAGES; }    //パケットをウィンドウメッセージで取得するオプション
          lcMsgBase   := WT_DEFBASE;
          lcPktData   := PACKETDATA;    //TTabletPacketの仕様を定義する定数
          lcPktMode   := PACKETMODE;
          lcMoveMask  := PACKETDATA;    //TTabletPacketの仕様を定義する定数
          lcBtnUpMask := lcBtnDnMask;

          lcInOrgX    := _PosMinX;    //デバイス座標系における描画範囲の最小Ｘ座標
          lcInOrgY    := _PosMinY;    //デバイス座標系における描画範囲の最小Ｙ座標
          lcInExtX    := _PosMaxX;    //デバイス座標系における描画範囲の最大Ｘ座標
          lcInExtY    := _PosMaxY;    //デバイス座標系における描画範囲の最大Ｙ座標
          lcOutOrgX   := _PosMinX;    //出力座標系における描画範囲の最小Ｘ座標
          lcOutOrgY   := _PosMinY;    //出力座標系における描画範囲の最小Ｙ座標
          lcOutExtX   := _PosMaxX;    //出力座標系における描画範囲の最大Ｘ座標
          lcOutExtY   := _PosMaxY;    //出力座標系における描画範囲の最大Ｙ座標
     end;
end;
```

設定する部分は大きく分けて４ヶ所。
* [2.3 Wintabの初期化](http://wdnet.jp/library/windows/wintab#2.3)

1. **コンテキストのタイプ**  
ペンと同期させてシステムカーソルを動かすか否かの違いにより、コンテキストのタイプを選ぶ。今回はデジタイジングコンキストを採用した。
    * `デジタイジングコンキスト`：システムカーソルを動かさない
    * `システムコンテキスト`：システムカーソルを動かす
1. **パケットの取得方法**  
パフォーマンスの観点から、今回はポーリング方式を採用した。
    * `メッセージ`：ペン先が動く度に飛ぶウィンドウメッセージを検知してパケットを一つずつ受信する。
    * `ポーリング`：ペン先の動きとは独立して、定期的に問い合わせ、その間に溜まったパケットを一括取得する。
1. **デバイス上の有効範囲**  
`lcInOrgX/Y`, `lcInExtX/Y` の４つの数値により、タブレット上の描画有効範囲を設定する。
全面を有効範囲として設定するのが一般的だが、あえて数値を狭めれば、意図的にタブレット上の一部の範囲を利用することもできる。
1. **出力座標範囲**  
`lcOutOrgX/Y`, `lcOutExtX/Y` の４つの数値により、パケットとして出力される座標範囲を設定することができる。
`lcInOrg*/Ext*` と同じ値にすれば、デバイス座標をそのまま出力することになるが、あえて数値を狭めれば、必要な数値範囲にマッピングすることできる。

なお、ペンタブレットの解像度や座標範囲は、`WTInfo` 関数によって初期化以前に取得することが可能である。
* [5.3 データの最大値、最小値の求めかた](http://wdnet.jp/library/windows/wintab#5.3)

```Delphi
procedure TPenTablet.GetInfos;
var
   A :AXIS;
   A3 :array [ 1..3 ] of AXIS;
begin
     WTInfo( WTI_DEVICES, DVC_X, @A );
     _PosMinX :=         A.axMin;    //デバイス座標系における描画範囲の最小Ｘ座標
     _PosMaxX :=         A.axMax;    //デバイス座標系における描画範囲の最大Ｘ座標
     _UniX    :=         A.axUnits;    //デバイス座標系におけるＸ方向の単位
     _ResX    := HIWORD( A.axResolution );    //デバイス座標系におけるＸ方向の解像度

     WTInfo( WTI_DEVICES, DVC_Y, @A );
     _PosMinY :=         A.axMin;    //デバイス座標系における描画範囲の最小Ｙ座標
     _PosMaxY :=         A.axMax;    //デバイス座標系における描画範囲の最大Ｙ座標
     _UniY    :=         A.axUnits;    //デバイス座標系におけるＹ方向の単位
     _ResY    := HIWORD( A.axResolution );    //デバイス座標系におけるＹ方向の解像度

     WTInfo( WTI_DEVICES, DVC_NPRESSURE, @A );
     _PreMin := A.axMin;    //筆圧の最小値
     _PreMax := A.axMax;    //筆圧の最大値

     WTInfo( WTI_DEVICES, DVC_TPRESSURE, @A );
     _WheMin := A.axMin;    //ホイール回転角の最小値（ペンでは利用不可）
     _WheMax := A.axMax;    //ホイール回転角の最小値（ペンでは利用不可）

     WTInfo( WTI_DEVICES, DVC_ORIENTATION, @A3 );
     _AziMin := A3[ 1 ].axMin;    //傾き方角の最小値
     _AziMax := A3[ 1 ].axMax;    //傾き方角の最大値
     _AltMin := A3[ 2 ].axMin;    //傾き角度の最小値
     _AltMax := A3[ 2 ].axMax;    //傾き角度の最大値
     _TwiMin := A3[ 3 ].axMin;    //ねじれの最小値（ペンでは利用不可）
     _TwiMax := A3[ 3 ].axMax;    //ねじれの最大値（ペンでは利用不可）
end;
```

#### ▽ パケットの取得
メッセージ方式の場合は、パケットを一つずつ取得する `WTPacket` 関数を用いるが、ポーリング方式の場合は、バッファに溜まったパケットをすべて取り出す `WTPacketsGet` 関数を用いるべきである。実際には、両方の実装に取り組んだ結果、メッセージ方式ではパフォーマンスが上がらず、ポーリング方式を採用せざるを得なかった。
* [3一般的な座標データ入力方法](http://wdnet.jp/library/windows/wintab#3)

```Delphi
function TPenTablet.GetPakets( var Packets_:array of TTabletPacket ) :Integer;
begin
     Result := WTPacketsGet( _Handle, Length( Packets_ ), @Packets_ );
end;
```


----
### ▼ TTabletFrame クラス
内部的に `TPenTablet` クラスを生成して利用し、定期的にパケットを読み出して描画を行なう疑似コンポーネントである。
```Delphi
     TTabletFrame = class( TFrame )
     ～
     protected
       _Tablet   :TPenTablet;             // タブレットAPIをラップしたクラス
       _Image    :TBitmap;                // ストロークを描画する内部的な画像
       _Packets  :TArray<TTabletPacket>;  // パケットの配列
       _PacketsN :Integer;                // 一度に取得するパケットの数
       _DrawArea :TRectF;                 // タブレット上の有効座標範囲
       _Timer    :TTimer;                 // パケットをポーリングするタイマー
       _Brush    :TBitmap;                // ペン先の画像
```

```Delphi
constructor TTabletFrame.Create( Owner_:TComponent );
begin
     inherited;
     _Tablet := TPenTablet.Create;    //TPenTablet クラスを生成
     SetLength( _Packets, _Tablet.QueueSize );    //パケットを読み込む配列を確保
     CalcDrawArea;    //TTabletFrameのサイズに合わせて、デバイス座標系における有効な描画範囲を計算
     _Image := TBitmap.Create;    //ストローク画像を生成
     with _Image do
     begin
          BitmapScale := GetDisplayScale;    //ディスプレイのスケールを取得
          SetSize( Round( GetDisplayScale * Width  ),
                   Round( GetDisplayScale * Height ) );    //ディスプレイのスケールに合わせてストローク画像の解像度を設定
     end;
     _Timer := TTimer.Create( Self );    //TTimer コンポーネントを生成
     _Timer.Interval{ms/f} := 1000{ms/s} div 100{f/s};    //TTimer の時間間隔を設定
     _Timer.OnTimer  := DrawFrame;    //タイマーイベントに DrawFrame メソッドを指定
     _Brush := TBitmap.Create( 64, 64 );    //ペン先画像を生成
     with _Brush.Canvas do
     begin
          BeginScene;    //デフォルトのペン先画像を描画
          Fill.Kind  := TBrushKind.Solid;
          Fill.Color := TAlphaColors.Black;
          FillEllipse( _Brush.BoundsF, 1 );
          EndScene;
     end;
end;
```

実際にストロークを描画しているのは、`TTimer` コンポーネントが定期的に呼び出している `DrawFrame` メソッドである。
`TTablet.GetPakets` メソッドを用いれば、パッファに溜まっているパケットを一気に読み出すことができ、戻り値によって何個読み込んだかが分る。
もちろんパッファにパケットが溜まっていなければ、戻り値はゼロとなる。
```Delphi
procedure TTabletFrame.DrawFrame(Sender: TObject);
const
     S = 20;
     R :TRectF = ( Left:-S; Top:-S; Right:+S; Bottom:+S );
var
   M :TMatrix;
   I :Integer;
   P :TTabletPacket;
   Pos :TPointF;
   Pre, Azi, Alt :Single;
begin
     _PacketsN := _Tablet.GetPakets( _Packets );
     if _PacketsN > 0 then
     begin
          with _Image.Canvas do
          begin
               BeginScene;
               M := Matrix;
               for I := 0 to _PacketsN-1 do
               begin
                    P := _Packets[ I ];
                    if P.Buttons = 1 then
                    begin
                         Pos := TabToScr( P.X, P.Y );    //デバイス座標をＵＩ座標へ変換
                         Pre := P.NormalPressure / _Tablet.PreMax;    //筆圧を 0～1 の範囲で取得
                         Azi := P.Orientation.orAzimuth  / _Tablet.AziMax * Pi*2;    //ペンの傾き方角を 0～2π の範囲で取得
                         Alt := P.Orientation.orAltitude / _Tablet.AltMax * Pi/2;    //ペンの傾き角度を 0～π/2 の範囲で取得
                         SetMatrix( TMatrix.CreateRotation( -Azi )
                                  * TMatrix.CreateScaling( Pre, Pre / Sin( Alt ) )
                                  * TMatrix.CreateRotation( +Azi )
                                  * TMatrix.CreateTranslation( Pos.X, Pos.Y )
                                  * M );    //筆圧や傾きによってペン先画像を変形
                         case P.Status of    //ペン先の種類
                         $00: begin    //ペン先で描いた場合
                                   DrawBitmap( _Brush, _Brush.BoundsF, R, 0.75 );    //ペン先画像を描画
                              end;
                         $10: begin    //消しゴムで描いた場合
                                   Fill.Kind  := TBrushKind.Solid;
                                   Fill.Color := TAlphaColors.White;
                                   FillEllipse( R, 0.5 );    //白い円を描画
                              end;
                         end;
                    end;
               end;
               EndScene;
          end;
     end;
     Repaint;
end;
```

----

[![Delphi Starter](https://github.com/delphiusers/FreeDelphi/raw/master/FreeDelphi_300px.png)](https://www.embarcadero.com/jp/products/delphi/starter)
