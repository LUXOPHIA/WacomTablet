unit Main;

interface //#################################################################### ■

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes,
  Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs,
  PaintWindow;

type
  TForm1 = class(TForm)
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
  private
    { Private 宣言 }
  public
    { Public 宣言 }
    _PaintWindow :TPaintWindow;
    ///// イベント
    procedure OnPenDown( Sender_:TObject; X_,Y_,P_:Single; Key_:boolean );
    procedure OnPenMove( Sender_:TObject; X_,Y_,P_:Single; Key_:boolean );
    procedure OnPenUp( Sender_:TObject; X_,Y_,P_:Single; Key_:boolean );
  end;

var
  Form1: TForm1;

implementation //############################################################### ■

{$R *.dfm}

///////////////////////////////////////////////////////////////////////// public

procedure TForm1.OnPenDown( Sender_:TObject; X_,Y_,P_:Single; Key_:boolean );
begin

end;

procedure TForm1.OnPenMove( Sender_:TObject; X_,Y_,P_:Single; Key_:boolean );
var
   X, Y, P :Integer;
begin
     X := Round( X_ );
     Y := Round( Y_ );
     P := Round( 5 * P_ );

     with _PaintWindow do
     begin
          with Bitmap.Canvas do
          begin
               Brush.Color := clRed;

               Ellipse( X-P, Y-P, X+P ,Y+P );
          end;

          Repaint;
     end;
end;

procedure TForm1.OnPenUp( Sender_:TObject; X_,Y_,P_:Single; Key_:boolean );
begin

end;

////////////////////////////////////////////////////////////////////////////////

procedure TForm1.FormCreate(Sender: TObject);
begin
     _PaintWindow := TPaintWindow.Create( Form1 );

     with _PaintWindow do
     begin
          Parent := Form1;

          Align := TAlign.alClient;

          SetDimension( Width, Height );

          OnPenDown := Form1.OnPenDown;
          OnPenMove := Form1.OnPenMove;
          OnPenUp   := Form1.OnPenUp  ;
     end;
end;

procedure TForm1.FormDestroy(Sender: TObject);
begin
     /////
end;

end. //######################################################################### ■
