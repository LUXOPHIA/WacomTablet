program WacomTabletVCL;

uses
  Vcl.Forms,
  Main in 'Main.pas' {Form1},
  PaintWindow in '_LIBRARY\LI-Qingrui\PaintWindow.pas',
  WinTab32 in '_LIBRARY\LI-Qingrui\WinTab32.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
