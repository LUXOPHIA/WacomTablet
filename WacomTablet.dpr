program WacomTablet;

uses
  System.StartUpCopy,
  FMX.Forms,
  Main in 'Main.pas' {Form1},
  WINTAB in '_LIBRARY\WINTAB.pas',
  Core in 'Core.pas',
  LUX.Win.Messaging in '_LIBRARY\LUX.Win.Messaging.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
