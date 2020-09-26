program WacomTablet;

uses
  System.StartUpCopy,
  FMX.Forms,
  Main in 'Main.pas' {Form1},
  WINTAB in '_LIBRARY\WINTAB.pas',
  Core in 'Core.pas',
  LUX.FMX.Messaging.Win in '_LIBRARY\LUXOPHIA\LUX\FMX\LUX.FMX.Messaging.Win.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
