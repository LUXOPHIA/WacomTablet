program WacomTablet;

uses
  System.StartUpCopy,
  FMX.Forms,
  Main in 'Main.pas' {Form1},
  WINTAB in '_LIBRARY\WINTAB.pas',
  LUX.FMX.Messaging.Win in '_LIBRARY\LUXOPHIA\LUX\FMX\LUX.FMX.Messaging.Win.pas',
  LUX.WinTab in '_LIBRARY\LUXOPHIA\LUX.PenTablet\LUX.WinTab.pas',
  LUX.WinTab.TabletFrame in '_LIBRARY\LUXOPHIA\LUX.PenTablet\LUX.WinTab.TabletFrame.pas' {TabletFrame: TFrame},
  LUX.FMX.Pratform in '_LIBRARY\LUXOPHIA\LUX\FMX\LUX.FMX.Pratform.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
