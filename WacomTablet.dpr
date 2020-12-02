program WacomTablet;

uses
  System.StartUpCopy,
  FMX.Forms,
  Main in 'Main.pas' {Form1},
  LUX.WinTab in '_LIBRARY\LUXOPHIA\LUX.PenTablet\LUX.WinTab.pas',
  WINTAB in '_LIBRARY\LUXOPHIA\LUX.PenTablet\WINTAB.pas',
  LUX.WinTab.TabletFrame in '_LIBRARY\LUXOPHIA\LUX.PenTablet\LUX.WinTab.TabletFrame.pas' {TabletFrame: TFrame};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
