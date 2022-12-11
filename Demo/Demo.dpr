program Demo;

uses
  Vcl.Forms,
  uDemo in 'uDemo.pas' {Form2},
  DotEnv4Delphi in '..\src\DotEnv4Delphi.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TForm2, Form2);
  Application.Run;
end.
