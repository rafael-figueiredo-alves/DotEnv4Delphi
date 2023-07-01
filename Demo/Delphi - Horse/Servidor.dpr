program Servidor;

{$APPTYPE CONSOLE}

{$R *.res}

uses
  System.SysUtils,
  Horse,
  DotEnv4Delphi;

begin
  THorse.Get('/ping',
    procedure(Req: THorseRequest; Res: THorseResponse)
    begin
      Res.Send('pong');
    end);

  {
   No JavaScript, usando o ExpressJS, teriamos uma linha como:
   app.listen(process.env.PORT || 3000);
   A Linha abaixo tem a mesma funcionalidade, sendo que
   DotEnv.PortorDefault(3000) é igual a process.env.PORT || 3000
  }
  THorse.Listen(DotEnv.PortOrDefault(3000));
end.
