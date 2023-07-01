unit unitMain;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, Buttons;

type

  { TFormMain }

  TFormMain = class(TForm)
    btnGetEnvVarsDefault: TButton;
    btnGetEnvVarsFromCreatedVars: TButton;
    lblAmbiente: TLabel;
    lbVAmbiente: TLabel;
    lblDotEnv: TLabel;
    lblversion: TLabel;
    LogVariaveis: TMemo;
    chOnlyFromFile: TToggleBox;
    procedure btnGetEnvVarsDefaultClick(Sender: TObject);
    procedure btnGetEnvVarsFromCreatedVarsClick(Sender: TObject);
    procedure chOnlyFromFileChange(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private

  public

  end;

var
  FormMain: TFormMain;

implementation

{$R *.lfm}

uses DotEnv4Delphi;

{ TFormMain }

procedure TFormMain.FormCreate(Sender: TObject);
begin
  //Pega versão da lib
  lblversion.Caption := DotEnv.GetVersion;

  //Verifica ambiente da aplicação (baseado na variável "Development"
  if DotEnv.isDevelopment then
   lbVAmbiente.Caption := 'DESENVOLVIMENTO'
  else
   lbVAmbiente.Caption := 'PRODUÇÃO';
end;

procedure TFormMain.btnGetEnvVarsDefaultClick(Sender: TObject);
begin
  //Pegando váriáveis do Ambiente diretamente pelo nome(string)
  LogVariaveis.Lines.Add('AppData: ' + DotEnv.Env('appdata'));
  LogVariaveis.Lines.Add('Nome do Computador: ' + DotEnv.Env('computername'));

  //Pegando variáveis do Ambiente através de Enumerado
  LogVariaveis.Lines.Add('AllUsersProfile: ' + DotEnv.Env(tenvVar.ALLUSERSPROFILE));
  LogVariaveis.Lines.Add('Sistema Operacional: ' + DotEnv.Env(tenvVar.OS));
  LogVariaveis.Lines.Add('Pasta do Windows: ' + DotEnv.Env(TEnvVar.WINDIR));

  //Pegando valores usando comandos diretos
  LogVariaveis.Lines.Add('Pasta dos Programas comum: ' + DotEnv.CommonProgramFiles);
end;

procedure TFormMain.btnGetEnvVarsFromCreatedVarsClick(Sender: TObject);
begin
  //Pegando váriáveis do Ambiente diretamente pelo nome(string)
  LogVariaveis.Lines.Add('MeuNome: ' + DotEnv.Env('MeuNome'));
  //[NOVO] Comando abaixo traz o valor da variável. Se não encontrar valor, ele fica com o valor padrão definida no parâmetro
  LogVariaveis.Lines.Add('Porta: ' + DotEnv.EnvOrDefault('Port', '5000'));

  //Pegando variáveis do Ambiente através de Enumerado
  LogVariaveis.Lines.Add('Base URL: ' + DotEnv.Env(tenvVar.BASE_URL));
  LogVariaveis.Lines.Add('API Token: ' + DotEnv.Env(tenvVar.API_TOKEN));

  //Pegando valores usando comandos diretos
  LogVariaveis.Lines.Add('Porta: ' + DotEnv.Port.ToString);
  LogVariaveis.Lines.Add('Porta(ou padrão): ' + DotEnv.PortOrDefault(3000).ToString);
  LogVariaveis.Lines.Add('String de Conexão: ' + DotEnv.ConnectionString);

  //Pega endereço do Executável (não depende de variável)
  LogVariaveis.Lines.Add('Caminho do App: ' + DotEnv.AppPath);
end;

procedure TFormMain.chOnlyFromFileChange(Sender: TObject);
begin
  //Usado para configurar a lib DotEnv4Delphi
  DotEnv.Config(chOnlyFromFile.Checked);

  {
   DotEnv.config(True) => Lê apenas valores do arquivo DotEnv(.Env)
   DotEnv.config(True) => Lê tanto do arquivo como do ambiente do sistema operacional (padrão)
   DotEnv.Config('c:\') => Seta localização do arquivo .env diferente do padrão, que é a pasta onde se encontra o arquivo executável
   DotEnv.Config('c:\', True) => As duas configurações juntas
  }
end;

end.

