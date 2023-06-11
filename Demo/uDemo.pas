unit uDemo;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls;

type
  TForm2 = class(TForm)
    Button1: TButton;
    Memo1: TMemo;
    Button2: TButton;
    Button3: TButton;
    Button4: TButton;
    Label1: TLabel;
    Label2: TLabel;
    LAmbiente: TLabel;
    LVersion: TLabel;
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure Button4Click(Sender: TObject);
    procedure Button3Click(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form2: TForm2;

implementation

uses
  DotEnv4Delphi;

{$R *.dfm}

procedure TForm2.Button1Click(Sender: TObject);
var teste: string;
    teste2: char;
begin
   //Pegando váriáveis do Ambiente diretamente pelo nome(string)
   Memo1.Lines.Add(DotEnv.Env('appdata'));

   //Pegando variáveis do Ambiente através de Enumerado
   Memo1.Lines.Add(DotEnv.Env(tenvVar.ALLUSERSPROFILE));

   Memo1.Lines.Add(DotEnv.Env('MeuNome'));
end;

procedure TForm2.Button2Click(Sender: TObject);
begin
  //Pegando variáveis do arquivo .env
  Memo1.Lines.Add(DotEnv.Env('MeuNome')); //Usando o nome da variável
  Memo1.Lines.Add(DotEnv.Env(DB_USERNAME)); //Usando o enumerado default (procura dentro do arquivo se há uma variável com o nome
  Memo1.Lines.Add(DotEnv.Env('Nome3'));

  //Forma de Definir caminho do arquivo
  //DotEnv.Config('D:\Meus Projetos\DotEnv4Delphi\Demo\Win32\Debug\.env');
  //Memo1.Lines.Add(DotEnv.Env('MeuNome'));
end;

procedure TForm2.Button3Click(Sender: TObject);
begin
  //Outro exemplo de variável direta, neste caso útil para determinar que o ambiente é ou não de desenvolvimento (variável é DEVELOPMENT com valores "True" ou "False")
  if DotEnv.isDevelopment then
   LAmbiente.Caption := 'Desenvolvimento'
  else
   LAmbiente.Caption := 'Produção';
end;

procedure TForm2.Button4Click(Sender: TObject);
begin
  //Exemplo de nova forma de obter alguns tipos de variáveis diretamente, sem precisatr do comando env
  Memo1.Lines.Add('Porta: ' + DotEnv.Port.ToString);
end;

procedure TForm2.FormCreate(Sender: TObject);
begin
  LVersion.Caption := DotEnv.GetVersion;
end;

end.
