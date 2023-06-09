unit DotEnv4Delphi;

interface

uses System.Generics.Collections;

type
{$Region 'EnumEnvVars'}
  TEnvVar = (ALLUSERSPROFILE, APPDATA, CLIENTNAME, COMMONPROGRAMFILES, COMPUTERNAME, COMSPEC, HOMEDRIVE, HOMEPATH, LOGONSERVER,
             NUMBER_OF_PROCESSORS, OS, PATH, PATHEXT, PROCESSOR_ARCHITECTURE, PROCESSOR_IDENTIFIER, PROCESSOR_LEVEL,
             PROCESSOR_REVISION, PROGRAMFILES, SESSIONNAME, SYSTEMDRIVE, SYSTEMROOT, TEMP, TMP, USERDOMAIN, USERNAME, USERPROFILE,
             WINDIR, DB_USERNAME, DBUSERNAME, DBPORT, DB_PORT, PORT, HOSTNAME, DB_HOST, DB_USER, DBHOST, DBUSER, DBPASS, DB_PASS,
             PASSWORD, DBPASSWORD, BASE_URL, TOKEN, API_TOKEN, CONNECTIONSTRING, DEVELOPMENT);
{$EndRegion}

{$Region 'DotEnv4Delphi´s interface'}
  iDotEnv4Delphi = interface
    ['{3BF1532F-91B1-4C1F-A40A-CD81F8754451}']
    function Config(const OnlyFromEnvFile: Boolean = False): iDotEnv4Delphi; overload;
    function Config(const path: string = ''; OnlyFromEnvFile: Boolean = False): iDotEnv4Delphi; overload;
    function Env(const name: string): string; overload;
    function Env(const EnvVar: TEnvVar): string; overload;
    function Port: integer;
    function Token: string;
    function Password: string;
    function BaseUrl: string;
    function Hostname: string;
    function DBHost: string;
    function DBPassword: string;
    function ConnectionString: string;
    function Development: Boolean;
    function GetVersion: string;
  end;
{$EndRegion}

{$Region 'DotEnv4Delphi´s class declaration'}
  TDotEnv4Delphi = class(TInterfacedObject, iDotEnv4Delphi)
    private
     class var FInstance: iDotEnv4Delphi;
     fromDotEnvFile: Boolean;
     EnvPath:        string;
     EnvDict:        TDictionary<string, string>;
     procedure ReadEnvFile;
     function ReadValueFromEnvFile(const key: string): string;
    public
     constructor Create;
     Destructor Destroy; override;
     class function New: iDotEnv4Delphi;
     function Config(const OnlyFromEnvFile: Boolean = False): iDotEnv4Delphi; overload;
     function Config(const path: string = ''; OnlyFromEnvFile: Boolean = False): iDotEnv4Delphi; overload;
     function Env(const name: string): string; overload;
     function Env(const EnvVar: TEnvVar): string; overload;
     function Port: integer;
     function Token: string;
     function Password: string;
     function BaseUrl: string;
     function Hostname: string;
     function DBHost: string;
     function DBPassword: string;
     function ConnectionString: string;
     function Development: Boolean;
     function GetVersion: string;
  end;
{$EndRegion}

var
 DotEnv: iDotEnv4Delphi;

implementation

uses
  System.SysUtils,
  System.TypInfo,
  System.Classes;

{ TDotEnv4Delphi }

function TDotEnv4Delphi.ReadValueFromEnvFile(const key: string): string;
begin
  EnvDict.TryGetValue(key.ToUpper, Result);
end;

function TDotEnv4Delphi.Token: string;
begin
  Result := Env('TOKEN');
end;

function TDotEnv4Delphi.Env(const name: string): string;
begin
  if fromDotEnvFile then
   begin
     Result := ReadValueFromEnvFile(name);
     Exit;
   end;

  Result := GetEnvironmentVariable(name);

  if Result = EmptyStr then
   Result := ReadValueFromEnvFile(name);
end;

function TDotEnv4Delphi.Config(const OnlyFromEnvFile: Boolean): iDotEnv4Delphi;
begin
  Result := Self;
  fromDotEnvFile := OnlyFromEnvFile;
end;

function TDotEnv4Delphi.BaseUrl: string;
var
 Base: string;
begin
  Base := Env('BASE_URL');

  if Base = EmptyStr then
   Base := Env('BASEURL');

  Result := Base;
end;

function TDotEnv4Delphi.Config(const path: string; OnlyFromEnvFile: Boolean): iDotEnv4Delphi;
begin
  Result := Self;
  fromDotEnvFile := OnlyFromEnvFile;
  if (path <> EmptyStr) and (path <> EnvPath) then
   begin
    EnvPath := path;
    ReadEnvFile;
   end;
end;

function TDotEnv4Delphi.ConnectionString: string;
var
  ConnStr: string;
begin
  ConnStr := Env('ConnectionString');

  if ConnStr = EmptyStr then
   ConnStr := Env('CONNECTIONSTRING');

  if ConnStr = EmptyStr then
   ConnStr := Env('Connection_String');

  if ConnStr = EmptyStr then
   ConnStr := Env('CONNECTION_STRING');

  Result := ConnStr;
end;

constructor TDotEnv4Delphi.Create;
begin
  EnvDict := TDictionary<string, string>.create;
  EnvPath := ExtractFilePath(ParamStr(0)) + '.env';
  fromDotEnvFile := False;
  ReadEnvFile;
end;

function TDotEnv4Delphi.DBHost: string;
var
  db_host: string;
begin
  db_host := Env('DBHOST');

  if db_host = EmptyStr then
   db_host := Env('DB_HOST');

  if db_host = EmptyStr then
   db_host := Env('DB_Host');

  if db_host = EmptyStr then
   db_host := Env('DbHost');

  Result := db_host;
end;

function TDotEnv4Delphi.DBPassword: string;
var
  Pass: string;
begin
  Pass := Env('DBPassword');

  if Pass = EmptyStr then
   Pass := Env('DB_Password');

  if Pass = EmptyStr then
   Pass := Env('DB_PASSWORD');

  if Pass = EmptyStr then
   Pass := Env('DBPASSWORD');

  Result := Pass;
end;

destructor TDotEnv4Delphi.Destroy;
begin
  FreeAndNil(EnvDict);
  inherited;
end;

function TDotEnv4Delphi.Development: Boolean;
var
 Dev: string;
begin
  Result := false;

  Dev := Env('Development');

  if Dev = EmptyStr then
   Dev := Env('DEVELOPMENT');

  if (Dev <> EmptyStr) and (Dev.ToUpper = 'TRUE')  then
   Result := True;
end;

function TDotEnv4Delphi.Env(const EnvVar: TEnvVar): string;
begin
  Result := Env(GetEnumName(TypeInfo(TEnvVar), integer(EnvVar)));
end;

function TDotEnv4Delphi.GetVersion: string;
begin
  Result := '1.1.0';
end;

function TDotEnv4Delphi.Hostname: string;
var
  _host: string;
begin
  _host := Env('Hostname');

  if _host = EmptyStr then
   _host := Env('HOSTNAME');

  if _host = EmptyStr then
   _host := Env('Host_Name');

  if _host = EmptyStr then
   _host := Env('HOST_NAME');

  Result := _host;
end;

class function TDotEnv4Delphi.New: iDotEnv4Delphi;
begin
  if not Assigned(FInstance) then
   FInstance := Self.Create;

  Result := FInstance;
end;

function TDotEnv4Delphi.Password: string;
var
  _pass: string;
begin
  _pass := Env('Password');

  if _pass = EmptyStr then
   _pass := Env('PASSWORD');

  Result := _pass;
end;

function TDotEnv4Delphi.Port: integer;
var
  _port: string;
begin
  Result := 0;

  _port := Env('Port');

  if _Port = EmptyStr then
   _port := Env('PORT');

  if _Port <> EmptyStr then
   Result := StrToInt(_Port);
end;

procedure TDotEnv4Delphi.ReadEnvFile;

  function PegarValor(const valor: string): string;

   function RemoverComentario(const valor: string): string;
   var
    positionOfLastQuote: integer;
   begin
     if (valor.StartsWith('"')) or (valor.StartsWith(#39)) then
      begin
        positionOfLastQuote := Pos('"', Copy(valor, 2, length(valor) - 1));
        if positionOfLastQuote = 0 then
         positionOfLastQuote := Pos(#39, Copy(valor, 2, length(valor) - 1));

        if positionOfLastQuote > 0 then
         begin
           if Pos('#', valor) > positionOfLastQuote then
            Result := Copy(valor, 1, Pos('#', valor) - 1);
         end;
      end
     else
      begin
       if Pos('#', valor) > 0 then
        Result := Copy(valor, 1, Pos('#', valor) - 1)
       else
        Result := valor;
      end;
   end;

   function Interpolar(const valor: string): string;
   var
    PosIni, PosFim : integer;
    chave, ValorChave: string;
   begin
     Result := valor;
     if (not valor.StartsWith('"')) and (not valor.StartsWith(#39)) then
      begin
        while Pos('${', Result) > 0 do
         begin
           PosIni := Pos('${', Result);
           PosFim := Pos('}', Result);
           chave := Copy(Result, PosIni + 2, PosFim - (PosIni + 2));
           ValorChave := Env(chave);
           Result := StringReplace(Result, '${' + chave + '}', ValorChave, [rfReplaceAll]);
         end;
      end;
   end;

   function RemoverAspas(const valor: string): string;
   var
    positionOfLastQuote: integer;
   begin
     if (valor.StartsWith('"')) or (valor.StartsWith(#39)) then
      begin
        positionOfLastQuote := Pos('"', Copy(valor, 2, length(valor) - 1));
        if positionOfLastQuote = 0 then
         positionOfLastQuote := Pos(#39, Copy(valor, 2, length(valor) - 1));

        if positionOfLastQuote > 0 then
         begin
          Result := StringReplace(valor, '"', '', [rfReplaceAll]);
          Result := StringReplace(valor, #39, '', [rfReplaceAll]);
         end;
      end
     else
      Result := valor;
   end;
  begin
    Result := Trim(RemoverAspas(Interpolar(RemoverComentario(valor))));
  end;

  procedure PopulateDictionary(const Dict: TDictionary<string, string>);
  var
    fFile    : tstringlist;
    EnvFile  : string;
    position : Integer;
  begin
    fFile := TStringList.Create;
    try
      fFile.LoadFromFile(EnvPath);
      for position := 0 to fFile.Count - 1 do
       begin
         if not (fFile.Names[position].ToUpper = EmptyStr) then
          begin
           Dict.Add(fFile.Names[position].ToUpper, PegarValor(fFile.Values[fFile.Names[position]]));
          end;
       end;
    finally
      FreeAndNil(fFile)
    end;
  end;
begin
  if FileExists(EnvPath) then
   begin
     EnvDict.Clear;
     PopulateDictionary(EnvDict);
   end;
end;

initialization

begin
  DotEnv := tDotEnv4Delphi.New;
end;

end.
