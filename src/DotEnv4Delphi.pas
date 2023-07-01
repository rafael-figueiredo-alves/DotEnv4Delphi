unit DotEnv4Delphi;

{$IFDEF FPC}
  {$mode delphi}
{$ENDIF}

interface

uses
  {$IFDEF FPC}
    Generics.Collections;
  {$ELSE}
    System.Generics.Collections;
  {$ENDIF}

type
{$Region 'EnumEnvVars'}
  TEnvVar = (ALLUSERSPROFILE, APPDATA, CLIENTNAME, COMMONPROGRAMFILES, COMPUTERNAME, COMSPEC, HOMEDRIVE, HOMEPATH, LOGONSERVER,
             NUMBER_OF_PROCESSORS, OS, PATH, PATHEXT, PROCESSOR_ARCHITECTURE, PROCESSOR_IDENTIFIER, PROCESSOR_LEVEL,
             PROCESSOR_REVISION, PROGRAMFILES, SESSIONNAME, SYSTEMDRIVE, SYSTEMROOT, TEMP, TMP, USERDOMAIN, USERNAME, USERPROFILE,
             WINDIR, DB_USERNAME, DBUSERNAME, DBPORT, DB_PORT, PORT, HOSTNAME, DB_HOST, DB_USER, DBHOST, DBUSER, DBPASS, DB_PASS,
             PASSWORD, DBPASSWORD, BASE_URL, TOKEN, API_TOKEN, CONNECTIONSTRING, DEVELOPMENT, DATABASE_URL, SECRET_KEY);
{$EndRegion}
//--------------------------------------------------------------------------------------------------------------------------
{$Region 'DotEnv4Delphi´s interface'}
  iDotEnv4Delphi = interface
    ['{3BF1532F-91B1-4C1F-A40A-CD81F8754451}']
    //Main methods
    function Config(const OnlyFromEnvFile: Boolean = False): iDotEnv4Delphi; overload;
    function Config(const path: string = ''; OnlyFromEnvFile: Boolean = False): iDotEnv4Delphi; overload;
    function Env(const name: string): string; overload;
    function Env(const EnvVar: TEnvVar): string; overload;
    function EnvOrDefault(const name, default: string): string; overload;
    function EnvOrDefault(const EnvVar: TEnvVar; default: string): string; overload;
    function GetVersion: string;

    //Specific methods to access specific variables

    //To access WebAPI specific variables
    function BaseUrl: string;
    function SecretKey: string;
    function Port: integer;
    function PortOrDefault(const default: integer = 0): integer;
    function Token: string;

    //To access Database Connection specific variables
    function Hostname: string;
    function DBHost: string;
    function DBPort: integer;
    function DBPortOrDefault(const default: integer = 0): integer;
    function DBPassword: string;
    function ConnectionString: string;
    function Password: string;
    function DatabaseURL: string;

    //To access Development specific variables
    function isDevelopment: Boolean;
    function ComputerName: string;
    function ProcessorArchitecture: string;
    function TEMP_Dir: string;
    function WindowsDir: string;
    function AppData: string;
    function ClientName: string;
    function CommonProgramFiles: string;
    function ProgramFiles: String;
    function OS: string;
    function AppPath: string;
  end;
{$EndRegion}
//--------------------------------------------------------------------------------------------------------------------------
{$Region 'DotEnv4Delphi´s class declaration'}
  TDotEnv4Delphi = class(TInterfacedObject, iDotEnv4Delphi)
    private
     class var FInstance: iDotEnv4Delphi;

     //Variables to manage the class
     fromDotEnvFile: Boolean;
     EnvPath:        string;
     EnvDict:        TDictionary<string, string>;

     //Methods to make the class work
     procedure ReadEnvFile;
     function ReadValueFromEnvFile(const key: string): string;
    public
     //Constructors and Destructors
     constructor Create;
     Destructor Destroy; override;
     class function New: iDotEnv4Delphi;

     //Main methods
     function Config(const OnlyFromEnvFile: Boolean = False): iDotEnv4Delphi; overload;
     function Config(const path: string = ''; OnlyFromEnvFile: Boolean = False): iDotEnv4Delphi; overload;
     function Env(const name: string): string; overload;
     function Env(const EnvVar: TEnvVar): string; overload;
     function EnvOrDefault(const name, default: string): string; overload;
     function EnvOrDefault(const EnvVar: TEnvVar; default: string): string; overload;
     function GetVersion: string;

     //Specific methods to access specific variables

     //To access WebAPI specific variables
     function BaseUrl: string;
     function SecretKey: string;
     function Port: integer;
     function PortOrDefault(const default: integer = 0): integer;
     function Token: string;

     //To access Database Connection specific variables
     function Hostname: string;
     function DBHost: string;
     function DBPort: integer;
     function DBPortOrDefault(const default: integer = 0): integer;
     function DBPassword: string;
     function ConnectionString: string;
     function DatabaseURL: string;
     function Password: string;

     //To access Development specific variables
     function isDevelopment: Boolean;
     function ComputerName: string;
     function ProcessorArchitecture: string;
     function TEMP_Dir: string;
     function WindowsDir: string;
     function AppData: string;
     function ClientName: string;
     function CommonProgramFiles: string;
     function ProgramFiles: String;
     function OS: string;
     function AppPath: string;     
  end;
{$EndRegion}
//--------------------------------------------------------------------------------------------------------------------------

//--------------------------------------------------------------------------------------------------------------------------
var
 DotEnv: iDotEnv4Delphi;
 
const
 fVersion = '1.4.0';  //Const to manage versioning
//-------------------------------------------------------------------------------------------------------------------------- 

implementation

uses
  {$IFDEF FPC}
    SysUtils,
    TypInfo,
    Classes;
  {$ELSE}
    System.SysUtils,
    System.TypInfo,
    System.Classes;
  {$ENDIF}

{ TDotEnv4Delphi }

{$REGION 'Constructor and Destructor'}
constructor TDotEnv4Delphi.Create;
begin
  EnvDict := TDictionary<string, string>.create;
  EnvPath := ExtractFilePath(ParamStr(0)) + '.env';
  fromDotEnvFile := False;
  ReadEnvFile;
end;

//Method to instantiate the class in a Singleton pattern way
class function TDotEnv4Delphi.New: iDotEnv4Delphi;
begin
  if not Assigned(FInstance) then
   FInstance := Self.Create;

  Result := FInstance;
end;

destructor TDotEnv4Delphi.Destroy;
begin
  FreeAndNil(EnvDict);
  inherited;
end;
{$ENDREGION}
//--------------------------------------------------------------------------------------------------------------------------
{$REGION 'Internal Methods to make the class work'}
function TDotEnv4Delphi.ReadValueFromEnvFile(const key: string): string;
begin
  EnvDict.TryGetValue(key.ToUpper, Result);
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
{$ENDREGION}
//--------------------------------------------------------------------------------------------------------------------------
{$REGION 'Main methods of DotEnv4Delphi Class'}
/// <summary>
///   Use this method to get the value of the variable you inform in the "name" parameter from either Environment or DotEnv file
/// </summary>
/// <param name="name">The name of the variable you want to get its value</param>
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

/// <summary>
///   Use this method to get the value of the variable you inform in the "EnvVar" parameter from either Environment or DotEnv file
/// </summary>
/// <param name="EnvVar">The Enum name of the variable you want to get its value</param>
function TDotEnv4Delphi.Env(const EnvVar: TEnvVar): string;
begin
  Result := Env(GetEnumName(TypeInfo(TEnvVar), integer(EnvVar)));
end;

/// <summary>
///   Use this method to get the value of the variable you inform in the "EnvVar" parameter from either Environment or DotEnv file
///   If it doesn't exist, it'll return the value you pass as Default value
/// </summary>
/// <param name="EnvVar">The Enum name of the variable you want to get its value</param>
/// <param name="Default">The default value you can get if the env var doesn't exist</param>
function TDotEnv4Delphi.EnvOrDefault(const EnvVar: TEnvVar; default: string): string;
var
  Value: string;
begin
  Value := Env(EnvVar);

  if Value = EmptyStr then
   Result := default
  else
   Result := Value;
end;

/// <summary>
///   Use this method to get the value of the variable you inform in the "name" parameter from either Environment or DotEnv file
///   If it doesn't exist, it'll return the value you pass as Default value
/// </summary>
/// <param name="name">The name of the variable you want to get its value</param>
/// <param name="Default">The default value you can get if the env var doesn't exist</param>
function TDotEnv4Delphi.EnvOrDefault(const name, default: string): string;
var
  Value: string;
begin
  Value := Env(name);

  if Value = EmptyStr then
   Result := default
  else
   Result := Value;
end;

/// <summary>
///   Use this method to get the version of DotEnv4Delphi
/// </summary>
function TDotEnv4Delphi.GetVersion: string;
begin
  Result := fVersion;
end;

/// <summary>
///   Use this method to set some specific configurations. The first could be if you only want to read values from a DotEnv file
/// </summary>
/// <param name="OnlyFromEnvFile">Set it to True to only use variables declared in the DotEnv file</param>
function TDotEnv4Delphi.Config(const OnlyFromEnvFile: Boolean): iDotEnv4Delphi;
begin
  Result := Self;
  fromDotEnvFile := OnlyFromEnvFile;
end;

/// <summary>
///   Use this method to set some specific configurations. The first could be if you only want to read values from a DotEnv file and you can specify a path to the DotEnv file
/// </summary>
/// <param name="path">Set the path of the DotEnv file you want to use</param>
/// <param name="OnlyFromEnvFile">Set it to True to only use variables declared in the DotEnv file</param>
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
{$ENDREGION}
//--------------------------------------------------------------------------------------------------------------------------
{$REGION 'Methods to help development'}
/// <summary>
///   Gets the variable value and returns True if the environment is set to Development
/// </summary>
function TDotEnv4Delphi.isDevelopment: Boolean;
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

/// <summary>
///   Gets the variable value and returns the Operating System
/// </summary>
function TDotEnv4Delphi.OS: string;
begin
  Result := Env(TEnvVar.OS);
end;

/// <summary>
///   Gets the variable value and returns the Name of Client machine.
/// </summary>
function TDotEnv4Delphi.ClientName: string;
begin
  Result := Env(TEnvVar.CLIENTNAME);
end;

/// <summary>
///   Gets the variable value and returns the Path of common program files folder.
/// </summary>
function TDotEnv4Delphi.CommonProgramFiles: string;
begin
  Result := Env(TEnvVar.COMMONPROGRAMFILES);
end;

/// <summary>
///   Gets the variable value and returns the Name of Computer code is running on.
/// </summary>
function TDotEnv4Delphi.ComputerName: string;
begin
  Result := Env(TEnvVar.COMPUTERNAME);
end;

/// <summary>
///   Gets the variable value and returns the Type of CPU architecture. For example, X86 for Intel Pentium processors.
/// </summary>
function TDotEnv4Delphi.ProcessorArchitecture: string;
begin
  Result := Env(TEnvVar.PROCESSOR_ARCHITECTURE);
end;

/// <summary>
///   Gets the variable value and returns the Path of the program files folder.
/// </summary>
function TDotEnv4Delphi.ProgramFiles: String;
begin
  Result := Env(TEnvVar.PROGRAMFILES);
end;

/// <summary>
///   Gets the variable value and returns the Path of the Windows folder.
/// </summary>
function TDotEnv4Delphi.WindowsDir: string;
begin
  Result := Env(TEnvVar.WINDIR);
end;

/// <summary>
///   Gets the variable value and returns the Path of the temporary files folder.
/// </summary>
function TDotEnv4Delphi.TEMP_Dir: string;
begin
  Result := Env(TEnvVar.TEMP);
end;

/// <summary>
///   Gets the variable value and returns the Path of the application data folder.
/// </summary>
function TDotEnv4Delphi.AppData: string;
begin
  Result := Env(TEnvVar.APPDATA);
end;

function TDotEnv4Delphi.AppPath: string;
begin
  Result := ExtractFileDir(ParamStr(0));
end;
{$ENDREGION}
//--------------------------------------------------------------------------------------------------------------------------
{$REGION 'Methods to access variables for a WebAPI'}
/// <summary>
///   Read the variable and fill the BaseUrl of the webAPI
/// </summary>
function TDotEnv4Delphi.BaseUrl: string;
var
 Base: string;
begin
  Base := Env('BASE_URL');

  if Base = EmptyStr then
   Base := Env('BASEURL');

  Result := Base;
end;

/// <summary>
///   Read the variable and fill the Secret Key of the webAPI
/// </summary>
function TDotEnv4Delphi.SecretKey: string;
var
  fSecret: string;
begin
  fSecret := Env('SECRET_KEY');

  if fSecret = EmptyStr then
   fSecret := Env('Secret_Key');

  if fSecret = EmptyStr then
   fSecret := Env('SecretKey');

  if fSecret = EmptyStr then
   fSecret := Env('SECRETKEY');

  Result := fSecret;
end;

/// <summary>
///   Read the variable and fill the Port of the webAPI
/// </summary>
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

/// <summary>
///   Read the variable and fill the Port of the webAPI. If it doesn't exist, it'll be filled with the default value defined in the parameter
/// </summary>
/// <param name="default">The default value</param>
function TDotEnv4Delphi.PortOrDefault(const default: integer): integer;
var
  _port: string;
begin
  _port := Env('Port');

  if _Port = EmptyStr then
   _port := Env('PORT');

  if _Port <> EmptyStr then
   Result := StrToInt(_Port)
  else
   Result := default;
end;

/// <summary>
///   Read the variable and fill the Token of the webAPI
/// </summary>
function TDotEnv4Delphi.Token: string;
begin
  Result := Env('TOKEN');
end;
{$ENDREGION}
//--------------------------------------------------------------------------------------------------------------------------
{$REGION 'Methods to work with Database connections'}
/// <summary>
///   Read variable and get the connection string
/// </summary>
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

/// <summary>
///   Read variable and get the Database URL
/// </summary>
function TDotEnv4Delphi.DatabaseURL: string;
var
  fDBURL: string;
begin
  fDBURL := Env('DATABASE_URL');

  if fDBURL = EmptyStr then
   fDBURL := Env('Database_URL');

  if fDBURL = EmptyStr then
   fDBURL := Env('DatabaseURL');

  if fDBURL = EmptyStr then
   fDBURL := Env('Database_URL');

  Result := fDBURL;
end;

/// <summary>
///   Read variable and get the DB Host
/// </summary>
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

/// <summary>
///   Read variable and get the DB Password
/// </summary>
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

/// <summary>
///   Read variable and get the Port to use to access the Database
/// </summary>
function TDotEnv4Delphi.DBPort: integer;
var
  _port: string;
begin
  Result := 0;

  _port := Env('DBPort');

  if _Port = EmptyStr then
   _port := Env('DBPORT');

  if _Port <> EmptyStr then
   Result := StrToInt(_Port);
end;

/// <summary>
///   Read variable and get the Port to use to access the Database otherwise returns the default value defined
/// </summary>
/// <param name="default">The default value</param>
function TDotEnv4Delphi.DBPortOrDefault(const default: integer = 0): integer;
var
  _port: string;
begin
  _port := Env('DBPort');

  if _Port = EmptyStr then
   _port := Env('DBPORT');

  if _Port <> EmptyStr then
   Result := StrToInt(_Port)
  else
   Result := default;
end;

/// <summary>
///   Read variable and get the Hostname to connect to the Database
/// </summary>
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

/// <summary>
///   Read variable and get the Password
/// </summary>
function TDotEnv4Delphi.Password: string;
var
  _pass: string;
begin
  _pass := Env('Password');

  if _pass = EmptyStr then
   _pass := Env('PASSWORD');

  Result := _pass;
end;
{$ENDREGION}

initialization

begin
  //Here the instance of the DotEnv4Delphi is created to be used in a Sigleton Pattern way
  DotEnv := tDotEnv4Delphi.New;
end;

end.
