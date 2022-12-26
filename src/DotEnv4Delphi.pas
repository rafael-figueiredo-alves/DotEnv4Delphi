unit DotEnv4Delphi;

interface

type
  TEnvVar = (ALLUSERSPROFILE, APPDATA, CLIENTNAME, COMMONPROGRAMFILES, COMPUTERNAME, COMSPEC, HOMEDRIVE, HOMEPATH, LOGONSERVER,
             NUMBER_OF_PROCESSORS, OS, PATH, PATHEXT, PROCESSOR_ARCHITECTURE, PROCESSOR_IDENTIFIER, PROCESSOR_LEVEL,
             PROCESSOR_REVISION, PROGRAMFILES, SESSIONNAME, SYSTEMDRIVE, SYSTEMROOT, TEMP, TMP, USERDOMAIN, USERNAME, USERPROFILE,
             WINDIR, DB_USERNAME, DBUSERNAME, DBPORT, DB_PORT, PORT, HOSTNAME, DB_HOST, DB_USER, DBHOST, DBUSER, DBPASS, DB_PASS,
             PASSWORD, DBPASSWORD, BASE_URL, TOKEN, API_TOKEN);

  TDotEnv4Delphi = class
    class function Env(const name: string; fromDotEnvFile: Boolean = False): string; overload;
    class function Env(const EnvVar: TEnvVar; fromDotEnvFile: Boolean = False): string; overload;
    class function GetVersion: string;
  end;

  DotEnv = TDotEnv4Delphi;

implementation

uses
  System.SysUtils,
  System.TypInfo,
  System.Generics.Collections, System.Classes;

{ TDotEnv4Delphi }

function ReadValueFromEnvFile(const key: string): string;
var
  EnvFile : TDictionary<string, string>;

  procedure PopulateDictionary(const Dict: TDictionary<string, string>);
  var
    fFile    : tstringlist;
    EnvFile  : string;
    position : Integer;
  begin
    EnvFile := ExtractFilePath(ParamStr(0)) + '.env';
    fFile := TStringList.Create;
    try
      fFile.LoadFromFile(EnvFile);
      for position := 0 to fFile.Count - 1 do
       begin
         if not (fFile.Names[position].ToUpper = EmptyStr) then
          Dict.Add(fFile.Names[position].ToUpper, fFile.Values[fFile.Names[position]]);
       end;
    finally
      FreeAndNil(fFile)
    end;
  end;
begin
  EnvFile := TDictionary<string, string>.create;
  try
    PopulateDictionary(EnvFile);
    EnvFile.TryGetValue(key.ToUpper, Result);
  finally
    FreeAndNil(EnvFile);
  end;
end;

class function TDotEnv4Delphi.Env(const name: string; fromDotEnvFile: Boolean): string;
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

class function TDotEnv4Delphi.Env(const EnvVar: TEnvVar; fromDotEnvFile: Boolean): string;
begin
  if fromDotEnvFile then
   begin
     Result := ReadValueFromEnvFile(GetEnumName(TypeInfo(TEnvVar), integer(EnvVar)));
     Exit;
   end;

  Result := GetEnvironmentVariable(GetEnumName(TypeInfo(TEnvVar), integer(EnvVar)));

  if Result = EmptyStr then
   Result := ReadValueFromEnvFile(GetEnumName(TypeInfo(TEnvVar), integer(EnvVar)));
end;

class function TDotEnv4Delphi.GetVersion: string;
begin
  Result := '0.0.2-a';
end;

end.
