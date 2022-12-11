unit DotEnv4Delphi;

interface

type
  TEnvVar = (ALLUSERSPROFILE, APPDATA, CLIENTNAME, COMMONPROGRAMFILES);

  TDotEnv4Delphi = class
    class function GetVar(const name: string; fromDotEnvFile: Boolean = False): string; overload;
    class function GetVar(const EnvVar: TEnvVar; fromDotEnvFile: Boolean = False): string; overload;
    class function GetVersion: string;
  end;

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

class function TDotEnv4Delphi.GetVar(const name: string; fromDotEnvFile: Boolean): string;
begin
  if fromDotEnvFile then
   begin
     Result := ReadValueFromEnvFile(name);
   end
  else
   Result := GetEnvironmentVariable(name);
end;

class function TDotEnv4Delphi.GetVar(const EnvVar: TEnvVar; fromDotEnvFile: Boolean): string;
begin
  if fromDotEnvFile then
   begin

   end
  else
   Result := GetEnvironmentVariable(GetEnumName(TypeInfo(TEnvVar), integer(EnvVar)));
end;

class function TDotEnv4Delphi.GetVersion: string;
begin
  Result := '0.0.1-a';
end;

end.
