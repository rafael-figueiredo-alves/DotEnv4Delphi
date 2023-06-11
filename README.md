<p align="center">
<img src="https://github.com/rafael-figueiredo-alves/DotEnv4Delphi/blob/main/Imagens/DotEnv4Delphi.png" width=50% height=50%>  
</p>

<p>
  <a href="#compatibility"><img src="https://img.shields.io/static/v1?label=rad%20studio&message=xe2%2B&color=silver&style=for-the-badge&logo=delphi&logoColor=white" alt="Delphi XE2+ support" /></a>
  <a href="#compatibility"><img src="https://img.shields.io/static/v1?label=platforms&message=cross-platform&color=silver&style=for-the-badge&logo=delphi&logoColor=white" alt="Cross-platform support" /></a>
  <a href="#compatibility"><img src="https://img.shields.io/static/v1?label=applications&message=console%2C%20fmx%2C%20vcl&color=silver&style=for-the-badge&logo=delphi&logoColor=white" alt="Console, FMX, VCL support" /></a>
</p>

#

## About

DotEnv4Delphi is a library to use **.env** files in Delphi/Lazarus. You can also get Environment Variables in a very easy and fast way.

## Latest version - New features

DotEnv4Delphi's latest version is **1.2.0**

New features implemented:
- Implemented access to a variable you can define called Development (a boolean variable) to tell the app it is a Development environment or production (using isDevelopment function)
- Implemented direct access to some variables:
  - Port
  - Hostname
  - Password
  - DbPassword
  - DbHost
  - Token
  - BaseURL
  - ConnectionString
  - Database_URL
  - Secret_Key 

## Features

- Access Environment Variables in a simple, easy, fast way;
- Use .env files to handle sensible variables and respect [the twelve-factor App](http://12factor.net/config);
- Bring a JavaScript and Python lib to Delphi/Lazarus;
- a lightweight unit using the Singleton Pattern.

# Summary

- [Installing the lib](#installing-the-lib) 
- [Using the library](#using-the-library)
   - [Adding the Unit](#adding-the-unit)
   - [Getting Environment Variables from OS](#getting-environment-variables-from-OS)
   - [Getting variable values from .Env files](#getting-variable-values-from-env-files)
     - [About .Env files](#about-env-files)
     - [Setting a different path to .Env file](#setting-a-different-path-to-env-file)
     - [Setting to only read values from .Env file](#setting-to-only-read-values0from-env-file)
- [Compatibility](#compatibility)
- [Version](#version) 

# Installing the lib

To be able to use the DotEnv4Delphi, you must install the lib. There are two simple ways to do it. First, you can copy the DotEnv4Delphi.pas unit from src folder to your project's source folder.

Another way is to use the package manager [Boss](https://github.com/HashLoad/boss). If you do so, you can simply open your project's folder, initialize Boss (you can do it with ```boss init ```) and then use the following command line to get DtoEnv4Delphi as a Dependency of your project:

  ```batch
  boss install https://github.com/rafael-figueiredo-alves/DotEnv4Delphi
  ```

  # Using the library

  ## Adding the Unit

  After installing the unit (by copying the file or using Boss to install it as a dependency), to start using the DotEnv4Delphi, you need to declare the DotEnv4Delphi.pas unit in the **uses clause**. Since it uses Singleton Pattern, we recommend you put the unit in the unit (or units) where you'll need to get variables, like in a database module or in a class unit that you get data from a REST API.

  ## Getting Environment Variables from OS

  If you want to get the value of an environment variable like **APPDATA** (you get the path of app data), you can do it by using the function ``` GetEnvironmentVariable('APPDATA') ``` or, by using the DotEnv4Delphi, you can do it in the same way you're going to get it from a **.env** file:

  ``` pascal 
  Memo1.Lines.Add(DotEnv.Env('appdata'));
  ```

  From the code above, you get the System Environment variable APPDATA and write its value in a TMemo line. So, the only thing you have to do to get the value is: ``` DotEnv.Env('APPDATA'); ```. It always returns a string that can be empty or not, depending if the variable exists or not. The Method tries to read the variable from the system and, if it doesn't find it, it starts to look for it in the **.Env** file. Simple, isn't it?

  You can also take advantage from the TEnvVar Enum from DotEnv4Delphi unit. There are all system Environment variables and some extra that are commonly used in .env files from JavaScript examples. See how easy it is to get the value:

  ``` pascal
  Memo1.Lines.Add(DotEnv.Env(tenvVar.ALLUSERSPROFILE));
  ```  

  or

  ``` pascal
  Memo1.Lines.Add(DotEnv.Env(ALLUSERSPROFILE));
  ```    

  ## Getting variable values from .Env files
  
  Getting values from variables from a **.Env File** is a piece of cake. You only have to do the same you learned from getting a system variable. 

  ``` pascal
  Memo1.Lines.Add(DotEnv.Env('MeuNome'));
  ```  

  In the example above, since there isn't a variable called "MeuNome" in the System Environment variables, the library tries to get it from a .Env file that should be stored in the same folder as the executable file of your app (in the same folder as your app). If there isn't a .env file, the code above will return an empty string. You can also use the TEnvVar enum, but you should remember that the library first looks for it in the system by default, but you can change it as you'll learn bellow.

  ### About .Env files

  ` .env ` files (a.k.a. "dotenv") store key-value pairs in a format descended from simple bash files that exported environment variables.

  This implementation cleaves closely to the format described by the original dotenv package, but it is not a direct match (by design).

  Typically, a dotenv (`.env`) file is formatted into simple key-value pairs:

  ```Batch
  S3_BUCKET=YOURS3BUCKET
  SECRET_KEY=YOURSECRETKEYGOESHERE
  ```

  #### Variable Names

  For the sake of portability (and sanity), environment variable names must consist solely of letters, digits, and the underscore ( `_ `) and must not begin with a digit. In regex-speak, the names must match the following pattern:

  [a-zA-Z_]+[a-zA-Z0-9_]*

  #### Values

  Values are to the right of the equals sign. They may be quoted. Using single or double quotes will prevent variables from being interpolated.

  ```batch
  SIMPLE=xyz123
  INTERPOLATED="Multiple\nLines and variable substitution: ${SIMPLE}"
  NON_INTERPOLATED='raw text without variable interpolation'
  ```

  #### Interpolation (a.k.a. Variable Substitution)

  Values left unquoted will interpolate variables in the ${VAR} syntax. This can be useful for referencing existing system environment variables or to reference variables previously parsed.

  For example:

  ```batch
  USER=admin
  EMAIL=${USER}@example.org
  ```

  #### Non-Interpolated

  If your values must retain ${} in their output, wrap the value in single or double quotes, e.g.:

  ```batch
  PASSWORD='!@G0${k}k'
  PASSWORD_API="!@G0${k}k"
  ```

  #### Comments

  The hash-tag # symbol denotes a comment when on its own line or when it follows a quoted value. It is not treated as a comment when it appears within quotes.

  ``` Batch
  # This is a comment
  SECRET_KEY=YOURSECRETKEYGOESHERE # also a comment
  SECRET_HASH="something-with-a-hash-#-this-is-not-a-comment"
  ```

  #### Create a .env-example

  A good thing about .env files is that you don't (and you mustn't) send them to your github repo. To do it, include the following line in the .gitignore:

  ```
  # DotEnv environment variables during Development
  .env
  ```

  Warning: If your .env is already part of your Git repository, adding it to .gitignore will not remove it. In this case, you’ll also need to tell Git to stop tracking .env.

  Because of it, it is strongly recommended to add a .env-example file to show others what they should implement on their own .env files to make your project work.

  ### Setting a different path to .Env file

  To set a different path to .Env file, just use the following command:

  ```DotEnv.Config('D:\Meus Projetos\DotEnv4Delphi\Demo\Win32\Debug\.env');```

  or 

  ```DotEnv.Config('D:\Meus Projetos\DotEnv4Delphi\Demo\.env');```

  ### Setting to only read values from .Env file

  In some cases, you must need only to get variables from your .env file. In that case, you can use the following line:

  ```DotEnv.Config(True);```

  If you want to set a different path and also set to only get variables from it, just include the following line:

  ```DotEnv.Config('D:\Meus Projetos\DotEnv4Delphi\Demo\.env', True);```

  If you set to false, you'll use both, the system environment variables and .env file variables. The same applies if you pass an empty string to the path if you want to use the default path. 

  **REMEMBER:** You don't need to use DotEnv.Config() if you don't want to change the path or set to only use the .env file. And anytime you use this line, you'll change the default and previous config.


  # Compatibility

  I guess that the DotEnv4Delphi library is compatible to any version of Delphi since Delphi XE2 and probably Lazarus as well. The Operating systems are the same supported by Delphi / Lazarus.

  # Version

  **[DotEnv4Delphi version 1.2.0](/../../releases/latest)**
