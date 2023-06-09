object Form2: TForm2
  Left = 0
  Top = 0
  BorderIcons = [biSystemMenu, biMinimize]
  BorderStyle = bsSingle
  Caption = 'Demo da lib DotEnv4Delphi'
  ClientHeight = 332
  ClientWidth = 482
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  Position = poDesktopCenter
  OnCreate = FormCreate
  TextHeight = 13
  object Label1: TLabel
    Left = 64
    Top = 265
    Width = 49
    Height = 13
    Caption = 'Ambiente:'
  end
  object Label2: TLabel
    Left = 264
    Top = 311
    Width = 125
    Height = 13
    Caption = 'Vers'#227'o do DotEnv4Delphi:'
  end
  object LAmbiente: TLabel
    Left = 119
    Top = 262
    Width = 202
    Height = 18
    AutoSize = False
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -15
    Font.Name = 'Tahoma'
    Font.Style = [fsBold]
    ParentFont = False
  end
  object LVersion: TLabel
    Left = 396
    Top = 308
    Width = 69
    Height = 18
    AutoSize = False
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -15
    Font.Name = 'Tahoma'
    Font.Style = [fsBold]
    ParentFont = False
  end
  object Button1: TButton
    Left = 64
    Top = 48
    Width = 161
    Height = 25
    Caption = 'Pegar Vari'#225'veis de Ambiente'
    TabOrder = 0
    OnClick = Button1Click
  end
  object Memo1: TMemo
    Left = 64
    Top = 111
    Width = 353
    Height = 146
    TabOrder = 1
  end
  object Button2: TButton
    Left = 64
    Top = 80
    Width = 161
    Height = 25
    Caption = 'Pegar Vari'#225'veis do .Env'
    TabOrder = 2
    OnClick = Button2Click
  end
  object Button3: TButton
    Left = 231
    Top = 48
    Width = 186
    Height = 25
    Caption = #201' ambiente de desenvolvimento'
    TabOrder = 3
    OnClick = Button3Click
  end
  object Button4: TButton
    Left = 231
    Top = 79
    Width = 186
    Height = 25
    Caption = 'Pegar valor da porta (Port)'
    TabOrder = 4
    OnClick = Button4Click
  end
end
