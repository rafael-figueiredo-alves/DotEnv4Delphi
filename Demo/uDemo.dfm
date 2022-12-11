object Form2: TForm2
  Left = 0
  Top = 0
  Caption = 'Demo da lib DotEnv4Delphi'
  ClientHeight = 332
  ClientWidth = 536
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  PixelsPerInch = 96
  TextHeight = 13
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
    Width = 185
    Height = 89
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
end
