object FrmServerMain: TFrmServerMain
  Left = 0
  Top = 0
  Caption = 'FrmServerMain'
  ClientHeight = 202
  ClientWidth = 447
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object Server: TDWServer
    Port = '80'
    DocDir = 'C:\Program Files (x86)\Embarcadero\Studio\18.0\bin\wwwroot'
    TemplateDir = 
      'C:\Program Files (x86)\Embarcadero\Studio\18.0\bin\wwwroot\templ' +
      'ates\'
    LibDir = 
      'C:\Program Files (x86)\Embarcadero\Studio\18.0\bin\wwwroot\dwlib' +
      '\'
    UrlBase = 'http://0.0.0.0:80'
    Left = 216
    Top = 104
  end
end
