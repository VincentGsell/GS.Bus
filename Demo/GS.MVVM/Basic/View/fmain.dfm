object Form1: TForm1
  Left = 0
  Top = 0
  Caption = 'Form1'
  ClientHeight = 680
  ClientWidth = 956
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  DesignSize = (
    956
    680)
  PixelsPerInch = 96
  TextHeight = 13
  object Label1: TLabel
    Left = 272
    Top = 133
    Width = 103
    Height = 13
    Caption = 'Response from Model'
  end
  object LblReflect: TLabel
    Left = 272
    Top = 96
    Width = 104
    Height = 19
    Caption = '<Response>'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -16
    Font.Name = 'Tahoma'
    Font.Style = [fsBold]
    ParentFont = False
  end
  object Label3: TLabel
    Left = 120
    Top = 80
    Width = 22
    Height = 13
    Caption = 'View'
  end
  object LblReflectFromModel: TLabel
    Left = 272
    Top = 145
    Width = 104
    Height = 19
    Caption = '<Response>'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -16
    Font.Name = 'Tahoma'
    Font.Style = [fsBold]
    ParentFont = False
  end
  object Label4: TLabel
    Left = 273
    Top = 80
    Width = 75
    Height = 13
    Caption = 'Diret link to edit'
  end
  object LabelTime: TLabel
    Left = 8
    Top = 60
    Width = 45
    Height = 13
    Caption = 'Labeltime'
  end
  object Label2: TLabel
    Left = 8
    Top = 79
    Width = 23
    Height = 13
    Caption = 'xpos'
  end
  object Label5: TLabel
    Left = 8
    Top = 98
    Width = 23
    Height = 13
    Caption = 'ypos'
  end
  object Label6: TLabel
    Left = 8
    Top = 117
    Width = 29
    Height = 13
    Caption = 'xypos'
  end
  object Edit1: TEdit
    Left = 120
    Top = 96
    Width = 121
    Height = 21
    TabOrder = 0
    Text = 'Edit1'
    OnChange = Edit1Change
  end
  object Panel1: TPanel
    Left = 0
    Top = 0
    Width = 956
    Height = 58
    Align = alTop
    Caption = 
      'Basic demo : "View edit is linked to a model property, which is ' +
      'reflected by label.'
    Color = clMoneyGreen
    ParentBackground = False
    TabOrder = 1
  end
  object Button1: TButton
    Left = 8
    Top = 216
    Width = 153
    Height = 25
    Caption = 'Bus State'
    TabOrder = 2
    OnClick = Button1Click
  end
  object Memo1: TMemo
    Left = 8
    Top = 247
    Width = 941
    Height = 422
    Anchors = [akLeft, akTop, akRight, akBottom]
    Lines.Strings = (
      'Memo1')
    TabOrder = 3
  end
end
