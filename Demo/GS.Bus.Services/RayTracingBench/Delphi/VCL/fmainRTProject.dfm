object Form1: TForm1
  Left = 0
  Top = 0
  Caption = 'Form1'
  ClientHeight = 693
  ClientWidth = 1045
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
    1045
    693)
  PixelsPerInch = 96
  TextHeight = 13
  object Image1: TImage
    Left = 320
    Top = 8
    Width = 717
    Height = 533
    Anchors = [akLeft, akTop, akRight, akBottom]
    Center = True
    Proportional = True
    Stretch = True
    OnMouseMove = Image1MouseMove
  end
  object Button1: TButton
    Left = 8
    Top = 32
    Width = 129
    Height = 57
    Caption = 'No thread'
    TabOrder = 0
    OnClick = Button1Click
  end
  object Memo1: TMemo
    Left = 143
    Top = 547
    Width = 894
    Height = 138
    Anchors = [akLeft, akRight, akBottom]
    Lines.Strings = (
      'Memo1')
    TabOrder = 1
  end
  object Button3: TButton
    Left = 8
    Top = 221
    Width = 129
    Height = 59
    Caption = 'ITask'
    TabOrder = 2
    OnClick = Button3Click
  end
  object Button4: TButton
    Left = 8
    Top = 158
    Width = 129
    Height = 57
    Caption = 'Classic TThread'
    TabOrder = 3
    OnClick = Button4Click
  end
  object Button5: TButton
    Left = 8
    Top = 286
    Width = 129
    Height = 57
    Caption = 'GS.Bus.Services'
    TabOrder = 4
    OnClick = Button5Click
  end
  object Button6: TButton
    Left = 8
    Top = 349
    Width = 129
    Height = 57
    Caption = 'GS.thread'
    TabOrder = 5
    OnClick = Button6Click
  end
  object TrackBar1: TTrackBar
    Left = 92
    Top = 547
    Width = 45
    Height = 110
    Max = 400
    Min = 1
    Orientation = trVertical
    Position = 5
    TabOrder = 6
    OnChange = TrackBar1Change
  end
  object Button2: TButton
    Left = 143
    Top = 111
    Width = 106
    Height = 40
    Caption = 'Bench (10)'
    TabOrder = 7
    OnClick = Button2Click
  end
  object Button7: TButton
    Left = 255
    Top = 111
    Width = 19
    Height = 19
    Caption = '+'
    TabOrder = 8
    OnClick = Button7Click
  end
  object Button8: TButton
    Left = 255
    Top = 132
    Width = 19
    Height = 17
    Caption = '-'
    TabOrder = 9
    OnClick = Button8Click
  end
  object Panel1: TPanel
    Left = 143
    Top = 157
    Width = 131
    Height = 57
    Caption = 'Panel1'
    TabOrder = 10
  end
  object Panel2: TPanel
    Left = 143
    Top = 220
    Width = 131
    Height = 59
    Caption = 'Panel1'
    TabOrder = 11
  end
  object Panel3: TPanel
    Left = 143
    Top = 285
    Width = 131
    Height = 57
    Caption = 'Panel1'
    TabOrder = 12
  end
  object Panel4: TPanel
    Left = 143
    Top = 348
    Width = 131
    Height = 57
    Caption = 'Panel1'
    TabOrder = 13
  end
end
