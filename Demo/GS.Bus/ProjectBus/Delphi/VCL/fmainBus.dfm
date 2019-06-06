object Form2: TForm2
  Left = 0
  Top = 0
  Caption = 'Embeded Bus'
  ClientHeight = 725
  ClientWidth = 1095
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  OnCloseQuery = FormCloseQuery
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  DesignSize = (
    1095
    725)
  PixelsPerInch = 96
  TextHeight = 13
  object Label6: TLabel
    Left = 536
    Top = 359
    Width = 31
    Height = 13
    Caption = 'Label6'
  end
  object Panel1: TPanel
    Left = 8
    Top = 3
    Width = 1079
    Height = 41
    Anchors = [akLeft, akTop, akRight]
    Caption = 'Panel1'
    TabOrder = 0
    object Label8: TLabel
      Left = 4
      Top = 14
      Width = 31
      Height = 13
      Caption = 'Label8'
    end
  end
  object PageControl1: TPageControl
    Left = 8
    Top = 50
    Width = 1074
    Height = 615
    ActivePage = tsBus
    TabOrder = 1
    object tsBus: TTabSheet
      Caption = 'Basic Bus Features'
      object pnl2: TPanel
        Left = 0
        Top = 0
        Width = 1066
        Height = 587
        Align = alClient
        TabOrder = 0
        DesignSize = (
          1066
          587)
        object Label3: TLabel
          Left = 151
          Top = 13
          Width = 50
          Height = 13
          Caption = 'to channel'
        end
        object Label2: TLabel
          Left = 728
          Top = 96
          Width = 158
          Height = 13
          Caption = 'GUI Received event'#39's hit count : '
        end
        object Label1: TLabel
          Left = 892
          Top = 96
          Width = 6
          Height = 13
          Caption = '0'
        end
        object lblChannels: TLabel
          Left = 436
          Top = 96
          Width = 123
          Height = 13
          Caption = 'Channels (Data from Bus)'
        end
        object lbl2: TLabel
          Left = 431
          Top = 285
          Width = 138
          Height = 13
          Caption = 'Subscribters (Data from Bus)'
        end
        object Label7: TLabel
          Left = 311
          Top = 69
          Width = 100
          Height = 13
          Caption = 'Messages Count limit'
        end
        object edt1: TEdit
          Left = 207
          Top = 9
          Width = 202
          Height = 21
          TabOrder = 0
          Text = 'My New Channel...'
        end
        object btn2: TButton
          Left = 16
          Top = 5
          Width = 129
          Height = 34
          Caption = 'Add subscribters'
          TabOrder = 1
          OnClick = btn2Click
        end
        object lst2: TListBox
          Left = 16
          Top = 304
          Width = 393
          Height = 272
          Anchors = [akLeft, akTop, akBottom]
          ItemHeight = 13
          TabOrder = 2
        end
        object btn4: TButton
          Left = 16
          Top = 42
          Width = 129
          Height = 32
          Caption = 'Remove subscribters'
          TabOrder = 3
          OnClick = btn4Click
        end
        object btn3: TButton
          Tag = 1000
          Left = 583
          Top = 58
          Width = 129
          Height = 31
          Caption = 'Send 1000 Messages'
          TabOrder = 4
          OnClick = btn3Click
        end
        object btn1: TButton
          Left = 431
          Top = 41
          Width = 129
          Height = 49
          Caption = 'Send Message'
          TabOrder = 5
          OnClick = btn1Click
        end
        object lvSubscripters: TListView
          Left = 431
          Top = 304
          Width = 627
          Height = 272
          Anchors = [akLeft, akTop, akRight, akBottom]
          Columns = <
            item
              Caption = 'ID'
            end
            item
              Caption = 'Channel'
              Width = 200
            end
            item
              Caption = 'Pending'
              Width = 70
            end
            item
              Caption = 'Processed'
              Width = 70
            end>
          DoubleBuffered = True
          ReadOnly = True
          RowSelect = True
          ParentDoubleBuffered = False
          TabOrder = 6
          ViewStyle = vsReport
        end
        object ListView2: TListView
          Left = 431
          Top = 115
          Width = 627
          Height = 164
          Anchors = [akLeft, akTop, akRight]
          Columns = <
            item
              Caption = 'ChannelID'
              Width = 200
            end
            item
              Caption = 'Type'
              Width = 100
            end
            item
              Caption = 'IsPersits'
              Width = 70
            end
            item
              Caption = 'Echo'
            end
            item
              Caption = 'Received'
              Width = 70
            end
            item
              Caption = 'Consumed'
              Width = 70
            end
            item
              Caption = 'Delivered'
              Width = 70
            end
            item
              Caption = 'Persists'
            end
            item
              Caption = 'Subscribter count'
            end>
          DoubleBuffered = True
          ReadOnly = True
          RowSelect = True
          ParentDoubleBuffered = False
          TabOrder = 7
          ViewStyle = vsReport
        end
        object GroupBox1: TGroupBox
          Left = 436
          Top = 0
          Width = 620
          Height = 35
          Anchors = [akLeft, akTop, akRight]
          Caption = ' Bus Stats'
          TabOrder = 8
          object Label4: TLabel
            Left = 15
            Top = 17
            Width = 95
            Height = 13
            Caption = 'Message Sended'
            Font.Charset = DEFAULT_CHARSET
            Font.Color = clWindowText
            Font.Height = -11
            Font.Name = 'Tahoma'
            Font.Style = [fsBold]
            ParentFont = False
          end
        end
        object Button3: TButton
          Tag = 50000
          Left = 848
          Top = 58
          Width = 129
          Height = 31
          Caption = 'Send 50K Messages (!)'
          TabOrder = 9
          OnClick = btn3Click
        end
        object Button4: TButton
          Tag = 10000
          Left = 716
          Top = 58
          Width = 129
          Height = 31
          Caption = 'Send 10K Messages'
          TabOrder = 10
          OnClick = btn3Click
        end
        object cbThreadedSend: TCheckBox
          Left = 583
          Top = 41
          Width = 97
          Height = 17
          Caption = 'Threaded send'
          TabOrder = 11
        end
        object btTest: TButton
          Left = 16
          Top = 242
          Width = 233
          Height = 25
          Caption = 'Test 1 (Create channels list)'
          TabOrder = 12
          OnClick = btTestClick
        end
        object btTest2: TButton
          Left = 16
          Top = 273
          Width = 233
          Height = 25
          Caption = 'Test 2 (Launch messages on all channel)'
          TabOrder = 13
          OnClick = btTest2Click
        end
        object RadioGroup1: TRadioGroup
          Left = 207
          Top = 63
          Width = 98
          Height = 117
          Caption = ' Channel Type'
          ItemIndex = 0
          Items.Strings = (
            'Topic'
            'Queue')
          TabOrder = 14
          OnClick = RadioGroup1Click
        end
        object Edit2: TEdit
          Left = 311
          Top = 88
          Width = 74
          Height = 21
          TabOrder = 15
          Text = '-1'
        end
        object UpDown1: TUpDown
          Left = 385
          Top = 88
          Width = 16
          Height = 21
          Associate = Edit2
          Min = -1
          Position = -1
          TabOrder = 16
        end
        object RadioGroup2: TRadioGroup
          Left = 311
          Top = 115
          Width = 98
          Height = 63
          Caption = ' Queue Type'
          Items.Strings = (
            'Fault tolerant'
            'Distributed')
          TabOrder = 17
          Visible = False
        end
        object Button5: TButton
          Left = 334
          Top = 184
          Width = 75
          Height = 25
          Caption = 'Apply'
          TabOrder = 18
          OnClick = Button5Click
        end
        object chkMemPersistant: TCheckBox
          Left = 208
          Top = 40
          Width = 198
          Height = 17
          Caption = 'Memory persistant channel'
          TabOrder = 19
        end
        object cbEchoEnabled: TCheckBox
          Left = 208
          Top = 186
          Width = 97
          Height = 17
          Caption = 'Echo enabled'
          Checked = True
          State = cbChecked
          TabOrder = 20
        end
      end
    end
    object tsDataRepo: TTabSheet
      Caption = 'In Memory DataRepo'
      ImageIndex = 1
      DesignSize = (
        1066
        587)
      object Button1: TButton
        Left = 3
        Top = 102
        Width = 198
        Height = 46
        Anchors = [akLeft, akTop, akRight]
        Caption = 'Create new dataRepo'
        TabOrder = 0
        OnClick = Button1Click
      end
      object Edit1: TEdit
        Left = 3
        Top = 75
        Width = 198
        Height = 21
        Anchors = [akLeft, akTop, akRight]
        TabOrder = 1
        Text = 'MyDataRepo'
      end
      object Memo1: TMemo
        Left = 3
        Top = 0
        Width = 1060
        Height = 69
        Anchors = [akLeft, akTop, akRight]
        Color = 11788021
        Lines.Strings = (
          
            'DataRepo is a "side" feature of TBus. It consists of a simple an' +
            'd In-Memory Key value list, available for all foreign thread.'
          '-> This allow to shared stream data very easely.'
          
            '-> This feature used the Bus thread to retrieve your data, so, b' +
            'e aware if you use it extensively with classic message exchange ' +
            ': They shared same thread power.'
          
            'For a more advanced Key Value system, please see to GS.LocalMemC' +
            'ached class, wich implement a TBus.Service oriented Key/Value se' +
            'rver on an abstract persister layer.')
        ReadOnly = True
        TabOrder = 2
      end
      object ListBox1: TListBox
        Left = 3
        Top = 154
        Width = 198
        Height = 394
        Anchors = [akLeft, akTop, akRight]
        ItemHeight = 13
        TabOrder = 3
      end
      object Panel2: TPanel
        Left = 207
        Top = 106
        Width = 554
        Height = 406
        Caption = 'Panel2'
        Color = 11119017
        ParentBackground = False
        TabOrder = 4
        object Label5: TLabel
          Left = 8
          Top = 3
          Width = 31
          Height = 13
          Caption = 'Label5'
        end
        object Label9: TLabel
          Left = 97
          Top = 39
          Width = 18
          Height = 13
          Caption = 'Key'
        end
        object Label10: TLabel
          Left = 97
          Top = 76
          Width = 65
          Height = 13
          Caption = 'Value (String)'
        end
        object Button2: TButton
          Left = 336
          Top = 136
          Width = 145
          Height = 41
          Caption = 'SetValue As String'
          TabOrder = 0
          OnClick = Button2Click
        end
        object Memo2: TMemo
          Left = 97
          Top = 88
          Width = 185
          Height = 89
          Lines.Strings = (
            'Memo2')
          TabOrder = 1
        end
        object Edit3: TEdit
          Left = 97
          Top = 52
          Width = 121
          Height = 21
          TabOrder = 2
          Text = 'test'
        end
        object Button7: TButton
          Left = 336
          Top = 96
          Width = 145
          Height = 40
          Caption = 'GetValue AsString'
          TabOrder = 3
          OnClick = Button7Click
        end
        object Button8: TButton
          Left = 336
          Top = 52
          Width = 145
          Height = 44
          Caption = 'Is Value already Exists ?'
          TabOrder = 4
          OnClick = Button8Click
        end
      end
      object Button6: TButton
        Left = 207
        Top = 75
        Width = 151
        Height = 25
        Caption = 'Stress Data repo'
        TabOrder = 5
        OnClick = Button6Click
      end
    end
  end
  object TimerBusQuery: TTimer
    Interval = 10
    OnTimer = TimerBusQueryTimer
    Left = 72
    Top = 456
  end
  object TimerGUI: TTimer
    Interval = 250
    OnTimer = TimerGUITimer
    Left = 72
    Top = 392
  end
end
