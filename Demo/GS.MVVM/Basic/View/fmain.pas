unit fmain;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.ExtCtrls, Vcl.StdCtrls,
  GS.MVVM;

type
  TForm1 = class(TForm)
    Edit1: TEdit;
    Label1: TLabel;
    LblReflect: TLabel;
    Label3: TLabel;
    Panel1: TPanel;
    LblReflectFromModel: TLabel;
    Label4: TLabel;
    Button1: TButton;
    Memo1: TMemo;
    LabelTime: TLabel;
    Label2: TLabel;
    Label5: TLabel;
    Label6: TLabel;
    procedure FormCreate(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure Edit1Change(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
    MyMVVMComponent : TGSMVVM;
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}



procedure TForm1.FormCreate(Sender: TObject);
begin
  MyMVVMComponent := TGSMVVM.Create;
  //First, we declare the controls Edit1 which are able to communicate with the MVVM system.
  MyMVVMComponent.Declare('MyMainForm', Edit1,'Text','MyMainEditOfMyForm');
  MyMVVMComponent.Declare('MyMainForm', LblReflect,'Caption','MyReflectLabelOfTheEdit');
  MyMVVMComponent.Declare('MyMainForm', LblReflectFromModel,'Caption','ReflectLabelForProcessingMainEdit');
  MyMVVMComponent.Declare('MyMainForm', Labeltime,'Caption','LabelToDisplayModel');
  MyMVVMComponent.Declare('MyMainForm', Memo1,'Text','MemoTextRessource');
  MyMVVMComponent.Declare('MyMainForm', Label2,'Caption','MyMainXPos');
  MyMVVMComponent.Declare('MyMainForm', Label5,'Caption','MyMainYPos');
  MyMVVMComponent.Declare('MyMainForm', Label6,'Caption','MyMainXYPos');

  //To be in a RAD spirit, as a code by design philosophy,
  //we ask what data we desire to connect to my MVVM ressource <aEditId>.
  // 1) Here we connect simply the edit value to the label.
  MyMVVMComponent.Link('MyMainEditOfMyForm','MyReflectLabelOfTheEdit');

  // 2) Here we want to process now the input. This process will be located in Model.
  //    -> We send the edit1 value to a MVVM ressource data called  "InputEngineString" (See dmData).
  MyMVVMComponent.Link('MyMainEditOfMyForm','InputEngineString');

  //    -> And we connect a label to the same MVVM ressource to reflect the result processing value processed in dmData.
  MyMVVMComponent.Link('InputEngineString','ReflectLabelForProcessingMainEdit');

  //And display hour, comming from dmData too.
  MyMVVMComponent.Link('MyModelTime','LabelToDisplayModel');


  //TODO --> Detect LOOP : !!!! For instance, only first level is detected such as. But it should detect path result...
  //MyMVVMComponent.Link('InputEngineString','MyMainEditOfMyForm'); //Uncomment this to leverage cool exception.
  ///

  //Mouse : Update directly some TLabedl : those data comes from a thread, which monitor mouse coord, located in dmdata.
  MyMVVMComponent.Link('MyMouseX','MyMainXPos');
  MyMVVMComponent.Link('MyMouseY','MyMainYPos');
  MyMVVMComponent.Link('MyMouseCoordAsString','MyMainXYPos');


end;

procedure TForm1.Edit1Change(Sender: TObject);
begin
  //GS.MVVM component is not threaded, or evented. They are *not* taking energy from a blackbox.
  //developer decide when it must be refreshed.
  //Refresh process touch only changed data.
  MyMVVMComponent.Activity(Sender);
end;


procedure TForm1.FormDestroy(Sender: TObject);
begin
  MyMVVMComponent.Free;
end;

procedure TForm1.Button1Click(Sender: TObject);
var l : TStringList;
begin
  l := TStringList(Memo1.Lines);
  GSMVVMEngine.GetChannelsConfigurationAsCSV(l);
end;

end.
