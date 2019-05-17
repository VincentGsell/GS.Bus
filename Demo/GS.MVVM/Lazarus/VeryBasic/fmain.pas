unit fmain;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, GS.MVVM;

type

  { TForm1 }

  TForm1 = class(TForm)
    Edit1: TEdit;
    Label1: TLabel;
    procedure Edit1Change(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private

  public
    MVVM : TGSMVVM;
  end;

var
  Form1: TForm1;

implementation

{$R *.lfm}

{ TForm1 }

procedure TForm1.FormCreate(Sender: TObject);
begin
  MVVM := TGSMVVM.Create;
  MVVM.Declare('myView',Edit1,'Text','MainEdit');
  MVVM.Declare('myView',Label1,'Caption','TargetCaption');

  MVVM.Link('MainEdit','TargetCaption',AtoB);
end;

procedure TForm1.Edit1Change(Sender: TObject);
begin
  MVVM.Activity(Edit1);
end;

end.

