unit Unit1;

interface

uses
  System.SysUtils, System.Classes, JS, Web, WEBLib.Graphics, WEBLib.Controls,
  WEBLib.Forms, WEBLib.Dialogs, WEBLIB.TextContComp, Vcl.Controls, Vcl.StdCtrls,
  WEBLib.StdCtrls;

type
  TForm1 = class(TWebForm)
    WebButton1: TWebButton;
    TextCont1: TTextCont;
    procedure WebButton1Click(Sender: TObject);
   private
   public
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

uses Dateutils;

procedure TForm1.WebButton1Click(Sender: TObject);
var
 S : String;
 JO : TJSObject;
begin
 // Show the YAML text stored in the component
 Console.log(TextCont1.Text);

 // Check for valid YAML
 Console.log('Is YAML: '+BoolToStr(TextCont1.IsYAMLText,True));

 // Convert YAML text to JSON text
 S := TextCont1.YAMLtoJSON;
 Console.log('JSON: '+S);

 // Chech for valid JSON
 TextCont1.Text := S;
 Console.log('Is JSON: '+BoolToStr(TextCont1.IsJSONText,True));

 // Convert JSON text to js object
 JO := TextCont1.AsTJSObject;
 Console.log(JO);

 // Convert JSON text to YAML text
 S := TextCont1.JSONtoYAML;
 Console.log('YAML: '+S);

 // Check for valid YAML
 TextCont1.Text := S;
 Console.log('Is YAML: '+BoolToStr(TextCont1.IsYAMLText,True));

 // Convert YAML text to js object
 JO := TextCont1.AsTJSObject;
 Console.log(JO);

end;

end.
