unit Unit1;

interface

uses
  System.SysUtils, System.Classes, JS, Web, WEBLib.Graphics, WEBLib.Controls,
  WEBLib.Forms, WEBLib.Dialogs, WEBLIB.TextContComp, Vcl.Controls, Vcl.StdCtrls,
  WEBLib.StdCtrls;

type
  TForm1 = class(TWebForm)
    WebButton1: TWebButton;
    TContINI: TTextCont;
    TContYAML: TTextCont;
    procedure WebButton1Click(Sender: TObject);
   private
   public
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

uses Dateutils,WEBLib.MemINI;

procedure TForm1.WebButton1Click(Sender: TObject);
var
 S : String;
 JO : TJSObject;
 INI : TMemINI;
begin
 Console.log('INI');
 Console.log('===');

 // Does the INI text container hold valid INI text?
 Console.log('Is INI: '+BoolToStr(TContINI.IsINIText,True));

 // Create an TMemINI instance from this INI text
 INI := TContINI.AsINI;

 // Change some data in the INI
 INI['foo','bar'] := 'lorem ipsum';
 INI.AsFloat['sci','fi'] := PI;
 Console.log('INI: '#13#10+INI.Text);

 // DateTime conversion
 Console.log('DateTime: '+DateTimeToStr(INI.AsDateTime['env:megaatmega2560','versionDate']));

 // Write the changed INI data back to the text container
 TContINI.Text := INI.Text;

 // Convert INI text into TJSObject
 JO := TContINI.AsTJSObject;
 Console.log('INI as TJSObject:');
 Console.log(JO);

 Console.log('YAML<->JSON');
 Console.log('===========');

 // Show the YAML text stored in the component
 Console.log(TContYAML.Text);

 // Check for valid YAML
 Console.log('Is YAML: '+BoolToStr(TContYAML.IsYAMLText,True));

 // Convert YAML text to JSON text
 S := TContYAML.YAMLtoJSON;
 Console.log('JSON: '+S);

 // Chech for valid JSON
 TContYAML.Text := S;
 Console.log('Is JSON: '+BoolToStr(TContYAML.IsJSONText,True));

 // Convert JSON text to js object
 JO := TContYAML.AsTJSObject;
 Console.log(JO);

 // Convert JSON text to YAML text
 S := TContYAML.JSONtoYAML;
 Console.log('YAML: '+S);

 // Check for valid YAML
 TContYAML.Text := S;
 Console.log('Is YAML: '+BoolToStr(TContYAML.IsYAMLText,True));

 // Convert YAML text to js object
 JO := TContYAML.AsTJSObject;
 Console.log('YAML as TJSObject:');
 Console.log(JO);
end;

end.
