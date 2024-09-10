unit WEBLib.TextContComp;

(*

  This TMS WEB Core component provides a place to store lengthy multiline
  text strings. The stored text can be interpreted as YAML, JSON or INI.

  This unit represents the "core source" component code used by pas2js.

  The component comes in handy in particular for multiline text, that otherwise
  would be written using a clumsy multiline string constant in '...'+ notation
  directly in the code.

  Typically, text stored here at design time contains configuration parameters
  of some kind. These parameters are often formatted as JSON, YAML on INI text.

  The component offers properties and functions to handle and convert between
  those formats, first of all being able to create a TJSValue, TJSObject or
  TJSArray from the stored text.

*)

interface

uses
 jsdelphisystem, System.SysUtils, System.Classes, WEBLib.ExtCtrls,
 WEBLib.Controls, WEBLib.REST, JS, WEBLib.MemINI;

type
  TTextCont=class(TComponent)
   private
    FLines  : TStrings;
    FWidth  : Integer;
    FHeight : Integer;
    procedure SetLines(const Value: TStrings);
    function GetText: string;
    procedure SetText(const Value: string);
    function GetAsJSValue: TJSValue;
    function GetAsJSArray: TJSArray;
    function GetAsJSObject: TJSObject;
    function GetIsJSONText: Boolean;
    function GetIsYAMLText: Boolean;
    function GetAsJSONStringFromYAML: string;
    function GetAsYAMLStringFromJSON: string;
    function CheckIsJSON(var JV: TJSValue; DoFail : Boolean): Boolean;
    function CheckIsYAML(var JV: TJSValue; DoFail : Boolean): Boolean;
    function CheckIsINI(var JV: TJSValue; DoFail: Boolean): Boolean;
    function GetAsJSON: string;
    function GetAsYAML: string;
    procedure SetAsJSON(const Value: string);
    procedure SetAsYAML(const Value: string);
    function GetIsINIText: Boolean;
    function GetAsINI: TMemINI;
   protected
    procedure Loaded; override;
   public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    // Shorts for Lines.Text
    property Text : string read GetText write SetText;
    property AsString : string read GetText write SetText;

    // Assumes the text is JSON or YAML and returns a TJSValue, a
    // TJSObject or TJSArray. Fails wit a generic exception when
    // the conversion fails or the text is empty.
    property AsTJSValue: TJSValue read GetAsJSValue;
    property AsTJSObject: TJSObject read GetAsJSObject;
    property AsTJSArray: TJSArray read GetAsJSArray;

    // Assumes the text is YAML text and excepts with a detailed exception
    // message if not. Empty text is allowed.
    property AsYAML : string read GetAsYAML write SetAsYAML;

    // Assumes the text is JSON text and excepts with a detailed exception
    // message if not. Empty text is allowed.
    property AsJSON : string read GetAsJSON write SetAsJSON;

    // Assumes the text is in INI format and excepts with a detailed exception
    // message if not. Empty text is allowed. To set an INI text, use the
    // Text property. The property returns a TWebMemINI instance to further
    // work with. Remember to store the Text of the instance back in this
    // component after making changes.
    property AsINI : TMemINI read GetAsINI;

    // Assumes the text is YAML and converts into a formatted JSON string.
    // Excepts on conversion error.
    property YAMLtoJSON: string read GetAsJSONStringFromYAML;

    // Assumes the text is JSON and converts into a formatted YAML string.
    // Excepts on conversion error.
    property JSONtoYAML: string read GetAsYAMLStringFromJSON;

    // Check if the text can be interpreted as JSON, YAML or INI.
    // Return False if the text is empty. Do not except.
    property IsYAMLText: Boolean read GetIsYAMLText;
    property IsJSONText: Boolean read GetIsJSONText;
    property IsINIText: Boolean read GetIsINIText;

   published
    // The TStrings in which the text is stored
    property Lines: TStrings read FLines write SetLines;

    // This component remembers the last dimensions from the edit window
    // of the object inspector.
    property Width: Integer read FWidth write FWidth;
    property Height: Integer read FHeight write FHeight;
  end;


 {##############################################################################}

implementation

uses WEBLib.Yaml;

constructor TTextCont.Create(AOwner: TComponent);
begin
 inherited Create(AOwner);
 FLines := TStringList.Create;
end;

{-------------------------------}

destructor TTextCont.Destroy;
begin
 FLines.Free;
 inherited;
end;

{-------------------------------}

procedure TTextCont.Loaded;
begin
 inherited;
end;

{-------------------------------}

procedure TTextCont.SetAsJSON(const Value: string);
var S : String;
begin
 S := trim(Value);
 if S='' then
  begin
   FLines.Clear;
   exit;
  end;

 TJSJSON.parse(S);
 FLines.text := S;
end;

{-------------------------------}

procedure TTextCont.SetAsYAML(const Value: string);
var S : String;
begin
 S := trim(Value);
 if S='' then
  begin
   FLines.Clear;
   exit;
  end;

 TYamlUtils.YamlToJson(FLines);
 FLines.text := S;
end;

{-------------------------------}

procedure TTextCont.SetLines(const Value: TStrings);
begin
 FLines.Free;
 FLines := Value;
end;

{-------------------------------}

function TTextCont.GetAsJSArray: TJSArray;
var JV : TJSValue;
begin
 JV := GetAsJSValue;
 if not isArray(JV) then raise Exception.Create('JSON is not an array');
 Result := toArray(JV);
end;

{-------------------------------}

function TTextCont.GetAsJSObject: TJSObject;
var JV : TJSValue;
begin
 JV := GetAsJSValue;
 if not isObject(JV) then raise Exception.Create('JSON is not an object');
 Result := toObject(JV);
end;

{-------------------------------}

function TTextCont.GetAsJSValue: TJSValue;
begin
 if CheckIsJSON(Result,false) then exit;
 if CheckIsYAML(Result,false) then exit;
 if CheckIsINI(Result,false) then exit;
 Raise Exception.Create('Not convertable to TJSValue');
end;

{-------------------------------}

function TTextCont.GetAsYAML: string;
begin
 Result := FLines.Text.Trim;
 if Result<>'' then
  TYamlUtils.YamlToJson(Result);
end;

{-------------------------------}

function TTextCont.GetAsJSON: string;
begin
 Result := FLines.Text.Trim;
 if Result<>'' then
  TJSJSON.parse(Result);
end;

{-------------------------------}

function TTextCont.GetAsYAMLStringFromJSON: string;
begin
 Result := TYamlUtils.JsonToYaml(FLines.Text);
end;

{-------------------------------}

function TTextCont.GetAsJSONStringFromYAML: string;
begin
 Result := TYamlUtils.YamlToJson(FLines);
end;

{-------------------------------}

function TTextCont.CheckIsJSON(var JV : TJSValue; DoFail : Boolean): Boolean;
var S : String;
begin
 Result := false;
 JV := nil;
 S := FLines.Text.Trim;
 if S='' then exit;
 try
  JV := TJSJSON.parse(S);
  Result := true;
 except
  JV := nil;
  if DoFail then raise;
 end;
End;

{-------------------------------}

function TTextCont.CheckIsYAML(var JV : TJSValue; DoFail : Boolean): Boolean;
var S : String;
begin
 Result := false;
 JV := nil;
 S := FLines.Text.Trim;
 if S='' then exit;
 try
  S := TYamlUtils.YamlToJson(S);
  JV := TJSJSON.parse(S);
  Result := true;
 except
  JV := nil;
  if DoFail then raise;
 end;
End;

{-------------------------------}

function TTextCont.CheckIsINI(var JV : TJSValue; DoFail : Boolean): Boolean;
var
 S   : String;
 INI : TMemINI;
begin
 Result := false;
 JV := nil;
 S := FLines.Text.Trim;
 if S='' then exit;
 try
  INI := TMemINI.Create(S);
  JV := INI.AsJSObject;
  Result := true;
 except
  JV := nil;
  if DoFail then raise;
 end;
End;

{-------------------------------}

function TTextCont.GetIsJSONText: Boolean;
var JV : TJSValue;
begin
 Result := CheckIsJSON(JV,false);
end;

{-------------------------------}

function TTextCont.GetIsYAMLText: Boolean;
var JV: TJSValue;
begin
 Result := CheckIsYAML(JV,false);
end;

{-------------------------------}

function TTextCont.GetText: string;
begin
 Result := FLines.Text;
end;

{-------------------------------}

procedure TTextCont.SetText(const Value: string);
begin
 FLines.Text := Value;
end;

{-------------------------------}

function TTextCont.GetIsINIText: Boolean;
begin
 Result := TMemINI.IsINIText(FLines.Text);
end;

{-------------------------------}

function TTextCont.GetAsINI: TMemINI;
begin
 Result := TMemINI.Create(FLines.Text.Trim)
end;

{-------------------------------}

end.
