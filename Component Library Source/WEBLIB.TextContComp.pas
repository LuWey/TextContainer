unit WEBLIB.TextContComp;

(*

  This TMS WEB Core component provides a place to store lengthy multiline
  text strings. The stored text can be interpreted as YAML, JSON or INI.

  This unit represents the "stub" component code faced towards the Delphi IDE.

  The component comes in handy in particular for multiline text, that otherwise
  would be written using a clumsy multiline string constant in '...'+ notation
  directly in the code.

  Typically, text stored here at design time contains configuration parameters
  of some kind. These parameters are often formatted as JSON, YAML on INI text.

  The component offers properties and functions to handle and convert between
  those formats, first of all being able to create a TJSValue, TJSObject or
  TJSArray from the stored text.

  Special design time feature:
  ----------------------------

  The preferred method to invoke the text editor dialog at design time is to
  just doubleclick or rightclick the component icon. This way, when the user
  resizes the edit dialog to have a better overview of the text entered,
  the dimensions of the resized editor dialog will also be stored persistently
  and next time the component is doubleclicked, the previous dimensions are
  restored.

*)

interface

uses
 Jsdelphisystem, System.SysUtils, System.Classes, JSON, WEBLIB.Controls, JS,
 WEBLib.MemINI;

type
  [ComponentPlatforms(TMSWebPlatform)]
  TTextCont=class(TComponent)
   private
    FLines: TStrings;
    FWidth: Integer;
    FHeight: Integer;
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
    function GetAsJSON: string;
    function GetAsYAML: string;
    procedure SetAsJSON(const Value: string);
    procedure SetAsYAML(const Value: string);
    function GetIsINIText: Boolean;
    function GetAsINI: TMemINI;
   protected
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

constructor TTextCont.Create(AOwner: TComponent);
begin
 inherited Create(AOwner);
 FLines := TStringList.Create;
end;

destructor TTextCont.Destroy;
begin
 FLines.Free;
 inherited;
end;

function TTextCont.GetAsJSArray: TJSArray;
begin
 Result := nil
end;

function TTextCont.GetAsJSObject: TJSObject;
begin
 Result := nil
end;

function TTextCont.GetAsJSON: string;
begin
 Result := '';
end;

function TTextCont.GetAsJSONStringFromYAML: string;
begin
 Result := '';
end;

function TTextCont.GetAsJSValue: TJSValue;
begin
 Result := nil
end;

function TTextCont.GetAsYAML: string;
begin
 Result := '';
end;

function TTextCont.GetAsYAMLStringFromJSON: string;
begin
 Result := '';
end;

function TTextCont.GetIsJSONText: Boolean;
begin
 Result := false;
end;

function TTextCont.GetIsYAMLText: Boolean;
begin
 Result := false;
end;

function TTextCont.GetText: string;
begin
 Result := '';
end;

procedure TTextCont.SetAsJSON(const Value: string);
begin
end;

procedure TTextCont.SetAsYAML(const Value: string);
begin
end;

procedure TTextCont.SetLines(const Value: TStrings);
begin
 FLines.SetStrings(Value);
end;

procedure TTextCont.SetText(const Value: string);
begin
end;

function TTextCont.GetIsINIText: Boolean;
begin
 Result := false;
end;

function TTextCont.GetAsINI: TMemINI;
begin
 Result := Nil;
end;

end.

