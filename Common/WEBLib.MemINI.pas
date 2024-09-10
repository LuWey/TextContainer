unit WEBLib.MemINI;

interface

uses System.Classes, System.SysUtils, JS;

type
  EINISectionNotFound = class(Exception);
  EINISectionEmpty = class(Exception);
  EINIKeyNotFound = class(Exception);
  EINIKeyEmpty = class(Exception);
  EINIDanglingItem = class(Exception);
  TMemINI=class
   private
    type
     TINIValue=class
     private
      FVal: string;
      FLineNo: Integer;
     public
      constructor Create(Val: string; LineNo: Integer);
     end;
     TINISection=class
     private
      FKVL: TStringList;
      FLineNo: Integer;
     public
      constructor Create(LineNo: Integer; CaseSensitive: Boolean);
      destructor Destroy; override;
     end;
   private
    FItems: TStringList;
    FSections: TStringList;
    FUseLocale: Boolean;
    FCaseSens: Boolean;
    FFormSets: TFormatSettings;
    function GetText: string;
    procedure SetText(const Value: string);
    function GetCaseSensitive: Boolean;
    procedure SetCaseSensitive(const Value: Boolean);
    function GetUseLocale: Boolean;
    procedure SetUseLocale(const Value: Boolean);
    function GetAsString(Section, Key: string): string;
    procedure SetAsString(Section, Key: string; const Value: string);
    function GetValue(Section, Key: string; DoFail: Boolean): TINIValue;
    function GetAsInteger(Section, Key: string): Integer;
    procedure SetAsInteger(Section, Key: string; const Value: Integer);
    function GetAsExtended(Section, Key: string): Extended;
    procedure SetAsExtended(Section, Key: string; const Value: Extended);
    function GetAsInt64(Section, Key: string): Int64;
    procedure SetAsInt64(Section, Key: string; const Value: Int64);
    function GetAsBoolean(Section, Key: string): Boolean;
    procedure SetAsBoolean(Section, Key: string; const Value: Boolean);
    function GetAsDate(Section, Key: string): TDate;
    function GetAsDateTime(Section, Key: string): TDateTime;
    function GetAsTime(Section, Key: string): TTime;
    procedure SetAsDate(Section, Key: string; const Value: TDate);
    procedure SetAsTime(Section, Key: string; const Value: TTime);
    procedure SetAsDateTime(Section, Key: string; const Value: TDateTime);
    function MaxSectionItemLine(Sec: TINISection): Integer;
    function GetLine(LineNo: Integer): string;
    function GetLineCount: Integer;
    function GetKeyLineNo(Section, Key: string): Integer;
    function GetSectionLineNo(Section, Key: string): Integer;
    function GetAsJSObject: TJSObject;
   public
    constructor Create(INIText: string; CaseSensitive: Boolean=False; UseLocale: Boolean=True);
    destructor Destroy; override;

    procedure Clear;
    procedure DeleteKey(Section, Key: string);
    procedure EraseSection(Section: string);
    procedure ReadSection(Section: string; Strings: TStrings);
    procedure ReadSections(Strings: TStrings);
    procedure ReadSectionValues(Section: string; Strings: TStrings);
    function ReadString(Section, Ident, Default: string): string;
    function SectionExists(Section: string): Boolean;
    function ValueExists(Section, Ident: string): Boolean;
    procedure WriteString(Section, Ident, Value: string);
    class function IsINIText(Text : String) : Boolean;

    property Text: string read GetText write SetText;
    property LineCount : Integer read GetLineCount;
    property Line[LineNo : Integer]: string read GetLine;
    property SectionLineNo[Section, Key: string]: Integer read GetSectionLineNo;
    property KeyLineNo[Section, Key: string]: Integer read GetKeyLineNo;

    property CaseSensitive: Boolean read GetCaseSensitive write SetCaseSensitive;
    property UseLocale: Boolean read GetUseLocale write SetUseLocale;
    property FormatSettings: TFormatSettings read FFormSets write FFormSets;

    property AsString[Section, Key: string]: string read GetAsString write SetAsString; default;
    property AsInteger[Section, Key: string]: Integer read GetAsInteger write SetAsInteger;
    property AsInt64[Section, Key: string]: Int64 read GetAsInt64 write SetAsInt64;
    property AsFloat[Section, Key: string]: Extended read GetAsExtended write SetAsExtended;
    property AsBoolean[Section, Key: string]: Boolean read GetAsBoolean write SetAsBoolean;
    property AsDate[Section, Key: string]: TDate read GetAsDate write SetAsDate;
    property AsTime[Section, Key: string]: TTime read GetAsTime write SetAsTime;
    property AsDateTime[Section, Key: string]: TDateTime read GetAsDateTime write SetAsDateTime;
    property AsJSObject: TJSObject read GetAsJSObject;

    class function ClassTest : Boolean;
  end;

 {##############################################################################}

implementation

{ TMemINI }

uses System.Types, System.DateUtils, Web;

{ TMemINI.TINIValue }

constructor TMemINI.TINIValue.Create(Val: string; LineNo: Integer);
begin
 FVal := Val;
 FLineNo := LineNo;
end;

{ TMemINI.TINISection }

constructor TMemINI.TINISection.Create(LineNo: Integer; CaseSensitive: Boolean);
begin
 FLineNo := LineNo;
 FKVL := TStringList.Create;
 FKVL.Sorted := true;
 FKVL.Duplicates := dupError;
 FKVL.OwnsObjects := true;
 FKVL.CaseSensitive := CaseSensitive;
end;

{ TMemINI }

destructor TMemINI.TINISection.Destroy;
begin
 FKVL.Free;
end;

constructor TMemINI.Create(INIText: string; CaseSensitive: Boolean=False;
 UseLocale: Boolean=True);
begin
 inherited Create;
 FItems := TStringList.Create;
 FUseLocale := UseLocale;
 FCaseSens := CaseSensitive;
 FSections := TStringList.Create;
 FSections.Sorted := true;
 FSections.Duplicates := dupError;
 FSections.OwnsObjects := true;
 FSections.CaseSensitive := CaseSensitive;
 FFormSets := TFormatSettings.Create('en-US');
 if INIText<>'' then SetText(INIText);
end;

{-------------------------------}

destructor TMemINI.Destroy;
begin
 FSections.Free;
 FItems.Free;
 inherited Destroy;
end;

{-------------------------------}

procedure TMemINI.Clear;
begin
 FSections.Clear;
 FItems.Clear;
end;

{-------------------------------}

function TMemINI.GetText: string;
begin
 Result := FItems.Text;
end;

procedure TMemINI.SetText(const Value: string);
var
 I, P: Integer;
 S, SecName,
 Key, Val: string;
 Sec: TINISection;
 INIVal: TINIValue;
begin
 FSections.Clear;
 FItems.Clear;
 if Value.Trim='' then exit;

 FItems.Text := Value;
 FItems.Text := FItems.Text;
 Sec := nil;
 for I := 0 to FItems.Count-1 do
  begin
   S := FItems[I];
   if (S='') or S.StartsWith('#') or S.StartsWith(';') then continue;
   if S.StartsWith('[') and S.EndsWith(']') then
    begin
     Sec := TINISection.Create(I, FCaseSens);
     SecName := Copy(S, 2, Length(S)-2);
     FSections.AddObject(SecName, Sec);
    end
   else
    begin
     if Sec=nil then raise EINIDanglingItem.Create('Item without section in line '+I.ToString);
     P := Pos('=', S);
     if P<1 then
      begin
       // If there is no "=" that separates key from value, then value is set to key
       Key := S;
       Val := S;
      end
     else
      begin
       Key := Copy(S, 1, P-1);
       Val := Copy(S, P+1, Length(S)-P);
      end;
     INIVal := TINIValue.Create(Val, I);
     Sec.FKVL.AddObject(Key, INIVal);
    end;
  end;
end;

{-------------------------------}

function TMemINI.GetCaseSensitive: Boolean;
begin
 Result := FSections.CaseSensitive;
end;

procedure TMemINI.SetCaseSensitive(const Value: Boolean);
var I: Integer;
begin
 if FSections.CaseSensitive<>Value then
  begin
   FSections.CaseSensitive := Value;
   for I := 0 to FSections.Count-1 do
    (FSections.Objects[I] as TStringList).CaseSensitive := Value;
  end;
end;

{-------------------------------}

function TMemINI.GetUseLocale: Boolean;
begin
 Result := FUseLocale;
end;

procedure TMemINI.SetUseLocale(const Value: Boolean);
begin
 if FUseLocale<>Value then
  FUseLocale := Value;
end;

{-------------------------------}

function TMemINI.GetValue(Section, Key: string; DoFail: Boolean): TINIValue;
var
 Ind: Integer;
 Sec: TINISection;
begin
 Result := nil;

 Section := Trim(Section);
 Key := Trim(Key);
 if Section='' then
  if DoFail then raise EINISectionEmpty.Create('Section is empty')
  else exit;

 if Key='' then
  if DoFail then raise EINIKeyEmpty.Create('Key is empty')
  else exit;

 if not FSections.Find(Section, Ind) then
  if DoFail then raise EINISectionNotFound.Create('Section not found: "'+Section+'"')
  else exit;

 Sec := FSections.Objects[Ind] as TINISection;
 if not Sec.FKVL.Find(Key, Ind) then
  if DoFail then raise EINIKeyNotFound.Create('Key "'+Key+'" not found in section: "'+Section+'"')
  else exit;

 Result := Sec.FKVL.Objects[Ind] as TINIValue;
end;

{-------------------------------}

function TMemINI.GetAsString(Section, Key: string): string;
begin
 Result := GetValue(Section, Key, true).FVal;
end;

procedure TMemINI.SetAsString(Section, Key: string; const Value: string);
var
 Ind,
 LineNo: Integer;
 S: string;
 Sec: TINISection;
begin
 Section := Trim(Section);
 Key := Trim(Key);
 if Section='' then raise EINISectionEmpty.Create('Section is empty');
 if Key='' then raise EINIKeyEmpty.Create('Key is empty');

 if not FSections.Find(Section, Ind) then
  begin
   S := FItems.Text.Trim+#13#10+
   #13#10+
   '['+Section+']'+#13#10+
   Key+'='+Value;
   SetText(S);
   Exit;
  end;

 Sec := FSections.Objects[Ind] as TINISection;
 if not Sec.FKVL.Find(Key, Ind) then
  begin
   LineNo := Sec.FLineNo;
   FItems.Insert(LineNo+1, Key+'='+Value);
   S := FItems.Text.Trim;
   SetText(S);
   Exit;
  end;

 // Only write key if key and value are equal
 if Key=Value then S := Key
 else S := Key+'='+Value;

 FItems[(Sec.FKVL.Objects[Ind] as TINIValue).FLineNo] := S;
 S := FItems.Text.Trim;
 SetText(S);
end;

{-------------------------------}

procedure TMemINI.DeleteKey(Section, Key: string);
var
 Ind: Integer;
 Sec: TINISection;
 LNo: Integer;
begin
 if not FSections.Find(Section, Ind) then exit;
 Sec := FSections.Objects[Ind] as TINISection;
 if not Sec.FKVL.Find(Key, Ind) then exit;
 LNo := (Sec.FKVL.Objects[Ind] as TINIValue).FLineNo;
 FItems.Delete(LNo);
 SetText(FItems.Text.Trim);
end;

{-------------------------------}

function TMemINI.MaxSectionItemLine(Sec: TINISection) : Integer;
var
 Ind: Integer;
 LNo: Integer;
begin
 Result := -1;
 if Sec.FKVL.Count<1 then exit;
 for Ind := 0 to Sec.FKVL.Count-1 do
  begin
   LNo := (Sec.FKVL.Objects[Ind] as TINIValue).FLineNo;
   if LNo>Result then Result := LNo;
  end;
end;

{-------------------------------}

procedure TMemINI.EraseSection(Section: string);
var
 Ind,
 Min,
 Max : Integer;
 Sec : TINISection;
begin
 if not FSections.Find(Section, Ind) then exit;
 Sec := FSections.Objects[Ind] as TINISection;
 Min := Sec.FLineNo;
 Max := MaxSectionItemLine(Sec);
 if Max>0 then
  for Ind := Max downto Min do
   FItems.Delete(Ind)
 else
  FItems.Delete(Min);
 SetText(FItems.Text.Trim);
end;

{-------------------------------}

procedure TMemINI.ReadSection(Section: string; Strings: TStrings);
var
 Ind: Integer;
 Sec: TINISection;
 LNo: Integer;
begin
 if not FSections.Find(Section, Ind) then
  raise EINISectionNotFound.Create('Section not found: "'+Section+'"');
 Strings.Clear;
 Sec := FSections.Objects[Ind] as TINISection;
 for Ind := 0 to Sec.FKVL.Count-1 do
  begin
   LNo := (Sec.FKVL.Objects[Ind] as TINIValue).FLineNo;
   Strings.Add(FItems[LNo]);
  end;
end;

{-------------------------------}

procedure TMemINI.ReadSections(Strings: TStrings);
var Ind: Integer;
begin
 Strings.Clear;
 if FSections.Count<1 then exit;
 for Ind := 0 to FSections.Count-1 do
  Strings.Add(FSections[Ind]);
end;

{-------------------------------}

procedure TMemINI.ReadSectionValues(Section: string; Strings: TStrings);
var
 Ind: Integer;
 Sec: TINISection;
begin
 Strings.Clear;

 Section := Trim(Section);
 if Section='' then raise EINISectionEmpty.Create('Section is empty');

 if not FSections.Find(Section, Ind) then
  raise EINISectionNotFound.Create('Section not found: "'+Section+'"');

 Sec := FSections.Objects[Ind] as TINISection;
 for Ind := 0 to Sec.FKVL.Count-1 do
  Strings.Add((Sec.FKVL.Objects[Ind] as TINIValue).FVal);
end;

{-------------------------------}

function TMemINI.ReadString(Section, Ident, Default: string): string;
var IVal: TINIValue;
begin
 IVal := GetValue(Section, Ident, False);
 if IVal<>nil then Result := IVal.FVal
 else Result := Default;
end;

{-------------------------------}

function TMemINI.SectionExists(Section: string): Boolean;
var Ind: Integer;
begin
 Section := Trim(Section);
 if Section='' then exit(false);
 Result := FSections.Find(Section, Ind);
end;

{-------------------------------}

function TMemINI.ValueExists(Section, Ident: string): Boolean;
begin
 Result := GetValue(Section, Ident, False)<>nil;
end;

{-------------------------------}

procedure TMemINI.WriteString(Section, Ident, Value: string);
begin
 SetAsString(Section, Ident, Value);
end;

{-------------------------------}

function TMemINI.GetAsInteger(Section, Key: string): Integer;
begin
 Result := GetAsString(Section, Key).ToInteger;
end;

procedure TMemINI.SetAsInteger(Section, Key: string; const Value: Integer);
begin
 SetAsString(Section, Key, Value.ToString);
end;

{-------------------------------}

function TMemINI.GetAsInt64(Section, Key: string): Int64;
begin
 Result := StrToInt64(GetAsString(Section, Key));
end;

procedure TMemINI.SetAsInt64(Section, Key: string; const Value: Int64);
begin
 SetAsString(Section, Key, Value.ToString);
end;

{-------------------------------}

function TMemINI.GetAsExtended(Section, Key: string): Extended;
var Val: string;
begin
 Val := GetAsString(Section, Key);
 Result := StrToFloat(Val, FFormSets);
end;

procedure TMemINI.SetAsExtended(Section, Key: string; const Value: Extended);
begin
 SetAsString(Section, Key, FloatToStr(Value, FFormSets));
end;

{-------------------------------}

function TMemINI.GetAsBoolean(Section, Key: string): Boolean;
begin
 Result := StrToBool(GetAsString(Section, Key));
end;

procedure TMemINI.SetAsBoolean(Section, Key: string; const Value: Boolean);
begin
 SetAsString(Section, Key, BoolToStr(Value, True));
end;

{-------------------------------}

function TMemINI.GetAsDate(Section, Key: string): TDate;
begin
 Result := Trunc(GetAsDateTime(Section, Key));
end;

procedure TMemINI.SetAsDate(Section, Key: string; const Value: TDate);
begin
 {$IFDEF PAS2JS}
 SetAsString(Section, Key, DateToRFC3339(Trunc(Value)));
 {$ENDIF}
end;

{-------------------------------}

function TMemINI.GetAsTime(Section, Key: string): TTime;
begin
 Result := Frac(GetAsDateTime(Section, Key));
end;

procedure TMemINI.SetAsTime(Section, Key: string; const Value: TTime);
begin
 {$IFDEF PAS2JS}
 SetAsString(Section, Key, TimeToRFC3339(Frac(Value)));
 {$ENDIF}
end;

{-------------------------------}

function TMemINI.GetAsDateTime(Section, Key: string): TDateTime;
var
 TimeOnly,
 DateOnly : Boolean;
 Val      : String;
 P        : Integer;
begin
 Result := 0;
 Val := GetAsString(Section, Key);
 P := Pos(':',Val);
 TimeOnly := (P=3);
 DateOnly := (P=0);
 if TimeOnly then Val := '2000-01-01T' + Val + 'Z';
 if DateOnly then Val := Val + 'T00:00:00.000Z';
 {$IFDEF PAS2JS}
 Result := RFC3339ToDateTime(Val);
 {$ENDIF}
 if TimeOnly then Result := Frac(Result);
 if DateOnly then Result := Trunc(Result);
end;

{-------------------------------}

procedure TMemINI.SetAsDateTime(Section, Key: string; const Value: TDateTime);
begin
 {$IFDEF PAS2JS}
 SetAsString(Section, Key, DateTimeToRFC3339(Value));
 {$ENDIF}
end;

{-------------------------------}

function TMemINI.GetLine(LineNo: Integer): string;
begin
 Result := FItems[LineNo];
end;

{-------------------------------}

function TMemINI.GetLineCount: Integer;
begin
 Result := FItems.Count;
end;

{-------------------------------}

function TMemINI.GetKeyLineNo(Section, Key: string): Integer;
begin
 Result := GetValue(Section, Key, true).FLineNo;
end;

{-------------------------------}

function TMemINI.GetSectionLineNo(Section, Key: string): Integer;
var
 Ind: Integer;
 Sec: TINISection;
begin
 Section := Trim(Section);
 if Section='' then raise EINISectionEmpty.Create('Section is empty');

 if not FSections.Find(Section, Ind) then
  raise EINISectionNotFound.Create('Section not found: "'+Section+'"');

 Sec := FSections.Objects[Ind] as TINISection;
 Result := Sec.FLineNo;
end;

{-------------------------------}

class function TMemINI.IsINIText(Text: String): Boolean;
var
 INI : TMemINI;
 SL1,
 SL2 : TStringList;
 S   : String;
begin
 Result := false;
 if Text.Trim='' then exit;
 SL1 := TStringList.Create;
 SL2 := TStringList.Create;
 INI := Nil;
 try
  try
   INI := TMemINI.Create(Text);
   // If there is not at least one section in the text, it's not an INI
   INI.ReadSections(SL1);
   if SL1.Count<1 then exit;
   // If there is no section that has at least 1 item, it's not an INI
   for S in SL1 do
    begin
     INI.ReadSection(S,SL2);
     if SL2.Count>0 then exit(true);
    end;
  except
   // In case of any error, it's not an INI
  end;
 finally
  INI.Free;
  SL1.Free;
  SL2.Free;
 end;
end;

{-------------------------------}

function TMemINI.GetAsJSObject: TJSObject;
var
 I, K             : Integer;
 SecNam, Key, Val : String;
 Sec              : TINISection;
 SecJO            : TJSObject;
 IV               : TINIValue;
Begin
 Result := TJSObject.new;
 for I := 0 to FSections.Count-1 do
  Begin
   SecNam := FSections[I];
   SecJO  := TJSObject.new;
   Sec    := FSections.Objects[I] as TINISection;
   for K := 0 to Sec.FKVL.Count-1 do
    begin
     Key := Sec.FKVL[K];
     IV  := Sec.FKVL.Objects[K] as TINIValue;
     Val := IV.FVal;
     SecJO[Key] := Val;
    end;
   Result[SecNam] := SecJO;
  End;
end;

{-------------------------------}

{$IFDEF DEBUG}
Const
 INITestData =
  '[General]'                                       +#13#10+
  'allowThirdPersonPlayer=0'                        +#13#10+
  '; This sets it on'                               +#13#10+
  'AllowCaveBuildingPvE=0'                          +#13#10+
  'alwaysNotifyPlayerJoined=0'                      +#13#10+
  'alwaysNotifyPlayerLeft=0'                        +#13#10+
  'bAllowFlyerCarryPvE=0'                           +#13#10+
  'bDisableStructureDecayPvE=0'                     +#13#10+
  'DayCycleSpeedScale=1.00000'                      +#13#10+
  'DayTimeSpeedScale=1.00000'                       +#13#10+
  'NightTimeSpeedScale=1.00000'                     +#13#10+
  'DinoCharacterFoodDrainMultiplier=1.00000'        +#13#10+
  'DinoCharacterHealthRecoveryMultiplier=1.00000'   +#13#10+
  'DinoCharacterStaminaDrainMultiplier=1.00000'     +#13#10+
  '; These are disable'                             +#13#10+
  '; DinoDamageMultiplier=1.00000'                  +#13#10+
  '; DinoResistanceMultiplier=1.00000'              +#13#10+
  '#globalVoiceChat=0'                              +#13#10+
  ''                                                +#13#10+
  '[ServerSettings]'                                +#13#10+
  'HarvestAmountMultiplier=1.00000'                 +#13#10+
  'HarvestHealthMultiplier=1.00000'                 +#13#10+
  'MaxStructuresInRange=6700'                       +#13#10+
  'noTributeDownloads=0'                            +#13#10+
  'PreventDownloadSurvivors=0'                      +#13#10+
  'PreventDownloadItems=0'                          +#13#10+
  'PreventDownloadDinos=0'                          +#13#10+
  'PlayerCharacterFoodDrainMultiplier=1.00000'      +#13#10+
  'PlayerCharacterHealthRecoveryMultiplier=1.00000' +#13#10+
  'PlayerCharacterStaminaDrainMultiplier=1.00000'   +#13#10+
  'PlayerCharacterWaterDrainMultiplier=1.00000'     +#13#10+
  'PlayerDamageMultiplier=1.00000'                  +#13#10+
  'PlayerResistanceMultiplier=1.00000'              +#13#10+
  'proximityChat=0'                                 +#13#10+
  'ResourceNoReplenishRadiusPlayers=1.00000'        +#13#10+
  'ResourceNoReplenishRadiusStructures=1.00000'     +#13#10+
  'ResourcesRespawnPeriodMultiplier=1.00000'        +#13#10+
  ''                                                +#13#10+
  '[LanguageTest]'                                  +#13#10+
  'ServerAdminPassword='                            +#13#10+
  'ServerMüßigkeit=22'                              +#13#10+
  'Änderungsbedürftigkeit=true'                     +#13#10+
  '创建日期=2024-03-12T18:22:46Z'                   +#13#10+
  'serverHardcore=0'                                +#13#10+
  'ServerPassword=NotGonnaTellYa'                   +#13#10+
  'ValueIsKey'                                      +#13#10+
  ''                                                +#13#10+
  'ShowMapPlayerLocation=0'                         +#13#10+
  'StructureDamageMultiplier=1.00000'               +#13#10+
  'StructureResistanceMultiplier=1.00000'           +#13#10+
  'TamedDinoDamageMultiplier=1.00000'               +#13#10+
  'TamedDinoResistanceMultiplier=1.00000'           +#13#10+
  'TamingSpeedMultiplier=1.00000'                   +#13#10+
  'XPMultiplier=1.00000'                            +#13#10+
  ''                                                +#13#10+
  '[CPU]'                                           +#13#10+
  'EnablePVPGamma=0'                                +#13#10+
  'EnablePVEGamma=0'                                +#13#10+
  'SpectatorPassword='                              +#13#10+
  'DifficultyOffset=0.50000'                        +#13#10+
  'PvEStructureDecayPeriodMultiplier=1.00000'       +#13#10+
  'PvEStructureDecayDestructionPeriod=1.00000'      +#13#10+
  'Banlist="http://arkdedicated.com/banlist.txt"'   +#13#10+
  'PvPStructureDecay=0'                             +#13#10+
  'DisableDinoDecayPvE=0'                           +#13#10+
  'PvEDinoDecayPeriodMultiplier=1.00000'            +#13#10+
  'AdminLogging=0'                                  +#13#10+
  'MaxTamedDinos=8000'                              +#13#10+
  'MaxNumbersofPlayersInTribe=2'                    +#13#10+
  'BattleNumOfTribestoStartGame=2'                  +#13#10+
  'TimeToCollapseROD=100'                           +#13#10+
  'BattleAutoStartGameInterval=100'                 +#13#10+
  'BattleSuddenDeathInterval=300'                   +#13#10+
  'KickIdlePlayersPeriod=1800'                      +#13#10+
  'PerPlatformMaxStructuresMultiplier=1.00000'      +#13#10+
  'StructureDamageRepairCooldown=180'               +#13#10+
  'bForceAllStructureLocking=1'                     +#13#10+
  'AutoDestroyOldStructuresMultiplier=0.00000'      +#13#10+
  'bUseVSync=0'                                     +#13#10+
  'MaxPlatformSaddleStructureLimit=100'             +#13#10+
  'bPassiveDefensesDamageRiderlessDinos=1'          +#13#10+
  'RCONPort=27020'                                  +#13#10+
  'AutoSavePeriodMinutes=20'                        +#13#10+
  'RCONServerGameLogBuffer=600'                     +#13#10+
  'OverrideStructurePlatformPrevention=0'           +#13#10+
  'PreventOfflinePvPInterval=60.0'                  +#13#10+
  'bPvPDinoDecay=1'                                 +#13#10+
  'bPvPStructureDecay=1'                            +#13#10+
  'DisableImprintDinoBuff=1'                        +#13#10+
  'AllowAnyoneBabyImprintCuddle=1'                  +#13#10+
  'EnableExtraStructurePreventionVolumes=1'         +#13#10+
  'ShowFloatingDamageText=1'                        +#13#10+
  'DestroyUnconnectedWaterPipes=0'                  +#13#10+
  'OverrideOfficialDifficulty=1.0'                  +#13#10+
  'TheMaxStructuresInRange=10500'                   +#13#10+
  'MinimumDinoReuploadInterval=0'                   +#13#10+
  'PvEAllowStructuresAtSupplyDrops=0'               +#13#10+
  'NPCNetworkStasisRangeScalePlayerCountStart=70'   +#13#10+
  'NPCNetworkStasisRangeScalePlayerCountEnd=120'    +#13#10+
  'NPCNetworkStasisRangeScalePercentEnd=0.50'       +#13#10+
  'MaxPersonalTamedDinos=500'                       +#13#10+
  'AutoDestroyDecayedDinos=1'                       +#13#10+
  ''                                                +#13#10+
  '[Oranization]'                                   +#13#10+
  'ClampItemSpoilingTimes=0'                        +#13#10+
  'UseOptimizedHarvestingHealth=1'                  +#13#10+
  'AllowCrateSpawnsOnTopOfStructures=1'             +#13#10+
  'ForceFlyerExplosives=0'                          +#13#10+
  'PreventOfflinePvP=1'                             +#13#10+
  'AllowFlyingStaminaRecovery=1'                    +#13#10+
  'AllowMultipleAttachedC4=1'                       +#13#10+
  'OxygenSwimSpeedStatMultiplier=1.00'              +#13#10+
  'bPvEDisableFriendlyFire=1'                       +#13#10+
  'ServerAutoForceRespawnWildDinosInterval=86400'   +#13#10+
  'DisableWeatherFog=0'                             +#13#10+
  'RandomSupplyCratePoints=0'                       +#13#10+
  'CrossARKAllowForeignDinoDownloads=0'             +#13#10+
  'PersonalTamedDinosSaddleStructureCost=19'        +#13#10+
  ''                                                +#13#10+
  '[/script/engine.gamesession]'                    +#13#10+
  'MaxPlayers=70'                                   +#13#10+
  ''                                                +#13#10+
  '[SessionSettings]'                               +#13#10+
  'SessionName=arkforum.de'                         +#13#10+
  ''                                                +#13#10+
  '[MessageOfTheDay]'                               +#13#10+
  'Duration=30'                                     +#13#10+
  'Message=arkforum.de';
{$ENDIF}

type
 ETestFail = class(Exception);

class function TMemINI.ClassTest : Boolean;
{$IFDEF DEBUG}
var
 INI: TMemINI;
 SL : TStringList;
 DT : TDateTime;

 procedure Fail(N : Integer);
 begin
  raise ETestFail.Create('Failed test: '+N.ToString);
 end;

begin
 Result := false;
 INI := TMemINI.Create(INITestData);
 SL := TStringList.Create;
 try
  try
   INI.Clear;
   INI.Text := INITestData;

   if not INI.ValueExists('Oranization', 'UseOptimizedHarvestingHealth') then Fail(10);
   if not INI.ValueExists('Oranization', 'useoptimizedharvestinghealth') then Fail(20);
   if not INI.ValueExists('Oranization', 'USEOPTIMIZEDHARVESTINGHEALTH') then Fail(30);

   INI.DeleteKey('Oranization', 'UseOptimizedHarvestingHealth');
   if INI.ValueExists('Oranization', 'UseOptimizedHarvestingHealth') then Fail(40);
   if INI.ValueExists('Oranization', 'useoptimizedharvestinghealth') then Fail(50);
   if INI.ValueExists('Oranization', 'USEOPTIMIZEDHARVESTINGHEALTH') then Fail(60);

   if not INI.SectionExists('Oranization') then Fail(70);
   if not INI.SectionExists('oranization') then Fail(80);
   if not INI.SectionExists('ORANIZATION') then Fail(90);
   INI.EraseSection('ORANIZATION');
   // console.log(INI.Text);
   if INI.SectionExists('Oranization') then Fail(100);
   if INI.SectionExists('oranization') then Fail(110);
   if INI.SectionExists('ORANIZATION') then Fail(120);

   if not INI.SectionExists('LanguageTest') then Fail(130);
   INI.ReadSection('LanguageTest', SL);
   if SL.Count<>14 then Fail(140);
   // console.log(SL.Text);
   if not INI.ValueExists('LanguageTest', 'Änderungsbedürftigkeit') then Fail(150);
   if not INI.ValueExists('LanguageTest', 'änderungsbedürftigkeit') then Fail(160);
   if not INI.ValueExists('LanguageTest', 'ÄNDERUNGSBEDÜRFTIGKEIT') then Fail(170);
   if not INI.ValueExists('LanguageTest', '创建日期') then Fail(180);

   INI.ReadSectionValues('LanguageTest', SL);
   if SL.Count<>14 then Fail(190);
   // console.log(SL.Text);

   if INI.ReadString('LanguageTest', 'ServerPassword', '42')<>'NotGonnaTellYa' then Fail(200);
   if INI.ReadString('LanguageTestX', 'ServerPassword', '42')<>'42' then Fail(210);
   if INI.ReadString('LanguageTest', 'ServerPasswordX', '42')<>'42' then Fail(220);
   if INI.ReadString('LanguageTestX', 'ServerPasswordX', '42')<>'42' then Fail(230);

   INI.WriteString('LanguageTest', 'ServerPassword', 'newpassword');
   if INI.ReadString('LanguageTest', 'ServerPassword', '42')<>'newpassword' then Fail(240);

   INI.WriteString('LanguageTestX', 'ServerPassword', 'newpassword1');
   if INI.ReadString('LanguageTestX', 'ServerPassword', '42')<>'newpassword1' then Fail(250);
   INI.WriteString('LanguageTest', 'ServerPasswordX', 'newpassword2');
   if INI.ReadString('LanguageTest', 'ServerPasswordX', '42')<>'newpassword2' then Fail(260);
   INI.WriteString('LanguageTestX', 'ServerPasswordX', 'newpassword3');
   if INI.ReadString('LanguageTestX', 'ServerPasswordX', '42')<>'newpassword3' then Fail(270);

   if INI.AsString['LanguageTest', 'ServerPassword']<>'newpassword' then Fail(280);
   try
    if INI.AsString['LanguageTest', 'ServerPasswordErr']<>'' then;
    Fail(290);
   except
    on EINIKeyNotFound do;
    else raise
   end;
   try
    if INI.AsString['LanguageTestErr', 'ServerPassword']<>'' then;
    Fail(300);
   except
    on EINISectionNotFound do;
    else raise
   end;
   try
    if INI.AsString['LanguageTestErr', 'ServerPasswordErr']<>'' then;
    Fail(310);
   except
    on EINISectionNotFound do;
    else raise
   end;
   try
    if INI.AsString['LanguageTest', '  ']<>'' then;
    Fail(320);
   except
    on EINIKeyEmpty do;
    else raise
   end;
   try
    if INI.AsString['', 'ServerPassword']<>'' then;
    Fail(330);
   except
    on EINISectionEmpty do;
    else raise
   end;
   try
    if INI.AsString['  ', '  ']<>'' then;
    Fail(340);
   except
    on EINISectionEmpty do;
    else raise
   end;

   INI.AsString['LanguageTest', 'ServerPassword'] := 'yetanotherone';
   if INI.AsString['LanguageTest', 'ServerPassword']<>'yetanotherone' then Fail(350);

   if INI.AsString['LanguageTest', 'ValueIsKey']<>'ValueIsKey' then Fail(360);

   INI.AsString['LanguageTest','ServerMüßigkeit'] := '12345';
   if INI.AsString['LanguageTest','ServerMüßigkeit']<>'12345' then Fail(370);

   INI.AsString['LanguageTest','ServerMüßigkeit'] := '';
   if INI.AsString['LanguageTest','ServerMüßigkeit']<>'' then Fail(380);

   INI.AsString['LanguageTest','ServerMüßigkeit'] := 'ServerMüßigkeit';
   if INI.AsString['LanguageTest','ServerMüßigkeit']<>'ServerMüßigkeit' then Fail(390);

   INI.ReadSectionValues('LanguageTest', SL);
   // console.log('----');
   // console.log(INI.Text);

   // console.log('LineCount: '+INI.LineCount.ToString);
   if INI.LineCount<>118 then Fail(400);

   // for Ind := INI.LineCount-1 downto 0 do
   //   console.log(Ind.ToString + ' - ' + INI.Line[Ind]);

   if INI.Line[41]<>'ServerMüßigkeit' then Fail(410);

   INI.AsInteger   ['A', 'B'] := 12345;
   if INI.AsInteger['A', 'B']<>12345  then Fail(420);

   INI.AsInteger   ['A', 'B'] := -12345;
   if INI.AsInteger['A', 'B']<>-12345  then Fail(430);

   INI.AsInteger   ['A', 'B'] := MaxInt;
   if INI.AsInteger['A', 'B']<>MaxInt  then Fail(440);

   INI.AsInt64   ['A', 'B'] := 12345;
   if INI.AsInt64['A', 'B']<>12345  then Fail(420);

   INI.AsInt64   ['A', 'B'] := -12345;
   if INI.AsInt64['A', 'B']<>-12345  then Fail(430);

   INI.AsInt64   ['A', 'B'] := 9007199254740991;
   if INI.AsInt64['A', 'B']<>9007199254740991 then Fail(440);

   INI.AsInt64   ['A', 'B'] := -9007199254740991;
   if INI.AsInt64['A', 'B']<>-9007199254740991 then Fail(450);

   INI.AsFloat   ['A', 'B'] := PI;
   if INI.AsFloat['A', 'B']<>3.14159265358979{3} then Fail(460);
   INI.AsFloat   ['A', 'B'] := -PI;
   if INI.AsFloat['A', 'B']<>-3.14159265358979{3} then Fail(470);
   INI.AsFloat   ['A', 'B'] := INI.AsFloat['A', 'B']*INI.AsFloat['A', 'B'];
   if INI.AsFloat['A', 'B']<>9.86960440108934 then Fail(480);

   INI.AsBoolean   ['A', 'B'] := True;
   if INI.AsBoolean['A', 'B']<>True then Fail(490);
   if INI.AsString ['A', 'B']<>'True' then Fail(500);

   INI.AsBoolean   ['A', 'B'] := False;
   if INI.AsBoolean['A', 'B']<>False then Fail(510);
   if INI.AsString ['A', 'B']<>'False' then Fail(520);

   DT := Now; //TTimeZone.Local.ToUniversalTime(Now);
   INI.AsDate       ['A', 'B'] := DT;
   if INI.AsDate    ['A', 'B']<>Trunc(DT) then Fail(530);
   INI.AsTime       ['A', 'B'] := DT;
   if INI.AsTime    ['A', 'B']<>Frac(DT) then Fail(540);
   INI.AsDateTime   ['A', 'B'] := DT;
   if INI.AsDateTime['A', 'B']<>DT then Fail(550);

   DT := EncodeDateTime(2024,3,12,18,22,46,0);
   if INI.AsDateTime['LanguageTest', '创建日期']<>DT then Fail(560);

   console.log('TMemINI.ClassTest passed!');
   Result := true;
  except on E:Exception do
   begin
    console.warn(E.Message);
    console.log('TMemINI.ClassTest failed!');
   end
  end;
 finally
  INI.Free;
 end;
end;
{$ELSE}
begin
end;
{$ENDIF}

end.
