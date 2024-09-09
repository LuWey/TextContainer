unit TextContCompEditor;

interface

uses
 System.SysUtils, DesignEditors, DesignIntf, StrEdit, StringsEdit,
 System.Classes, WEBLIB.TextContComp;

type
 TMyTextContEditor=class(TComponentEditor)
 private
  FOrigOnShow : TNotifyEvent;
  FLTComp     : TTextCont;
  FSED        : TStringsEditDlg;
  procedure OnDialogShow(Sender: TObject);
 public
  function GetVerbCount: Integer; override;
  function GetVerb(Index: Integer): string; override;
  procedure ExecuteVerb(Index: Integer); override;
 end;

procedure Register;

{##############################################################################}

implementation

uses Vcl.Forms, Vcl.Controls, Vcl.Dialogs, ColnEdit;

procedure Register;
begin
 RegisterComponents('TMS WEB 3rd party', [TTextCont]);
 RegisterComponentEditor(TTextCont, TMyTextContEditor);
end;

procedure TMyTextContEditor.OnDialogShow(Sender: TObject);
begin
 FOrigOnShow(Sender);
 if (FLTComp.Width>100)and(FLTComp.Height>100) then
  begin
   FSED.Width := FLTComp.Width;
   FSED.Height := FLTComp.Height;
  end;
end;

procedure TMyTextContEditor.ExecuteVerb(Index: Integer);
begin
 inherited;
 if Index<>0 then Exit;

 FSED := TStringsEditDlg.Create(nil);
 try
  FLTComp := Self.Component as TTextCont;
  FSED.Lines := FLTComp.Lines;

  FOrigOnShow := FSED.OnShow;
  FSED.OnShow := OnDialogShow;
  var Res := FSED.ShowModal;

  if (FLTComp.Width<>FSED.Width)or(FLTComp.Height<>FSED.Height) then
   begin
    FLTComp.Width := FSED.Width;
    FLTComp.Height := FSED.Height;
    Designer.Modified;
   end;

  if Res<>mrOK then exit;

  FLTComp.Lines := FSED.Lines;
  Designer.Modified;
 finally
  FSED.Free;
 end;
end;

function TMyTextContEditor.GetVerb(Index: Integer): string;
begin
 if Index=0 then Result := 'Edit Text'
 else Result := '';
end;

function TMyTextContEditor.GetVerbCount: Integer;
begin
 Result := 1;
end;

end.
