unit WEBLib.TextContCompReg;

interface

uses
 System.Classes, WEBLib.TextContComp, WEBLib.DesignIntf;

procedure Register;

{##############################################################################}

implementation

procedure Register;
begin
 RegisterComponents('TMS WEB 3rd party', [TTextCont]);
 RegisterClass(TTextCont);
end;

end.

