unit rb18LazSynRegister;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, rb18SynEditSource;

procedure Register;

implementation

procedure Register;
 begin
   RegisterComponents('Scripts',[TRuby18SynEditSource]);
 end;

end.

