unit rb18LazLCLRegister;

{$mode objfpc}{$H+}

interface

uses
  Classes,
  rb18Script;

procedure Register;

implementation

procedure Register;
 begin
 RegisterComponents('Scripts',[TRuby18Script, TRuby18Source, TRuby18FileSource, TRuby18EditSource]);
 end;

end.

