unit rb18LazLCLRegister;

{$mode objfpc}{$H+}

interface

uses
  Classes,
  rb18Source, rb18Script;

procedure Register;

implementation

procedure Register;
 begin
 RegisterComponents('Scripts',[TRuby18Source, TRuby18Script]);
 end;

end.

