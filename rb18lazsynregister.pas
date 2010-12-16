unit rb18LazSynRegister;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,
  SynHighlighterRuby;

procedure Register;

implementation

procedure Register;
 begin
 RegisterComponents('SynEdit',[TSynRubySyn]);
 end;

end.

