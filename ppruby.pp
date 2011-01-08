unit ppRuby;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, DynLibs;

type
  PVALUE = ^VALUE;
  VALUE = packed record
    data : PtrUInt;
  end;

  PID = ^ID;
  ID = packed record
    data : PtrUInt;
  end;

type
  TRubyVersion = (
    rvNone,
    rvRuby18, rvRuby19,
    (* future versions ... *)
    rvUnknown
  );

function Version : TRubyVersion; inline;

implementation

var
  verRuby : TRubyVersion = rvNone;

function Version : TRubyVersion; inline;
 begin
  Result := verRuby;
 end;

end.

