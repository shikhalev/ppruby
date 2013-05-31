unit RubyTypes;

{$mode objfpc}{$H+}

interface

type
  PVALUE = ^VALUE;
  VALUE = record
    value : PtrUInt
  end;

  PID = ^ID;
  ID = record
    value : PtrUInt;
  end;

implementation

end.

