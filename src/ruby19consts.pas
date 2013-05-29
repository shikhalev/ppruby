unit Ruby19Consts;

{$mode objfpc}{$H+}

interface

type
  VALUE = type PtrUInt;
  ID = type PtrUInt;

{$notes off}
type
  ruby_special_consts = (
    RUBY_Qfalse = 0,
    RUBY_Qtrue  = 2,
    RUBY_Qnil   = 4,
    RUBY_Qundef = 6,

    RUBY_IMMEDIATE_MASK = $03,
    RUBY_FIXNUM_FLAG    = $01,
    RUBY_SYMBOL_FLAG    = $0E,
    RUBY_SPECIAL_SHIFT  = 8
  );
{$notes on}

const
  Qfalse = VALUE(RUBY_Qfalse);
  Qtrue  = VALUE(RUBY_Qtrue);
  Qnil   = VALUE(RUBY_Qnil);
  Qundef = VALUE(RUBY_Qundef);

  IMMEDIATE_MASK = PtrUInt(RUBY_IMMEDIATE_MASK);
  FIXNUM_FLAG    = PtrUInt(RUBY_FIXNUM_FLAG);
  SYMBOL_FLAG    = PtrUInt(RUBY_SYMBOL_FLAG);
  SPECIAL_SHIFT  = PtrUInt(RUBY_SPECIAL_SHIFT);

function RTEST (v : VALUE) : Boolean; inline;
function NIL_P (v : VALUE) : Boolean; inline;

implementation

function RTEST (v : VALUE) : Boolean; inline;
  begin
  result := (v and not Qnil) <> 0
  end;

function NIL_P (v : VALUE) : Boolean; inline;
  begin
  result := (v = Qnil)
  end;

end.

