(*
   Unit: ruby18dynamic.pp
   Description: Interface for shared library 'ruby18'
                with dynamic loading
   License: GNU General Public License
*)

{$mode objfpc}{$h+}

unit ruby18dynamic;

interface

uses
  SysUtils, DynLibs;

const
  RUBY18_LIB = 'libruby18.so';

type
  PPVALUE = ^PVALUE;
  PVALUE = ^VALUE;
  VALUE = packed record
    data : ptruint;
  end;

  PID = ^ID;
  ID = packed record
    data : ptruint;
  end;

type
  PRBasic = ^RBasic;
  RBasic = record
    flags   : ptruint;
    klass   : VALUE;
  end;

  RUBY_DATA_FUNC  = procedure (arg : pointer); cdecl;

  PRData  = ^RData;
  RData   = packed record
    basic   : RBasic;
    dmark   : RUBY_DATA_FUNC;
    dfree   : RUBY_DATA_FUNC;
    data    : pointer;
  end;

const
  IMMEDIATE_MASK  = $03;

  SYMBOL_FLAG = $0E;

  _Qfalse = 0;
  _Qtrue  = 2;
  _Qnil   = 4;
  _Qundef = 6;
  Qfalse : VALUE = (data : _Qfalse);
  Qtrue  : VALUE = (data : _Qtrue);
  Qnil   : VALUE = (data : _Qnil);
  Qundef : VALUE = (data : _Qundef);

  T_NONE   = $00;

  T_NIL    = $01;
  T_OBJECT = $02;
  T_CLASS  = $03;
  T_ICLASS = $04;
  T_MODULE = $05;
  T_FLOAT  = $06;
  T_STRING = $07;
  T_REGEXP = $08;
  T_ARRAY  = $09;
  T_FIXNUM = $0A;
  T_HASH   = $0B;
  T_STRUCT = $0C;
  T_BIGNUM = $0D;
  T_FILE   = $0E;

  T_TRUE   = $20;
  T_FALSE  = $21;
  T_DATA   = $22;
  T_MATCH  = $23;
  T_SYMBOL = $24;

  T_BLKTAG = $3B;
  T_UNDEF  = $3C;
  T_VARMAP = $3D;
  T_SCOPE  = $3E;
  T_NODE   = $3F;

  T_MASK   = $3F;

const
  FIXNUM_MAX  = high(ptrint) shr 1;
  FIXNUM_MIN  = (low(ptrint) shr 1) or low(ptrint);

  FIXNUM_FLAG = $01;

type
  Pfunc = function (arg : VALUE) : VALUE; cdecl;
  Pmethod = pointer;

var
  ruby_init : procedure (); cdecl;
  ruby_init_loadpath : procedure (); cdecl;
  ruby_script : procedure (name : pchar); cdecl;
  ruby_cleanup : function (ex : integer) : integer; cdecl;
  rb_inspect : function (obj : VALUE) : VALUE; cdecl;
  rb_num2long : function (val : VALUE) : ptrint; cdecl;
  rb_num2ulong : function (val : VALUE) : ptruint; cdecl;
  rb_num2dbl : function (val : VALUE) : double; cdecl;
  rb_protect : function (func : Pfunc; data : VALUE; state : PInteger) : VALUE; cdecl;
  rb_string_value_cstr : function (constref str : VALUE) : pchar; cdecl;
  rb_int2inum : function (n : ptrint) : VALUE; cdecl;
  rb_uint2inum : function (n : ptruint) : VALUE; cdecl;
  rb_float_new : function (d : double) : VALUE; cdecl;
  rb_str_new2 : function (ptr : pchar) : VALUE; cdecl;
  rb_intern : function (name : pchar) : ID; cdecl;
  rb_id2name : function (id : ID) : pchar; cdecl;
  rb_to_id : function (name : VALUE) : ID; cdecl;
  rb_data_object_alloc : function (klass : VALUE; datap : pointer; dmark : RUBY_DATA_FUNC; dfree : RUBY_DATA_FUNC) : VALUE; cdecl;
  rb_define_class : function (name : pchar; super : VALUE) : VALUE; cdecl;
  rb_define_alloc_func : procedure (klass : VALUE; func : Pfunc); cdecl;
  rb_define_method : procedure (klass : VALUE; name : pchar; func : Pmethod; argc : integer); cdecl;
  rb_define_singleton_method : procedure (obj : VALUE; name : pchar; func : Pmethod; argc : integer); cdecl;
  rb_yield : function (val : VALUE) : VALUE; cdecl;
  rb_include_module : procedure (klass : VALUE; module : VALUE); cdecl;
  rb_eval_string_protect : function (str : pchar; state : PInteger) : VALUE; cdecl;
  rb_define_global_function : procedure (name : pchar; func : Pmethod; argc : integer); cdecl;

var
  rb_cObject : VALUE;
  rb_cFixnum : VALUE;
  rb_cNilClass : VALUE;
  rb_cFalseClass : VALUE;
  rb_cTrueClass : VALUE;
  rb_cSymbol : VALUE;

  rb_mEnumerable : VALUE;

type
  ERubyError = class(Exception)end;
  ERubyLibError = class(Exception)end;

resourcestring
  msgRubyLoadError =
    'Error while load the Ruby library.';
  msgRubyUnloadError =
    'Error while unload the Ruby library.';
  msgRubyAlreadyLoaded =
    'The Ruby library is already loaded.';
  msgRubyIsNotLoaded =
    'The Ruby library is not loaded.';
  msgRubyLibError =
    'The Ruby library error #%d.';

function isRubyLoaded : boolean; inline;

procedure LoadRuby;
procedure UnloadRuby;

function rb_class_of (obj : VALUE) : VALUE; inline;
function rb_type (obj : VALUE) : integer; inline;

operator = (a, b : VALUE) : boolean; inline;
operator = (a, b : ID) : boolean; inline;

implementation

var
  hLib : TLibHandle = 0;

function isRubyLoaded : boolean; inline;
 begin
  result := (hLib <> 0)
 end;

procedure load_functions; inline;
 begin
  pointer(ruby_init)                  := GetProcedureAddress(hLib, 'ruby_init');
  pointer(ruby_init_loadpath)         := GetProcedureAddress(hLib, 'ruby_init_loadpath');
  pointer(ruby_script)                := GetProcedureAddress(hLib, 'ruby_script');
  pointer(ruby_cleanup)               := GetProcedureAddress(hLib, 'ruby_cleanup');
  pointer(rb_data_object_alloc)       := GetProcedureAddress(hLib, 'rb_data_object_alloc');
  pointer(rb_define_alloc_func)       := GetProcedureAddress(hLib, 'rb_define_alloc_func');
  pointer(rb_define_class)            := GetProcedureAddress(hLib, 'rb_define_class');
  pointer(rb_define_global_function)  := GetProcedureAddress(hLib, 'rb_define_global_function');
  pointer(rb_define_method)           := GetProcedureAddress(hLib, 'rb_define_method');
  pointer(rb_define_singleton_method) := GetProcedureAddress(hLib, 'rb_define_singleton_method');
  pointer(rb_eval_string_protect)     := GetProcedureAddress(hLib, 'rb_eval_string_protect');
  pointer(rb_float_new)               := GetProcedureAddress(hLib, 'rb_float_new');
  pointer(rb_id2name)                 := GetProcedureAddress(hLib, 'rb_id2name');
  pointer(rb_include_module)          := GetProcedureAddress(hLib, 'rb_include_module');
  pointer(rb_inspect)                 := GetProcedureAddress(hLib, 'rb_inspect');
  pointer(rb_int2inum)                := GetProcedureAddress(hLib, 'rb_int2inum');
  pointer(rb_intern)                  := GetProcedureAddress(hLib, 'rb_intern');
  pointer(rb_num2dbl)                 := GetProcedureAddress(hLib, 'rb_num2dbl');
  pointer(rb_num2long)                := GetProcedureAddress(hLib, 'rb_num2long');
  pointer(rb_num2ulong)               := GetProcedureAddress(hLib, 'rb_num2ulong');
  pointer(rb_protect)                 := GetProcedureAddress(hLib, 'rb_protect');
  pointer(rb_string_value_cstr)       := GetProcedureAddress(hLib, 'rb_string_value_cstr');
  pointer(rb_str_new2)                := GetProcedureAddress(hLib, 'rb_str_new2');
  pointer(rb_to_id)                   := GetProcedureAddress(hLib, 'rb_to_id');
  pointer(rb_uint2inum)               := GetProcedureAddress(hLib, 'rb_uint2inum');
  pointer(rb_yield)                   := GetProcedureAddress(hLib, 'rb_yield');
 end;

procedure load_variables; inline;
 var
   res : integer;
 begin
  rb_cFalseClass := rb_eval_string_protect('FalseClass', @res);
  if res = 0
     then rb_cFixnum := rb_eval_string_protect('Fixnum', @res);
  if res = 0
     then rb_cNilClass := rb_eval_string_protect('NilClass', @res);
  if res = 0
     then rb_cObject := rb_eval_string_protect('Object', @res);
  if res = 0
     then rb_cSymbol := rb_eval_string_protect('Symbol', @res);
  if res = 0
     then rb_cTrueClass := rb_eval_string_protect('TrueClass', @res);
  if res = 0
     then rb_mEnumerable := rb_eval_string_protect('Enumerable', @res);
  if not (res = 0)
     then raise ERubyLibError.CreateFmt(msgRubyLibError, [res]);
 end;

procedure LoadRuby;
 begin
  if hLib <> 0
     then raise ERubyLibError.Create(msgRubyAlreadyLoaded);
  hLib := LoadLibrary(RUBY18_LIB);
  if hLib = 0
     then raise ERubyLibError.Create(msgRubyLoadError);
  load_functions();
  ruby_init();
  ruby_init_loadpath();
  ruby_script(pchar(ParamStr(0)));
  load_variables();
 end;

procedure UnloadRuby;
 begin
  if hLib = 0
     then raise ERubyLibError.Create(msgRubyIsNotLoaded);
  ruby_cleanup(0);
  if not UnloadLibrary(hLib)
     then raise ERubyLibError.Create(msgRubyUnloadError);
  hLib := 0;
 end;

function rb_class_of (obj : VALUE) : VALUE; inline;
 begin
  if (obj.data and FIXNUM_FLAG) <> 0
     then result := rb_cFixnum
     else case obj.data of
            _Qnil :
              result := rb_cNilClass;
            _Qfalse :
              result := rb_cFalseClass;
            _Qtrue :
              result := rb_cTrueClass;
            else
              if (obj.data and $FF) = $0E
                 then result := rb_cSymbol
                 else result := PRBasic(obj)^.klass;
          end;
 end;

function rb_type (obj : VALUE) : integer; inline;
 begin
  if (obj.data and FIXNUM_FLAG) <> 0
     then result := T_FIXNUM
     else case obj.data of
            _Qnil :
              result := T_NIL;
            _Qfalse :
              result := T_FALSE;
            _Qtrue :
              result := T_TRUE;
            _Qundef :
              result := T_UNDEF
            else
              if (obj.data and $FF) = $0E
                 then result := T_SYMBOL
                 else result := (PRBasic(obj)^.flags and T_MASK)
          end;
 end;

operator = (a, b : VALUE) : boolean; inline;
 begin
  result := (a.data = b.data)
 end;

operator = (a, b : ID) : boolean; inline;
 begin
  result := (a.data = b.data)
 end;

end.
