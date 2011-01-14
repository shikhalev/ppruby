{$codepage utf8}
{$smartlink on}
{$mode objfpc}{$h+}

unit ppRuby;

(*
   Package : RubyFCL
   File : ppruby.pp
   Desc : core unit of ruby binding
   License : GNU GPL
*)

interface

uses
  Classes, SysUtils, DynLibs, TypInfo;

type
  PVALUE = ^VALUE;
  VALUE = packed record
    data : PtrUInt;
  end;

  PID = ^ID;
  ID = packed record
    data : PtrUInt;
  end;

operator = (x, y : VALUE) : Boolean; inline;

function Qnil : VALUE; inline;
function Qfalse : VALUE; inline;
function Qtrue : VALUE; inline;
function Qundef : VALUE; inline;

operator explicit (v : VALUE) : UTF8String;
operator explicit (v : VALUE) : ansistring;
operator explicit (v : VALUE) : UnicodeString;

operator explicit (const s : UTF8String) : VALUE;
operator explicit (const s : ansistring) : VALUE;
operator explicit (const s : UnicodeString) : VALUE;

operator explicit (v : VALUE) : PtrInt;
operator explicit (v : VALUE) : PtrUInt;
operator explicit (v : VALUE) : Double;
operator explicit (v : VALUE) : Boolean;
operator explicit (v : VALUE) : TObject;
operator explicit (v : VALUE) : TClass;

operator explicit (const v : PtrInt) : VALUE;
operator explicit (const v : PtrUInt) : VALUE;
operator explicit (const v : Double) : VALUE;
operator explicit (const v : Boolean) : VALUE;
operator explicit (const v : TObject) : VALUE;
operator explicit (const v : TClass) : VALUE;

{$IFDEF CPU32}
operator explicit (v : VALUE) : Int64;
operator explicit (v : VALUE) : QWord;
operator explicit (const v : Int64) : VALUE;
operator explicit (const v : QWord) : VALUE;
{$ENDIF CPU32}

operator explicit (v : VALUE) : ID;
operator explicit (v : ID) : VALUE;
operator explicit (v : ID) : ansistring;
operator explicit (const v : ansistring) : ID;

type
  TRubyVersion = (
    rvNone,
    rvRuby18, rvRuby19
    (* , future versions ... *)
  );

  TRubyMethod0   = function (instance : VALUE) : VALUE; cdecl;
  TRubyMethod1   = function (instance : VALUE; a : VALUE) : VALUE; cdecl;
  TRubyMethod2   = function (instance : VALUE; a, b : VALUE) : VALUE; cdecl;
  TRubyMethod3   = function (instance : VALUE; a, b, c : VALUE) : VALUE; cdecl;
  TRubyMethod4   = function (instance : VALUE; a, b, c, d : VALUE) : VALUE; cdecl;
  TRubyMethod5   = function (instance : VALUE; a, b, c, d, e : VALUE) : VALUE; cdecl;
  TRubyMethod6   = function (instance : VALUE; a, b, c, d, e, f : VALUE) : VALUE; cdecl;
  TRubyMethod7   = function (instance : VALUE; a, b, c, d, e, f, g : VALUE) : VALUE; cdecl;
  TRubyMethod8   = function (instance : VALUE; a, b, c, d, e, f, g, h : VALUE) : VALUE; cdecl;
  TRubyMethod9   = function (instance : VALUE; a, b, c, d, e, f, g, h, i : VALUE) : VALUE; cdecl;
  TRubyMethod10  = function (instance : VALUE; a, b, c, d, e, f, g, h, i, j : VALUE) : VALUE; cdecl;
  TRubyMethod11  = function (instance : VALUE; a, b, c, d, e, f, g, h, i, j, k : VALUE) : VALUE; cdecl;
  TRubyMethod12  = function (instance : VALUE; a, b, c, d, e, f, g, h, i, j, k, l : VALUE) : VALUE; cdecl;
  TRubyMethod13  = function (instance : VALUE; a, b, c, d, e, f, g, h, i, j, k, l, m : VALUE) : VALUE; cdecl;
  TRubyMethod14  = function (instance : VALUE; a, b, c, d, e, f, g, h, i, j, k, l, m, n : VALUE) : VALUE; cdecl;
  TRubyMethod15  = function (instance : VALUE; a, b, c, d, e, f, g, h, i, j, k, l, m, n, o : VALUE) : VALUE; cdecl;
  TRubyMethod16  = function (instance : VALUE; a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p : VALUE) : VALUE; cdecl;
  TRubyMethod17  = function (instance : VALUE; a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q : VALUE) : VALUE; cdecl;
  TRubyMethodArr = function (argc : LongInt; argv : PVALUE; instance : VALUE) : VALUE; cdecl;

procedure DefineMethod (module : VALUE; const name : ansistring; method : TRubyMethod0);
procedure DefineMethod (module : VALUE; const name : ansistring; method : TRubyMethod1);
procedure DefineMethod (module : VALUE; const name : ansistring; method : TRubyMethod2);
procedure DefineMethod (module : VALUE; const name : ansistring; method : TRubyMethod3);
procedure DefineMethod (module : VALUE; const name : ansistring; method : TRubyMethod4);
procedure DefineMethod (module : VALUE; const name : ansistring; method : TRubyMethod5);
procedure DefineMethod (module : VALUE; const name : ansistring; method : TRubyMethod6);
procedure DefineMethod (module : VALUE; const name : ansistring; method : TRubyMethod7);
procedure DefineMethod (module : VALUE; const name : ansistring; method : TRubyMethod8);
procedure DefineMethod (module : VALUE; const name : ansistring; method : TRubyMethod9);
procedure DefineMethod (module : VALUE; const name : ansistring; method : TRubyMethod10);
procedure DefineMethod (module : VALUE; const name : ansistring; method : TRubyMethod11);
procedure DefineMethod (module : VALUE; const name : ansistring; method : TRubyMethod12);
procedure DefineMethod (module : VALUE; const name : ansistring; method : TRubyMethod13);
procedure DefineMethod (module : VALUE; const name : ansistring; method : TRubyMethod14);
procedure DefineMethod (module : VALUE; const name : ansistring; method : TRubyMethod15);
procedure DefineMethod (module : VALUE; const name : ansistring; method : TRubyMethod16);
procedure DefineMethod (module : VALUE; const name : ansistring; method : TRubyMethod17);
procedure DefineMethod (module : VALUE; const name : ansistring; method : TRubyMethodArr);

procedure DefineSingletonMethod (instance : VALUE; const name : ansistring; method : TRubyMethod0);
procedure DefineSingletonMethod (instance : VALUE; const name : ansistring; method : TRubyMethod1);
procedure DefineSingletonMethod (instance : VALUE; const name : ansistring; method : TRubyMethod2);
procedure DefineSingletonMethod (instance : VALUE; const name : ansistring; method : TRubyMethod3);
procedure DefineSingletonMethod (instance : VALUE; const name : ansistring; method : TRubyMethod4);
procedure DefineSingletonMethod (instance : VALUE; const name : ansistring; method : TRubyMethod5);
procedure DefineSingletonMethod (instance : VALUE; const name : ansistring; method : TRubyMethod6);
procedure DefineSingletonMethod (instance : VALUE; const name : ansistring; method : TRubyMethod7);
procedure DefineSingletonMethod (instance : VALUE; const name : ansistring; method : TRubyMethod8);
procedure DefineSingletonMethod (instance : VALUE; const name : ansistring; method : TRubyMethod9);
procedure DefineSingletonMethod (instance : VALUE; const name : ansistring; method : TRubyMethod10);
procedure DefineSingletonMethod (instance : VALUE; const name : ansistring; method : TRubyMethod11);
procedure DefineSingletonMethod (instance : VALUE; const name : ansistring; method : TRubyMethod12);
procedure DefineSingletonMethod (instance : VALUE; const name : ansistring; method : TRubyMethod13);
procedure DefineSingletonMethod (instance : VALUE; const name : ansistring; method : TRubyMethod14);
procedure DefineSingletonMethod (instance : VALUE; const name : ansistring; method : TRubyMethod15);
procedure DefineSingletonMethod (instance : VALUE; const name : ansistring; method : TRubyMethod16);
procedure DefineSingletonMethod (instance : VALUE; const name : ansistring; method : TRubyMethod17);
procedure DefineSingletonMethod (instance : VALUE; const name : ansistring; method : TRubyMethodArr);

procedure DefineAlias (module : VALUE; const newname, oldname : ansistring);

procedure IncludeModule (cls : VALUE; module : VALUE);

function ModComparable : VALUE;
function ModEnumerable : VALUE;
function ModPrecision : VALUE;

function Version : TRubyVersion; inline;

function Load (ver : TRubyVersion) : Boolean;
function Load : TRubyVersion;
procedure Unload;

function getActive : Boolean;
procedure setActive (val : Boolean);
property Active : Boolean read getActive write setActive;

type
  TLoadHook = procedure;
  TUnloadHook = procedure;

procedure AddLoadHook (hook : TLoadHook);
procedure RemoveLoadHook (hook : TLoadHook);
procedure AddUnloadHook (hook : TUnloadHook);
procedure RemoveUnloadHook (hook : TUnloadHook);

type
  TClassHook = procedure(cls : VALUE);

procedure AddClassHook (cls : TClass; hook : TClassHook);
procedure RemoveClassHook (cls : TClass; hook : TClassHook);

function ErrorInfo : VALUE;
function Inspect (v : VALUE) : VALUE;
function Yield (v : VALUE) : VALUE;

procedure WrapObject (val : VALUE; obj : TObject);
function MakeArray (const vals : array of VALUE) : VALUE;
function MakeHash : VALUE;
function HashGet (hash : VALUE; key : VALUE) : VALUE;
function HashSet (hash : VALUE; key, val : VALUE) : VALUE;

type
  TRubyValueType = (
    rtNone, rtNil, rtObject, rtClass, rtModule, rtFloat, rtString, rtRegExp,
    rtArray, rtFixNum, rtHash, rtBigNum, rtFile, rtStruct, rtData, rtSymbol,
    rtTrue, rtFalse, rtUndef
  );

function ValType (v : VALUE) : TRubyValueType;
function ValClass (v : VALUE) : VALUE;

type
  ERubyError = class(Exception)
  private
    fldErrInfo: VALUE;
  public
    property ErrInfo : VALUE read fldErrInfo write fldErrInfo;
  public
    constructor Create (const msg : UTF8String);
    constructor CreateFmt (const msg : UTF8String; const args : array of const);
  end;

  ELibraryError = class(ERubyError);
    EUnknownLibrary = class(ELibraryError);
  ERubyInactive = class(ERubyError);
  ERubyAlreadyLoaded = class(ERubyError);
  ERubyConversion = class(ERubyError);
    ERubyNotObject = class(ERubyConversion);
    ERubyNotClass = class(ERubyConversion);
  ERubyDefinition = class(ERubyError);
  EIncompatibleType = class(ERubyError);

resourcestring
  msgActivateError =
    'Error while activation.';
  msgRubyInactive =
    'Ruby is inactive.';
  msgUnknownLibrary =
    'Unknown library version.';
  msgAlreadyLoaded =
    'Ruby library is already loaded.';
  msgConversionError =
    'Illegal value conversion.';
  msgDefineError =
    'Error in definition.';
  msgNotObject =
    'Value is not a pascal object.';
  msgNotClass =
    'Value is not a pascal class.';
  msgIncompatible =
    'Incompatible type of value.';

type
  TRubyMsg = record
    msg : shortstring;
    argc : Integer;
    argv : PVALUE;
    result : PVALUE;
  end;

implementation

procedure errInactive; inline;
 begin
  raise ERubyInactive.Create(msgRubyInactive);
 end;

procedure errUnknown; inline;
 begin
  raise EUnknownLibrary.Create(msgUnknownLibrary);
 end;

procedure chkConversion (res : Integer); inline;
 begin
  if res <> 0
     then raise ERubyConversion.Create(msgConversionError);
 end;

var
  verRuby : TRubyVersion = rvNone;
  libRuby : TLibHandle = 0;
  hooksLoad : array of TLoadHook = nil;
  hooksUnload : array of TUnloadHook = nil;
  hooksClasses : array of record
    cls : TClass;
    hooks : array of TClassHook;
  end = nil;
  cacheObjects : array of record
    obj : TObject;
    val : VALUE;
  end = nil;
  cacheClasses : array of record
    cls : TClass;
    val : VALUE;
  end = nil;

function find_object (obj : TObject; out idx : PtrInt) : Boolean;
 var
   min, max : PtrInt;
 begin
  idx := Length(cacheObjects);
  if idx = 0
     then Exit(false);
  if Pointer(obj) > Pointer(cacheObjects[idx - 1].obj)
     then Exit(false);
  max := idx - 1;
  idx := 0;
  if Pointer(obj) < Pointer(cacheObjects[0].obj)
     then Exit(false);
  min := 0;
  repeat
   if Pointer(obj) = Pointer(cacheObjects[min].obj)
      then idx := min
      else if Pointer(obj) = Pointer(cacheObjects[max].obj)
              then idx := max
              else idx := (max + min) div 2;
   if Pointer(obj) = Pointer(cacheObjects[idx].obj)
      then exit(true)
      else if Pointer(obj) < Pointer(cacheObjects[idx].obj)
              then max := idx
              else min := idx;
  until max - min <= 1;
  idx := min + 1;
  Result := false;
 end;

procedure insert_object(obj : TObject; val : VALUE; idx : PtrInt);
 var
   len, ins : PtrInt;
 begin
  len := Length(cacheObjects);
  SetLength(cacheObjects, len + 1);
  for ins := len downto idx + 1 do
      cacheObjects[ins] := cacheObjects[ins - 1];
  cacheObjects[idx].obj := obj;
  cacheObjects[idx].val := val;
 end;

const
{$IFDEF UNIX}
  LIB18 = 'libruby18.so';
  LIB19 = 'libruby19.so';
{$ELSE !UNIX}
 {$IFDEF WINDOWS}
  LIB18 = 'msvcrt-ruby18.dll';
  LIB19 = 'msvcrt-ruby19.dll';
 {$ELSE !WINDOWS}
  LIB18 = 'ruby18'; // ... I don't know...
  LIB19 = 'ruby19';
 {$ENDIF WINDOWS}
{$ENDIF UNIX}

procedure init_18_19; forward;
procedure done_18_19; forward;

const
  LIBRARIES : array [succ(rvNone)..high(TRubyVersion)] of record
    name : ansistring;
    init : procedure;
    done : procedure;
  end = (
    (name : LIB18; init : @init_18_19; done : @done_18_19),
    (name : LIB19; init : @init_18_19; done : @done_18_19)
  );

type
  Pfunc = function (v : VALUE) : VALUE; cdecl;
  Pdatafunc = procedure (p : Pointer); cdecl;

var
  p_ruby_errinfo : PVALUE;

  f_rb_inspect : function (v : VALUE) : VALUE; cdecl;
  f_rb_protect : function (func : Pfunc; data : VALUE; state : PInteger) : VALUE; cdecl;
  f_rb_string_value_cstr : function (constref str : VALUE) : pchar; cdecl;
  f_rb_define_method : procedure (module : VALUE; name : PChar; func : Pointer; argc : Integer); cdecl;
  f_rb_define_singleton_method : procedure (module : VALUE; name : PChar; func : Pointer; argc : Integer); cdecl;
  f_rb_str_new2 : function (ptr : PChar) : VALUE; cdecl;
  f_rb_num2int : function (val : VALUE) : PtrInt; cdecl;
  f_rb_num2uint : function (val : VALUE) : PtrUInt; cdecl;
  f_rb_num2dbl : function (val : VALUE) : Double; cdecl;
  f_rb_int2inum : function (n : PtrInt) : VALUE; cdecl;
  f_rb_uint2inum : function (n : PtrUInt) : VALUE; cdecl;
  f_rb_float_new : function (d : Double) : VALUE; cdecl;
  f_rb_data_object_alloc : function (cls : VALUE; ptr : pointer; dmark, dfree : Pdatafunc) : VALUE; cdecl;
  f_rb_define_class : function (name : pchar; super : VALUE) : VALUE; cdecl;
  f_rb_to_id : function (name : VALUE) : ID; cdecl;
  f_rb_intern : function (name : PChar) : ID; cdecl;
  f_rb_id2name : function (id : ID) : PChar; cdecl;
  f_rb_funcall2 : function (receiver : VALUE; mid : ID; argc : Integer; argv : PVALUE) : VALUE; cdecl;
  f_rb_raise : procedure (exc : VALUE; fmt : pchar); varargs; cdecl;
{$IFDEF CPU32}
  f_rb_ll2inum : function (n : Int64) : VALUE; cdecl;
  f_rb_ull2inum : function (n : QWord) : VALUE; cdecl;
  f_rb_num2ll : function (v : VALUE) : Int64; cdecl;
  f_rb_num2ull : function (v : VALUE) : QWord; cdecl;
{$ENDIF CPU32}
  f_ruby_init : procedure (); cdecl;
  f_ruby_init_loadpath : procedure (); cdecl;
  f_ruby_script : procedure (name : PChar); cdecl;
  f_ruby_finalize : procedure (); cdecl;
  f_rb_eval_string_protect : function (str : pchar; state : PInteger) : VALUE; cdecl;
  f_rb_define_alloc_func : procedure (klass : VALUE; func : Pfunc); cdecl;
  f_rb_yield : function (val : VALUE) : VALUE; cdecl;
  f_rb_include_module : procedure (klass : VALUE; module : VALUE); cdecl;
  f_rb_ary_new4 : function (n : ptrint; elts : PVALUE) : VALUE; cdecl;
  f_rb_hash_new : function () : VALUE; cdecl;
  f_rb_hash_aref : function (hash, key : VALUE) : VALUE; cdecl;
  f_rb_hash_aset : function (hash, key, val : VALUE) : VALUE; cdecl;
  f_rb_define_alias : procedure (klass : VALUE; name1, name2 : PChar); cdecl;

  v_rb_cObject : VALUE;
  v_rb_cFixnum : VALUE;
  v_rb_cNilClass : VALUE;
  v_rb_cFalseClass : VALUE;
  v_rb_cTrueClass : VALUE;
  v_rb_cSymbol : VALUE;
  v_rb_mComparable : VALUE;
  v_rb_mEnumerable : VALUE;
  v_rb_mPrecision : VALUE;
  v_rb_eNoMethodError : VALUE;

procedure init_18_19;
 var
   res : Integer;
 begin
  // init p_ vars
  p_ruby_errinfo := GetProcedureAddress(libRuby, 'ruby_errinfo');
  // init funcs
  Pointer(f_rb_inspect) := GetProcedureAddress(libRuby, 'rb_inspect');
  Pointer(f_rb_protect) := GetProcedureAddress(libRuby, 'rb_protect');
  Pointer(f_rb_string_value_cstr) := GetProcedureAddress(libRuby, 'rb_string_value_cstr');
  Pointer(f_rb_define_method) := GetProcedureAddress(libRuby, 'rb_define_method');
  Pointer(f_rb_define_singleton_method) := GetProcedureAddress(libRuby, 'rb_define_singleton_method');
  Pointer(f_rb_str_new2) := GetProcedureAddress(libRuby, 'rb_str_new2');
  Pointer(f_rb_num2int) := GetProcedureAddress(libRuby, 'rb_num2int');
  Pointer(f_rb_num2uint) := GetProcedureAddress(libRuby, 'rb_num2uint');
  Pointer(f_rb_num2dbl) := GetProcedureAddress(libRuby, 'rb_num2dbl');
  Pointer(f_rb_int2inum) := GetProcedureAddress(libRuby, 'rb_int2inum');
  Pointer(f_rb_uint2inum) := GetProcedureAddress(libRuby, 'rb_uint2inum');
  Pointer(f_rb_float_new) := GetProcedureAddress(libRuby, 'rb_float_new');
  Pointer(f_rb_data_object_alloc) := GetProcedureAddress(libRuby, 'rb_data_object_alloc');
  Pointer(f_rb_define_class) := GetProcedureAddress(libRuby, 'rb_define_class');
  Pointer(f_rb_to_id) := GetProcedureAddress(libRuby, 'rb_to_id');
  Pointer(f_rb_intern) := GetProcedureAddress(libRuby, 'rb_intern');
  Pointer(f_rb_id2name) := GetProcedureAddress(libRuby, 'rb_id2name');
  Pointer(f_rb_funcall2) := GetProcedureAddress(libRuby, 'rb_funcall2');
  Pointer(f_rb_raise) := GetProcedureAddress(libRuby, 'rb_raise');
{$IFDEF CPU32}
  Pointer(f_rb_ll2inum) := GetProcedureAddress(libRuby, 'rb_ll2inum');
  Pointer(f_rb_ull2inum) := GetProcedureAddress(libRuby, 'rb_ull2inum');
  Pointer(f_rb_num2ll) := GetProcedureAddress(libRuby, 'rb_num2ll');
  Pointer(f_rb_num2ull) := GetProcedureAddress(libRuby, 'rb_num2ull');
{$ENDIF CPU32}
  Pointer(f_ruby_init) := GetProcedureAddress(libRuby, 'ruby_init');
  Pointer(f_ruby_init_loadpath) := GetProcedureAddress(libRuby, 'ruby_init_loadpath');
  Pointer(f_ruby_script) := GetProcedureAddress(libRuby, 'ruby_script');
  Pointer(f_ruby_finalize) := GetProcedureAddress(libRuby, 'ruby_finalize');
  Pointer(f_rb_eval_string_protect) := GetProcedureAddress(libRuby, 'rb_eval_string_protect');
  Pointer(f_rb_define_alloc_func) := GetProcedureAddress(libRuby, 'rb_define_alloc_func');
  Pointer(f_rb_yield) := GetProcedureAddress(libRuby, 'rb_yield');
  Pointer(f_rb_include_module) := GetProcedureAddress(libRuby, 'rb_include_module');
  Pointer(f_rb_ary_new4) := GetProcedureAddress(libRuby, 'rb_ary_new4');
  Pointer(f_rb_hash_new) := GetProcedureAddress(libRuby, 'rb_hash_new');
  Pointer(f_rb_hash_aref) := GetProcedureAddress(libRuby, 'rb_hash_aref');
  Pointer(f_rb_hash_aset) := GetProcedureAddress(libRuby, 'rb_hash_aset');
  Pointer(f_rb_define_alias) := GetProcedureAddress(libRuby, 'rb_define_alias');
  // init library
  f_ruby_init();
  f_ruby_init_loadpath();
  f_ruby_script(PChar(ParamStr(0)));
  f_rb_eval_string_protect('$-K = "UTF8"', @res); // TODO: check for 1.9
  if res <> 0
     then raise ELibraryError.Create(msgActivateError);
  // init v_ vars
  v_rb_cObject := PVALUE(GetProcedureAddress(libRuby, 'rb_cObject'))^;
  v_rb_cFixnum := PVALUE(GetProcedureAddress(libRuby, 'rb_cFixnum'))^;
  v_rb_cNilClass := PVALUE(GetProcedureAddress(libRuby, 'rb_cNilClass'))^;
  v_rb_cFalseClass := PVALUE(GetProcedureAddress(libRuby, 'rb_cFalseClass'))^;
  v_rb_cTrueClass := PVALUE(GetProcedureAddress(libRuby, 'rb_cTrueClass'))^;
  v_rb_cSymbol := PVALUE(GetProcedureAddress(libRuby, 'rb_cSymbol'))^;
  v_rb_mComparable := PVALUE(GetProcedureAddress(libRuby, 'rb_mComparable'))^;
  v_rb_mEnumerable := PVALUE(GetProcedureAddress(libRuby, 'rb_mEnumerable'))^;
  v_rb_mPrecision := PVALUE(GetProcedureAddress(libRuby, 'rb_mPrecision'))^;
  v_rb_eNoMethodError := PVALUE(GetProcedureAddress(libRuby, 'rb_eNoMethodError'))^;
 end;

procedure done_18_19;
 begin
  f_ruby_finalize();
 end;

const
  _Qfalse = 0;
  _Qtrue  = 2;
  _Qnil   = 4;
  _Qundef = 6;

operator = (x, y : VALUE) : Boolean; inline;
 begin
  Result := (x.data = y.data);
 end;

function Qnil : VALUE; inline;
 begin
  case Version of
       rvNone :
         errInactive;
       rvRuby18, rvRuby19 :
         Result.data := _Qnil;
       else
         errUnknown;
  end;
 end;

function Qfalse : VALUE; inline;
 begin
  case Version of
       rvNone :
         errInactive;
       rvRuby18, rvRuby19 :
         Result.data := _Qfalse;
       else
         errUnknown;
  end;
 end;

function Qtrue : VALUE; inline;
 begin
  case Version of
       rvNone :
         errInactive;
       rvRuby18, rvRuby19 :
         Result.data := _Qtrue;
       else
         errUnknown;
  end;
 end;

function Qundef : VALUE; inline;
 begin
  case Version of
       rvNone :
         errInactive;
       rvRuby18, rvRuby19 :
         Result.data := _Qundef;
       else
         errUnknown;
  end;
 end;

const
//  IMMEDIATE_MASK = $03;

  FIXNUM_FLAG = $01;
  SYMBOL_FLAG = $0E;

  T_NONE      = $00;

  T_NIL       = $01;
  T_OBJECT    = $02;
  T_CLASS     = $03;
//  T_ICLASS    = $04;
  T_MODULE    = $05;
  T_FLOAT     = $06;
  T_STRING    = $07;
  T_REGEXP    = $08;
  T_ARRAY     = $09;
  T_FIXNUM    = $0A;
  T_HASH      = $0B;
  T_STRUCT    = $0C;
  T_BIGNUM    = $0D;
  T_FILE      = $0E;

  T_TRUE      = $20;
  T_FALSE     = $21;
  T_DATA      = $22;
//  T_MATCH     = $23;
  T_SYMBOL    = $24;

//  T_BLKTAG    = $3B;
  T_UNDEF     = $3C;
//  T_VARMAP    = $3D;
//  T_SCOPE     = $3E;
//  T_NODE      = $3F;

  T_MASK      = $3F;

type
  PRBasic = ^RBasic;
  RBasic = record
    flags : PtrUInt;
    klass : VALUE;
  end;

  PRData = ^RData;
  RData = record
    basic : RBasic;
    dmark, dfree : Pdatafunc;
    data : Pointer;
  end;

function rb_type (obj : VALUE) : Integer; inline;
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
                 result := T_UNDEF;
               else
                 if (obj.data and $FF) = SYMBOL_FLAG
                    then result := T_SYMBOL
                    else result := (PRBasic(obj)^.flags and T_MASK)
          end;
 end;

type
  PStrRec = ^TStrRec;
  TStrRec = record
    str : pchar;
    val : VALUE;
  end;

function try_val2str (v : VALUE) : VALUE; cdecl;
 begin
  PStrRec(v)^.str := f_rb_string_value_cstr(PStrRec(v)^.val);
  result.data := _Qnil;
 end;

operator explicit (v : VALUE) : UTF8String;
 var
   rec : TStrRec;
   res : Integer;
 begin
  case Version of
       rvNone :
         errInactive;
       rvRuby18, rvRuby19 :
         begin
          rec.val := v;
          f_rb_protect(@try_val2str, VALUE(@rec), @res);
          chkConversion(res);
          Result := UTF8String(rec.str);
         end;
       else
         errUnknown;
  end;
 end;

operator explicit (v : VALUE) : ansistring;
 begin
  Result := UTF8String(v);
 end;

operator explicit (v : VALUE) : UnicodeString;
 begin
  Result := UTF8Decode(UTF8String(v));
 end;

operator explicit (const s : UTF8String) : VALUE;
 begin
  case Version of
       rvNone :
         errInactive;
       rvRuby18, rvRuby19 :
         Result := f_rb_str_new2(PChar(s));
       else
         errUnknown;
  end;
 end;

operator explicit (const s : ansistring) : VALUE;
 begin
  Result := VALUE(UTF8String(s));
 end;

operator explicit (const s : UnicodeString) : VALUE;
 begin
  Result := VALUE(UTF8Encode(s));
 end;

type
  PIntRec = ^TIntRec;
  TIntRec = record
    int : PtrInt;
    val : VALUE;
  end;

function try_val2int (v : VALUE) : VALUE; cdecl;
 begin
  PIntRec(v)^.int := f_rb_num2int(PIntRec(v)^.val);
  Result.data := _Qnil;
 end;

operator explicit (v : VALUE) : PtrInt;
 var
   rec : TIntRec;
   res : Integer;
 begin
  case Version of
       rvNone :
         errInactive;
       rvRuby18, rvRuby19 :
         begin
          rec.val := v;
          f_rb_protect(@try_val2int, VALUE(@rec), @res);
          chkConversion(res);
          Result := rec.int;
         end;
       else
         errUnknown;
  end;
 end;

type
  PUIntRec = ^TUIntRec;
  TUIntRec = record
    uint : PtrUInt;
    val : VALUE;
  end;

function try_val2uint (v : VALUE) : VALUE; cdecl;
 begin
  PUIntRec(v)^.uint := f_rb_num2uint(PUIntRec(v)^.val);
  Result.data := _Qnil;
 end;

operator explicit (v : VALUE) : PtrUInt;
 var
   rec : TUIntRec;
   res : Integer;
 begin
  case Version of
       rvNone :
         errInactive;
       rvRuby18, rvRuby19 :
         begin
          rec.val := v;
          f_rb_protect(@try_val2uint, VALUE(@rec), @res);
          chkConversion(res);
          Result := rec.uint;
         end;
  end;
 end;

type
  PDblRec = ^TDblRec;
  TDblRec = record
    dbl : Double;
    val : VALUE;
  end;

function try_val2dbl (v : VALUE) : VALUE; cdecl;
 begin
  PDblRec(v)^.dbl := f_rb_num2dbl(PDblRec(v)^.val);
  Result.data := _Qnil;
 end;

operator explicit (v : VALUE) : Double;
 var
   rec : TDblRec;
   res : Integer;
 begin
  case Version of
       rvNone :
         errInactive;
       rvRuby18, rvRuby19 :
         begin
          rec.val := v;
          f_rb_protect(@try_val2dbl, VALUE(@rec), @res);
          chkConversion(res);
          Result := rec.dbl;
         end;
       else
         errUnknown;
  end;
 end;

operator explicit (v : VALUE) : Boolean;
 begin
  case Version of
       rvNone :
         errInactive;
       rvRuby18, rvRuby19 :
         Result := not ((v.data = _Qnil) or (v.data = _Qfalse));
       else
         errUnknown;
  end;
 end;

operator explicit (v : VALUE) : TObject;
 var
   idx : PtrInt;
 begin
  case Version of
       rvNone :
         errInactive;
       rvRuby18, rvRuby19 :
         begin
          if v.data = _Qnil
             then exit(nil);
          if (rb_type(v) <> T_DATA) or not find_object(TObject(PRData(v)^.data), idx)
             then raise ERubyNotObject.Create(msgNotObject);
          if cacheObjects[idx].val <> v
             then raise ERubyNotObject.Create(msgNotObject);
          Result := cacheObjects[idx].obj;
         end;
       else
         errUnknown;
  end;
 end;

operator explicit (v : VALUE) : TClass;
 var
   idx : PtrInt;
 begin
  case Version of
       rvNone :
         errInactive;
       rvRuby18, rvRuby19 :
         if rb_type(v) = T_CLASS
            then begin
                  for idx := 0 to High(cacheClasses) do
                      if cacheClasses[idx].val = v
                         then exit(cacheClasses[idx].cls);
                  raise ERubyNotClass.Create(msgNotClass);
                 end
            else raise ERubyNotClass.Create(msgNotClass);
       else
         errUnknown;
  end;
 end;

operator explicit (const v : PtrInt) : VALUE;
 begin
  case Version of
       rvNone :
         errInactive;
       rvRuby18, rvRuby19 :
         Result := f_rb_int2inum(v);
       else
         errUnknown;
  end;
 end;

operator explicit (const v : PtrUInt) : VALUE;
 begin
  case Version of
       rvNone :
         errInactive;
       rvRuby18, rvRuby19 :
         Result := f_rb_uint2inum(v);
       else
         errUnknown;
  end;
 end;

operator explicit (const v : Double) : VALUE;
 begin
  case Version of
       rvNone :
         errInactive;
       rvRuby18, rvRuby19 :
         Result := f_rb_float_new(v);
       else
         errUnknown;
  end;
 end;

operator explicit (const v : Boolean) : VALUE;
 begin
  case Version of
       rvNone :
         errInactive;
       rvRuby18, rvRuby19 :
         if v
            then Result.data := _Qtrue
            else Result.data := _Qfalse;
       else
         errUnknown;
  end;
 end;

operator explicit (const v : TObject) : VALUE;
 var
   idx : PtrInt;
 begin
  case Version of
       rvNone :
         errInactive;
       rvRuby18, rvRuby19 :
         if v = nil
            then Result.data := _Qnil
            else if find_object(v, idx)
                    then Result := cacheObjects[idx].val
                    else begin
                          Result := f_rb_data_object_alloc(VALUE(v.ClassType), Pointer(v), nil, nil);
                          insert_object(v, Result, idx);
                         end;
       else
         errUnknown;
  end;
 end;

type
  PPropRec = ^TPropRec;
  TPropRec = record
    instance, id, result : VALUE;
  end;

function try_get_subcomponent(v : VALUE) : VALUE; cdecl;
 begin
  PPropRec(v)^.result := f_rb_funcall2(PPropRec(v)^.instance, ID('[]'), 1, @PPropRec(v)^.id);
  Result.data := _Qnil;
 end;

procedure do_property_get (instance : VALUE; mid : VALUE; obj : TObject; const name : ansistring; var return : VALUE);
 var
   info : PPropInfo;
   rec : TPropRec;
   res : Integer;
 begin
  info := GetPropInfo(obj, name);
  if info <> nil
     then case info^.PropType^.Kind of
               tkInteger :
                 return := VALUE(GetOrdProp(obj, info));
               tkInt64 :
                 return := VALUE(GetInt64Prop(obj, info));
               tkQWord :
                 return := VALUE(QWord(GetInt64Prop(obj, info)));
               tkEnumeration :
                 return := VALUE(ID(GetEnumProp(obj, info)));
               tkFloat :
                 return := VALUE(GetFloatProp(obj, info));
               tkSString, tkLString, tkAString :
                 return := VALUE(GetStrProp(obj, info));
               tkWString, tkUString :
                 return := VALUE(GetUnicodeStrProp(obj, info));
               tkBool :
                 return := VALUE(GetOrdProp(obj, info) <> 0);
               tkClass :
                 return := VALUE(GetObjectProp(obj, info));
          end;
  if return.data = _Qundef   // try get subcomponent
     then begin
           rec.instance := instance;
           rec.id := mid;
           f_rb_protect(@try_get_subcomponent, VALUE(@rec), @res);
           if res = 0
              then return := rec.result;
          end;
 end;

procedure do_property_set (obj : TObject; const name : ansistring; val : VALUE; var return : VALUE);
 var
   info : PPropInfo;
 begin
  info := GetPropInfo(obj, name);
  if info <> nil
     then begin
           return := val;
           case info^.PropType^.Kind of
                tkInteger :
                  SetOrdProp(obj, info, PtrInt(val));
                tkInt64 :
                  SetInt64Prop(obj, info, Int64(val));
                tkQWord :
                  SetInt64Prop(obj, info, Int64(QWord(val)));
                tkEnumeration :
                  SetEnumProp(obj, info, ansistring(ID(val)));
                tkFloat :
                  SetFloatProp(obj, info, Double(val));
                tkSString, tkLString, tkAString :
                  SetStrProp(obj, info, ansistring(val));
                tkWString, tkUString :
                  SetUnicodeStrProp(obj, info, UnicodeString(val));
                tkBool :
                  SetOrdProp(obj, info, Ord(Boolean(val)));
                tkClass :
                  SetObjectProp(obj, info, TObject(val));
                else
                  return.data := _Qundef;
           end;
          end;
 end;

function do_method_missing (argc : Integer; argv : PVALUE; instance : VALUE) : VALUE; cdecl;
 var
   msg : TRubyMsg;
   obj : TObject;
 begin
  Result.data := _Qundef;
  obj := TObject(instance);
  msg.msg := ansistring(ID(argv[0]));
  msg.argc := argc - 1;
  msg.argv := @argv[1];
  msg.result := @Result;
  obj.DispatchStr(msg);
  if Result.data = _Qundef
     then if argc = 1   // may be property get
             then do_property_get(instance, argv[0], obj, msg.msg, Result)
             else if (argc = 2) and (msg.msg[Length(msg.msg)] = '=') // may be property set
                     then do_property_set(obj, Copy(msg.msg, 1, Length(msg.msg) - 1), argv[1], Result);
  if Result.data = _Qundef
     then f_rb_raise(v_rb_eNoMethodError, 'No method ''%s'' for %s.', PChar(ansistring(msg.msg)), PChar(ansistring(f_rb_inspect(instance))));
 end;

function do_alloc (cls : VALUE) : VALUE; cdecl;
 begin
  Result := f_rb_data_object_alloc(cls, nil, nil, nil);
 end;

operator explicit (const v : TClass) : VALUE;
 var
   idx, cidx, hidx : PtrInt;
 begin
  case Version of
       rvNone :
         errInactive;
       rvRuby18, rvRuby19 :
         begin
          if v = nil
             then exit(v_rb_cObject);
          for idx := 0 to High(cacheClasses) do
              if cacheClasses[idx].cls = v
                 then exit(cacheClasses[idx].val);
          Result := f_rb_define_class(PChar(ansistring(v.ClassName)), VALUE(v.ClassParent));
          idx := Length(cacheClasses);
          SetLength(cacheClasses, idx + 1);
          cacheClasses[idx].cls := v;
          cacheClasses[idx].val := Result;
          f_rb_define_alloc_func(Result, @do_alloc);
          DefineMethod(Result, 'method_missing', @do_method_missing);
          for cidx := 0 to High(hooksClasses) do
              if hooksClasses[cidx].cls = v
                 then begin
                       for hidx := 0 to High(hooksClasses[cidx].hooks) do
                           hooksClasses[cidx].hooks[hidx](Result);
                       break;
                      end;
         end;
       else
         errUnknown;
  end;
 end;

{$IFDEF CPU32}

type
  PI64Rec = ^TI64Rec;
  TI64Rec = record
    i64 : Int64;
    val : VALUE;
  end;

function try_val2i64 (v : VALUE) : VALUE; cdecl;
 begin
  PI64Rec(v)^.i64 := f_rb_num2ll(PI64Rec(v)^.val);
  Result := Qnil;
 end;

operator explicit (v : VALUE) : Int64;
 var
   rec : TI64Rec;
   res : Integer;
 begin
  case Version of
       rvNone :
         errInactive;
       rvRuby18, rvRuby19 :
         begin
          rec.val := v;
          f_rb_protect(@try_val2i64, VALUE(@rec), @res);
          chkConversion(res);
          Result := rec.i64;
         end;
  end;
 end;

type
  PW64Rec = ^TW64Rec;
  TW64Rec = record
    w64 : QWord;
    val : VALUE;
  end;

function try_val2w64 (v : VALUE) : VALUE; cdecl;
 begin
  PW64Rec(v)^.w64 := f_rb_num2ull(PW64Rec(v)^.val);
  Result := Qnil;
 end;

operator explicit (v : VALUE) : QWord;
 var
   rec : TW64Rec;
   res : Integer;
 begin
  case Version of
       rvNone :
         errInactive;
       rvRuby18, rvRuby19 :
         begin
          rec.val := v;
          f_rb_protect(@try_val2w64, VALUE(@rec), @res);
          chkConversion(res);
          Result := rec.w64;
         end;
  end;
 end;

operator explicit (const v : Int64) : VALUE;
 begin
  case Version of
       rvNone :
         errInactive;
       rvRuby18, rvRuby19 :
         Result := f_rb_ll2inum(v);
       else
         errUnknown;
  end;
 end;

operator explicit (const v : QWord) : VALUE;
 begin
  case Version of
       rvNone :
         errInactive;
       rvRuby18, rvRuby19 :
         Result := f_rb_ull2inum(v);
       else
         errUnknown;
  end;
 end;
{$ENDIF CPU32}

type
  PIDRec = ^TIDRec;
  TIDRec = record
    id : ID;
    val : VALUE;
  end;

function try_val2id (v : VALUE) : VALUE; cdecl;
 begin
  PIDRec(v)^.id := f_rb_to_id(PIDRec(v)^.val);
  Result.data := _Qnil;
 end;

operator explicit (v : VALUE) : ID;
 var
   rec : TIDRec;
   res : Integer;
 begin
  case Version of
       rvNone :
         errInactive;
       rvRuby18, rvRuby19 :
         begin
          rec.val := v;
          f_rb_protect(@try_val2id, VALUE(@rec), @res);
          chkConversion(res);
          Result := rec.id;
         end
       else
         errUnknown;
  end;
 end;

operator explicit (v : ID) : VALUE;
 begin
  case Version of
       rvNone :
         errInactive;
       rvRuby18, rvRuby19 :
         Result.data := (v.data shl 8) or SYMBOL_FLAG;
       else
         errUnknown;
  end;
 end;

operator explicit (v : ID) : ansistring;
 begin
  case Version of
       rvNone :
         errInactive;
       rvRuby18, rvRuby19 :
         Result := ansistring(f_rb_id2name(v));
       else
         errUnknown;
  end;
 end;

operator explicit (const v : ansistring) : ID;
 begin
  case Version of
       rvNone :
         errInactive;
       rvRuby18, rvRuby19 :
         Result := f_rb_intern(PChar(v));
       else
         errUnknown;
  end;
 end;

type
  PMethodRec = ^TMethodRec;
  TMethodRec = record
    value  : VALUE;
    name   : PChar;
    method : Pointer;
    argc   : Integer;
  end;

function try_define_method (v : VALUE) : VALUE; cdecl;
 begin
  f_rb_define_method(PMethodRec(v)^.value, PMethodRec(v)^.name, PMethodRec(v)^.method, PMethodRec(v)^.argc);
  Result.data := _Qnil;
 end;

procedure protected_define_method (module : VALUE; name : PChar; method : Pointer; argc : Integer);
 var
   rec : TMethodRec;
   res : Integer;
 begin
  case Version of
       rvNone :
         errInactive;
       rvRuby18, rvRuby19 :
         begin
          rec.value := module;
          rec.name := name;
          rec.method := method;
          rec.argc := argc;
          f_rb_protect(@try_define_method, VALUE(@rec), @res);
          if res <> 0
             then raise ERubyDefinition.Create(msgDefineError);
         end;
       else
         errUnknown;
  end;
 end;

procedure DefineMethod (module : VALUE; const name : ansistring; method : TRubyMethod0);
 begin
  protected_define_method(module, PChar(name), Pointer(method), 0);
 end;

procedure DefineMethod (module : VALUE; const name : ansistring; method: TRubyMethod1);
 begin
  protected_define_method(module, PChar(name), Pointer(method), 1);
 end;

procedure DefineMethod (module : VALUE; const name : ansistring; method: TRubyMethod2);
 begin
  protected_define_method(module, PChar(name), Pointer(method), 2);
 end;

procedure DefineMethod (module : VALUE; const name : ansistring; method: TRubyMethod3);
 begin
  protected_define_method(module, PChar(name), Pointer(method), 3);
 end;

procedure DefineMethod (module : VALUE; const name : ansistring; method: TRubyMethod4);
 begin
  protected_define_method(module, PChar(name), Pointer(method), 4);
 end;

procedure DefineMethod (module : VALUE; const name : ansistring; method: TRubyMethod5);
 begin
  protected_define_method(module, PChar(name), Pointer(method), 5);
 end;

procedure DefineMethod (module : VALUE; const name : ansistring; method: TRubyMethod6);
 begin
  protected_define_method(module, PChar(name), Pointer(method), 6);
 end;

procedure DefineMethod (module : VALUE; const name : ansistring; method: TRubyMethod7);
 begin
  protected_define_method(module, PChar(name), Pointer(method), 7);
 end;

procedure DefineMethod (module : VALUE; const name : ansistring; method: TRubyMethod8);
 begin
  protected_define_method(module, PChar(name), Pointer(method), 8);
 end;

procedure DefineMethod (module : VALUE; const name : ansistring; method: TRubyMethod9);
 begin
  protected_define_method(module, PChar(name), Pointer(method), 9);
 end;

procedure DefineMethod (module : VALUE; const name : ansistring; method: TRubyMethod10);
 begin
  protected_define_method(module, PChar(name), Pointer(method), 10);
 end;

procedure DefineMethod (module : VALUE; const name : ansistring; method: TRubyMethod11);
 begin
  protected_define_method(module, PChar(name), Pointer(method), 11);
 end;

procedure DefineMethod (module : VALUE; const name : ansistring; method: TRubyMethod12);
 begin
  protected_define_method(module, PChar(name), Pointer(method), 12);
 end;

procedure DefineMethod (module : VALUE; const name : ansistring; method: TRubyMethod13);
 begin
  protected_define_method(module, PChar(name), Pointer(method), 13);
 end;

procedure DefineMethod (module : VALUE; const name : ansistring; method: TRubyMethod14);
 begin
  protected_define_method(module, PChar(name), Pointer(method), 14);
 end;

procedure DefineMethod (module : VALUE; const name : ansistring; method: TRubyMethod15);
 begin
  protected_define_method(module, PChar(name), Pointer(method), 15);
 end;

procedure DefineMethod (module : VALUE; const name : ansistring; method: TRubyMethod16);
 begin
  protected_define_method(module, PChar(name), Pointer(method), 16);
 end;

procedure DefineMethod (module : VALUE; const name : ansistring; method: TRubyMethod17);
 begin
  protected_define_method(module, PChar(name), Pointer(method), 17);
 end;

procedure DefineMethod (module : VALUE; const name : ansistring; method: TRubyMethodArr);
 begin
  protected_define_method(module, PChar(name), Pointer(method), -1);
  // TODO: check for 1.9 — may be wrong!
 end;

function try_define_singleton_method (v : VALUE) : VALUE; cdecl;
 begin
  f_rb_define_singleton_method(PMethodRec(v)^.value, PMethodRec(v)^.name, PMethodRec(v)^.method, PMethodRec(v)^.argc);
  Result.data := _Qnil;
 end;

procedure protected_define_singleton_method (instance : VALUE; name : PChar; method : Pointer; argc : Integer);
 var
   rec : TMethodRec;
   res : Integer;
 begin
  case Version of
       rvNone :
         errInactive;
       rvRuby18, rvRuby19 :
         begin
          rec.value := instance;
          rec.name := name;
          rec.method := method;
          rec.argc := argc;
          f_rb_protect(@try_define_singleton_method, VALUE(@rec), @res);
          if res <> 0
             then raise ERubyDefinition.Create(msgDefineError);
         end;
       else
         errUnknown;
  end;
 end;

procedure DefineSingletonMethod (instance : VALUE; const name : ansistring; method : TRubyMethod0);
 begin
  protected_define_singleton_method(instance, PChar(name), Pointer(method), 0);
 end;

procedure DefineSingletonMethod (instance : VALUE; const name : ansistring; method : TRubyMethod1);
 begin
  protected_define_singleton_method(instance, PChar(name), Pointer(method), 1);
 end;

procedure DefineSingletonMethod (instance : VALUE; const name : ansistring; method : TRubyMethod2);
 begin
  protected_define_singleton_method(instance, PChar(name), Pointer(method), 2);
 end;

procedure DefineSingletonMethod (instance : VALUE; const name : ansistring; method : TRubyMethod3);
 begin
  protected_define_singleton_method(instance, PChar(name), Pointer(method), 3);
 end;

procedure DefineSingletonMethod (instance : VALUE; const name : ansistring; method : TRubyMethod4);
 begin
  protected_define_singleton_method(instance, PChar(name), Pointer(method), 4);
 end;

procedure DefineSingletonMethod (instance : VALUE; const name : ansistring; method : TRubyMethod5);
 begin
  protected_define_singleton_method(instance, PChar(name), Pointer(method), 5);
 end;

procedure DefineSingletonMethod (instance : VALUE; const name : ansistring; method : TRubyMethod6);
 begin
  protected_define_singleton_method(instance, PChar(name), Pointer(method), 6);
 end;

procedure DefineSingletonMethod (instance : VALUE; const name : ansistring; method : TRubyMethod7);
 begin
  protected_define_singleton_method(instance, PChar(name), Pointer(method), 7);
 end;

procedure DefineSingletonMethod (instance : VALUE; const name : ansistring; method : TRubyMethod8);
 begin
  protected_define_singleton_method(instance, PChar(name), Pointer(method), 8);
 end;

procedure DefineSingletonMethod (instance : VALUE; const name : ansistring; method : TRubyMethod9);
 begin
  protected_define_singleton_method(instance, PChar(name), Pointer(method), 9);
 end;

procedure DefineSingletonMethod (instance : VALUE; const name : ansistring; method : TRubyMethod10);
 begin
  protected_define_singleton_method(instance, PChar(name), Pointer(method), 10);
 end;

procedure DefineSingletonMethod (instance : VALUE; const name : ansistring; method : TRubyMethod11);
 begin
  protected_define_singleton_method(instance, PChar(name), Pointer(method), 11);
 end;

procedure DefineSingletonMethod (instance : VALUE; const name : ansistring; method : TRubyMethod12);
 begin
  protected_define_singleton_method(instance, PChar(name), Pointer(method), 12);
 end;

procedure DefineSingletonMethod (instance : VALUE; const name : ansistring; method : TRubyMethod13);
 begin
  protected_define_singleton_method(instance, PChar(name), Pointer(method), 13);
 end;

procedure DefineSingletonMethod (instance : VALUE; const name : ansistring; method : TRubyMethod14);
 begin
  protected_define_singleton_method(instance, PChar(name), Pointer(method), 14);
 end;

procedure DefineSingletonMethod (instance : VALUE; const name : ansistring; method : TRubyMethod15);
 begin
  protected_define_singleton_method(instance, PChar(name), Pointer(method), 15);
 end;

procedure DefineSingletonMethod (instance : VALUE; const name : ansistring; method : TRubyMethod16);
 begin
  protected_define_singleton_method(instance, PChar(name), Pointer(method), 16);
 end;

procedure DefineSingletonMethod (instance : VALUE; const name : ansistring; method : TRubyMethod17);
 begin
  protected_define_singleton_method(instance, PChar(name), Pointer(method), 17);
 end;

procedure DefineSingletonMethod (instance : VALUE; const name : ansistring; method : TRubyMethodArr);
 begin
  protected_define_singleton_method(instance, PChar(name), Pointer(method), -1);
 end;

procedure DefineAlias(module : VALUE; const newname, oldname : ansistring);
 begin
  case Version of
       rvNone :
         errInactive;
       rvRuby18, rvRuby19 :
         f_rb_define_alias(module, PChar(newname), PChar(oldname));
       else
         errUnknown;
  end;
 end;

procedure IncludeModule(cls : VALUE; module : VALUE);
 begin
  case Version of
       rvNone :
         errInactive;
       rvRuby18, rvRuby19 :
         f_rb_include_module(cls, module);
       else
         errUnknown;
  end;
 end;

function ModComparable : VALUE;
 begin
  case Version of
       rvNone :
         errInactive;
       rvRuby18, rvRuby19 :
         Result := v_rb_mComparable;
       else
         errUnknown;
  end;
 end;

function ModEnumerable : VALUE;
 begin
  case Version of
       rvNone :
         errInactive;
       rvRuby18, rvRuby19 :
         Result := v_rb_mEnumerable;
       else
         errUnknown;
  end;
 end;

function ModPrecision : VALUE;
 begin
  case Version of
       rvNone :
         errInactive;
       rvRuby18, rvRuby19 :
         Result := v_rb_mPrecision;
       else
         errUnknown;
  end;
 end;

function Version : TRubyVersion; inline;
 begin
  Result := verRuby;
 end;

function Load (ver : TRubyVersion) : Boolean;
 var
   idx : PtrInt;
 begin
  if getActive
     then raise ERubyAlreadyLoaded.Create(msgAlreadyLoaded);
  if not (ver in [succ(rvNone)..high(TRubyVersion)])
     then errUnknown;
  libRuby := LoadLibrary(LIBRARIES[ver].name);
  Result := (libRuby <> 0);
  if Result
     then begin
           verRuby := ver;
           LIBRARIES[ver].init();
           for idx := 0 to High(hooksLoad) do
               hooksLoad[idx]();
          end;
 end;

function Load : TRubyVersion;
 var
   ver : TRubyVersion;
 begin
  if getActive
     then raise ERubyAlreadyLoaded.Create(msgAlreadyLoaded);
  for ver := high(TRubyVersion) downto succ(rvNone) do
      if Load(ver)
         then break;
  Result := verRuby;
 end;

procedure Unload;
 var
   idx : PtrInt;
 begin
  if not getActive
     then errInactive;
  for idx := 0 to High(hooksUnload) do
      hooksUnload[idx]();
  SetLength(cacheClasses, 0);
  SetLength(cacheObjects, 0);
  LIBRARIES[verRuby].done();
  UnloadLibrary(libRuby);
  libRuby := 0;
  verRuby := rvNone;
 end;

function getActive: Boolean;
 begin
  Result := (Version <> rvNone);
 end;

procedure setActive (val : Boolean);
 begin
  if val <> getActive()
     then if val
             then begin
                   if Load = rvNone
                      then raise ELibraryError.Create(msgActivateError);
                  end
             else Unload
 end;

procedure AddLoadHook (hook : TLoadHook);
 var
   len : PtrInt;
 begin
  if hook <> nil
     then begin
           len := Length(hooksLoad);
           SetLength(hooksLoad, len + 1);
           hooksLoad[len] := hook;
          end;
 end;

procedure RemoveLoadHook (hook : TLoadHook);
 var
   h, idx, delidx : PtrInt;
 begin
  if hook <> nil
     then begin
           h := High(hooksLoad);
           for idx := h downto 0 do
               if hooksLoad[idx] = hook
                  then begin
                        for delidx := idx to h - 1 do
                            hooksLoad[delidx] := hooksLoad[delidx + 1];
                        SetLength(hooksLoad, h);
                        exit;
                       end;
          end;
 end;

procedure AddUnloadHook(hook : TUnloadHook);
 var
   len : PtrInt;
 begin
  if hook <> nil
     then begin
           len := Length(hooksUnload);
           SetLength(hooksUnload, len + 1);
           hooksUnload[len] := hook;
          end;
 end;

procedure RemoveUnloadHook(hook : TUnloadHook);
 var
   h, idx, delidx : PtrInt;
 begin
  if hook <> nil
     then begin
           h := High(hooksUnload);
           for idx := h downto 0 do
               if hooksUnload[idx] = hook
                  then begin
                        for delidx := idx to h - 1 do
                            hooksUnload[delidx] := hooksUnload[delidx + 1];
                        SetLength(hooksUnload, h);
                        exit;
                       end;
          end;
 end;

procedure AddClassHook(cls : TClass; hook : TClassHook);
 var
   len, idx : PtrInt;
 begin
  len := Length(hooksClasses);
  for idx := 0 to len - 1 do
      if hooksClasses[idx].cls = cls
         then begin
               len := Length(hooksClasses[idx].hooks);
               SetLength(hooksClasses[idx].hooks, len + 1);
               hooksClasses[idx].hooks[len] := hook;
               exit;
              end;
  SetLength(hooksClasses, len + 1);
  hooksClasses[len].cls := cls;
  SetLength(hooksClasses[len].hooks, 1);
  hooksClasses[len].hooks[0] := hook;
 end;

procedure RemoveClassHook(cls : TClass; hook : TClassHook);
 var
   h, clsidx, idx, delidx : PtrInt;
 begin
  for clsidx := 0 to High(hooksClasses) do
      if hooksClasses[clsidx].cls = cls
         then begin
               h := High(hooksClasses[clsidx].hooks);
               for idx := h downto 0 do
                   if hooksClasses[clsidx].hooks[idx] = hook
                      then begin
                            for delidx := idx to h - 1 do
                                hooksClasses[clsidx].hooks[delidx] := hooksClasses[clsidx].hooks[delidx + 1];
                            SetLength(hooksClasses[clsidx].hooks, h);
                            exit;
                           end;
              end;
 end;

function ErrorInfo : VALUE;
 begin
  case Version of
       rvRuby18, rvRuby19 :
         Result := p_ruby_errinfo^;
       else
         Result.data := 0;
  end;
 end;

function Inspect (v : VALUE) : VALUE;
 begin
  case Version of
       rvNone :
         errInactive;
       rvRuby18, rvRuby19 :
         result := f_rb_inspect(v);
       else
         errUnknown;
  end;
 end;

function Yield (v : VALUE) : VALUE;
 begin
  case Version of
       rvNone :
         errInactive;
       rvRuby18, rvRuby19 :
         Result := f_rb_yield(v);
       else
         errUnknown;
  end;
 end;

procedure WrapObject (val : VALUE; obj : TObject);
 var
   idx : PtrInt;
 begin
  case Version of
       rvNone :
         errInactive;
       rvRuby18, rvRuby19 :
         begin
          if rb_type(val) <> T_DATA
             then raise EIncompatibleType.Create(msgIncompatible);
          PRData(val)^.data := Pointer(obj);
          if find_object(obj, idx)
             then cacheObjects[idx].val := val
             else insert_object(obj, val, idx);
         end;
  end;
 end;

function MakeArray (const vals : array of VALUE) : VALUE;
 begin
  case Version of
       rvNone :
         errInactive;
       rvRuby18, rvRuby19 :
         Result := f_rb_ary_new4(Length(vals), @vals[0]);
       else
         errUnknown;
  end;
 end;

function MakeHash : VALUE;
 begin
  case Version of
       rvNone :
         errInactive;
       rvRuby18, rvRuby19 :
         Result := f_rb_hash_new();
       else
         errUnknown;
  end;
 end;

function HashGet (hash : VALUE; key : VALUE) : VALUE;
 begin
  case Version of
       rvNone :
         errInactive;
       rvRuby18, rvRuby19 :
         Result := f_rb_hash_aref(hash, key);
       else
         errUnknown;
  end;
 end;

function HashSet (hash : VALUE; key, val : VALUE) : VALUE;
 begin
  case Version of
       rvNone :
         errInactive;
       rvRuby18, rvRuby19 :
         Result := f_rb_hash_aset(hash, key, val);
       else
         errUnknown;
  end;
 end;

function ValType (v : VALUE) : TRubyValueType;
 begin
  case Version of
       rvNone :
         errInactive;
       rvRuby18, rvRuby19 :
         case rb_type(v) of
              T_NONE :
                Result := rtNone;
              T_NIL :
                Result := rtNil;
              T_TRUE :
                Result := rtTrue;
              T_FALSE :
                Result := rtFalse;
              T_FIXNUM :
                Result := rtFixNum;
              T_BIGNUM :
                Result := rtBigNum;
              T_REGEXP :
                Result := rtRegExp;
              T_ARRAY :
                Result := rtArray;
              T_HASH :
                Result := rtHash;
              T_STRUCT :
                Result := rtStruct;
              T_STRING :
                Result := rtString;
              T_FILE :
                Result := rtFile;
              T_SYMBOL :
                Result := rtSymbol;
              T_OBJECT :
                Result := rtObject;
              T_CLASS :
                Result := rtClass;
              T_MODULE :
                Result := rtModule;
              T_FLOAT :
                Result := rtFloat;
              T_DATA :
                Result := rtData;
              else
                Result := rtUndef;
         end;
       else
         errUnknown;
  end;
 end;

function rb_class_of (obj : VALUE) : VALUE; inline;
 begin
  if (obj.data and FIXNUM_FLAG) <> 0
     then result := v_rb_cFixnum
     else case obj.data of
               _Qnil :
                 result := v_rb_cNilClass;
               _Qfalse :
                 result := v_rb_cFalseClass;
               _Qtrue :
                 result := v_rb_cTrueClass
               else
                 if (obj.data and $FF) = $0E
                    then result := v_rb_cSymbol
                    else result := PRBasic(obj)^.klass
        end;
    end;

function ValClass(v : VALUE) : VALUE;
 begin
  case Version of
       rvNone :
         errInactive;
       rvRuby18, rvRuby19 :
         Result := rb_class_of(v);
       else
         errUnknown;
  end;
 end;

{ ERubyError }

constructor ERubyError.Create(const msg : UTF8String);
 begin
  case Version of
       rvRuby18, rvRuby19 :
         begin
          fldErrInfo := ErrorInfo;
          if (fldErrInfo.data = _Qnil) or (fldErrInfo.data = _Qundef)
             then inherited Create(msg)
             else inherited Create(msg + LineEnding + LineEnding + UTF8String(Inspect(fldErrInfo)));
         end;
       else
         inherited Create(msg);
  end;
 end;

constructor ERubyError.CreateFmt (const msg : UTF8String; const args : array of const);
 begin
  Create(Format(msg, args));
 end;

end.
