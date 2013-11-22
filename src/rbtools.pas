{$include rbopts.inc}

unit RbTools;

interface

uses
  SysUtils, Ruby;

var
  pp_mPascal : VALUE;
  pp_ePascalError : VALUE;

type
  TWrapper = procedure is nested;

procedure pp_protect (wrapper : TWrapper);
procedure pp_try (wrapper : TWrapper);

// shortcuts for ruby functions (unwrapped!)
function IV (x : PtrInt ) : VALUE; inline;
function UV (x : PtrUInt) : VALUE; inline;
function BV (x : Boolean) : VALUE; inline;
function DV (x : Double ) : VALUE; inline;
function SV (x : PChar)   : VALUE; inline;
function SY (x : PChar)   : VALUE; inline;
function VI (x : VALUE) : PtrInt;  inline;
function VU (x : VALUE) : PtrUInt; inline;
function VB (x : VALUE) : Boolean; inline;
function VD (x : VALUE) : Double;  inline;
function VP (x : VALUE) : PChar;   inline;
function YP (x : VALUE) : PChar;   inline;
function SD (x : PChar) : ID; inline;
function DP (x : ID) : PChar; inline;

// conversion functions (wrapped!)
function Int2Val  (const x : PtrInt       ) : VALUE;
function UInt2Val (const x : PtrUInt      ) : VALUE;
function Bool2Val (const x : Boolean      ) : VALUE;
function Dbl2Val  (const x : Double       ) : VALUE;
function Str2Val  (const x : UTF8String   ) : VALUE;
function UStr2Val (const x : UnicodeString) : VALUE;
function Str2Sym  (const x : UTF8String   ) : VALUE;
function UStr2Sym (const x : UnicodeString) : VALUE;
function Val2Int  (v : VALUE) : PtrInt;
function Val2UInt (v : VALUE) : PtrUInt;
function Val2Bool (v : VALUE) : Boolean;
function Val2Dbl  (v : VALUE) : Double;
function Val2Str  (v : VALUE) : UTF8String;
function Val2UStr (v : VALUE) : UnicodeString;
function Sym2Str  (v : VALUE) : UTF8String;
function Sym2UStr (v : VALUE) : UnicodeString;

function Str2ID (const x : UTF8String) : ID;
function UStr2ID (const x : UnicodeString) : ID;
function ID2Str (id : ID) : UTF8String;
function ID2UStr (id : ID) : UnicodeString;

// other helpers (unwrapped!)
function C (obj : VALUE; method : PChar; const args : array of VALUE) : VALUE;
function S (v : VALUE) : PChar; inline;
function XS (v : VALUE) : PChar; inline;

// other functions (wrapped!)
function pp_inspect (v : VALUE) : UTF8String;
function pp_eval (const code : UTF8String) : VALUE;
function pp_call (obj : VALUE; const method : UTF8String;
                          const args : array of VALUE) : VALUE;
function pp_send (obj : VALUE; const method : UTF8String;
                          const args : array of VALUE) : VALUE;
function pp_to_s (obj : VALUE) : UTF8String;

// attribute setup
type
  pp_getter = function (obj : VALUE) : VALUE; cdecl;
  pp_setter = function (obj, value : VALUE) : VALUE; cdecl;
procedure pp_define_attr (klass : VALUE; name : PChar;
                          getter : pp_getter; setter : pp_setter);

type
  pp_b_getter = function (obj, idx : VALUE) : VALUE; cdecl;
  pp_b_setter = function (obj, idx, value : VALUE) : VALUE; cdecl;
procedure pp_define_braces (klass : VALUE;
                          getter : pp_b_getter; setter : pp_b_setter);

implementation

function protected_wrapper (v : VALUE) : VALUE; cdecl;
 type
   PWrapper = ^TWrapper;
 begin
  PWrapper(v)^();
  Result := Qnil;
 end;

procedure pp_protect (wrapper : TWrapper);
 var
   res : Integer;
 begin
  rb_protect(@protected_wrapper, VALUE(@wrapper), @res);
  pp_check_result(res);
 end;

const
  msgPascalError = 'Error in pascal code [%s]:' + LineEnding + '%s';

procedure pp_try (wrapper : TWrapper);
 begin
  try
   wrapper();
  except
    on ex : Exception do
       rb_raise(pp_ePascalError, msgPascalError,
                PChar(AnsiString(ex.ClassName)), PChar(ex.Message));
  end;
 end;

var
  utf8 : Pointer = nil;

function IV (x : PtrInt) : VALUE; inline;
 begin
  Result := rb_int2inum(x);
 end;

function UV(x : PtrUInt) : VALUE; inline;
 begin
  Result := rb_uint2inum(x);
 end;

function BV(x : Boolean) : VALUE; inline;
 begin
  if x
     then Result := Qtrue
     else Result := Qfalse;
 end;

function DV(x : Double) : VALUE; inline;
 begin
  Result := p_rb_float_new(x);
 end;

function SV(x : PChar) : VALUE; inline;
 begin
  Result := rb_enc_str_new(x, strlen(x), utf8);
 end;

function SY(x : PChar) : VALUE; inline;
 begin
  Result := ID2SYM(SD(x));
 end;

function VI(x : VALUE) : PtrInt; inline;
 begin
  Result := rb_num2int(x);
 end;

function VU(x : VALUE) : PtrUInt; inline;
 begin
  Result := rb_num2uint(x);
 end;

function VB(x : VALUE) : Boolean; inline;
 begin
  Result := RTEST(x);
 end;

function VD(x : VALUE) : Double; inline;
 begin
  Result := rb_num2dbl(x);
 end;

function VP(x : VALUE) : PChar; inline;
 begin
  Result := rb_string_value_cstr(@x);
 end;

function YP(x : VALUE) : PChar; inline;
 begin
  Result := DP(SYM2ID(x));
 end;

function SD (x : PChar) : ID; inline;
 begin
  Result := rb_intern3(PChar(x), strlen(PChar(x)), utf8);
 end;

function DP(x : ID) : PChar; inline;
 begin
  Result := rb_id2name(x);
 end;

function Int2Val(const x : PtrInt) : VALUE;

 procedure _;
  begin
   Result := IV(x);
  end;

 begin
  pp_protect(@_);
 end;

function UInt2Val(const x : PtrUInt) : VALUE;

 procedure _;
  begin
   Result := UV(x);
  end;

 begin
  pp_protect(@_);
 end;

function Bool2Val(const x : Boolean) : VALUE;

 procedure _;
  begin
   Result := BV(x)
  end;

 begin
  pp_protect(@_);
 end;

function Dbl2Val(const x : Double) : VALUE;

 procedure _;
  begin
   Result := DV(x);
  end;

 begin
  pp_protect(@_);
 end;

function Str2Val(const x : UTF8String) : VALUE;

 procedure _;
  begin
   Result := SV(PChar(x))
  end;

 begin
  pp_protect(@_);
 end;

function UStr2Val(const x : UnicodeString) : VALUE;
 begin
  Result := Str2Val(UTF8Encode(x));
 end;

function Str2Sym(const x : UTF8String) : VALUE;

 procedure _;
  begin
   Result := SY(PChar(x));
  end;

 begin
  pp_protect(@_);
 end;

function UStr2Sym(const x : UnicodeString) : VALUE;
 begin
  Result := Str2Sym(UTF8Encode(x));
 end;

function Val2Int(v : VALUE) : PtrInt;

 procedure _;
  begin
   Result := VI(v);
  end;

 begin
  pp_protect(@_);
 end;

function Val2UInt(v : VALUE) : PtrUInt;

 procedure _;
  begin
   Result := VU(v);
  end;

 begin
  pp_protect(@_);
 end;

function Val2Bool(v : VALUE) : Boolean;

 procedure _;
  begin
   Result := VB(v);
  end;

 begin
  pp_protect(@_);
 end;

function Val2Dbl(v : VALUE) : Double;

 procedure _;
  begin
   Result := VD(v);
  end;

 begin
  pp_protect(@_);
 end;

function pr_val2str (v : VALUE) : VALUE; cdecl;
 begin
  Result := VALUE(VP(v));
 end;

function Val2Str(v : VALUE) : UTF8String;

 procedure _;
  begin
   Result := '' + VP(v);
  end;

 begin
  pp_protect(@_);
 end;

function Val2UStr(v : VALUE) : UnicodeString;
 begin
  Result := UTF8Decode(Val2Str(v));
 end;

function Sym2Str(v : VALUE) : UTF8String;

 procedure _;
  begin
   Result := YP(v);
  end;

 begin
  pp_protect(@_);
 end;

function Sym2UStr(v : VALUE) : UnicodeString;
 begin
  Result := UTF8Decode(Sym2Str(v));
 end;

function Str2ID(const x : UTF8String) : ID;

 procedure _;
  begin
   Result := SD(PChar(x));
  end;

 begin
  pp_protect(@_);
 end;

function UStr2ID(const x : UnicodeString) : ID;
 begin
  Result := Str2ID(UTF8Encode(x));
 end;

function ID2Str (id : ID) : UTF8String;

 procedure _;
  begin
   Result := DP(id);
  end;

 begin
  pp_protect(@_);
 end;

function ID2UStr (id : ID) : UnicodeString;
 begin
  Result := UTF8Decode(ID2Str(id));
 end;

function C(obj : VALUE; method : PChar;
  const args : array of VALUE) : VALUE;
 begin
  Result := rb_funcall2(obj, SD(method), Length(args), @args[0]);
 end;

function S(v : VALUE) : PChar; inline;
 begin
  Result := VP(rb_inspect(v));
 end;

function XS(v : VALUE) : PChar; inline;
 begin
  Result := VP(C(v, 'to_s', []));
 end;

function pp_inspect (v : VALUE) : UTF8String;

 procedure _;
  begin
   Result := S(v);
  end;

 begin
  pp_protect(@_);
 end;

function pp_eval(const code : UTF8String) : VALUE;

 procedure _;
  var
    s : VALUE;
  begin
   s := SV(PChar(code));
   Result := rb_funcall2(rb_mKernel, SD('eval'), 1, @s);
  end;

 begin
  pp_protect(@_);
 end;

function pp_call (obj : VALUE; const method : UTF8String;
  const args : array of VALUE) : VALUE;

 procedure _;
  begin
   Result := rb_funcall3(obj, SD(PChar(method)), Length(args), @args[0]);
  end;

 begin
  pp_protect(@_);
 end;

function pp_send (obj : VALUE; const method : UTF8String;
  const args : array of VALUE) : VALUE;

 procedure _;
  begin
   Result := C(obj, PChar(method), args);
  end;

 begin
  pp_protect(@_);
 end;

function pp_to_s(obj : VALUE) : UTF8String;

 procedure _;
  begin
   Result := XS(obj);
  end;

 begin
  pp_protect(@_);
 end;

function B(x : Boolean) : Integer; inline;
 begin
  if x
     then Result := 1
     else Result := 0;
 end;

procedure pp_define_attr(klass : VALUE; name : PChar; getter : pp_getter;
  setter : pp_setter);
 begin
  rb_define_attr(klass, name, B(getter <> nil), B(setter <> nil));
  if getter <> nil
     then rb_define_method(klass, name, getter, 0);
  if setter <> nil
     then rb_define_method(klass, PChar(name + '='), setter, 1);
 end;

procedure pp_define_braces(klass : VALUE; getter : pp_b_getter;
  setter : pp_b_setter);
 begin
  if getter <> nil
     then rb_define_method(klass, '[]', getter, 1);
  if setter <> nil
     then rb_define_method(klass, '[]=', setter, 2);
 end;

procedure init;

 procedure _;
  begin
   pp_mPascal := rb_define_module('Pascal');
   pp_ePascalError := rb_define_class_under(pp_mPascal, 'Error',
                                            rb_eStandardError);
   utf8 := rb_utf8_encoding();
   rb_enc_set_default_external(SV('UTF-8'));
   rb_enc_set_default_internal(SV('UTF-8'));
  end;

 begin
  pp_protect(@_);
 end;

initialization
 pp_reg_init_hook(@init);
end.

