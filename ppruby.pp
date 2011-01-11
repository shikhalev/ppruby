(*
   Package : RubyFCL
   File : ppruby.pp
   Desc : core unit of ruby binding
   License : GNU GPL
*)

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

operator = (x, y : VALUE) : Boolean; inline;

function Qnil : VALUE; inline;
function Qfalse : VALUE; inline;
function Qtrue : VALUE; inline;
function Qundef : VALUE; inline;

operator explicit (v : VALUE) : UTF8String;
operator explicit (v : VALUE) : ansistring;

type
  TRubyVersion = (
    rvNone,
    rvRuby18, rvRuby19
    (* , future versions ... *)
  );

  TRubyMethod0   = function (slf : VALUE) : VALUE; cdecl;
  TRubyMethod1   = function (slf : VALUE; a : VALUE) : VALUE; cdecl;
  TRubyMethod2   = function (slf : VALUE; a, b : VALUE) : VALUE; cdecl;
  TRubyMethod3   = function (slf : VALUE; a, b, c : VALUE) : VALUE; cdecl;
  TRubyMethod4   = function (slf : VALUE; a, b, c, d : VALUE) : VALUE; cdecl;
  TRubyMethod5   = function (slf : VALUE; a, b, c, d, e : VALUE) : VALUE; cdecl;
  TRubyMethod6   = function (slf : VALUE; a, b, c, d, e, f : VALUE) : VALUE; cdecl;
  TRubyMethod7   = function (slf : VALUE; a, b, c, d, e, f, g : VALUE) : VALUE; cdecl;
  TRubyMethod8   = function (slf : VALUE; a, b, c, d, e, f, g, h : VALUE) : VALUE; cdecl;
  TRubyMethod9   = function (slf : VALUE; a, b, c, d, e, f, g, h, i : VALUE) : VALUE; cdecl;
  TRubyMethod10  = function (slf : VALUE; a, b, c, d, e, f, g, h, i, j : VALUE) : VALUE; cdecl;
  TRubyMethod11  = function (slf : VALUE; a, b, c, d, e, f, g, h, i, j, k : VALUE) : VALUE; cdecl;
  TRubyMethod12  = function (slf : VALUE; a, b, c, d, e, f, g, h, i, j, k, l : VALUE) : VALUE; cdecl;
  TRubyMethod13  = function (slf : VALUE; a, b, c, d, e, f, g, h, i, j, k, l, m : VALUE) : VALUE; cdecl;
  TRubyMethod14  = function (slf : VALUE; a, b, c, d, e, f, g, h, i, j, k, l, m, n : VALUE) : VALUE; cdecl;
  TRubyMethod15  = function (slf : VALUE; a, b, c, d, e, f, g, h, i, j, k, l, m, n, o : VALUE) : VALUE; cdecl;
  TRubyMethod16  = function (slf : VALUE; a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p : VALUE) : VALUE; cdecl;
  TRubyMethod17  = function (slf : VALUE; a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q : VALUE) : VALUE; cdecl;
  TRubyMethodArr = function (argc : LongInt; argv : PVALUE; slf : VALUE) : VALUE; cdecl;

procedure DefineMethod (cls : VALUE; method : TRubyMethod0);
procedure DefineMethod (cls : VALUE; method : TRubyMethod1);
procedure DefineMethod (cls : VALUE; method : TRubyMethod2);
procedure DefineMethod (cls : VALUE; method : TRubyMethod3);
procedure DefineMethod (cls : VALUE; method : TRubyMethod4);
procedure DefineMethod (cls : VALUE; method : TRubyMethod5);
procedure DefineMethod (cls : VALUE; method : TRubyMethod6);
procedure DefineMethod (cls : VALUE; method : TRubyMethod7);
procedure DefineMethod (cls : VALUE; method : TRubyMethod8);
procedure DefineMethod (cls : VALUE; method : TRubyMethod9);
procedure DefineMethod (cls : VALUE; method : TRubyMethod10);
procedure DefineMethod (cls : VALUE; method : TRubyMethod11);
procedure DefineMethod (cls : VALUE; method : TRubyMethod12);
procedure DefineMethod (cls : VALUE; method : TRubyMethod13);
procedure DefineMethod (cls : VALUE; method : TRubyMethod14);
procedure DefineMethod (cls : VALUE; method : TRubyMethod15);
procedure DefineMethod (cls : VALUE; method : TRubyMethod16);
procedure DefineMethod (cls : VALUE; method : TRubyMethod17);
procedure DefineMethod (cls : VALUE; method : TRubyMethodArr);

procedure DefineSingletonMethod (cls : VALUE; method : TRubyMethod0);
procedure DefineSingletonMethod (cls : VALUE; method : TRubyMethod1);
procedure DefineSingletonMethod (cls : VALUE; method : TRubyMethod2);
procedure DefineSingletonMethod (cls : VALUE; method : TRubyMethod3);
procedure DefineSingletonMethod (cls : VALUE; method : TRubyMethod4);
procedure DefineSingletonMethod (cls : VALUE; method : TRubyMethod5);
procedure DefineSingletonMethod (cls : VALUE; method : TRubyMethod6);
procedure DefineSingletonMethod (cls : VALUE; method : TRubyMethod7);
procedure DefineSingletonMethod (cls : VALUE; method : TRubyMethod8);
procedure DefineSingletonMethod (cls : VALUE; method : TRubyMethod9);
procedure DefineSingletonMethod (cls : VALUE; method : TRubyMethod10);
procedure DefineSingletonMethod (cls : VALUE; method : TRubyMethod11);
procedure DefineSingletonMethod (cls : VALUE; method : TRubyMethod12);
procedure DefineSingletonMethod (cls : VALUE; method : TRubyMethod13);
procedure DefineSingletonMethod (cls : VALUE; method : TRubyMethod14);
procedure DefineSingletonMethod (cls : VALUE; method : TRubyMethod15);
procedure DefineSingletonMethod (cls : VALUE; method : TRubyMethod16);
procedure DefineSingletonMethod (cls : VALUE; method : TRubyMethod17);
procedure DefineSingletonMethod (cls : VALUE; method : TRubyMethodArr);

function Version : TRubyVersion; inline;

function Load (ver : TRubyVersion) : Boolean;
function Load : TRubyVersion;
procedure Unload;

function getActive : Boolean;
procedure setActive (val : Boolean);
property Active : Boolean read getActive write setActive;

function ErrorInfo : VALUE;
function Inspect (v : VALUE) : VALUE;

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

const
{$IFDEF UNIX}
  LIB18 = 'libruby18.so';
  LIB19 = 'libruby19.so';
{$ELSE !UNIX}
 {$IFDEF WINDOWS}
  LIB18 = 'mingw-ruby18.dll';
  LIB19 = 'mingw-ruby19.dll';
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

var
  p_ruby_errinfo : PVALUE;

  f_rb_inspect : function (v : VALUE) : VALUE; cdecl;
  f_rb_protect : function (func : Pfunc; data : VALUE; state : PInteger) : VALUE; cdecl;
  f_rb_string_value_cstr : function (constref str : VALUE) : pchar; cdecl;

procedure init_18_19;
 begin
  // init p_ vars
  p_ruby_errinfo := GetProcedureAddress(libRuby, 'ruby_errinfo');
  // init funcs
  Pointer(f_rb_inspect) := GetProcedureAddress(libRuby, 'rb_inspect');
  Pointer(f_rb_protect) := GetProcedureAddress(libRuby, 'rb_protect');
  Pointer(f_rb_string_value_cstr) := GetProcedureAddress(libRuby, 'rb_string_value_cstr');
 end;

procedure done_18_19;
 begin

 end;

const
  _Qfalse = 0;
  _Qtrue  = 2;
  _Qnil   = 4;
  _Qundef = 6;

operator = (x, y : VALUE) : Boolean;
 begin
  Result := (x.data = y.data);
 end;

function Qnil : VALUE; inline;
 begin
  Result.data := _Qnil;
 end;

function Qfalse : VALUE; inline;
 begin
  Result.data := _Qfalse;
 end;

function Qtrue : VALUE; inline;
 begin
  Result.data := _Qtrue;
 end;

function Qundef : VALUE; inline;
 begin
  Result.data := _Qundef;
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
  result := Qnil;
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
         end;
       else
         errUnknown;
  end;
 end;

operator explicit (v : VALUE) : ansistring;
 begin
  Result := UTF8String(v);
 end;

procedure DefineMethod(cls: VALUE; method: TRubyMethod0);
 begin

 end;

procedure DefineMethod(cls: VALUE; method: TRubyMethod1);
 begin

 end;

procedure DefineMethod(cls: VALUE; method: TRubyMethod2);
 begin

 end;

procedure DefineMethod(cls: VALUE; method: TRubyMethod3);
 begin

 end;

procedure DefineMethod(cls: VALUE; method: TRubyMethod4);
 begin

 end;

procedure DefineMethod(cls: VALUE; method: TRubyMethod5);
 begin

 end;

procedure DefineMethod(cls: VALUE; method: TRubyMethod6);
 begin

 end;

procedure DefineMethod(cls: VALUE; method: TRubyMethod7);
 begin

 end;

procedure DefineMethod(cls: VALUE; method: TRubyMethod8);
 begin

 end;

procedure DefineMethod(cls: VALUE; method: TRubyMethod9);
 begin

 end;

procedure DefineMethod(cls: VALUE; method: TRubyMethod10);
 begin

 end;

procedure DefineMethod(cls: VALUE; method: TRubyMethod11);
 begin

 end;

procedure DefineMethod(cls: VALUE; method: TRubyMethod12);
 begin

 end;

procedure DefineMethod(cls: VALUE; method: TRubyMethod13);
 begin

 end;

procedure DefineMethod(cls: VALUE; method: TRubyMethod14);
 begin

 end;

procedure DefineMethod(cls: VALUE; method: TRubyMethod15);
 begin

 end;

procedure DefineMethod(cls: VALUE; method: TRubyMethod16);
 begin

 end;

procedure DefineMethod(cls: VALUE; method: TRubyMethod17);
 begin

 end;

procedure DefineMethod(cls: VALUE; method: TRubyMethodArr);
 begin

 end;

procedure DefineSingletonMethod(cls : VALUE; method : TRubyMethod0);
begin

end;

procedure DefineSingletonMethod(cls : VALUE; method : TRubyMethod1);
begin

end;

procedure DefineSingletonMethod(cls : VALUE; method : TRubyMethod2);
begin

end;

procedure DefineSingletonMethod(cls : VALUE; method : TRubyMethod3);
begin

end;

procedure DefineSingletonMethod(cls : VALUE; method : TRubyMethod4);
begin

end;

procedure DefineSingletonMethod(cls : VALUE; method : TRubyMethod5);
begin

end;

procedure DefineSingletonMethod(cls : VALUE; method : TRubyMethod6);
begin

end;

procedure DefineSingletonMethod(cls : VALUE; method : TRubyMethod7);
begin

end;

procedure DefineSingletonMethod(cls : VALUE; method : TRubyMethod8);
begin

end;

procedure DefineSingletonMethod(cls : VALUE; method : TRubyMethod9);
begin

end;

procedure DefineSingletonMethod(cls : VALUE; method : TRubyMethod10);
begin

end;

procedure DefineSingletonMethod(cls : VALUE; method : TRubyMethod11);
begin

end;

procedure DefineSingletonMethod(cls : VALUE; method : TRubyMethod12);
begin

end;

procedure DefineSingletonMethod(cls : VALUE; method : TRubyMethod13);
begin

end;

procedure DefineSingletonMethod(cls : VALUE; method : TRubyMethod14);
begin

end;

procedure DefineSingletonMethod(cls : VALUE; method : TRubyMethod15);
begin

end;

procedure DefineSingletonMethod(cls : VALUE; method : TRubyMethod16);
begin

end;

procedure DefineSingletonMethod(cls : VALUE; method : TRubyMethod17);
begin

end;

procedure DefineSingletonMethod(cls : VALUE; method : TRubyMethodArr);
 begin

 end;

function Version : TRubyVersion; inline;
 begin
  Result := verRuby;
 end;

function Load (ver : TRubyVersion) : Boolean;
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
 begin
  if not getActive
     then errInactive;
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

function ErrorInfo : VALUE;
 begin
  case Version of
       rvNone :
         Result := Qnil;
       rvRuby18, rvRuby19 :
         Result := p_ruby_errinfo^;
       else
         Result := Qundef;
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

{ ERubyError }

constructor ERubyError.Create(const msg : UTF8String);
 begin
  fldErrInfo := ErrorInfo;
  if (fldErrInfo = Qnil) or (fldErrInfo = Qundef)
     then inherited Create(msg)
     else inherited Create(msg + LineEnding + LineEnding + UTF8String(Inspect(fldErrInfo)));
 end;

constructor ERubyError.CreateFmt (const msg : UTF8String; const args : array of const);
 begin
  Create(Format(msg, args));
 end;

end.
