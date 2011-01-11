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
operator explicit (v : VALUE) : UnicodeString;

operator explicit (const s : UTF8String) : VALUE;
operator explicit (const s : ansistring) : VALUE;
operator explicit (const s : UnicodeString) : VALUE;

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
  ERubyDefinition = class(ERubyError);

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
  f_rb_define_method : procedure (module : VALUE; name : PChar; func : Pointer; argc : Integer); cdecl;
  f_rb_define_singleton_method : procedure (module : VALUE; name : PChar; func : Pointer; argc : Integer); cdecl;
  f_rb_str_new2 : function (ptr : PChar) : VALUE; cdecl;

procedure init_18_19;
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
  Result := Qnil;
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
  Result := Qnil;
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
           for idx := 0 to h do
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
           for idx := 0 to h do
               if hooksUnload[idx] = hook
                  then begin
                        for delidx := idx to h - 1 do
                            hooksUnload[delidx] := hooksUnload[delidx + 1];
                        SetLength(hooksUnload, h);
                        exit;
                       end;
          end;
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
