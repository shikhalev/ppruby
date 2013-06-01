{$mode ObjFPC}

unit RubyEngine;

{$ifdef DARWIN}
  {$linkframework Ruby}
{$endif}

interface

uses
  SysUtils, DynLibs, ctypes, typinfo;

type
  VALUE = type PtrUInt;
  PVALUE = ^VALUE;

  ID = type PtrUInt;

type

  { TRuby }

  TRuby = class
  private
    // methods
  protected
    // fields: lib
    libHandle : THandle;
    // fields: ruby_ functions
    ruby_init, ruby_init_loadpath, ruby_finalize : procedure; cdecl;
    ruby_script : procedure (script : PChar); cdecl;
    rb_define_module : function (name : PChar) : VALUE; cdecl;
    // fields: own ruby objects
    rb_mPascal : VALUE;
    // methods
    procedure loadFunc (out field; const name : UTF8String);
    procedure load; virtual;
    procedure setup; virtual;
  public
    // constants
    const
      Qfalse = VALUE(0);
      Qtrue  = VALUE(2);
      Qnil   = VALUE(4);
      Qundef = VALUE(6);
    // class methods
    // constructor & destructor
    constructor Create (const lib, script : UTF8String); virtual;
    // properties (wrappers)
    property mPascal : VALUE read rb_mPascal;
    // methods (wrappers)
    function DefineModule (const name : UTF8String) : VALUE;
    // properties (self)
    // methods (self)
  end;

  TRuby18 = class(TRuby)
  protected
  public
  end;

(*
type
  TVersion = record
    major, minor : Integer
  end;


type
  TRubyEngine = class;

  TRegisterClassHook = procedure (ruby : TRubyEngine; fpc_class : TClass; rb_class : VALUE);

  TRubyClass = class of TRubyEngine;

  { TRubyEngine }

  TRubyEngine = class(TObject)
  protected
    fldLib : THandle;
    ruby_init : procedure (); cdecl;
    ruby_init_loadpath : procedure (); cdecl;
    ruby_script : procedure (script : PChar); cdecl;
    ruby_finalize : procedure (); cdecl;
    rb_eval_string_protect : function (Str : PChar; out Err : Integer) : VALUE;
                                      cdecl;
    rb_string_value_cstr : function (constref v : VALUE) : PChar; cdecl;
    rb_inspect : function (v : VALUE) : VALUE; cdecl;
    rb_define_module : function (name : PChar) : VALUE; cdecl;
    rb_define_module_under : function (up : VALUE; name : PChar) : VALUE; cdecl;
    rb_define_class : function (name : PChar; super : VALUE) : VALUE; cdecl;
    rb_define_class_under : function (up : VALUE; name : PChar; super : VALUE) : VALUE; cdecl;
    rb_define_const : procedure (ns : VALUE; name : PChar; v : VALUE); cdecl;
    rb_define_global_const : procedure (name : PChar; v : VALUE); cdecl;
    rb_funcall2 : function (obj : VALUE; meth : ID; cnt : cint; args : PVALUE) : VALUE; cdecl;
    rb_intern : function (name : PChar) : ID; cdecl;
    rb_str_new2 : function (s : PChar) : VALUE; cdecl;
    rb_cObject : VALUE;
  protected
    rb_mPascal : VALUE;
  protected
    class function Version : TVersion; virtual; abstract;
    class function DefaultScript : UTF8String; virtual;
    procedure SetupUTF8; virtual; abstract;
  public
    class function DefaultLibrary : UTF8String; virtual;
    class function AutoCreate (
                   const Vers : array of TRubyClass;
                   const Scr : UTF8String = ''
                   ) : TRubyEngine;
    function Execute (const Str : UTF8String) : VALUE; virtual;
    function Description : UTF8String; virtual; abstract;
    function ErrInfo : VALUE; virtual; abstract;
    function Inspect (v : VALUE) : VALUE; virtual;
    function VALUE2String (v : VALUE) : UTF8String; virtual;
    function String2VALUE (const s : UTF8String) : VALUE; virtual;
    function String2ID (const s : UTF8String) : ID; virtual;
  protected
    procedure RegisterRTTI (cls : TClass; rb : VALUE); virtual;
  public
    class procedure RegisterClassHook (const Classes : array of TClass; Hook : TRegisterClassHook);
    class procedure UnregisterClassHook (const Classes : array of TClass; Hook : TRegisterClassHook);
    function RegisterClass (Cls : TClass) : VALUE; virtual;
    function RegisterUnit (const nm : AnsiString) : VALUE; virtual;
    function DefaultAncestor : VALUE; virtual; abstract;
  public
    property cObject : VALUE read rb_cObject;
    property mPascal : VALUE read rb_mPascal;
  public
    function DefineModule (const Name : UTF8String) : VALUE; virtual;
    function DefineModule (NS : VALUE; const Name : UTF8String) : VALUE; virtual;
    function DefineClass (const Name : UTF8String; Super : VALUE) : VALUE; virtual;
    function DefineClass (const Name : UTF8String) : VALUE; virtual;
    function DefineClass (NS : VALUE; const Name : UTF8String; Super : VALUE) : VALUE; virtual;
    function DefineClass (NS : VALUE; const Name : UTF8String) : VALUE; virtual;
    procedure DefineConstant (ns : VALUE; const name : UTF8String; v : VALUE); virtual;
    procedure DefineConstant (const name : UTF8String; v : VALUE); virtual;
    function Call (obj : VALUE; msg : ID; const args : array of VALUE) : VALUE; virtual;
    function Call (obj : VALUE; const method : UTF8String; const args : array of VALUE) : VALUE; virtual;
  public
    const
      Qfalse = VALUE(0);
      Qtrue  = VALUE(2);
    class function Qnil : VALUE; virtual;
    class function Qundef : VALUE; virtual;
  public
    constructor Create (const Lib : UTF8String; const Scr : UTF8String = '');
                                      virtual;
    constructor DefaultCreate; virtual;
    destructor Destroy; override;
  end;

  { TRuby18 }

  TRuby18 = class(TRubyEngine)
  protected
    p_ruby_errinfo : PVALUE;
    ruby_description : PChar;
  protected
    class function Version : TVersion; override;
    procedure SetupUTF8; override;
  public
    function Description : UTF8String; override;
    function ErrInfo : VALUE; override;
    function DefaultAncestor : VALUE; override;
  public
    constructor Create (const Lib : UTF8String; const Scr : UTF8String = '');
                                      override;
  end;

  { TRuby19 }

  TRuby19 = class(TRubyEngine)
  protected
    rb_errinfo : function : VALUE; cdecl;
    rb_set_errinfo : procedure (v : VALUE); cdecl;
    ruby_description : PChar;
    rb_cData : VALUE;
  protected
    class function Version : TVersion; override;
    procedure SetupUTF8; override;
  public
    function Description : UTF8String; override;
    function ErrInfo : VALUE; override;
    function Execute (const Str : UTF8String) : VALUE; override;
    function DefaultAncestor : VALUE; override;
  public
    property cData : VALUE read rb_cData;
  public
    constructor Create (const Lib : UTF8String; const Scr : UTF8String = '');
                                      override;
  end;

  { TRuby20 }

  TRuby20 = class(TRuby19)
  protected
    class function Version : TVersion; override;
  end;

type
  ERubyError = class(Exception);

  ERubyLibraryNotFound = class(ERubyError);
  ERubyInitError = class(ERubyError);

  { ERubyExecError }

  ERubyExecError = class(ERubyError)
  private
    fldErrInfo : VALUE;
  public
    property ErrInfo : VALUE read fldErrInfo;
  public
    constructor Create(info : VALUE; const msg : UTF8String);
  end;

const
  msgRubyLibraryNotFound = 'Ruby Library "%s" not found (or is not a library).';
  msgRubyInitError = 'Error #%d while initialization (%s).';    *)

type
  ERuby = class(Exception);

  ENoRuby = class(ERuby);

implementation

const
  msgNoRuby = 'The file "%s" not found or not a ruby library.';

{ TRuby }

procedure TRuby.loadFunc(out field; const name : UTF8String);
 begin
 Pointer(field) := GetProcAddress(libHandle, name);
 end;

procedure TRuby.load;
 begin
 // ruby_ functions
 loadFunc(ruby_init,          'ruby_init');
 loadFunc(ruby_init_loadpath, 'ruby_init_loadpath');
 loadFunc(ruby_script,        'ruby_script');
 loadFunc(ruby_finalize,      'ruby_finalize');
 end;

procedure TRuby.setup;
 begin
 // namespace
 rb_mPascal := rb_define_module('Pascal');
 end;

constructor TRuby.Create(const lib, script : UTF8String);
 begin
 inherited Create;
 libHandle := LoadLibrary(lib);
 if libHandle = 0
    then raise ENoRuby.CreateFmt(msgNoRuby, [lib]);
 load;
 ruby_init;
 ruby_init_loadpath;
 ruby_script(PChar(script));
 setup;
 end;

function TRuby.DefineModule(const name : UTF8String) : VALUE;
 begin
 result := rb_define_module(PChar(name));
 end;

{ ERubyExecError }
(*
constructor ERubyExecError.Create(info : VALUE; const msg : UTF8String);
 begin
 inherited Create(msg);
 fldErrInfo := info;
 end;

{ TRuby20 }

class function TRuby20.Version: TVersion;
 begin
 result.major := 2;
 result.minor := 0;
 end;

{ TRuby18 }

class function TRuby18.Version : TVersion;
 begin
 result.major := 1;
 result.minor := 8;
 end;

procedure TRuby18.SetupUTF8;
 var
   res : Integer;
 begin
 rb_eval_string_protect('$-K = "UTF-8"', res);
 if res <> 0
    then raise ERubyInitError.CreateFmt(msgRubyInitError, [res, 'SetupUTF8']);
 end;

function TRuby18.Description : UTF8String;
 begin
 result := UTF8String(ruby_description) + '';
 end;

constructor TRuby18.Create (const Lib : UTF8String; const Scr : UTF8String);
 begin
 inherited Create(Lib, Scr);
 Pointer(p_ruby_errinfo) := GetProcedureAddress(fldLib, 'ruby_errinfo');
 ruby_description := PPChar(GetProcedureAddress(fldLib, 'ruby_description'))^;
 end;

{ TRuby19 }

class function TRuby19.Version : TVersion;
 begin
 result.major := 1;
 result.minor := 9;
 end;

procedure TRuby19.SetupUTF8;
 var
   res : Integer;
 begin
 rb_eval_string_protect('Encoding.default_internal = "UTF-8"', res);
 if res <> 0
    then raise ERubyInitError.CreateFmt(msgRubyInitError, [res, 'SetupUTF8']);
 end;

function TRuby19.Description : UTF8String;
 begin
 result := UTF8String(ruby_description) + '';
 end;

function TRuby19.ErrInfo : VALUE;
 begin
 result := rb_errinfo();
 end;

function TRuby19.Execute (const Str : UTF8String) : VALUE;
 begin
 rb_set_errinfo(Qnil);
 result := inherited Execute(Str);
 end;

function TRuby19.DefaultAncestor : VALUE;
 begin
 result := rb_cData
 end;

constructor TRuby19.Create (const Lib : UTF8String; const Scr : UTF8String);
 begin
 inherited Create(Lib, Scr);
 Pointer(rb_errinfo) := GetProcedureAddress(fldLib, 'rb_errinfo');
 Pointer(rb_set_errinfo) := GetProcedureAddress(fldLib, 'rb_set_errinfo');
 ruby_description := PChar(GetProcedureAddress(fldLib, 'ruby_description'));
 rb_cData := PVALUE(GetProcedureAddress(fldLib, 'rb_cData'))^;
 end;

{ TRubyEngine }

class function TRubyEngine.DefaultLibrary : UTF8String;
 var
   ver : TVersion;
 begin
 ver := Version;
{$ifdef UNIX}
 {$ifdef DARWIN}
 // Mac OS X
 result := '/System/Library/Frameworks/Ruby.framework/Versions/' +
           IntToStr(ver.major) + '.' + IntToStr(ver.minor) + '/Ruby'
 {$else}
 // Linux or BSD
 result := 'libruby' + IntToStr(ver.major) + IntToStr(ver.minor) + '.so'
 {$endif}
{$else}
 {$ifdef WINDOWS}
 // MS Windows
 result := 'msvcrt-ruby' + IntToStr(ver.major) + IntToStr(ver.minor) + '.dll'
 {$else}
 // Unknown OS
  {$error Unknown Ruby Target!}
 {$endif}
{$endif}
 end;

class function TRubyEngine.DefaultScript : UTF8String;
 begin
 result := ParamStr(0);
 end;

class function TRubyEngine.AutoCreate (
               const Vers : array of TRubyClass;
               const Scr : UTF8String = ''
               ) : TRubyEngine;
 var
   idx : Integer;
 begin
 for idx := 0 to High(Vers) do
     try
       result := Vers[idx].Create(Vers[idx].DefaultLibrary, Scr);
       Exit;
     except
       on ERubyError do
          Continue;
       on Exception do
          raise
     end;
 result := nil
 end;

function TRubyEngine.Execute (const Str : UTF8String) : VALUE;
 var
   res : Integer;
 begin
 result := rb_eval_string_protect(PChar(Str), res);
 if res <> 0
    then raise ERubyExecError.Create(ErrInfo, VALUE2String(Inspect(ErrInfo)));
 end;

function TRubyEngine.Inspect(v : VALUE) : VALUE;
 begin
 result := rb_inspect(v);
 end;

function TRuby18.ErrInfo : VALUE;
 begin
 result := p_ruby_errinfo^;
 end;

function TRuby18.DefaultAncestor : VALUE;
 begin
 result := rb_cObject
 end;

function TRubyEngine.VALUE2String(v : VALUE) : UTF8String;
 begin
 result := UTF8String(rb_string_value_cstr(v)) + ''
 end;

function TRubyEngine.String2VALUE(const s : UTF8String) : VALUE;
 begin
 result := rb_str_new2(PChar(s));
 end;

function TRubyEngine.String2ID(const s : UTF8String) : ID;
 begin
 result := rb_intern(PChar(s))
 end;

function MethName (const nm : UTF8String) : UTF8String;
 begin
 result := nm;
 result[1] := LowerCase(nm[1]);
 end;

procedure TRubyEngine.RegisterRTTI (cls : TClass; rb : VALUE);
 var
   data : PTypeData;
   list : PPropList;
   prop : PPropInfo;
   idx : Integer;
   cmd : UTF8String;
   nmm : UTF8String;
   param : VALUE;
 begin
 data := GetTypeData(cls.ClassInfo);
 GetPropList(cls, list);
 for idx := 0 to data^.PropCount - 1 do
     begin
     prop := list^[idx];
     nmm := MethName(prop^.Name);
     cmd := 'attr_accessor :' + nmm + LineEnding;
     cmd := cmd + 'def ' + nmm + LineEnding +
                  '  pascal_get_prop(:' + prop^.Name + ')' + LineEnding +
                  'end' + LineEnding;
     cmd := cmd + 'def ' + nmm + '=(value)' + LineEnding +
                  '  pascal_set_prop(:' + prop^.Name + ', value)' + LineEnding +
                  'end';
     param := String2VALUE(cmd);
     rb_funcall2(rb, String2ID('class_eval'), 1, @param);
     end;
 FreeMem(list)
 end;

type
  TClassHookRec = record
    Cls : TClass;
    Hook : TRegisterClassHook
  end;

var
  ClassHooks : array of TClassHookRec = nil;

function FindClassHook (Cls : TClass; Hook : TRegisterClassHook; out found : Integer) : Boolean;
 var
   idx : Integer;
 begin
 for idx := 0 to High(ClassHooks) do
     if (ClassHooks[idx].Cls = Cls) and (ClassHooks[idx].Hook = Hook)
        then begin
             found := idx;
             result := true;
             Exit;
             end;
 result := false;
 end;

class procedure TRubyEngine.RegisterClassHook (const Classes : array of TClass;
  Hook : TRegisterClassHook);
 var
   cidx, hidx : Integer;
 begin
 for cidx := 0 to High(Classes) do
     if not FindClassHook(Classes[cidx], Hook, hidx)
        then begin
             SetLength(ClassHooks, Length(ClassHooks) + 1);
             ClassHooks[High(ClassHooks)].Cls := Classes[cidx];
             ClassHooks[High(ClassHooks)].Hook := Hook;
             end;
 end;

class procedure TRubyEngine.UnregisterClassHook (const Classes : array of TClass;
  Hook : TRegisterClassHook);
 var
   cidx, hidx, didx : Integer;
 begin
 for cidx := 0 to High(Classes) do
     if FindClassHook(Classes[cidx], Hook, hidx)
        then begin
             for didx := hidx to High(ClassHooks) - 1 do
                 ClassHooks[didx] := ClassHooks[didx + 1];
             SetLength(ClassHooks, Length(ClassHooks) - 1);
             end;
 end;

type
  TRegClassRec = record
                 Cls : TClass;
                 Rb : VALUE;
                 end;

var
  RegisteredClasses : array of TRegClassRec = nil;

function FindRegisteredClass (Cls : TClass; out v : VALUE) : Boolean;
 var
   idx : Integer;
 begin
 for idx := 0 to High(RegisteredClasses) do
     if RegisteredClasses[idx].Cls = Cls
        then begin
             v := RegisteredClasses[idx].Rb;
             result := true;
             Exit;
             end;
 result := false;
 end;

function ConstName (const nm : AnsiString) : AnsiString;
 begin
 result := nm;
 result[1] := UpCase(nm[1]);
 end;

function TRubyEngine.RegisterClass (Cls : TClass) : VALUE;
 var
   rb_u : VALUE;
   rb_s : VALUE;
   idx : Integer;
 begin
 if FindRegisteredClass(Cls, result)
    then Exit;
 if Cls.UnitName = ''
    then rb_u := rb_mPascal
    else rb_u := RegisterUnit(Cls.UnitName);
 if Cls = TObject
    then rb_s := DefaultAncestor
    else rb_s := RegisterClass(Cls.ClassParent);
 result := rb_define_class_under(rb_u, PChar(ConstName(cls.ClassName)), rb_s);
 SetLength(RegisteredClasses, Length(RegisteredClasses) + 1);
 RegisteredClasses[High(RegisteredClasses)].Cls := Cls;
 RegisteredClasses[High(RegisteredClasses)].Rb := result;
 RegisterRTTI(Cls, result);
 rb_define_const(rb_mPascal, PChar(ConstName(cls.ClassName)), result);
 for idx := 0 to High(ClassHooks) do
     if ClassHooks[idx].Cls = Cls
        then ClassHooks[idx].Hook(self, Cls, result);
 end;

type
  TRegUnitRec = record
                nm : AnsiString;
                v : VALUE
                end;

var
  RegisteredUnits : array of TRegUnitRec = nil;

function FindRegisteredUnit (const nm : AnsiString; out v : VALUE) : Boolean;
 var
   idx : Integer;
 begin
 for idx := 0 to High(RegisteredUnits) do
     if RegisteredUnits[idx].nm = nm
        then begin
             v := RegisteredUnits[idx].v;
             result := true;
             Exit;
             end;
 result := false;
 end;

function TRubyEngine.RegisterUnit(const nm : AnsiString) : VALUE;
 var
   nmm : AnsiString;
 begin
 nmm := ConstName(nm);
 if FindRegisteredUnit(nmm, result)
    then Exit;
 result := rb_define_module_under(rb_mPascal, PChar(nmm));
 SetLength(RegisteredUnits, Length(RegisteredUnits) + 1);
 RegisteredUnits[High(RegisteredUnits)].nm := nmm;
 RegisteredUnits[High(RegisteredUnits)].v := result;
 end;

function TRubyEngine.DefineModule (const Name : UTF8String) : VALUE;
 begin
 result := rb_define_module(PChar(Name))
 end;

function TRubyEngine.DefineModule (NS : VALUE; const Name : UTF8String) : VALUE;
 begin
 result := rb_define_module_under(NS, PChar(Name))
 end;

function TRubyEngine.DefineClass (const Name : UTF8String;
  Super : VALUE) : VALUE;
 begin
 result := rb_define_class(PChar(Name), Super);
 end;

function TRubyEngine.DefineClass (const Name : UTF8String) : VALUE;
 begin
 result := rb_define_class(PChar(Name), rb_cObject);
 end;

function TRubyEngine.DefineClass (NS : VALUE; const Name : UTF8String;
  Super : VALUE) : VALUE;
 begin
 result := rb_define_class_under(NS, PChar(Name), Super);
 end;

function TRubyEngine.DefineClass (NS : VALUE; const Name : UTF8String) : VALUE;
 begin
 result := rb_define_class_under(NS, PChar(Name), rb_cObject);
 end;

procedure TRubyEngine.DefineConstant (ns : VALUE; const name : UTF8String;
  v : VALUE);
 begin
 rb_define_const(ns, PChar(name), v);
 end;

procedure TRubyEngine.DefineConstant (const name : UTF8String; v : VALUE);
 begin
 rb_define_global_const(PChar(name), v);
 end;

function TRubyEngine.Call(obj : VALUE; msg : ID;
  const args : array of VALUE) : VALUE;
 begin
 result := rb_funcall2(obj, msg, Length(args), @args[0]);
 end;

function TRubyEngine.Call(obj : VALUE; const method : UTF8String;
  const args : array of VALUE) : VALUE;
 begin
 result := Call(obj, String2ID(method), args)
 end;

(* class function TRubyEngine.Qfalse : VALUE;
 begin
 result := VALUE(0)
 end;

class function TRubyEngine.Qtrue : VALUE;
 begin
 result := VALUE(2)
 end;   *)

class function TRubyEngine.Qnil : VALUE;
 begin
 result := VALUE(4)
 end;

class function TRubyEngine.Qundef : VALUE;
 begin
 result := VALUE(6)
 end;

constructor TRubyEngine.Create (
               const Lib : UTF8String;
               const Scr : UTF8String = ''
               );
 begin
 inherited Create;
 fldLib := LoadLibrary(Lib);
 if fldLib = 0
    then raise ERubyLibraryNotFound.CreateFmt(msgRubyLibraryNotFound, [Lib]);
 Pointer(ruby_init) := GetProcedureAddress(fldLib, 'ruby_init');
 Pointer(ruby_init_loadpath) := GetProcedureAddress(fldLib,
                                  'ruby_init_loadpath');
 Pointer(ruby_script) := GetProcedureAddress(fldLib, 'ruby_script');
 Pointer(rb_eval_string_protect) := GetProcedureAddress(fldLib,
                                      'rb_eval_string_protect');
 Pointer(ruby_finalize) := GetProcedureAddress(fldLib, 'ruby_finalize');
 Pointer(rb_string_value_cstr) := GetProcedureAddress(fldLib,
                                    'rb_string_value_cstr');
 Pointer(rb_inspect) := GetProcedureAddress(fldLib, 'rb_inspect');
 Pointer(rb_define_module) := GetProcedureAddress(fldLib, 'rb_define_module');
 Pointer(rb_define_module_under) := GetProcedureAddress(fldLib,
                                      'rb_define_module_under');
 Pointer(rb_define_class) := GetProcedureAddress(fldLib, 'rb_define_class');
 Pointer(rb_define_class_under) := GetProcedureAddress(fldLib, 'rb_define_class_under');
 Pointer(rb_define_const) := GetProcAddress(fldLib, 'rb_define_const');
 pointer(rb_define_global_const) := GetProcAddress(fldLib, 'rb_define_global_const');
 Pointer(rb_funcall2) := GetProcedureAddress(fldLib, 'rb_funcall2');
 Pointer(rb_intern) := GetProcedureAddress(fldLib, 'rb_intern');
 Pointer(rb_str_new2) := GetProcedureAddress(fldLib, 'rb_str_new2');
 rb_cObject := PVALUE(GetProcedureAddress(fldLib, 'rb_cObject'))^;
 ruby_init();
 ruby_init_loadpath();
 if Scr = ''
    then ruby_script(PChar(DefaultScript))
    else ruby_script(PChar(Scr));
 SetupUTF8;
 rb_mPascal := DefineModule('Pascal');
 end;

constructor TRubyEngine.DefaultCreate;
 begin
 Create(DefaultLibrary, DefaultScript)
 end;

destructor TRubyEngine.Destroy;
 begin
 if fldLib <> 0
    then begin
         ruby_finalize;
         FreeLibrary(fldLib);
         end;
 inherited Destroy;
 end;
*)
end.

