{$mode ObjFPC}

unit Ruby;

{$if defined(DARWIN)}
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

  TRuby = class;

  FRegisterClassHook = procedure (ruby : TRuby; cls : TClass; value : VALUE);
  FInitHook = procedure (ruby : TRuby);

  FRubyDataFunc = procedure (p : Pointer); cdecl;
  FRubyFunc = function (value : VALUE) : VALUE; cdecl;

  FRubyMethod0   = function (obj : VALUE) : VALUE; cdecl;
  FRubyMethod1   = function (obj : VALUE; a : VALUE) : VALUE; cdecl;
  FRubyMethod2   = function (obj : VALUE; a, b : VALUE) : VALUE; cdecl;
  FRubyMethod3   = function (obj : VALUE; a, b, c : VALUE) : VALUE; cdecl;
  FRubyMethod4   = function (obj : VALUE; a, b, c, d : VALUE) : VALUE; cdecl;
  FRubyMethod5   = function (obj : VALUE; a, b, c, d, e : VALUE) : VALUE; cdecl;
  FRubyMethod6   = function (obj : VALUE; a, b, c, d, e, f : VALUE) : VALUE; cdecl;
  FRubyMethod7   = function (obj : VALUE; a, b, c, d, e, f, g : VALUE) : VALUE; cdecl;
  FRubyMethod8   = function (obj : VALUE; a, b, c, d, e, f, g, h : VALUE) : VALUE; cdecl;
  FRubyMethod9   = function (obj : VALUE; a, b, c, d, e, f, g, h, i : VALUE) : VALUE; cdecl;
  FRubyMethod10  = function (obj : VALUE; a, b, c, d, e, f, g, h, i, j : VALUE) : VALUE; cdecl;
  FRubyMethod11  = function (obj : VALUE; a, b, c, d, e, f, g, h, i, j, k : VALUE) : VALUE; cdecl;
  FRubyMethod12  = function (obj : VALUE; a, b, c, d, e, f, g, h, i, j, k, l : VALUE) : VALUE; cdecl;
  FRubyMethod13  = function (obj : VALUE; a, b, c, d, e, f, g, h, i, j, k, l, m : VALUE) : VALUE; cdecl;
  FRubyMethod14  = function (obj : VALUE; a, b, c, d, e, f, g, h, i, j, k, l, m, n : VALUE) : VALUE; cdecl;
  FRubyMethod15  = function (obj : VALUE; a, b, c, d, e, f, g, h, i, j, k, l, m, n, o : VALUE) : VALUE; cdecl;
  FRubyMethod16  = function (obj : VALUE; a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p : VALUE) : VALUE; cdecl;
  FRubyMethod17  = function (obj : VALUE; a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q : VALUE) : VALUE; cdecl;
  FRubyMethodArr = function (argc : LongInt; argv : PVALUE; obj : VALUE) : VALUE; cdecl;

  TRubyClass = class of TRuby;
  TPascalOutClass = class of TPascalOut;

  TPack = record
    obj : TObject;
    rb : TRuby;
  end;
  PPack = ^TPack;

  TEventCompanion = class
  protected
    ruby : TRuby;
    proc : VALUE;
    function Call (const args : array of VALUE) : VALUE;
    function GetHandler : TMethod; virtual; abstract;
  public
    constructor Create (r : TRuby; p : VALUE); virtual;
  end;

  TEventCompanionClass = class of TEventCompanion;

  { TRuby }

  TRuby = class
  private
    class var
      listHooks : array of record
          cls : TClass;
          hook : FRegisterClassHook;
        end;
    class function findHook (cls : TClass; hook : FRegisterClassHook; out idx : Integer) : Boolean;
    class var
      listCompanions : array of record
          name : ansistring;
          cls : TEventCompanionClass;
        end;
    var
      cacheClasses : array of record
          cls : TClass;
          value : VALUE;
        end;
    function findClass (cls : TClass; out value : VALUE) : Boolean;
    var
      cacheUnits : array of record
          name : AnsiString;
          value : VALUE;
        end;
    function findUnit (const name : AnsiString; out value : VALUE) : Boolean;
    var
      cacheCompanions : array of TEventCompanion;
    var
      cacheOutClasses : array of record
          cls : TPascalOutClass;
          value : VALUE;
        end;
      function findOutClass (cls : TPascalOutClass; out value : VALUE) : Boolean;
  private
    type
      TModPack = record
        engine : TRuby;
        module : VALUE;
      end;
      TModPackArray = array of TModPack;
    class var
      cacheModules : TModPackArray;
  public
    class function EngineByModule (value : VALUE) : TRuby;
  protected
    procedure registerModule (module : VALUE);
    procedure registerModules (const modules : array of VALUE);
    procedure freeModules;
  private
    class var
      listInitHooks : array of FInitHook;
  public
    class procedure AddInitHook (hook : FInitHook);
    class procedure DelInitHook (hook : FInitHook);
  protected
    // fields: lib
    libHandle : THandle;
    // fields: ruby_ functions
    ruby_init, ruby_init_loadpath, ruby_finalize : procedure; cdecl;
    ruby_script : procedure (script : PChar); cdecl;
    rb_define_module : function (name : PChar) : VALUE; cdecl;
    rb_define_module_under : function (ns : VALUE; name : PChar) : VALUE; cdecl;
    rb_define_class : function (name : PChar; super : VALUE) : VALUE; cdecl;
    rb_define_class_under : function (ns : VALUE; name : PChar; super : VALUE) : VALUE; cdecl;
    rb_define_const : procedure (ns : VALUE; name : PChar; value : VALUE); cdecl;
    rb_define_global_const : procedure (name : PChar; value : VALUE); cdecl;
    rb_define_method, rb_define_module_function  : procedure (module : VALUE; name : PChar; func : Pointer; argc : cint);
    rb_define_singleton_method : procedure (obj : VALUE; name : PChar; func : Pointer; argc : cint);
    rb_define_global_function : procedure (name : PChar; func : Pointer; argc : cint);
    rb_eval_string_protect : function (str : PChar; out state : cint) : VALUE; cdecl;
    rb_string_value_cstr : function (constref v : VALUE) : PChar; cdecl;
    rb_inspect : function (value : VALUE) : VALUE; cdecl;
    rb_funcall2, rb_funcall3 : function (obj : VALUE; method : ID; argc : cint; argv : PVALUE) : VALUE; cdecl;
    rb_intern : function (name : PChar) : ID; cdecl;
    rb_str_new2, rb_locale_str_new_cstr : function (str : PChar) : VALUE; cdecl;
    rb_utf8_encoding : function : Pointer; cdecl;
    rb_enc_str_new : function (str : PChar; len : clong; enc : Pointer) : VALUE; cdecl;
    rb_data_object_alloc : function (cls : VALUE; data : Pointer; mark, free : FRubyDataFunc) : VALUE; cdecl;
    rb_protect : function (func : FRubyFunc; data : VALUE; out state : cint) : VALUE; cdecl;
    rb_check_type : procedure (value : VALUE; t : cint); cdecl;
    rb_int2inum : function (n : PtrInt) : VALUE; cdecl;
    rb_num2int : function (value : VALUE) : PtrInt; cdecl;
    rb_ll2inum : function (n : Int64) : VALUE; cdecl;
    rb_ull2inum : function (n : QWord) : VALUE; cdecl;
    rb_num2ll : function (v : VALUE) : Int64; cdecl;
    rb_num2ull : function (v : VALUE) : QWord; cdecl;
    rb_require : function (name : PChar) : VALUE; cdecl;
    rb_float_new : function (d : Double) : VALUE; cdecl;
    rb_to_id : function (value : VALUE) : ID; cdecl;
    rb_id2name : function (id : ID) : PChar; cdecl;
    rb_num2dbl : function (value : VALUE) : Double; cdecl;
    rb_include_module : procedure (target, source : VALUE); cdecl;
    rb_define_alias : procedure (module : VALUE; target, source : PChar); cdecl;
    rb_gv_get : function (name : PChar) : VALUE; cdecl;
    rb_gv_set : function (name : PChar; value : VALUE) : VALUE; cdecl;
    rb_block_given_p : function : cint; cdecl;
    rb_yield : function (arg : VALUE) : VALUE; cdecl;
    rb_ary_new4 : function (size : clong; data : PVALUE) : VALUE; cdecl;
    rb_hash_new : function : VALUE; cdecl;
    rb_hash_aref : function (hash : VALUE; key : VALUE) : VALUE; cdecl;
    rb_hash_aset : function (hash : VALUE; key, value : VALUE) : VALUE; cdecl;
    rb_ary_new : function : VALUE; cdecl;
    rb_ary_new2 : function (size : clong) : VALUE; cdecl;
    rb_ary_push : function (arr : VALUE; value : VALUE) : VALUE; cdecl;
    // fields: ruby objects
    rb_mKernel, rb_mComparable, rb_mEnumerable, rb_mErrno, rb_mFileTest, rb_mGC,
      rb_mMath, rb_mProcess : VALUE;
    rb_cObject, rb_cArray, rb_cBignum, rb_cBinding, rb_cClass, rb_cCont,
      rb_cDir, rb_cData, rb_cEnumerator, rb_cFalseClass, rb_cFile, rb_cFixnum,
      rb_cFloat, rb_cHash, rb_cInteger, rb_cIO, rb_cMatch, rb_cMethod,
      rb_cModule, rb_cNameErrorMesg, rb_cNilClass, rb_cNumeric, rb_cProc,
      rb_cRange, rb_cRegexp, rb_cStat, rb_cString, rb_cStruct, rb_cSymbol,
      rb_cThread, rb_cTime, rb_cTrueClass, rb_cUnboundMethod : VALUE;
    rb_eException, rb_eStandardError, rb_eSystemExit, rb_eInterrupt, rb_eSignal,
      rb_eFatal, rb_eArgError, rb_eEOFError, rb_eIndexError, rb_eStopIteration,
      rb_eRangeError, rb_eIOError, rb_eRuntimeError, rb_eSecurityError,
      rb_eSystemCallError, rb_eThreadError, rb_eTypeError, rb_eZeroDivError,
      rb_eNotImpError, rb_eNoMemError, rb_eNoMethodError, rb_eFloatDomainError,
      rb_eLocalJumpError, rb_eSysStackError, rb_eRegexpError, rb_eScriptError,
      rb_eNameError, rb_eSyntaxError, rb_eLoadError : VALUE;
    rb_stdin, rb_stdout, rb_stderr : PVALUE;
    // fields: other ruby vars
    ruby_description : Pointer;
    // fields: own ruby objects
    rb_mPascal, rb_ePascalError : VALUE;
    // methods
    procedure loadFunc (out field; const name : UTF8String);
    procedure loadPtr (out field; const name : UTF8String);
    procedure loadValue (out field; const name : UTF8String);
    procedure load; virtual;
    procedure loadVals; virtual;
    procedure init (const script : UTF8String); virtual;
    procedure setup; virtual;
    procedure done; virtual;
    function defaultSuperclass : VALUE; virtual;
    procedure registerProperties (cls : TClass; value : VALUE); virtual;
    procedure check_type (value : VALUE; t : Integer); virtual;
    procedure check_data (value : VALUE); virtual; abstract;
    function get_data (value : VALUE) : Pointer; virtual;
    // property access
    function getErrInfo : VALUE; virtual; abstract;
    function getStdIn : VALUE; virtual;
    function getStdOut : VALUE; virtual;
    function getStdErr : VALUE; virtual;
    procedure setErrInfo (value : VALUE); virtual; abstract;
    procedure setStdIn (value : VALUE); virtual;
    procedure setStdOut (value : VALUE); virtual;
    procedure setStdErr (value : VALUE); virtual;
    // class methods
    class function defaultLibrary : UTF8String; virtual; abstract;
    class function defaultScript : UTF8String; virtual;
    class function constName (const name : AnsiString) : AnsiString;
    class function methodName (const name : AnsiString) : AnsiString;
  public
    rb_scan_args : function (argc : cint; argv : PVALUE; fmt : PChar) : cint; cdecl; varargs;
    rb_raise : procedure (exc : VALUE; fmt : PChar); cdecl; varargs;
    rb_notimplement : procedure; cdecl;
  public
    // constants
    const
      Qfalse = VALUE(0);
      Qtrue  = VALUE(2);
      Qnil   = VALUE(4);
      Qundef = VALUE(6);
      RUBY_SPECIAL_SHIFT = 8;
      FIXNUM_FLAG = $01;
      SYMBOL_FLAG = $0E;
    // class methods
    class function Auto : TRuby;
    class function Auto (const vers : array of TRubyClass) : TRuby;
    class procedure AddRegisterClassHook (cls : TClass; hook : FRegisterClassHook);
    class procedure DelRegisterClassHook (cls : TClass; hook : FRegisterClassHook);
    class procedure AddCompanionClass (const etype : ansistring; cls : TEventCompanionClass);
    class function GetCompanionClass (const etype : ansistring) : TEventCompanionClass;
    // constructor & destructor
    constructor Create (const lib, script : UTF8String); virtual;
    destructor Destroy; override;
    // properties (wrappers)
    property ErrInfo : VALUE read getErrInfo write setErrInfo;
    property StdIn : VALUE read getStdIn write setStdIn;
    property StdOut : VALUE read getStdOut write setStdOut;
    property StdErr : VALUE read getStdErr write setStdErr;
    function Description : UTF8String; virtual;
    // properties (wrappers: modules)
    property mKernel : VALUE read rb_mKernel;
    property mComparable : VALUE read rb_mComparable;
    property mEnumerable : VALUE read rb_mEnumerable;
    property mErrno : VALUE read rb_mErrno;
    property mFileTest : VALUE read rb_mFileTest;
    property mGC : VALUE read rb_mGC;
    property mMath : VALUE read rb_mMath;
    property mProcess : VALUE read rb_mProcess;
    // properties (wrappers: classes)
    property cObject : VALUE read rb_cObject;
    property cArray : VALUE read rb_cArray;
    property cBignum : VALUE read rb_cBignum;
    property cBinding : VALUE read rb_cBinding;
    property cClass : VALUE read rb_cClass;
    property cCont : VALUE read rb_cCont;
    property cData : VALUE read rb_cData;
    property cDir : VALUE read rb_cDir;
    property cEnumerator : VALUE read rb_cEnumerator;
    property cFalseClass : VALUE read rb_cFalseClass;
    property cFile : VALUE read rb_cFile;
    property cFixnum : VALUE read rb_cFixnum;
    property cFloat : VALUE read rb_cFloat;
    property cHash : VALUE read rb_cHash;
    property cInteger : VALUE read rb_cInteger;
    property cIO : VALUE read rb_cIO;
    property cMatch : VALUE read rb_cMatch;
    property cMethod : VALUE read rb_cMethod;
    property cModule : VALUE read rb_cModule;
    property cNameErrorMesg : VALUE read rb_cNameErrorMesg;
    property cNilClass : VALUE read rb_cNilClass;
    property cNumeric : VALUE read rb_cNumeric;
    property cProc : VALUE read rb_cProc;
    property cRange : VALUE read rb_cRange;
    property cRegexp : VALUE read rb_cRegexp;
    property cStat : VALUE read rb_cStat;
    property cString : VALUE read rb_cString;
    property cStruct : VALUE read rb_cStruct;
    property cSymbol : VALUE read rb_cSymbol;
    property cThread : VALUE read rb_cThread;
    property cTime : VALUE read rb_cTime;
    property cTrueClass : VALUE read rb_cTrueClass;
    property cUnboundMethod : VALUE read rb_cUnboundMethod;
    // properties (wrappers: exceptions)
    property eException : VALUE read rb_eException;
    property eStandardError : VALUE read rb_eStandardError;
    property eSystemExit : VALUE read rb_eSystemExit;
    property eInterrupt : VALUE read rb_eInterrupt;
    property eSignal : VALUE read rb_eSignal;
    property eFatal : VALUE read rb_eFatal;
    property eArgError : VALUE read rb_eArgError;
    property eEOFError : VALUE read rb_eEOFError;
    property eIndexError : VALUE read rb_eIndexError;
    property eStopIteration : VALUE read rb_eStopIteration;
    property eRangeError : VALUE read rb_eRangeError;
    property eIOError : VALUE read rb_eIOError;
    property eRuntimeError : VALUE read rb_eRuntimeError;
    property eSecurityError : VALUE read rb_eSecurityError;
    property eSystemCallError : VALUE read rb_eSystemCallError;
    property eThreadError : VALUE read rb_eThreadError;
    property eTypeError : VALUE read rb_eTypeError;
    property eZeroDivError : VALUE read rb_eZeroDivError;
    property eNotImpError : VALUE read rb_eNotImpError;
    property eNoMemError : VALUE read rb_eNoMemError;
    property eNoMethodError : VALUE read rb_eNoMethodError;
    property eFloatDomainError : VALUE read rb_eFloatDomainError;
    property eLocalJumpError : VALUE read rb_eLocalJumpError;
    property eSysStackError : VALUE read rb_eSysStackError;
    property eRegexpError : VALUE read rb_eRegexpError;
    property eScriptError : VALUE read rb_eScriptError;
    property eNameError : VALUE read rb_eNameError;
    property eSyntaxError : VALUE read rb_eSyntaxError;
    property eLoadError : VALUE read rb_eLoadError;
    // properties (wrap own)
    property mPascal : VALUE read rb_mPascal;
    // methods (wrappers)
    function Require (const name : UTF8String) : VALUE;
    function EvalString (const str : UTF8String) : VALUE;
    function DefineModule (const name : UTF8String) : VALUE;
    function DefineModule (ns : VALUE; const name : UTF8String) : VALUE;
    function DefineClass (const name : UTF8String; super : VALUE) : VALUE;
    function DefineClass (ns : VALUE; const name : UTF8String; super : VALUE) : VALUE;
    function DefineClass (const name : UTF8String) : VALUE;
    function DefineClass (ns : VALUE; const name : UTF8String) : VALUE;
    procedure DefineConstant (const name : UTF8String; value : VALUE);
    procedure DefineConstant (ns : VALUE; const name : UTF8String; value : VALUE);
    function Inspect (value : VALUE) : VALUE;
    function Intern (const str : UTF8String) : ID;
    function Val2Str (value : VALUE) : UTF8String;
    function Str2Val (const str : UTF8String) : VALUE; virtual;
    function Call (obj : VALUE; method : ID; const args : array of VALUE) : VALUE;
    function Call (obj : VALUE; name : UTF8String; const args : array of VALUE) : VALUE;
    function Send (obj : VALUE; method : ID; const args : array of VALUE) : VALUE;
    function Send (obj : VALUE; name : UTF8String; const args : array of VALUE) : VALUE;
    procedure DefineMethod (module : VALUE; const name : UTF8String; func : FRubyMethod0);
    procedure DefineMethod (module : VALUE; const name : UTF8String; func : FRubyMethod1);
    procedure DefineMethod (module : VALUE; const name : UTF8String; func : FRubyMethod2);
    procedure DefineMethod (module : VALUE; const name : UTF8String; func : FRubyMethod3);
    procedure DefineMethod (module : VALUE; const name : UTF8String; func : FRubyMethod4);
    procedure DefineMethod (module : VALUE; const name : UTF8String; func : FRubyMethod5);
    procedure DefineMethod (module : VALUE; const name : UTF8String; func : FRubyMethod6);
    procedure DefineMethod (module : VALUE; const name : UTF8String; func : FRubyMethod7);
    procedure DefineMethod (module : VALUE; const name : UTF8String; func : FRubyMethod8);
    procedure DefineMethod (module : VALUE; const name : UTF8String; func : FRubyMethod9);
    procedure DefineMethod (module : VALUE; const name : UTF8String; func : FRubyMethod10);
    procedure DefineMethod (module : VALUE; const name : UTF8String; func : FRubyMethod11);
    procedure DefineMethod (module : VALUE; const name : UTF8String; func : FRubyMethod12);
    procedure DefineMethod (module : VALUE; const name : UTF8String; func : FRubyMethod13);
    procedure DefineMethod (module : VALUE; const name : UTF8String; func : FRubyMethod14);
    procedure DefineMethod (module : VALUE; const name : UTF8String; func : FRubyMethod15);
    procedure DefineMethod (module : VALUE; const name : UTF8String; func : FRubyMethod16);
    procedure DefineMethod (module : VALUE; const name : UTF8String; func : FRubyMethod17);
    procedure DefineMethod (module : VALUE; const name : UTF8String; func : FRubyMethodArr);
    procedure DefineModuleFunction (module : VALUE; const name : UTF8String; func : FRubyMethod0);
    procedure DefineModuleFunction (module : VALUE; const name : UTF8String; func : FRubyMethod1);
    procedure DefineModuleFunction (module : VALUE; const name : UTF8String; func : FRubyMethod2);
    procedure DefineModuleFunction (module : VALUE; const name : UTF8String; func : FRubyMethod3);
    procedure DefineModuleFunction (module : VALUE; const name : UTF8String; func : FRubyMethod4);
    procedure DefineModuleFunction (module : VALUE; const name : UTF8String; func : FRubyMethod5);
    procedure DefineModuleFunction (module : VALUE; const name : UTF8String; func : FRubyMethod6);
    procedure DefineModuleFunction (module : VALUE; const name : UTF8String; func : FRubyMethod7);
    procedure DefineModuleFunction (module : VALUE; const name : UTF8String; func : FRubyMethod8);
    procedure DefineModuleFunction (module : VALUE; const name : UTF8String; func : FRubyMethod9);
    procedure DefineModuleFunction (module : VALUE; const name : UTF8String; func : FRubyMethod10);
    procedure DefineModuleFunction (module : VALUE; const name : UTF8String; func : FRubyMethod11);
    procedure DefineModuleFunction (module : VALUE; const name : UTF8String; func : FRubyMethod12);
    procedure DefineModuleFunction (module : VALUE; const name : UTF8String; func : FRubyMethod13);
    procedure DefineModuleFunction (module : VALUE; const name : UTF8String; func : FRubyMethod14);
    procedure DefineModuleFunction (module : VALUE; const name : UTF8String; func : FRubyMethod15);
    procedure DefineModuleFunction (module : VALUE; const name : UTF8String; func : FRubyMethod16);
    procedure DefineModuleFunction (module : VALUE; const name : UTF8String; func : FRubyMethod17);
    procedure DefineModuleFunction (module : VALUE; const name : UTF8String; func : FRubyMethodArr);
    procedure DefineSingletonMethod (obj : VALUE; const name : UTF8String; func : FRubyMethod0);
    procedure DefineSingletonMethod (obj : VALUE; const name : UTF8String; func : FRubyMethod1);
    procedure DefineSingletonMethod (obj : VALUE; const name : UTF8String; func : FRubyMethod2);
    procedure DefineSingletonMethod (obj : VALUE; const name : UTF8String; func : FRubyMethod3);
    procedure DefineSingletonMethod (obj : VALUE; const name : UTF8String; func : FRubyMethod4);
    procedure DefineSingletonMethod (obj : VALUE; const name : UTF8String; func : FRubyMethod5);
    procedure DefineSingletonMethod (obj : VALUE; const name : UTF8String; func : FRubyMethod6);
    procedure DefineSingletonMethod (obj : VALUE; const name : UTF8String; func : FRubyMethod7);
    procedure DefineSingletonMethod (obj : VALUE; const name : UTF8String; func : FRubyMethod8);
    procedure DefineSingletonMethod (obj : VALUE; const name : UTF8String; func : FRubyMethod9);
    procedure DefineSingletonMethod (obj : VALUE; const name : UTF8String; func : FRubyMethod10);
    procedure DefineSingletonMethod (obj : VALUE; const name : UTF8String; func : FRubyMethod11);
    procedure DefineSingletonMethod (obj : VALUE; const name : UTF8String; func : FRubyMethod12);
    procedure DefineSingletonMethod (obj : VALUE; const name : UTF8String; func : FRubyMethod13);
    procedure DefineSingletonMethod (obj : VALUE; const name : UTF8String; func : FRubyMethod14);
    procedure DefineSingletonMethod (obj : VALUE; const name : UTF8String; func : FRubyMethod15);
    procedure DefineSingletonMethod (obj : VALUE; const name : UTF8String; func : FRubyMethod16);
    procedure DefineSingletonMethod (obj : VALUE; const name : UTF8String; func : FRubyMethod17);
    procedure DefineSingletonMethod (obj : VALUE; const name : UTF8String; func : FRubyMethodArr);
    procedure DefineGlobalFunction (const name : UTF8String; func : FRubyMethod0);
    procedure DefineGlobalFunction (const name : UTF8String; func : FRubyMethod1);
    procedure DefineGlobalFunction (const name : UTF8String; func : FRubyMethod2);
    procedure DefineGlobalFunction (const name : UTF8String; func : FRubyMethod3);
    procedure DefineGlobalFunction (const name : UTF8String; func : FRubyMethod4);
    procedure DefineGlobalFunction (const name : UTF8String; func : FRubyMethod5);
    procedure DefineGlobalFunction (const name : UTF8String; func : FRubyMethod6);
    procedure DefineGlobalFunction (const name : UTF8String; func : FRubyMethod7);
    procedure DefineGlobalFunction (const name : UTF8String; func : FRubyMethod8);
    procedure DefineGlobalFunction (const name : UTF8String; func : FRubyMethod9);
    procedure DefineGlobalFunction (const name : UTF8String; func : FRubyMethod10);
    procedure DefineGlobalFunction (const name : UTF8String; func : FRubyMethod11);
    procedure DefineGlobalFunction (const name : UTF8String; func : FRubyMethod12);
    procedure DefineGlobalFunction (const name : UTF8String; func : FRubyMethod13);
    procedure DefineGlobalFunction (const name : UTF8String; func : FRubyMethod14);
    procedure DefineGlobalFunction (const name : UTF8String; func : FRubyMethod15);
    procedure DefineGlobalFunction (const name : UTF8String; func : FRubyMethod16);
    procedure DefineGlobalFunction (const name : UTF8String; func : FRubyMethod17);
    procedure DefineGlobalFunction (const name : UTF8String; func : FRubyMethodArr);
    procedure DefineAlias (module : VALUE; const target, source : UTF8String);
    procedure DefineAttribute(module : VALUE; const name : UTF8String;
                getter : FRubyMethod0; setter : FRubyMethod1;
                isbool : Boolean = false);
    procedure Include (target, source : VALUE);
    function GlobalVarGet (const name : UTF8String) : VALUE;
    procedure GlobalVarSet (const name : UTF8String; value : VALUE);
    function BlockGiven : Boolean;
    function Yield (arg : VALUE) : VALUE;
    function ArrayNew : VALUE;
    function ArrayPush (arr : VALUE; value : VALUE) : VALUE;
    function HashNew : VALUE;
    function HashGet (hash : VALUE; key : VALUE) : VALUE;
    function HashSet (hash : VALUE; key, value : VALUE) : VALUE;
    procedure Error (e : Exception);
    // methods (coversions)
    function Int2Val (n : PtrInt) : VALUE;
    function SLL2Val (const n : Int64) : VALUE;
    function ULL2Val (const n : QWord) : VALUE;
    function Str2Sym (const str : UTF8String) : VALUE;
    function Str2Set (const str : UTF8String) : VALUE;
    function Flt2Val (const x : Double) : VALUE;
    function Bln2Val (b : Boolean) : VALUE;
    function Val2Int (value : VALUE) : PtrInt;
    function Val2SLL (value : VALUE) : Int64;
    function Val2ULL (value : VALUE) : QWord;
    function Sym2Str (value : VALUE) : UTF8String;
    function Set2Str (value : VALUE) : UTF8String;
    function Val2Flt (value : VALUE) : Double;
    function Val2Bln (value : VALUE) : Boolean;
    function IsSymbol (value : VALUE) : Boolean;
    function IsFixnum (value : VALUE) : Boolean;
    function ValType (value : VALUE) : Integer; virtual; abstract;
    function IsData (value : VALUE) : Boolean; virtual; abstract;
    // properties (self)
    property GlobalVars [const name : UTF8String] : VALUE read GlobalVarGet write GlobalVarSet; default;
    // methods (self)
    function Cls2Val (cls : TClass) : VALUE;
    function Val2Cls (value : VALUE) : TClass;
    function RegisterUnit (const name : AnsiString) : VALUE;
    function Obj2Val (obj : TObject) : VALUE;
    function Val2Obj (value : VALUE) : TObject;
  end;

  { TRuby18 }

  TRuby18 = class(TRuby)
  protected
    const
      T_NIL    = $01;
      T_FIXNUM = $0A;
      T_TRUE   = $20;
      T_FALSE  = $21;
      T_DATA   = $22;
      T_SYMBOL = $24;
      T_UNDEF  = $3C;
      T_MASK   = $3F;
  protected
    // fields: error
    ruby_errinfo : PVALUE;
    // fields: ruby objects
    rb_mPrecision : VALUE;
    // overrided methods
    procedure load; override;
    procedure loadVals; override;
    procedure setup; override;
    procedure check_data(value : VALUE); override;
    // property access
    function getErrInfo : VALUE; override;
    procedure setErrInfo(value : VALUE); override;
    // class methods
    class function defaultLibrary : UTF8String; override;
  public
    // methods
    function ValType (value : VALUE) : Integer; override;
    function IsData(value : VALUE) : Boolean; override;
    // properties (wrappers)
    property mPrecision : VALUE read rb_mPrecision;
    function Description : UTF8String; override;
  end;

  { TRuby19 }

  TRuby19 = class(TRuby)
  protected
    const
      T_DATA   = $0C;
      T_NIL    = $11;
      T_TRUE   = $12;
      T_FALSE  = $13;
      T_SYMBOL = $14;
      T_FIXNUM = $15;
      T_UNDEF  = $1B;
      T_MASK   = $1F;
  protected
    // fields: rb_ functions
    rb_errinfo : function : VALUE; cdecl;
    rb_set_errinfo : procedure (value : VALUE); cdecl;
    // fields: ruby objects
    rb_mWaitReadable, rb_mWaitWritable : VALUE;
    rb_cBasicObject, rb_cEncoding, rb_cRandom, rb_cRational,
      rb_cComplex : VALUE;
    rb_eKeyError, rb_eEncodingError, rb_eEncCompatError : VALUE;
    // overrided methods
    procedure load; override;
    procedure loadVals; override;
    procedure setup; override;
    procedure check_data(value : VALUE); override;
    // property access
    function getErrInfo : VALUE; override;
    procedure setErrInfo(value : VALUE); override;
    // class methods
    class function defaultLibrary : UTF8String; override;
  public
    // methods
    function Str2Val(const str : UTF8String) : VALUE; override;
    function ValType (value : VALUE) : Integer; override;
    function IsData(value : VALUE) : Boolean; override;
    // properties (wrappers)
    property mWaitReadable : VALUE read rb_mWaitReadable;
    property mWaitWritable : VALUE read rb_mWaitWritable;
    property cBasicObject : VALUE read rb_cBasicObject;
    property cEncoding : VALUE read rb_cEncoding;
    property cRandom : VALUE read rb_cRandom;
    property cRational : VALUE read rb_cRational;
    property cComplex : VALUE read rb_cComplex;
    property eKeyError : VALUE read rb_eKeyError;
    property eEncodingError : VALUE read rb_eEncodingError;
    property eEncCompatError : VALUE read rb_eEncCompatError;
  end;

  { TRuby20 }

  TRuby20 = class(TRuby19)
  protected
    // class methods
    class function defaultLibrary : UTF8String; override;
  end;

  TOutClass = class of TPascalOut;

  TPascalOut = class
  private
    rbOwner : TRuby;
    function getIO : VALUE;
  protected
    function Write (txt : VALUE) : VALUE; virtual; abstract;
  public
    class function RbClass (rb : TRuby) : VALUE;
    constructor Create (rb : TRuby);
    property Owner : TRuby read rbOwner;
    property IO : VALUE read getIO;
  end;

{ Common }

procedure unpack_object (value : VALUE; out rec : TPack);

type
  ERuby = class(Exception);

  ENoRuby = class(ERuby);

  { ERubyEval }

  ERubyEval = class(ERuby)
  private
    fldErrInfo : VALUE;
  public
    property ErrInfo : VALUE read fldErrInfo;
    constructor Create (info : VALUE; const msg : UTF8String);
  end;

  ERubyType = class(ERubyEval);

implementation

const
  msgNoRuby = 'The file "%s" not found or not a ruby library.';
  msgPasErr = '%s: %s';

{ ERubyEval }

constructor ERubyEval.Create(info : VALUE; const msg : UTF8String);
 begin
 fldErrInfo := info;
 inherited Create(msg)
 end;

{ TRuby }

class function TRuby.findHook(cls : TClass; hook : FRegisterClassHook; out
  idx : Integer) : Boolean;
 var
   i : Integer;
 begin
 for i := 0 to High(listHooks) do
     if (listHooks[i].cls = cls) and (listHooks[i].hook = hook)
        then begin
             idx := i;
             result := true;
             Exit;
             end;
 result := false;
 end;

function TRuby.findClass(cls : TClass; out value : VALUE) : Boolean;
 var
   i : Integer;
 begin
 for i := 0 to High(cacheClasses) do
     if cacheClasses[i].cls = cls
        then begin
             value := cacheClasses[i].value;
             result := true;
             Exit;
             end;
 result := false;
 end;

function TRuby.findUnit(const name : AnsiString; out value : VALUE) : Boolean;
 var
   i : Integer;
   nm : AnsiString;
 begin
 nm := UpCase(name);
 for i := 0 to High(cacheUnits) do
     if cacheUnits[i].name = nm
        then begin
             value := cacheUnits[i].value;
             result := true;
             Exit;
             end;
 result := false;
 end;

function TRuby.findOutClass(cls : TPascalOutClass; out value : VALUE) : Boolean;
 var
   i : Integer;
 begin
  for i := 0 to High(cacheOutClasses) do
      if cacheOutClasses[i].cls = cls
         then begin
               value := cacheOutClasses[i].value;
               result := true;
               Exit;
              end;
  result := false;
 end;

class function TRuby.EngineByModule(value : VALUE) : TRuby;
 var
   i : Integer;
 begin
  for i := 0 to Length(cacheModules) do
      if cacheModules[i].module = value
         then begin
               result := cacheModules[i].engine;
               Exit;
              end;
  result := nil;
 end;

procedure TRuby.registerModule(module : VALUE);
 var
   l : Integer;
 begin
  l := Length(cacheModules);
  SetLength(cacheModules, l + 1);
  cacheModules[l].engine := self;
  cacheModules[l].module := module;
 end;

procedure TRuby.registerModules(const modules : array of VALUE);
 var
   i : Integer;
 begin
  for i := 0 to High(modules) do
      registerModule(modules[i]);
 end;

procedure TRuby.freeModules;
 var
   newModules : TModPackArray;
   i, l : Integer;
 begin
  SetLength(newModules, 0);
  for i := 0 to High(cacheModules) do
      if cacheModules[i].engine <> self
         then begin
               l := Length(newModules);
               SetLength(newModules, l + 1);
               newModules[l] := cacheModules[i];
              end;
  SetLength(cacheModules, 0);
  cacheModules := newModules;
 end;

class procedure TRuby.AddInitHook(hook : FInitHook);
 var
   l : Integer;
 begin
  l := Length(listInitHooks);
  SetLength(listInitHooks, l + 1);
  listInitHooks[l] := hook;
 end;

class procedure TRuby.DelInitHook(hook : FInitHook);
 var
   i, h, d : Integer;
 begin
  h := High(listInitHooks);
  for i := 0 to h do
      if listInitHooks[i] = hook
         then begin
               for d := i to h - 1 do
                   listInitHooks[d] := listInitHooks[d + 1];
               SetLength(listInitHooks, h);
               Exit;
              end;
 end;

procedure TRuby.loadFunc (out field; const name : UTF8String);
 begin
 Pointer(field) := GetProcedureAddress(libHandle, name);
 end;

procedure TRuby.loadPtr (out field; const name : UTF8String);
 begin
 Pointer(field) := GetProcedureAddress(libHandle, name);
 end;

procedure TRuby.loadValue (out field; const name : UTF8String);
 var
   v : VALUE;
 begin
 v := PVALUE(GetProcedureAddress(libHandle, name))^;
 VALUE(field) := v;
 registerModule(v);
 end;

procedure TRuby.load;
 begin
 // ruby_ functions
 loadFunc(ruby_init,          'ruby_init');
 loadFunc(ruby_init_loadpath, 'ruby_init_loadpath');
 loadFunc(ruby_script,        'ruby_script');
 loadFunc(ruby_finalize,      'ruby_finalize');
 // rb_ functions
 loadFunc(rb_define_module,           'rb_define_module');
 loadFunc(rb_define_module_under,     'rb_define_module_under');
 loadFunc(rb_define_class,            'rb_define_class');
 loadFunc(rb_define_class_under,      'rb_define_class_under');
 loadFunc(rb_define_const,            'rb_define_const');
 loadFunc(rb_define_global_const,     'rb_define_global_const');
 loadFunc(rb_define_method,           'rb_define_method');
 loadFunc(rb_define_module_function,  'rb_define_module_function');
 loadFunc(rb_define_singleton_method, 'rb_define_singleton_method');
 loadFunc(rb_define_global_function,  'rb_define_global_function');
 loadFunc(rb_eval_string_protect,     'rb_eval_string_protect');
 loadFunc(rb_string_value_cstr,       'rb_string_value_cstr');
 loadFunc(rb_inspect,                 'rb_inspect');
 loadFunc(rb_funcall2,                'rb_funcall2');
 loadFunc(rb_funcall3,                'rb_funcall3');
 loadFunc(rb_intern,                  'rb_intern');
 loadFunc(rb_str_new2,                'rb_str_new2');
 loadFunc(rb_locale_str_new_cstr,     'rb_locale_str_new_cstr');
 loadFunc(rb_data_object_alloc,       'rb_data_object_alloc');
 loadFunc(rb_protect,                 'rb_protect');
 loadFunc(rb_check_type,              'rb_check_type');
 loadFunc(rb_int2inum,                'rb_int2inum');
 loadFunc(rb_num2int,                 'rb_num2int');
 loadFunc(rb_ll2inum,                 'rb_ll2inum');
 loadFunc(rb_ull2inum,                'rb_ull2inum');
 loadFunc(rb_num2ll,                  'rb_num2ll');
 loadFunc(rb_num2ull,                 'rb_num2ull');
 loadFunc(rb_require,                 'rb_require');
 loadFunc(rb_float_new,               'rb_float_new');
 loadFunc(rb_to_id,                   'rb_to_id');
 loadFunc(rb_id2name,                 'rb_id2name');
 loadFunc(rb_num2dbl,                 'rb_num2dbl');
 loadFunc(rb_include_module,          'rb_include_module');
 loadFunc(rb_define_alias,            'rb_define_alias');
 loadFunc(rb_gv_get,                  'rb_gv_get');
 loadFunc(rb_gv_set,                  'rb_gv_set');
 loadFunc(rb_block_given_p,           'rb_block_given_p');
 loadFunc(rb_yield,                   'rb_yield');
 loadFunc(rb_ary_new4,                'rb_ary_new4');
 loadFunc(rb_hash_new,                'rb_hash_new');
 loadFunc(rb_hash_aref,               'rb_hash_aref');
 loadFunc(rb_hash_aset,               'rb_hash_aset');
 loadFunc(rb_ary_new,                 'rb_ary_new');
 loadFunc(rb_ary_new2,                'rb_ary_new2');
 loadFunc(rb_ary_push,                'rb_ary_push');
 loadFunc(rb_scan_args,               'rb_scan_args');
 loadFunc(rb_raise,                   'rb_raise');
 loadFunc(rb_notimplement,            'rb_notimplement');
 loadFunc(rb_enc_str_new,             'rb_enc_str_new');
 loadFunc(rb_utf8_encoding,           'rb_utf8_encoding');
 // io
 loadPtr(rb_stdin,  'rb_stdin');
 loadPtr(rb_stdout, 'rb_stdout');
 loadPtr(rb_stderr, 'rb_stderr');
 // other
 loadPtr(ruby_description, 'ruby_description');
 end;

procedure TRuby.loadVals;
 begin
 // modules
 loadValue(rb_mKernel,     'rb_mKernel');
 loadValue(rb_mComparable, 'rb_mComparable');
 loadValue(rb_mEnumerable, 'rb_mEnumerable');
 loadValue(rb_mErrno,      'rb_mErrno');
 loadValue(rb_mFileTest,   'rb_mFileTest');
 loadValue(rb_mGC,         'rb_mGC');
 loadValue(rb_mMath,       'rb_mMath');
 loadValue(rb_mProcess,    'rb_mProcess');
 // classes
 loadValue(rb_cObject,        'rb_cObject');
 loadValue(rb_cArray,         'rb_cArray');
 loadValue(rb_cBignum,        'rb_cBignum');
 loadValue(rb_cBinding,       'rb_cBinding');
 loadValue(rb_cClass,         'rb_cClass');
 loadValue(rb_cData,          'rb_cData');
 loadValue(rb_cDir,           'rb_cDir');
 loadValue(rb_cEnumerator,    'rb_cEnumerator');
 loadValue(rb_cFalseClass,    'rb_cFalseClass');
 loadValue(rb_cFile,          'rb_cFile');
 loadValue(rb_cFixnum,        'rb_cFixnum');
 loadValue(rb_cFloat,         'rb_cFloat');
 loadValue(rb_cHash,          'rb_cHash');
 loadValue(rb_cInteger,       'rb_cInteger');
 loadValue(rb_cIO,            'rb_cIO');
 loadValue(rb_cMatch,         'rb_cMatch');
 loadValue(rb_cMethod,        'rb_cMethod');
 loadValue(rb_cModule,        'rb_cModule');
 loadValue(rb_cNameErrorMesg, 'rb_cNameErrorMesg');
 loadValue(rb_cProc,          'rb_cProc');
 loadValue(rb_cRange,         'rb_cRange');
 loadValue(rb_cRegexp,        'rb_cRegexp');
 loadValue(rb_cStat,          'rb_cStat');
 loadValue(rb_cString,        'rb_cString');
 loadValue(rb_cStruct,        'rb_cStruct');
 loadValue(rb_cSymbol,        'rb_cSymbol');
 loadValue(rb_cThread,        'rb_cThread');
 loadValue(rb_cTime,          'rb_cTime');
 loadValue(rb_cTrueClass,     'rb_cTrueClass');
 loadValue(rb_cUnboundMethod, 'rb_cUnboundMethod');
 // exceptions
 loadValue(rb_eException,        'rb_eException');
 loadValue(rb_eStandardError,    'rb_eStandardError');
 loadValue(rb_eSystemExit,       'rb_eSystemExit');
 loadValue(rb_eInterrupt,        'rb_eInterrupt');
 loadValue(rb_eSignal,           'rb_eSignal');
 loadValue(rb_eFatal,            'rb_eFatal');
 loadValue(rb_eArgError,         'rb_eArgError');
 loadValue(rb_eEOFError,         'rb_eEOFError');
 loadValue(rb_eIndexError,       'rb_eIndexError');
 loadValue(rb_eStopIteration,    'rb_eStopIteration');
 loadValue(rb_eRangeError,       'rb_eRangeError');
 loadValue(rb_eIOError,          'rb_eIOError');
 loadValue(rb_eRuntimeError,     'rb_eRuntimeError');
 loadValue(rb_eSecurityError,    'rb_eSecurityError');
 loadValue(rb_eSystemCallError,  'rb_eSystemCallError');
 loadValue(rb_eThreadError,      'rb_eThreadError');
 loadValue(rb_eTypeError,        'rb_eTypeError');
 loadValue(rb_eZeroDivError,     'rb_eZeroDivError');
 loadValue(rb_eNotImpError,      'rb_eNotImpError');
 loadValue(rb_eNoMemError,       'rb_eNoMemError');
 loadValue(rb_eNoMethodError,    'rb_eNoMethodError');
 loadValue(rb_eFloatDomainError, 'rb_eFloatDomainError');
 loadValue(rb_eLocalJumpError,   'rb_eLocalJumpError');
 loadValue(rb_eSysStackError,    'rb_eSysStackError');
 loadValue(rb_eRegexpError,      'rb_eRegexpError');
 loadValue(rb_eScriptError,      'rb_eScriptError');
 loadValue(rb_eNameError,        'rb_eNameError');
 loadValue(rb_eSyntaxError,      'rb_eSyntaxError');
 loadValue(rb_eLoadError,        'rb_eLoadError');
 end;

procedure TRuby.init(const script : UTF8String);
 begin
 ruby_init;
 ruby_init_loadpath;
 ruby_script(PChar(script));
 end;

procedure TRuby.setup;
 begin
  rb_require('set');
  rb_mPascal := DefineModule('Pascal');
  rb_ePascalError := DefineClass(rb_mPascal, 'Error', rb_eStandardError);
  registerModule(EvalString('self'));
 end;

procedure TRuby.done;
 begin
 ruby_finalize;
 UnloadLibrary(libHandle);
 end;

function TRuby.defaultSuperclass : VALUE;
 begin
 result := cData;
 end;

const
  RubifiedProperties = [tkInteger, tkInt64, tkQWord, tkEnumeration, tkSet,
                        tkFloat, tkSString, tkLString, tkAString, tkUString,
                        tkWString, tkUChar, tkChar, tkWChar, tkBool, tkClass];

  RubifiedEvents = [tkMethod];

procedure TRuby.registerProperties (cls : TClass; value : VALUE);
 var
   data : PTypeData;
   list : PPropList;
   prop : PPropInfo;
   i : Integer;
   nm, tp : AnsiString;
   code : UTF8String;
 begin
 data := GetTypeData(cls.ClassInfo);
 GetPropList(cls, list);
 for i := 0 to data^.PropCount - 1 do
     begin
     prop := list^[i];
     if prop^.PropType^.Kind in RubifiedProperties
        then begin
             nm := methodName(prop^.Name);
             code := 'attr_accessor :' + nm + LineEnding +
                     'def ' + nm + LineEnding +
                     '  pascal_get_prop(''' + nm + ''')' + LineEnding +
                     'end' + LineEnding +
                     'def ' + nm + '=(value)' + LineEnding +
                     '  pascal_set_prop(''' + nm + ''', value)' + LineEnding +
                     'end';
             if prop^.PropType^.Kind = tkBool
                then code := code + LineEnding + 'alias :' + nm + '? :' + nm;
             Send(value, 'class_eval', [Str2Val(code)]);
             end
        else if prop^.PropType^.Kind in RubifiedEvents
                then begin
                     nm := methodName(prop^.Name);
                     tp := UpperCase(prop^.PropType^.Name);
                     code := 'def ' + nm + '(&block)' + LineEnding +
                             '  $pp_events ||= []' + LineEnding +
                             '  $pp_events << block' + LineEnding +
                             '  pascal_event(''' + nm + ''', ''' + tp + ''', &block)' + LineEnding +
                             'end';
                     Send(value, 'class_eval', [Str2Val(code)]);
                     end;
     end;
 end;

type
  TCheckTypeRec = record
    func : procedure (value : VALUE; t : cint); cdecl;
    value : VALUE;
    t : Integer;
  end;
  PCheckTypeRec = ^TCheckTypeRec;

function check_type_wrapper (value : VALUE) : VALUE; cdecl;
 var
   p : PCheckTypeRec;
 begin
{$hints off}
 p := PCheckTypeRec(value);
{$hints on}
 p^.func(p^.value, p^.t);
 result := TRuby.Qtrue
 end;

procedure TRuby.check_type(value : VALUE; t : Integer);
 var
   rec : TCheckTypeRec;
   val : VALUE;
   res : Integer;
   err : VALUE;
 begin
 rec.func := rb_check_type;
 rec.value := value;
 rec.t := t;
{$hints off}
 val := PtrUInt(@rec);
{$hints on}
 rb_protect(@check_type_wrapper, val, res);
 if res <> 0
    then begin
         err := getErrInfo;
         raise ERubyType.Create(err, Val2Str(Inspect(err)));
         end;
 end;

type
  RBasic = record
    flags : VALUE;
    klass : VALUE;
  end;
  PRBasic = ^RBasic;

  RData = record
    basic : RBasic;
    dmark, dfree : FRubyDataFunc;
    data : Pointer;
  end;
  PRData = ^RData;

function TRuby.get_data (value : VALUE) : Pointer;
 begin
 check_data(value);
{$hints off}
 result := PRData(value)^.data;
{$hints on}
 end;

function TRuby.getStdIn : VALUE;
 begin
 result := rb_stdin^;
 end;

function TRuby.getStdOut : VALUE;
 begin
 result := rb_stdout^;
 end;

function TRuby.getStdErr : VALUE;
 begin
 result := rb_stderr^;
 end;

procedure TRuby.setStdIn(value : VALUE);
 begin
 rb_stdin^ := value;
 end;

procedure TRuby.setStdOut(value : VALUE);
 begin
 rb_stdout^ := value;
 end;

procedure TRuby.setStdErr(value : VALUE);
 begin
 rb_stderr^ := value;
 end;

class function TRuby.defaultScript : UTF8String;
 begin
 result := ParamStr(0);
 end;

class function TRuby.constName(const name : AnsiString) : AnsiString;
 begin
 result := name;
 result[1] := UpCase(name[1]);
 end;

class function TRuby.methodName(const name : AnsiString) : AnsiString;
 begin
 result := name;
 result[1] := LowerCase(name[1]);
 end;

class function TRuby.Auto : TRuby;
 begin
 result := Create(defaultLibrary, defaultScript)
 end;

class function TRuby.Auto(const vers : array of TRubyClass) : TRuby;
 var
   idx : Integer;
 begin
 for idx := 0 to High(vers) do
     try
       result := vers[idx].Auto;
     except
       on ENoRuby do
          Continue;
     end;
 result := nil;
 end;

class procedure TRuby.AddRegisterClassHook(cls : TClass;
  hook : FRegisterClassHook);
 var
   i, l : Integer;
 begin
 if not findHook(cls, hook, i)
    then begin
         l := Length(listHooks);
         SetLength(listHooks, l + 1);
         listHooks[l].cls := cls;
         listHooks[l].hook := hook;
         end;
 end;

class procedure TRuby.DelRegisterClassHook(cls : TClass;
  hook : FRegisterClassHook);
 var
   i, h, d : Integer;
 begin
 if findHook(cls, hook, i)
    then begin
         h := High(listHooks);
         for d := i to h - 1 do
             listHooks[d] := listHooks[d + 1];
         SetLength(listHooks, h);
         end;
 end;

class procedure TRuby.AddCompanionClass(const etype : ansistring;
  cls : TEventCompanionClass);
 var
   l : Integer;
 begin
 l := Length(listCompanions);
 SetLength(listCompanions, l + 1);
 listCompanions[l].name := UpperCase(etype);
 listCompanions[l].cls := cls;
 end;

class function TRuby.GetCompanionClass(
  const etype : ansistring) : TEventCompanionClass;
 var
   i : Integer;
   n : ansistring;
 begin
 n := UpperCase(etype);
 for i := 0 to High(listCompanions) do
     if listCompanions[i].name = n
        then begin
             result := listCompanions[i].cls;
             Exit;
             end;
 result := nil;
 end;

constructor TRuby.Create(const lib, script : UTF8String);
 var
   i : Integer;
 begin
 inherited Create;
 libHandle := LoadLibrary(lib);
 if libHandle = 0
    then raise ENoRuby.CreateFmt(msgNoRuby, [lib]);
 load;
 init(script);
 loadVals;
 setup;
 for i := 0 to High(listInitHooks) do
     listInitHooks[i](self);
 end;

destructor TRuby.Destroy;
 var
   i : Integer;
 begin
 done;
 for i := 0 to High(cacheCompanions) do
     cacheCompanions[i].Free;
 SetLength(cacheCompanions, 0);
 freeModules;
 inherited Destroy;
 end;

function TRuby.Description : UTF8String;
 begin
 result := UTF8String(PChar(ruby_description)) + '';
 end;

function TRuby.Require(const name : UTF8String) : VALUE;
 begin
 result := rb_require(PChar(name));
 end;

function TRuby.EvalString(const str : UTF8String) : VALUE;
 var
   res : Integer;
   err : VALUE;
 begin
 setErrInfo(Qnil);
 rb_gv_set('script_string', Str2Val(str));
 result := rb_eval_string_protect('eval $script_string', res);
 if res <> 0
    then begin
         err := getErrInfo;
         raise ERubyEval.Create(err, Val2Str(Inspect(err)));
         result := err;
         end;
 end;

function TRuby.DefineModule(const name : UTF8String) : VALUE;
 begin
 result := rb_define_module(PChar(name));
 registerModule(result);
 end;

function TRuby.DefineModule(ns : VALUE; const name : UTF8String) : VALUE;
 begin
 result := rb_define_module_under(ns, PChar(name));
 registerModule(result);
 end;

function TRuby.DefineClass(const name : UTF8String; super : VALUE) : VALUE;
 begin
 result := rb_define_class(PChar(name), super);
 registerModule(result);
 end;

function TRuby.DefineClass(ns : VALUE; const name : UTF8String;
  super : VALUE) : VALUE;
 begin
 result := rb_define_class_under(ns, PChar(name), super);
 registerModule(result);
 end;

function TRuby.DefineClass(const name : UTF8String) : VALUE;
 begin
 result := rb_define_class(PChar(name), rb_cObject);
 registerModule(result);
 end;

function TRuby.DefineClass(ns : VALUE; const name : UTF8String) : VALUE;
 begin
 result := rb_define_class_under(ns, PChar(name), rb_cObject);
 registerModule(result);
 end;

procedure TRuby.DefineConstant(const name : UTF8String; value : VALUE);
 begin
 rb_define_global_const(PChar(name), value);
 end;

procedure TRuby.DefineConstant(ns : VALUE; const name : UTF8String;
  value : VALUE);
 begin
 rb_define_const(ns, PChar(name), value);
 end;

function TRuby.Inspect(value : VALUE) : VALUE;
 begin
 result := rb_inspect(value);
 end;

function TRuby.Intern(const str : UTF8String) : ID;
 begin
 result := rb_intern(PChar(str));
 end;

function TRuby.Val2Str(value : VALUE) : UTF8String;
 begin
 result := UTF8String(rb_string_value_cstr(value)) + '';
 end;

function TRuby.Str2Val(const str : UTF8String) : VALUE;
 begin
 result := rb_str_new2(PChar(str));
 end;

function TRuby.Call (obj : VALUE; method : ID;
  const args : array of VALUE) : VALUE;
 begin
 result := rb_funcall3(obj, method, Length(args), @args[0]);
 end;

function TRuby.Call(obj : VALUE; name : UTF8String;
  const args : array of VALUE) : VALUE;
 begin
 result := Call(obj, Intern(name), args);
 end;

function TRuby.Send(obj : VALUE; method : ID;
  const args : array of VALUE) : VALUE;
 begin
 result := rb_funcall2(obj, method, Length(args), @args[0]);
 end;

function TRuby.Send(obj : VALUE; name : UTF8String;
  const args : array of VALUE) : VALUE;
 begin
 result := Send(obj, Intern(name), args);
 end;

procedure TRuby.DefineMethod(module : VALUE; const name : UTF8String;
  func : FRubyMethod0);
 begin
 rb_define_method(module, PChar(name), func, 0);
 end;

procedure TRuby.DefineMethod(module : VALUE; const name : UTF8String;
  func : FRubyMethod1);
 begin
 rb_define_method(module, PChar(name), func, 1);
 end;

procedure TRuby.DefineMethod(module : VALUE; const name : UTF8String;
  func : FRubyMethod2);
 begin
 rb_define_method(module, PChar(name), func, 2);
 end;

procedure TRuby.DefineMethod(module : VALUE; const name : UTF8String;
  func : FRubyMethod3);
 begin
 rb_define_method(module, PChar(name), func, 3);
 end;

procedure TRuby.DefineMethod(module : VALUE; const name : UTF8String;
  func : FRubyMethod4);
 begin
 rb_define_method(module, PChar(name), func, 4);
 end;

procedure TRuby.DefineMethod(module : VALUE; const name : UTF8String;
  func : FRubyMethod5);
 begin
 rb_define_method(module, PChar(name), func, 5);
 end;

procedure TRuby.DefineMethod(module : VALUE; const name : UTF8String;
  func : FRubyMethod6);
 begin
 rb_define_method(module, PChar(name), func, 6);
 end;

procedure TRuby.DefineMethod(module : VALUE; const name : UTF8String;
  func : FRubyMethod7);
 begin
 rb_define_method(module, PChar(name), func, 7);
 end;

procedure TRuby.DefineMethod(module : VALUE; const name : UTF8String;
  func : FRubyMethod8);
 begin
 rb_define_method(module, PChar(name), func, 8);
 end;

procedure TRuby.DefineMethod(module : VALUE; const name : UTF8String;
  func : FRubyMethod9);
 begin
 rb_define_method(module, PChar(name), func, 9);
 end;

procedure TRuby.DefineMethod(module : VALUE; const name : UTF8String;
  func : FRubyMethod10);
 begin
 rb_define_method(module, PChar(name), func, 10);
 end;

procedure TRuby.DefineMethod(module : VALUE; const name : UTF8String;
  func : FRubyMethod11);
 begin
 rb_define_method(module, PChar(name), func, 11);
 end;

procedure TRuby.DefineMethod(module : VALUE; const name : UTF8String;
  func : FRubyMethod12);
 begin
 rb_define_method(module, PChar(name), func, 12);
 end;

procedure TRuby.DefineMethod(module : VALUE; const name : UTF8String;
  func : FRubyMethod13);
 begin
 rb_define_method(module, PChar(name), func, 13);
 end;

procedure TRuby.DefineMethod(module : VALUE; const name : UTF8String;
  func : FRubyMethod14);
 begin
 rb_define_method(module, PChar(name), func, 14);
 end;

procedure TRuby.DefineMethod(module : VALUE; const name : UTF8String;
  func : FRubyMethod15);
 begin
 rb_define_method(module, PChar(name), func, 15);
 end;

procedure TRuby.DefineMethod(module : VALUE; const name : UTF8String;
  func : FRubyMethod16);
 begin
 rb_define_method(module, PChar(name), func, 16);
 end;

procedure TRuby.DefineMethod(module : VALUE; const name : UTF8String;
  func : FRubyMethod17);
 begin
 rb_define_method(module, PChar(name), func, 17);
 end;

procedure TRuby.DefineMethod(module : VALUE; const name : UTF8String;
  func : FRubyMethodArr);
 begin
 rb_define_method(module, PChar(name), func, -1);
 end;

procedure TRuby.DefineModuleFunction(module : VALUE; const name : UTF8String;
  func : FRubyMethod0);
 begin
 rb_define_module_function(module, PChar(name), func, 0);
 end;

procedure TRuby.DefineModuleFunction(module : VALUE; const name : UTF8String;
  func : FRubyMethod1);
 begin
 rb_define_module_function(module, PChar(name), func, 1);
 end;

procedure TRuby.DefineModuleFunction(module : VALUE; const name : UTF8String;
  func : FRubyMethod2);
 begin
 rb_define_module_function(module, PChar(name), func, 2);
 end;

procedure TRuby.DefineModuleFunction(module : VALUE; const name : UTF8String;
  func : FRubyMethod3);
 begin
 rb_define_module_function(module, PChar(name), func, 3);
 end;

procedure TRuby.DefineModuleFunction(module : VALUE; const name : UTF8String;
  func : FRubyMethod4);
 begin
 rb_define_module_function(module, PChar(name), func, 4);
 end;

procedure TRuby.DefineModuleFunction(module : VALUE; const name : UTF8String;
  func : FRubyMethod5);
 begin
 rb_define_module_function(module, PChar(name), func, 5);
 end;

procedure TRuby.DefineModuleFunction(module : VALUE; const name : UTF8String;
  func : FRubyMethod6);
 begin
 rb_define_module_function(module, PChar(name), func, 6);
 end;

procedure TRuby.DefineModuleFunction(module : VALUE; const name : UTF8String;
  func : FRubyMethod7);
 begin
 rb_define_module_function(module, PChar(name), func, 7);
 end;

procedure TRuby.DefineModuleFunction(module : VALUE; const name : UTF8String;
  func : FRubyMethod8);
 begin
 rb_define_module_function(module, PChar(name), func, 8);
 end;

procedure TRuby.DefineModuleFunction(module : VALUE; const name : UTF8String;
  func : FRubyMethod9);
 begin
 rb_define_module_function(module, PChar(name), func, 9);
 end;

procedure TRuby.DefineModuleFunction(module : VALUE; const name : UTF8String;
  func : FRubyMethod10);
 begin
 rb_define_module_function(module, PChar(name), func, 10);
 end;

procedure TRuby.DefineModuleFunction(module : VALUE; const name : UTF8String;
  func : FRubyMethod11);
 begin
 rb_define_module_function(module, PChar(name), func, 11);
 end;

procedure TRuby.DefineModuleFunction(module : VALUE; const name : UTF8String;
  func : FRubyMethod12);
 begin
 rb_define_module_function(module, PChar(name), func, 12);
 end;

procedure TRuby.DefineModuleFunction(module : VALUE; const name : UTF8String;
  func : FRubyMethod13);
 begin
 rb_define_module_function(module, PChar(name), func, 13);
 end;

procedure TRuby.DefineModuleFunction(module : VALUE; const name : UTF8String;
  func : FRubyMethod14);
 begin
 rb_define_module_function(module, PChar(name), func, 14);
 end;

procedure TRuby.DefineModuleFunction(module : VALUE; const name : UTF8String;
  func : FRubyMethod15);
 begin
 rb_define_module_function(module, PChar(name), func, 15);
 end;

procedure TRuby.DefineModuleFunction(module : VALUE; const name : UTF8String;
  func : FRubyMethod16);
 begin
 rb_define_module_function(module, PChar(name), func, 16);
 end;

procedure TRuby.DefineModuleFunction(module : VALUE; const name : UTF8String;
  func : FRubyMethod17);
 begin
 rb_define_module_function(module, PChar(name), func, 17);
 end;

procedure TRuby.DefineModuleFunction(module : VALUE; const name : UTF8String;
  func : FRubyMethodArr);
 begin
 rb_define_module_function(module, PChar(name), func, -1);
 end;

procedure TRuby.DefineSingletonMethod(obj : VALUE; const name : UTF8String;
  func : FRubyMethod0);
 begin
 rb_define_singleton_method(obj, PChar(name), func, 0);
 end;

procedure TRuby.DefineSingletonMethod(obj : VALUE; const name : UTF8String;
  func : FRubyMethod1);
 begin
 rb_define_singleton_method(obj, PChar(name), func, 1);
 end;

procedure TRuby.DefineSingletonMethod(obj : VALUE; const name : UTF8String;
  func : FRubyMethod2);
 begin
 rb_define_singleton_method(obj, PChar(name), func, 2);
 end;

procedure TRuby.DefineSingletonMethod(obj : VALUE; const name : UTF8String;
  func : FRubyMethod3);
 begin
 rb_define_singleton_method(obj, PChar(name), func, 3);
 end;

procedure TRuby.DefineSingletonMethod(obj : VALUE; const name : UTF8String;
  func : FRubyMethod4);
 begin
 rb_define_singleton_method(obj, PChar(name), func, 4);
 end;

procedure TRuby.DefineSingletonMethod(obj : VALUE; const name : UTF8String;
  func : FRubyMethod5);
 begin
 rb_define_singleton_method(obj, PChar(name), func, 5);
 end;

procedure TRuby.DefineSingletonMethod(obj : VALUE; const name : UTF8String;
  func : FRubyMethod6);
 begin
 rb_define_singleton_method(obj, PChar(name), func, 6);
 end;

procedure TRuby.DefineSingletonMethod(obj : VALUE; const name : UTF8String;
  func : FRubyMethod7);
 begin
 rb_define_singleton_method(obj, PChar(name), func, 7);
 end;

procedure TRuby.DefineSingletonMethod(obj : VALUE; const name : UTF8String;
  func : FRubyMethod8);
 begin
 rb_define_singleton_method(obj, PChar(name), func, 8);
 end;

procedure TRuby.DefineSingletonMethod(obj : VALUE; const name : UTF8String;
  func : FRubyMethod9);
 begin
 rb_define_singleton_method(obj, PChar(name), func, 9);
 end;

procedure TRuby.DefineSingletonMethod(obj : VALUE; const name : UTF8String;
  func : FRubyMethod10);
 begin
 rb_define_singleton_method(obj, PChar(name), func, 10);
 end;

procedure TRuby.DefineSingletonMethod(obj : VALUE; const name : UTF8String;
  func : FRubyMethod11);
 begin
 rb_define_singleton_method(obj, PChar(name), func, 11);
 end;

procedure TRuby.DefineSingletonMethod(obj : VALUE; const name : UTF8String;
  func : FRubyMethod12);
 begin
 rb_define_singleton_method(obj, PChar(name), func, 12);
 end;

procedure TRuby.DefineSingletonMethod(obj : VALUE; const name : UTF8String;
  func : FRubyMethod13);
 begin
 rb_define_singleton_method(obj, PChar(name), func, 13);
 end;

procedure TRuby.DefineSingletonMethod(obj : VALUE; const name : UTF8String;
  func : FRubyMethod14);
 begin
 rb_define_singleton_method(obj, PChar(name), func, 14);
 end;

procedure TRuby.DefineSingletonMethod(obj : VALUE; const name : UTF8String;
  func : FRubyMethod15);
 begin
 rb_define_singleton_method(obj, PChar(name), func, 15);
 end;

procedure TRuby.DefineSingletonMethod(obj : VALUE; const name : UTF8String;
  func : FRubyMethod16);
 begin
 rb_define_singleton_method(obj, PChar(name), func, 16);
 end;

procedure TRuby.DefineSingletonMethod(obj : VALUE; const name : UTF8String;
  func : FRubyMethod17);
 begin
 rb_define_singleton_method(obj, PChar(name), func, 17);
 end;

procedure TRuby.DefineSingletonMethod(obj : VALUE; const name : UTF8String;
  func : FRubyMethodArr);
 begin
 rb_define_singleton_method(obj, PChar(name), func, -1);
 end;

procedure TRuby.DefineGlobalFunction(const name : UTF8String;
  func : FRubyMethod0);
 begin
 rb_define_global_function(PChar(name), func, 0);
 end;

procedure TRuby.DefineGlobalFunction(const name : UTF8String;
  func : FRubyMethod1);
 begin
 rb_define_global_function(PChar(name), func, 1);
 end;

procedure TRuby.DefineGlobalFunction(const name : UTF8String;
  func : FRubyMethod2);
 begin
 rb_define_global_function(PChar(name), func, 2);
 end;

procedure TRuby.DefineGlobalFunction(const name : UTF8String;
  func : FRubyMethod3);
 begin
 rb_define_global_function(PChar(name), func, 3);
 end;

procedure TRuby.DefineGlobalFunction(const name : UTF8String;
  func : FRubyMethod4);
 begin
 rb_define_global_function(PChar(name), func, 4);
 end;

procedure TRuby.DefineGlobalFunction(const name : UTF8String;
  func : FRubyMethod5);
 begin
 rb_define_global_function(PChar(name), func, 5);
 end;

procedure TRuby.DefineGlobalFunction(const name : UTF8String;
  func : FRubyMethod6);
 begin
 rb_define_global_function(PChar(name), func, 6);
 end;

procedure TRuby.DefineGlobalFunction(const name : UTF8String;
  func : FRubyMethod7);
 begin
 rb_define_global_function(PChar(name), func, 7);
 end;

procedure TRuby.DefineGlobalFunction(const name : UTF8String;
  func : FRubyMethod8);
 begin
 rb_define_global_function(PChar(name), func, 8);
 end;

procedure TRuby.DefineGlobalFunction(const name : UTF8String;
  func : FRubyMethod9);
 begin
 rb_define_global_function(PChar(name), func, 9);
 end;

procedure TRuby.DefineGlobalFunction(const name : UTF8String;
  func : FRubyMethod10);
 begin
 rb_define_global_function(PChar(name), func, 10);
 end;

procedure TRuby.DefineGlobalFunction(const name : UTF8String;
  func : FRubyMethod11);
 begin
 rb_define_global_function(PChar(name), func, 11);
 end;

procedure TRuby.DefineGlobalFunction(const name : UTF8String;
  func : FRubyMethod12);
 begin
 rb_define_global_function(PChar(name), func, 12);
 end;

procedure TRuby.DefineGlobalFunction(const name : UTF8String;
  func : FRubyMethod13);
 begin
 rb_define_global_function(PChar(name), func, 13);
 end;

procedure TRuby.DefineGlobalFunction(const name : UTF8String;
  func : FRubyMethod14);
 begin
 rb_define_global_function(PChar(name), func, 14);
 end;

procedure TRuby.DefineGlobalFunction(const name : UTF8String;
  func : FRubyMethod15);
 begin
 rb_define_global_function(PChar(name), func, 15);
 end;

procedure TRuby.DefineGlobalFunction(const name : UTF8String;
  func : FRubyMethod16);
 begin
 rb_define_global_function(PChar(name), func, 16);
 end;

procedure TRuby.DefineGlobalFunction(const name : UTF8String;
  func : FRubyMethod17);
 begin
 rb_define_global_function(PChar(name), func, 17);
 end;

procedure TRuby.DefineGlobalFunction(const name : UTF8String;
  func : FRubyMethodArr);
 begin
 rb_define_global_function(PChar(name), func, -1);
 end;

procedure TRuby.DefineAlias(module : VALUE; const target, source : UTF8String);
 begin
 rb_define_alias(module, PChar(target), PChar(source));
 end;

procedure TRuby.DefineAttribute(module : VALUE; const name : UTF8String;
  getter : FRubyMethod0; setter : FRubyMethod1; isbool : Boolean);
 var
   at : UTF8String;
 begin
 if getter = nil
    then at := 'attr_writer'
    else if setter = nil
            then at := 'attr_reader'
            else at := 'attr_accessor';
 Send(module, at, [Str2Sym(name)]);
 if getter <> nil
    then begin
          DefineMethod(module, name, getter);
          if isbool
             then DefineAlias(module, name + '?', name);
         end;
 if setter <> nil
    then DefineMethod(module, name + '=', setter);
 end;

procedure TRuby.Include(target, source : VALUE);
 begin
 rb_include_module(target, source);
 end;

function TRuby.GlobalVarGet(const name : UTF8String) : VALUE;
 begin
 result := rb_gv_get(PChar(name));
 end;

procedure TRuby.GlobalVarSet(const name : UTF8String; value : VALUE);
 begin
 rb_gv_set(PChar(name), value);
 end;

function TRuby.BlockGiven : Boolean;
 begin
 result := rb_block_given_p() <> 0;
 end;

function TRuby.Yield(arg : VALUE) : VALUE;
 begin
 result := rb_yield(arg);
 end;

function TRuby.ArrayNew : VALUE;
 begin
 result := rb_ary_new2(0);
 end;

function TRuby.ArrayPush(arr : VALUE; value : VALUE) : VALUE;
 begin
 result := rb_ary_push(arr, value);
 end;

function TRuby.HashNew : VALUE;
 begin
 result := rb_hash_new();
 end;

function TRuby.HashGet(hash : VALUE; key : VALUE) : VALUE;
 begin
 result := rb_hash_aref(hash, key);
 end;

function TRuby.HashSet(hash : VALUE; key, value : VALUE) : VALUE;
 begin
 result := rb_hash_aset(hash, key, value);
 end;

procedure TRuby.Error(e : Exception);
 begin
 rb_raise(rb_ePascalError, msgPasErr, PChar(ansistring(e.ClassName)), PChar(e.Message));
 end;

function TRuby.Int2Val(n : PtrInt) : VALUE;
 begin
 result := rb_int2inum(n)
 end;

function TRuby.SLL2Val(const n : Int64) : VALUE;
 begin
 result := rb_ll2inum(n);
 end;

function TRuby.ULL2Val(const n : QWord) : VALUE;
 begin
 result := rb_ull2inum(n);
 end;

function TRuby.Str2Sym(const str : UTF8String) : VALUE;
 var
   id : Ruby.ID;
 begin
 id := Intern(str);
 result := (id shl RUBY_SPECIAL_SHIFT) or SYMBOL_FLAG
 end;

function TRuby.Str2Set(const str : UTF8String) : VALUE;
 begin
 result := EvalString('Set[:' + StringReplace(str, ',', ',:', [rfReplaceAll]) + ']');
 end;

function TRuby.Flt2Val(const x : Double) : VALUE;
 begin
 result := rb_float_new(x);
 end;

function TRuby.Bln2Val(b : Boolean) : VALUE;
 begin
 if b
    then result := Qtrue
    else result := Qfalse;
 end;

function TRuby.Val2Int(value : VALUE) : PtrInt;
 begin
 result := rb_num2int(value);
 end;

function TRuby.Val2SLL(value : VALUE) : Int64;
 begin
 result := rb_num2ll(value);
 end;

function TRuby.Val2ULL(value : VALUE) : QWord;
 begin
 result := rb_num2ull(value);
 end;

function TRuby.Sym2Str(value : VALUE) : UTF8String;
 begin
 result := rb_id2name(rb_to_id(value));
 end;

function TRuby.Set2Str(value : VALUE) : UTF8String;
 begin
 result := Val2Str(Call(Call(value, 'to_a', []), 'join', [Str2Val(',')]));
 end;

function TRuby.Val2Flt(value : VALUE) : Double;
 begin
 result := rb_num2dbl(value);
 end;

function TRuby.Val2Bln(value : VALUE) : Boolean;
 begin
 result := (value <> Qfalse) and (value <> Qnil)
 end;

function TRuby.IsSymbol(value : VALUE) : Boolean;
 begin
 result := (value and PtrUInt($0FF)) = SYMBOL_FLAG;
 end;

function TRuby.IsFixnum(value : VALUE) : Boolean;
 begin
 result := (value and PtrUInt(FIXNUM_FLAG)) <> 0
 end;

function TRuby.Cls2Val(cls : TClass) : VALUE;
 var
   rb_unit : VALUE;
   rb_parent : VALUE;
   l, i : Integer;
 begin
 if findClass(cls, result)
    then Exit;
 if cls.UnitName = ''
    then rb_unit := rb_mPascal
    else rb_unit := RegisterUnit(cls.UnitName);
 // todo: nested classes support
 if cls.ClassParent = nil
    then rb_parent := defaultSuperclass
    else rb_parent := Cls2Val(cls.ClassParent);
 result := DefineClass(rb_unit, constName(cls.ClassName), rb_parent);
 registerProperties(cls, result);
 l := Length(cacheClasses);
 SetLength(cacheClasses, l + 1);
 cacheClasses[l].cls := cls;
 cacheClasses[l].value := result;
 for i := 0 to High(listHooks) do
     if listHooks[i].cls = cls
        then listHooks[i].hook(self, cls, result);
 end;

function TRuby.Val2Cls(value : VALUE) : TClass;
 var
   idx : Integer;
 begin
 for idx := 0 to High(cacheClasses) do
     if cacheClasses[idx].value = value
        then begin
             result := cacheClasses[idx].cls;
             Exit;
             end;
 result := nil;
 end;

function TRuby.RegisterUnit(const name : AnsiString) : VALUE;
 var
   l : Integer;
 begin
 if findUnit(name, result)
    then Exit;
 result := DefineModule(rb_mPascal, constName(name));
 l := Length(cacheUnits);
 SetLength(cacheUnits, l + 1);
 cacheUnits[l].name := UpCase(name);
 cacheUnits[l].value := result;
 end;

procedure obj_free (p : Pointer); cdecl;
 begin
 FreeMem(p);
 end;

function TRuby.Obj2Val(obj : TObject) : VALUE;
 var
   p : PPack;
 begin
 if obj = nil
    then begin
         result := Qnil;
         Exit;
         end;
 p := GetMem(sizeof(TPack));
 p^.obj := obj;
 p^.rb := self;
 result := rb_data_object_alloc(Cls2Val(obj.ClassType), p, nil, @obj_free);
 end;

function TRuby.Val2Obj(value : VALUE) : TObject;
 begin
 if value = Qnil
    then result := nil
    else result := PPack(get_data(value))^.obj;
 end;

{ TRuby18 }

procedure TRuby18.load;
 begin
 inherited load;
 loadPtr(ruby_errinfo, 'ruby_errinfo');
 end;

procedure TRuby18.loadVals;
 begin
 inherited loadVals;
 loadValue(rb_mPrecision, 'rb_mPrecision');
 end;

procedure TRuby18.setup;
 begin
 EvalString('$-K = "UTF-8"');
 inherited setup;
 end;

procedure TRuby18.check_data(value : VALUE);
 begin
 check_type(value, T_DATA);
 end;

function TRuby18.getErrInfo : VALUE;
 begin
 result := ruby_errinfo^;
 end;

procedure TRuby18.setErrInfo(value : VALUE);
 begin
 ruby_errinfo^ := value;
 end;

class function TRuby18.defaultLibrary : UTF8String;
 begin
{$if defined(DARWIN)}
 result := '/System/Library/Frameworks/Ruby.framework/Versions/1.8/Ruby'
{$elseif defined(UNIX)}
 result := 'libruby18.so'
{$elseif defined(WINDOWS)}
 result := 'msvcrt-ruby18.dll'
{$else}
 {$error Unsupported OS!}
{$endif}
 end;

function TRuby18.ValType(value : VALUE) : Integer;
 begin
 if IsSymbol(value)
    then result := T_SYMBOL
    else if IsFixnum(value)
            then result := T_FIXNUM
            else if value = Qfalse
                    then result := T_FALSE
                    else if value = Qtrue
                            then result := T_TRUE
                            else if value = Qnil
                                    then result := T_NIL
                                    else if value = Qundef
                                            then result := T_UNDEF
{$hints off}
                                            else result := PRBasic(value)^.flags and T_MASK;
{$hints on}
 end;

function TRuby18.IsData(value : VALUE) : Boolean;
 begin
 result := ValType(value) = T_DATA;
 end;

function TRuby18.Description : UTF8String;
 begin
 result := UTF8String(PPChar(ruby_description)^) + '';
 end;

{ TRuby19 }

procedure TRuby19.load;
 begin
 inherited load;
 loadFunc(rb_errinfo,     'rb_errinfo');
 loadFunc(rb_set_errinfo, 'rb_set_errinfo');
 loadPtr(ruby_description, 'ruby_description');
 end;

procedure TRuby19.loadVals;
 begin
 inherited loadVals;
 loadValue(rb_mWaitReadable, 'rb_mWaitReadable');
 loadValue(rb_mWaitWritable, 'rb_mWaitWritable');
 loadValue(rb_cBasicObject, 'rb_cBasicObject');
 loadValue(rb_cEncoding,    'rb_cEncoding');
 loadValue(rb_cRandom,      'rb_cRandom');
 loadValue(rb_cRational,    'rb_cRational');
 loadValue(rb_cComplex,     'rb_cComplex');
 loadValue(rb_eKeyError,       'rb_eKeyError');
 loadValue(rb_eEncodingError,  'rb_eEncodingError');
 loadValue(rb_eEncCompatError, 'rb_eEncCompatError');
 end;

procedure TRuby19.setup;
 begin
 EvalString('# encoding: utf-8');
 EvalString('Encoding.default_internal = "UTF-8"');
 EvalString('Encoding.default_external = "UTF-8"');
 inherited setup;
 end;

procedure TRuby19.check_data(value : VALUE);
 begin
 check_type(value, T_DATA);
 end;

function TRuby19.getErrInfo : VALUE;
 begin
 result := rb_errinfo();
 end;

procedure TRuby19.setErrInfo(value : VALUE);
 begin
 rb_set_errinfo(value);
 end;

class function TRuby19.defaultLibrary : UTF8String;
 begin
{$if defined(DARWIN)}
 result := '/System/Library/Frameworks/Ruby.framework/Versions/1.9/Ruby'
{$elseif defined(UNIX)}
 result := 'libruby19.so'
{$elseif defined(WINDOWS)}
 result := 'msvcrt-ruby19.dll'
{$else}
 {$error Unsupported OS!}
{$endif}
 end;

function TRuby19.Str2Val(const str : UTF8String) : VALUE;
 begin
  result := rb_locale_str_new_cstr(PChar(str));
 end;

function TRuby19.ValType(value : VALUE) : Integer;
 begin
 if IsSymbol(value)
    then result := T_SYMBOL
    else if IsFixnum(value)
            then result := T_FIXNUM
            else if value = Qfalse
                    then result := T_FALSE
                    else if value = Qtrue
                            then result := T_TRUE
                            else if value = Qnil
                                    then result := T_NIL
                                    else if value = Qundef
                                            then result := T_UNDEF
{$hints off}
                                            else result := PRBasic(value)^.flags and T_MASK;
{$hints on}
 end;

function TRuby19.IsData(value : VALUE) : Boolean;
 begin
 result := ValType(value) = T_DATA;
 end;

{ TRuby20 }

class function TRuby20.defaultLibrary : UTF8String;
 begin
{$if defined(DARWIN)}
 result := '/System/Library/Frameworks/Ruby.framework/Versions/2.0/Ruby'
{$elseif defined(UNIX)}
 result := 'libruby20.so'
{$elseif defined(WINDOWS)}
 result := 'msvcrt-ruby20.dll'
{$else}
 {$error Unsupported OS!}
{$endif}
 end;

{ TEventCompanion }

function TEventCompanion.Call(const args : array of VALUE) : VALUE;
 begin
 result := ruby.Send(proc, 'call', args);
 end;

constructor TEventCompanion.Create(r : TRuby; p : VALUE);
 var
   l : Integer;
 begin
 inherited Create;
 ruby := r;
 proc := p;
 l := Length(ruby.cacheCompanions);
 SetLength(ruby.cacheCompanions, l + 1);
 ruby.cacheCompanions[l] := self;
 end;

{ TPascalOut }

function TPascalOut.getIO : VALUE;
 var
   p : PPack;
 begin
 p := GetMem(sizeof(TPack));
 p^.rb := rbOwner;
 p^.obj := self;
 result := rbOwner.rb_data_object_alloc(RbClass(rbOwner), p, nil, @obj_free);
 end;

function out_write (slf : VALUE; txt : VALUE) : VALUE; cdecl;
 var
   p : TPack;
 begin
  unpack_object(slf, p);
  try
    result := (p.obj as TPascalOut).Write(txt);
  except
    on e : Exception do
       p.rb.Error(e);
  end;
 end;

class function TPascalOut.RbClass(rb : TRuby) : VALUE;
 var
   l : Integer;
 begin
  if not rb.findOutClass(self, result)
     then begin
           result := rb.DefineClass(rb.RegisterUnit(self.UnitName), self.ClassName, rb.rb_cIO);
           rb.DefineMethod(result, 'write', @out_write);
           l := Length(rb.cacheOutClasses);
           SetLength(rb.cacheOutClasses, l + 1);
           rb.cacheOutClasses[l].cls := self;
           rb.cacheOutClasses[l].value := result;
          end;
 end;

constructor TPascalOut.Create(rb : TRuby);
 begin
  inherited Create;
  rbOwner := rb;
 end;

{ Init related }

procedure unpack_object (value : VALUE; out rec : TPack);
 begin
{$hints off}
 rec := PPack(PRData(value)^.data)^;
{$hints on}
 end;

function A2A (c : Integer) : AnsiString; inline;
 begin
 result := AnsiString(AnsiChar(c));
 end;

function U2U (c : Integer) : UnicodeString; inline;
 begin
 result := UnicodeString(UnicodeChar(c));
 end;

function pascal_get_prop (obj : VALUE; prop : VALUE) : VALUE; cdecl;
 var
   p : TPack;
   n : UTF8String;
   i : PPropInfo;
 begin
 unpack_object(obj, p);
 try
   n := p.rb.Val2Str(prop);
   i := GetPropInfo(p.obj, n);
   if i <> nil
      then case i^.PropType^.Kind of
              tkInteger :
                result := p.rb.Int2Val(GetOrdProp(p.obj, i));
              tkInt64 :
                result := p.rb.SLL2Val(GetInt64Prop(p.obj, i));
              tkQWord :
                result := p.rb.ULL2Val(QWord(GetInt64Prop(p.obj, i)));
              tkEnumeration :
                result := p.rb.Str2Sym(GetEnumProp(p.obj, i));
              tkSet :
                result := p.rb.Str2Set(GetSetProp(p.obj, i, false));
              tkFloat :
                result := p.rb.Flt2Val(GetFloatProp(p.obj, i));
              tkSString, tkLString, tkAString :
                result := p.rb.Str2Val(GetStrProp(p.obj, i));
              tkWString, tkUString :
                result := p.rb.Str2Val(GetUnicodeStrProp(p.obj, i));
              tkChar :
                result := p.rb.Str2Val(A2A(GetOrdProp(p.obj, i)));
              tkWChar, tkUChar :
                result := p.rb.Str2Val(U2U(GetOrdProp(p.obj, i)));
              tkBool :
                result := p.rb.Bln2Val(GetOrdProp(p.obj, i) <> 0);
              tkClass :
                result := p.rb.Obj2Val(GetObjectProp(p.obj, i));
              else
                result := p.rb.Qnil;
           end
      else result := p.rb.Qnil;
  except
    on e : Exception do
       p.rb.Error(e);
  end;
 end;

function A1 (const s : ansistring) : Integer; inline;
 begin
 result := Ord(AnsiChar(s[1]));
 end;

function U1 (const s : UnicodeString) : Integer; inline;
 begin
 result := Ord(UnicodeChar(s[1]));
 end;

function pascal_set_prop (obj : VALUE; prop, value : VALUE) : VALUE; cdecl;
 var
   p : TPack;
   n : UTF8String;
   i : PPropInfo;
 begin
 unpack_object(obj, p);
 try
   n := p.rb.Val2Str(prop);
   i := GetPropInfo(p.obj, n);
   if i <> nil
      then case i^.PropType^.Kind of
              tkInteger :
                SetOrdProp(p.obj, i, p.rb.Val2Int(value));
              tkInt64 :
                SetInt64Prop(p.obj, i, p.rb.Val2SLL(value));
              tkQWord :
                SetInt64Prop(p.obj, i,
                             Int64(p.rb.Val2ULL(value)));
              tkEnumeration :
                SetEnumProp(p.obj, i, p.rb.Sym2Str(value));
              tkSet :
                SetSetProp(p.obj, i, p.rb.Set2Str(value));
              tkFloat :
                SetFloatProp(p.obj, i, p.rb.Val2Flt(value));
              tkSString, tkLString, tkAString :
                SetStrProp(p.obj, i, p.rb.Val2Str(value));
              tkWString, tkUString :
                SetUnicodeStrProp(p.obj, i,
                                  p.rb.Val2Str(value));
              tkChar :
                SetOrdProp(p.obj, i,
                           A1(p.rb.Val2Str(value)));
              tkUChar, tkWChar :
                SetOrdProp(p.obj, i,
                           U1(p.rb.Val2Str(value)));
              tkBool :
                SetOrdProp(p.obj, i, Ord(p.rb.Val2Bln(value)));
              tkClass :
                SetObjectProp(p.obj, i, p.rb.Val2Obj(value));
           end;
   result := value;
 except
   on e : Exception do
      p.rb.Error(e);
 end;
 end;

function object_equals (obj : VALUE; other : VALUE) : VALUE; cdecl;
 var
   p, op : TPack;
 begin
 unpack_object(obj, p);
 try
   if not p.rb.IsData(other)
      then result := p.rb.Qfalse
      else begin
            unpack_object(other, op);
            result := p.rb.Bln2Val(p.obj.Equals(op.obj));
           end;
 except
   on e : Exception do
      p.rb.Error(e);
 end;
 end;

function pascal_event (argc : cint; argv : PVALUE; slf : VALUE) : VALUE; cdecl;
 var
   p : TPack;
   nm, tp, blk : VALUE;
   m : TMethod;
   c : TEventCompanion;
   cc : TEventCompanionClass;
 begin
  unpack_object(slf, p);
  try
    p.rb.rb_scan_args(argc, argv, '2&', @nm, @tp, @blk);
    if blk = p.rb.Qnil
       then begin
            m.Code := nil;
            m.Data := nil;
            SetMethodProp(p.obj, p.rb.Val2Str(nm), m);
            end
       else begin
            cc := p.rb.GetCompanionClass(p.rb.Val2Str(tp));
            if cc <> nil
               then begin
                    c := cc.Create(p.rb, blk);
                    m := c.GetHandler;
                    SetMethodProp(p.obj, p.rb.Val2Str(nm), m);
                    end;
            end;
    result := blk;
  except
    on e : Exception do
       p.rb.Error(e);
  end;
 end;

function object_new (slf : VALUE) : VALUE; cdecl;
 var
   rb : TRuby;
 begin
  rb := TRuby.EngineByModule(slf);
  rb.rb_notimplement();
 end;

{$hints off}
procedure HookTObject (ruby : TRuby; cls : TClass; value : VALUE);
 begin
 ruby.DefineMethod(value, 'pascal_get_prop', @pascal_get_prop);
 ruby.DefineMethod(value, 'pascal_set_prop', @pascal_set_prop);
 ruby.DefineMethod(value, 'pascal_event',    @pascal_event);
 ruby.Send(value, 'private', [ruby.Str2Sym('pascal_get_prop'),
                              ruby.Str2Sym('pascal_set_prop'),
                              ruby.Str2Sym('pascal_event')]);
 ruby.DefineMethod(value, '==', @object_equals);
 ruby.DefineSingletonMethod(value, 'new', @object_new);
 end;
{$hints on}

initialization
 TRuby.AddRegisterClassHook(TObject, @hookTObject);
end.

