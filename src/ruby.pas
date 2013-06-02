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

  TObjectPack = record
    obj : TObject;
    ruby : TRuby;
  end;
  PObjectPack = ^TObjectPack;

  { TRuby }

  TRuby = class
  private
    class var
      listHooks : array of record
          cls : TClass;
          hook : FRegisterClassHook;
        end;
    class function findHook (cls : TClass; hook : FRegisterClassHook; out idx : Integer) : Boolean;
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
      cacheObjects : array of record
          obj : TObjectPack;
          value : VALUE;
        end;
    function findObject (obj : TObject; out value : VALUE) : Boolean;
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
    rb_str_new2 : function (str : PChar) : VALUE; cdecl;
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
    rb_mPascal : VALUE;
    // methods
    procedure loadFunc (out field; const name : UTF8String);
    procedure loadPtr (out field; const name : UTF8String);
    procedure loadValue (out field; const name : UTF8String);
    procedure load; virtual;
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
    // constants
    const
      Qfalse = VALUE(0);
      Qtrue  = VALUE(2);
      Qnil   = VALUE(4);
      Qundef = VALUE(6);
      RUBY_SPECIAL_SHIFT = 8;
      SYMBOL_FLAG = $0E;
    // class methods
    class function Auto : TRuby;
    class function Auto (const vers : array of TRubyClass) : TRuby;
    class procedure AddRegisterClassHook (cls : TClass; hook : FRegisterClassHook);
    class procedure DelRegisterClassHook (cls : TClass; hook : FRegisterClassHook);
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
    function StringValue2String (value : VALUE) : UTF8String;
    function StrNew (const str : UTF8String) : VALUE;
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
    procedure Include (target, source : VALUE);
    // methods (coversions)
    function Int2Val (n : PtrInt) : VALUE;
    function QInt2Val (const n : Int64) : VALUE;
    function QWord2Val (const n : QWord) : VALUE;
    function Str2SymVal (const str : UTF8String) : VALUE;
    function SetStr2Val (const str : UTF8String) : VALUE;
    function Float2Val (const x : Double) : VALUE;
    function Bool2Val (b : Boolean) : VALUE;
    function Val2Int (value : VALUE) : PtrInt;
    function Val2QInt (value : VALUE) : Int64;
    function Val2QWord (value : VALUE) : QWord;
    function SymVal2Str (value : VALUE) : UTF8String;
    function Val2SetStr (value : VALUE) : UTF8String;
    function Val2Float (value : VALUE) : Double;
    function Val2Bool (value : VALUE) : Boolean;
    // properties (self)
    // methods (self)
    function RegisterClass (cls : TClass) : VALUE;
    function RegisterUnit (const name : AnsiString) : VALUE;
    function WrapObject (obj : TObject) : VALUE;
    function GetObject (value : VALUE) : TObject;
  end;

  { TRuby18 }

  TRuby18 = class(TRuby)
  protected
    const
      T_DATA = $22;
  protected
    // fields: error
    ruby_errinfo : PVALUE;
    // fields: ruby objects
    rb_mPrecision : VALUE;
    // overrided methods
    procedure load; override;
    procedure setup; override;
    procedure check_data(value : VALUE); override;
    // property access
    function getErrInfo : VALUE; override;
    procedure setErrInfo(value : VALUE); override;
    // class methods
    class function defaultLibrary : UTF8String; override;
  public
    // properties (wrappers)
    property mPrecision : VALUE read rb_mPrecision;
    function Description : UTF8String; override;
  end;

  { TRuby19 }

  TRuby19 = class(TRuby)
  protected
    const
      T_DATA = $0C;
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
    procedure setup; override;
    procedure check_data(value : VALUE); override;
    // property access
    function getErrInfo : VALUE; override;
    procedure setErrInfo(value : VALUE); override;
    // class methods
    class function defaultLibrary : UTF8String; override;
  public
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

{ Common }

procedure unpack_object (value : VALUE; out rec : TObjectPack);

type
  ERuby = class(Exception);

  ENoRuby = class(ERuby);

  { ERubyEval }

  ERubyEval = class(Exception)
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

function TRuby.findObject(obj : TObject; out value : VALUE) : Boolean;
 var
   i : Integer;
 begin
 for i := 0 to High(cacheObjects) do
     if cacheObjects[i].obj.obj = obj
        then begin
             value := cacheObjects[i].value;
             result := true;
             Exit;
             end;
 result := false;
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
 begin
 VALUE(field) := PVALUE(GetProcedureAddress(libHandle, name))^;
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
// loadValue(rb_cCont,          'rb_cCont');
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
 // io
 loadPtr(rb_stdin,  'rb_stdin');
 loadPtr(rb_stdout, 'rb_stdout');
 loadPtr(rb_stderr, 'rb_stderr');
 // other
 loadPtr(ruby_description, 'ruby_description');
 end;

procedure TRuby.init(const script : UTF8String);
 begin
 ruby_init;
 ruby_init_loadpath;
 ruby_script(PChar(script));
 end;

procedure TRuby.setup;
 begin
 // namespace
 rb_require('set');
 rb_mPascal := DefineModule('Pascal');
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
  // todo: events support
  // RubifiedEvents = [tkMethod];

procedure TRuby.registerProperties (cls : TClass; value : VALUE);
 var
   data : PTypeData;
   list : PPropList;
   prop : PPropInfo;
   i : Integer;
   nm : AnsiString;
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
                     '  pascal_get_prop("' + prop^.Name + '")' + LineEnding +
                     'end' + LineEnding +
                     'def ' + nm + '=(value)' + LineEnding +
                     '  pascal_set_prop("' + prop^.Name + '", value)' + LineEnding +
                     'end';
             Send(value, 'class_eval', [StrNew(code)]);
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
         raise ERubyType.Create(err, StringValue2String(Inspect(err)));
         end;
 end;

type
  RBasic = record
    flags : VALUE;
    klass : VALUE;
  end;

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

constructor TRuby.Create(const lib, script : UTF8String);
 begin
 inherited Create;
 libHandle := LoadLibrary(lib);
 if libHandle = 0
    then raise ENoRuby.CreateFmt(msgNoRuby, [lib]);
 load;
 init(script);
 setup;
 end;

destructor TRuby.Destroy;
 begin
 done;
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
 result := rb_eval_string_protect(PChar(str), res);
 if res <> 0
    then begin
         err := getErrInfo;
         raise ERubyEval.Create(err, StringValue2String(Inspect(err)));
         end;
 end;

function TRuby.DefineModule(const name : UTF8String) : VALUE;
 begin
 result := rb_define_module(PChar(name));
 end;

function TRuby.DefineModule(ns : VALUE; const name : UTF8String) : VALUE;
 begin
 result := rb_define_module_under(ns, PChar(name));
 end;

function TRuby.DefineClass(const name : UTF8String; super : VALUE) : VALUE;
 begin
 result := rb_define_class(PChar(name), super);
 end;

function TRuby.DefineClass(ns : VALUE; const name : UTF8String;
  super : VALUE) : VALUE;
 begin
 result := rb_define_class_under(ns, PChar(name), super);
 end;

function TRuby.DefineClass(const name : UTF8String) : VALUE;
 begin
 result := rb_define_class(PChar(name), rb_cObject);
 end;

function TRuby.DefineClass(ns : VALUE; const name : UTF8String) : VALUE;
 begin
 result := rb_define_class_under(ns, PChar(name), rb_cObject);
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

function TRuby.StringValue2String(value : VALUE) : UTF8String;
 begin
 result := UTF8String(rb_string_value_cstr(value)) + '';
 end;

function TRuby.StrNew(const str : UTF8String) : VALUE;
 begin
 result := rb_str_new2(PChar(str));
 end;

function TRuby.Call (obj : VALUE; method : ID;
  const args : array of VALUE) : VALUE;
 begin
 result := rb_funcall3(obj, method, Length(args), @(args[0]));
 end;

function TRuby.Call(obj : VALUE; name : UTF8String;
  const args : array of VALUE) : VALUE;
 begin
 result := Call(obj, Intern(name), args);
 end;

function TRuby.Send(obj : VALUE; method : ID;
  const args : array of VALUE) : VALUE;
 begin
 result := rb_funcall2(obj, method, Length(args), @(args[0]));
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

procedure TRuby.Include(target, source : VALUE);
 begin
 rb_include_module(target, source);
 end;

function TRuby.Int2Val(n : PtrInt) : VALUE;
 begin
 result := rb_int2inum(n)
 end;

function TRuby.QInt2Val(const n : Int64) : VALUE;
 begin
 result := rb_ll2inum(n);
 end;

function TRuby.QWord2Val(const n : QWord) : VALUE;
 begin
 result := rb_ull2inum(n);
 end;

function TRuby.Str2SymVal(const str : UTF8String) : VALUE;
 var
   id : Ruby.ID;
 begin
 id := Intern(str);
 result := (id shl RUBY_SPECIAL_SHIFT) or SYMBOL_FLAG
 end;

function TRuby.SetStr2Val(const str : UTF8String) : VALUE;
 begin
 result := EvalString('Set[:' + StringReplace(str, ',', ',:', [rfReplaceAll]) + ']');
 end;

function TRuby.Float2Val(const x : Double) : VALUE;
 begin
 result := rb_float_new(x);
 end;

function TRuby.Bool2Val(b : Boolean) : VALUE;
 begin
 if b
    then result := Qtrue
    else result := Qfalse;
 end;

function TRuby.Val2Int(value : VALUE) : PtrInt;
 begin
 result := rb_num2int(value);
 end;

function TRuby.Val2QInt(value : VALUE) : Int64;
 begin
 result := rb_num2ll(value);
 end;

function TRuby.Val2QWord(value : VALUE) : QWord;
 begin
 result := rb_num2ull(value);
 end;

function TRuby.SymVal2Str(value : VALUE) : UTF8String;
 begin
 result := rb_id2name(rb_to_id(value));
 end;

function TRuby.Val2SetStr(value : VALUE) : UTF8String;
 begin
 result := StringValue2String(Call(Call(value, 'to_a', []), 'join', [StrNew(',')]));
 end;

function TRuby.Val2Float(value : VALUE) : Double;
 begin
 result := rb_num2dbl(value);
 end;

function TRuby.Val2Bool(value : VALUE) : Boolean;
 begin
 result := (value <> Qfalse) and (value <> Qnil)
 end;

function TRuby.RegisterClass(cls : TClass) : VALUE;
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
    else rb_parent := RegisterClass(cls.ClassParent);
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

function TRuby.WrapObject(obj : TObject) : VALUE;
 var
   l : Integer;
 begin
 if obj = nil
    then begin
         result := Qnil;
         Exit;
         end;
 if not findObject(obj, result)
    then begin
         l := Length(cacheObjects);
         SetLength(cacheObjects, l + 1);
         cacheObjects[l].obj.obj := obj;
         cacheObjects[l].obj.ruby := self;
         result := rb_data_object_alloc(RegisterClass(obj.ClassType), @cacheObjects[l].obj, nil, nil);
         cacheObjects[l].value := result;
         end;
 end;

function TRuby.GetObject(value : VALUE) : TObject;
 begin
 if value = Qnil
    then result := nil
    else result := PObjectPack(get_data(value))^.obj;
 end;

{ TRuby18 }

procedure TRuby18.load;
 begin
 inherited load;
 loadPtr(ruby_errinfo, 'ruby_errinfo');
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
 loadPtr(ruby_description, 'ruby_description');
 end;

procedure TRuby19.setup;
 begin
 EvalString('Encoding.default_internal = "UTF-8"');
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

{ Init related }

procedure unpack_object (value : VALUE; out rec : TObjectPack);
 begin
{$hints off}
 rec := PObjectPack(PRData(value)^.data)^;
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
   pack : TObjectPack;
   name : UTF8String;
   info : PPropInfo;
 begin
 unpack_object(obj, pack);
 name := pack.ruby.StringValue2String(prop);
 info := GetPropInfo(pack.obj, name);
 if info <> nil
    then case info^.PropType^.Kind of
              tkInteger :
                result := pack.ruby.Int2Val(GetOrdProp(pack.obj, info));
              tkInt64 :
                result := pack.ruby.QInt2Val(GetInt64Prop(pack.obj, info));
              tkQWord :
                result := pack.ruby.QWord2Val(QWord(GetInt64Prop(pack.obj,
                                                                 info)));
              tkEnumeration :
                result := pack.ruby.Str2SymVal(GetEnumProp(pack.obj, info));
              tkSet :
                result := pack.ruby.SetStr2Val(GetSetProp(pack.obj, info,
                                                          false));
              tkFloat :
                result := pack.ruby.Float2Val(GetFloatProp(pack.obj, info));
              tkSString, tkLString, tkAString :
                result := pack.ruby.StrNew(GetStrProp(pack.obj, info));
              tkWString, tkUString :
                result := pack.ruby.StrNew(GetUnicodeStrProp(pack.obj, info));
              tkChar :
                result := pack.ruby.StrNew(A2A(GetOrdProp(pack.obj, info)));
              tkWChar, tkUChar :
                result := pack.ruby.StrNew(U2U(GetOrdProp(pack.obj, info)));
              tkBool :
                result := pack.ruby.Bool2Val(GetOrdProp(pack.obj, info) <> 0);
              tkClass :
                result := pack.ruby.WrapObject(GetObjectProp(pack.obj, info));
              else
                result := pack.ruby.Qnil;
         end
    else result := pack.ruby.Qnil;
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
   pack : TObjectPack;
   name : UTF8String;
   info : PPropInfo;
 begin
 unpack_object(obj, pack);
 name := pack.ruby.StringValue2String(prop);
 info := GetPropInfo(pack.obj, name);
 if info <> nil
    then case info^.PropType^.Kind of
              tkInteger :
                SetOrdProp(pack.obj, info, pack.ruby.Val2Int(value));
              tkInt64 :
                SetInt64Prop(pack.obj, info, pack.ruby.Val2QInt(value));
              tkQWord :
                SetInt64Prop(pack.obj, info,
                             Int64(pack.ruby.Val2QWord(value)));
              tkEnumeration :
                SetEnumProp(pack.obj, info, pack.ruby.SymVal2Str(value));
              tkSet :
                SetSetProp(pack.obj, info, pack.ruby.Val2SetStr(value));
              tkFloat :
                SetFloatProp(pack.obj, info, pack.ruby.Val2Float(value));
              tkSString, tkLString, tkAString :
                SetStrProp(pack.obj, info, pack.ruby.StringValue2String(value));
              tkWString, tkUString :
                SetUnicodeStrProp(pack.obj, info,
                                  pack.ruby.StringValue2String(value));
              tkChar :
                SetOrdProp(pack.obj, info,
                           A1(pack.ruby.StringValue2String(value)));
              tkUChar, tkWChar :
                SetOrdProp(pack.obj, info,
                           U1(pack.ruby.StringValue2String(value)));
              tkBool :
                SetOrdProp(pack.obj, info, Ord(pack.ruby.Val2Bool(value)));
              tkClass :
                SetObjectProp(pack.obj, info, pack.ruby.GetObject(value));
         end;
 result := value;
 end;

function object_equals (obj : VALUE; other : VALUE) : VALUE; cdecl;
 var
   pack, otherpack : TObjectPack;
 begin
 unpack_object(obj, pack);
 unpack_object(other, otherpack);
 result := pack.ruby.Bool2Val(pack.obj.Equals(otherpack.obj));
 end;

function object_to_s (obj : VALUE) : VALUE; cdecl;
 var
   pack : TObjectPack;
 begin
 unpack_object(obj, pack);
 result := pack.ruby.StrNew('#<Pascal: #' + HexStr(Pointer(pack.obj)) + ' [' +
                            pack.obj.ClassName + ']>');
 end;

{$hints off}
procedure HookTObject (ruby : TRuby; cls : TClass; value : VALUE);
 begin
 ruby.DefineMethod(value, 'pascal_get_prop', @pascal_get_prop);
 ruby.DefineMethod(value, 'pascal_set_prop', @pascal_set_prop);
 ruby.Send(value, 'private', [ruby.Str2SymVal('pascal_get_prop'),
                              ruby.Str2SymVal('pascal_set_prop')]);
 ruby.DefineMethod(value, '==', @object_equals);
 ruby.DefineMethod(value, 'to_s', @object_to_s);
 end;
{$hints on}

initialization
 TRuby.AddRegisterClassHook(TObject, @hookTObject);
end.

