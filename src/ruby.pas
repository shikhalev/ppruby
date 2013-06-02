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

  TRubyClass = class of TRuby;

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
    rb_define_module_under : function (ns : VALUE; name : PChar) : VALUE; cdecl;
    rb_define_class : function (name : PChar; super : VALUE) : VALUE; cdecl;
    rb_define_class_under : function (ns : VALUE; name : PChar; super : VALUE) : VALUE; cdecl;
    rb_define_const : procedure (ns : VALUE; name : PChar; value : VALUE); cdecl;
    rb_define_global_const : procedure (name : PChar; value : VALUE); cdecl;
    rb_eval_string_protect : function (str : PChar; out res : cint) : VALUE; cdecl;
    rb_string_value_cstr : function (constref v : VALUE) : PChar; cdecl;
    rb_inspect : function (value : VALUE) : VALUE; cdecl;
    rb_funcall2, rb_funcall3 : function (obj : VALUE; method : ID; argc : cint; argv : PVALUE) : VALUE; cdecl;
    rb_intern : function (name : PChar) : ID; cdecl;
    rb_str_new2 : function (str : PChar) : VALUE; cdecl;                                            // fields: ruby objects
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
  public
    // constants
    const
      Qfalse = VALUE(0);
      Qtrue  = VALUE(2);
      Qnil   = VALUE(4);
      Qundef = VALUE(6);
    // class methods
    class function Auto : TRuby;
    class function Auto (const vers : array of TRubyClass) : TRuby;
    // constructor & destructor
    constructor Create (const lib, script : UTF8String); virtual;
    destructor Destroy; override;
    // properties (wrappers)
    property ErrInfo : VALUE read getErrInfo write setErrInfo;
    property StdIn : VALUE read getStdIn write setStdIn;
    property StdOut : VALUE read getStdOut write setStdOut;
    property StdErr : VALUE read getStdErr write setStdErr;
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
    // properties (self)
    // methods (self)
  end;

  { TRuby18 }

  TRuby18 = class(TRuby)
  protected
    // fields: error
    ruby_errinfo : PVALUE;
    // fields: ruby objects
    rb_mPrecision : VALUE;
    // overrided methods
    procedure load; override;
    procedure setup; override;
    // property access
    function getErrInfo : VALUE; override;
    procedure setErrInfo(value : VALUE); override;
    // class methods
    class function defaultLibrary : UTF8String; override;
  public
    // properties (wrappers)
    property mPrecision : VALUE read rb_mPrecision;
  end;

  { TRuby19 }

  TRuby19 = class(TRuby)
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

  { ERubyEval }

  ERubyEval = class(Exception)
  private
    fldErrInfo : VALUE;
  public
    property ErrInfo : VALUE read fldErrInfo;
    constructor Create (info : VALUE; const msg : UTF8String);
  end;

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
 loadFunc(rb_define_module,       'rb_define_module');
 loadFunc(rb_define_module_under, 'rb_define_module_under');
 loadFunc(rb_define_class,        'rb_define_class');
 loadFunc(rb_define_class_under,  'rb_define_class_under');
 loadFunc(rb_define_const,        'rb_define_const');
 loadFunc(rb_define_global_const, 'rb_define_global_const');
 loadFunc(rb_eval_string_protect, 'rb_eval_string_protect');
 loadFunc(rb_string_value_cstr,   'rb_string_value_cstr');
 loadFunc(rb_inspect,             'rb_inspect');
 loadFunc(rb_funcall2,            'rb_funcall2');
 loadFunc(rb_funcall3,            'rb_funcall3');
 loadFunc(rb_intern,              'rb_intern');
 loadFunc(rb_str_new2,            'rb_str_new2');
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
 loadValue(rb_cCont,          'rb_cCont');
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
 rb_mPascal := DefineModule('Pascal');
 end;

procedure TRuby.done;
 begin
 ruby_finalize;
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
 end;

procedure TRuby19.setup;
 begin
 EvalString('Encoding.default_internal = "UTF-8"');
 inherited setup;
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

(*

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

 class function TRubyEngine.Qfalse : VALUE;
 begin
 result := VALUE(0)
 end;

class function TRubyEngine.Qtrue : VALUE;
 begin
 result := VALUE(2)
 end;

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

