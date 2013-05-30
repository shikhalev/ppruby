{$mode ObjFPC}

unit RubyEngine;

{$ifdef DARWIN}
  {$linkframework Ruby}
{$endif}

interface

uses
  SysUtils, DynLibs;

type
  PVALUE = ^VALUE;
  VALUE = record
    value : PtrUInt
  end;

  PID = ^ID;
  ID = record
    value : PtrUInt;
  end;

type
  TVersion = record
    major, minor : Integer
  end;

type

  { TRubyEngine }

  TRubyEngine = class(TObject)
  protected
    fldLib : THandle;
    ruby_init : procedure (); cdecl;
    ruby_init_loadpath : procedure (); cdecl;
    ruby_script : procedure (script : PChar); cdecl;
    ruby_finalize : procedure (); cdecl;
    rb_eval_string_protect : function (Str : PChar; out Err : Integer) : VALUE; cdecl;
  protected
    class function Version : TVersion; virtual; abstract;
    class function LibName : UnicodeString; virtual;
    procedure SetupUTF8; virtual; abstract;
  public
    function Execute (const Str : UnicodeString) : VALUE;
  public
    constructor Create (const Lib : UnicodeString; const Scr : UnicodeString = ''); virtual;
    constructor Create (const Scr : UnicodeString = '');
    destructor Destroy; override;
  end;

  { TRuby18 }

  TRuby18 = class(TRubyEngine)
  protected
    class function Version: TVersion; override;
    procedure SetupUTF8; override;
  end;

  { TRuby19 }

  TRuby19 = class(TRuby18)
  protected
    class function Version: TVersion; override;
    procedure SetupUTF8; override;
  end;

type
  ERubyError = class(Exception);

  ERubyLibraryNotFound = class(ERubyError);
  ERubyInitError = class(ERubyError);
  ERubyExecError = class(ERubyError);

const
  msgRubyLibraryNotFound = 'Ruby Library "%s" not found (or is not a library).';
  msgRubyInitError = 'Error #%d while initialization (%s).';
  msgRubyExecError = 'Error #%d while execution.';

implementation

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

{ TRubyEngine }

class function TRubyEngine.LibName : UnicodeString;
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

function TRubyEngine.Execute (const Str : UnicodeString) : VALUE;
 var
   res : Integer;
 begin
 result := rb_eval_string_protect(PChar(Str), res);
 if res <> 0
    then raise ERubyExecError.CreateFmt(msgRubyExecError, [res]);
 end;

constructor TRubyEngine.Create (const Lib : UnicodeString; const Scr : UnicodeString = '');
 begin
 inherited Create;
 fldLib := LoadLibrary(Lib);
 if fldLib = 0
    then raise ERubyLibraryNotFound.CreateFmt(msgRubyLibraryNotFound, [Lib]);
 Pointer(ruby_init) := GetProcedureAddress(fldLib, 'ruby_init');
 Pointer(ruby_init_loadpath) := GetProcedureAddress(fldLib, 'ruby_init_loadpath');
 Pointer(ruby_script) := GetProcedureAddress(fldLib, 'ruby_script');
 Pointer(rb_eval_string_protect) := GetProcedureAddress(fldLib, 'rb_eval_string_protect');
 Pointer(ruby_finalize) := GetProcedureAddress(fldLib, 'ruby_finalize');
 ruby_init();
 ruby_init_loadpath();
 if Scr = ''
    then ruby_script(PChar(ParamStr(0)))
    else ruby_script(PChar(Scr));
 SetupUTF8;
 end;

constructor TRubyEngine.Create (const Scr : UnicodeString = '');
 begin
 Create(LibName, Scr)
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

end.

