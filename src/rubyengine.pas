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
  TRubyClass = class of TRubyEngine;

  { TRubyEngine }

  TRubyEngine = class(TObject)
  protected
    fldLib : THandle;
    p_ruby_errinfo : PVALUE;
    ruby_init : procedure (); cdecl;
    ruby_init_loadpath : procedure (); cdecl;
    ruby_script : procedure (script : PChar); cdecl;
    ruby_finalize : procedure (); cdecl;
    rb_eval_string_protect : function (Str : PChar; out Err : Integer) : VALUE;
                                      cdecl;
    rb_string_value_cstr : function (constref v : VALUE) : PChar; cdecl;
  protected
    class function Version : TVersion; virtual; abstract;
    class function DefaultScript : UTF8String; virtual;
    procedure SetupUTF8; virtual; abstract;
  public
    class function DefaultLibrary : UTF8String; virtual;
    class function Detect (
                   const Vers : array of TRubyClass;
                   const Scr : UTF8String = ''
                   ) : TRubyEngine;
    function Execute (const Str : UTF8String) : VALUE; virtual;
    function ErrInfo : VALUE; virtual;
    function ValueToString (v : VALUE) : UTF8String; virtual;
  public
    constructor Create (const Lib : UTF8String; const Scr : UTF8String = '');
                                      virtual;
    constructor Create;
    destructor Destroy; override;
  end;

  { TRuby18 }

  TRuby18 = class(TRubyEngine)
  protected
    class function Version : TVersion; override;
    procedure SetupUTF8; override;
  end;

  { TRuby19 }

  TRuby19 = class(TRuby18)
  protected
    class function Version : TVersion; override;
    procedure SetupUTF8; override;
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
  ERubyExecError = class(ERubyError);

const
  msgRubyLibraryNotFound = 'Ruby Library "%s" not found (or is not a library).';
  msgRubyInitError = 'Error #%d while initialization (%s).';
  msgRubyExecError = 'Error #%d while execution.';

implementation

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

class function TRubyEngine.Detect (
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
    then raise ERubyExecError.Create(Format(msgRubyExecError, [res]) +
                 LineEnding + LineEnding + ValueToString(p_ruby_errinfo^));
 end;

function TRubyEngine.ErrInfo : VALUE;
 begin
 result := p_ruby_errinfo^;
 end;

function TRubyEngine.ValueToString(v : VALUE) : UTF8String;
 begin
 result := UTF8String(rb_string_value_cstr(v)) + ''
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
 Pointer(p_ruby_errinfo) := GetProcedureAddress(fldLib, 'ruby_errinfo');
 Pointer(ruby_init) := GetProcedureAddress(fldLib, 'ruby_init');
 Pointer(ruby_init_loadpath) := GetProcedureAddress(fldLib,
                                  'ruby_init_loadpath');
 Pointer(ruby_script) := GetProcedureAddress(fldLib, 'ruby_script');
 Pointer(rb_eval_string_protect) := GetProcedureAddress(fldLib,
                                      'rb_eval_string_protect');
 Pointer(ruby_finalize) := GetProcedureAddress(fldLib, 'ruby_finalize');
 Pointer(rb_string_value_cstr) := GetProcedureAddress(fldLib,
                                    'rb_string_value_cstr');
 ruby_init();
 ruby_init_loadpath();
 if Scr = ''
    then ruby_script(PChar(DefaultScript))
    else ruby_script(PChar(Scr));
 SetupUTF8;
 end;

constructor TRubyEngine.Create;
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

end.

