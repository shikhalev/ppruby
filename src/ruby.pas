{$include rbopts.inc}

{$include rbdefs.inc}

unit Ruby;

{$if defined(DARWIN)}
 {$linkframework Ruby}
{$endif}

interface

uses
  ctypes, SysUtils;

type
  VALUE = packed record value : PtrUInt end;
  PVALUE = ^VALUE;
  PPVALUE = ^PVALUE;
  ID = packed record id : PtrUInt end;
  PID = ^ID;

{$include rbtypes.inc}

{$if defined(RUBY_DYNAMIC)}
 {$include rbdynintf.inc}
{$elseif defined(RUBY_STATIC)}
 {$include rbstatintf.inc}
{$endif}

{$include rbmacrointf.inc}

type
  pp_init_hook = procedure;
  pp_done_hook = procedure;

procedure pp_reg_init_hook (hook : pp_init_hook);
procedure pp_reg_done_hook (hook : pp_done_hook);
procedure pp_ruby_init (const version : array of cint);
procedure pp_ruby_init;
procedure pp_ruby_done (check_errinfo : Boolean = True);
function pp_ruby_active : Boolean; inline;

{$if defined(RUBY_DYNAMIC)}
type
  RbInfo = record
    check_flonum : Boolean;
  end;
  pp_load_hook = procedure (lib : THandle);

const
  Rb19 : RbInfo = ( check_flonum : False );
  Rb20 : RbInfo = ( check_flonum : True  );

const
  V_ANY = -1;

procedure pp_reg_ruby_version (const version : array of cint;
                               const info : RbInfo;
                               const libs : array of UnicodeString);
{$endif}

type
  ERuby = class(Exception);

  ERubyLibraryError = class(ERuby);
  ERubyException = class(ERuby)
  private
    fld_errinfo : VALUE;
  public
    property errinfo : VALUE read fld_errinfo;
    constructor Create (err : VALUE);
  end;

procedure pp_check_active; inline;
procedure pp_check_result (res : cint); inline;

implementation

{$if defined(RUBY_DYNAMIC)}
 {$include rbdynimpl.inc}
{$endif}

{$include rbmacroimpl.inc}

var
  init_hooks : array of pp_init_hook = nil;
  done_hooks : array of pp_done_hook = nil;

procedure pp_reg_init_hook (hook : pp_init_hook);
 var
   l : Integer;
 begin
  l := Length(init_hooks);
  SetLength(init_hooks, l + 1);
  init_hooks[l] := hook;
 end;

procedure pp_reg_done_hook (hook : pp_done_hook);
 var
   l : Integer;
 begin
  l := Length(done_hooks);
  SetLength(done_hooks, l + 1);
  done_hooks[l] := hook;
 end;

resourcestring
  msgAlreadyActive = 'Ruby library is already active!';
  msgInactive = 'Ruby library is not active!';
  msgInvalidVersion = 'Invalid Ruby library version: %s!';
  msgNotFound = 'Ruby library v%s is not found!';
  msgException = 'Ruby exception occured:' + LineEnding + '%s';
  msgFinalization = 'Error in Ruby finalization!';

var
  flag_active : Boolean = False;

function version_match (const actual, needed : array of cint) : Boolean; local;
 var
   i, ah, nh : Integer;
 begin
  ah := High(actual);
  nh := High(needed);
  for i := 0 to 3 do
      if (ah < i) or (nh < i) or (actual[i] = V_ANY) or (needed[i] = V_ANY)
         then exit(True)
         else if actual[i] <> needed[i]
                 then exit(False);
  Result := True;
 end;

function check_version (const needed : array of cint) : Boolean; local;
 var
   actual : array [0..3] of cint;
 begin
  actual[0] := ruby_api_version[0];
  actual[1] := ruby_api_version[1];
  actual[2] := ruby_api_version[2];
  actual[3] := ruby_patchlevel;
  Result := version_match(actual, needed);
 end;

function version_string (const version : array of cint) : AnsiString; local;
 var
   i : Integer;
 begin
  for i := 0 to High(version) do
      if version[i] = V_ANY
         then break
         else if i = 0
                 then Result := IntToStr(version[i])
                 else Result := Result + '.' + IntToStr(version[i]);
 end;

{$if defined(RUBY_DYNAMIC)}
var
  pp_ruby_versions : array of record
    version : array [0..3] of cint;
    info : RbInfo;
    libs : array of UnicodeString;
  end = nil;

procedure pp_reg_ruby_version (const version : array of cint; const info : RbInfo;
  const libs : array of UnicodeString);
 var
   l, i, h : Integer;
 begin
  l := Length(pp_ruby_versions);
  SetLength(pp_ruby_versions, l + 1);
  h := High(version);
  for i := 0 to 3 do
      if i <= h
         then pp_ruby_versions[l].version[i] := version[i]
         else pp_ruby_versions[l].version[i] := V_ANY;
  pp_ruby_versions[l].info := info;
  h := High(libs);
  SetLength(pp_ruby_versions[l].libs, h + 1);
  for i := 0 to h do
      pp_ruby_versions[l].libs[i] := libs[i];
 end;

procedure ruby_load_begin (const version : array of cint); local;
 var
   vi, ni : Integer;
 begin
  for vi := High(pp_ruby_versions) downto 0 do
      if version_match(version, pp_ruby_versions[vi].version)
         then for ni := 0 to High(pp_ruby_versions[vi].libs) do
                  begin
                   libHandle := LoadLibrary(pp_ruby_versions[vi].libs[ni]);
                   if libHandle <> 0
                      then begin
                            load_procs;
                            load_consts(pp_ruby_versions[vi].info.check_flonum);
                            if not check_version(version)
                               then begin
                                     FreeLibrary(libHandle);
                                     libHandle := 0;
                                    end;
                           end;
                   if libHandle <> 0
                      then exit;
                  end;
  raise ERubyLibraryError.CreateFmt(msgNotFound, [version_string(version)]);
 end;

procedure ruby_load_end; local;
 begin
  load_vars;
 end;

procedure ruby_free; local;
 begin
  FreeLibrary(libHandle);
  libHandle := 0;
 end;

{$endif}

procedure pp_ruby_init (const version : array of cint);
 var
   i : Integer;
 begin
  if pp_ruby_active
     then raise ERubyLibraryError.Create(msgAlreadyActive);
  {$if defined(RUBY_DYNAMIC)}
  ruby_load_begin(version);
  {$else}
  if not check_version(version)
     then raise ERubyLibraryError.CreateFmt(msgInvalidVersion,
                                            [version_string(version)]);
  {$endif}
  ruby_sysinit(@argc, @argv);
  ruby_init();
  ruby_init_loadpath();
  ruby_script(PChar(ParamStr(0)));
  flag_active := True;
  {$if defined(RUBY_DYNAMIC)}
  ruby_load_end;
  {$endif}
  for i := 0 to High(init_hooks) do
      if init_hooks[i] <> nil
         then init_hooks[i]();
 end;

procedure pp_ruby_init;
 begin
  pp_ruby_init([]);
 end;

procedure pp_ruby_done (check_errinfo : Boolean = True);
 var
   i : Integer;
   e : VALUE;
 begin
  if not pp_ruby_active
     then raise ERubyLibraryError.Create(msgInactive);
  for i := High(done_hooks) downto 0 do
      if done_hooks[i] <> nil
         then done_hooks[i]();
  if check_errinfo
     then begin
           e := rb_errinfo();
           if e <> Qnil
              then raise ERubyException.Create(e);
          end
     else rb_set_errinfo(Qnil);
  if ruby_cleanup(0) <> 0
     then ERubyLibraryError.Create(msgFinalization);
  flag_active := False;
  {$if defined(RUBY_DYNAMIC)}
  ruby_free;
  {$endif}
 end;

function pp_ruby_active : Boolean; inline;
 begin
  Result := flag_active;
 end;

{ ERubyException }

constructor ERubyException.Create (err : VALUE);
 var
   str : VALUE;
 begin
  fld_errinfo := err;
  str := rb_inspect(err);
  rb_set_errinfo(Qnil);
  inherited CreateFmt(msgException, [rb_string_value_cstr(@str)]);
 end;

procedure pp_check_active; inline;
 begin
  if not pp_ruby_active
     then raise ERubyLibraryError.Create(msgInactive);
 end;

procedure pp_check_result (res : cint); inline;
 begin
  if res <> 0
     then raise ERubyException.Create(rb_errinfo());
 end;

initialization
{$if defined(RUBY_DYNAMIC)}
 {$include rbdyninit.inc}
{$endif}
end.

