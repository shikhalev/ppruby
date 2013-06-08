{$if defined(RUBY20)}
  {$define RUBY19}
{$endif}

{$mode ObjFPC}{$H+}

unit RubyAPI;

{$if defined(DARWIN)}
  {$linkframework Ruby}
{$endif}

interface

uses
  ctypes;

type
  VALUE = type PtrUInt;
  PVALUE = ^VALUE;

  ID = type PtrUInt;
  PID = ^ID;

const
{$if defined(RUBY20)}
  VER_MAJOR = '2';
  VER_MINOR = '0';
{$elseif defined(RUBY19)}
  VER_MAJOR = '1';
  VER_MINOR = '9';
{$elseif defined(RUBY18)}
  VER_MAJOR = '1';
  VER_MINOR = '8';
{$else}
  {$error Indefined Ruby version! }
{$endif}
{$if defined(DARWIN)}
  RUBYLIB = '/System/Library/Frameworks/Ruby.framework/Versions/' + VER_MAJOR +
    '.' + VER_MINOR + '/Ruby';
{$elseif defined(UNIX)}
  RUBYLIB = 'libruby' + VER_MAJOR + VER_MINOR + '.so';
{$elseif defined(WINDOWS)}
  RUBYLIB = 'msvcrt-ruby' + VER_MAJOR + VER_MINOR + '.dll';
{$else}
  {$error Unknown OS! }
{$endif}

procedure ruby_init; cdecl; external RUBYLIB;
procedure ruby_init_loadpath; cdecl; external RUBYLIB;
procedure ruby_finalize; cdecl; external RUBYLIB;

procedure ruby_script (name : PChar); cdecl; external RUBYLIB;

function rb_define_module (name : PChar) : VALUE; cdecl; external RUBYLIB;
function rb_define_module_under (namespace : VALUE; name : PChar) : VALUE;
         cdecl; external RUBYLIB;
function rb_define_class (name : PChar; superclass : VALUE) : VALUE; cdecl;
         external RUBYLIB;
function rb_define_class_under (namespace : VALUE; name : PChar;
         superclass : VALUE) : VALUE; cdecl; external RUBYLIB;
procedure rb_define_const (namespace : VALUE; name : PChar; value : VALUE);
         cdecl; external RUBYLIB;
procedure rb_define_global_const (name : PChar; value : VALUE); cdecl;
         external RUBYLIB;
procedure rb_define_method (module : VALUE; name : PChar; func : Pointer;
         argc : cint); cdecl; external RUBYLIB;
procedure rb_define_module_function (module : VALUE; name : PChar;
         func : Pointer; argc : cint); cdecl; external RUBYLIB;
procedure rb_define_singleton_method (instance : VALUE; name : PChar;
         func : Pointer; argc : cint); cdecl; external RUBYLIB;
procedure rb_define_global_function (name : PChar; func : Pointer; argc : cint);
         cdecl; external RUBYLIB;

implementation

end.

