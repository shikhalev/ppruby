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
  {$fatal Undefined Ruby version! }
{$endif}
{$if defined(DARWIN)}
  RUBYLIB = '/System/Library/Frameworks/Ruby.framework/Versions/' + VER_MAJOR +
    '.' + VER_MINOR + '/Ruby';
{$elseif defined(UNIX)}
  RUBYLIB = 'libruby' + VER_MAJOR + VER_MINOR + '.so';
{$elseif defined(WINDOWS)}
  RUBYLIB = 'msvcrt-ruby' + VER_MAJOR + VER_MINOR + '.dll';
{$else}
  {$fatal Unknown OS! }
{$endif}

// common types

type
  VALUE = type PtrUInt;
  ID = type PtrUInt;

// common functions

procedure rb_alias (klass : VALUE; name, def : ID); cdecl; external RUBYLIB;

// common variables

{$if defined(RUBY19)}

// Ruby 1.9 types

type
  rb_event_flag_t = cuint;
  rb_event_hook_func_t = procedure (event : rb_event_flag_t; data : VALUE;
    value : VALUE; id : ID; klass : VALUE); cdecl;

// Ruby 1.9 functions

procedure rb_add_event_hook (func : rb_event_hook_func_t;
  events : rb_event_flag_t; data : VALUE); cdecl; external RUBYLIB;

// Ruby 1.9 variables

{$elseif defined(RUBY18)}

// Ruby 1.8 types

type
  RNode = record end;
  PRNode = ^RNode;

type
  rb_event_t = cuint;
  rb_event_hook_func_t = procedure (event : rb_event_t; node : PRNode;
    value : VALUE; id : ID; klass : VALUE); cdecl;

// Ruby 1.8 functions

procedure rb_add_event_hook (func : rb_event_hook_func_t; events : rb_event_t);
  cdecl; external RUBYLIB;

// Ruby 1.8 variables

{$endif}

implementation

end.

