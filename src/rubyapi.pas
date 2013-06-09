{$smartlink on}
{$mode ObjFPC}{$H+}
{$packrecords C}

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
  PVALUE = ^VALUE;
  ID = type PtrUInt;

  F_bl_proc = type Pointer; // function : VALUE; cdecl; varargs;
  F_func    = function : VALUE; cdecl;

type
  RNode = record end;
  PRNode = ^RNode;

// common functions

procedure rb_alias (klass : VALUE; name, def : ID); cdecl; external RUBYLIB;
procedure rb_alias_variable (name1, name2 : ID); cdecl; external RUBYLIB;
function rb_any_to_s (obj : VALUE) : VALUE; cdecl; external RUBYLIB;
function rb_apply (recv : VALUE; mid : ID; args : VALUE) : VALUE; cdecl;
  external RUBYLIB;
function rb_Array (val : VALUE) : VALUE; cdecl; external RUBYLIB;
function rb_ary_aref (argc : cint; argv : PVALUE; ary : VALUE) : VALUE; cdecl;
  external RUBYLIB;
function rb_ary_assoc (ary, key : VALUE) : VALUE; cdecl; external RUBYLIB;
function rb_ary_clear (ary : VALUE) : VALUE; cdecl; external RUBYLIB;
function rb_ary_cmp (ary1, ary2 : VALUE) : VALUE; cdecl; external RUBYLIB;
function rb_ary_concat (x, y : VALUE) : VALUE; cdecl; external RUBYLIB;
function rb_ary_delete (ary, item : VALUE) : VALUE; cdecl; external RUBYLIB;
function rb_ary_delete_at (ary : VALUE; pos : clong) : VALUE; cdecl;
  external RUBYLIB;
function rb_ary_dup (ary : VALUE) : VALUE; cdecl; external RUBYLIB;
function rb_ary_each (ary : VALUE) : VALUE; cdecl; external RUBYLIB;
function rb_ary_entry (ary : VALUE; offset : clong) : VALUE; cdecl;
  external RUBYLIB;
function rb_ary_freeze (ary : VALUE) : VALUE; cdecl; external RUBYLIB;
function rb_ary_includes (ary, item : VALUE) : VALUE; cdecl; external RUBYLIB;
function rb_ary_join (ary, sep : VALUE) : VALUE; cdecl; external RUBYLIB;
function rb_ary_new : VALUE; cdecl; external RUBYLIB;
function rb_ary_new2 (len : clong) : VALUE; cdecl; external RUBYLIB;
function rb_ary_new3 (n : clong) : VALUE; cdecl; varargs; external RUBYLIB;
function rb_ary_new4 (n : clong; elts : PVALUE) : VALUE; cdecl;
  external RUBYLIB;
function rb_ary_plus (x, y : VALUE) : VALUE; cdecl; external RUBYLIB;
function rb_ary_pop (ary : VALUE) : VALUE; cdecl; external RUBYLIB;
function rb_ary_push (ary, item : VALUE) : VALUE; cdecl; external RUBYLIB;
function rb_ary_rassoc (ary, value : VALUE) : VALUE; cdecl; external RUBYLIB;
function rb_ary_reverse (ary : VALUE) : VALUE; cdecl; external RUBYLIB;
function rb_ary_shift (ary : VALUE) : VALUE; cdecl; external RUBYLIB;
function rb_ary_sort (ary : VALUE) : VALUE; cdecl; external RUBYLIB;
function rb_ary_sort_bang (ary : VALUE) : VALUE; cdecl; external RUBYLIB;
procedure rb_ary_store (ary : VALUE; idx : clong; val : VALUE); cdecl;
  external RUBYLIB;
function rb_ary_to_ary (obj : VALUE) : VALUE; cdecl; external RUBYLIB;
function rb_ary_to_s (ary : VALUE) : VALUE; cdecl; external RUBYLIB;
function rb_ary_unshift (ary, item : VALUE) : VALUE; cdecl; external RUBYLIB;
function rb_assoc_new (car, cdr : VALUE) : VALUE; cdecl; external RUBYLIB;
procedure rb_attr (klass : VALUE; id : ID; read, write, ex : cint); cdecl;
  external RUBYLIB;
function rb_attr_get (obj : VALUE; id : ID) : VALUE; cdecl; external RUBYLIB;
procedure rb_autoload (_mod : VALUE; id : ID; _file : PChar); cdecl;
  external RUBYLIB;
function rb_autoload_load (_mod : VALUE; id : ID) : VALUE; cdecl;
  external RUBYLIB;
function rb_autoload_p (_mod : VALUE; id : ID) : VALUE; cdecl; external RUBYLIB;
function rb_backref_get : VALUE; cdecl; external RUBYLIB;
procedure rb_backref_set (val : VALUE); cdecl; external RUBYLIB;
procedure rb_backtrace; cdecl; external RUBYLIB;
procedure rb_big_2comp (x : VALUE); cdecl; external RUBYLIB;
function rb_big2dbl (x : VALUE) : cdouble; cdecl; external RUBYLIB;
function rb_big2ll (x : VALUE) : clonglong; cdecl; external RUBYLIB;
function rb_big2long (x : VALUE) : clong; cdecl; external RUBYLIB;
function rb_big2str (x : VALUE; base : cint) : VALUE; cdecl; external RUBYLIB;
function rb_big2str0 (x : VALUE; base, trim : cint) : VALUE; cdecl;
  external RUBYLIB;
function rb_big2ull (x : VALUE) : culonglong; cdecl; external RUBYLIB;
function rb_big2ulong (x : VALUE) : culong; cdecl; external RUBYLIB;
function rb_big2ulong_pack (x : VALUE) : culong; cdecl; external RUBYLIB;
function rb_big_and (xx, yy : VALUE) : VALUE; cdecl; external RUBYLIB;
function rb_big_clone (x : VALUE) : VALUE; cdecl; external RUBYLIB;
function rb_big_divmod (x, y : VALUE) : VALUE; cdecl; external RUBYLIB;
function rb_big_lshift (x, y : VALUE) : VALUE; cdecl; external RUBYLIB;
function rb_big_minus (x, y : VALUE) : VALUE; cdecl; external RUBYLIB;
function rb_big_mul (x, y : VALUE) : VALUE; cdecl; external RUBYLIB;
function rb_big_norm (x : VALUE) : VALUE; cdecl; external RUBYLIB;
function rb_big_or (xx, yy : VALUE) : VALUE; cdecl; external RUBYLIB;
procedure rb_big_pack (val : VALUE; buf : pculong; num_longs : clong); cdecl;
  external RUBYLIB;
function rb_big_plus (x, y : VALUE) : VALUE; cdecl; external RUBYLIB;
function rb_big_pow (x, y : VALUE) : VALUE; cdecl; external RUBYLIB;
function rb_big_rshift (x, y : VALUE) : VALUE; cdecl; external RUBYLIB;
function rb_big_unpack (bug : pculong; num_longs : clong) : VALUE; cdecl;
  external RUBYLIB;
function rb_big_xor (xx, yy : VALUE) : VALUE; cdecl; external RUBYLIB;
function rb_block_call (obj : VALUE; mid : ID; argc : cint; argv : PVALUE;
  bl_proc : F_bl_proc; data2 : VALUE) : VALUE; cdecl; external RUBYLIB;
function rb_block_given_p : cint; cdecl; external RUBYLIB;
function rb_block_proc : VALUE; cdecl; external RUBYLIB;
procedure rb_bug (fmt : PChar); cdecl; varargs; external RUBYLIB;
function rb_call_super (argc : cint; argv : PVALUE) : VALUE; cdecl;
  external RUBYLIB;
function rb_catch (tag : PChar; func : F_func; data : VALUE) : VALUE; cdecl;
  external RUBYLIB;
function rb_check_array_type (ary : VALUE) : VALUE; cdecl; external RUBYLIB;
function rb_check_convert_type (val : VALUE; _type : cint; tname : PChar;
  method : PChar) : VALUE; cdecl; external RUBYLIB;
procedure rb_check_frozen (obj : VALUE); cdecl; external RUBYLIB;
procedure rb_check_inheritable (super : VALUE); cdecl; external RUBYLIB;
procedure rb_check_safe_obj (x : VALUE); cdecl; external RUBYLIB;
procedure rb_check_safe_str (x : VALUE); cdecl; external RUBYLIB;
function rb_check_string_type (str : VALUE) : VALUE; cdecl; external RUBYLIB;
function rb_check_to_integer (val : VALUE; method : PChar) : VALUE; cdecl;
  external RUBYLIB;
procedure rb_check_type (x : VALUE; t : cint); cdecl; external RUBYLIB;
function rb_class2name (klass : VALUE) : PChar; cdecl; external RUBYLIB;
function rb_class_boot (super : VALUE) : VALUE; cdecl; external RUBYLIB;
function rb_class_inherited (super, klass : VALUE) : VALUE; cdecl;
  external RUBYLIB;
function rb_class_inherited_p (_mod, arg : VALUE) : VALUE; cdecl;
  external RUBYLIB;
function rb_class_instance_methods (argc : cint; argv : PVALUE;
  _mod : VALUE) : VALUE; cdecl; external RUBYLIB;
function rb_class_name (klass : VALUE) : VALUE; cdecl; external RUBYLIB;
function rb_class_new (super : VALUE) : VALUE; cdecl; external RUBYLIB;
function rb_class_new_instance (argc : cint; argv : PVALUE;
  klass : VALUE) : VALUE; cdecl; external RUBYLIB;
function rb_class_path (klass : VALUE) : VALUE; cdecl; external RUBYLIB;
function rb_class_private_instance_methods (argc : cint; argv : PVALUE;
  _mod : VALUE) : VALUE; cdecl; external RUBYLIB;
function rb_class_protected_instance_methods (argc : cint; argv : PVALUE;
  _mod : VALUE) : VALUE; cdecl; external RUBYLIB;
function rb_class_public_instance_methods (argc : cint; argv : PVALUE;
  _mod : VALUE) : VALUE; cdecl; external RUBYLIB;
function rb_class_real (cl : VALUE) : VALUE; cdecl; external RUBYLIB;
procedure rb_clear_cache; cdecl; external RUBYLIB;
procedure rb_clear_cache_by_class (klass : VALUE); cdecl; external RUBYLIB;
procedure rb_cmperr (x, y : VALUE); cdecl; external RUBYLIB;
function rb_cmpint (val, a, b : VALUE) : cint; cdecl; external RUBYLIB;
function rb_compile_cstr (f, s : PChar; len, line : cint) : PRNode; cdecl;
  external RUBYLIB;
procedure rb_compile_error_append (fmt : PChar); cdecl; varargs;
  external RUBYLIB;
function rb_compile_file (f : PChar; _file : VALUE; start : cint) : PRNode;
  cdecl; external RUBYLIB;
function rb_compile_string (f : PChar; s : VALUE; line : cint) : PRNode; cdecl;
  external RUBYLIB;
function rb_const_defined (klass : VALUE; id : ID) : cint; cdecl;
  external RUBYLIB;
function rb_const_defined_at (klass : VALUE; id : ID) : cint; cdecl;
  external RUBYLIB;
function rb_const_defined_from (klass : VALUE; id : ID) : cint; cdecl;
  external RUBYLIB;
function rb_const_get (klass : VALUE; id : ID) : VALUE; cdecl; external RUBYLIB;
function rb_const_get_at (klass : VALUE; id : ID) : VALUE; cdecl;
  external RUBYLIB;
function rb_const_get_from (klass : VALUE; id : ID) : VALUE; cdecl;
  external RUBYLIB;
function rb_const_list (data : Pointer) : VALUE; cdecl; external RUBYLIB;
procedure rb_const_set (klass : VALUE; id : ID; val : VALUE); cdecl;
  external RUBYLIB;
function rb_convert_type (val : VALUE; _type : cint; tname : PChar;
  method : PChar) : VALUE; cdecl; external RUBYLIB;
procedure rb_copy_generic_ivar (clone, obj : VALUE); cdecl; external RUBYLIB;

// common variables

var
  rb_argv0 : VALUE; cvar; external RUBYLIB;

  rb_cArray         : VALUE; cvar; external RUBYLIB;
  rb_cBignum        : VALUE; cvar; external RUBYLIB;
  rb_cBinding       : VALUE; cvar; external RUBYLIB;
  rb_cClass         : VALUE; cvar; external RUBYLIB;
  rb_cData          : VALUE; cvar; external RUBYLIB;
  rb_cDir           : VALUE; cvar; external RUBYLIB;
  rb_cEnumerator    : VALUE; cvar; external RUBYLIB;
  rb_cFalseClass    : VALUE; cvar; external RUBYLIB;
  rb_cFile          : VALUE; cvar; external RUBYLIB;
  rb_cFixnum        : VALUE; cvar; external RUBYLIB;
  rb_cFloat         : VALUE; cvar; external RUBYLIB;
  rb_cHash          : VALUE; cvar; external RUBYLIB;
  rb_cInteger       : VALUE; cvar; external RUBYLIB;
  rb_cIO            : VALUE; cvar; external RUBYLIB;
  rb_cMatch         : VALUE; cvar; external RUBYLIB;
  rb_cMethod        : VALUE; cvar; external RUBYLIB;
  rb_cModule        : VALUE; cvar; external RUBYLIB;
  rb_cNameErrorMesg : VALUE; cvar; external RUBYLIB;
  rb_cNilClass      : VALUE; cvar; external RUBYLIB;
  rb_cNumeric       : VALUE; cvar; external RUBYLIB;
  rb_cObject        : VALUE; cvar; external RUBYLIB;
  rb_cProc          : VALUE; cvar; external RUBYLIB;
  rb_cRange         : VALUE; cvar; external RUBYLIB;
  rb_cRegexp        : VALUE; cvar; external RUBYLIB;
  rb_cStat          : VALUE; cvar; external RUBYLIB;

{$if defined(RUBY19) or defined(RUBY20)}

// Ruby 1.9 types

type
  rb_event_flag_t = cuint;
  rb_event_hook_func_t = procedure (event : rb_event_flag_t; data : VALUE;
    value : VALUE; id : ID; klass : VALUE); cdecl;
  rb_encoding = record end;
  Prb_encoding = ^rb_encoding;
  rb_data_type_t = record end;
  Prb_data_type_t = ^rb_data_type_t;

// Ruby 1.9 functions

procedure rb_add_event_hook (func : rb_event_hook_func_t;
  events : rb_event_flag_t; data : VALUE); cdecl; external RUBYLIB;
function rb_alloc_tmp_buffer (store : PVALUE; len : cint) : Pointer; cdecl;
  external RUBYLIB;
procedure rb_ary_free (ary : VALUE); cdecl; external RUBYLIB;
function rb_ary_memsize (ary : VALUE) : csize_t; cdecl; external RUBYLIB;
procedure rb_ary_modify (ary : VALUE); cdecl; external RUBYLIB;
function rb_ary_replace (copy, orig : VALUE) : VALUE; cdecl; external RUBYLIB;
function rb_ary_resize (ary : VALUE; len : clong) : VALUE; cdecl;
  external RUBYLIB;
function rb_ary_resurrect (ary : VALUE) : VALUE; cdecl; external RUBYLIB;
function rb_ary_subseq (ary : VALUE; beg, len : clong) : VALUE; cdecl;
  external RUBYLIB;
function rb_ary_tmp_new (capa : clong) : VALUE; cdecl; external RUBYLIB;
function rb_ascii8bit_encindex : cint; cdecl; external RUBYLIB;
function rb_ascii8bit_encoding : Prb_encoding; cdecl; external RUBYLIB;
function rb_barrier_destroy (_self : VALUE) : VALUE; cdecl; external RUBYLIB;
function rb_barrier_new : VALUE; cdecl; external RUBYLIB;
function rb_barrier_release (_self : VALUE) : VALUE; cdecl; external RUBYLIB;
function rb_barrier_wait (_self : VALUE) : VALUE; cdecl; external RUBYLIB;
function rb_big_cmp (x, y : VALUE) : VALUE; cdecl; external RUBYLIB;
function rb_big_div (x, y : VALUE) : VALUE; cdecl; external RUBYLIB;
function rb_big_eq (x, y : VALUE) : VALUE; cdecl; external RUBYLIB;
function rb_big_eql (x, y : VALUE) : VALUE; cdecl; external RUBYLIB;
function rb_big_idiv (x, y : VALUE) : VALUE; cdecl; external RUBYLIB;
function rb_big_modulo (x, y : VALUE) : VALUE; cdecl; external RUBYLIB;
function rb_big_new (len : clong; sign : cint) : VALUE; cdecl; external RUBYLIB;
procedure rb_big_resize (big : VALUE; len : clong); cdecl; external RUBYLIB;
function rb_bigzero_p (x : VALUE) : cint; cdecl; external RUBYLIB;
function rb_binding_new : VALUE; cdecl; external RUBYLIB;
procedure rb_bug_errno (mesg : PChar; errno_arg : cint); cdecl; external RUBYLIB;
function rb_catch_obj (tag : VALUE; func : F_func; data : VALUE) : VALUE; cdecl;
  external RUBYLIB;
function rb_check_funcall (recv : VALUE; mid : ID; argc : cint;
  argv : PVALUE) : VALUE; cdecl; external RUBYLIB;
function rb_check_hash_type (hash : VALUE) : VALUE; cdecl; external RUBYLIB;
function rb_check_to_float (val : VALUE) : VALUE; cdecl; external RUBYLIB;
procedure rb_check_typeddata (obj : VALUE; data_type : Prb_data_type_t); cdecl;
  external RUBYLIB;
function rb_class_get_superclass (klass : VALUE) : VALUE; cdecl;
  external RUBYLIB;
function rb_class_superclass (klass : VALUE) : VALUE; cdecl; external RUBYLIB;
procedure rb_close_before_exec (lowfd, maxhint : cint; noclose_fds : VALUE);
  cdecl; external RUBYLIB;
procedure rb_compile_error (_file : PChar; line : cint; fmt : PChar); cdecl;
  varargs; external RUBYLIB;
procedure rb_compile_error_with_enc (_file : PChar; line : cint; enc : Pointer;
  fmt : PChar); cdecl; varargs; external RUBYLIB;
procedure rb_compile_warn (_file : PChar; line : cint; fmt : PChar); cdecl;
  varargs; external RUBYLIB;
procedure rb_compile_warning (_file : PChar; line : cint; fmt : PChar); cdecl;
  varargs; external RUBYLIB;
function rb_Complex (x, y : VALUE) : VALUE; cdecl; external RUBYLIB;
function rb_complex_new (x, y : VALUE) : VALUE; cdecl; external RUBYLIB;
function rb_complex_polar (x, y : VALUE) : VALUE; cdecl; external RUBYLIB;
function rb_complex_raw (x, y : VALUE) : VALUE; cdecl; external RUBYLIB;
function rb_const_remove (_mod : VALUE; id : ID) : VALUE; cdecl;
  external RUBYLIB;

// Ruby 1.9 variables

var
  rb_cBasicObject : VALUE; cvar; external RUBYLIB;
  rb_cComplex     : VALUE; cvar; external RUBYLIB;
  rb_cEncoding    : VALUE; cvar; external RUBYLIB;
  rb_cEnv         : VALUE; cvar; external RUBYLIB;
  rb_cISeq        : VALUE; cvar; external RUBYLIB;
  rb_cRandom      : VALUE; cvar; external RUBYLIB;
  rb_cRational    : VALUE; cvar; external RUBYLIB;
  rb_cRubyVM      : VALUE; cvar; external RUBYLIB;

{$elseif defined(RUBY18)}

// Ruby 1.8 types

type
  rb_event_t = cuint;
  rb_event_hook_func_t = procedure (event : rb_event_t; node : PRNode;
    value : VALUE; id : ID; klass : VALUE); cdecl;
  st_table = record end;
  Pst_table = ^st_table;

// Ruby 1.8 functions

procedure rb_add_event_hook (func : rb_event_hook_func_t; events : rb_event_t);
  cdecl; external RUBYLIB;
procedure rb_add_method (klass : VALUE; mid : ID; node : PRNode; noex : cint);
  cdecl; external RUBYLIB;
function rb_big_mul0 (x, y : VALUE) : VALUE; cdecl; external RUBYLIB;
function rb_big_rand (max : VALUE; rand_buf : pcdouble) : VALUE; cdecl;
  external RUBYLIB;
function rb_block_dup (_self, klass, cref : VALUE) : VALUE; cdecl;
  external RUBYLIB;
procedure rb_call_inits; cdecl; external RUBYLIB;
function rb_check_backtrace (bt : VALUE) : VALUE; cdecl; external RUBYLIB;
function rb_class_init_copy (clone, orig : VALUE) : VALUE; cdecl;
  external RUBYLIB;
procedure rb_compile_error (fmt : PChar); cdecl; varargs; external RUBYLIB;
function rb_copy_node_scope (node, rval : PRNode) : PRNode; cdecl;
  external RUBYLIB;

// Ruby 1.8 variables

var
  rb_argv : VALUE; cvar; external RUBYLIB;

  rb_cCont : VALUE; cvar; external RUBYLIB;

  rb_class_tbl : Pst_table; cvar; external RUBYLIB;

{$endif}

implementation

end.

