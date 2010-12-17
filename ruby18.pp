(*
    unit ruby18
    license : GNU GPL
*)

{$mode objfpc}{$h+}
{$packenum 4}
{$packrecords C}
{$smartlink on}
{$writeableconst off}

unit ruby18;

interface

function ruby_xmalloc (size : ptrint) : pointer; cdecl; external 'ruby18';
function ruby_xcalloc (n, size : ptrint) : pointer; cdecl; external 'ruby18';
function ruby_xrealloc (ptr : pointer; size : ptrint) : pointer;
    cdecl; external 'ruby18';
procedure ruby_xfree (x : pointer); cdecl; external 'ruby18';

type
    PPVALUE = ^PVALUE;
    PVALUE  = ^VALUE;
    VALUE   = record data : ptruint end;

    PID = ^ID;
    ID  = record data : ptruint end;

operator = (a, b : VALUE) : boolean; inline;
operator = (a, b : ID) : boolean; inline;

const
    FIXNUM_MAX  = high(ptrint) shr 1;
    FIXNUM_MIN  = (low(ptrint) shr 1) or low(ptrint);

    FIXNUM_FLAG = $01;

function rb_int2inum (n : ptrint) : VALUE; cdecl; external 'ruby18';
function rb_uint2inum (n : ptruint) : VALUE; cdecl; external 'ruby18';

function rb_ll2inum (n : int64) : VALUE; cdecl; external 'ruby18';
function rb_ull2inum (n : qword) : VALUE; cdecl; external 'ruby18';

const
    IMMEDIATE_MASK  = $03;

    SYMBOL_FLAG = $0E;

    _Qfalse = 0;
    _Qtrue  = 2;
    _Qnil   = 4;
    _Qundef = 6;
    Qfalse  : VALUE = (data : _Qfalse);
    Qtrue   : VALUE = (data : _Qtrue);
    Qnil    : VALUE = (data : _Qnil);
    Qundef  : VALUE = (data : _Qundef);

    T_NONE      = $00;

    T_NIL       = $01;
    T_OBJECT    = $02;
    T_CLASS     = $03;
    T_ICLASS    = $04;
    T_MODULE    = $05;
    T_FLOAT     = $06;
    T_STRING    = $07;
    T_REGEXP    = $08;
    T_ARRAY     = $09;
    T_FIXNUM    = $0A;
    T_HASH      = $0B;
    T_STRUCT    = $0C;
    T_BIGNUM    = $0D;
    T_FILE      = $0E;

    T_TRUE      = $20;
    T_FALSE     = $21;
    T_DATA      = $22;
    T_MATCH     = $23;
    T_SYMBOL    = $24;

    T_BLKTAG    = $3B;
    T_UNDEF     = $3C;
    T_VARMAP    = $3D;
    T_SCOPE     = $3E;
    T_NODE      = $3F;

    T_MASK      = $3F;

procedure rb_check_type (x : VALUE; t : integer); cdecl; external 'ruby18';

function rb_str_to_str (str : VALUE) : VALUE; cdecl; external 'ruby18';
function rb_string_value (var str : VALUE) : VALUE; cdecl; external 'ruby18';
function rb_string_value_ptr (var str : VALUE) : PByte; cdecl; external 'ruby18';
function rb_string_value_cstr (var str : VALUE) : pchar; cdecl; external 'ruby18';

procedure rb_check_safe_obj (x : VALUE); cdecl; external 'ruby18';
procedure rb_check_safe_str (x : VALUE); cdecl; external 'ruby18';

procedure rb_secure (level : integer); cdecl; external 'ruby18';

var
    ruby_safe_level : integer; cvar; external 'ruby18';

procedure rb_set_safe_level (level : integer); cdecl; external 'ruby18';
procedure rb_secure_update (obj : VALUE); cdecl; external 'ruby18';

function rb_num2long (val : VALUE) : ptrint; cdecl; external 'ruby18';
function rb_num2ulong (val : VALUE) : ptruint; cdecl; external 'ruby18';

function rb_num2int (val : VALUE) : ptrint; cdecl; external 'ruby18';
function rb_fix2int (val : VALUE) : ptrint; cdecl; external 'ruby18';
function rb_num2uint (val : VALUE) : ptruint; cdecl; external 'ruby18';
function rb_fix2uint (val : VALUE) : ptruint; cdecl; external 'ruby18';

function rb_num2ll (val : VALUE) : int64; cdecl; external 'ruby18';
function rb_num2ull (val : VALUE) : qword; cdecl; external 'ruby18';

function rb_num2dbl (val : VALUE) : double; cdecl; external 'ruby18';

function rb_str2cstr (str : VALUE; len : ptrint) : pchar; cdecl; external 'ruby18';

function rb_newobj () : VALUE; cdecl; external 'ruby18';

// BEGIN st.h
type
    Pst_data_t  = ^st_data_t;
    st_data_t   = ptruint;

    Pst_hash_type   = ^st_hash_type;
    st_hash_type    = record
        compare : function () : integer; cdecl;
        hash    : function () : integer; cdecl;
    end;

    Pst_table   = ^st_table;
    st_table    = record
        _type       : Pst_hash_type;
        num_bins    : integer;
        num_entries : integer;
        bins        : pointer;
    end;

function st_init_table (_type : Pst_hash_type) : Pst_table;
    cdecl; external 'ruby18';
function st_init_table_with_size (
    _type   : Pst_hash_type;
    size    : integer
) : Pst_table;
    cdecl; external 'ruby18';
function st_init_numtable () : Pst_table; cdecl; external 'ruby18';
function st_init_numtable_with_size (size : integer) : Pst_table;
    cdecl; external 'ruby18';
function st_init_strtable () : Pst_table; cdecl; external 'ruby18';
function st_init_strtable_with_size (size : integer) : Pst_table;
    cdecl; external 'ruby18';
function st_delete (
    table   : Pst_table;
    key     : Pst_data_t;
    value   : Pst_data_t
) : integer;
    cdecl; external 'ruby18';
function st_delete_safe (
    table   : Pst_table;
    key     : Pst_data_t;
    value   : Pst_data_t;
    never   : st_data_t
) : integer;
    cdecl; external 'ruby18';
function st_insert (
    table   : Pst_table;
    key     : st_data_t;
    value   : st_data_t
) : integer;
    cdecl; external 'ruby18';
function st_lookup (
    table   : Pst_table;
    key     : st_data_t;
    value   : Pst_data_t
) : integer;
    cdecl; external 'ruby18';

type
    Pst_foreach_function = function (
        key, _record, arg : st_data_t
    ) : integer;
        cdecl;

function st_foreach (
    table   : Pst_table;
    func    : Pst_foreach_function;
    arg     : st_data_t
) : integer;
    cdecl; external 'ruby18';
procedure st_add_direct (table : Pst_table; key : st_data_t; value : st_data_t);
    cdecl; external 'ruby18';
procedure st_free_table (table : Pst_table); cdecl; external 'ruby18';
procedure st_cleanup_safe (table : Pst_table; never : st_data_t);
    cdecl; external 'ruby18';
function st_copy (old_table : Pst_table) : Pst_table; cdecl; external 'ruby18';
function st_strhash () : integer; cdecl; external 'ruby18';
// END st.h

type
    PRBasic = ^RBasic;
    RBasic = record
        flags   : ptruint;
        klass   : VALUE;
    end;

    PRObject    = ^RObject;
    RObject     = record
        basic   : RBasic;
        iv_tbl  : Pst_table;
    end;

    PRClass = ^RClass;
    RClass  = record
        basic   : RBasic;
        iv_tbl  : Pst_table;
        m_tbl   : Pst_table;
        super   : VALUE;
    end;

    PRFloat = ^RFloat;
    RFloat  = record
        basic   : RBasic;
        value   : double;
    end;

    PRString    = ^RString;
    RString     = record
        basic   : RBasic;
        len     : ptrint;
        ptr     : pchar;
        aux     : record
            case byte of
            0   : (capa : ptrint);
            1   : (shared   : VALUE);
        end;
    end;

    PRArray = ^RArray;
    RArray  = record
        basic   : RBasic;
        len     : ptrint;
        aux     : record
            case byte of
            0   : (capa : ptrint);
            1   : (shared   : VALUE);
        end;
        ptr     : PVALUE;
    end;

// BEGIN re,h regex.h
function re_mbctab () : PByte; cdecl; external 'ruby18';
procedure re_mbcinit (mbctype : integer); cdecl; external 'ruby18';

type
    Pregister_info_type = ^register_info_type;
    register_info_type  = record
        case byte of
        0   : (word : pchar);
        1   : (
            bits    : bitpacked record
                is_active           : 0..1;
                matched_something   : 0..1;
            end;
        );
    end;

    Pre_pattern_buffer  = ^re_pattern_buffer;
    re_pattern_buffer   = record
        buffer              : pchar;
        allocated           : integer;
        used                : integer;
        fastmap             : pchar;
        must                : pchar;
        must_skip           : PInteger;
        options             : ptrint;
        re_nsub             : ptrint;
        fastmap_accurate    : byte;
        can_be_null         : byte;
        regstart            : Ppchar;
        regend              : Ppchar;
        old_regstart        : Ppchar;
        old_regend          : Ppchar;
        reg_info            : Pregister_info_type;
        best_regstart       : Ppchar;
        best_regend         : Ppchar;
    end;

    Pre_registers   = ^re_registers;
    re_registers    = record
        allocated   : integer;
        num_regs    : integer;
        beg         : PInteger;
        _end        : PInteger;
    end;

    Pregoff_t   = ^regoff_t;
    regoff_t    = ptrint;

    Pregmatch_t = ^regmatch_t;
    regmatch_t  = record
        rm_so   : regoff_t;
        rm_eo   : regoff_t;
    end;

function re_compile_pattern (
    pattern : pchar;
    size    : integer;
    bufp    : Pre_pattern_buffer
) : PByte;
    cdecl; external 'ruby18';
procedure re_free_pattern (bufp : Pre_pattern_buffer); cdecl; external 'ruby18';
function re_adjust_startpos (
    bufp    : Pre_pattern_buffer;
    str     : pchar;
    size, startpos, range   : integer
) : integer;
    cdecl; external 'ruby18';
procedure re_compile_fastmap (bufp : Pre_pattern_buffer);
    cdecl; external 'ruby18';
function re_search (
    bufp    : Pre_pattern_buffer;
    str     : pchar;
    size, startpos, range   : integer;
    regs    : Pre_registers
) : integer;
    cdecl; external 'ruby18';
function re_match (
    bufp    : Pre_pattern_buffer;
    str     : pchar;
    size, pos   : integer;
    regs    : Pre_registers
) : integer;
    cdecl; external 'ruby18';
procedure re_set_casetable (table : pchar); cdecl; external 'ruby18';
procedure re_copy_registers (regs1, regs2 : Pre_registers);
    cdecl; external 'ruby18';
procedure re_free_registers (regs : Pre_registers); cdecl; external 'ruby18';

type
    PRegexp = ^Regexp;
    Regexp  = re_pattern_buffer;

    PRMatch = ^RMatch;
    RMatch  = record
        basic   : RBasic;
        str     : VALUE;
        regs    : Pre_registers
    end;

function rb_reg_regcomp (str : VALUE) : VALUE; cdecl; external 'ruby18';
function rb_reg_search (re, str : VALUE; pos, reverse : ptrint) : ptrint;
    cdecl; external 'ruby18';
function rb_reg_regsub (str, src : VALUE; regs : Pre_registers) : VALUE;
    cdecl; external 'ruby18';
function rb_reg_adjust_startpos (
    re, str : VALUE;
    pos, reverse : ptrint
) : ptrint;
    cdecl; external 'ruby18';
procedure rb_match_busy (match : VALUE); cdecl; external 'ruby18';
function rb_reg_quote (str : VALUE) : VALUE; cdecl; external 'ruby18';

var
    ruby_ignorecase : integer; cvar; external 'ruby18';

function rb_reg_mbclen2 (c : cardinal; re : VALUE) : integer;
    cdecl; external 'ruby18';
// END re.h regex.h

type
    PRRegexp    = ^RRegexp;
    RRegexp     = record
        basic   : RBasic;
        ptr     : Pre_pattern_buffer;
        len     : ptrint;
        str     : pchar;
    end;

    PRHash  = ^RHash;
    RHash   = record
        basic       : RBasic;
        tbl         : Pst_table;
        iter_lev    : integer;
        ifnone      : VALUE;
    end;

// BEGIN rubyio.h
type
    Prb_io_t    = ^rb_io_t;
    rb_io_t     = record
        f           : pointer;
        f2          : pointer;
        mode        : integer;
        pid         : integer;
        lineno      : integer;
        path        : pchar;
        finalize    : procedure (io : Prb_io_t; arg : integer); cdecl;
    end;

function rb_fopen (fname, mode : pchar) : pointer; cdecl; external 'ruby18';
function rb_fdopen (fd : integer; mode : pchar) : pointer;
    cdecl; external 'ruby18';
function rb_getc (f : pointer) : integer; cdecl; external 'ruby18';
function rb_io_fread (ptr : pchar; len : ptrint; f : pointer) : ptrint;
    cdecl; external 'ruby18';
function rb_io_fwrite (ptr : pchar; len : ptrint; f : pointer) : ptrint;
    cdecl; external 'ruby18';
function rb_io_mode_flags (mode : pchar) : integer; cdecl; external 'ruby18';
function rb_io_modenum_flags (mode : integer) : integer;
    cdecl; external 'ruby18';
procedure rb_io_check_writable (fptr : Prb_io_t); cdecl; external 'ruby18';
procedure rb_io_check_readable (fptr : Prb_io_t); cdecl; external 'ruby18';
procedure rb_io_fptr_finalize (fptr : Prb_io_t); cdecl; external 'ruby18';
procedure rb_io_synchronized (fptr : Prb_io_t); cdecl; external 'ruby18';
procedure rb_io_check_initialized (fptr : Prb_io_t); cdecl; external 'ruby18';
procedure rb_io_check_closed (fptr : Prb_io_t); cdecl; external 'ruby18';
function rb_io_wait_readable (f : integer) : integer; cdecl; external 'ruby18';
function rb_io_wait_writable (f : integer) : integer; cdecl; external 'ruby18';
procedure rb_io_set_nonblock (fptr : Prb_io_t); cdecl; external 'ruby18';

function rb_io_taint_check (io : VALUE) : VALUE; cdecl; external 'ruby18';
procedure rb_eof_error (); cdecl; external 'ruby18';

procedure rb_read_check (fp : pointer); cdecl; external 'ruby18';
function rb_read_pending (fp : pointer) : integer; cdecl; external 'ruby18';
// END rubyio.h

type
    PRFile  = ^RFile;
    RFile   = record
        basic   : RBasic;
        fptr    : Prb_io_t;
    end;

    RUBY_DATA_FUNC  = procedure (arg : pointer); cdecl;

    PRData  = ^RData;
    RData   = record
        basic   : RBasic;
        dmark   : RUBY_DATA_FUNC;
        dfree   : RUBY_DATA_FUNC;
        data    : pointer;
    end;

function rb_data_object_alloc (
    klass   : VALUE;
    datap   : pointer;
    dmark   : RUBY_DATA_FUNC;
    dfree   : RUBY_DATA_FUNC
) : VALUE;
    cdecl; external 'ruby18';

type
    PRStruct    = ^RStruct;
    RStruct     = record
        basic   : RBasic;
        len     : ptrint;
        ptr     : PVALUE;
    end;

    PRBignum    = ^RBignum;
    RBignum     = record
        basic   : RBasic;
        sign    : shortint;
        len     : ptrint;
        digits  : pointer;
    end;

const
    FL_MARK     = 1 shl 6;
    FL_FINALIZE = 1 shl 7;
    FL_TAINT    = 1 shl 8;
    FL_EXIVAR   = 1 shl 9;
    FL_FREEZE   = 1 shl 10;

    FL_USHIFT   = 11;
    FL_USER0    = 1 shl (FL_USHIFT+0);
    FL_USER1    = 1 shl (FL_USHIFT+1);
    FL_USER2    = 1 shl (FL_USHIFT+2);
    FL_USER3    = 1 shl (FL_USHIFT+3);
    FL_USER4    = 1 shl (FL_USHIFT+4);
    FL_USER5    = 1 shl (FL_USHIFT+5);
    FL_USER6    = 1 shl (FL_USHIFT+6);
    FL_USER7    = 1 shl (FL_USHIFT+7);

    FL_UMASK    = $FF shl FL_USHIFT;

    FL_SINGLETON    = FL_USER0;

procedure rb_obj_infect (obj1, obj2 : VALUE); cdecl; external 'ruby18';

(*
type
    ruby_glob_func  = function (name : pchar; arg : VALUE) : integer; cdecl;
procedure rb_glob (path : pchar; func : ruby_glob_func; arg : VALUE);
    cdecl; external 'ruby18';
procedure rb_globi (path : pchar; func : ruby_glob_func; arg : VALUE);
    cdecl; external 'ruby18';
*)

function rb_define_class (name : pchar; super : VALUE) : VALUE;
    cdecl; external 'ruby18';
function rb_define_module (name : pchar) : VALUE; cdecl; external 'ruby18';
function rb_define_class_under (
    outer   : VALUE;
    name    : pchar;
    super   : VALUE
) : VALUE;
    cdecl; external 'ruby18';
function rb_define_module_under (outer : VALUE; name : pchar) : VALUE;
    cdecl; external 'ruby18';

procedure rb_include_module (klass : VALUE; module : VALUE);
    cdecl; external 'ruby18';
procedure rb_extend_object (obj : VALUE; module : VALUE);
    cdecl; external 'ruby18';

procedure rb_define_variable (name : pchar; v : PVALUE);
    cdecl; external 'ruby18';

type
    Pgetter = function (args : array of const) : VALUE; cdecl;
    Psetter = procedure (args : array of const); cdecl;
procedure rb_define_virtual_variable (
    name    : pchar;
    getter  : Pgetter;
    setter  : Psetter
);
    cdecl; external 'ruby18';
procedure rb_define_hooked_variable (
    name    : pchar;
    v       : PVALUE;
    getter  : Pgetter;
    setter  : Psetter
);
    cdecl; external 'ruby18';
procedure rb_define_readonly_variable (name : pchar; v : PVALUE);
    cdecl; external 'ruby18';
procedure rb_define_const (klass : VALUE; name : pchar; val : VALUE);
    cdecl; external 'ruby18';
procedure rb_define_global_const (name : pchar; val : VALUE);
    cdecl; external 'ruby18';

type
    Pmethod = function (args : array of const) : VALUE; cdecl;
procedure rb_define_method (
    klass   : VALUE;
    name    : pchar;
    func    : Pmethod;
    argc    : integer
);
    cdecl; external 'ruby18';
procedure rb_define_module_function (
    module  : VALUE;
    name    : pchar;
    func    : Pmethod;
    argc    : integer
);
    cdecl; external 'ruby18';
procedure rb_define_global_function (
    name    : pchar;
    func    : Pmethod;
    argc    : integer
);
    cdecl; external 'ruby18';

procedure rb_undef_method (klass : VALUE; name : pchar);
    cdecl; external 'ruby18';
procedure rb_define_alias (klass : VALUE; name1, name2 : pchar);
    cdecl; external 'ruby18';
procedure rb_define_attr (klass : VALUE; name : pchar; r, w : integer);
    cdecl; external 'ruby18';

procedure rb_global_variable (v : PVALUE); cdecl; external 'ruby18';
procedure rb_gc_register_address (addr : PVALUE); cdecl; external 'ruby18';
procedure rb_gc_unregister_address (addr : PVALUE); cdecl; external 'ruby18';

function rb_intern (name : pchar) : ID; cdecl; external 'ruby18';
function rb_id2name (id : ID) : pchar; cdecl; external 'ruby18';
function rb_to_id (name : VALUE) : ID; cdecl; external 'ruby18';

function rb_class2name (klass : VALUE) : pchar; external 'ruby18';
function rb_obj_classname (obj : VALUE) : pchar; external 'ruby18';

procedure rb_p (obj : VALUE); cdecl; external 'ruby18';

function rb_eval_string (str : pchar) : VALUE; cdecl; external 'ruby18';
function rb_eval_string_protect (str : pchar; state : PInteger) : VALUE;
    cdecl; external 'ruby18';
function rb_eval_string_wrap (str : pchar; state : PInteger) : VALUE;
    cdecl; external 'ruby18';
function rb_funcall (recv : VALUE; mid : ID; n : integer) : VALUE; varargs;
    cdecl; external 'ruby18';
function rb_funcall2 (
    recv    : VALUE;
    mid     : ID;
    argc    : integer;
    argv    : PVALUE
) : VALUE;
    cdecl; external 'ruby18';
function rb_funcall3 (
    recv    : VALUE;
    mid     : ID;
    argc    : integer;
    argv    : PVALUE
) : VALUE;
    cdecl; external 'ruby18';
function rb_scan_args (
    argc    : integer;
    argv    : PVALUE;
    fmt     : pchar
) : integer; varargs;
    cdecl; external 'ruby18';
function rb_call_super (argc : integer; argv : PVALUE) : VALUE;
    cdecl; external 'ruby18';

function rb_gv_set (name : pchar; val : VALUE) : VALUE;
    cdecl; external 'ruby18';
function rb_gv_get (name : pchar) : VALUE; cdecl; external 'ruby18';
function rb_iv_set (obj : VALUE; name : pchar; val : VALUE) : VALUE;
    cdecl; external 'ruby18';
function rb_iv_get (obj : VALUE; name : pchar) : VALUE;
    cdecl; external 'ruby18';

function rb_equal (obj1, obj2 : VALUE) : VALUE; cdecl; external 'ruby18';

var
    ruby_verbose    : VALUE; cvar; external 'ruby18';
    ruby_debug      : VALUE; cvar; external 'ruby18';

procedure rb_raise (exc : VALUE; fmt : pchar); varargs;
    cdecl; external 'ruby18';
procedure rb_fatal (fmt : pchar); varargs;
    cdecl; external 'ruby18';
procedure rb_bug (fmt : pchar); varargs; cdecl; external 'ruby18';
procedure rb_sys_fail (mesg : pchar); cdecl; external 'ruby18';
procedure rb_iter_break (); cdecl; external 'ruby18';
procedure rb_exit (status : integer); cdecl; external 'ruby18';
procedure rb_notimplement (); cdecl; external 'ruby18';

procedure rb_warning (fmt : pchar); varargs;
    cdecl; external 'ruby18';
procedure rb_sys_warning (fmt : pchar); varargs;
    cdecl; external 'ruby18';
procedure rb_warn (fmt : pchar); varargs;
    cdecl; external 'ruby18';

type
    rb_block_call_func  = function (a, b : VALUE) : VALUE; cdecl;

function rb_each (obj : VALUE) : VALUE; cdecl; external 'ruby18';
function rb_yield (val : VALUE) : VALUE; cdecl; external 'ruby18';
function rb_yield_values (n : integer) : VALUE; varargs;
    cdecl; external 'ruby18';
function rb_yield_splat (values : VALUE) : VALUE; cdecl; external 'ruby18';
function rb_block_given_p () : integer; cdecl; external 'ruby18';
procedure rb_need_block (); cdecl; external 'ruby18';

function rb_require (fname : pchar) : VALUE; cdecl; external 'ruby18';

procedure ruby_init_stack (p : PVALUE); cdecl; external 'ruby18';

procedure ruby_init (); cdecl; external 'ruby18';
procedure ruby_options (argc : integer; argv : PPChar);
    cdecl; external 'ruby18';
procedure ruby_run (); cdecl; external 'ruby18';

var
    rb_mKernel          : VALUE; cvar; external 'ruby18';
    rb_mComparable      : VALUE; cvar; external 'ruby18';
    rb_mEnumerable      : VALUE; cvar; external 'ruby18';
    rb_mPrecision       : VALUE; cvar; external 'ruby18';
    rb_mErrno           : VALUE; cvar; external 'ruby18';
    rb_mFileTest        : VALUE; cvar; external 'ruby18';
    rb_mGC              : VALUE; cvar; external 'ruby18';
    rb_mMath            : VALUE; cvar; external 'ruby18';
    rb_mProcess         : VALUE; cvar; external 'ruby18';
    rb_cObject          : VALUE; cvar; external 'ruby18';
    rb_cArray           : VALUE; cvar; external 'ruby18';
    rb_cBignum          : VALUE; cvar; external 'ruby18';
    rb_cBinding         : VALUE; cvar; external 'ruby18';
    rb_cClass           : VALUE; cvar; external 'ruby18';
    rb_cCont            : VALUE; cvar; external 'ruby18';
    rb_cDir             : VALUE; cvar; external 'ruby18';
    rb_cData            : VALUE; cvar; external 'ruby18';
    rb_cEnumerator      : VALUE; cvar; external 'ruby18';
    rb_cFalseClass      : VALUE; cvar; external 'ruby18';
    rb_cFile            : VALUE; cvar; external 'ruby18';
    rb_cFixnum          : VALUE; cvar; external 'ruby18';
    rb_cFloat           : VALUE; cvar; external 'ruby18';
    rb_cHash            : VALUE; cvar; external 'ruby18';
    rb_cInteger         : VALUE; cvar; external 'ruby18';
    rb_cIO              : VALUE; cvar; external 'ruby18';
    rb_cMatch           : VALUE; cvar; external 'ruby18';
    rb_cMethod          : VALUE; cvar; external 'ruby18';
    rb_cModule          : VALUE; cvar; external 'ruby18';
    rb_cNameErrorMesg   : VALUE; cvar; external 'ruby18';
    rb_cNilClass        : VALUE; cvar; external 'ruby18';
    rb_cNumeric         : VALUE; cvar; external 'ruby18';
    rb_cProc            : VALUE; cvar; external 'ruby18';
    rb_cRange           : VALUE; cvar; external 'ruby18';
    rb_cRegexp          : VALUE; cvar; external 'ruby18';
    rb_cStat            : VALUE; cvar; external 'ruby18';
    rb_cString          : VALUE; cvar; external 'ruby18';
    rb_cStruct          : VALUE; cvar; external 'ruby18';
    rb_cSymbol          : VALUE; cvar; external 'ruby18';
    rb_cThread          : VALUE; cvar; external 'ruby18';
    rb_cTime            : VALUE; cvar; external 'ruby18';
    rb_cTrueClass       : VALUE; cvar; external 'ruby18';
    rb_cUnboundMethod   : VALUE; cvar; external 'ruby18';
    rb_eException       : VALUE; cvar; external 'ruby18';
    rb_eStandardError   : VALUE; cvar; external 'ruby18';
    rb_eSystemExit      : VALUE; cvar; external 'ruby18';
    rb_eInterrupt       : VALUE; cvar; external 'ruby18';
    rb_eSignal          : VALUE; cvar; external 'ruby18';
    rb_eFatal           : VALUE; cvar; external 'ruby18';
    rb_eArgError        : VALUE; cvar; external 'ruby18';
    rb_eEOFError        : VALUE; cvar; external 'ruby18';
    rb_eIndexError      : VALUE; cvar; external 'ruby18';
    rb_eStopIteration   : VALUE; cvar; external 'ruby18';
    rb_eRangeError      : VALUE; cvar; external 'ruby18';
    rb_eIOError         : VALUE; cvar; external 'ruby18';
    rb_eRuntimeError    : VALUE; cvar; external 'ruby18';
    rb_eSecurityError   : VALUE; cvar; external 'ruby18';
    rb_eSystemCallError : VALUE; cvar; external 'ruby18';
    rb_eThreadError     : VALUE; cvar; external 'ruby18';
    rb_eTypeError       : VALUE; cvar; external 'ruby18';
    rb_eZeroDivError    : VALUE; cvar; external 'ruby18';
    rb_eNotImpError     : VALUE; cvar; external 'ruby18';
    rb_eNoMemError      : VALUE; cvar; external 'ruby18';
    rb_eNoMethodError   : VALUE; cvar; external 'ruby18';
    rb_eFloatDomainError: VALUE; cvar; external 'ruby18';
    rb_eLocalJumpError  : VALUE; cvar; external 'ruby18';
    rb_eSysStackError   : VALUE; cvar; external 'ruby18';
    rb_eRegexpError     : VALUE; cvar; external 'ruby18';
    rb_eScriptError     : VALUE; cvar; external 'ruby18';
    rb_eNameError       : VALUE; cvar; external 'ruby18';
    rb_eSyntaxError     : VALUE; cvar; external 'ruby18';
    rb_eLoadError       : VALUE; cvar; external 'ruby18';
    rb_stdin            : VALUE; cvar; external 'ruby18';
    rb_stdout           : VALUE; cvar; external 'ruby18';
    rb_stderr           : VALUE; cvar; external 'ruby18';
    ruby_errinfo        : VALUE; cvar; external 'ruby18';

function rb_class_of (obj : VALUE) : VALUE; inline;
function rb_type (obj : VALUE) : integer; inline;

procedure rb_mem_clear (mem : PVALUE; size : ptrint);
    register; external 'ruby18';
function rb_assoc_new (car, cdr : VALUE) : VALUE; cdecl; external 'ruby18';
function rb_check_array_type (ary : VALUE) : VALUE; cdecl; external 'ruby18';
function rb_ary_new () : VALUE; cdecl; external 'ruby18';
function rb_ary_new2 (len : ptrint) : VALUE; cdecl; external 'ruby18';
function rb_ary_new3 (n : ptrint) : VALUE; varargs; cdecl; external 'ruby18';
function rb_ary_new4 (n : ptrint; elts : PVALUE) : VALUE;
    cdecl; external 'ruby18';
function rb_ary_freeze (ary : VALUE) : VALUE; cdecl; external 'ruby18';
function rb_ary_aref (argc : integer; argv : PVALUE; ary : VALUE) : VALUE;
    cdecl; external 'ruby18';
procedure rb_ary_store (ary : VALUE; idx : ptrint; val : VALUE);
    cdecl; external 'ruby18';
function rb_ary_dup (ary : VALUE) : VALUE; cdecl; external 'ruby18';
function rb_ary_to_ary (obj : VALUE) : VALUE; cdecl; external 'ruby18';
function rb_ary_to_s (ary : VALUE) : VALUE; cdecl; external 'ruby18';
function rb_ary_push (ary, item : VALUE) : VALUE; cdecl; external 'ruby18';
function rb_ary_pop (ary : VALUE) : VALUE; cdecl; external 'ruby18';
function rb_ary_shift (ary : VALUE) : VALUE; cdecl; external 'ruby18';
function rb_ary_unshift (ary, item : VALUE) : VALUE; cdecl; external 'ruby18';
function rb_ary_entry (ary : VALUE; offset : ptrint) : VALUE;
    cdecl; external 'ruby18';
function rb_ary_each (ary : VALUE) : VALUE; cdecl; external 'ruby18';
function rb_ary_join (ary, sep : VALUE) : VALUE; cdecl; external 'ruby18';
function rb_ary_reverse (ary : VALUE) : VALUE; cdecl; external 'ruby18';
function rb_ary_sort (ary : VALUE) : VALUE; cdecl; external 'ruby18';
function rb_ary_sort_bang (ary : VALUE) : VALUE; cdecl; external 'ruby18';
function rb_ary_delete (ary, item : VALUE) : VALUE; cdecl; external 'ruby18';
function rb_ary_delete_at (ary : VALUE; pos : ptrint) : VALUE;
    cdecl; external 'ruby18';
function rb_ary_clear (ary : VALUE) : VALUE; cdecl; external 'ruby18';
function rb_ary_plus (x, y : VALUE) : VALUE; cdecl; external 'ruby18';
function rb_ary_concat (x, y : VALUE) : VALUE; cdecl; external 'ruby18';
function rb_ary_assoc (ary, key : VALUE) : VALUE; cdecl; external 'ruby18';
function rb_ary_rassoc (ary, value : VALUE) : VALUE; cdecl; external 'ruby18';
function rb_ary_includes (ary, item : VALUE) : VALUE; cdecl; external 'ruby18';
function rb_ary_cmp (ary1, ary2 : VALUE) : VALUE; cdecl; external 'ruby18';

type
    Pvalue_func = function (args : array of const) : VALUE; cdecl;

function rb_protect_inspect (func : Pvalue_func; obj, arg : VALUE) : VALUE;
    cdecl; external 'ruby18';
function rb_inspecting_p (obj : VALUE) : VALUE; cdecl; external 'ruby18';

function rb_big_clone (x : VALUE) : VALUE; cdecl; external 'ruby18';
procedure rb_big_2comp (x : VALUE); cdecl; external 'ruby18';
function rb_big_norm (x : VALUE) : VALUE; cdecl; external 'ruby18';
function rb_uint2big (n : ptruint) : VALUE; cdecl; external 'ruby18';
function rb_int2big (n : ptrint) : VALUE; cdecl; external 'ruby18';
// function rb_uint2inum (n : ptruint) : VALUE; cdecl; external 'ruby18';
// function rb_int2inum (n : ptrint) : VALUE; cdecl; external 'ruby18';
function rb_cstr_to_inum (str : pchar; base, badcheck : integer) : VALUE;
    cdecl; external 'ruby18';
function rb_str_to_inum (str : VALUE; base, badcheck : integer) : VALUE;
    cdecl; external 'ruby18';
function rb_cstr2inum (str : pchar; base : integer) : VALUE;
    cdecl; external 'ruby18';
function rb_str2inum (str : VALUE; base : integer) : VALUE;
    cdecl; external 'ruby18';
function rb_big2str (x : VALUE; base : integer) : VALUE;
    cdecl; external 'ruby18';
function rb_big2long (x : VALUE) : ptrint; cdecl; external 'ruby18';
function rb_big2ulong (x : VALUE) : ptruint; cdecl; external 'ruby18';

// function rb_ll2inum (n : int64) : VALUE; cdecl; external 'ruby18';
// function rb_ull2inum (n : qword) : VALUE; cdecl; external 'ruby18';
function rb_big2ll (x : VALUE) : int64; cdecl; external 'ruby18';
function rb_big2ull (x : VALUE) : qword; cdecl; external 'ruby18';

procedure rb_quad_pack (buf : pchar; val : VALUE); cdecl; external 'ruby18';
function rb_quad_unpack (buf : pchar; sign : integer) : VALUE;
    cdecl; external 'ruby18';

function rb_dbl2big (d : double) : VALUE; cdecl; external 'ruby18';
function rb_big2dbl (x : VALUE) : double; cdecl; external 'ruby18';

function rb_big_plus (x, y : VALUE) : VALUE; cdecl; external 'ruby18';
function rb_big_minus (x, y : VALUE) : VALUE; cdecl; external 'ruby18';
function rb_big_mul (x, y : VALUE) : VALUE; cdecl; external 'ruby18';
function rb_big_divmod (x, y : VALUE) : VALUE; cdecl; external 'ruby18';
function rb_big_pow (x, y : VALUE) : VALUE; cdecl; external 'ruby18';
function rb_big_and (x, y : VALUE) : VALUE; cdecl; external 'ruby18';
function rb_big_or (x, y : VALUE) : VALUE; cdecl; external 'ruby18';
function rb_big_xor (x, y : VALUE) : VALUE; cdecl; external 'ruby18';
function rb_big_lshift (x, y : VALUE) : VALUE; cdecl; external 'ruby18';
function rb_big_rshift (x, y : VALUE) : VALUE; cdecl; external 'ruby18';
function rb_big_rand (max : VALUE; rand_buf : PDouble) : VALUE;
    cdecl; external 'ruby18';

function rb_class_boot (super : VALUE) : VALUE; cdecl; external 'ruby18';
function rb_class_new (super : VALUE) : VALUE; cdecl; external 'ruby18';
function rb_mod_init_copy (clone, orig : VALUE) : VALUE;
    cdecl; external 'ruby18';
function rb_class_init_copy (clone, orig : VALUE) : VALUE;
    cdecl; external 'ruby18';
function rb_singleton_class_clone (obj : VALUE) : VALUE;
    cdecl; external 'ruby18';
procedure rb_singleton_class_attached (klass, obj : VALUE);
    cdecl; external 'ruby18';
function rb_make_metaclass (obj, super : VALUE) : VALUE;
    cdecl; external 'ruby18';
procedure rb_check_inheritable (super : VALUE); cdecl; external 'ruby18';
function rb_class_inherited (super, klass : VALUE) : VALUE;
    cdecl; external 'ruby18';
function rb_define_class_id (id : ID; super : VALUE) : VALUE;
    cdecl; external 'ruby18';
function rb_module_new () : VALUE; cdecl; external 'ruby18';
function rb_define_module_id (id : ID) : VALUE; cdecl; external 'ruby18';
function rb_mod_included_modules (m : VALUE) : VALUE; cdecl; external 'ruby18';
function rb_mod_include_p (m, m2 : VALUE) : VALUE; cdecl; external 'ruby18';
function rb_mod_ancestors (m : VALUE) : VALUE; cdecl; external 'ruby18';
function rb_class_instance_methods (
    argc    : integer;
    argv    : PVALUE;
    m       : VALUE
) : VALUE;
    cdecl; external 'ruby18';
function rb_class_public_instance_methods (
    argc    : integer;
    argv    : PVALUE;
    m       : VALUE
) : VALUE;
    cdecl; external 'ruby18';
function rb_class_protected_instance_methods (
    argc    : integer;
    argv    : PVALUE;
    m       : VALUE
) : VALUE;
    cdecl; external 'ruby18';
function rb_class_private_instance_methods (
    argc    : integer;
    argv    : PVALUE;
    m       : VALUE
) : VALUE;
    cdecl; external 'ruby18';
function rb_obj_singleton_methods (
    argc    : integer;
    argv    : PVALUE;
    obj     : VALUE
) : VALUE;
    cdecl; external 'ruby18';
function rb_define_method_id (
    klass   : VALUE;
    name    : ID;
    func    : Pmethod;
    argc    : integer
) : VALUE;
    cdecl; external 'ruby18';
procedure rb_frozen_class_p (klass : VALUE); cdecl; external 'ruby18';
procedure rb_undef (klass : VALUE; id : ID); cdecl; external 'ruby18';
procedure rb_define_protected_method (
    klass   : VALUE;
    name    : pchar;
    func    : Pmethod;
    argc    : integer
);
    cdecl; external 'ruby18';
procedure rb_define_private_method (
    klass   : VALUE;
    name    : pchar;
    func    : Pmethod;
    argc    : integer
);
    cdecl; external 'ruby18';
procedure rb_define_singleton_method (
    obj     : VALUE;
    name    : pchar;
    func    : Pmethod;
    argc    : integer
);
    cdecl; external 'ruby18';
function rb_singleton_class (obj : VALUE) : VALUE; cdecl; external 'ruby18';

function rb_cmpint (val, a, b : VALUE) : integer; cdecl; external 'ruby18';
procedure rb_cmperr (x, y : VALUE); cdecl; external 'ruby18';

var
    ruby_nerrs  : integer; cvar; external 'ruby18';

function rb_exc_new (etype : VALUE; ptr : pchar; len : ptrint) : VALUE;
    cdecl; external 'ruby18';
function rb_exc_new2 (etype : VALUE; s : pchar) : VALUE;
    cdecl; external 'ruby18';
function rb_exc_new3 (etype, str : VALUE) : VALUE; cdecl; external 'ruby18';
procedure rb_loaderror (fmt : pchar); varargs; cdecl; external 'ruby18';
procedure rb_name_error (id : ID; fmt : pchar); varargs;
    cdecl; external 'ruby18';
procedure rb_invalid_str (str, _type : pchar); cdecl; external 'ruby18';
procedure rb_compile_error (fmt : pchar); varargs; cdecl; external 'ruby18';
procedure rb_compile_error_append (fmt : pchar); varargs; cdecl; external 'ruby18';
procedure rb_load_fail (path : pchar); cdecl; external 'ruby18';
procedure rb_error_frozen (what : pchar); cdecl; external 'ruby18';
procedure rb_check_frozen (obj : VALUE); cdecl; external 'ruby18';

// BEGIN node.h
type
    Pnode_type  = ^node_type;
    node_type   = (
        NODE_METHOD,
        NODE_FBODY,
        NODE_CFUNC,
        NODE_SCOPE,
        NODE_BLOCK,
        NODE_IF,
        NODE_CASE,
        NODE_WHEN,
        NODE_OPT_N,
        NODE_WHILE,
        NODE_UNTIL,
        NODE_ITER,
        NODE_FOR,
        NODE_BREAK,
        NODE_NEXT,
        NODE_REDO,
        NODE_RETRY,
        NODE_BEGIN,
        NODE_RESCUE,
        NODE_RESBODY,
        NODE_ENSURE,
        NODE_AND,
        NODE_OR,
        NODE_NOT,
        NODE_MASGN,
        NODE_LASGN,
        NODE_DASGN,
        NODE_DASGN_CURR,
        NODE_GASGN,
        NODE_IASGN,
        NODE_CDECL,
        NODE_CVASGN,
        NODE_CVDECL,
        NODE_OP_ASGN1,
        NODE_OP_ASGN2,
        NODE_OP_ASGN_AND,
        NODE_OP_ASGN_OR,
        NODE_CALL,
        NODE_FCALL,
        NODE_VCALL,
        NODE_SUPER,
        NODE_ZSUPER,
        NODE_ARRAY,
        NODE_ZARRAY,
        NODE_HASH,
        NODE_RETURN,
        NODE_YIELD,
        NODE_LVAR,
        NODE_DVAR,
        NODE_GVAR,
        NODE_IVAR,
        NODE_CONST,
        NODE_CVAR,
        NODE_NTH_REF,
        NODE_BACK_REF,
        NODE_MATCH,
        NODE_MATCH2,
        NODE_MATCH3,
        NODE_LIT,
        NODE_STR,
        NODE_DSTR,
        NODE_XSTR,
        NODE_DXSTR,
        NODE_EVSTR,
        NODE_DREGX,
        NODE_DREGX_ONCE,
        NODE_ARGS,
        NODE_ARGSCAT,
        NODE_ARGSPUSH,
        NODE_SPLAT,
        NODE_TO_ARY,
        NODE_SVALUE,
        NODE_BLOCK_ARG,
        NODE_BLOCK_PASS,
        NODE_DEFN,
        NODE_DEFS,
        NODE_ALIAS,
        NODE_VALIAS,
        NODE_UNDEF,
        NODE_CLASS,
        NODE_MODULE,
        NODE_SCLASS,
        NODE_COLON2,
        NODE_COLON3,
        NODE_CREF,
        NODE_DOT2,
        NODE_DOT3,
        NODE_FLIP2,
        NODE_FLIP3,
        NODE_ATTRSET,
        NODE_SELF,
        NODE_NIL,
        NODE_TRUE,
        NODE_FALSE,
        NODE_DEFINED,
        NODE_NEWLINE,
        NODE_POSTEXE,
        NODE_ALLOCA,
        NODE_DMETHOD,
        NODE_BMETHOD,
        NODE_MEMO,
        NODE_IFUNC,
        NODE_DSYM,
        NODE_ATTRASGN,
        NODE_LAST
    );

    PRNode  = ^RNode;
    RNode   = record
        flags   : ptruint;
        nd_file : pchar;
        u1      : record
            case byte of
            0   : (node     : PRNode);
            1   : (id       : ID);
            2   : (value    : VALUE);
            3   : (cfunc    : function (args : array of const) : VALUE; cdecl;);
            4   : (tbl      : PID)
        end;
        u2      : record
            case byte of
            0   : (node     : PRNode);
            1   : (id       : ID);
            2   : (argc     : ptrint);
            3   : (value    : VALUE)
        end;
        u3      : record
            case byte of
            0   : (node     : PRNode);
            1   : (id       : ID);
            2   : (state    : ptrint);
            3   : (entry    : pointer);
            4   : (cnt      : ptrint);
            5   : (value    : VALUE)
        end;
    end;

var
    ruby_cref       : PRNode; cvar; external 'ruby18';
    ruby_top_cref   : PRNode; cvar; external 'ruby18';

const
    NOEX_PUBLIC     = 0;
    NOEX_NOSUPER    = 1;
    NOEX_PRIVATE    = 2;
    NOEX_PROTECTED  = 4;
    NOEX_MASK       = 6;

    NOEX_UNDEF  = NOEX_NOSUPER;

function rb_compile_cstr (f, s : pchar; len, line : integer) : PRNode;
    cdecl; external 'ruby18';
function rb_compile_string (f : pchar; s : VALUE; line : integer) : PRNode;
    cdecl; external 'ruby18';
function rb_compile_file (f : pchar; _file : VALUE; start : integer) : PRNode;
    cdecl; external 'ruby18';

procedure rb_add_method (
    klass   : VALUE;
    mid     : ID;
    node    : PRNode;
    noex    : integer
);
    cdecl; external 'ruby18';

function rb_node_newnode (_type : node_type; a0, a1, a2 : VALUE) : PRNode;
    cdecl; external 'ruby18';
function rb_method_node (klass : VALUE; id : ID) : PRNode;
    cdecl; external 'ruby18';

function rb_global_entry (id : ID) : pointer; cdecl; external 'ruby18';

function rb_gvar_get (entry : pointer) : VALUE; cdecl; external 'ruby18';
function rb_gvar_set (entry : pointer; val : VALUE) : VALUE;
    cdecl; external 'ruby18';
function rb_gvar_defined (entry : pointer) : VALUE; cdecl; external 'ruby18';

type
    Prb_event_t = ^rb_event_t;
    rb_event_t  = cardinal;

const
    RUBY_EVENT_NONE     = $00;
    RUBY_EVENT_LINE     = $01;
    RUBY_EVENT_CLASS    = $02;
    RUBY_EVENT_END      = $04;
    RUBY_EVENT_CALL     = $08;
    RUBY_EVENT_RETURN   = $10;
    RUBY_EVENT_C_CALL   = $20;
    RUBY_EVENT_C_RETURN = $40;
    RUBY_EVENT_RAISE    = $80;
    RUBY_EVENT_ALL      = $FF;

type
    rb_event_hook_func_t = procedure (
        event   : rb_event_t;
        node    : PRNode;
        _self   : VALUE;
        id      : ID;
        klass   : VALUE
    );
        cdecl;

procedure rb_add_event_hook (func : rb_event_hook_func_t; events : rb_event_t);
    cdecl; external 'ruby18';
function rb_remove_event_hook (func : rb_event_hook_func_t) : integer;
    cdecl; external 'ruby18';
// END node.h

var
    ruby_current_node   : PRNode; cvar; external 'ruby18';

procedure ruby_set_current_source (); cdecl; external 'ruby18';
procedure rb_exc_raise (mesg : VALUE); cdecl; external 'ruby18';
procedure rb_exc_fatal (mesg : VALUE); cdecl; external 'ruby18';

function rb_f_exit (argc : integer; argv : PVALUE) : VALUE;
    cdecl; external 'ruby18';
function rb_f_abort (argc : integer; argv : PVALUE) : VALUE;
    cdecl; external 'ruby18';

procedure rb_remove_method (klass : VALUE; name : pchar);
    cdecl; external 'ruby18';

type
    Palloc_func = function (arg : VALUE) : VALUE; cdecl;

procedure rb_define_alloc_func (klass : VALUE; func : Palloc_func);
    cdecl; external 'ruby18';
procedure rb_undef_alloc_func (klass : VALUE); cdecl; external 'ruby18';

procedure rb_clear_cache (); cdecl; external 'ruby18';
procedure rb_clear_cache_by_class (klass : VALUE); cdecl; external 'ruby18';

procedure rb_alias (klass : VALUE; name, def : ID); cdecl; external 'ruby18';
procedure rb_attr (klass : VALUE; id : ID; r, w, ex : integer);
    cdecl; external 'ruby18';

function rb_method_boundp (klass : VALUE; id : ID; ex : integer) : integer;
    cdecl; external 'ruby18';

function rb_dvar_defined (id : ID) : VALUE; cdecl; external 'ruby18';
function rb_dvar_curr (id : ID) : VALUE; cdecl; external 'ruby18';
function rb_dvar_ref (id : ID) : VALUE; cdecl; external 'ruby18';

procedure rb_dvar_asgn (id : ID; value : VALUE); cdecl; external 'ruby18';
procedure rb_dvar_push (id : ID; value : VALUE); cdecl; external 'ruby18';

function rb_svar (cnt : integer) : PVALUE; cdecl; external 'ruby18';

function rb_eval_cmd (cmd, arg : VALUE; level : integer) : VALUE;
    cdecl; external 'ruby18';

function rb_obj_respond_to (obj : VALUE; id : ID; priv : integer) : integer;
    cdecl; external 'ruby18';
function rb_respond_to (obj : VALUE; id : ID) : integer;
    cdecl; external 'ruby18';

procedure rb_interrupt (); cdecl; external 'ruby18';

function rb_apply (recv : VALUE; mid : ID; args : VALUE) : VALUE;
    cdecl; external 'ruby18';

procedure rb_backtrace (); cdecl; external 'ruby18';

function rb_frame_last_func () : VALUE; cdecl; external 'ruby18';
function rb_frame_this_func () : VALUE; cdecl; external 'ruby18';

function rb_obj_instance_eval (
    argc    : integer;
    argv    : PVALUE;
    _self   : VALUE
) : VALUE;
    cdecl; external 'ruby18';
function rb_mod_module_eval (
    argc    : integer;
    argv    : PVALUE;
    _mod    : VALUE
) : VALUE;
    cdecl; external 'ruby18';

procedure rb_load (fname : VALUE; wrap : integer); cdecl; external 'ruby18';
procedure rb_load_protect (fname : VALUE; wrap : integer; state : PInteger);
    cdecl; external 'ruby18';

procedure rb_jump_tag (tag : integer); cdecl; external 'ruby18';

function rb_provided (feature : pchar) : integer; cdecl; external 'ruby18';
procedure rb_provide (feature : pchar); cdecl; external 'ruby18';

function rb_f_require (obj, fname : VALUE) : VALUE; cdecl; external 'ruby18';
function rb_require_safe (fname : VALUE; safe : integer) : VALUE;
    cdecl; external 'ruby18';

procedure rb_obj_call_init (obj : VALUE; argc : integer; argv : PVALUE);
    cdecl; external 'ruby18';

function rb_class_new_instance (
    argc    : integer;
    argv    : PVALUE;
    klass   : VALUE
) : VALUE;
    cdecl; external 'ruby18';

function rb_block_proc () : VALUE; cdecl; external 'ruby18';
function rb_f_lambda () : VALUE; cdecl; external 'ruby18';

function rb_obj_method (obj, vid : VALUE) : VALUE; cdecl; external 'ruby18';

type
    Pprotect_func   = function (arg : VALUE) : VALUE; cdecl;
function rb_protect (
    proc    : Pprotect_func;
    data    : VALUE;
    state   : PInteger
) : VALUE;
    cdecl; external 'ruby18';

type
    Pend_proc   = procedure (arg : VALUE); cdecl;
procedure rb_set_end_proc (func : Pend_proc; data : VALUE);
    cdecl; external 'ruby18';
procedure rb_mark_end_proc (); cdecl; external 'ruby18';
procedure rb_exec_end_proc (); cdecl; external 'ruby18';
procedure ruby_finalize (); cdecl; external 'ruby18';
procedure ruby_stop (ex : integer); cdecl; external 'ruby18';
function ruby_cleanup (ex : integer) : integer; cdecl; external 'ruby18';
function ruby_exec () : integer; cdecl; external 'ruby18';

function rb_file_s_expand_path (argc : integer; argv : PVALUE) : VALUE;
    cdecl; external 'ruby18';
function rb_file_expand_path (fname, dname : VALUE) : VALUE;
    cdecl; external 'ruby18';
procedure rb_file_const (name : pchar; value : VALUE); cdecl; external 'ruby18';
function rb_find_file_ext (filep : PVALUE; ext : pchar) : integer;
    cdecl; external 'ruby18';
function rb_find_file (path : VALUE) : VALUE; cdecl; external 'ruby18';
function rb_path_next (s : pchar) : pchar; cdecl; external 'ruby18';
function rb_path_skip_prefix (path : pchar) : pchar; cdecl; external 'ruby18';
function rb_path_last_separator (path : pchar) : pchar;
    cdecl; external 'ruby18';
function rb_path_end (path : pchar) : pchar; cdecl; external 'ruby18';

procedure rb_memerror (); cdecl; external 'ruby18';
function ruby_stack_check () : integer; cdecl; external 'ruby18';
function ruby_stack_length (p : PPVALUE) : integer; cdecl; external 'ruby18';

function rb_source_filename (f : pchar) : pchar; cdecl; external 'ruby18';

procedure rb_gc_mark_locations (start, _end : PVALUE); cdecl; external 'ruby18';
procedure rb_mark_tbl (tbl : Pst_table); cdecl; external 'ruby18';
procedure rb_mark_hash (tbl : Pst_table); cdecl; external 'ruby18';
procedure rb_gc_mark_maybe (obj : VALUE); cdecl; external 'ruby18';
procedure rb_gc_mark (ptr : VALUE); cdecl; external 'ruby18';
procedure rb_gc_force_recycle (p : VALUE); cdecl; external 'ruby18';
procedure rb_gc (); cdecl; external 'ruby18';
procedure rb_gc_copy_finalizer (dest, obj : VALUE); cdecl; external 'ruby18';
procedure rb_gc_finalize_deferred (); cdecl; external 'ruby18';
procedure rb_gc_call_finalizer_at_exit (); cdecl; external 'ruby18';
function rb_gc_enable () : VALUE; cdecl; external 'ruby18';
function rb_gc_disable () : VALUE; cdecl; external 'ruby18';
function rb_gc_start () : VALUE; cdecl; external 'ruby18';

type
    Pforeach_func   = function (args : array of const) : integer; cdecl;
procedure st_foreach_safe (
    table   : Pst_table;
    func    : Pforeach_func;
    a       : st_data_t
);
    cdecl; external 'ruby18';
procedure rb_hash_foreach (hash : VALUE; func : Pforeach_func; farg : VALUE);
    cdecl; external 'ruby18';
function rb_hash (obj : VALUE) : VALUE; cdecl; external 'ruby18';
function rb_hash_new () : VALUE; cdecl; external 'ruby18';
function rb_hash_freeze (hash : VALUE) : VALUE; cdecl; external 'ruby18';
function rb_hash_aref (hash, key : VALUE) : VALUE; cdecl; external 'ruby18';
function rb_hash_aset (hash, key, val : VALUE) : VALUE;
    cdecl; external 'ruby18';
function rb_hash_delete_if (hash : VALUE) : VALUE; cdecl; external 'ruby18';
function rb_hash_delete (hash, key : VALUE) : VALUE; cdecl; external 'ruby18';

function rb_path_check (path : pchar) : integer; cdecl; external 'ruby18';
function rb_env_path_tainted () : integer; cdecl; external 'ruby18';

var
    rb_fs           : VALUE; cvar; external 'ruby18';
    rb_output_fs    : VALUE; cvar; external 'ruby18';
    rb_rs           : VALUE; cvar; external 'ruby18';
    rb_default_rs   : VALUE; cvar; external 'ruby18';
    rb_output_rs    : VALUE; cvar; external 'ruby18';

function rb_io_write (io, str : VALUE) : VALUE; cdecl; external 'ruby18';
function rb_io_gets (io : VALUE) : VALUE; cdecl; external 'ruby18';
function rb_io_getc (io : VALUE) : VALUE; cdecl; external 'ruby18';
function rb_io_ungetc (io, c : VALUE) : VALUE; cdecl; external 'ruby18';
function rb_io_close (io : VALUE) : VALUE; cdecl; external 'ruby18';
function rb_io_eof (io : VALUE) : VALUE; cdecl; external 'ruby18';
function rb_io_binmode (io : VALUE) : VALUE; cdecl; external 'ruby18';
function rb_io_addstr (io, str : VALUE) : VALUE; cdecl; external 'ruby18';
function rb_io_printf (argc : integer; argv : PVALUE; _out : VALUE) : VALUE;
    cdecl; external 'ruby18';
function rb_io_print (argc : integer; argv : PVALUE; _out : VALUE) : VALUE;
    cdecl; external 'ruby18';
function rb_io_puts (argc : integer; argv : PVALUE; _out : VALUE) : VALUE;
    cdecl; external 'ruby18';
function rb_file_open (fname, mode : pchar) : VALUE; cdecl; external 'ruby18';
function rb_gets () : VALUE; cdecl; external 'ruby18';
procedure rb_write_error (mesg : pchar); cdecl; external 'ruby18';
procedure rb_write_error2 (mesg : pchar; len : ptrint);
    cdecl; external 'ruby18';

function rb_marshal_dump (obj, port : VALUE) : VALUE; cdecl; external 'ruby18';
function rb_marshal_load (port : VALUE) : VALUE; cdecl; external 'ruby18';

procedure rb_num_zerodiv (); cdecl; external 'ruby18';

function rb_num_coerce_bin (x, y : VALUE) : VALUE; cdecl; external 'ruby18';
function rb_num_coerce_cmp (x, y : VALUE) : VALUE; cdecl; external 'ruby18';
function rb_num_coerce_relop (x, y : VALUE) : VALUE; cdecl; external 'ruby18';
function rb_float_new (d : double) : VALUE; cdecl; external 'ruby18';
function rb_num2fix (val : VALUE) : VALUE; cdecl; external 'ruby18';
function rb_fix2str (x : VALUE; base : integer) : VALUE;
    cdecl; external 'ruby18';
function rb_dbl_cmp (a, b : double) : VALUE; cdecl; external 'ruby18';

function rb_eql (obj1, obj2 : VALUE) : integer; cdecl; external 'ruby18';
function rb_any_to_s (obj : VALUE) : VALUE; cdecl; external 'ruby18';
function rb_inspect (obj : VALUE) : VALUE; cdecl; external 'ruby18';
function rb_obj_is_instance_of (obj, c : VALUE) : VALUE;
    cdecl; external 'ruby18';
function rb_obj_is_kind_of (obj, c : VALUE) : VALUE; cdecl; external 'ruby18';
function rb_obj_alloc (klass : VALUE) : VALUE; cdecl; external 'ruby18';
function rb_obj_clone (obj : VALUE) : VALUE; cdecl; external 'ruby18';
function rb_obj_dup (obj : VALUE) : VALUE; cdecl; external 'ruby18';
function rb_obj_init_copy (obj, orig : VALUE) : VALUE; cdecl; external 'ruby18';
function rb_obj_taint (obj : VALUE) : VALUE; cdecl; external 'ruby18';
function rb_obj_tainted (obj : VALUE) : VALUE; cdecl; external 'ruby18';
function rb_obj_untaint (obj : VALUE) : VALUE; cdecl; external 'ruby18';
function rb_obj_freeze (obj : VALUE) : VALUE; cdecl; external 'ruby18';
function rb_obj_id (obj : VALUE) : VALUE; cdecl; external 'ruby18';
function rb_obj_class (obj : VALUE) : VALUE; cdecl; external 'ruby18';
function rb_class_real (cl : VALUE) : VALUE; cdecl; external 'ruby18';
function rb_class_inherited_p (_mod, arg : VALUE) : VALUE;
    cdecl; external 'ruby18';
function rb_convert_type (
    val     : VALUE;
    _type   : integer;
    tname,
    method  : pchar
) : VALUE;
    cdecl; external 'ruby18';
function rb_check_convert_type (
    val     : VALUE;
    _type   : integer;
    tname,
    method  : pchar
) : VALUE;
    cdecl; external 'ruby18';
function rb_to_int (val : VALUE) : VALUE; cdecl; external 'ruby18';
function rb_Integer (val : VALUE) : VALUE; cdecl; external 'ruby18';
function rb_Float (val : VALUE) : VALUE; cdecl; external 'ruby18';
function rb_String (val : VALUE) : VALUE; cdecl; external 'ruby18';
function rb_Array (val : VALUE) : VALUE; cdecl; external 'ruby18';

function rb_cstr_to_dbl (p : pchar; badcheck : integer) : double;
    cdecl; external 'ruby18';
function rb_str_to_dbl (str : VALUE; badcheck : integer) : double;
    cdecl; external 'ruby18';

var
    ruby_sourceline : integer; cvar; external 'ruby18';
    ruby_sourcefile : pchar; cvar; external 'ruby18';

function ruby_yyparse () : integer; cdecl; external 'ruby18';
function rb_id_attrset (id : ID) : ID; cdecl; external 'ruby18';
procedure rb_parser_append_print (); cdecl; external 'ruby18';
procedure rb_parser_while_loop (chop, split : integer);
    cdecl; external 'ruby18';
function ruby_parser_stack_on_heap () : integer; cdecl; external 'ruby18';
procedure rb_gc_mark_parser (); cdecl; external 'ruby18';

function rb_is_const_id (id : ID) : integer; cdecl; external 'ruby18';
function rb_is_instance_id (id : ID) : integer; cdecl; external 'ruby18';
function rb_is_class_id (id : ID) : integer; cdecl; external 'ruby18';
function rb_is_local_id (id : ID) : integer; cdecl; external 'ruby18';
function rb_is_junk_id (id : ID) : integer; cdecl; external 'ruby18';

function rb_symname_p (name : pchar) : integer; cdecl; external 'ruby18';

function rb_backref_get () : VALUE; cdecl; external 'ruby18';
procedure rb_backref_set (val : VALUE); cdecl; external 'ruby18';
function rb_lastline_get () : VALUE; cdecl; external 'ruby18';
procedure rb_lastline_set (val : VALUE); cdecl; external 'ruby18';

function rb_sym_all_symbols () : VALUE; cdecl; external 'ruby18';

function rb_proc_exec (str : pchar) : integer; cdecl; external 'ruby18';
function rb_f_exec (argc : integer; argv : PVALUE) : VALUE;
    cdecl; external 'ruby18';
function rb_waitpid (pid : integer; st : PInteger; flags : integer) : integer;
    cdecl; external 'ruby18';
procedure rb_syswait (pid : integer); cdecl; external 'ruby18';
function rb_proc_times (obj : VALUE) : VALUE; cdecl; external 'ruby18';
function rb_detach_process (pid : integer) : VALUE; cdecl; external 'ruby18';

function rb_range_new (beg, _end : VALUE; exclude_end : integer) : VALUE;
    cdecl; external 'ruby18';
function rb_range_beg_len (
    range   : VALUE;
    begp,
    lenp    : PPtrInt;
    len     : ptrint;
    err     : integer
) : VALUE;
    cdecl; external 'ruby18';

function rb_memcmp (p1, p2 : pchar; len : ptrint) : integer;
    cdecl; external 'ruby18';
function rb_memcicmp (p1, p2 : pchar; len : ptrint) : integer;
    cdecl; external 'ruby18';

function rb_memsearch(
    x0  : pointer;
    m   : ptrint;
    y0  : pointer;
    n   : ptrint
) : ptrint;
    cdecl; external 'ruby18';

function rb_reg_nth_defined (nth : integer; match : VALUE) : VALUE;
    cdecl; external 'ruby18';
function rb_reg_nth_match (nth : integer; match : VALUE) : VALUE;
    cdecl; external 'ruby18';
function rb_reg_last_match (match : VALUE) : VALUE; cdecl; external 'ruby18';
function rb_reg_match_pre (match : VALUE) : VALUE; cdecl; external 'ruby18';
function rb_reg_match_post (match : VALUE) : VALUE; cdecl; external 'ruby18';
function rb_reg_match_last (match : VALUE) : VALUE; cdecl; external 'ruby18';
function rb_reg_new (s : pchar; len : ptrint; options : integer) : VALUE;
    cdecl; external 'ruby18';
function rb_reg_match (re, str : VALUE) : VALUE; cdecl; external 'ruby18';
function rb_reg_match2 (re : VALUE) : VALUE; cdecl; external 'ruby18';
function rb_reg_options (re : VALUE) : integer; cdecl; external 'ruby18';

procedure rb_set_kcode (code : pchar); cdecl; external 'ruby18';
function rb_get_kcode () : pchar; cdecl; external 'ruby18';

var
    rb_argv     : VALUE; cvar; external 'ruby18';
    rb_argv0    : VALUE; cvar; external 'ruby18';

procedure rb_load_file (fname : pchar); cdecl; external 'ruby18';
procedure ruby_script (name : pchar); cdecl; external 'ruby18';
procedure ruby_prog_init (); cdecl; external 'ruby18';
procedure ruby_set_argv (argc : integer; argv : PPChar);
    cdecl; external 'ruby18';
procedure ruby_process_options (argc : integer; argv : PPChar);
    cdecl; external 'ruby18';
procedure ruby_load_script (); cdecl; external 'ruby18';
procedure ruby_init_loadpath (); cdecl; external 'ruby18';
procedure ruby_incpush (path : pchar); cdecl; external 'ruby18';

function rb_f_kill (argc : integer; argv : PVALUE) : VALUE;
    cdecl; external 'ruby18';
procedure rb_gc_mark_trap_list (); cdecl; external 'ruby18';

procedure rb_trap_exit (); cdecl; external 'ruby18';
procedure rb_trap_exec (); cdecl; external 'ruby18';
function ruby_signal_name (no : integer) : pchar; cdecl; external 'ruby18';

function rb_f_sprintf (argc : integer; argv : PVALUE) : VALUE;
    cdecl; external 'ruby18';

function rb_str_new (ptr : pchar; len : ptrint) : VALUE;
    cdecl; external 'ruby18';
function rb_str_new2 (ptr : pchar) : VALUE; cdecl; external 'ruby18';
function rb_str_new3 (str : VALUE) : VALUE; cdecl; external 'ruby18';
function rb_str_new4 (orig : VALUE) : VALUE; cdecl; external 'ruby18';
function rb_str_new5 (obj : VALUE; ptr : pchar; len : ptrint) : VALUE;
    cdecl; external 'ruby18';
function rb_tainted_str_new (ptr : pchar; len : ptrint) : VALUE;
    cdecl; external 'ruby18';
function rb_tainted_str_new2 (ptr : pchar) : VALUE; cdecl; external 'ruby18';
function rb_str_buf_new (capa : ptrint) : VALUE; cdecl; external 'ruby18';
function rb_str_buf_new2 (ptr : pchar) : VALUE; cdecl; external 'ruby18';
function rb_str_buf_append (str, str2 : VALUE) : VALUE;
    cdecl; external 'ruby18';
function rb_str_buf_cat (str : VALUE; ptr : pchar; len : ptrint) : VALUE;
    cdecl; external 'ruby18';
function rb_str_buf_cat2 (str : VALUE; ptr : pchar) : VALUE;
    cdecl; external 'ruby18';
function rb_obj_as_string (obj : VALUE) : VALUE; cdecl; external 'ruby18';
function rb_check_string_type (str : VALUE) : VALUE; cdecl; external 'ruby18';
function rb_str_dup (str : VALUE) : VALUE; cdecl; external 'ruby18';
function rb_str_locktmp (str : VALUE) : VALUE; cdecl; external 'ruby18';
function rb_str_unlocktmp (str : VALUE) : VALUE; cdecl; external 'ruby18';
function rb_str_dup_frozen (str : VALUE) : VALUE; cdecl; external 'ruby18';
function rb_str_plus (str1, str2 : VALUE) : VALUE; cdecl; external 'ruby18';
function rb_str_times (str, times : VALUE) : VALUE; cdecl; external 'ruby18';
function rb_str_substr (str : VALUE; beg, len : ptrint) : VALUE;
    cdecl; external 'ruby18';
procedure rb_str_modify (str : VALUE); cdecl; external 'ruby18';
function rb_str_freeze (str : VALUE) : VALUE; cdecl; external 'ruby18';
function rb_str_resize (str : VALUE; len : ptrint) : VALUE;
    cdecl; external 'ruby18';
function rb_str_cat (str : VALUE; ptr : pchar; len : ptrint) : VALUE;
    cdecl; external 'ruby18';
function rb_str_cat2 (str : VALUE; ptr : pchar) : VALUE;
    cdecl; external 'ruby18';
function rb_str_append (str, str2 : VALUE) : VALUE; cdecl; external 'ruby18';
function rb_str_concat (str1, str2 : VALUE) : VALUE; cdecl; external 'ruby18';
function rb_str_hash (str : VALUE) : integer; cdecl; external 'ruby18';
function rb_str_cmp (str1, str2 : VALUE) : integer; cdecl; external 'ruby18';
function rb_str_upto (beg, _end : VALUE; excl : integer) : VALUE;
    cdecl; external 'ruby18';
procedure rb_str_update (str : VALUE; beg, len : ptrint; val : VALUE);
    cdecl; external 'ruby18';
function rb_str_inspect (str : VALUE) : VALUE; cdecl; external 'ruby18';
function rb_str_dump (str : VALUE) : VALUE; cdecl; external 'ruby18';
function rb_str_split (str : VALUE; sep0 : pchar) : VALUE;
    cdecl; external 'ruby18';
procedure rb_str_associate (str, add : VALUE); cdecl; external 'ruby18';
function rb_str_associated (str : VALUE) : VALUE; cdecl; external 'ruby18';
procedure rb_str_setter (val : VALUE; id : ID; v : PVALUE);
    cdecl; external 'ruby18';
function rb_str_intern (s : VALUE) : VALUE; cdecl; external 'ruby18';

function rb_struct_new (klass : VALUE) : VALUE; varargs;
    cdecl; external 'ruby18';
function rb_struct_define (name : pchar) : VALUE; varargs;
    cdecl; external 'ruby18';
function rb_struct_alloc (klass, values : VALUE) : VALUE;
    cdecl; external 'ruby18';
function rb_struct_aref (s, idx : VALUE) : VALUE; cdecl; external 'ruby18';
function rb_struct_aset (s, idx, val : VALUE) : VALUE; cdecl; external 'ruby18';
function rb_struct_getmember (obj : VALUE; id : ID) : VALUE;
    cdecl; external 'ruby18';
function rb_struct_iv_get (c : VALUE; name : pchar) : VALUE;
    cdecl; external 'ruby18';
function rb_struct_s_members (klass : VALUE) : VALUE; cdecl; external 'ruby18';
function rb_struct_members (s : VALUE) : VALUE; cdecl; external 'ruby18';

type
    Ptime_t = ^time_t;
    time_t  = ptruint; // ???

function rb_time_new (sec, usec : time_t) : VALUE; cdecl; external 'ruby18';

function rb_mod_name (m : VALUE) : VALUE; cdecl; external 'ruby18';
function rb_class_path (klass : VALUE) : VALUE; cdecl; external 'ruby18';
procedure rb_set_class_path (klass, under : VALUE; name : pchar);
    cdecl; external 'ruby18';
function rb_path2class (path : pchar) : VALUE; cdecl; external 'ruby18';
procedure rb_name_class (klass : VALUE; id : ID); cdecl; external 'ruby18';
function rb_class_name (klass : VALUE) : VALUE; cdecl; external 'ruby18';
procedure rb_autoload (m : VALUE; id : ID; _file : pchar);
    cdecl; external 'ruby18';
function rb_autoload_load (klass : VALUE; id : ID) : VALUE;
    cdecl; external 'ruby18';
function rb_autoload_p (m : VALUE; id : ID) : VALUE; cdecl; external 'ruby18';

procedure rb_gc_mark_global_tbl (); cdecl; external 'ruby18';

function rb_f_trace_var (argc : integer; argv : PVALUE) : VALUE;
    cdecl; external 'ruby18';
function rb_f_untrace_var (argc : integer; argv : PVALUE) : VALUE;
    cdecl; external 'ruby18';
function rb_f_global_variables () : VALUE; cdecl; external 'ruby18';
procedure rb_alias_variable (name1, name2 : ID); cdecl; external 'ruby18';
function rb_generic_ivar_table (obj : VALUE) : Pst_table;
    cdecl; external 'ruby18';
procedure rb_copy_generic_ivar (clone, obj : VALUE); cdecl; external 'ruby18';
procedure rb_mark_generic_ivar (obj : VALUE); cdecl; external 'ruby18';
procedure rb_mark_generic_ivar_tbl (); cdecl; external 'ruby18';
procedure rb_free_generic_ivar (obj : VALUE); cdecl; external 'ruby18';
function rb_ivar_get (obj : VALUE; id : ID) : VALUE; cdecl; external 'ruby18';
function rb_ivar_set (obj : VALUE; id : ID; val : VALUE) : VALUE;
    cdecl; external 'ruby18';
function rb_ivar_defined (obj : VALUE; id : ID) : VALUE;
    cdecl; external 'ruby18';
//function rb_iv_set (obj : VALUE; name : pchar; val : VALUE) : VALUE;
//  cdecl; external 'ruby18';
//function rb_iv_get (obj : VALUE; name : pchar) : VALUE;
//  cdecl; external 'ruby18';
function rb_attr_get (obj : VALUE; id : VALUE) : VALUE;
    cdecl; external 'ruby18';
function rb_obj_instance_variables (obj : VALUE) : VALUE;
    cdecl; external 'ruby18';
function rb_obj_remove_instance_variable (obj, name : VALUE) : VALUE;
    cdecl; external 'ruby18';
function rb_mod_const_at (m : VALUE; data : pointer) : pointer;
    cdecl; external 'ruby18';
function rb_mod_const_of (m : VALUE; data : pointer) : pointer;
    cdecl; external 'ruby18';
function rb_const_list (data : pointer) : VALUE; cdecl; external 'ruby18';
function rb_mod_constants (m : VALUE) : VALUE; cdecl; external 'ruby18';
function rb_mod_remove_const (m, name : VALUE) : VALUE;
    cdecl; external 'ruby18';
function rb_const_defined (klass : VALUE; id : ID) : integer;
    cdecl; external 'ruby18';
function rb_const_defined_at (klass : VALUE; id : ID) : integer;
    cdecl; external 'ruby18';
function rb_const_defined_from (klass : VALUE; id : ID) : integer;
    cdecl; external 'ruby18';
function rb_const_get (klass : VALUE; id : ID) : VALUE;
    cdecl; external 'ruby18';
function rb_const_get_at (klass : VALUE; id : ID) : VALUE;
    cdecl; external 'ruby18';
function rb_const_get_from (klass : VALUE; id : ID) : VALUE;
    cdecl; external 'ruby18';
procedure rb_const_set (klass : VALUE; id : ID; val : VALUE);
    cdecl; external 'ruby18';
// function rb_mod_constants (m : VALUE) : VALUE; cdecl; external 'ruby18';
function rb_mod_const_missing (klass, name : VALUE) : VALUE;
    cdecl; external 'ruby18';
function rb_cvar_defined (klass : VALUE; id : ID) : VALUE;
    cdecl; external 'ruby18';

const
    RB_CVAR_SET_4ARGS   = 1;

procedure rb_cvar_set (klass : VALUE; id : ID; val : VALUE; warn : integer);
    cdecl; external 'ruby18';
function rb_cvar_get (klass : VALUE; id : ID) : VALUE; cdecl; external 'ruby18';
procedure rb_cv_set (klass : VALUE; name : pchar; val : VALUE);
    cdecl; external 'ruby18';
function rb_cv_get (klass : VALUE; name : pchar) : VALUE;
    cdecl; external 'ruby18';
procedure rb_define_class_variable (klass : VALUE; name : pchar; val : VALUE);
    cdecl; external 'ruby18';
function rb_mod_class_variables (obj : VALUE) : VALUE; cdecl; external 'ruby18';
function rb_mod_remove_cvar (m, name : VALUE) : VALUE; cdecl; external 'ruby18';

procedure ruby_show_version (); cdecl; external 'ruby18';
procedure ruby_show_copyright (); cdecl; external 'ruby18';

function Data_Wrap_Struct (klass : VALUE; mark, free : RUBY_DATA_FUNC; data : pointer) : VALUE; inline;

implementation

function rb_class_of (obj : VALUE) : VALUE; inline;
    begin
    if (obj.data and FIXNUM_FLAG) <> 0 then
        result := rb_cFixnum
    else
        case obj.data of
            _Qnil :
                result := rb_cNilClass;
            _Qfalse :
                result := rb_cFalseClass;
            _Qtrue :
                result := rb_cTrueClass
        else
            if (obj.data and $FF) = $0E then
                result := rb_cSymbol
            else
                result := PRBasic(obj)^.klass
        end;
    end;

function rb_type (obj : VALUE) : integer; inline;
    begin
    if (obj.data and FIXNUM_FLAG) <> 0 then
        result := T_FIXNUM
    else
        case obj.data of
            _Qnil :
                result := T_NIL;
            _Qfalse :
                result := T_FALSE;
            _Qtrue :
                result := T_TRUE;
            _Qundef :
                result := T_UNDEF
        else
            if (obj.data and $FF) = $0E then
                result := T_SYMBOL
            else
                result := (PRBasic(obj)^.flags and T_MASK)
        end;
    end;

function Data_Wrap_Struct (klass : VALUE; mark, free : RUBY_DATA_FUNC; data : pointer) : VALUE; inline;
 begin
 result := rb_data_object_alloc(klass, data, mark, free)
 end;

operator = (a, b : VALUE) : boolean; inline;
 begin
 result := (a.data = b.data)
 end;

operator = (a, b : ID) : boolean; inline;
 begin
 result := (a.data = b.data)
 end;

end.
