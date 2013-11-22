type
  rb_event_flag_t = type culong;
  rb_event_hook_func_t = procedure (evflag : rb_event_flag_t; data, slf : VALUE;
                                                mid : ID; klass : VALUE); cdecl;
  RUBY_DATA_FUNC = procedure (p : Pointer); cdecl;
  rb_gvar_getter_t = function (id : ID; data, gvar : Pointer) : VALUE; cdecl;
  rb_gvar_setter_t = procedure (val : VALUE; id : ID; data, gvar : Pointer);
                                                                          cdecl;
  rb_econv_result_t = (
    econv_invalid_byte_sequence,
    econv_undefined_conversion,
    econv_destination_buffer_full,
    econv_source_buffer_empty,
    econv_finished,
    econv_after_output,
    econv_incomplete_input
  );
  time_t = clong;
  timeval = record
    tv_sec : time_t;
    tv_usec : clong;
  end;
  ptimeval = ^timeval;
  timespec = record
    tv_sec : time_t;
    tv_nsec : clong;
  end;
  rb_alloc_func_t = function (v : VALUE) : VALUE; cdecl;
  rb_foreach_func = function (a, b, c : VALUE) : cint; cdecl;
  st_index_t = culong;
  rb_hash_update_func = function (newkey, oldkey, value : VALUE) : VALUE; cdecl;
  iseq_type = (
    ISEQ_TYPE_TOP,
    ISEQ_TYPE_METHOD,
    ISEQ_TYPE_BLOCK,
    ISEQ_TYPE_CLASS,
    ISEQ_TYPE_RESCUE,
    ISEQ_TYPE_ENSURE,
    ISEQ_TYPE_EVAL,
    ISEQ_TYPE_MAIN,
    ISEQ_TYPE_DEFINED_GUARD
  );
  each_obj_callback = function (pstart, pend : Pointer; size : csize_t;
                                                  data : Pointer) : cint; cdecl;
  rb_blocking_function_t = function (data : Pointer) : VALUE; cdecl;
  rb_unblock_function_t = procedure (data : Pointer); cdecl;
  ruby_glob_func = function (path : PChar; arg : VALUE; enc : Pointer) : cint;
                                                                          cdecl;
  rb_debug_inspector_func_t = function (dc, data : Pointer) : VALUE; cdecl;
  ruby_value_type = (
    T_NONE   = $00,

    T_OBJECT = $01,
    T_CLASS  = $02,
    T_MODULE = $03,
    T_FLOAT  = $04,
    T_STRING = $05,
    T_REGEXP = $06,
    T_ARRAY  = $07,
    T_HASH   = $08,
    T_STRUCT = $09,
    T_BIGNUM = $0a,
    T_FILE   = $0b,
    T_DATA   = $0c,
    T_MATCH  = $0d,
    T_COMPLEX  = $0e,
    T_RATIONAL = $0f,

    T_NIL    = $11,
    T_TRUE   = $12,
    T_FALSE  = $13,
    T_SYMBOL = $14,
    T_FIXNUM = $15,

    T_UNDEF  = $1b,
    T_NODE   = $1c,
    T_ICLASS = $1d,
    T_ZOMBIE = $1e,

    T_MASK   = $1f
  );

  p_catch_func = function () : VALUE; cdecl;
  p_func = function (v : VALUE) : VALUE; cdecl;
  p_recursive_func = function (obj, arg : VALUE; recursive : cint) : VALUE;
                                                                          cdecl;
  p_chfunc = function (arg : Pointer) : cint; cdecl;
  p_chfunc_msg = function (arg : Pointer; msg : PChar; len : csize_t) : cint;
                                                                          cdecl;
  p_objlen_func = function (obj : VALUE; len : clong) : VALUE; cdecl;
  p_glob_func = procedure (path : PChar; arg : VALUE; enc : Pointer); cdecl;
  p_dumper = function (obj : VALUE) : VALUE; cdecl;
  p_loader = function (obj, data : VALUE) : VALUE; cdecl;
  p_end_proc = procedure (v : VALUE); cdecl;
  p_gvl_func = function (data : Pointer) : Pointer; cdecl;
  p_each_words_func = procedure (str : PChar; len : cint; arg : Pointer); cdecl;
  p_init_ext_func = procedure; cdecl;
  p_cmp_func = function (x, y, d : Pointer) : cint; cdecl;
  p_at_exit = procedure (vm : Pointer); cdecl;
  p_double_constructor = function (d : cdouble) : VALUE; cdecl;
  p_iseq_line_trace_func = function (line : cint; events_ptr : pculong;
                                                     d : Pointer) : cint; cdecl;
  p_reachable_objects_func = procedure (v : VALUE; p : Pointer); cdecl;


