operator = (x, y : VALUE) : Boolean; inline;
 begin
  Result := (x.value = y.value);
 end;

operator = (x, y : ID) : Boolean; inline;
 begin
  Result := (x.id = y.id);
 end;

type
  RBasic = record
    flags, klass : VALUE;
  end;
  PRBasic = ^RBasic;
  RData = record
    basic : RBasic;
    dmark, dfree : RUBY_DATA_FUNC;
    data : Pointer;
  end;
  PRData = ^RData;

function rb_type (obj : VALUE) : ruby_value_type; inline;
 begin
  if IMMEDIATE_P(obj)
     then begin
           if FIXNUM_P(obj)
              then exit(T_FIXNUM);
           if FLONUM_P(obj)
              then exit(T_FLOAT);
           if obj = Qtrue
              then exit(T_TRUE);
           if SYMBOL_P(obj)
              then exit(T_SYMBOL);
           if obj = Qundef
              then exit(T_UNDEF);
          end
     else if not RTEST(obj)
             then begin
                   if obj = Qfalse
                      then exit(T_FALSE);
                   if obj = Qnil
                      then exit(T_NIL);
                  end;
  Result := BUILTIN_TYPE(obj);
 end;

function IMMEDIATE_P (obj : VALUE) : Boolean; inline;
 begin
  Result := Boolean(obj.value and IMMEDIATE_MASK);
 end;

function FIXNUM_P (obj : VALUE) : Boolean; inline;
 begin
  Result := Boolean(obj.value and FIXNUM_FLAG);
 end;

function FLONUM_P (obj : VALUE) : Boolean; inline;
 begin
  Result := (obj.value and FLONUM_MASK) = FLONUM_FLAG;
 end;

function SYMBOL_P (obj : VALUE) : Boolean; inline;
 begin
  Result := (obj.value and not ((not PtrUInt(0)) shl SPECIAL_SHIFT)) =
                                                                    SYMBOL_FLAG;
 end;

function RTEST (obj : VALUE) : Boolean; inline;
 begin
  Result := not ((obj.value and not Qnil.value) = 0);
 end;

function NIL_P (obj : VALUE) : Boolean; inline;
 begin
  Result := obj = Qnil;
 end;

function BUILTIN_TYPE (obj : VALUE) : ruby_value_type; inline;
 begin
  Result := ruby_value_type(Integer(PRBasic(obj)^.flags.value and
                                                              PtrUInt(T_MASK)));
 end;

function ID2SYM (id : ID) : VALUE; inline;
 begin
  Result.value := (PtrUInt(id) shl SPECIAL_SHIFT) or SYMBOL_FLAG;
 end;

function SYM2ID (sym : VALUE) : ID; inline;
 begin
  Result.id := sym.value shr SPECIAL_SHIFT;
 end;

function pp_data_object (obj : VALUE) : Pointer; inline;
 begin
  rb_check_type(obj, T_DATA);
  Result := PRData(obj)^.data;
 end;


