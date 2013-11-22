operator = (x, y : VALUE) : Boolean; inline;
operator = (x, y : ID) : Boolean; inline;

function rb_type (obj : VALUE) : ruby_value_type; inline;
function IMMEDIATE_P (obj : VALUE) : Boolean; inline;
function FIXNUM_P (obj : VALUE) : Boolean; inline;
function FLONUM_P (obj : VALUE) : Boolean; inline;
function SYMBOL_P (obj : VALUE) : Boolean; inline;
function RTEST (obj : VALUE) : Boolean; inline;
function NIL_P (obj : VALUE) : Boolean; inline;
function BUILTIN_TYPE (obj : VALUE) : ruby_value_type; inline;

function ID2SYM (id : ID) : VALUE; inline;
function SYM2ID (sym : VALUE) : ID; inline;

function pp_data_object (obj : VALUE) : Pointer; inline;


