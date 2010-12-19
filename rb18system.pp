(*
    License: GNU General Public License
*)

{$codepage utf8}
{$mode objfpc}{$h+}
{$packrecords C}
{$smartlink on}
{$typeinfo on}

unit rb18System;

interface

uses
  SysUtils, TypInfo,
  ruby18dynamic;

function getActive : boolean; inline;
procedure setActive (Val : boolean);

property Active : boolean read getActive write setActive;
procedure Initialize;
procedure Finalize;

function ValueToInt    (v : VALUE) : ptrint;
function ValueToUInt   (v : VALUE) : ptruint;
function ValueToDouble (v : VALUE) : double;
function ValueToBool   (v : VALUE) : boolean;
function ValueToStr    (v : VALUE) : utf8string;
function ValueToUStr   (v : VALUE) : unicodestring;

function    IntToValue (      v : ptrint       ) : VALUE;
function   UIntToValue (      v : ptruint      ) : VALUE;
function DoubleToValue (const v : double       ) : VALUE;
function   BoolToValue (      v : boolean      ) : VALUE;
function    StrToValue (const v : utf8string   ) : VALUE;
function   UStrToValue (const v : unicodestring) : VALUE;

{$IFDEF CPU32}
function ValueToInt64  (v : VALUE) : int64;
function ValueToUInt64 (v : VALUE) : qword;
function  Int64ToValue (const v : int64) : VALUE;
function UInt64ToValue (const v : qword) : VALUE;
{$ENDIF CPU32}

function ValueToId (v : VALUE) : ID;
function IdToValue (id : ID) : VALUE; inline;

function IdToStr (id : ID) : utf8string;
function StrToId (const v : utf8string) : ID;

function ValueToClass  (v : VALUE) : TClass;
function ValueToObject (v : VALUE) : TObject;
function  ClassToValue (cls : TClass ) : VALUE;
function ObjectToValue (obj : TObject) : VALUE;

type
  TRubyArgs = array of VALUE;
  TRubyMsg = record
    Msg : shortstring;
    Args : TRubyArgs;
    Return : VALUE;
  end;

type
  ERubyInactive = class(ERubyError)end;
  ERubyConversion = class(ERubyError)end;

resourcestring
  msgRubyInactive =
    'Ruby binding is inactive.';
  msgRubyConversion =
    'Error while Ruby <-> Pascal conversion.';
  msgRubyNotClass =
    '<%s> is not a Class.';
  msgRubyNotPascalClass =
    '<%s> is not a Pascal Class.';
  msgRubyNotPascalObject =
    '<%s> is not a Pascal object.';
  msgRubyNilClass =
    'Nil class value is not allowed.';

operator := (v : VALUE) : ptrint; inline;
operator := (v : VALUE) : ptruint; inline;
operator := (v : VALUE) : double; inline;
operator := (v : VALUE) : boolean; inline;
operator := (v : VALUE) : utf8string; inline;
operator := (v : VALUE) : unicodestring; inline;

operator := (v : ptrint) : VALUE; inline;
operator := (v : ptruint) : VALUE; inline;
operator := (const v : double) : VALUE; inline;
operator := (v : boolean) : VALUE; inline;
operator := (const v : utf8string) : VALUE; inline;
operator := (const v : unicodestring) : VALUE; inline;

{$IFDEF CPU32}
operator := (v : VALUE) : int64; inline;
operator := (v : VALUE) : qword; inline;
operator := (const v : int64) : VALUE; inline;
operator := (const v : qword) : VALUE; inline;
{$ENDIF CPU32}

operator := (v : VALUE) : ID; inline;
operator := (v : ID) : VALUE; inline;

operator := (v : ID) : utf8string; inline;
operator := (const v : utf8string) : ID; inline;

operator := (v : VALUE) : TClass; inline;
operator := (v : VALUE) : TObject; inline;
operator := (v : TClass) : VALUE; inline;
operator := (v : TObject) : VALUE; inline;

procedure RegisterObject (obj : TObject; v : VALUE);

type
  TRubyClassHook = procedure (rb_class : VALUE);

procedure AddClassHook (cls : TClass; hook : TRubyClassHook);
procedure DelClassHook (cls : TClass; hook : TRubyClassHook);

type
  TRubyHook = procedure;

procedure AddInitHook (hook : TRubyHook);
procedure DelInitHook (hook : TRubyHook);

implementation

var
  scriptName : utf8string;
  clsCache : array of record
    rb_class : VALUE;
    pp_class : TClass;
  end = nil;
  objCache : array of record
    rb_object : VALUE;
    pp_object : TObject;
  end = nil;
  clsHooks : array of record
    cls : TClass;
    hooks : array of TRubyClassHook;
  end;
  hooksActivate : array of TRubyHook = nil;

procedure AddClassHook (cls : TClass; hook : TRubyClassHook);
 var
   clsidx : ptrint;
 begin
 for clsidx := 0 to high(clsHooks) do
     if clsHooks[clsidx].cls = cls
        then begin
             setLength(clsHooks[clsidx].hooks, length(clsHooks[clsidx].hooks) + 1);
             clsHooks[clsidx].hooks[high(clsHooks[clsidx].hooks)] := hook;
             exit;
             end;
 setLength(clsHooks, length(clsHooks) + 1);
 clsHooks[high(clsHooks)].cls := cls;
 setLength(clsHooks[high(clsHooks)].hooks, 1);
 clsHooks[high(clsHooks)].hooks[0] := hook;
 end;

procedure DelClassHook (cls : TClass; hook : TRubyClassHook);
 var
   clsidx, hookidx, delidx : ptrint;
 begin
 for clsidx := 0 to high(clsHooks) do
     if clsHooks[clsidx].cls = cls
        then for hookidx := 0 to high(clsHooks[clsidx].hooks) do
                 if clsHooks[clsidx].hooks[hookidx] = hook
                    then begin
                         for delidx := hookidx to high(clsHooks[clsidx].hooks) - 1 do
                             clsHooks[clsidx].hooks[delidx] := clsHooks[clsidx].hooks[delidx + 1];
                         setLength(clsHooks[clsidx].hooks, length(clsHooks[clsidx].hooks) - 1);
                         end;
 end;

procedure AddInitHook (hook : TRubyHook);
 var
   len : ptrint;
 begin
 if hook <> nil
    then begin
         len := length(hooksActivate);
         setLength(hooksActivate, len + 1);
         hooksActivate[len] := hook
         end;
 end;

procedure DelInitHook (hook : TRubyHook);
 var
   idx, delidx : ptrint;
 begin
 if hook <> nil
    then begin
         for idx := 0 to high(hooksActivate) do
             if hooksActivate[idx] = hook
                then begin
                     for delidx := idx to high(hooksActivate) - 1 do
                         hooksActivate[delidx] := hooksActivate[delidx + 1];
                     setLength(hooksActivate, length(hooksActivate) - 1)
                     end;
         end;
 end;

function do_find_object (obj : TObject; out idx : ptrint) : boolean;
 var
   left, right : ptrint;
 begin
 idx := 0;
 if length(objCache) = 0
    then exit(false);
 if ptrint(obj) < ptrint(objCache[idx].pp_object)
    then exit(false);
 if ptrint(obj) = ptrint(objCache[idx].pp_object)
    then exit(true);
 idx := length(objCache);
 if ptrint(obj) > ptrint(objCache[idx - 1].pp_object)
    then exit(false);
 idx := idx - 1;
 if ptrint(obj) = ptrint(objCache[idx].pp_object)
    then exit(true);
 left := 0;
 right := idx;
 repeat
   idx := (left + right) div 2;
   if ptrint(obj) = ptrint(objCache[idx].pp_object)
      then exit(true);
   if ptrint(obj) < ptrint(objCache[idx].pp_object)
      then right := idx
      else left := idx
 until right - left <= 1;
 idx := right;
 result := false;
 end;

procedure do_insert_object (obj : TObject; v : VALUE; idx : ptrint);
 var
   len, wrk : ptrint;
 begin
 len := length(objCache);
 setLength(objCache, len + 1);
 for wrk := len downto idx + 1 do
     objCache[wrk] := objCache[wrk - 1];
 objCache[idx].pp_object := obj;
 objCache[idx].rb_object := v;
 end;

procedure do_init;
 var
   idx : ptrint;
 begin
 LoadRuby();
 setLength(objCache, 0);
 setLength(clsCache, 0);
 for idx := 0 to high(hooksActivate) do
     if hooksActivate[idx] <> nil
        then hooksActivate[idx]();
 end;

procedure do_done;
 begin
 UnloadRuby;
 setLength(objCache, 0);
 setLength(clsCache, 0);
 end;

procedure do_check_active; inline;
 begin
 if not isRubyLoaded
    then raise ERubyInactive.Create(msgRubyInactive);
 end;

procedure do_check_conversion (res : integer); inline;
 begin
 if res <> 0
    then raise ERubyConversion.Create(msgRubyConversion);
 end;

function do_inspect (v : VALUE) : utf8string; inline;
 begin
 result := ValueToStr(rb_inspect(v));
 end;

function do_classname (cls : TClass) : ansistring;
 begin
 result := cls.ClassName;
 end;

function do_args (argc : integer; argv : PVALUE) : TRubyArgs;
 var
   idx : integer;
 begin
 setLength(result, argc);
 if argc > 0
    then for idx := 0 to argc - 1 do
             result[idx] := argv[idx]
 end;

function getActive : boolean; inline;
 begin
 result := isRubyLoaded
 end;

procedure setActive (Val : boolean);
 begin
 if not (Val = isRubyLoaded)
    then if Val
            then do_init
            else do_done;
 end;

procedure Initialize;
 begin
 if not isRubyLoaded
    then do_init;
 end;

procedure Finalize;
 begin
 if isRubyLoaded
    then do_done;
 end;

type
  PRecInt = ^TRecInt;
  TRecInt = record
    ptrint : ptrint;
    value : VALUE;
  end;

function try_valuetoint (v : VALUE) : VALUE; cdecl;
 begin
 PRecInt(v)^.ptrint := rb_num2long(PRecInt(v)^.value);
 result := Qnil
 end;

function ValueToInt (v : VALUE) : ptrint;
 var
   rec : TRecInt;
   res : integer;
 begin
 do_check_active;
 rec.value := v;
 rb_protect(@try_valuetoint, VALUE(@rec), @res);
 do_check_conversion(res);
 result := rec.ptrint;
 end;

type
  PRecUInt = ^TRecUInt;
  TRecUInt = record
    ptruint : ptruint;
    value : VALUE;
  end;

function try_valuetouint (v : VALUE) : VALUE; cdecl;
 begin
 PRecUInt(v)^.ptruint := rb_num2ulong(PRecUInt(v)^.value);
 result := Qnil
 end;

function ValueToUInt (v : VALUE) : ptruint;
 var
   rec : TRecUInt;
   res : integer;
 begin
 do_check_active;
 rec.value := v;
 rb_protect(@try_valuetouint,VALUE(@rec),@res);
 do_check_conversion(res);
 result := rec.ptruint;
 end;

type
  PRecDouble = ^TRecDouble;
  TRecDouble = record
    double : double;
    value : VALUE;
  end;

function try_valuetodouble (v : VALUE) : VALUE; cdecl;
 begin
 PRecDouble(v)^.double := rb_num2dbl(PRecDouble(v)^.value);
 result := Qnil
 end;

function ValueToDouble (v : VALUE) : double;
 var
   rec : TRecDouble;
   res : integer;
 begin
 do_check_active;
 rec.value := v;
 rb_protect(@try_valuetodouble,VALUE(@rec),@res);
 do_check_conversion(res);
 result := rec.double;
 end;

function ValueToBool (v : VALUE) : boolean;
 begin
 result := not ((v = Qfalse) or (v = Qnil))
 end;

type
  PRecStr = ^TRecStr;
  TRecStr = record
    pchar : pchar;
    value : VALUE;
  end;

function try_valuetostr (v : VALUE) : VALUE; cdecl;
 begin
 PRecStr(v)^.pchar := rb_string_value_cstr(PRecStr(v)^.value);
 result := Qnil
 end;

function ValueToStr (v : VALUE) : utf8string;
 var
   rec : TRecStr;
   res : integer;
 begin
 do_check_active;
 rec.value := v;
 rb_protect(@try_valuetostr,VALUE(@rec),@res);
 do_check_conversion(res);
 result := utf8string(rec.pchar) + '';
 end;

function ValueToUStr (v : VALUE) : unicodestring;
 begin
 result := ValueToStr(v);
 end;

function try_inttovalue (v : VALUE) : VALUE; cdecl;
 begin
 PRecInt(v)^.value := rb_int2inum(PRecInt(v)^.ptrint);
 result := Qnil;
 end;

function IntToValue (v : ptrint) : VALUE;
 var
   rec : TRecInt;
   res : integer;
 begin
 do_check_active;
 rec.ptrint := v;
 rb_protect(@try_inttovalue,VALUE(@rec),@res);
 do_check_conversion(res);
 result := rec.value;
 end;

function try_uinttovalue (v : VALUE) : VALUE; cdecl;
 begin
 PRecUInt(v)^.value := rb_uint2inum(PRecUInt(v)^.ptruint);
 result := Qnil;
 end;

function UIntToValue (v : ptruint) : VALUE;
 var
   rec : TRecUInt;
   res : integer;
 begin
 do_check_active;
 rec.ptruint := v;
 rb_protect(@try_uinttovalue,VALUE(@rec),@res);
 do_check_conversion(res);
 result := rec.value;
 end;

function try_doubletovalue (v : VALUE) : VALUE; cdecl;
 begin
 PRecDouble(v)^.value := rb_float_new(PRecDouble(v)^.double);
 result := Qnil;
 end;

function DoubleToValue (const v : double) : VALUE;
 var
   rec : TRecDouble;
   res : integer;
 begin
 do_check_active;
 rec.double := v;
 rb_protect(@try_doubletovalue,VALUE(@rec),@res);
 do_check_conversion(res);
 result := rec.value;
 end;

function BoolToValue (v : boolean) : VALUE;
 begin
 if v
    then result := Qtrue
    else result := Qfalse
 end;

function try_strtovalue (v : VALUE) : VALUE; cdecl;
 begin
 PRecStr(v)^.value := rb_str_new2(PRecStr(v)^.pchar);
 result := Qnil;
 end;

function StrToValue (const v : utf8string) : VALUE;
 var
   rec : TRecStr;
   res : integer;
 begin
 do_check_active;
 rec.pchar := pchar(v);
 rb_protect(@try_strtovalue,VALUE(@rec),@res);
 do_check_conversion(res);
 result := rec.value;
 end;

function UStrToValue (const v : unicodestring) : VALUE;
 begin
 result := StrToValue(v)
 end;

{$IFDEF CPU32}
type
  PRecInt64 = ^TRecInt64;
  TRecInt64 = record
    int64 : int64;
    value : VALUE;
  end;

function try_valuetoint (v : VALUE) : VALUE; cdecl;
 begin
 PRecInt64(v)^.int64 := rb_num2ll(PRecInt64(v)^.value);
 result := Qnil
 end;

function ValueToInt64 (v : VALUE) : int64;
 var
   rec : TRecInt64;
   res : integer;
 begin
 do_check_active('ValueToInt64()');
 rec.value := v;
 rb_protect(@try_valuetoint64, VALUE(@rec), @res);
 do_check_conversion('ValueToInt64()',res);
 result := rec.int64;
 end;

type
  PRecUInt64 = ^TRecUInt64;
  TRecUInt64 = record
    qword : qword;
    value : VALUE;
  end;

function try_valuetouint64 (v : VALUE) : VALUE; cdecl;
 begin
 PRecUInt64(v)^.qword := rb_num2ull(PRecUInt64(v)^.value);
 result := Qnil
 end;

function ValueToUInt64 (v : VALUE) : qword;
 var
   rec : TRecUInt64;
   res : integer;
 begin
 do_check_active('ValueToUInt64()');
 rec.value := v;
 rb_protect(@try_valuetouint64,VALUE(@rec),@res);
 do_check_conversion('ValueToUInt64()',res);
 result := rec.qword;
 end;

function try_int64tovalue (v : VALUE) : VALUE; cdecl;
 begin
 PRecInt64(v)^.value := rb_ll2inum(PRecInt64(v)^.int64);
 result := Qnil;
 end;

function Int64ToValue (const v : int64) : VALUE;
 var
   rec : TRecInt64;
   res : integer;
 begin
 do_check_active('Int64ToValue()');
 rec.int64 := v;
 rb_protect(@try_int64tovalue,VALUE(@rec),@res);
 do_check_conversion('Int64ToValue()',res);
 result := rec.value;
 end;

function try_uint64tovalue (v : VALUE) : VALUE; cdecl;
 begin
 PRecUInt64(v)^.value := rb_ull2inum(PRecUInt64(v)^.qword);
 result := Qnil;
 end;

function UInt64ToValue (v : qword) : VALUE;
 var
   rec : TRecUInt64;
   res : integer;
 begin
 do_check_active('UInt64ToValue()');
 rec.qword := v;
 rb_protect(@try_uint64tovalue,VALUE(@rec),@res);
 do_check_conversion('UInt64ToValue()',res);
 result := rec.value;
 end;

operator := (v : VALUE) : int64; inline;
 begin
 result := ValueToInt64(v);
 end;

operator := (v : VALUE) : qword; inline;
 begin
 result := ValueToUInt64(v);
 end;

operator := (const v : int64) : VALUE; inline;
 begin
 result := Int64ToValue(v);
 end;

operator := (const v : qword) : VALUE; inline;
 begin
 result := UInt64ToValue(v);
 end;
{$ENDIF CPU32}

type
  PRecId = ^TRecId;
  TRecId = record
    id : ID;
    value : VALUE;
  end;

function try_valuetoid (v : VALUE) : VALUE; cdecl;
 begin
 PRecId(v)^.id := rb_to_id(PRecId(v)^.value);
 result := Qnil;
 end;

function ValueToId (v : VALUE) : ID;
 var
   rec : TRecId;
   res : integer;
 begin
 do_check_active;
 rec.value := v;
 rb_protect(@try_valuetoid,VALUE(@rec),@res);
 do_check_conversion(res);
 result := rec.id;
 end;

function IdToValue (id : ID) : VALUE;
 begin
 result.data := (id.data shl 8) or SYMBOL_FLAG
 end;

type
  PRecIStr = ^TRecIStr;
  TRecIStr = record
    pchar : pchar;
    id : ID;
  end;

function try_idtostr (v : VALUE) : VALUE; cdecl;
 begin
 PRecIStr(v)^.pchar := rb_id2name(PRecIStr(v)^.id);
 result := Qnil;
 end;

function IdToStr (id : ID) : utf8string;
 var
   rec : TRecIStr;
   res : integer;
 begin
 do_check_active;
 rec.id := id;
 rb_protect(@try_idtostr,VALUE(@rec),@res);
 do_check_conversion(res);
 result := utf8string(rec.pchar);
 end;

function try_strtoid (v : VALUE) : VALUE; cdecl;
 begin
 PRecIStr(v)^.id := rb_intern(PRecIStr(v)^.pchar);
 result := Qnil;
 end;

function StrToId (const v : utf8string) : ID;
 var
   rec : TRecIStr;
   res : integer;
 begin
 do_check_active;
 rec.pchar := pchar(v);
 rb_protect(@try_strtoid,VALUE(@rec),@res);
 do_check_conversion(res);
 result := rec.id;
 end;

function ValueToClass (v : VALUE) : TClass;
 var
   idx : ptrint;
 begin
 do_check_active;
 if not (rb_type(v) = T_CLASS)
    then raise ERubyConversion.CreateFmt(msgRubyNotClass,[do_inspect(v)]);
 for idx := 0 to high(clsCache) do
     if clsCache[idx].rb_class = v
        then exit(clsCache[idx].pp_class);
 raise ERubyConversion.CreateFmt(msgRubyNotPascalClass,[do_inspect(v)]);
 end;

function ValueToObject (v : VALUE) : TObject;
 begin
 do_check_active;
 if not (rb_type(v) = T_DATA)
    then raise ERubyConversion.CreateFmt(msgRubyNotPascalObject,[do_inspect(v)]);
 result := TObject(PRData(v)^.data);
 end;

function do_property_get(obj : TObject; const name : shortstring; out return : VALUE) : boolean; inline;
 var
   info : PPropInfo;
 begin
 info := GetPropInfo(obj, name);
 result := false;
 if info = nil
    then exit;
 result := true;
 case info^.PropType^.Kind of
      tkInteger :
        return := IntToValue(GetOrdProp(obj, info));
      {$IFDEF CPU64}
      tkInt64 :
        return := IntToValue(GetInt64Prop(obj, info));
      tkQWord :
        return := UIntToValue(qword(GetInt64Prop(obj, info)));
      {$ELSE ~CPU64}
      tkInt64 :
        return := Int64ToValue(GetInt64Prop(obj, info));
      tkQWord :
        return := UInt64ToValue(qword(GetInt64Prop(obj, info)));
      {$ENDIF CPU64}
      tkEnumeration :
        return := IdToValue(StrToId(GetEnumProp(obj, info)));
      tkFloat :
        return := DoubleToValue(GetFloatProp(obj, info));
      tkSString, tkLString, tkAString :
        return := StrToValue(GetStrProp(obj, info));
      tkWString, tkUString :
        return := UStrToValue(GetUnicodeStrProp(obj, info));
      tkBool :
        return := BoolToValue(GetOrdProp(obj, info) <> 0);
      tkClass :
        return := ObjectToValue(GetObjectProp(obj, info));
      else
        result := false
      end;
 end;

function do_property_set(obj : TObject; const name : shortstring; const val : VALUE) : boolean; inline;
 var
   info : PPropInfo;
 begin
 info := GetPropInfo(obj, name);
 if info = nil
    then exit(false);
 result := true;
 case info^.PropType^.Kind of
      tkInteger :
        SetOrdProp(obj, info, ValueToInt(val));
      {$IFDEF CPU64}
      tkInt64 :
        SetInt64Prop(obj, info, ValueToInt(val));
      tkQWord :
        SetInt64Prop(obj, info, ValueToUInt(val));
      {$ELSE ~CPU64}
      tkInt64 :
        SetInt64Prop(obj, info, ValueToInt64(val));
      tkQWord :
        SetInt64Prop(obj, info, ValueToUInt64(val));
      {$ENDIF CPU64}
      tkEnumeration :
        SetEnumProp(obj, info, IdToStr(ValueToId(val)));
      tkFloat :
        SetFloatProp(obj, info, ValueToDouble(val));
      tkSString, tkLString, tkAString :
        SetStrProp(obj, info, ValueToStr(val));
      tkWString, tkUString :
        SetUnicodeStrProp(obj, info, ValueToUStr(val));
      tkBool :
        SetOrdProp(obj, info, ord(ValueToBool(val)));
      tkClass :
        SetObjectProp(obj, info, ValueToObject(val));
      else
        result := false;
      end;
 end;

type
  PRecProp = ^TRecProp;
  TRecProp = record
    obj, name, value : VALUE;
  end;

function try_alt_property_get (v : VALUE) : VALUE; cdecl;
 begin
 PRecProp(v)^.value := rb_funcall2(PRecProp(v)^.obj, rb_intern('[]'), 1, @PRecProp(v)^.name);
 result := Qnil;
 end;

function alt_property_get (obj : VALUE; id : VALUE; out return : VALUE) : boolean;
 var
   rec : TRecProp;
   res : integer;
 begin
 rec.obj := obj;
 rec.name := id;
 rb_protect(@try_alt_property_get, VALUE(@rec), @res);
 return := rec.value;
 result := (res = 0) and (return <> Qnil);
 end;

function try_alt_property_set (v : VALUE) : VALUE; cdecl;
 begin
 rb_funcall2(PRecProp(v)^.obj, rb_intern('[]='), 2, @PRecProp(v)^.name);
 result := Qnil;
 end;

function alt_property_set(obj : VALUE; id : VALUE; v : VALUE) : boolean;
 var
   rec : TRecProp;
   res : integer;
 begin
 rec.obj := obj;
 rec.name := id;
 rec.value := v;
 rb_protect(@try_alt_property_set, VALUE(@rec), @res);
 result := (res = 0)
 end;

function do_method_missing (argc : integer; argv : PVALUE; slf : VALUE) : VALUE; cdecl;
 var
   obj : TObject;
   msg : TRubyMsg;
 begin
 obj := ValueToObject(slf);
 msg.Msg := IdToStr(ValueToId(argv[0]));
 msg.Return := Qundef;
 if (argc = 1)
    then if do_property_get(obj, msg.Msg, result)
            then exit
            else if alt_property_get(slf, argv[0], result)
                    then exit;
 result := argv[1];
 if (argc = 2) and (msg.Msg[length(msg.Msg)] = '=')
    then if do_property_set(obj, Copy(msg.Msg, 1, length(msg.Msg) - 1), argv[1])
            then exit
            else if alt_property_set(slf, argv[0], argv[1])
                    then exit;
 msg.Args := do_args(argc - 1, @(argv[1]));
 obj.DispatchStr(msg);
 setLength(msg.Args, 0);
 result := msg.Return;
 if result = Qundef
    then rb_raise(rb_eNoMethodError, 'no method ''%s'' for %s', pchar(ansistring(msg.Msg)), pchar(do_inspect(slf)));
 end;

function do_unitname (slf : VALUE) : VALUE; cdecl;
 begin
 result := StrToValue(ValueToClass(slf).UnitName)
 end;

function do_to_s (slf : VALUE) : VALUE; cdecl;
 begin
 result := StrToValue(ValueToObject(slf).toString);
 end;

function ClassToValue (cls : TClass) : VALUE;
 var
   idx, clsidx, hookidx : ptrint;
 begin
 if cls = nil
    then raise ERubyConversion.Create(msgRubyNilClass);
 do_check_active;
 for idx := 0 to high(clsCache) do
     if clsCache[idx].pp_class = cls
        then exit(clsCache[idx].rb_class);
 idx := length(clsCache);
 setLength(clsCache, idx + 1);
 clsCache[idx].pp_class := cls;
 if cls = TObject
    then result := rb_define_class(pchar(do_classname(cls)),rb_cObject)
    else result := rb_define_class(pchar(do_classname(cls)),ClassToValue(cls.ClassParent));
 clsCache[idx].rb_class := result;
 rb_define_method(result,'method_missing',Pmethod(@do_method_missing),-1);
 for clsidx := 0 to high(clsHooks) do
     if clsHooks[clsidx].cls = cls
        then for hookidx := 0 to high(clsHooks[clsidx].hooks) do
                 clsHooks[clsidx].hooks[hookidx](result);
 end;

function ObjectToValue(obj : TObject) : VALUE;
 var
   idx : ptrint;
 begin
  if obj = nil
     then exit(Qnil);
  if do_find_object(obj, idx)
     then exit(objCache[idx].rb_object);
  result := rb_data_object_alloc(ClassToValue(obj.ClassType), pointer(obj), nil, nil);
  do_insert_object(obj, result, idx);
 end;

operator := (v : VALUE) : ptrint;
 begin
 result := ValueToInt(v)
 end;

operator := (v : VALUE) : ptruint;
 begin
 result := ValueToUInt(v);
 end;

operator := (v : VALUE) : double;
 begin
 result := ValueToDouble(v);
 end;

operator := (v : VALUE) : boolean;
 begin
 result := ValueToBool(v);
 end;

operator := (v : VALUE) : utf8string;
 begin
 result := ValueToStr(v);
 end;

operator := (v : VALUE) : unicodestring;
 begin
 result := ValueToUStr(v);
 end;

operator := (v : ptrint) : VALUE;
 begin
 result := IntToValue(v);
 end;

operator := (v : ptruint) : VALUE;
 begin
 result := UIntToValue(v);
 end;

operator := (const v : double) : VALUE;
 begin
 result := DoubleToValue(v);
 end;

operator := (v : boolean) : VALUE;
 begin
 result := BoolToValue(v);
 end;

operator := (const v : utf8string) : VALUE;
 begin
 result := StrToValue(v);
 end;

operator := (const v : unicodestring) : VALUE;
 begin
 result := UStrToValue(v);
 end;

operator := (v : VALUE) : ID;
 begin
 result := ValueToId(v)
 end;

operator := (v : ID) : VALUE;
 begin
 result := IdToValue(v)
 end;

operator := (v : ID) : utf8string;
 begin
 result := IdToStr(v)
 end;

operator := (const v : utf8string) : ID;
 begin
 result := StrToId(v)
 end;

operator := (v : VALUE) : TClass; inline;
 begin
 result := ValueToClass(v);
 end;

operator := (v : VALUE) : TObject; inline;
 begin
 result := ValueToObject(v);
 end;

operator := (v : TClass) : VALUE;
 begin
 result := ClassToValue(v);
 end;

operator := (v : TObject) : VALUE;
 begin
 result := ObjectToValue(v);
 end;

procedure RegisterObject(obj : TObject; v : VALUE);
 var
   idx : ptrint;
 begin
 if do_find_object(obj, idx)
    then objCache[idx].rb_object := v
    else do_insert_object(obj, v, idx);
 end;

function do_object_equals (slf : VALUE; obj : VALUE) : VALUE; cdecl;
 begin
  result := (ValueToObject(slf).Equals(ValueToObject(obj)));
 end;

procedure TObjectHook (cObject : VALUE);
 begin
 rb_define_singleton_method(cObject,'unitname',Pmethod(@do_unitname),0);
 rb_define_method(cObject,'to_s',Pmethod(@do_to_s),0);
 rb_define_method(cObject,'equals',Pmethod(@do_object_equals), 1);
 rb_define_alias(cObject, '===', 'equals');
 (* Здесь должна была быть реализация each для классов, поддерживающих
    IEnumerable, но как это сделать — непонятно. *)
 end;

initialization
 scriptName := ParamStr(0);
 AddClassHook(TObject, @TObjectHook);
finalization
 Finalize;
end.
