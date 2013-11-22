{$include rbopts.inc}

unit RbObjects;

interface

uses
  SysUtils, typinfo, Ruby, RbTools;

var
  pp_cPascalIO : VALUE;

type
  IOutput = interface(IUnknown)
  ['{83A91283-5CD0-4408-BC2A-BE02274D8902}']
    function Write (val : VALUE) : VALUE;
  end;

// unwrapped helpers
function OV (x : TObject) : VALUE;
function NV (x : TObject) : VALUE;
function CV (x : TClass ) : VALUE;
function WV (x : Pointer) : VALUE;
function VO (x : VALUE) : TObject;
function VJ (x : VALUE) : TObject;
function VC (x : VALUE) : TClass;
function VW (x : VALUE) : Pointer;

// wrapped helpers
function Obj2Val (const x : TObject) : VALUE;
function New2Val (const x : TObject) : VALUE;
function Cls2Val (const x : TClass ) : VALUE;
function Out2Val (const x : IOutput) : VALUE;
function Val2Obj (v : VALUE; nn : Boolean = False) : TObject;
function Val2Cls (v : VALUE) : TClass;
function Val2Out (v : VALUE) : IOutput;

// class initializators
type
  pp_unit_hook = procedure (v : VALUE);
  pp_class_hook = procedure (v : VALUE);

procedure pp_reg_unit_hook (const name : AnsiString; hook : pp_unit_hook);
procedure pp_reg_class_hook (cls : TClass; hook : pp_class_hook);

function CName (const name : AnsiString) : AnsiString; inline;
function mname (const name : AnsiString) : AnsiString; inline;

// other helpers (unwrapped!)
procedure pp_invalidtype (v : VALUE; _type : PChar); inline;

implementation

uses
  Classes;

const
  msgType = '%s is not a valid %s!';

const
  CLASS_IV = 'pascal_class';

var
  id_class_eval : ID;

var
  unit_hooks : array of record
    name : AnsiString;
    hook : pp_unit_hook;
  end;

var
  cls_hooks : array of record
    cls : TClass;
    hook : pp_class_hook;
  end;

procedure free_new (p : Pointer); cdecl;
 begin
  TObject(p).Free;
 end;

function OV(x : TObject) : VALUE;
 begin
  if x = nil
     then Result := Qnil
     else Result := rb_data_object_alloc(CV(x.ClassType), x, nil, nil);
 end;

function NV(x : TObject) : VALUE;
 begin
  if x = nil
     then Result := Qnil
     else Result := rb_data_object_alloc(CV(x.ClassType), x, nil, @free_new);
 end;

var
  unit_cache : array of record
    name : AnsiString;
    value : VALUE;
  end = nil;

function TV (const name : AnsiString) : VALUE; local;
 var
   l, i : Integer;
   nm : AnsiString;
 begin
  nm := UpperCase(name);
  l := Length(unit_cache);
  for i := 0 to l - 1 do
      if unit_cache[i].name = nm
         then exit(unit_cache[i].value);
  Result := rb_define_module_under(pp_mPascal, PChar(name));
  SetLength(unit_cache, l + 1);
  unit_cache[l].name := UpperCase(name);
  unit_cache[l].value := Result;
  for i := 0 to High(unit_hooks) do
      if (unit_hooks[i].name = nm) and (unit_hooks[i].hook <> nil)
         then unit_hooks[i].hook(Result);
 end;

var
  cls_cache : array of record
    cls : TClass;
    value : VALUE;
  end = nil;

function find_cls (cls : TClass; out val : VALUE;
                                 out idx : PtrInt) : Boolean; local;
 var
   min, max, med : PtrInt;
 begin
  min := 0;
  max := High(cls_cache);
  if max < 0
     then begin
           idx := 0;
           exit(False);
          end;
  while True do
        begin
         if cls_cache[min].cls = cls
            then begin
                  idx := min;
                  val := cls_cache[min].value;
                  exit(True);
                 end;
         if Pointer(cls_cache[min].cls) > Pointer(cls)
            then begin
                  idx := min;
                  break;
                 end;
         if cls_cache[max].cls = cls
            then begin
                  idx := max;
                  val := cls_cache[max].value;
                  exit(True);
                 end;
         if Pointer(cls_cache[max].cls) < Pointer(cls)
            then begin
                  idx := max + 1;
                  break;
                 end;
         if max - min <= 1
            then begin
                  idx := min + 1;
                  break;
                 end;
         med := (min + max) div 2;
         if Pointer(cls_cache[med].cls) <= Pointer(cls)
            then min := med
            else max := med;
        end;
  Result := False;
 end;

procedure define_props (cls : TClass; val : VALUE); local;
 const
   VALID_PROPS = [tkInteger, tkInt64, tkQWord, tkEnumeration, tkSet, tkFloat,
                  tkSString, tkLString, tkAString, tkUString, tkWString,
                  tkUChar, tkChar, tkWChar, tkBool, tkClass {, tkClassRef }];
 var
   list : PPropList;
   prop : PPropInfo;
   l, i : Integer;
   name, code : AnsiString;
   str : VALUE;
 begin
  l := GetPropList(cls, list);
  for i := 0 to l - 1 do
      begin
       prop := list^[i];
       if prop^.PropType^.Kind in VALID_PROPS
          then begin
                name := mname(prop^.Name);
                code := 'attr_accessor :' + name + LineEnding +
                        'def ' + name + LineEnding +
                        '  pascal_get_prop(''' + name + ''')' + LineEnding +
                        'end' + LineEnding +
                        'def ' + name + '=(value)' + LineEnding +
                        '  pascal_set_prop(''' + name + ''', value)' +
                                                                   LineEnding +
                        'end';
                str := SV(PChar(code));
                rb_funcall2(val, id_class_eval, 1, @str);
               end;
      end;
 end;

procedure add_cls (cls : TClass; out val : VALUE; idx : PtrInt); local;
 var
   l, i : PtrInt;
   u, p : VALUE;
 begin
  l := Length(cls_cache);
  SetLength(cls_cache, l + 1);
  for i := l downto idx + 1 do
      cls_cache[i] := cls_cache[i - 1];
  u := TV(cls.UnitName);
  if cls.ClassParent = nil
     then p := rb_cData
     else if cls = Exception
             then p := pp_ePascalError
             else p := CV(cls.ClassParent);
  val := rb_define_class_under(u, PChar(CName(cls.ClassName)), p);
  cls_cache[idx].cls := cls;
  cls_cache[idx].value := val;
  {$hints off}
  rb_iv_set(val, CLASS_IV, UV(PtrUInt(cls)));
  {$hints on}
  define_props(cls, val);
  for i := 0 to High(cls_hooks) do
      if cls_hooks[i].cls = cls
         then cls_hooks[i].hook(val);
 end;

function CV(x : TClass) : VALUE;
 var
   i : PtrInt;
 begin
  if not find_cls(x, Result, i)
     then add_cls(x, Result, i);
 end;

procedure free_out (p : Pointer); cdecl;
 begin
  IOutput(p)._Release;
 end;

function WV(x : Pointer) : VALUE;
 begin
  Result := rb_data_object_alloc(pp_cPascalIO, x, nil, @free_out);
  IOutput(x)._AddRef;
 end;

function VO(x : VALUE) : TObject;
 begin
  if x = Qnil
     then exit(nil);
  if not ((rb_type(x) = T_DATA) and (rb_obj_is_kind_of(x, rb_cData) = Qtrue))
     then pp_invalidtype(x, 'Pascal object')
     else Result := TObject(pp_data_object(x));
 end;

function VJ(x : VALUE) : TObject;
 begin
  if not ((rb_type(x) = T_DATA) and (rb_obj_is_kind_of(x, rb_cData) = Qtrue))
     then pp_invalidtype(x, 'Pascal object')
     else Result := TObject(pp_data_object(x));
 end;

function VC(x : VALUE) : TClass;
 begin
  if x = Qnil
     then exit(nil);
  if not ((rb_type(x) = T_CLASS) and
          (rb_class_inherited_p(x, rb_cData) = Qtrue))
     then pp_invalidtype(x, 'Pascal class')
     else {$hints off}Result := TClass(VU(rb_iv_get(x, CLASS_IV)));{$hints on}
 end;

function VW(x : VALUE) : Pointer;
 begin
  if not ((rb_type(x) = T_DATA) and
          (rb_obj_is_kind_of(x, pp_cPascalIO) = Qtrue))
     then pp_invalidtype(x, 'Pascal::IO')
     else Result := pp_data_object(x);
 end;

function Obj2Val(const x : TObject) : VALUE;

 procedure _;
  begin
   Result := OV(x);
  end;

 begin
  pp_protect(@_);
 end;

function New2Val(const x : TObject) : VALUE;

 procedure _;
  begin
   Result := NV(x);
  end;

 begin
  pp_protect(@_);
 end;

function Cls2Val(const x : TClass) : VALUE;
 procedure _;
  begin
   Result := CV(x);
  end;
 begin
  pp_protect(@_);
 end;

function Out2Val(const x : IOutput) : VALUE;

 procedure _;
  begin
   Result := WV(Pointer(x));
  end;

 begin
  pp_protect(@_);
 end;

function Val2Obj(v : VALUE; nn : Boolean = False) : TObject;

 procedure _;
  begin
   if nn
      then Result := VJ(v)
      else Result := VO(v);
  end;

 begin
  pp_protect(@_);
 end;

function Val2Cls(v : VALUE) : TClass;

 procedure _;
  begin
   Result := VC(v);
  end;

 begin
  pp_protect(@_);
 end;

function Val2Out (v : VALUE) : IOutput;

 procedure _;
  begin
   Pointer(Result) := VW(v);
  end;

 begin
  pp_protect(@_);
 end;

procedure pp_reg_unit_hook(const name : AnsiString; hook : pp_unit_hook);
 var
   l : Integer;
 begin
  l := Length(unit_hooks);
  SetLength(unit_hooks, l + 1);
  unit_hooks[l].name := UpperCase(name);
  unit_hooks[l].hook := hook;
 end;

procedure pp_reg_class_hook(cls : TClass; hook : pp_class_hook);
 var
   l : Integer;
 begin
  l := Length(cls_hooks);
  SetLength(cls_hooks, l + 1);
  cls_hooks[l].cls := cls;
  cls_hooks[l].hook := hook;
 end;

function CName(const name : AnsiString) : AnsiString; inline;
 begin
  Result := name;
  Result[1] := UpCase(name[1]);
 end;

function mname(const name : AnsiString) : AnsiString; inline;
 begin
  Result := LowerCase(name);
 end;

procedure pp_invalidtype (v : VALUE; _type : PChar); inline;
 begin
  rb_raise(rb_eTypeError, msgType, S(v), _type);
 end;

function Str2Set (const s : UTF8String) : VALUE; inline;
 begin
  if s = ''
     then Result := rb_eval_string('Set.new')
     else Result := rb_eval_string(PChar('Set[:' +
                            StringReplace(s, ',', ',:', [rfReplaceAll]) + ']'));
 end;

function get_str_prop_value (obj : TObject; prop : PPropInfo) : VALUE; local;
 var
   p : PChar;
 procedure _;
  var
    s : UTF8String;
  begin
   s := GetStrProp(obj, prop);
   p := strnew(PChar(s));
   s := '';
  end;
 begin
  pp_try(@_);
  Result := SV(p);
  StrDispose(p);
 end;

function get_ustr_prop_value (obj : TObject; prop : PPropInfo) : VALUE; local;
 var
   p : PChar;
 procedure _;
  var
    s : UTF8String;
    u : UnicodeString;
  begin
   u := GetUnicodeStrProp(obj, prop);
   s := UTF8Encode(u);
   p := strnew(PChar(s));
   u := '';
   s := '';
  end;
 begin
  pp_try(@_);
  Result := SV(p);
  StrDispose(p);
 end;

function pascal_get_prop (obj, prop : VALUE) : VALUE; cdecl;
 var
   o : TObject;
   n : PChar;
   info : PPropInfo;
 begin
  o := VJ(obj);
  n := VP(prop);
  info := GetPropInfo(o, n);
  if info = nil
     then rb_notimplement()
     else case info^.PropType^.Kind of
            tkInteger :
              Result := IV(GetOrdProp(o, info));
            tkInt64 :
              Result := rb_ll2inum(GetInt64Prop(o, info));
            tkQWord :
              Result := rb_ull2inum(QWord(GetInt64Prop(o, info)));
            tkEnumeration :
              Result := SY(PChar(GetEnumProp(o, info)));
            tkSet :
              Result := Str2Set(GetSetProp(o, info, False));
            tkFloat :
              Result := DV(GetFloatProp(o, info));
            tkLString, tkSString, tkAString :
              Result := get_str_prop_value(o, info);
            tkWString, tkUString :
              Result := get_ustr_prop_value(o, info);
            tkChar :
              Result := SV(PChar(UTF8String(AnsiChar(GetOrdProp(o, info)))));
            tkUChar, tkWChar :
              Result := SV(PChar(UTF8Encode(
                          UnicodeString(UnicodeChar(GetOrdProp(o, info)))
                        )));
            tkBool :
              Result := BV(GetOrdProp(o, info) <> 0);
            tkClass :
              Result := OV(GetObjectProp(o, info));
           { tkClassRef :
              Result := CV(TClass(GetObjectProp(o, info))); }
            else
              rb_notimplement();
          end;
 end;

function Set2Str (v : VALUE) : UTF8String;
 begin
  Result:= '' + VP(C(C(v, 'to_a', []), 'join', [SV(',')]));
 end;

function pascal_set_prop (obj, prop, val : VALUE) : VALUE; cdecl;
 var
   o : TObject;
   n : PChar;
   info : PPropInfo;
 begin
  o := VJ(obj);
  n := VP(prop);
  info := GetPropInfo(o, n);
  if info = nil
     then rb_notimplement()
     else case info^.PropType^.Kind of
           tkInteger :
             SetOrdProp(o, info, VI(val));
           tkInt64 :
             SetInt64Prop(o, info, rb_num2ll(val));
           tkQWord :
             SetInt64Prop(o, info, Int64(rb_num2ull(val)));
           tkEnumeration :
             SetEnumProp(o, info, YP(val));
           tkSet :
             SetSetProp(o, info, Set2Str(val));
           tkFloat :
             SetFloatProp(o, info, VD(val));
           tkLString, tkSString, tkAString :
             SetStrProp(o, info, VP(val));
           tkWString, tkUString :
             SetUnicodeStrProp(o, info, UTF8Decode(VP(val)));
           tkChar :
             SetOrdProp(o, info, Ord(AnsiChar(AnsiString(VP(val))[1])));
           tkWChar, tkUChar :
             SetOrdProp(o, info, Ord(UnicodeChar(UTF8Decode(VP(val))[1])));
           tkBool :
             SetOrdProp(o, info, Ord(VB(val)));
           tkClass :
             SetObjectProp(o, info, VO(val));
          { tkClassRef :
             SetObjectProp(o, info, TObject(VC(val))); }
           else
             rb_notimplement();
          end;
  Result := val;
 end;

function pascal_equals (obj, v : VALUE) : VALUE; cdecl;
 var
   o : TObject;
 begin
  o := VJ(obj);
  if (rb_type(v) = T_DATA) and o.Equals(VJ(v))
     then Result := Qtrue
     else Result := Qfalse;
 end;

procedure cls_TObject (val : VALUE);
 begin
  rb_define_private_method(val, 'pascal_get_prop', @pascal_get_prop, 1);
  rb_define_private_method(val, 'pascal_set_prop', @pascal_set_prop, 2);
  rb_define_method(val, '==', @pascal_equals, 1);
  rb_undef_alloc_func(val);
 end;

function pascal_io_write (obj, val : VALUE) : VALUE; cdecl;
 procedure _;
  begin
   Result := IOutput(pp_data_object(obj)).Write(val);
  end;
 begin
  pp_try(@_);
 end;

procedure init;

 procedure _;
  begin
   rb_require('set');
   id_class_eval := SD('class_eval');
   pp_cPascalIO := rb_define_class_under(pp_mPascal, 'IO', rb_cIO);
   rb_undef_alloc_func(pp_cPascalIO);
   rb_define_method(pp_cPascalIO, 'write', @pascal_io_write, 1);
  end;

 begin
  pp_protect(@_);
 end;

procedure done;
 begin
  SetLength(unit_cache, 0);
  SetLength(cls_cache, 0);
 end;

initialization
 pp_reg_init_hook(@init);
 pp_reg_done_hook(@done);
 pp_reg_class_hook(TObject, @cls_TObject);
end.

