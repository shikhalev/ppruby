(*
    License: GNU General Public License
*)

{$codepage utf8}
{$mode objfpc}{$h+}
{$smartlink on}
{$typeinfo on}

unit rb18Classes;

interface

uses
  Classes, SysUtils,
  ruby18, rb18System;

implementation

function do_persistent_assign (slf : VALUE; src : VALUE) : VALUE; cdecl;
 begin
 (ValueToObject(slf) as TPersistent).Assign(ValueToObject(src) as TPersistent);
 result := slf;
 end;

function do_persistent_getnamepath (slf : VALUE) : VALUE; cdecl;
 begin
 result := StrToValue((ValueToObject(slf) as TPersistent).GetNamePath);
 end;

function do_prealloc (cls : VALUE) : VALUE; cdecl;
 begin
 result := rb_data_object_alloc(cls, nil, nil, nil);
 end;

function do_component_initialize (slf : VALUE; owner : VALUE) : VALUE; cdecl;
 var
   obj : TComponent;
 begin
 if owner = Qnil
    then obj := TComponentClass(ValueToClass(rb_class_of(slf))).Create(nil)
    else obj := TComponentClass(ValueToClass(rb_class_of(slf))).Create(ValueToObject(owner) as TComponent);
 PRData(slf)^.data := pointer(obj);
 RegObject(obj, slf);
 result := slf;
 end;

function do_component_child (slf : VALUE; name : VALUE) : VALUE; cdecl;
 begin
 case rb_type(name) of
      T_FIXNUM :
        result := ObjectToValue((ValueToObject(slf) as TComponent).Components[ValueToInt(name)]);
      T_SYMBOL :
        result := ObjectToValue((ValueToObject(slf) as TComponent).FindComponent(IdToStr(ValueToId(name))));
      else
        result := ObjectToValue((ValueToObject(slf) as TComponent).FindComponent(ValueToStr(name)));
      end;
 end;

function do_component_each (slf : VALUE) : VALUE; cdecl;
 var
   obj : TComponent;
   item : TObject;
 begin
 obj := ValueToObject(slf) as TComponent;
 for item in obj do
     result := rb_yield(ObjectToValue(item));
 end;

function do_component_owner (slf : VALUE) : VALUE; cdecl;
 begin
 result := ObjectToValue((ValueToObject(slf) as TComponent).Owner);
 end;

function do_component_name (slf : VALUE) : VALUE; cdecl;
 begin
 result := IdToValue(StrToId((ValueToObject(slf) as TComponent).Name));
 end;

function do_component_setname (slf : VALUE; name : VALUE) : VALUE; cdecl;
 begin
 case rb_type(name) of
      T_SYMBOL :
        (ValueToObject(slf) as TComponent).Name := IdToStr(ValueToId(name));
      else
        (ValueToObject(slf) as TComponent).Name := ValueToStr(name);
      end;
 result := slf;
 end;

function do_component_tag (slf : VALUE) : VALUE; cdecl;
 begin
 result := ValueToInt((ValueToObject(slf) as TComponent).Tag);
 end;

function do_component_settag (slf : VALUE; tag : VALUE) : VALUE; cdecl;
 begin
 (ValueToObject(slf) as TComponent).Tag := ValueToInt(tag);
 result := slf;
 end;

procedure InitHook;
 var
   cPersistent : VALUE;
   cComponent : VALUE;
//   cStrings : VALUE;
//   cCollection : VALUE;
//   cCollectionItem : VALUE;
 begin
 cPersistent := ClassToValue(TPersistent);
 rb_define_method(cPersistent,'assign',Pmethod(@do_persistent_assign),1);
 rb_define_method(cPersistent,'namepath',Pmethod(@do_persistent_getnamepath),0);
 cComponent := ClassToValue(TComponent);
 rb_define_alloc_func(cComponent,@do_prealloc);
 rb_define_method(cComponent,'initialize',Pmethod(@do_component_initialize),1);
 rb_define_method(cComponent,'[]',Pmethod(@do_component_child),1);
 rb_include_module(cComponent, rb_mEnumerable);
 rb_define_method(cComponent,'each',Pmethod(@do_component_each),0);
 rb_define_method(cComponent,'owner',Pmethod(@do_component_owner),0);
 rb_define_method(cComponent,'name',Pmethod(@do_component_name),0);
 rb_define_method(cComponent,'name=',Pmethod(@do_component_setname),1);
 rb_define_method(cComponent,'tag',Pmethod(@do_component_tag),0);
 rb_define_method(cComponent,'tag=',Pmethod(@do_component_settag),1);
// cStrings := ClassToValue(TStrings); // Надо бы сделать, да дофига...
// cCollection := ClassToValue(TCollection);
// cCollectionItem := ClassToValue(TCollectionItem);
 end;

initialization
 rb18System.AddInitHook(@InitHook);
end.
