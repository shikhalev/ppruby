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
  ruby18dynamic, rb18System;

implementation

// TODO: Коллекции

function do_persistent_assign (slf : VALUE; src : VALUE) : VALUE; cdecl;
 begin
 (ValueToObject(slf) as TPersistent).Assign(ValueToObject(src) as TPersistent);
 result := slf;
 end;

function do_persistent_getnamepath (slf : VALUE) : VALUE; cdecl;
 begin
 result := StrToValue((ValueToObject(slf) as TPersistent).GetNamePath);
 end;

procedure TPersistentHook (cPersistent : VALUE);
 begin
 rb_define_method(cPersistent,'assign',Pmethod(@do_persistent_assign),1);
 rb_define_method(cPersistent,'namepath',Pmethod(@do_persistent_getnamepath),0);
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
 RegisterObject(obj, slf);
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

function do_component_parent (slf : VALUE) : VALUE; cdecl;
 begin
 result := ObjectToValue((ValueToObject(slf) as TComponent).GetParentComponent);
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

procedure TComponentHook (cComponent : VALUE);
 begin
 rb_define_alloc_func(cComponent,@do_prealloc);
 rb_define_method(cComponent,'initialize',Pmethod(@do_component_initialize),1);
 rb_define_method(cComponent,'[]',Pmethod(@do_component_child),1);
 rb_include_module(cComponent, rb_mEnumerable);
 rb_define_method(cComponent,'each',Pmethod(@do_component_each),0);
 rb_define_method(cComponent,'owner',Pmethod(@do_component_owner),0);
 rb_define_method(cComponent,'parent',Pmethod(@do_component_parent),0);
 rb_define_method(cComponent,'name',Pmethod(@do_component_name),0);
 rb_define_method(cComponent,'name=',Pmethod(@do_component_setname),1);
 rb_define_method(cComponent,'tag',Pmethod(@do_component_tag),0);
 rb_define_method(cComponent,'tag=',Pmethod(@do_component_settag),1);
 end;

function do_collection_owner (slf : VALUE) : VALUE; cdecl;
 begin
  result := ObjectToValue((ValueToObject(slf) as TCollection).Owner);
 end;

function do_collection_add (slf : VALUE) : VALUE; cdecl;
 begin
  result := ObjectToValue((ValueToObject(slf) as TCollection).Add);
 end;

function do_collection_clear (slf : VALUE) : VALUE; cdecl;
 begin
  (ValueToObject(slf) as TCollection).Clear;
  result := slf;
 end;

function do_collection_delete (slf : VALUE; idx : VALUE) : VALUE; cdecl;
 begin
  (ValueToObject(slf) as TCollection).Delete(ValueToInt(idx));
  result := slf;
 end;

function do_collection_insert (slf : VALUE; idx : VALUE) : VALUE; cdecl;
 begin
  result := ObjectToValue((ValueToObject(slf) as TCollection).Insert(ValueToInt(idx)));
 end;

function do_collection_count (slf : VALUE) : VALUE; cdecl;
 begin
  result := IntToValue((ValueToObject(slf) as TCollection).Count);
 end;

function do_collection_itemclass (slf : VALUE) : VALUE; cdecl;
 begin
  result := ClassToValue((ValueToObject(slf) as TCollection).ItemClass);
 end;

function do_collection_item (slf : VALUE; idx : VALUE) : VALUE; cdecl;
 begin
  result := ObjectToValue((ValueToObject(slf) as TCollection).Items[ValueToInt(idx)]);
 end;

function do_collection_setitem (slf : VALUE; idx : VALUE; item : VALUE) : VALUE; cdecl;
 begin
  (ValueToObject(slf) as TCollection).Items[ValueToInt(idx)] := ValueToObject(item) as TCollectionItem;
  result := slf;
 end;

function do_collection_each (slf : VALUE) : VALUE; cdecl;
 var
   obj : TCollection;
   item : TObject;
 begin
  obj := ValueToObject(slf) as TCollection;
  for item in obj do
      result := rb_yield(ObjectToValue(item));
 end;

function do_collection_find (slf : VALUE; id : VALUE) : VALUE; cdecl;
 begin
  result := ObjectToValue((ValueToObject(slf) as TCollection).FindItemID(ValueToInt(id)));
 end;

procedure TCollectionHook (cCollection : VALUE);
 begin
  rb_define_method(cCollection, 'owner', Pmethod(@do_collection_owner), 0);
  rb_define_method(cCollection, 'add', Pmethod(@do_collection_add), 0);
  rb_define_method(cCollection, 'clear', Pmethod(@do_collection_clear), 0);
  rb_define_method(cCollection, 'delete', Pmethod(@do_collection_delete), 1);
  rb_define_method(cCollection, 'insert', Pmethod(@do_collection_insert), 1);
  rb_define_method(cCollection, 'count', Pmethod(@do_collection_count), 0);
  rb_define_method(cCollection, 'itemclass', Pmethod(@do_collection_itemclass), 0);
  rb_define_method(cCollection, '[]', Pmethod(@do_collection_item), 1);
  rb_define_method(cCollection, '[]=', Pmethod(@do_collection_setitem), 2);
  rb_include_module(cCollection, rb_mEnumerable);
  rb_define_method(cCollection, 'find', Pmethod(@do_collection_find), 1);
  rb_define_method(cCollection, 'each', Pmethod(@do_collection_each), 0);
 end;

function do_collectionitem_collection (slf : VALUE) : VALUE; cdecl;
 begin
  result := ObjectToValue((ValueToObject(slf) as TCollectionItem).Collection);
 end;

function do_collectionitem_setcollection (slf : VALUE; c : VALUE) : VALUE; cdecl;
 begin
  (ValueToObject(slf) as TCollectionItem).Collection := ValueToObject(c) as TCollection;
  result := slf;
 end;

function do_collectionitem_displayname (slf : VALUE) : VALUE; cdecl;
 begin
  result := StrToValue((ValueToObject(slf) as TCollectionItem).DisplayName);
 end;

function do_collectionitem_setdisplayname (slf : VALUE; name : VALUE) : VALUE; cdecl;
 begin
  (ValueToObject(slf) as TCollectionItem).DisplayName := ValueToStr(name);
  result := slf;
 end;

function do_collectionitem_id (slf : VALUE) : VALUE; cdecl;
 begin
  result := IntToValue((ValueToObject(slf) as TCollectionItem).ID);
 end;

function do_collectionitem_index (slf : VALUE) : VALUE; cdecl;
 begin
  result := IntToValue((ValueToObject(slf) as TCollectionItem).Index);
 end;

function do_collectionitem_setindex (slf : VALUE; idx : VALUE) : VALUE; cdecl;
 begin
  (ValueToObject(slf) as TCollectionItem).Index := ValueToInt(idx);
  result := slf;
 end;

procedure TCollectionItemHook (cCollectionItem : VALUE);
 begin
  rb_define_method(cCollectionItem, 'collection', Pmethod(@do_collectionitem_collection), 0);
  rb_define_method(cCollectionItem, 'collection=', Pmethod(@do_collectionitem_setcollection), 1);
  rb_define_method(cCollectionItem, 'displayname', Pmethod(@do_collectionitem_displayname), 0);
  rb_define_method(cCollectionItem, 'displayname=', Pmethod(@do_collectionitem_setdisplayname), 1);
  rb_define_method(cCollectionItem, 'id', Pmethod(@do_collectionitem_id), 0);
  rb_define_method(cCollectionItem, 'index', Pmethod(@do_collectionitem_index), 0);
  rb_define_method(cCollectionItem, 'index=', Pmethod(@do_collectionitem_setindex), 1);
 end;

function do_strings_add (slf : VALUE; s : VALUE) : VALUE; cdecl;
 begin
  result := IntToValue((ValueToObject(slf) as TStrings).Add(ValueToStr(s)));
 end;

function do_strings_addstrings (slf : VALUE; s : VALUE) : VALUE; cdecl;
 begin
  (ValueToObject(slf) as TStrings).AddStrings(ValueToObject(s) as TStrings);
  result := slf;
 end;

function do_strings_append (slf : VALUE; s : VALUE) : VALUE; cdecl;
 begin
  (ValueToObject(slf) as TStrings).Append(ValueToStr(s));
  result := slf;
 end;

function do_strings_clear (slf : VALUE) : VALUE; cdecl;
 begin
  (ValueToObject(slf) as TStrings).Clear;
  result := slf;
 end;

function do_strings_delete (slf : VALUE; idx : VALUE) : VALUE; cdecl;
 begin
  (ValueToObject(slf) as TStrings).Delete(ValueToInt(idx));
  result := slf;
 end;

function do_strings_equals (slf : VALUE; obj : VALUE) : VALUE; cdecl;
 begin
  result := (ValueToObject(slf).Equals(ValueToObject(obj)));
 end;

function do_strings_insert (slf : VALUE; idx : VALUE; str : VALUE) : VALUE; cdecl;
 begin
  (ValueToObject(slf) as TStrings).Insert(ValueToInt(idx), ValueToStr(str));
  result := slf;
 end;

function do_strings_loadfromfile (slf : VALUE; name : VALUE) : VALUE; cdecl;
 begin
  (ValueToObject(slf) as TStrings).LoadFromFile(ValueToStr(name));
  result := slf;
 end;

function do_strings_loadfromstream (slf : VALUE; stream : VALUE) : VALUE; cdecl;
 begin
  (ValueToObject(slf) as TStrings).LoadFromStream(ValueToObject(stream) as TStream);
  result := slf;
 end;

function do_strings_savetofile (slf : VALUE; name : VALUE) : VALUE; cdecl;
 begin
  (ValueToObject(slf) as TStrings).SaveToFile(ValueToStr(name));
  result := slf;
 end;

function do_strings_savetostream (slf : VALUE; stream : VALUE) : VALUE; cdecl;
 begin
  (ValueToObject(slf) as TStrings).SaveToStream(ValueToObject(stream) as TStream);
  result := slf;
 end;

function do_strings_count (slf : VALUE) : VALUE; cdecl;
 begin
  result := IntToValue((ValueToObject(slf) as TStrings).Count);
 end;

function do_strings_text (slf : VALUE) : VALUE; cdecl;
 begin
  result := StrToValue((ValueToObject(slf) as TStrings).Text);
 end;

function do_strings_settext (slf : VALUE; str : VALUE) : VALUE; cdecl;
 begin
  (ValueToObject(slf) as TStrings).Text := ValueToStr(str);
  result := slf;
 end;

function do_strings_item (slf : VALUE; idx : VALUE) : VALUE; cdecl;
 var
   obj : TStrings;
 begin
  obj := ValueToObject(slf) as TStrings;
  case rb_type(idx) of
       T_FIXNUM :
         result := StrToValue(obj.Strings[ValueToInt(idx)]);
       T_SYMBOL :
         result := StrToValue(obj.Values[IdToStr(ValueToId(idx))]);
       else
         result := StrToValue(obj.Values[ValueToStr(idx)]);
  end;
 end;

function do_strings_setitem (slf : VALUE; idx : VALUE; v : VALUE) : VALUE; cdecl;
 var
   obj : TStrings;
 begin
  obj := ValueToObject(slf) as TStrings;
  case rb_type(idx) of
       T_FIXNUM :
         obj.Strings[ValueToInt(idx)] := ValueToStr(v);
       T_SYMBOL :
         obj.Values[IdToStr(ValueToId(idx))] := ValueToStr(v);
       else
         obj.Values[ValueToStr(idx)] := ValueToStr(v);
  end;
  result := slf;
 end;

function do_strings_each (slf : VALUE) : VALUE; cdecl;
 var
   obj : TStrings;
   item : utf8string;
 begin
  obj := ValueToObject(slf) as TStrings;
  for item in obj do
      result := rb_yield(StrToValue(item));
 end;

procedure TStringsHook (cStrings : VALUE);
 begin
  rb_define_method(cStrings, 'add', Pmethod(@do_strings_add), 1);
  rb_define_method(cStrings, 'addstrings', Pmethod(@do_strings_addstrings), 1);
  rb_define_method(cStrings, 'append', Pmethod(@do_strings_append), 1);
  rb_define_method(cStrings, 'clear', Pmethod(@do_strings_clear), 0);
  rb_define_method(cStrings, 'delete', Pmethod(@do_strings_delete), 1);
  rb_define_method(cStrings, 'equals', Pmethod(@do_strings_equals), 1);
  rb_define_method(cStrings, 'insert', Pmethod(@do_strings_insert), 2);
  rb_define_method(cStrings, 'loadfromfile', Pmethod(@do_strings_loadfromfile), 1);
  rb_define_method(cStrings, 'loadfromstream', Pmethod(@do_strings_loadfromstream), 1);
  rb_define_method(cStrings, 'savetofile', Pmethod(@do_strings_savetofile), 1);
  rb_define_method(cStrings, 'savetostream', Pmethod(@do_strings_savetostream), 1);
  rb_define_method(cStrings, 'count', Pmethod(@do_strings_count), 0);
  rb_define_method(cStrings, 'text', Pmethod(@do_strings_text), 0);
  rb_define_method(cStrings, 'text=', Pmethod(@do_strings_settext), 0);
  rb_define_method(cStrings, '[]', Pmethod(@do_strings_item), 1);
  rb_define_method(cStrings, '[]=', Pmethod(@do_strings_setitem), 2);
  rb_include_module(cStrings, rb_mEnumerable);
  rb_define_method(cStrings, 'each', Pmethod(@do_strings_each), 0);
  rb_define_alias(cStrings, '<<', 'append');
  rb_define_alias(cStrings, '===', 'equals');
 end;

initialization
 rb18System.AddClassHook(TPersistent, @TPersistentHook);
 rb18System.AddClassHook(TComponent, @TComponentHook);
 rb18System.AddClassHook(TCollection, @TCollectionHook);
 rb18System.AddClassHook(TCollectionItem, @TCollectionItemHook);
end.
