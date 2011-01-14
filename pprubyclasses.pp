unit ppRubyClasses;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, ppRuby;

procedure TObjectClassHook (cObject : VALUE);
procedure TPersistentClassHook (cPersistent : VALUE);
procedure TComponentClassHook (cComponent : VALUE);
procedure TStringsClassHook (cStrings : VALUE);

implementation

function m_tobject_to_s (instance : VALUE) : VALUE; cdecl;
 begin
  Result := VALUE(TObject(instance).ToString);
 end;

function m_tobject_equals (instance : VALUE; other : VALUE) : VALUE; cdecl;
 begin
  try
   Result := VALUE(TObject(instance).Equals(TObject(other)));
  except
   Result := Qfalse;
  end;
 end;

function m_tclass_unitname (cls : VALUE) : VALUE; cdecl;
 begin
  Result := VALUE(TClass(cls).UnitName);
 end;

procedure TObjectClassHook (cObject : VALUE);
 begin
  DefineMethod(cObject, 'to_s', @m_tobject_to_s);
  DefineMethod(cObject, '===', @m_tobject_equals);
  DefineSingletonMethod(cObject, 'unitname', @m_tclass_unitname);
 end;

function m_tpersistent_assign (instance : VALUE; other : VALUE) : VALUE; cdecl;
 begin
  (TObject(instance) as TPersistent).Assign(TObject(other) as TPersistent);
  Result := instance;
 end;

function m_tpersistent_namepath (instance : VALUE) : VALUE; cdecl;
 begin
  Result := VALUE((TObject(instance) as TPersistent).GetNamePath);
 end;

procedure TPersistentClassHook(cPersistent : VALUE);
 begin
  DefineMethod(cPersistent, 'assign', @m_tpersistent_assign);
 end;

function m_tcomponent_name (instance : VALUE) : VALUE; cdecl;
 begin
  Result := VALUE((TObject(instance) as TComponent).Name);
 end;

function m_tcomponent_name_set (instance : VALUE; name : VALUE) : VALUE; cdecl;
 begin
  if ValType(name) = rtSymbol
     then (TObject(instance) as TComponent).Name := ansistring(ID(name))
     else (TObject(instance) as TComponent).Name := ansistring(name);
  Result := name;
 end;

function m_tcomponent_owner (instance : VALUE) : VALUE; cdecl;
 begin
  Result := VALUE((TObject(instance) as TComponent).Owner);
 end;

function m_tcomponent_tag (instance : VALUE) : VALUE; cdecl;
 begin
  Result := VALUE((TObject(instance) as TComponent).Tag);
 end;

function m_tcomponent_tag_set (instance : VALUE; tag : VALUE) : VALUE; cdecl;
 begin
  (TObject(instance) as TComponent).Tag := PtrInt(tag);
  Result := tag;
 end;

function m_tcomponent_index (instance : VALUE) : VALUE; cdecl;
 begin
  Result := VALUE((TObject(instance) as TComponent).ComponentIndex);
 end;

function m_tcomponent_index_set (instance : VALUE; index : VALUE) : VALUE; cdecl;
 begin
  (TObject(instance) as TComponent).ComponentIndex := PtrInt(index);
  Result := index;
 end;

function m_tcomponent_count (instance : VALUE) : VALUE; cdecl;
 begin
  Result := VALUE((TObject(instance) as TComponent).ComponentCount);
 end;

function m_tcomponent_sub (instance : VALUE; index : VALUE) : VALUE; cdecl;
 begin
  try
   case ValType(index) of
        rtFixNum :
          Result := VALUE((TObject(instance) as TComponent).Components[PtrInt(index)]);
        rtSymbol :
          Result := VALUE((TObject(instance) as TComponent).FindComponent(ansistring(ID(index))));
        else
          Result := VALUE((TObject(instance) as TComponent).FindComponent(ansistring(index)));
   end;
  except
   Result := Qnil;
  end;
 end;

function m_tcomponent_each (instance : VALUE) : VALUE; cdecl;
 var
   obj : TComponent;
   item : TObject;
 begin
  obj := TObject(instance) as TComponent;
  for item in obj do
      Result := Yield(VALUE(item));
 end;

function m_tcomponent_parent (instance : VALUE) : VALUE; cdecl;
 var
   obj : TComponent;
 begin
  obj := TObject(instance) as TComponent;
  if obj.HasParent
     then Result := VALUE(obj.GetParentComponent)
     else Result := Qfalse;
 end;

function m_tcomponent_initialize (instance : VALUE; owner : VALUE) : VALUE; cdecl;
 var
   obj : TComponent;
 begin
  obj := TComponentClass(TClass(ValClass(instance))).Create(TObject(owner) as TComponent);
  WrapObject(instance, obj);
  Result := instance;
 end;

function m_tcomponent_insert (instance : VALUE; component : VALUE) : VALUE; cdecl;
 begin
  (TObject(instance) as TComponent).InsertComponent(TObject(component) as TComponent);
  Result := instance;
 end;

function m_tcomponent_remove (instance : VALUE; component : VALUE) : VALUE; cdecl;
 begin
  (TObject(instance) as TComponent).RemoveComponent(TObject(component) as TComponent);
  Result := instance;
 end;

function m_tcomponent_components (instance : VALUE) : VALUE; cdecl;
 var
   arr : array of VALUE;
   obj : TComponent;
   idx : Integer;
 begin
  obj := TObject(instance) as TComponent;
  SetLength(arr, obj.ComponentCount);
  for idx := 0 to obj.ComponentCount - 1 do
      arr[idx] := VALUE(obj.Components[idx]);
  Result := MakeArray(arr);
 end;

function m_tcomponent_to_s (instance : VALUE) : VALUE; cdecl;
 var
   obj : TComponent;
 begin
  obj := TObject(instance) as TComponent;
  Result := VALUE('<' + obj.Name + ' : ' + obj.ClassName + '>');
 end;

procedure TComponentClassHook (cComponent : VALUE);
 begin
  DefineMethod(cComponent, 'name', @m_tcomponent_name);
  DefineMethod(cComponent, 'name=', @m_tcomponent_name_set);
  DefineMethod(cComponent, 'owner', @m_tcomponent_owner);
  DefineMethod(cComponent, 'index', @m_tcomponent_index);
  DefineMethod(cComponent, 'index=', @m_tcomponent_index_set);
  DefineMethod(cComponent, 'parent', @m_tcomponent_parent);
  DefineMethod(cComponent, 'tag', @m_tcomponent_tag);
  DefineMethod(cComponent, 'tag=', @m_tcomponent_tag_set);
  DefineMethod(cComponent, 'count', @m_tcomponent_count);
  DefineMethod(cComponent, '[]', @m_tcomponent_sub);
  DefineMethod(cComponent, 'components', @m_tcomponent_components);
  DefineMethod(cComponent, 'insert', @m_tcomponent_insert);
  DefineMethod(cComponent, 'remove', @m_tcomponent_remove);
  IncludeModule(cComponent, ModEnumerable);
  DefineMethod(cComponent, 'each', @m_tcomponent_each);
  DefineMethod(cComponent, 'initialize', @m_tcomponent_initialize);
 end;

function m_tstrings_add (instance : VALUE; str : VALUE) : VALUE; cdecl;
 begin
  Result := VALUE((TObject(instance) as TStrings).Add(ansistring(str)));
 end;

function m_tstrings_addobject (instance : VALUE; str : VALUE; obj : VALUE) : VALUE; cdecl;
 begin
  Result := VALUE((TObject(instance) as TStrings).AddObject(ansistring(str), TObject(obj)));
 end;

function m_tstrings_addstrings (instance : VALUE; strings : VALUE) : VALUE; cdecl;
 begin
  (TObject(instance) as TStrings).AddStrings(TObject(strings) as TStrings);
  Result := instance;
 end;

function m_tstrings_clear (instance : VALUE) : VALUE; cdecl;
 begin
  (TObject(instance) as TStrings).Clear;
  Result := instance;
 end;

function m_tstrings_delete (instance : VALUE; index : VALUE) : VALUE; cdecl;
 begin
  (TObject(instance) as TStrings).Delete(PtrInt(index));
  Result := instance;
 end;

function m_tstrings_exchange (instance : VALUE; idx1, idx2 : VALUE) : VALUE; cdecl;
 begin
  (TObject(instance) as TStrings).Exchange(PtrInt(idx1), PtrInt(idx2));
  Result := instance;
 end;

function m_tstrings_index_of (instance : VALUE; str : VALUE) : VALUE; cdecl;
 begin
  Result := VALUE((TObject(instance) as TStrings).IndexOf(ansistring(str)));
 end;

function m_tstrings_index_of_name (instance : VALUE; name : VALUE) : VALUE; cdecl;
 begin
  Result := VALUE((TObject(instance) as TStrings).IndexOfName(ansistring(name)));
 end;

function m_tstrings_index_of_object (instance : VALUE; obj : VALUE) : VALUE; cdecl;
 begin
  Result := VALUE((TObject(instance) as TStrings).IndexOfObject(TObject(obj)));
 end;

function m_tstrings_insert (instance : VALUE; index : VALUE; str : VALUE) : VALUE; cdecl;
 begin
  (TObject(instance) as TStrings).Insert(PtrInt(index), ansistring(str));
  Result := instance;
 end;

function m_tstrings_insertobject (instance : VALUE; index : VALUE; str : VALUE; obj : VALUE) : VALUE; cdecl;
 begin
  (TObject(instance) as TStrings).InsertObject(PtrInt(index), ansistring(str), TObject(obj));
  Result := instance;
 end;

function m_tstrings_loadfromfile (instance : VALUE; filename : VALUE) : VALUE; cdecl;
 begin
  (TObject(instance) as TStrings).LoadFromFile(ansistring(filename));
  Result := instance;
 end;

function m_tstrings_loadfromstream (instance : VALUE; stream : VALUE) : VALUE; cdecl;
 begin
  (TObject(instance) as TStrings).LoadFromStream(TObject(stream) as TStream);
  Result := instance;
 end;

function m_tstrings_move (instance : VALUE; idx1, idx2 : VALUE) : VALUE; cdecl;
 begin
  (TObject(instance) as TStrings).Move(PtrInt(idx1), PtrInt(idx2));
  Result := instance;
 end;

function m_tstrings_savetofile (instance : VALUE; filename : VALUE) : VALUE; cdecl;
 begin
  (TObject(instance) as TStrings).SaveToFile(ansistring(filename));
  Result := instance;
 end;

function m_tstrings_savetostream (instance : VALUE; stream : VALUE) : VALUE; cdecl;
 begin
  (TObject(instance) as TStrings).SaveToStream(TObject(stream) as TStream);
  Result := instance;
 end;

function m_tstrings_extractname (instance : VALUE; str : VALUE) : VALUE; cdecl;
 begin
  Result := VALUE((TObject(instance) as TStrings).ExtractName(ansistring(str)));
 end;

function m_tstrings_delimiter (instance : VALUE) : VALUE; cdecl;
 begin
  Result := VALUE((TObject(instance) as TStrings).Delimiter+'');
 end;

procedure TStringsClassHook(cStrings : VALUE);
 begin
  DefineMethod(cStrings, 'add', @m_tstrings_add);
  DefineMethod(cStrings, 'addobject', @m_tstrings_addobject);
  DefineMethod(cStrings, 'addstrings', @m_tstrings_addstrings);
  DefineMethod(cStrings, 'clear', @m_tstrings_clear);
  DefineMethod(cStrings, 'delete', @m_tstrings_delete);
  DefineMethod(cStrings, 'exchange', @m_tstrings_exchange);
  DefineMethod(cStrings, 'index_of', @m_tstrings_index_of);
  DefineMethod(cStrings, 'index_of_name', @m_tstrings_index_of_name);
  DefineMethod(cStrings, 'index_of_object', @m_tstrings_index_of_object);
  DefineMethod(cStrings, 'insert', @m_tstrings_insert);
  DefineMethod(cStrings, 'insert_object', @m_tstrings_insertobject);
  DefineMethod(cStrings, 'loadfromfile', @m_tstrings_loadfromfile);
  DefineMethod(cStrings, 'loadfromstream', @m_tstrings_loadfromstream);
  DefineMethod(cStrings, 'move', @m_tstrings_move);
  DefineMethod(cStrings, 'savetofile', @m_tstrings_savetofile);
  DefineMethod(cStrings, 'savetostream', @m_tstrings_savetostream);
  DefineMethod(cStrings, 'extractname', @m_tstrings_extractname);
 end;

end.

