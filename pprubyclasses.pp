unit ppRubyClasses;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, ppRuby;

procedure TObjectClassHook (cObject : VALUE);
procedure TPersistentClassHook (cPersistent : VALUE);
procedure TComponentClassHook (cComponent : VALUE);

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
  DefineMethod(cComponent, 'insert', @m_tcomponent_insert);
  DefineMethod(cComponent, 'remove', @m_tcomponent_remove);
  IncludeModule(cComponent, ModEnumerable);
  DefineMethod(cComponent, 'each', @m_tcomponent_each);
  DefineMethod(cComponent, 'initialize', @m_tcomponent_initialize)
 end;

end.

