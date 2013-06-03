unit RubyClasses;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Ruby;

implementation

function persistent_assign (obj : VALUE; source : VALUE) : VALUE; cdecl;
 var
   pack, spack : TObjectPack;
 begin
 unpack_object(obj, pack);
 unpack_object(source, spack);
 (pack.obj as TPersistent).Assign(spack.obj as TPersistent);
 result := obj;
 end;

function persistent_namepath (obj : VALUE) : VALUE; cdecl;
 var
   pack : TObjectPack;
 begin
 unpack_object(obj, pack);
 result := pack.ruby.Str2Val((pack.obj as TPersistent).GetNamePath);
 end;

{$hints off}
procedure hookTPersistent (ruby : TRuby; cls : TClass; value : VALUE);
 begin
 ruby.DefineMethod(value, 'assign!', @persistent_assign);
 ruby.DefineMethod(value, 'namepath', @persistent_namepath);
 end;
{$hints on}

function component_name (obj : VALUE) : VALUE; cdecl;
 var
   pack : TObjectPack;
 begin
 unpack_object(obj, pack);
 result := pack.ruby.Str2Sym((pack.obj as TComponent).name);
 end;

function component_setname (obj : VALUE; value : VALUE) : VALUE; cdecl;
 var
   pack : TObjectPack;
   name : UTF8String;
 begin
 unpack_object(obj, pack);
 if pack.ruby.IsSymbol(value)
    then name := pack.ruby.Sym2Str(value)
    else name := pack.ruby.Val2Str(value);
 (pack.obj as TComponent).Name := name;
 result := value;
 end;

function component_get (obj : VALUE; nm_or_i : VALUE) : VALUE; cdecl;
 var
   pack : TObjectPack;
   name : UTF8String;
   idx : PtrInt;
 begin
 name := '';
 unpack_object(obj, pack);
 if pack.ruby.IsFixnum(nm_or_i)
    then idx := pack.ruby.Val2Int(nm_or_i)
    else if pack.ruby.IsSymbol(nm_or_i)
            then name := pack.ruby.Sym2Str(nm_or_i)
            else name := pack.ruby.Val2Str(nm_or_i);
 try
   if name = ''
      then result := pack.ruby.Obj2Val((pack.obj as TComponent).Components[idx])
      else result := pack.ruby.Obj2Val((pack.obj as TComponent).FindComponent(name));
 except
   result := pack.ruby.Qnil;
 end;
 end;

function component_each (obj : VALUE) : VALUE; cdecl;
 var
   pack : TObjectPack;
   idx : Integer;
 begin
 unpack_object(obj, pack);
 if pack.ruby.BlockGiven
    then begin
         for idx := 0 to (pack.obj as TComponent).ComponentCount - 1 do
             pack.ruby.Yield(pack.ruby.Obj2Val((pack.obj as TComponent).Components[idx]));
         result := obj;
         end
    else result := pack.ruby.Send(pack.ruby.cEnumerator, 'new', [obj]);
 end;

function component_components (obj : VALUE) : VALUE; cdecl;
 var
   pack : TObjectPack;
   data : array of VALUE;
   idx : Integer;
 begin
 unpack_object(obj, pack);
 SetLength(data, (pack.obj as TComponent).ComponentCount);
 for idx := 0 to (pack.obj as TComponent).ComponentCount - 1 do
     data[idx] := pack.ruby.Obj2Val((pack.obj as TComponent).Components[idx]);
 result := pack.ruby.MkArray(data);
 SetLength(data, 0);     {$warning monster bug}
 raise exception.create('WTF!');
 end;

function component_owner (obj : VALUE) : VALUE; cdecl;
 var
   pack : TObjectPack;
 begin
 unpack_object(obj, pack);
 result := pack.ruby.Obj2Val((pack.obj as TComponent).Owner);
 end;

function component_to_h (obj : VALUE) : VALUE; cdecl;
 var
   pack : TObjectPack;
   idx : Integer;
   child : TComponent;
 begin
 unpack_object(obj, pack);
 result := pack.ruby.HashNew;
 for idx := 0 to (pack.obj as TComponent).ComponentCount - 1 do
     begin
     child := (pack.obj as TComponent).Components[idx];
     pack.ruby.HashSet(result, pack.ruby.Str2Sym(child.Name), pack.ruby.Obj2Val(child));
     end;
 end;

{$hints off}
procedure hookTComponent (ruby : TRuby; cls : TClass; value : VALUE);
 begin
 ruby.Include(value, ruby.mEnumerable);
 ruby.DefineMethod(value, 'name', @component_name);
 ruby.DefineMethod(value, 'name=', @component_setname);
 ruby.DefineMethod(value, '[]', @component_get);
 ruby.DefineMethod(value, 'each', @component_each);
 ruby.DefineMethod(value, 'components', @component_components);
 ruby.DefineAlias(value, 'to_a', 'components');
 ruby.Send(value, 'attr_reader', [ruby.Str2Sym('owner')]);
 ruby.DefineMethod(value, 'owner', @component_owner);
 ruby.DefineMethod(value, 'to_h', @component_to_h);
 end;
{$hints on}

initialization
 TRuby.AddRegisterClassHook(TPersistent, @hookTPersistent);
 TRuby.AddRegisterClassHook(TComponent, @hookTComponent);
end.

