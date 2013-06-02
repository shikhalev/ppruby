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
 result := pack.ruby.StrNew((pack.obj as TPersistent).GetNamePath);
 end;

{$hints off}
procedure hookTPersistent (ruby : TRuby; cls : TClass; value : VALUE);
 begin
 ruby.DefineMethod(value, 'assign!', @persistent_assign);
 ruby.DefineMethod(value, 'namepath', @persistent_namepath);
 end;
{$hints on}

function component_name (obj : VALUE) : VALUE; cdecl;
 begin
 // todo: to Symbol
 {$warning TODO}
 end;

function component_setname (obj : VALUE; value : VALUE) : VALUE; cdecl;
 begin

 {$warning TODO}
 end;

function component_get (obj : VALUE; nm_or_i : VALUE) : VALUE; cdecl;
 begin

 {$warning TODO}
 end;

function component_each (obj : VALUE) : VALUE; cdecl;
 begin

 {$warning TODO}
 end;

function component_components (obj : VALUE) : VALUE; cdecl;
 begin

 {$warning TODO}
 end;

function component_owner (obj : VALUE) : VALUE; cdecl;
 begin

 {$warning TODO}
 end;

{$hints off}
procedure hookTComponent (ruby : TRuby; cls : TClass; value : VALUE);
 begin
 ruby.Include(value, ruby.mEnumerable);
 ruby.DefineMethod(value, 'name', @component_name);
 ruby.DefineMethod(value, 'name=', @component_setname);
 ruby.DefineMethod(value, 'each', @component_each);
 ruby.DefineMethod(value, 'components', @component_components);
 ruby.DefineAlias(value, 'to_a', 'components');
 ruby.Send(value, 'attr_reader', [ruby.Str2SymVal('owner')]);
 ruby.DefineMethod(value, 'owner', @component_owner);
 end;
{$hints on}

initialization
 TRuby.AddRegisterClassHook(TPersistent, @hookTPersistent);
 TRuby.AddRegisterClassHook(TComponent, @hookTComponent);
end.

