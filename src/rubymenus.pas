unit RubyMenus;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Menus, Ruby;

implementation

function menu_parent (slf : VALUE) : VALUE; cdecl;
  var
    p : TPack;
  begin
  unpack_object(slf, p);
  result := p.rb.Obj2Val((p.obj as TMenu).Parent);
  end;

function menu_parent_set (slf : VALUE; value : VALUE) : VALUE; cdecl;
  var
    p : TPack;
  begin
  unpack_object(slf, p);
  (p.obj as TMenu).Parent := p.rb.Val2Obj(value) as TComponent;
  result := value;
  end;

procedure hookTMenu (ruby : TRuby; cls : TClass; value : VALUE);
 begin
 ruby.DefineAttribute(value, 'parent', @menu_parent, @menu_parent_set);
 end;

initialization
 TRuby.AddRegisterClassHook(TMenu, @hookTMenu);
end.

