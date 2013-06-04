unit RubyMenus;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, ctypes, Menus, Ruby;

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

{$hints off}
procedure hookTMenu (ruby : TRuby; cls : TClass; value : VALUE);
 begin
 ruby.DefineAttribute(value, 'parent', @menu_parent, @menu_parent_set);
 end;
{$hints on}

function mni_find (slf : VALUE; caption : VALUE) : VALUE; cdecl;
 var
   p : TPack;
 begin
  unpack_object(slf, p);
  try
    result := p.rb.Obj2Val((p.obj as TMenuItem).Find(p.rb.Val2Str(caption)));
  except
    on e : Exception do
       p.rb.Error(e);
  end;
 end;

function mni_index_of (slf : VALUE; item : VALUE) : VALUE; cdecl;
 var
   p : TPack;
   m : TMenuItem;
 begin
  unpack_object(slf, p);
  try
    m := p.obj as TMenuItem;
    if p.rb.IsData(item)
       then result := p.rb.Int2Val(m.IndexOf(TMenuItem(p.rb.Val2Obj(item))))
       else result := p.rb.Int2Val(m.IndexOfCaption(p.rb.Val2Str(item)));
  except
    on e : Exception do
       p.rb.Error(e);
  end;
 end;

function mni_add (argc : cint; argv : PVALUE; slf : VALUE) : VALUE; cdecl;
 var
   p : TPack;
   i : Integer;
   m : TMenuItem;
 begin
  unpack_object(slf, p);
  try
    m := p.obj as TMenuItem;
    if argc = 0
       then m.AddSeparator
       else for i := 0 to argc - 1 do
                if argv[i] = p.rb.Qnil
                   then m.AddSeparator
                   else m.Add(TMenuItem(p.rb.Val2Obj(argv[i])));
    result := slf;
  except
    on e : Exception do
       p.rb.Error(e);
  end;
 end;

{$hints off}
procedure hookTMenuItem (ruby : TRuby; cls : TClass; value : VALUE);
 begin
  ruby.DefineMethod(value, 'find',     @mni_find);
  ruby.DefineMethod(value, 'index_of', @mni_index_of);
  {$note TODO: ... }
 end;
{$hints on}

initialization
 TRuby.AddRegisterClassHook(TMenu, @hookTMenu);
end.

