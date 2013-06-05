unit RubyMenus;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, ctypes, Menus, Ruby;

implementation

type
  TMenuChangeCompanion = class(TEventCompanion)
    procedure Handler (Sender: TObject; Source: TMenuItem;
                                    Rebuild: Boolean);
  end;

{ TMenuChangeCompanion }

procedure TMenuChangeCompanion.Handler(Sender : TObject; Source : TMenuItem;
    Rebuild : Boolean);
 begin
  Call([ruby.Obj2Val(Sender), ruby.Obj2Val(Source), ruby.Bln2Val(Rebuild)]);
 end;

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

function mnu_items (slf : VALUE) : VALUE; cdecl;
 var
   p : TPack;
 begin
  unpack_object(slf, p);
  try
    result := p.rb.Obj2Val((p.obj as TMenu).Items);
  except
    on e : Exception do
       p.rb.Error(e);
  end;
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
       then result := p.rb.Int2Val(m.IndexOf(p.rb.Val2Obj(item) as TMenuItem))
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

function mni_delete (slf : VALUE; item : VALUE) : VALUE; cdecl;
 var
   p : TPack;
   m : TMenuItem;
   i : Integer;
 begin
  unpack_object(slf, p);
  try
    m := p.obj as TMenuItem;
    if p.rb.IsFixnum(item)
       then begin
            i := p.rb.Val2Int(item);
            result := p.rb.Obj2Val(m.Items[i]);
            m.Delete(i);
            end
       else begin
            result := item;
            (p.obj as TMenuItem).Remove(p.rb.Val2Obj(item) as TMenuItem);
            end;
  except
    on e : Exception do
       p.rb.Error(e);
  end;
 end;

function mni_insert (slf : VALUE; idx : VALUE; item : VALUE) : VALUE; cdecl;
 var
   p : TPack;
   i : Integer;
   m, s : TMenuItem;
 begin
  unpack_object(slf, p);
  try
    m := p.obj as TMenuItem;
    i := p.rb.Val2Int(idx);
    if item = p.rb.Qnil
       then begin
            s := TMenuItem.Create(m);
            s.Caption := cLineCaption;
            end
       else s := TMenuItem(p.rb.Val2Obj(item));
    m.Insert(i, s);
    result := slf;
  except
    on e : Exception do
       p.rb.Error(e);
  end;
 end;

function mni_click (slf : VALUE) : VALUE; cdecl;
 var
   p : TPack;
 begin
  unpack_object(slf, p);
  try
    (p.obj as TMenuItem).Click;
    result := slf;
  except
    on e : Exception do
       p.rb.Error(e);
  end;
 end;

function mni_clear (slf : VALUE) : VALUE; cdecl;
 var
   p : TPack;
 begin
  unpack_object(slf, p);
  try
    (p.obj as TMenuItem).Clear;
    result := slf;
  except
    on e : Exception do
       p.rb.Error(e);
  end;
 end;

function mni_ischeck (slf : VALUE) : VALUE; cdecl;
 var
   p : TPack;
 begin
  unpack_object(slf, p);
  try
    result := p.rb.Bln2Val((p.obj as TMenuItem).IsCheckItem);
  except
    on e : Exception do
       p.rb.Error(e);
  end;
 end;

function mni_isline (slf : VALUE) : VALUE; cdecl;
 var
   p : TPack;
 begin
  unpack_object(slf, p);
  try
    result := p.rb.Bln2Val((p.obj as TMenuItem).IsLine);
  except
    on e : Exception do
       p.rb.Error(e);
  end;
 end;

function mni_isinbar (slf : VALUE) : VALUE; cdecl;
 var
   p : TPack;
 begin
  unpack_object(slf, p);
  try
    result := p.rb.Bln2Val((p.obj as TMenuItem).IsInMenuBar);
  except
    on e : Exception do
       p.rb.Error(e);
  end;
 end;

function mni_count (slf : VALUE) : VALUE; cdecl;
 var
   p : TPack;
 begin
  unpack_object(slf, p);
  try
    result := p.rb.Int2Val((p.obj as TMenuItem).Count);
  except
    on e : Exception do
       p.rb.Error(e);
  end;
 end;

function mni_items (slf : VALUE) : VALUE; cdecl;
 var
   p : TPack;
   i : Integer;
   m : TMenuItem;
 begin
  unpack_object(slf, p);
  try
    m := p.obj as TMenuItem;
    result := p.rb.ArrayNew;
    for i := 0 to m.Count - 1 do
        p.rb.ArrayPush(result, p.rb.Obj2Val(m.Items[i]));
  except
    on e : Exception do
       p.rb.Error(e);
  end;
 end;

function mni_index (slf : VALUE) : VALUE; cdecl;
 var
   p : TPack;
 begin
  unpack_object(slf, p);
  try
    result := p.rb.Int2Val((p.obj as TMenuItem).MenuIndex);
  except
    on e : Exception do
       p.rb.Error(e);
  end;
 end;

function mni_index_set (slf : VALUE; value : VALUE) : VALUE; cdecl;
 var
   p : TPack;
 begin
  unpack_object(slf, p);
  try
    (p.obj as TMenuItem).MenuIndex := p.rb.Val2Int(value);
    result := value;
  except
    on e : Exception do
       p.rb.Error(e);
  end;
 end;

function mni_menu (slf : VALUE) : VALUE; cdecl;
 var
   p : TPack;
 begin
  unpack_object(slf, p);
  try
    result := p.rb.Obj2Val((p.obj as TMenuItem).Menu);
  except
    on e : Exception do
       p.rb.Error(e);
  end;
 end;

function mni_parent (slf : VALUE) : VALUE; cdecl;
 var
   p : TPack;
 begin
  unpack_object(slf, p);
  try
    result := p.rb.Obj2Val((p.obj as TMenuItem).Parent);
  except
    on e : Exception do
       p.rb.Error(e);
  end;
 end;

function mni_each (slf : VALUE) : VALUE; cdecl;
 var
   p : TPack;
   m : TMenuItem;
   i : Integer;
 begin
  unpack_object(slf, p);
  try
    if p.rb.BlockGiven
       then begin
            m := p.obj as TMenuItem;
            for i := 0 to m.Count - 1 do
                p.rb.Yield(p.rb.Obj2Val(m.Items[i]));
            result := slf;
            end
       else result := p.rb.Send(p.rb.cEnumerator, 'new', [slf]);
  except
    on e : Exception do
       p.rb.Error(e);
  end;
 end;

{$hints off}
procedure hookTMenuItem (ruby : TRuby; cls : TClass; value : VALUE);
 begin
  ruby.Include(value, ruby.mEnumerable);
  ruby.DefineAttribute(value, 'index',  @mni_index,  @mni_index_set);
  ruby.DefineAttribute(value, 'menu',   @mni_menu,   nil);
  ruby.DefineAttribute(value, 'parent', @mni_parent, nil);
  ruby.DefineMethod(value, 'find',     @mni_find);
  ruby.DefineMethod(value, 'index_of', @mni_index_of);
  ruby.DefineMethod(value, 'add',      @mni_add);
  ruby.DefineMethod(value, 'delete',   @mni_delete);
  ruby.DefineMethod(value, 'insert',   @mni_insert);
  ruby.DefineMethod(value, 'clear',    @mni_clear);
  ruby.DefineMethod(value, 'click',    @mni_click);
  ruby.DefineMethod(value, 'check?',   @mni_ischeck);
  ruby.DefineMethod(value, 'line?',    @mni_isline);
  ruby.DefineMethod(value, 'in_bar?',  @mni_isinbar);
  ruby.DefineMethod(value, 'count',    @mni_count);
  ruby.DefineMethod(value, 'items',    @mni_items);
  ruby.DefineMethod(value, 'each',     @mni_each);
  ruby.DefineAlias(value, 'to_a', 'items');
 end;
{$hints on}

function ppm_popup (argc : cint; argv : PVALUE; slf : VALUE) : VALUE; cdecl;
 var
   p : TPack;
   m : TPopupMenu;
 begin
  unpack_object(slf, p);
  try
    m := p.obj as TPopupMenu;
    if argc = 2
       then m.PopUp(p.rb.Val2Int(argv[0]), p.rb.Val2Int(argv[1]))
       else m.PopUp;
    result := slf;
  except
    on e : Exception do
       p.rb.Error(e);
  end;
 end;

{$hints off}
procedure hookTPopupMenu (ruby : TRuby; cls : TClass; value : VALUE);
 begin
  ruby.DefineMethod(value, 'popup', @ppm_popup);
 end;
{$hints on}

initialization
 TRuby.AddRegisterClassHook(TMenu,      @hookTMenu);
 TRuby.AddRegisterClassHook(TMenuItem,  @hookTMenuItem);
 TRuby.AddRegisterClassHook(TPopupMenu, @hookTPopupMenu);
end.

