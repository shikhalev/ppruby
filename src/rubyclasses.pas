unit RubyClasses;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, ctypes, Ruby;

implementation

function persistent_assign (obj : VALUE; source : VALUE) : VALUE; cdecl;
 var
   p, sp : TPack;
 begin
 unpack_object(obj, p);
 unpack_object(source, sp);
 (p.obj as TPersistent).Assign(sp.obj as TPersistent);
 result := obj;
 end;

function persistent_namepath (obj : VALUE) : VALUE; cdecl;
 var
   p : TPack;
 begin
 unpack_object(obj, p);
 result := p.rb.Str2Val((p.obj as TPersistent).GetNamePath);
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
   p : TPack;
 begin
 unpack_object(obj, p);
 result := p.rb.Str2Sym((p.obj as TComponent).name);
 end;

function component_setname (obj : VALUE; value : VALUE) : VALUE; cdecl;
 var
   p : TPack;
   n : UTF8String;
 begin
 unpack_object(obj, p);
 if p.rb.IsSymbol(value)
    then n := p.rb.Sym2Str(value)
    else n := p.rb.Val2Str(value);
 (p.obj as TComponent).Name := n;
 result := value;
 end;

function component_get (obj : VALUE; nm_or_i : VALUE) : VALUE; cdecl;
 var
   p : TPack;
   n : UTF8String;
   i : PtrInt;
 begin
 n := '';
 unpack_object(obj, p);
 if p.rb.IsFixnum(nm_or_i)
    then i := p.rb.Val2Int(nm_or_i)
    else if p.rb.IsSymbol(nm_or_i)
            then n := p.rb.Sym2Str(nm_or_i)
            else n := p.rb.Val2Str(nm_or_i);
 try
   if n = ''
      then result := p.rb.Obj2Val((p.obj as TComponent).Components[i])
      else result := p.rb.Obj2Val((p.obj as TComponent).FindComponent(n));
 except
   result := p.rb.Qnil;
 end;
 end;

function component_each (obj : VALUE) : VALUE; cdecl;
 var
   p : TPack;
   i : Integer;
 begin
 unpack_object(obj, p);
 if p.rb.BlockGiven
    then begin
         for i := 0 to (p.obj as TComponent).ComponentCount - 1 do
             p.rb.Yield(p.rb.Obj2Val((p.obj as TComponent).Components[i]));
         result := obj;
         end
    else result := p.rb.Send(p.rb.cEnumerator, 'new', [obj]);
 end;

function component_components (obj : VALUE) : VALUE; cdecl;
 var
   p : TPack;
   i : Integer;
 begin
 unpack_object(obj, p);
 result := p.rb.ArrayNew;
 for i := 0 to (p.obj as TComponent).ComponentCount - 1 do
     p.rb.ArrayPush(result, p.rb.Obj2Val((p.obj as TComponent).Components[i]));
 end;

function component_owner (obj : VALUE) : VALUE; cdecl;
 var
   p : TPack;
 begin
 unpack_object(obj, p);
 result := p.rb.Obj2Val((p.obj as TComponent).Owner);
 end;

function component_to_h (obj : VALUE) : VALUE; cdecl;
 var
   p : TPack;
   i : Integer;
   c : TComponent;
 begin
 unpack_object(obj, p);
 result := p.rb.HashNew;
 for i := 0 to (p.obj as TComponent).ComponentCount - 1 do
     begin
     c := (p.obj as TComponent).Components[i];
     p.rb.HashSet(result, p.rb.Str2Sym(c.Name), p.rb.Obj2Val(c));
     end;
 end;

function component_index (obj : VALUE) : VALUE; cdecl;
 var
   p : TPack;
 begin
 unpack_object(obj, p);
 result := p.rb.Int2Val((p.obj as TComponent).ComponentIndex);
 end;

function component_index_set (obj : VALUE; idx : VALUE) : VALUE; cdecl;
 var
   p : TPack;
 begin
 unpack_object(obj, p);
 (p.obj as TComponent).ComponentIndex := p.rb.Val2Int(idx);
 result := idx;
 end;

{$hints off}
procedure hookTComponent (ruby : TRuby; cls : TClass; value : VALUE);
 begin
 ruby.Include(value, ruby.mEnumerable);
 ruby.Send(value, 'attr_accessor', [ruby.Str2Sym('name')]);
 ruby.DefineMethod(value, 'name', @component_name);
 ruby.DefineMethod(value, 'name=', @component_setname);
 ruby.DefineMethod(value, '[]', @component_get);
 ruby.DefineMethod(value, 'each', @component_each);
 ruby.DefineMethod(value, 'components', @component_components);
 ruby.DefineAlias(value, 'to_a', 'components');
 ruby.Send(value, 'attr_reader', [ruby.Str2Sym('owner')]);
 ruby.DefineMethod(value, 'owner', @component_owner);
 ruby.DefineMethod(value, 'to_h', @component_to_h);
 ruby.Send(value, 'attr_accessor', [ruby.Str2Sym('index')]);
 ruby.DefineMethod(value, 'index', @component_index);
 ruby.DefineMethod(value, 'index=', @component_index_set);
 end;
{$hints on}

function basicaction_execute (obj : VALUE) : VALUE; cdecl;
 var
   p : TPack;
 begin
 unpack_object(obj, p);
 result := p.rb.Bln2Val((p.obj as TBasicAction).Execute);
 end;

function basicaction_update (obj : VALUE) : VALUE; cdecl;
 var
   p : TPack;
 begin
 unpack_object(obj, p);
 result := p.rb.Bln2Val((p.obj as TBasicAction).Update);
 end;

{$hints off}
procedure hookTBasicAction (ruby : TRuby; cls : TClass; value : VALUE);
 begin
 ruby.DefineMethod(value, 'execute', @basicaction_execute);
 ruby.DefineMethod(value, 'update', @basicaction_update);
 end;
{$hints on}

function collection_owner (obj : VALUE) : VALUE; cdecl;
 var
   p : TPack;
 begin
 unpack_object(obj, p);
 result := p.rb.Obj2Val((p.obj as TCollection).Owner);
 end;

function collection_add (obj : VALUE) : VALUE; cdecl;
 var
   p : TPack;
 begin
 unpack_object(obj, p);
 result := p.rb.Obj2Val((p.obj as TCollection).Add);
 end;

function collection_update (obj : VALUE) : VALUE; cdecl;
 var
   p : TPack;
   c : TCollection;
 begin
 unpack_object(obj, p);
 if p.rb.BlockGiven
    then begin
         c := p.obj as TCollection;
         c.BeginUpdate;
         result := p.rb.Yield(obj);
         c.EndUpdate;
         end
    else result := p.rb.Qnil;
 end;

function collection_delete (obj : VALUE; idx : VALUE) : VALUE; cdecl;
 var
   p : TPack;
   c : TCollection;
   i : PtrInt;
 begin
 unpack_object(obj, p);
 c := p.obj as TCollection;
 i := p.rb.Val2Int(idx);
 result := p.rb.Obj2Val(c.Items[i]);
 c.Delete(i);
 end;

function collection_insert (obj : VALUE; idx : VALUE) : VALUE; cdecl;
 var
   p : TPack;
 begin
 unpack_object(obj, p);
 result := p.rb.Obj2Val((p.obj as TCollection).Insert(p.rb.Val2Int(idx)));
 end;

function collection_find (obj : VALUE; id : VALUE) : VALUE; cdecl;
 var
   p : TPack;
 begin
 unpack_object(obj, p);
 result := p.rb.Obj2Val((p.obj as TCollection).FindItemID(p.rb.Val2Int(id)));
 end;

function collection_clear (obj : VALUE) : VALUE; cdecl;
 var
   p : TPack;
 begin
 unpack_object(obj, p);
 (p.obj as TCollection).Clear;
 result := obj;
 end;

function collection_exchange (obj : VALUE; idx1, idx2 : VALUE) : VALUE; cdecl;
 var
   p : TPack;
 begin
 unpack_object(obj, p);
 (p.obj as TCollection).Exchange(p.rb.Val2Int(idx1), p.rb.Val2Int(idx2));
 result := obj;
 end;

var
  sort_rb : TRuby;
  sort_bl : VALUE;
  sort_cs : TRTLCriticalSection;

function rb_collectionitem_compare (a, b : TCollectionItem) : Integer;
 begin
 result := sort_rb.Val2Int(sort_rb.Send(sort_rb.Obj2Val(a), '<=>', [sort_rb.Obj2Val(b)]));
 end;

function rb_collectionitem_compare_block (a, b : TCollectionItem) : Integer;
 begin
 result := sort_rb.Val2Int(sort_rb.Send(sort_bl, 'call', [sort_rb.Obj2Val(a), sort_rb.Obj2Val(b)]));
 end;

function collection_sort (argc : cint; argv : PVALUE; obj : VALUE) : VALUE; cdecl;
 var
   p : TPack;
   c : TCollection;
 begin
 EnterCriticalsection(sort_cs);
 try
   unpack_object(obj, p);
   sort_rb := p.rb;
   c := p.obj as TCollection;
   p.rb.rb_scan_args(argc, argv, '&', @sort_bl);
   if sort_bl = p.rb.Qnil
      then c.Sort(@rb_collectionitem_compare)
      else c.Sort(@rb_collectionitem_compare_block);
   result := obj;
 finally
   LeaveCriticalsection(sort_cs);
 end;
 end;

function collection_each (obj : VALUE) : VALUE; cdecl;
 var
   p : TPack;
   i : Integer;
   c : TCollection;
 begin
 unpack_object(obj, p);
 c := p.obj as TCollection;
 if p.rb.BlockGiven
    then begin
         for i := 0 to c.Count - 1 do
             p.rb.Yield(p.rb.Obj2Val(c.Items[i]));
         result := obj;
         end
    else result := p.rb.Send(p.rb.cEnumerator, 'new', [obj]);
 end;

function collection_count (obj : VALUE) : VALUE; cdecl;
 var
   p : TPack;
 begin
 unpack_object(obj, p);
 result := p.rb.Int2Val((p.obj as TCollection).Count);
 end;

function collection_get (obj : VALUE; idx : VALUE) : VALUE; cdecl;
 var
   p : TPack;
 begin
 unpack_object(obj, p);
 result := p.rb.Obj2Val((p.obj as TCollection).Items[p.rb.Val2Int(idx)]);
 end;

function collection_set (obj : VALUE; idx, value : VALUE) : VALUE; cdecl;
 var
   p : TPack;
 begin
 unpack_object(obj, p);
 (p.obj as TCollection).Items[p.rb.Val2Int(idx)] := p.rb.Val2Obj(value) as TCollectionItem;
 result := value;
 end;

function collection_items (obj : VALUE) : VALUE; cdecl;
 var
   p : TPack;
   c : TCollection;
   i : Integer;
 begin
 unpack_object(obj, p);
 result := p.rb.ArrayNew;
 c := p.obj as TCollection;
 for i := 0 to c.Count - 1 do
     p.rb.ArrayPush(result, p.rb.Obj2Val(c.Items[i]));
 end;

function collection_itemclass (obj : VALUE) : VALUE; cdecl;
 var
   p : TPack;
 begin
 unpack_object(obj, p);
 result := p.rb.Cls2Val((p.obj as TCollection).ItemClass);
 end;

{$hints off}
procedure hookTCollection (ruby : TRuby; cls : TClass; value : VALUE);
 begin
 ruby.Include(value, ruby.mEnumerable);
 ruby.Send(value, 'attr_reader', [ruby.Str2Sym('owner')]);
 ruby.DefineMethod(value, 'owner', @collection_owner);
 ruby.DefineMethod(value, 'add', @collection_add);
 ruby.DefineMethod(value, 'delete', @collection_delete);
 ruby.DefineMethod(value, 'insert', @collection_insert);
 ruby.DefineMethod(value, 'exchange', @collection_exchange);
 ruby.DefineMethod(value, 'clear', @collection_clear);
 ruby.DefineMethod(value, 'update', @collection_update);
 ruby.DefineMethod(value, 'find', @collection_find);
 ruby.DefineMethod(value, 'sort!', @collection_sort);
 ruby.DefineMethod(value, 'each', @collection_each);
 ruby.DefineMethod(value, 'count', @collection_count);
 ruby.DefineMethod(value, '[]', @collection_get);
 ruby.DefineMethod(value, '[]=', @collection_set);
 ruby.DefineMethod(value, 'items', @collection_items);
 ruby.DefineAlias(value, 'to_a', 'items');
 ruby.DefineMethod(value, 'itemclass', @collection_itemclass);
 end;
{$hints on}

{$hints off}
procedure hookTCollectionItem (ruby : TRuby; cls : TClass; value : VALUE);
 begin

 end;
{$hints on}

initialization
 InitCriticalSection(sort_cs);
 TRuby.AddRegisterClassHook(TPersistent, @hookTPersistent);
 TRuby.AddRegisterClassHook(TComponent, @hookTComponent);
 TRuby.AddRegisterClassHook(TBasicAction, @hookTBasicAction);
 TRuby.AddRegisterClassHook(TCollection, @hookTCollection);
 TRuby.AddRegisterClassHook(TCollectionItem, @hookTCollectionItem);
finalization
 DoneCriticalsection(sort_cs);
end.

