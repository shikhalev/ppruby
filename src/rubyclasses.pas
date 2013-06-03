unit RubyClasses;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, ctypes, Ruby;

implementation

function prs_assign (obj : VALUE; source : VALUE) : VALUE; cdecl;
 var
   p, sp : TPack;
 begin
 unpack_object(obj, p);
 unpack_object(source, sp);
 (p.obj as TPersistent).Assign(sp.obj as TPersistent);
 result := obj;
 end;

function prs_namepath (obj : VALUE) : VALUE; cdecl;
 var
   p : TPack;
 begin
 unpack_object(obj, p);
 result := p.rb.Str2Val((p.obj as TPersistent).GetNamePath);
 end;

{$hints off}
procedure hookTPersistent (ruby : TRuby; cls : TClass; value : VALUE);
 begin
 ruby.DefineMethod(value, 'assign!', @prs_assign);
 ruby.DefineAttribute(value, 'namepath', @prs_namepath, nil);
 end;
{$hints on}

function cmp_name (obj : VALUE) : VALUE; cdecl;
 var
   p : TPack;
 begin
 unpack_object(obj, p);
 result := p.rb.Str2Sym((p.obj as TComponent).name);
 end;

function cmp_name_set (obj : VALUE; value : VALUE) : VALUE; cdecl;
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

function cmp_get (obj : VALUE; nm_or_i : VALUE) : VALUE; cdecl;
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

function cmp_each (obj : VALUE) : VALUE; cdecl;
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

function cmp_components (obj : VALUE) : VALUE; cdecl;
 var
   p : TPack;
   i : Integer;
 begin
 unpack_object(obj, p);
 result := p.rb.ArrayNew;
 for i := 0 to (p.obj as TComponent).ComponentCount - 1 do
     p.rb.ArrayPush(result, p.rb.Obj2Val((p.obj as TComponent).Components[i]));
 end;

function cmp_owner (obj : VALUE) : VALUE; cdecl;
 var
   p : TPack;
 begin
 unpack_object(obj, p);
 result := p.rb.Obj2Val((p.obj as TComponent).Owner);
 end;

function cmp_hash (obj : VALUE) : VALUE; cdecl;
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

function cmp_index (obj : VALUE) : VALUE; cdecl;
 var
   p : TPack;
 begin
 unpack_object(obj, p);
 result := p.rb.Int2Val((p.obj as TComponent).ComponentIndex);
 end;

function cmp_index_set (obj : VALUE; idx : VALUE) : VALUE; cdecl;
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
 ruby.DefineAttribute(value, 'name', @cmp_name, @cmp_name_set);
 ruby.DefineAttribute(value, 'owner', @cmp_owner, nil);
 ruby.DefineAttribute(value, 'index', @cmp_index, @cmp_index_set);
 ruby.DefineMethod(value, '[]', @cmp_get);
 ruby.DefineMethod(value, 'each', @cmp_each);
 ruby.DefineMethod(value, 'components', @cmp_components);
 ruby.DefineMethod(value, 'to_h', @cmp_hash);
 ruby.DefineAlias(value, 'to_a', 'components');
 end;
{$hints on}

function act_execute (obj : VALUE) : VALUE; cdecl;
 var
   p : TPack;
 begin
 unpack_object(obj, p);
 result := p.rb.Bln2Val((p.obj as TBasicAction).Execute);
 end;

function act_update (obj : VALUE) : VALUE; cdecl;
 var
   p : TPack;
 begin
 unpack_object(obj, p);
 result := p.rb.Bln2Val((p.obj as TBasicAction).Update);
 end;

{$hints off}
procedure hookTBasicAction (ruby : TRuby; cls : TClass; value : VALUE);
 begin
 ruby.DefineMethod(value, 'execute', @act_execute);
 ruby.DefineMethod(value, 'update', @act_update);
 end;
{$hints on}

function col_owner (obj : VALUE) : VALUE; cdecl;
 var
   p : TPack;
 begin
 unpack_object(obj, p);
 result := p.rb.Obj2Val((p.obj as TCollection).Owner);
 end;

function col_add (obj : VALUE) : VALUE; cdecl;
 var
   p : TPack;
 begin
 unpack_object(obj, p);
 result := p.rb.Obj2Val((p.obj as TCollection).Add);
 end;

function col_update (obj : VALUE) : VALUE; cdecl;
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

function col_delete (obj : VALUE; idx : VALUE) : VALUE; cdecl;
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

function col_insert (obj : VALUE; idx : VALUE) : VALUE; cdecl;
 var
   p : TPack;
 begin
 unpack_object(obj, p);
 result := p.rb.Obj2Val((p.obj as TCollection).Insert(p.rb.Val2Int(idx)));
 end;

function col_find (obj : VALUE; id : VALUE) : VALUE; cdecl;
 var
   p : TPack;
 begin
 unpack_object(obj, p);
 result := p.rb.Obj2Val((p.obj as TCollection).FindItemID(p.rb.Val2Int(id)));
 end;

function col_clear (obj : VALUE) : VALUE; cdecl;
 var
   p : TPack;
 begin
 unpack_object(obj, p);
 (p.obj as TCollection).Clear;
 result := obj;
 end;

function col_exchange (obj : VALUE; idx1, idx2 : VALUE) : VALUE; cdecl;
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
 result := sort_rb.Val2Int(sort_rb.Send(sort_rb.Obj2Val(a), '<=>',
                                        [sort_rb.Obj2Val(b)]));
 end;

function rb_collectionitem_compare_block (a, b : TCollectionItem) : Integer;
 begin
 result := sort_rb.Val2Int(sort_rb.Send(sort_bl, 'call', [sort_rb.Obj2Val(a),
                                                          sort_rb.Obj2Val(b)]));
 end;

function col_sort (argc : cint; argv : PVALUE; obj : VALUE) : VALUE; cdecl;
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

function col_each (obj : VALUE) : VALUE; cdecl;
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

function col_count (obj : VALUE) : VALUE; cdecl;
 var
   p : TPack;
 begin
 unpack_object(obj, p);
 result := p.rb.Int2Val((p.obj as TCollection).Count);
 end;

function col_get (obj : VALUE; idx : VALUE) : VALUE; cdecl;
 var
   p : TPack;
 begin
 unpack_object(obj, p);
 result := p.rb.Obj2Val((p.obj as TCollection).Items[p.rb.Val2Int(idx)]);
 end;

function col_set (obj : VALUE; idx, value : VALUE) : VALUE; cdecl;
 var
   p : TPack;
 begin
 unpack_object(obj, p);
 (p.obj as TCollection).Items[p.rb.Val2Int(idx)] := p.rb.Val2Obj(value) as TCollectionItem;
 result := value;
 end;

function col_items (obj : VALUE) : VALUE; cdecl;
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

function col_itemclass (obj : VALUE) : VALUE; cdecl;
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
 ruby.DefineAttribute(value, 'owner', @col_owner, nil);
 ruby.DefineMethod(value, 'add', @col_add);
 ruby.DefineMethod(value, 'delete', @col_delete);
 ruby.DefineMethod(value, 'insert', @col_insert);
 ruby.DefineMethod(value, 'exchange', @col_exchange);
 ruby.DefineMethod(value, 'clear', @col_clear);
 ruby.DefineMethod(value, 'update', @col_update);
 ruby.DefineMethod(value, 'find', @col_find);
 ruby.DefineMethod(value, 'sort!', @col_sort);
 ruby.DefineMethod(value, 'each', @col_each);
 ruby.DefineMethod(value, 'count', @col_count);
 ruby.DefineMethod(value, '[]', @col_get);
 ruby.DefineMethod(value, '[]=', @col_set);
 ruby.DefineMethod(value, 'items', @col_items);
 ruby.DefineMethod(value, 'itemclass', @col_itemclass);
 ruby.DefineAlias(value, 'to_a', 'items');
 end;
{$hints on}

function coi_owner (obj : VALUE) : VALUE; cdecl;
 var
   p : TPack;
 begin
 unpack_object(obj, p);
 result := p.rb.Obj2Val((p.obj as TCollectionItem).Collection);
 end;

function coi_owner_set (obj : VALUE; value : VALUE) : VALUE; cdecl;
 var
   p : TPack;
 begin
 unpack_object(obj, p);
 (p.obj as TCollectionItem).Collection := p.rb.Val2Obj(value) as TCollection;
 result := value;
 end;

function coi_id (obj : VALUE) : VALUE; cdecl;
 var
   p : TPack;
 begin
 unpack_object(obj, p);
 result := p.rb.Int2Val((p.obj as TCollectionItem).ID);
 end;

function coi_index (obj : VALUE) : VALUE; cdecl;
 var
   p : TPack;
 begin
 unpack_object(obj, p);
 result := p.rb.Int2Val((p.obj as TCollectionItem).Index);
 end;

function coi_index_set (obj : VALUE; value : VALUE) : VALUE; cdecl;
 var
   p : TPack;
 begin
 unpack_object(obj, p);
 (p.obj as TCollectionItem).Index := p.rb.Val2Int(value);
 result := value;
 end;

function coi_displayname (obj : VALUE) : VALUE; cdecl;
 var
   p : TPack;
 begin
 unpack_object(obj, p);
 result := p.rb.Str2Val((p.obj as TCollectionItem).DisplayName);
 end;

function coi_displayname_set (obj : VALUE; value : VALUE) : VALUE; cdecl;
 var
   p : TPack;
 begin
 unpack_object(obj, p);
 (p.obj as TCollectionItem).DisplayName := p.rb.Val2Str(value);
 result := value;
 end;

{$hints off}
procedure hookTCollectionItem (ruby : TRuby; cls : TClass; value : VALUE);
 begin
 ruby.DefineAttribute(value, 'owner', @coi_owner, @coi_owner_set);
 ruby.DefineAttribute(value, 'id', @coi_id, nil);
 ruby.DefineAttribute(value, 'index', @coi_index, @coi_index_set);
 ruby.DefineAttribute(value, 'displayname', @coi_displayname, @coi_displayname_set);
 end;
{$hints on}

function sts_add (obj : VALUE; str : VALUE) : VALUE; cdecl;
 var
   p : TPack;
 begin
 unpack_object(obj, p);
 result := p.rb.Int2Val((p.obj as TStrings).Add(p.rb.Val2Str(str)));
 end;

function sts_addobject (obj : VALUE; str, obj2 : VALUE) : VALUE; cdecl;
 var
   p : TPack;
 begin
 unpack_object(obj, p);
 result := p.rb.Int2Val((p.obj as TStrings).AddObject(p.rb.Val2Str(str),
                                                      p.rb.Val2Obj(obj2)));
 end;

procedure hookTStrings (ruby : TRuby; cls : TClass; value : VALUE);
 begin
 ruby.DefineMethod(value, 'add', @sts_add);
 ruby.DefineMethod(value, 'addobject', @sts_addobject);
 end;

initialization
 InitCriticalSection(sort_cs);
 TRuby.AddRegisterClassHook(TPersistent, @hookTPersistent);
 TRuby.AddRegisterClassHook(TComponent, @hookTComponent);
 TRuby.AddRegisterClassHook(TBasicAction, @hookTBasicAction);
 TRuby.AddRegisterClassHook(TCollection, @hookTCollection);
 TRuby.AddRegisterClassHook(TCollectionItem, @hookTCollectionItem);
 TRuby.AddRegisterClassHook(TStrings, @hookTStrings);
finalization
 DoneCriticalsection(sort_cs);
end.

