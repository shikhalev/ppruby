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
 ruby.DefineAttribute(value, 'name',  @cmp_name,  @cmp_name_set);
 ruby.DefineAttribute(value, 'owner', @cmp_owner, nil);
 ruby.DefineAttribute(value, 'index', @cmp_index, @cmp_index_set);
 ruby.DefineMethod(value, '[]',         @cmp_get);
 ruby.DefineMethod(value, 'each',       @cmp_each);
 ruby.DefineMethod(value, 'components', @cmp_components);
 ruby.DefineMethod(value, 'to_h',       @cmp_hash);
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
 ruby.DefineMethod(value, 'update',  @act_update);
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
 ruby.DefineMethod(value, 'add',       @col_add);
 ruby.DefineMethod(value, 'delete',    @col_delete);
 ruby.DefineMethod(value, 'insert',    @col_insert);
 ruby.DefineMethod(value, 'exchange',  @col_exchange);
 ruby.DefineMethod(value, 'clear',     @col_clear);
 ruby.DefineMethod(value, 'update',    @col_update);
 ruby.DefineMethod(value, 'find',      @col_find);
 ruby.DefineMethod(value, 'sort!',     @col_sort);
 ruby.DefineMethod(value, 'each',      @col_each);
 ruby.DefineMethod(value, 'count',     @col_count);
 ruby.DefineMethod(value, '[]',        @col_get);
 ruby.DefineMethod(value, '[]=',       @col_set);
 ruby.DefineMethod(value, 'items',     @col_items);
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
 ruby.DefineAttribute(value, 'owner',       @coi_owner,       @coi_owner_set);
 ruby.DefineAttribute(value, 'id',          @coi_id,          nil);
 ruby.DefineAttribute(value, 'index',       @coi_index,       @coi_index_set);
 ruby.DefineAttribute(value, 'displayname', @coi_displayname,
                                            @coi_displayname_set);
 end;
{$hints on}

function sts_add (obj : VALUE; str : VALUE) : VALUE; cdecl;
 var
   p : TPack;
 begin
 unpack_object(obj, p);
 result := p.rb.Int2Val((p.obj as TStrings).Add(p.rb.Val2Str(str)));
 end;

function sts_add (argc : cint; argv : PVALUE; slf : VALUE) : VALUE; cdecl;
 var
   p : TPack;
   s : TStrings;
 begin
 unpack_object(slf, p);
 s := p.obj as TStrings;
 if argc = 2
    then result := p.rb.Int2Val(s.AddObject(p.rb.Val2Str(argv[0]),
                                            p.rb.Val2Obj(argv[1])))
    else if p.rb.IsData(argv[0])
            then begin
                 result := p.rb.Int2Val(s.Count);
                 s.AddStrings(p.rb.Val2Obj(argv[0]) as TStrings);
                 end
            else result := p.rb.Int2Val(s.Add(p.rb.Val2Str(argv[0])));
 end;

function sts_append (argc : cint; argv : PVALUE; slf : VALUE) : VALUE; cdecl;
 var
   p : TPack;
   s : TStrings;
 begin
 unpack_object(slf, p);
 s := p.obj as TStrings;
 if argc = 2
    then s.AddObject(p.rb.Val2Str(argv[0]), p.rb.Val2Obj(argv[1]))
    else if p.rb.IsData(argv[0])
            then s.AddStrings(p.rb.Val2Obj(argv[0]) as TStrings)
            else s.Append(p.rb.Val2Str(argv[0]));
 result := slf
 end;

function sts_update (slf : VALUE) : VALUE; cdecl;
 var
   p : TPack;
   s : TStrings;
 begin
 unpack_object(slf, p);
 if p.rb.BlockGiven
    then begin
         s := p.obj as TStrings;
         s.BeginUpdate;
         result := p.rb.Yield(slf);
         s.EndUpdate;
         end
    else result := p.rb.Qnil;
 end;

function sts_clear (slf : VALUE) : VALUE; cdecl;
 var
   p : TPack;
 begin
 unpack_object(slf, p);
 (p.obj as TStrings).Clear;
 result := slf;
 end;

function sts_delete (slf : VALUE; idx : VALUE) : VALUE; cdecl;
 var
   p : TPack;
   s : TStrings;
   i : Integer;
 begin
 unpack_object(slf, p);
 s := p.obj as TStrings;
 i := p.rb.Val2Int(idx);
 result := p.rb.Str2Val(s[i]);
 s.Delete(i);
 end;

function sts_exchange (slf : VALUE; idx1, idx2 : VALUE) : VALUE; cdecl;
 var
   p : TPack;
 begin
 unpack_object(slf, p);
 (p.obj as TStrings).Exchange(p.rb.Val2Int(idx1), p.rb.Val2Int(idx2));
 result := slf;
 end;

function sts_text (slf : VALUE) : VALUE; cdecl;
 var
   p : TPack;
 begin
 unpack_object(slf, p);
 result := p.rb.Str2Val((p.obj as TStrings).Text);
 end;

function sts_text_set (slf : VALUE; txt : VALUE) : VALUE; cdecl;
 var
   p : TPack;
 begin
 unpack_object(slf, p);
 (p.obj as TStrings).Text := p.rb.Val2Str(txt);
 result := txt;
 end;

function sts_find (slf : VALUE; value : VALUE) : VALUE; cdecl;
 var
   p : TPack;
 begin
 unpack_object(slf, p);
 if p.rb.IsData(value)
    then result := p.rb.Val2Int(
                     (p.obj as TStrings).IndexOfObject(p.rb.Val2Obj(value))
                   )
    else result := p.rb.Val2Int(
                     (p.obj as TStrings).IndexOf(p.rb.Val2Str(value))
                   );
 end;

function sts_find_key (slf : VALUE; key : VALUE) : VALUE; cdecl;
 var
   p : TPack;
 begin
 unpack_object(slf, p);
 result := p.rb.Val2Int((p.obj as TStrings).IndexOfName(p.rb.Val2Str(key)));
 end;

function sts_insert (argc : cint; argv : PVALUE; slf : VALUE) : VALUE; cdecl;
 var
   p : TPack;
 begin
 unpack_object(slf, p);
 if argc = 3
    then (p.obj as TStrings).InsertObject(p.rb.Val2Int(argv[0]),
                                          p.rb.Val2Str(argv[1]),
                                          p.rb.Val2Obj(argv[2]))
    else (p.obj as TStrings).Insert(p.rb.Val2Int(argv[0]),
                                    p.rb.Val2Str(argv[1]));
 result := slf;
 end;

function sts_load (slf : VALUE; arg : VALUE) : VALUE; cdecl;
 var
   p : TPack;
 begin
 unpack_object(slf, p);
 try
   if p.rb.IsData(arg)
      then (p.obj as TStrings).LoadFromStream(p.rb.Val2Obj(arg) as TStream)
      else (p.obj as TStrings).LoadFromFile(p.rb.Val2Str(arg));
   result := slf;
 except
   result := p.rb.Qnil;
 end;
 end;

function sts_save (slf : VALUE; arg : VALUE) : VALUE; cdecl;
 var
   p : TPack;
 begin
 unpack_object(slf, p);
 try
   if p.rb.IsData(arg)
      then (p.obj as TStrings).SaveToStream(p.rb.Val2Obj(arg) as TStream)
      else (p.obj as TStrings).SaveToFile(p.rb.Val2Str(arg));
   result := slf;
 except
   result := p.rb.Qnil;
 end;
 end;

function sts_move (slf : VALUE; idx1, idx2 : VALUE) : VALUE; cdecl;
 var
   p : TPack;
 begin
 unpack_object(slf, p);
 (p.obj as TStrings).Move(p.rb.Val2Int(idx1), p.rb.Val2Int(idx2));
 result := slf;
 end;

function sts_pair (slf : VALUE; idx : VALUE) : VALUE; cdecl;
 var
   p : TPack;
   k, v : string;
 begin
 unpack_object(slf, p);
 (p.obj as TStrings).GetNameValue(p.rb.Val2Int(idx), k, v);
 result := p.rb.ArrayNew;
 p.rb.ArrayPush(result, p.rb.Str2Val(k));
 p.rb.ArrayPush(result, p.rb.Str2Val(v));
 end;

function sts_count (slf : VALUE) : VALUE; cdecl;
 var
   p : TPack;
 begin
 unpack_object(slf, p);
 result := p.rb.Int2Val((p.obj as TStrings).Count);
 end;

function sts_capacity (slf : VALUE) : VALUE; cdecl;
 var
   p : TPack;
 begin
 unpack_object(slf, p);
 result := p.rb.Int2Val((p.obj as TStrings).Capacity);
 end;

function sts_capacity_set (slf : VALUE; value : VALUE) : VALUE; cdecl;
 var
   p : TPack;
 begin
 unpack_object(slf, p);
 (p.obj as TStrings).Capacity := p.rb.Val2Int(value);
 result := value;
 end;

function linebreak2value (rb : TRuby; lb : TTextLineBreakStyle) : VALUE; inline;
 begin
 case lb of
   tlbsCR :
     result := rb.Str2Sym('CR');
   tlbsCRLF :
     result := rb.Str2Sym('CRLF');
   tlbsLF :
     result := rb.Str2Sym('LF');
   else
     result := rb.Qnil;
 end;
 end;

function sts_linebreak (slf : VALUE) : VALUE; cdecl;
 var
   p : TPack;
 begin
 unpack_object(slf, p);
 result := linebreak2value(p.rb, (p.obj as TStrings).TextLineBreakStyle);
 end;

function value2linebreak (rb : TRuby; lb : VALUE) : TTextLineBreakStyle; inline;
 begin
 if lb = rb.Str2Sym('CR')
    then result := tlbsCR
    else if lb = rb.Str2Sym('CRLF')
            then result := tlbsCRLF
            else if lb = rb.Str2Sym('LF')
                    then result := tlbsLF
                    else result := DefaultTextLineBreakStyle;
 end;

function sts_linebreak_set (slf : VALUE; value : VALUE) : VALUE; cdecl;
 var
   p : TPack;
 begin
 unpack_object(slf, p);
 (p.obj as TStrings).TextLineBreakStyle := value2linebreak(p.rb, value);
 result := value;
 end;

function sts_delimiter (slf : VALUE) : VALUE; cdecl;
 var
   p : TPack;
 begin
 unpack_object(slf, p);
 result := p.rb.Str2Val((p.obj as TStrings).Delimiter);
 end;

function sts_delimiter_set (slf : VALUE; value : VALUE) : VALUE; cdecl;
 var
   p : TPack;
 begin
 unpack_object(slf, p);
 (p.obj as TStrings).Delimiter := p.rb.Val2Str(value)[1];
 result := value;
 end;

function sts_delimited_text (slf : VALUE) : VALUE; cdecl;
 var
   p : TPack;
 begin
 unpack_object(slf, p);
 result := p.rb.Str2Val((p.obj as TStrings).DelimitedText);
 end;

function sts_delimited_text_set (slf : VALUE; value : VALUE) : VALUE; cdecl;
 var
   p : TPack;
 begin
 unpack_object(slf, p);
 (p.obj as TStrings).DelimitedText := p.rb.Val2Str(value);
 result := value;
 end;

function sts_strict_delimiter (slf : VALUE) : VALUE; cdecl;
  var
    p : TPack;
  begin
  unpack_object(slf, p);
  result := p.rb.Bln2Val((p.obj as TStrings).StrictDelimiter);
  end;

function sts_strict_delimiter_set (slf : VALUE; value : VALUE) : VALUE; cdecl;
  var
    p : TPack;
  begin
  unpack_object(slf, p);
  (p.obj as TStrings).StrictDelimiter := p.rb.Val2Bln(value);
  result := value;
  end;

function sts_quote (slf : VALUE) : VALUE; cdecl;
  var
    p : TPack;
  begin
  unpack_object(slf, p);
  result := p.rb.Str2Val((p.obj as TStrings).QuoteChar);
  end;

function sts_quote_set (slf : VALUE; value : VALUE) : VALUE; cdecl;
  var
    p : TPack;
  begin
  unpack_object(slf, p);
  (p.obj as TStrings).QuoteChar := p.rb.Val2Str(value)[1];
  result := value;
  end;

function sts_separator (slf : VALUE) : VALUE; cdecl;
  var
    p : TPack;
  begin
  unpack_object(slf, p);
  result := p.rb.Str2Val((p.obj as TStrings).NameValueSeparator);
  end;

function sts_separator_set (slf : VALUE; value : VALUE) : VALUE; cdecl;
  var
    p : TPack;
  begin
  unpack_object(slf, p);
  (p.obj as TStrings).NameValueSeparator := p.rb.Val2Str(value)[1];
  result := value;
  end;

function sts_comma_text (slf : VALUE) : VALUE; cdecl;
  var
    p : TPack;
  begin
  unpack_object(slf, p);
  result := p.rb.Str2Val((p.obj as TStrings).CommaText);
  end;

function sts_comma_text_set (slf : VALUE; value : VALUE) : VALUE; cdecl;
  var
    p : TPack;
  begin
  unpack_object(slf, p);
  (p.obj as TStrings).CommaText := p.rb.Val2Str(value);
  result := value;
  end;

function sts_strings (slf : VALUE) : VALUE; cdecl;
  var
    p : TPack;
    i : Integer;
    s : TStrings;
  begin
  unpack_object(slf, p);
  s := p.obj as TStrings;
  result := p.rb.ArrayNew;
  for i := 0 to s.Count - 1 do
      p.rb.ArrayPush(result, p.rb.Str2Val(s.Strings[i]));
  end;

function sts_objects (slf : VALUE) : VALUE; cdecl;
  var
    p : TPack;
    i : Integer;
    s : TStrings;
  begin
  unpack_object(slf, p);
  s := p.obj as TStrings;
  result := p.rb.ArrayNew;
  for i := 0 to s.Count - 1 do
      p.rb.ArrayPush(result, p.rb.Obj2Val(s.Objects[i]));
  end;

function sts_keys (slf : VALUE) : VALUE; cdecl;
  var
    p : TPack;
    i : Integer;
    s : TStrings;
  begin
  unpack_object(slf, p);
  s := p.obj as TStrings;
  result := p.rb.ArrayNew;
  for i := 0 to s.Count - 1 do
      p.rb.ArrayPush(result, p.rb.Str2Val(s.Names[i]));
  end;

function sts_pairs (slf : VALUE) : VALUE; cdecl;
  var
    p : TPack;
    i : Integer;
    s : TStrings;
  begin
  unpack_object(slf, p);
  s := p.obj as TStrings;
  result := p.rb.HashNew;
  for i := 0 to s.Count - 1 do
      p.rb.HashSet(result, p.rb.Str2Val(s.Names[i]),
                           p.rb.Str2Val(s.ValueFromIndex[i]));
  end;

function sts_each (slf : VALUE) : VALUE; cdecl;
 var
   p : TPack;
   i : Integer;
   s : TStrings;
 begin
 unpack_object(slf, p);
 if p.rb.BlockGiven
    then begin
         s := p.obj as TStrings;
         for i := 0 to s.Count - 1 do
             p.rb.Yield(p.rb.Str2Val(s.Strings[i]));
         result := slf;
         end
    else result := p.rb.Send(p.rb.cEnumerator, 'new', [slf]);
 end;

function sts_each_object (slf : VALUE) : VALUE; cdecl;
  var
    p : TPack;
    i : Integer;
    s : TStrings;
  begin
  unpack_object(slf, p);
  if p.rb.BlockGiven
     then begin
          s := p.obj as TStrings;
          for i := 0 to s.Count - 1 do
              p.rb.Yield(p.rb.Obj2Val(s.Objects[i]));
          result := slf;
          end
     else result := p.rb.Send(p.rb.cEnumerator, 'new',
                              [slf, p.rb.Str2Sym('each_object')]);
  end;

function sts_each_pair (slf : VALUE) : VALUE; cdecl;
  var
    p : TPack;
    i : Integer;
    s : TStrings;
    v : VALUE;
  begin
  unpack_object(slf, p);
  if p.rb.BlockGiven
     then begin
          s := p.obj as TStrings;
          for i := 0 to s.Count - 1 do
              begin
              v := p.rb.ArrayNew;
              p.rb.ArrayPush(v, p.rb.Str2Val(s.Names[i]));
              p.rb.ArrayPush(v, p.rb.Str2Val(s.ValueFromIndex[i]));
              p.rb.Yield(v);
              end;
          result := slf;
          end
     else result := p.rb.Send(p.rb.cEnumerator, 'new',
                              [slf, p.rb.Str2Sym('each_pair')]);
  end;

{$hints off}
procedure hookTStrings (ruby : TRuby; cls : TClass; value : VALUE);
 begin
 ruby.Include(value, ruby.mEnumerable);
 ruby.DefineAttribute(value, 'text',      @sts_text,      @sts_text_set);
 ruby.DefineAttribute(value, 'count',     @sts_count,     nil);
 ruby.DefineAttribute(value, 'capacity',  @sts_capacity,  @sts_capacity_set);
 ruby.DefineAttribute(value, 'linebreak', @sts_linebreak, @sts_linebreak_set);
 ruby.DefineAttribute(value, 'delimiter', @sts_delimiter, @sts_delimiter_set);
 ruby.DefineAttribute(value, 'delimited_text', @sts_delimited_text,
                                               @sts_delimited_text_set);
 ruby.DefineAttribute(value, 'strict_delimiter', @sts_strict_delimiter,
                                                 @sts_strict_delimiter_set);
 ruby.DefineAttribute(value, 'quote',     @sts_quote,     @sts_quote_set);
 ruby.DefineAttribute(value, 'separator', @sts_separator, @sts_separator_set);
 ruby.DefineAttribute(value, 'comma_text', @sts_comma_text,
                                           @sts_comma_text_set);
 ruby.DefineMethod(value, 'add',         @sts_add);
 ruby.DefineMethod(value, 'append',      @sts_append);
 ruby.DefineMethod(value, 'update',      @sts_update);
 ruby.DefineMethod(value, 'clear',       @sts_clear);
 ruby.DefineMethod(value, 'delete',      @sts_delete);
 ruby.DefineMethod(value, 'insert',      @sts_insert);
 ruby.DefineMethod(value, 'exchange',    @sts_exchange);
 ruby.DefineMethod(value, 'move',        @sts_move);
 ruby.DefineMethod(value, 'find',        @sts_find);
 ruby.DefineMethod(value, 'find_key',    @sts_find_key);
 ruby.DefineMethod(value, 'load',        @sts_load);
 ruby.DefineMethod(value, 'save',        @sts_save);
 ruby.DefineMethod(value, 'pair',        @sts_pair);
 ruby.DefineMethod(value, 'strings',     @sts_strings);
 ruby.DefineMethod(value, 'objects',     @sts_objects);
 ruby.DefineMethod(value, 'keys',        @sts_keys);
 ruby.DefineMethod(value, 'pairs',       @sts_pairs);
 ruby.DefineMethod(value, 'each',        @sts_each);
 ruby.DefineMethod(value, 'each_object', @sts_each_object);
 ruby.DefineMethod(value, 'each_pair',   @sts_each_pair);
 ruby.DefineAlias(value, '<<',          'append');
 ruby.DefineAlias(value, 'to_a',        'strings');
 ruby.DefineAlias(value, 'to_h',        'pairs');
 ruby.DefineAlias(value, 'each_string', 'each');
 end;
{$hints on}

function sls_sort (slf : VALUE) : VALUE; cdecl;
  var
    p : TPack;
  begin
  unpack_object(slf, p);
  (p.obj as TStringList).Sort;
  result := slf;
  end;

function duplicates2value (rb : TRuby; dp : TDuplicates) : VALUE; inline;
 begin
 case dp of
   dupAccept :
     result := rb.Str2Sym('accept');
   dupError :
     result := rb.Str2Sym('error');
   dupIgnore :
     result := rb.Str2Sym('ignore');
   else
     result := rb.Qnil;
 end;
 end;

function sls_duplicates (slf : VALUE) : VALUE; cdecl;
  var
    p : TPack;
  begin
  unpack_object(slf, p);
  result := duplicates2value(p.rb, (p.obj as TStringList).Duplicates);
  end;

function value2duplicates (rb : TRuby; dp : VALUE) : TDuplicates; inline;
 begin
 if dp = rb.Str2Sym('accept')
    then result := dupAccept
    else if dp = rb.Str2Sym('error')
            then result := dupError
            else result := dupIgnore;
 end;

function sls_duplicates_set (slf : VALUE; value : VALUE) : VALUE; cdecl;
  var
    p : TPack;
  begin
  unpack_object(slf, p);
  (p.obj as TStringList).Duplicates := value2duplicates(p.rb, value);
  result := value;
  end;

function sls_sorted (slf : VALUE) : VALUE; cdecl;
  var
    p : TPack;
  begin
  unpack_object(slf, p);
  result := p.rb.Bln2Val((p.obj as TStringList).Sorted);
  end;

function sls_sorted_set (slf : VALUE; value : VALUE) : VALUE; cdecl;
  var
    p : TPack;
  begin
  unpack_object(slf, p);
  (p.obj as TStringList).Sorted := p.rb.Val2Bln(value);
  result := value;
  end;

function sls_case_sensitive (slf : VALUE) : VALUE; cdecl;
  var
    p : TPack;
  begin
  unpack_object(slf, p);
  result := p.rb.Bln2Val((p.obj as TStringList).CaseSensitive);
  end;

function sls_case_sensitive_set (slf : VALUE; value : VALUE) : VALUE; cdecl;
  var
    p : TPack;
  begin
  unpack_object(slf, p);
  (p.obj as TStringList).CaseSensitive := p.rb.Val2Bln(value);
  result := value;
  end;

{$hints off}
procedure hookTStringList (ruby : TRuby; cls : TClass; value : VALUE);
 begin
 ruby.DefineAttribute(value, 'duplicates', @sls_duplicates,
                                           @sls_duplicates_set);
 ruby.DefineAttribute(value, 'sorted', @sls_sorted, @sls_sorted_set);
 ruby.DefineAttribute(value, 'casesensitive', @sls_case_sensitive,
                                              @sls_case_sensitive_set);
 ruby.DefineMethod(value, 'sort!', @sls_sort);
 end;
{$hints on}

initialization
 InitCriticalSection(sort_cs);
 TRuby.AddRegisterClassHook(TPersistent,     @hookTPersistent);
 TRuby.AddRegisterClassHook(TComponent,      @hookTComponent);
 TRuby.AddRegisterClassHook(TBasicAction,    @hookTBasicAction);
 TRuby.AddRegisterClassHook(TCollection,     @hookTCollection);
 TRuby.AddRegisterClassHook(TCollectionItem, @hookTCollectionItem);
 TRuby.AddRegisterClassHook(TStrings,        @hookTStrings);
 TRuby.AddRegisterClassHook(TStringList,     @hookTStringList);
finalization
 DoneCriticalsection(sort_cs);
end.

