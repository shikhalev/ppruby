{$codepage utf8}
{$smartlink on}
{$mode objfpc}{$h+}

unit ppRubyClasses;

(*
   Package : RubyFCL
   File : pprubyclasses.pp
   Desc : core unit of ruby binding
   License : GNU GPL
*)

interface

uses
  Classes, SysUtils, ppRuby;

procedure TObjectClassHook (cObject : VALUE);
procedure TPersistentClassHook (cPersistent : VALUE);
procedure TComponentClassHook (cComponent : VALUE);
procedure TStringsClassHook (cStrings : VALUE);
procedure TStringListClassHook (cStringList : VALUE);
procedure TStreamClassHook (cStream : VALUE);
procedure TFileStreamClassHook (cFileStream : VALUE);
procedure TCustomMemoryStreamClassHook (cCustomMemoryStream : VALUE);

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
  DefineMethod(cPersistent, 'namepath', @m_tpersistent_namepath);
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
      Yield(VALUE(item));
  Result := instance;
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
  SetLength(arr, 0);
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
  DefineMethod(cComponent, 'to_s', @m_tcomponent_to_s);
  DefineAlias(cComponent, '<<', 'insert');
  DefineAlias(cComponent, 'to_a', 'components');
  DefineAlias(cComponent, 'to_ary', 'components');
  DefineAlias(cComponent, 'to_str', 'to_s');
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

function m_tstrings_append (instance : VALUE; str : VALUE) : VALUE; cdecl;
 begin
  (TObject(instance) as TStrings).Append(ansistring(str));
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
  Result := VALUE((TObject(instance) as TStrings).Delimiter + '');
 end;

function m_tstrings_delimiter_set (instance : VALUE; delimiter : VALUE) : VALUE; cdecl;
 begin
  if ValType(delimiter) = rtFixNum
     then (TObject(instance) as TStrings).Delimiter := Chr(PtrInt(delimiter))
     else (TObject(instance) as TStrings).Delimiter := ansistring(delimiter)[1];
  Result := delimiter;
 end;

function m_tstrings_delimitedtext (instance : VALUE) : VALUE; cdecl;
 begin
  Result := VALUE((TObject(instance) as TStrings).DelimitedText);
 end;

function m_tstrings_delimitedtext_set (instance : VALUE; text : VALUE) : VALUE; cdecl;
 begin
  (TObject(instance) as TStrings).DelimitedText := ansistring(text);
  Result := text;
 end;

function m_tstrings_strictdelimiter (instance : VALUE) : VALUE; cdecl;
 begin
  Result := VALUE((TObject(instance) as TStrings).StrictDelimiter);
 end;

function m_tstrings_strictdelimiter_set (instance : VALUE; strict : VALUE) : VALUE; cdecl;
 begin
  (TObject(instance) as TStrings).StrictDelimiter := Boolean(strict);
  Result := strict;
 end;

function m_tstrings_quotechar (instance : VALUE) : VALUE; cdecl;
 begin
  Result := VALUE((TObject(instance) as TStrings).QuoteChar + '');
 end;

function m_tstrings_quotechar_set (instance : VALUE; quote : VALUE) : VALUE; cdecl;
 begin
  if ValType(quote) = rtFixNum
     then (TObject(instance) as TStrings).QuoteChar := Chr(PtrInt(quote))
     else (TObject(instance) as TStrings).QuoteChar := ansistring(quote)[1];
  Result := quote;
 end;

function m_tstrings_separator (instance : VALUE) : VALUE; cdecl;
 begin
  Result := VALUE((TObject(instance) as TStrings).NameValueSeparator + '');
 end;

function m_tstrings_separator_set (instance : VALUE; separator : VALUE) : VALUE; cdecl;
 begin
  if ValType(separator) = rtFixNum
     then (TObject(instance) as TStrings).NameValueSeparator := Chr(PtrInt(separator))
     else (TObject(instance) as TStrings).NameValueSeparator := ansistring(separator)[1];
  Result := separator;
 end;

function m_tstrings_commatext (instance : VALUE) : VALUE; cdecl;
 begin
  Result := VALUE((TObject(instance) as TStrings).CommaText);
 end;

function m_tstrings_commatext_set (instance : VALUE; text : VALUE) : VALUE; cdecl;
 begin
  (TObject(instance) as TStrings).CommaText := ansistring(text);
  Result := text;
 end;

function m_tstrings_count (instance : VALUE) : VALUE; cdecl;
 begin
  Result := VALUE((TObject(instance) as TStrings).Count);
 end;

function m_tstrings_names (instance : VALUE) : VALUE; cdecl;
 var
   arr : array of VALUE;
   obj : TStrings;
   idx : Integer;
 begin
  obj := TObject(instance) as TStrings;
  SetLength(arr, obj.Count);
  for idx := 0 to obj.Count - 1 do
      arr[idx] := VALUE(obj.Names[idx]);
  Result := MakeArray(arr);
  SetLength(arr, 0);
 end;

function m_tstrings_objects (instance : VALUE) : VALUE; cdecl;
 var
   arr : array of VALUE;
   obj : TStrings;
   idx : Integer;
 begin
  obj := TObject(instance) as TStrings;
  SetLength(arr, obj.Count);
  for idx := 0 to obj.Count - 1 do
      arr[idx] := VALUE(obj.Objects[idx]);
  Result := MakeArray(arr);
  SetLength(arr, 0);
 end;

function m_tstrings_values (instance : VALUE) : VALUE; cdecl;
 var
   obj : TStrings;
   idx : Integer;
 begin
  obj := TObject(instance) as TStrings;
  Result := MakeHash;
  for idx := 0 to obj.Count - 1 do
      HashSet(Result, VALUE(ID(obj.Names[idx])), VALUE(obj.ValueFromIndex[idx]));
 end;

function m_tstrings_strings (instance : VALUE) : VALUE; cdecl;
 var
   arr : array of VALUE;
   obj : TStrings;
   idx : Integer;
 begin
  obj := TObject(instance) as TStrings;
  SetLength(arr, obj.Count);
  for idx := 0 to obj.Count do
      arr[idx] := VALUE(obj.Strings[idx]);
  Result := MakeArray(arr);
  SetLength(arr, 0);
 end;

function m_tstrings_sub (instance : VALUE; index : VALUE) : VALUE; cdecl;
 begin
  try
   case ValType(index) of
        rtFixNum :
          Result := VALUE((TObject(instance) as TStrings).Strings[PtrInt(index)]);
        rtSymbol :
          Result := VALUE((TObject(instance) as TStrings).Values[ansistring(ID(index))]);
        else
          Result := VALUE((TObject(instance) as TStrings).Values[ansistring(index)]);
   end;
  except
   Result := Qnil;
  end;
 end;

function m_tstrings_sub_set (instance : VALUE; index : VALUE; val : VALUE) : VALUE; cdecl;
 begin
  case ValType(index) of
       rtFixNum :
         (TObject(instance) as TStrings).Strings[PtrInt(index)] := ansistring(val);
       rtSymbol :
         (TObject(instance) as TStrings).Values[ansistring(ID(index))] := ansistring(val);
       else
         (TObject(instance) as TStrings).Values[ansistring(index)] := ansistring(val);
  end;
  Result := val;
 end;

function m_tstrings_each (instance : VALUE) : VALUE; cdecl;
 var
   obj : TStrings;
   item : ansistring;
 begin
  obj := TObject(instance) as TStrings;
  for item in obj do
      Yield(VALUE(item));
  Result := instance;
 end;

function m_tstrings_each_value (instance : VALUE) : VALUE; cdecl;
 var
   obj : TStrings;
   idx : Integer;
 begin
  obj := TObject(instance) as TStrings;
  for idx := 0 to obj.Count - 1 do
      Yield(MakeArray([VALUE(ID(obj.Names[idx])), VALUE(obj.ValueFromIndex[idx])]));
  Result := instance;
 end;

function m_tstrings_text (instance : VALUE) : VALUE; cdecl;
 begin
  Result := VALUE((TObject(instance) as TStrings).Text);
 end;

function m_tstrings_text_set (instance : VALUE; text : VALUE) : VALUE; cdecl;
 begin
  (TObject(instance) as TStrings).Text := ansistring(text);
  Result := text;
 end;

procedure TStringsClassHook(cStrings : VALUE);
 begin
  DefineMethod(cStrings, 'add', @m_tstrings_add);
  DefineMethod(cStrings, 'addobject', @m_tstrings_addobject);
  DefineMethod(cStrings, 'addstrings', @m_tstrings_addstrings);
  DefineMethod(cStrings, 'append', @m_tstrings_append);
  DefineMethod(cStrings, 'clear', @m_tstrings_clear);
  DefineMethod(cStrings, 'delete', @m_tstrings_delete);
  DefineMethod(cStrings, 'exchange', @m_tstrings_exchange);
  DefineMethod(cStrings, 'index_of', @m_tstrings_index_of);
  DefineMethod(cStrings, 'index_of_name', @m_tstrings_index_of_name);
  DefineMethod(cStrings, 'index_of_object', @m_tstrings_index_of_object);
  DefineMethod(cStrings, 'insert', @m_tstrings_insert);
  DefineMethod(cStrings, 'insertobject', @m_tstrings_insertobject);
  DefineMethod(cStrings, 'loadfromfile', @m_tstrings_loadfromfile);
  DefineMethod(cStrings, 'loadfromstream', @m_tstrings_loadfromstream);
  DefineMethod(cStrings, 'move', @m_tstrings_move);
  DefineMethod(cStrings, 'savetofile', @m_tstrings_savetofile);
  DefineMethod(cStrings, 'savetostream', @m_tstrings_savetostream);
  DefineMethod(cStrings, 'extractname', @m_tstrings_extractname);
  DefineMethod(cStrings, 'delimiter', @m_tstrings_delimiter);
  DefineMethod(cStrings, 'delimiter=', @m_tstrings_delimiter_set);
  DefineMethod(cStrings, 'delimitedtext', @m_tstrings_delimitedtext);
  DefineMethod(cStrings, 'delimitedtext=', @m_tstrings_delimitedtext_set);
  DefineMethod(cStrings, 'strictdelimiter?', @m_tstrings_strictdelimiter);
  DefineMethod(cStrings, 'strictdelimiter=', @m_tstrings_strictdelimiter_set);
  DefineMethod(cStrings, 'quotechar', @m_tstrings_quotechar);
  DefineMethod(cStrings, 'quotechar=', @m_tstrings_quotechar_set);
  DefineMethod(cStrings, 'separator', @m_tstrings_separator);
  DefineMethod(cStrings, 'separator=', @m_tstrings_separator_set);
  DefineMethod(cStrings, 'commatext', @m_tstrings_commatext);
  DefineMethod(cStrings, 'commatext=', @m_tstrings_commatext_set);
  DefineMethod(cStrings, 'count', @m_tstrings_count);
  DefineMethod(cStrings, 'names', @m_tstrings_names);
  DefineMethod(cStrings, 'objects', @m_tstrings_objects);
  DefineMethod(cStrings, 'values', @m_tstrings_values);
  DefineMethod(cStrings, 'strings', @m_tstrings_strings);
  DefineMethod(cStrings, '[]', @m_tstrings_sub);
  DefineMethod(cStrings, '[]=', @m_tstrings_sub_set);
  IncludeModule(cStrings, ModEnumerable);
  DefineMethod(cStrings, 'each', @m_tstrings_each);
  DefineMethod(cStrings, 'each_value', @m_tstrings_each_value);
  DefineMethod(cStrings, 'text', @m_tstrings_text);
  DefineMethod(cStrings, 'text=', @m_tstrings_text_set);
  DefineAlias(cStrings, 'to_s', 'text');
  DefineAlias(cStrings, 'to_a', 'strings');
  DefineAlias(cStrings, 'to_h', 'values');
  DefineAlias(cStrings, 'to_str', 'text');
  DefineAlias(cStrings, 'to_ary', 'strings');
  DefineAlias(cStrings, 'to_hash', 'values');
  DefineAlias(cStrings, '<<', 'append');
 end;

type
  TStringListClass = class of TStringList;

function m_tstringlist_initialize (instance : VALUE) : VALUE; cdecl;
 var
   obj : TStringList;
 begin
  obj := TStringListClass(TClass(ValClass(instance))).Create;
  WrapObject(instance, obj);
  Result := instance;
 end;

function m_tstringlist_sort (instance : VALUE) : VALUE; cdecl;
 begin
  (TObject(instance) as TStringList).Sort;
  Result := instance;
 end;

function m_tstringlist_sorted (instance : VALUE) : VALUE; cdecl;
 begin
  Result := VALUE((TObject(instance) as TStringList).Sorted);
 end;

function m_tstringlist_sorted_set (instance : VALUE; sorted : VALUE) : VALUE; cdecl;
 begin
  (TObject(instance) as TStringList).Sorted := Boolean(sorted);
  Result := sorted;
 end;

procedure TStringListClassHook (cStringList : VALUE);
 begin
  DefineMethod(cStringList, 'initialize', @m_tstringlist_initialize);
  DefineMethod(cStringList, 'sort', @m_tstringlist_sort);
  DefineMethod(cStringList, 'sorted?', @m_tstringlist_sorted);
  DefineMethod(cStringList, 'sorted=', @m_tstringlist_sorted_set);
 end;

function m_tstream_position (instance : VALUE) : VALUE; cdecl;
 begin
  Result := VALUE((TObject(instance) as TStream).Position);
 end;

function m_tstream_position_set (instance : VALUE; pos : VALUE) : VALUE; cdecl;
 begin
  (TObject(instance) as TStream).Position := Int64(pos);
  Result := pos;
 end;

function m_tstream_size (instance : VALUE) : VALUE; cdecl;
 begin
  Result := VALUE((TObject(instance) as TStream).Size);
 end;

function m_tstream_size_set (instance : VALUE; size : VALUE) : VALUE; cdecl;
 begin
  (TObject(instance) as TStream).Size := Int64(size);
  Result := size;
 end;

function m_tstream_copy (instance : VALUE; source : VALUE; count : VALUE) : VALUE; cdecl;
 begin
  Result := VALUE((TObject(instance) as TStream).CopyFrom(TObject(source) as TStream, Int64(count)));
 end;

function m_tstream_readcomponent (instance : VALUE; component : VALUE) : VALUE; cdecl;
 begin
  try
   Result := VALUE((TObject(instance) as TStream).ReadComponent(TObject(component) as TComponent));
  except
   Result := Qnil;
  end;
 end;

function m_tstream_writecomponent (instance : VALUE; component : VALUE) : VALUE; cdecl;
 begin
  (TObject(instance) as TStream).WriteComponent(TObject(component) as TComponent);
  Result := instance;
 end;

function m_tstream_readbyte (instance : VALUE) : VALUE; cdecl;
 begin
  Result := VALUE((TObject(instance) as TStream).ReadByte);
 end;

function m_tstream_readword (instance : VALUE) : VALUE; cdecl;
 begin
  Result := VALUE((TObject(instance) as TStream).ReadWord);
 end;

function m_tstream_readdword (instance : VALUE) : VALUE; cdecl;
 begin
  Result := VALUE((TObject(instance) as TStream).ReadDWord);
 end;

function m_tstream_readqword (instance : VALUE) : VALUE; cdecl;
 begin
  Result := VALUE((TObject(instance) as TStream).ReadQWord);
 end;

function m_tstream_readstring (instance : VALUE) : VALUE; cdecl;
 begin
  Result := VALUE((TObject(instance) as TStream).ReadAnsiString);
 end;

function m_tstream_writebyte (instance : VALUE; v : VALUE) : VALUE; cdecl;
 begin
  (TObject(instance) as TStream).WriteByte(PtrInt(v));
  Result := instance;
 end;

function m_tstream_writeword (instance : VALUE; v : VALUE) : VALUE; cdecl;
 begin
  (TObject(instance) as TStream).WriteWord(PtrInt(v));
  Result := instance;
 end;

function m_tstream_writedword (instance : VALUE; v : VALUE) : VALUE; cdecl;
 begin
  (TObject(instance) as TStream).WriteDWord(PtrInt(v));
  Result := instance;
 end;

function m_tstream_writeqword (instance : VALUE; v : VALUE) : VALUE; cdecl;
 begin
  (TObject(instance) as TStream).WriteQWord(Int64(v));
  Result := instance;
 end;

function m_tstream_writestring (instance : VALUE; v : VALUE) : VALUE; cdecl;
 begin
  (TObject(instance) as TStream).WriteAnsiString(ansistring(v));
  Result := instance;
 end;

procedure TStreamClassHook(cStream : VALUE);
 begin
  DefineMethod(cStream, 'copy', @m_tstream_copy);
  DefineMethod(cStream, 'position', @m_tstream_position);
  DefineMethod(cStream, 'position=', @m_tstream_position_set);
  DefineMethod(cStream, 'size', @m_tstream_size);
  DefineMethod(cStream, 'size=', @m_tstream_size_set);
  DefineMethod(cStream, 'readcomponent', @m_tstream_readcomponent);
  DefineMethod(cStream, 'writecomponent', @m_tstream_writecomponent);
  DefineMethod(cStream, 'readbyte', @m_tstream_readbyte);
  DefineMethod(cStream, 'readword', @m_tstream_readword);
  DefineMethod(cStream, 'readdword', @m_tstream_readdword);
  DefineMethod(cStream, 'readqword', @m_tstream_readqword);
  DefineMethod(cStream, 'readstring', @m_tstream_readstring);
  DefineMethod(cStream, 'writebyte', @m_tstream_writebyte);
  DefineMethod(cStream, 'writeword', @m_tstream_writeword);
  DefineMethod(cStream, 'writedword', @m_tstream_writedword);
  DefineMethod(cStream, 'writeqword', @m_tstream_writeqword);
  DefineMethod(cStream, 'writestring', @m_tstream_writestring);
 end;

type
  TFileStreamClass = class of TFileStream;

function m_tfilestream_initialize (instance : VALUE; filename : VALUE; mode : VALUE) : VALUE; cdecl;
 var
   obj : TFileStream;
 begin
  obj := TFileStreamClass(TClass(ValClass(instance))).Create(ansistring(filename), PtrInt(mode));
  WrapObject(instance, obj);
  Result := instance;
 end;

function m_tfilestream_filename (instance : VALUE) : VALUE; cdecl;
 begin
  Result := VALUE((TObject(instance) as TFileStream).FileName);
 end;

procedure TFileStreamClassHook(cFileStream : VALUE);
 begin
  DefineMethod(cFileStream, 'initialize', @m_tfilestream_initialize);
  DefineMethod(cFileStream, 'filename', @m_tfilestream_filename);
 end;

function m_tcustommemorystream_savetofile (instance : VALUE; filename : VALUE) : VALUE; cdecl;
 begin
  (TObject(instance) as TCustomMemoryStream).SaveToFile(ansistring(filename));
  Result := instance;
 end;

function m_tcustommemorystream_savetostream (instance : VALUE; stream : VALUE) : VALUE; cdecl;
 begin
  (TObject(instance) as TCustomMemoryStream).SaveToStream(TObject(stream) as TStream);
  Result := instance;
 end;

procedure TCustomMemoryStreamClassHook(cCustomMemoryStream : VALUE);
 begin
  DefineMethod(cCustomMemoryStream, 'savetofile', @m_tcustommemorystream_savetofile);
  DefineMethod(cCustomMemoryStream, 'savetostream', @m_tcustommemorystream_savetostream);
 end;

end.

