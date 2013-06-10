{$mode ObjFPC}

unit ValCache;

interface

type
  TData = type Pointer;

  TCacheRec = record
    cache, key, value : TData;
  end;
  PCacheRec = ^TCacheRec;

function Put (cache, key, value : TData) : PCacheRec;

function Get (cache, key : TData) : PCacheRec;
function Get (value : TData) : PCacheRec;

function GetCache (value : TData) : TData;
function GetKey (value : TData) : TData;
function GetValue (cache, key : TData) : TData;

procedure Free (cache, key : TData);
procedure Free (value : TData);

procedure Free (rec : PCacheRec);

procedure Clear (cache : TData);

implementation

type
  PCacheRecord = ^TCacheRecord;
  TCacheRecord = record
    rec : TCacheRec;
    key, value : record
      up, left, right : PCacheRecord;
    end;
  end;

var
  key : PCacheRecord = nil;
  value : PCacheRecord = nil;

procedure insert_key_to (rec : PCacheRecord; var node : PCacheRecord; owner : PCacheRecord);
 begin
  if node = nil
     then begin
           node := rec;
           rec^.key.up := owner;
          end
     else if rec^.rec.cache > node^.rec.cache
             then insert_key_to(rec, node^.key.right, node)
             else if rec^.rec.cache < node^.rec.cache
                     then insert_key_to(rec, node^.key.left, node)
                     else // equal cache
                          if rec^.rec.key > node^.rec.key
                             then insert_key_to(rec, node^.key.right, node)
                             else if rec^.rec.key < node^.rec.key
                                     then insert_key_to(rec, node^.key.left, node)
                                     else {$note todo: check inconsistence ... };
 end;

procedure insert_key (rec : PCacheRecord);
 begin
  rec^.key.left := nil;
  rec^.key.right := nil;
  insert_key_to(rec, key, nil);
 end;

procedure insert_value_to (rec : PCacheRecord; var node : PCacheRecord; owner : PCacheRecord);
 begin
  if node = nil
     then begin
           node := rec;
           rec^.value.up := owner;
          end
     else if rec^.rec.value > node^.rec.value
             then insert_value_to(rec, node^.value.right, node)
             else if rec^.rec.value < node^.rec.value
                     then insert_value_to(rec, node^.value.left, node)
                     else {$note todo: check inconsistence ...};
 end;

procedure insert_value (rec : PCacheRecord);
 begin
  rec^.value.left := nil;
  rec^.value.right := nil;
  insert_value_to(rec, value, nil);
 end;

function find_key (cache, key : TData) : PCacheRecord;
 begin

 end;

function find_value (value : TData) : PCacheRecord;
 begin

 end;

procedure delete_key (rec : PCacheRecord);
 begin

 end;

procedure delete_value (rec : PCacheRecord);
 begin

 end;

function Put(cache, key, value : TData) : PCacheRec;
 begin
  result := GetMem(SizeOf(TCacheRecord));
  result^.cache := cache;
  result^.key := key;
  result^.value := value;
  insert_key(PCacheRecord(result));
  insert_value(PCacheRecord(result));
 end;

function Get(cache, key : TData) : PCacheRec;
 begin
  result := PCacheRec(find_key(cache, key));
 end;

function Get(value : TData) : PCacheRec;
 begin
  result := PCacheRec(find_value(value));
 end;

function GetCache(value : TData) : TData;
 begin
  result := find_value(value)^.rec.cache;
 end;

function GetKey(value : TData) : TData;
 begin
  result := find_value(value)^.rec.key;
 end;

function GetValue(cache, key : TData) : TData;
 begin
  result := find_key(cache, key)^.rec.value;
 end;

procedure Free(cache, key : TData);
 begin
  Free(PCacheRec(find_key(cache, key)));
 end;

procedure Free(value : TData);
 begin
  Free(PCacheRec(find_value(value)));
 end;

procedure Free(rec : PCacheRec);
 begin
  delete_key(PCacheRecord(rec));
  delete_value(PCacheRecord(rec));
  FreeMem(PCacheRecord(rec));
 end;

procedure Clear(cache : TData);
 begin

 end;

end.

