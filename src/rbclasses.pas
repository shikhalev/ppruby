{$include rbopts.inc}

unit RbClasses;

interface

uses
  ctypes, Classes, SysUtils, Ruby, RbTools, RbObjects;

implementation

{ TBasicAction }

function BasicAction_HandlesTarget (obj, target : VALUE) : VALUE; cdecl;
 var
   o, t : TObject;
   r : Boolean;
 procedure _;
  begin
   r := (o as TBasicAction).HandlesTarget(t);
  end;
 begin
  o := VJ(obj);
  t := VJ(target);
  pp_try(@_);
  Result := BV(r);
 end;

function BasicAction_UpdateTarget (obj, target : VALUE) : VALUE; cdecl;
 var
   o, t : TObject;
 procedure _;
  begin
   (o as TBasicAction).UpdateTarget(t);
  end;
 begin
  o := VJ(obj);
  t := VJ(target);
  pp_try(@_);
  Result := obj
 end;

function BasicAction_ExecuteTarget (obj, target : VALUE) : VALUE; cdecl;
 var
   o, t : TObject;
 procedure _;
  begin
   (o as TBasicAction).ExecuteTarget(t);
  end;
 begin
  o := VJ(obj);
  t := VJ(target);
  pp_try(@_);
  Result := obj;
 end;

function BasicAction_Execute (obj : VALUE) : VALUE; cdecl;
 var
   o : TObject;
   r : Boolean;
 procedure _;
  begin
   r := (o as TBasicAction).Execute;
  end;
 begin
  o := VJ(obj);
  pp_try(@_);
  Result := BV(r);
 end;

function BasicAction_Update (obj : VALUE) : VALUE; cdecl;
 var
   o : TObject;
   r : Boolean;
 procedure _;
  begin
   r := (o as TBasicAction).Update;
  end;
 begin
  o := VJ(obj);
  pp_try(@_);
  Result := BV(r);
 end;

function BasicAction_RegisterChanges (obj, link : VALUE) : VALUE; cdecl;
 var
   o, l : TObject;
 procedure _;
  begin
   (o as TBasicAction).RegisterChanges(l as TBasicActionLink);
  end;
 begin
  o := VJ(obj);
  l := VJ(link);
  pp_try(@_);
  Result := obj;
 end;

function BasicAction_UnregisterChanges (obj, link : VALUE) : VALUE; cdecl;
 var
   o, l : TObject;
 procedure _;
  begin
   (o as TBasicAction).UnRegisterChanges(l as TBasicActionLink);
  end;
 begin
  o := VJ(obj);
  l := VJ(link);
  pp_try(@_);
  Result := obj
 end;

function BasicAction_ActionComponent (obj : VALUE) : VALUE; cdecl;
 var
   o, r : TObject;
 procedure _;
  begin
   r := (o as TBasicAction).ActionComponent;
  end;
 begin
  o := VJ(obj);
  pp_try(@_);
  Result := OV(r);
 end;

function BasicAction_set_ActionComponent (obj, value : VALUE) : VALUE; cdecl;
 var
   o, c : TObject;
 procedure _;
  begin
   (o as TBasicAction).ActionComponent := TComponent(c);
  end;
 begin
  o := VJ(obj);
  c := VO(value);
  pp_try(@_);
  Result := value;
 end;

procedure init_TBasicAction (klass : VALUE);
 begin
  rb_define_method(klass, 'handlestarget',     @BasicAction_HandlesTarget,     1);
  rb_define_method(klass, 'executetarget',     @BasicAction_ExecuteTarget,     1);
  rb_define_method(klass, 'updatetarget',      @BasicAction_UpdateTarget,      1);
  rb_define_method(klass, 'execute',           @BasicAction_Execute,           0);
  rb_define_method(klass, 'update',            @BasicAction_Update,            0);
  rb_define_method(klass, 'registerchanges',   @BasicAction_RegisterChanges,   1);
  rb_define_method(klass, 'unregisterchanges', @BasicAction_UnregisterChanges, 1);
  pp_define_attr(klass, 'actioncomponent', @BasicAction_ActionComponent,
                                         @BasicAction_set_ActionComponent);
 end;

{ TBasicActionLink }

function BasicActionLink_Execute (argc : cint; argv : PVALUE;
  obj : VALUE) : VALUE; cdecl;
 var
   o, c : TObject;
   r : Boolean;
 procedure _;
  begin
   r := (o as TBasicActionLink).Execute(TComponent(c));
  end;
 begin
  o := VJ(obj);
  if argc = 0
     then c := nil
     else c := VO(argv[0]);
  pp_try(@_);
  Result := BV(r);
 end;

function BasicActionLink_Update (obj : VALUE) : VALUE; cdecl;
 var
   o : TObject;
   r : Boolean;
 procedure _;
  begin
   r := (o as TBasicActionLink).Update;
  end;
 begin
  o := VJ(obj);
  pp_try(@_);
  Result := BV(r);
 end;

function BasicActionLink_Action (obj : VALUE) : VALUE; cdecl;
 var
   o, r : TObject;
 procedure _;
  begin
   r := (o as TBasicActionLink).Action;
  end;
 begin
  o := VJ(obj);
  pp_try(@_);
  Result := OV(r);
 end;

function BasicActionLink_set_Action (obj, value : VALUE) : VALUE; cdecl;
 var
   o, a : TObject;
 procedure _;
  begin
   (o as TBasicActionLink).Action := TBasicAction(a);
  end;
 begin
  o := VJ(obj);
  a := VO(value);
  pp_try(@_);
  Result := value;
 end;

procedure init_TBasicActionLink (klass : VALUE);
 begin
  rb_define_method(klass, 'execute', @BasicActionLink_Execute, -1);
  rb_define_method(klass, 'update',  @BasicActionLink_Update,   0);
  pp_define_attr(klass, 'action', @BasicActionLink_Action,
                                @BasicActionLink_set_Action);
 end;

{ TBits }

function Bits_GetFSize (obj : VALUE) : VALUE; cdecl;
 var
   o : TObject;
   r : PtrUInt;
 procedure _;
  begin
   r := (o as TBits).GetFSize;
  end;
 begin
  o := VJ(obj);
  pp_try(@_);
  Result := IV(r);
 end;

function Bits_SetOn (obj, bit : VALUE) : VALUE; cdecl;
 var
   o : TObject;
   b : PtrInt;
 procedure _;
  begin
   (o as TBits).SetOn(b);
  end;
 begin
  o := VJ(obj);
  b := VI(bit);
  pp_try(@_);
  Result := obj;
 end;

function Bits_Clear (obj, bit : VALUE) : VALUE; cdecl;
 var
   o : TObject;
   b : PtrInt;
 procedure _;
  begin
   (o as TBits).Clear(b);
  end;
 begin
  o := VJ(obj);
  b := VI(bit);
  pp_try(@_);
  Result := obj;
 end;

function Bits_ClearAll (obj : VALUE) : VALUE; cdecl;
 var
   o : TObject;
 procedure _;
  begin
   (o as TBits).Clearall;
  end;
 begin
  o := VJ(obj);
  pp_try(@_);
  Result := obj;
 end;

function Bits_AndBits (obj, bits : VALUE) : VALUE; cdecl;
 var
   o, b : TObject;
 procedure _;
  begin
   (o as TBits).AndBits(b as TBits);
  end;
 begin
  o := VJ(obj);
  b := VJ(bits);
  pp_try(@_);
  Result := obj;
 end;

function Bits_OrBits (obj, bits : VALUE) : VALUE; cdecl;
 var
   o, b : TObject;
 procedure _;
  begin
   (o as TBits).OrBits(b as TBits);
  end;
 begin
  o := VJ(obj);
  b := VJ(bits);
  pp_try(@_);
  Result := obj;
 end;

function Bits_XorBits (obj, bits : VALUE) : VALUE; cdecl;
 var
   o, b : TObject;
 procedure _;
  begin
   (o as TBits).XorBits(b as TBits);
  end;
 begin
  o := VJ(obj);
  b := VJ(bits);
  pp_try(@_);
  Result := obj;
 end;

function Bits_NotBits (obj, bits : VALUE) : VALUE; cdecl;
 var
   o, b : TObject;
 procedure _;
  begin
   (o as TBits).NotBits(b as TBits);
  end;
 begin
  o := VJ(obj);
  b := VJ(bits);
  pp_try(@_);
  Result := obj;
 end;

function Bits_Get (obj, bit : VALUE) : VALUE; cdecl;
 var
   o : TObject;
   b : PtrInt;
   r : Boolean;
 procedure _;
  begin
   r := (o as TBits).Get(b);
  end;
 begin
  o := VJ(obj);
  b := VI(bit);
  pp_try(@_);
  Result := BV(r);
 end;

function Bits_Grow (obj, nbit : VALUE) : VALUE; cdecl;
 var
   o : TObject;
   n : PtrInt;
 procedure _;
  begin
   (o as TBits).Grow(n);
  end;
 begin
  o := VJ(obj);
  n := VI(nbit);
  pp_try(@_);
  Result := obj;
 end;

function Bits_SetIndex (obj, idx : VALUE) : VALUE; cdecl;
 var
   o : TObject;
   i : PtrInt;
 procedure _;
  begin
   (o as TBits).SetIndex(i);
  end;
 begin
  o := VJ(obj);
  i := VI(idx);
  pp_try(@_);
  Result := obj;
 end;

function Bits_FindFirstBit (obj, state : VALUE) : VALUE; cdecl;
 var
   o : TObject;
   s : Boolean;
   r : PtrUInt;
 procedure _;
  begin
   r := (o as TBits).FindFirstBit(s);
  end;
 begin
  o := VJ(obj);
  s := VB(state);
  pp_try(@_);
  Result := IV(r);
 end;

function Bits_FindNextBit (obj : VALUE) : VALUE; cdecl;
 var
   o : TObject;
   r : PtrInt;
 procedure _;
  begin
   r := (o as TBits).FindNextBit;
  end;
 begin
  o := VJ(obj);
  pp_try(@_);
  Result := IV(r);
 end;

function Bits_FindPrevBit (obj : VALUE) : VALUE; cdecl;
 var
   o : TObject;
   r : PtrInt;
 procedure _;
  begin
   r := (o as TBits).FindPrevBit;
  end;
 begin
  o := VJ(obj);
  pp_try(@_);
  Result := IV(r);
 end;

function Bits_OpenBit (obj : VALUE) : VALUE; cdecl;
 var
   o : TObject;
   r : PtrInt;
 procedure _;
  begin
   r := (o as TBits).OpenBit;
  end;
 begin
  o := VJ(obj);
  pp_try(@_);
  Result := IV(r);
 end;

function Bits_set (obj, idx, value : VALUE) : VALUE; cdecl;
 var
   o : TObject;
   i : PtrInt;
   v : Boolean;
 procedure _;
  begin
   (o as TBits).Bits[i] := v;
  end;
 begin
  o := VJ(obj);
  i := VI(idx);
  v := VB(value);
  pp_try(@_);
  Result := value;
 end;

function Bits_Size (obj : VALUE) : VALUE; cdecl;
 var
   o : TObject;
   r : PtrInt;
 procedure _;
  begin
   r := (o as TBits).Size;
  end;
 begin
  o := VJ(obj);
  pp_try(@_);
  Result := IV(r);
 end;

function Bits_set_Size (obj, value : VALUE) : VALUE; cdecl;
 var
   o : TObject;
   v : PtrInt;
 procedure _;
  begin
   (o as TBits).Size := v;
  end;
 begin
  o := VJ(obj);
  v := VI(value);
  pp_try(@_);
  Result := value;
 end;

function Bits_New (argc : cint; argv : PVALUE; klass : VALUE) : VALUE; cdecl;
 type
   TBitsClass = class of TBits;
 var
   c : TBitsClass;
   s : PtrInt;
   r : TBits;
 procedure _;
  begin
   r := c.Create(s);
  end;
 begin
  c := TBitsClass(VC(klass));
  if argc = 0
     then s := 0
     else s := VI(argv[0]);
  pp_try(@_);
  Result := NV(r);
 end;

function Bits_And (obj, bits : VALUE) : VALUE; cdecl;
 var
   r : TBits;
   o, b : TObject;
 procedure _;
  begin
   r := TBits.Create;
   r.OrBits(o as TBits);
   r.AndBits(b as TBits);
  end;
 begin
  o := VJ(obj);
  b := VJ(bits);
  pp_try(@_);
  Result := NV(r);
 end;

function Bits_Or (obj, bits : VALUE) : VALUE; cdecl;
 var
   r : TBits;
   o, b : TObject;
 procedure _;
  begin
   r := TBits.Create;
   r.OrBits(o as TBits);
   r.OrBits(b as TBits);
  end;
 begin
  o := VJ(obj);
  b := VJ(bits);
  pp_try(@_);
  Result := NV(r);
 end;

function Bits_Xor (obj, bits : VALUE) : VALUE; cdecl;
 var
   r : TBits;
   o, b : TObject;
 procedure _;
  begin
   r := TBits.Create;
   r.OrBits(o as TBits);
   r.XorBits(b as TBits);
  end;
 begin
  o := VJ(obj);
  b := VJ(bits);
  pp_try(@_);
  Result := NV(r);
 end;

function Bits_Not (obj : VALUE) : VALUE; cdecl;
 var
   r : TBits;
   o : TObject;
 procedure _;
  begin
   r := TBits.Create;
   r.NotBits(o as TBits);
  end;
 begin
  o := VJ(obj);
  pp_try(@_);
  Result := NV(r);
 end;

procedure init_TBits (klass : VALUE);
 begin
  rb_define_method(klass, 'getfsize',     @Bits_GetFSize,     0);
  rb_define_method(klass, 'seton',        @Bits_SetOn,        1);
  rb_define_method(klass, 'clear',        @Bits_Clear,        1);
  rb_define_method(klass, 'clearall',     @Bits_ClearAll,     0);
  rb_define_method(klass, 'andbits',      @Bits_AndBits,      1);
  rb_define_method(klass, 'orbits',       @Bits_OrBits,       1);
  rb_define_method(klass, 'xorbits',      @Bits_XorBits,      1);
  rb_define_method(klass, 'notbits',      @Bits_NotBits,      1);
  rb_define_method(klass, 'get',          @Bits_Get,          1);
  rb_define_method(klass, 'grow',         @Bits_Grow,         1);
  rb_define_method(klass, 'setindex',     @Bits_SetIndex,     1);
  rb_define_method(klass, 'findfirstbit', @Bits_FindFirstBit, 1);
  rb_define_method(klass, 'findnextbit',  @Bits_FindNextBit,  0);
  rb_define_method(klass, 'findnextbit',  @Bits_FindPrevBit,  0);
  rb_define_method(klass, 'openbit',      @Bits_OpenBit,      0);
  pp_define_braces(klass, @Bits_Get, @Bits_set);
  pp_define_attr(klass, 'size', @Bits_Size, @Bits_set_Size);
  rb_define_singleton_method(klass, 'new', @Bits_New, -1);
  rb_define_method(klass, '&', @Bits_And, 1);
  rb_define_method(klass, '|', @Bits_Or,  1);
  rb_define_method(klass, '^', @Bits_Xor, 1);
  rb_define_method(klass, '~', @Bits_Not, 0);
 end;

{ TCollection }

function Collection_Owner (obj : VALUE) : VALUE; cdecl;
 var
   o, r : TObject;
 procedure _;
  begin
   r := (o as TCollection).Owner;
  end;
 begin
  o := VJ(obj);
  pp_try(@_);
  Result := OV(r);
 end;

function Collection_Add (obj : VALUE) : VALUE; cdecl;
 var
   o, r : TObject;
 procedure _;
  begin
   r := (o as TCollection).Add;
  end;
 begin
  o := VJ(obj);
  pp_try(@_);
  Result := OV(r);
 end;

function Collection_BeginUpdate (obj : VALUE) : VALUE; cdecl;
 var
   o : TObject;
 procedure _;
  begin
   (o as TCollection).BeginUpdate;
  end;
 begin
  o := VJ(obj);
  pp_try(@_);
  Result := obj;
 end;

function Collection_EndUpdate (obj : VALUE) : VALUE; cdecl;
 var
   o : TObject;
 procedure _;
  begin
   (o as TCollection).EndUpdate;
  end;
 begin
  o := VJ(obj);
  pp_try(@_);
  Result := obj;
 end;

function Collection_Clear (obj : VALUE) : VALUE; cdecl;
 var
   o : TObject;
 procedure _;
  begin
   (o as TCollection).Clear;
  end;
 begin
  o := VJ(obj);
  pp_try(@_);
  Result := obj
 end;

function Collection_Delete (obj, idx : VALUE) : VALUE; cdecl;
 var
   o : TObject;
   i : PtrInt;
 procedure _;
  begin
   (o as TCollection).Delete(i);
  end;
 begin
  o := VJ(obj);
  i := VI(idx);
  pp_try(@_);
  Result := obj;
 end;

function Collection_Insert (obj, idx : VALUE) : VALUE; cdecl;
 var
   o, r : TObject;
   i : PtrInt;
 procedure _;
  begin
   r := (o as TCollection).Insert(i);
  end;
 begin
  o := VJ(obj);
  i := VI(idx);
  pp_try(@_);
  Result := OV(r);
 end;

function Collection_FindItemID (obj, id : VALUE) : VALUE; cdecl;
 var
   o, r : TObject;
   i : PtrInt;
 procedure _;
  begin
   r := (o as TCollection).FindItemID(i);
  end;
 begin
  o := VJ(obj);
  i := VI(id);
  pp_try(@_);
  Result := OV(r);
 end;

function Collection_Exchange (obj, idx1, idx2 : VALUE) : VALUE; cdecl;
 var
   o : TObject;
   i1, i2 : PtrInt;
 procedure _;
  begin
   (o as TCollection).Exchange(i1, i2);
  end;
 begin
  o := VJ(obj);
  i1 := VI(idx1);
  i2 := VI(idx2);
  pp_try(@_);
  Result := obj;
 end;

threadvar
  rbcompareproc : VALUE;

function rbcomparefunc (i1, i2 : TCollectionItem) : Integer;
 procedure _;
  begin
   Result := VI(C(rbcompareproc, 'call', [OV(i1), OV(i2)]));
  end;
 begin
  pp_protect(@_);
 end;

function Collection_Sort (obj : VALUE) : VALUE; cdecl;
 var
   o : TObject;
 procedure _;
  begin
   (o as TCollection).Sort(@rbcomparefunc);
  end;
 begin
  o := VJ(obj);
  rbcompareproc := rb_block_proc();
  if rbcompareproc <> Qnil
     then begin
           pp_try(@_);
           Result := obj;
          end
     else Result := Qfalse;
 end;

function Collection_Count (obj : VALUE) : VALUE; cdecl;
 var
   o : TObject;
   r : PtrInt;
 procedure _;
  begin
   r := (o as TCollection).Count;
  end;
 begin
  o := VJ(obj);
  pp_try(@_);
  Result := IV(r);
 end;

function Collection_ItemClass (obj : VALUE) : VALUE; cdecl;
 var
   o : TObject;
   r : TClass;
 procedure _;
  begin
   r := (o as TCollection).ItemClass;
  end;
 begin
  o := VJ(obj);
  pp_try(@_);
  Result := CV(r);
 end;

function Collection_Items (obj, idx : VALUE) : VALUE; cdecl;
 var
   o, r : TObject;
   i : PtrInt;
 procedure _;
  begin
   r := (o as TCollection).Items[i];
  end;
 begin
  o := VJ(obj);
  i := VI(idx);
  pp_try(@_);
  Result := OV(r);
 end;

function Collection_set_Items (obj, idx, item : VALUE) : VALUE; cdecl;
 var
   o, m : TObject;
   i : PtrInt;
 procedure _;
  begin
   (o as TCollection).Items[i] := m as TCollectionItem;
  end;
 begin
  o := VJ(obj);
  i := VI(idx);
  m := VJ(item);
  pp_try(@_);
  Result := item;
 end;

function Collection_Each (obj : VALUE) : VALUE; cdecl;
 var
   o, m : TObject;
   i, l : PtrInt;
 procedure _cnt;
  begin
   l := (o as TCollection).Count;
  end;
 procedure _itm;
  begin
   m := TCollection(o).Items[i];
  end;
 begin
  if rb_block_given_p() <> 0
     then begin
           o := VJ(obj);
           pp_try(@_cnt);
           for i := 0 to l - 1 do
               begin
                pp_try(@_itm);
                rb_yield(OV(m));
               end;
          end;
  Result := obj;
 end;

function Collection_Update (obj : VALUE) : VALUE; cdecl;
 var
   o : TObject;
 procedure _bgn;
  begin
   (o as TCollection).BeginUpdate;
  end;
 procedure _end;
  begin
   TCollection(o).EndUpdate;
  end;
 begin
  o := VJ(obj);
  pp_try(@_bgn);
  Result := rb_yield(obj);
  pp_try(@_end);
 end;

procedure init_TCollection (klass : VALUE);
 begin
  rb_define_method(klass, 'owner',       @Collection_Owner,       0);
  rb_define_method(klass, 'add',         @Collection_Add,         0);
  rb_define_method(klass, 'beginupdate', @Collection_BeginUpdate, 0);
  rb_define_method(klass, 'endupdate',   @Collection_EndUpdate,   0);
  rb_define_method(klass, 'clear',       @Collection_Clear,       0);
  rb_define_method(klass, 'delete',      @Collection_Delete,      1);
  rb_define_method(klass, 'insert',      @Collection_Insert,      1);
  rb_define_method(klass, 'finditemid',  @Collection_FindItemID,  1);
  rb_define_method(klass, 'exchange',    @Collection_Exchange,    2);
  rb_define_method(klass, 'sort',        @Collection_Sort,        0);
  pp_define_attr(klass, 'count',     @Collection_Count,     nil);
  pp_define_attr(klass, 'itemclass', @Collection_ItemClass, nil);
  pp_define_braces(klass, @Collection_Items, @Collection_set_Items);
  rb_include_module(klass, rb_mEnumerable);
  rb_define_method(klass, 'each', @Collection_Each, 0);
  rb_define_method(klass, 'update', @Collection_Update, 0);
 end;

function CollectionItem_Collection (obj : VALUE) : VALUE; cdecl;
 var
   o, r : TObject;
 procedure _;
  begin
   r := (o as TCollectionItem).Collection;
  end;
 begin
  o := VJ(obj);
  pp_try(@_);
  Result := OV(r);
 end;

function CollectionItem_set_Collection (obj, value : VALUE) : VALUE; cdecl;
 var
   o, v : TObject;
 procedure _;
  begin
   (o as TCollectionItem).Collection := TCollection(v);
  end;
 begin
  o := VJ(obj);
  v := VO(value);
  pp_try(@_);
  Result := value;
 end;

function CollectionItem_ID (obj : VALUE) : VALUE; cdecl;
 var
   o : TObject;
   r : PtrInt;
 procedure _;
  begin
   r := (o as TCollectionItem).ID;
  end;
 begin
  o := VJ(obj);
  pp_try(@_);
  Result := IV(r);
 end;

function CollectionItem_Index (obj : VALUE) : VALUE; cdecl;
 var
   o : TObject;
   r : PtrInt;
 procedure _;
  begin
   r := (o as TCollectionItem).Index;
  end;
 begin
  o := VJ(obj);
  pp_try(@_);
  Result := IV(r);
 end;

function CollectionItem_set_Index (obj, value : VALUE) : VALUE; cdecl;
 var
   o : TObject;
   v : PtrInt;
 procedure _;
  begin
   (o as TCollectionItem).Index := v;
  end;
 begin
  o := VJ(obj);
  v := VI(value);
  pp_try(@_);
  Result := value;
 end;

function CollectionItem_DisplayName (obj : VALUE) : VALUE; cdecl;
 var
   o : TObject;
   r : PChar;
 procedure _;
  begin
   r := PChar((o as TCollectionItem).DisplayName);
  end;
 begin
  o := VJ(obj);
  pp_try(@_);
  Result := SV(r);
 end;

function CollectionItem_set_DisplayName (obj, value : VALUE) : VALUE; cdecl;
 var
   o : TObject;
   v : PChar;
 procedure _;
  begin
   (o as TCollectionItem).DisplayName := v;
  end;
 begin
  o := VJ(obj);
  v := VP(value);
  pp_try(@_);
  Result := value;
 end;

procedure init_TCollectionItem (klass : VALUE);
 begin
  pp_define_attr(klass, 'collection',  @CollectionItem_Collection,
                                     @CollectionItem_set_Collection);
  pp_define_attr(klass, 'id',          @CollectionItem_ID,             nil);
  pp_define_attr(klass, 'index',       @CollectionItem_Index,
                                     @CollectionItem_set_Index);
  pp_define_attr(klass, 'displayname', @CollectionItem_DisplayName,
                                     @CollectionItem_set_DisplayName);
 end;

{ TComponent }

function Component_WriteState (obj, wrt : VALUE) : VALUE; cdecl;
 var
   o, w : TObject;
 procedure _;
  begin
   (o as TComponent).WriteState(w as TWriter);
  end;
 begin
  o := VJ(obj);
  w := VJ(wrt);
  pp_try(@_);
  Result := obj;
 end;

function Component_ExecuteAction (obj, action : VALUE) : VALUE; cdecl;
 var
   o, a : TObject;
   r : Boolean;
 procedure _;
  begin
   r := (o as TComponent).ExecuteAction(a as TBasicAction);
  end;
 begin
  o := VJ(obj);
  a := VJ(action);
  pp_try(@_);
  Result := BV(r);
 end;

function Component_FindComponent (obj, name : VALUE) : VALUE; cdecl;
 var
   o, r : TObject;
   n : PChar;
 procedure _;
  begin
   r := (o as TComponent).FindComponent(n);
  end;
 begin
  o := VJ(obj);
  if SYMBOL_P(name)
     then n := YP(name)
     else n := VP(name);
  pp_try(@_);
  Result := OV(r);
 end;

function Component_FreeNotification (obj, other : VALUE) : VALUE; cdecl;
 var
   o, t : TObject;
 procedure _;
  begin
   (o as TComponent).FreeNotification(t as TComponent);
  end;
 begin
  o := VJ(obj);
  t := VJ(other);
  pp_try(@_);
  Result := obj;
 end;

function Component_RemoveFreeNotification (obj, other : VALUE) : VALUE; cdecl;
 var
   o, t : TObject;
 procedure _;
  begin
   (o as TComponent).RemoveFreeNotification(t as TComponent);
  end;
 begin
  o := VJ(obj);
  t := VJ(other);
  pp_try(@_);
  Result := obj;
 end;

function Component_GetParentComponent (obj : VALUE) : VALUE; cdecl;
 var
   o, r : TObject;
 procedure _;
  begin
   r := (o as TComponent).GetParentComponent;
  end;
 begin
  o := VJ(obj);
  pp_try(@_);
  Result := OV(r);
 end;

function Component_HasParent (obj : VALUE) : VALUE; cdecl;
 var
   o : TObject;
   r : Boolean;
 procedure _;
  begin
   r := (o as TComponent).HasParent;
  end;
 begin
  o := VJ(obj);
  pp_try(@_);
  Result := BV(r);
 end;

function Component_InsertComponent (obj, child : VALUE) : VALUE; cdecl;
 var
   o, c : TObject;
 procedure _;
  begin
   (o as TComponent).InsertComponent(c as TComponent);
  end;
 begin
  o := VJ(obj);
  c := VJ(child);
  pp_try(@_);
  Result := obj;
 end;

function Component_RemoveComponent (obj, child : VALUE) : VALUE; cdecl;
 var
   o, c : TObject;
 procedure _;
  begin
   (o as TComponent).RemoveComponent(c as TComponent);
  end;
 begin
  o := VJ(obj);
  c := VJ(child);
  pp_try(@_);
  Result := obj;
 end;

function Component_UpdateAction (obj, action : VALUE) : VALUE; cdecl;
 var
   o, a : TObject;
   r : Boolean;
 procedure _;
  begin
   r := (o as TComponent).UpdateAction(a as TBasicAction);
  end;
 begin
  o := VJ(obj);
  a := VJ(action);
  pp_try(@_);
  Result := BV(r);
 end;

function Component_Components (obj, idx : VALUE) : VALUE; cdecl;
 var
   o, r : TObject;
   i : PtrInt;
 procedure _;
  begin
   r := (o as TComponent).Components[i];
  end;
 begin
  o := VJ(obj);
  i := VI(idx);
  pp_try(@_);
  Result := OV(r);
 end;

function Component_ComponentCount (obj : VALUE) : VALUE; cdecl;
 var
   o : TObject;
   r : PtrInt;
 procedure _;
  begin
   r := (o as TComponent).ComponentCount;
  end;
 begin
  o := VJ(obj);
  pp_try(@_);
  Result := IV(r);
 end;

function Component_ComponentIndex (obj : VALUE) : VALUE; cdecl;
 var
   o : TObject;
   r : PtrInt;
 procedure _;
  begin
   r := (o as TComponent).ComponentIndex;
  end;
 begin
  o := VJ(obj);
  pp_try(@_);
  Result := IV(r);
 end;

function Component_set_ComponentIndex (obj, value : VALUE) : VALUE; cdecl;
 var
   o : TObject;
   v : PtrInt;
 procedure _;
  begin
   (o as TComponent).ComponentIndex := v;
  end;
 begin
  o := VJ(obj);
  v := VI(value);
  pp_try(@_);
  Result := value;
 end;

function Component_Owner (obj : VALUE) : VALUE; cdecl;
 var
   o, r : TObject;
 procedure _;
  begin
   r := (o as TComponent).Owner;
  end;
 begin
  o := VJ(obj);
  pp_try(@_);
  Result := OV(r);
 end;

function Component_Each (obj : VALUE) : VALUE; cdecl;
 var
   o, c : TObject;
   i, l : PtrInt;
 procedure _cnt;
  begin
   l := (o as TComponent).ComponentCount;
  end;
 procedure _itm;
  begin
   c := TComponent(o).Components[i];
  end;
 begin
  if rb_block_given_p() <> 0
     then begin
           o := VJ(obj);
           pp_try(@_cnt);
           for i := 0 to l - 1 do
               begin
                pp_try(@_itm);
                rb_yield(OV(c));
               end;
          end;
  Result := obj;
 end;

function Component_Get (obj, key : VALUE) : VALUE; cdecl;
 begin
  if FIXNUM_P(key)
     then Result := Component_Components(obj, key)
     else Result := Component_FindComponent(obj, key);
 end;

function Component_ToHash (obj : VALUE) : VALUE; cdecl;
 var
   o, c : TObject;
   i, l : PtrInt;
   n : PChar;
 procedure _cnt;
  begin
   l := (o as TComponent).ComponentCount;
  end;
 procedure _itm;
  begin
   c := TComponent(o).Components[i];
   n := PChar((c as TComponent).Name);
  end;
 begin
  o := VJ(obj);
  pp_try(@_cnt);
  Result := rb_hash_new();
  for i := 0 to l - 1 do
      begin
       pp_try(@_itm);
       rb_hash_aset(Result, SY(n), OV(c));
      end;
 end;

function Component_Missing (obj, method : VALUE) : VALUE; cdecl;
 var
   o, c : TObject;
   m : PChar;
 procedure _;
  begin
   c := (o as TComponent).FindComponent(m);
  end;
 begin
  o := VJ(obj);
  m := YP(method);
  pp_try(@_);
  if c <> nil
     then Result := OV(c)
     else rb_call_super(1, @method);
 end;

procedure init_TComponent (klass : VALUE);
 begin
  rb_define_method(klass, 'writestate',         @Component_WriteState,         1);
  rb_define_method(klass, 'executeaction',      @Component_ExecuteAction,      1);
  rb_define_method(klass, 'updateaction',       @Component_UpdateAction,       1);
  rb_define_method(klass, 'findcomponent',      @Component_FindComponent,      1);
  rb_define_method(klass, 'freenotification',   @Component_FreeNotification,   1);
  rb_define_method(klass, 'removefreenotification',
                        @Component_RemoveFreeNotification, 1);
  rb_define_method(klass, 'getparentcomponent', @Component_GetParentComponent, 0);
  rb_define_method(klass, 'hasparent',          @Component_HasParent,          0);
  rb_define_alias(klass, 'parent', 'getparentcomponent');
  rb_define_alias(klass, 'parent?', 'hasparent');
  rb_define_method(klass, 'insertcomponent',    @Component_InsertComponent,    1);
  rb_define_method(klass, 'removecomponent',    @Component_RemoveComponent,    1);
  rb_define_method(klass, 'components',         @Component_Components,         1);
  pp_define_attr(klass, 'componentcount', @Component_ComponentCount, nil);
  pp_define_attr(klass, 'componentindex', @Component_ComponentIndex,
                                        @Component_set_ComponentIndex);
  pp_define_attr(klass, 'owner',          @Component_Owner,          nil);
  rb_include_module(klass, rb_mEnumerable);
  rb_define_method(klass, 'each', @Component_Each, 0);
  pp_define_braces(klass, @Component_Get,  nil);
  rb_define_method(klass, 'to_hash', @Component_ToHash, 0);
  rb_define_alias(klass, 'to_h', 'to_hash');
  rb_define_method(klass, 'method_missing', @Component_Missing, 1);
 end;

{ TPersistent }

function Persistent_Assign (obj, src : VALUE) : VALUE; cdecl;
 var
   o, s : TObject;
 procedure _;
  begin
   (o as TPersistent).Assign(s as TPersistent);
  end;
 begin
  o := VJ(obj);
  s := VJ(src);
  pp_try(@_);
  Result := obj;
 end;

function Persistent_GetNamePath (obj : VALUE) : VALUE; cdecl;
 var
   o : TObject;
   r : PChar;
 procedure _;
  begin
   r := PChar((o as TPersistent).GetNamePath);
  end;
 begin
  o := VJ(obj);
  pp_try(@_);
  Result := SV(r);
 end;

procedure init_TPersistent (klass : VALUE);
 begin
  rb_define_method(klass, 'assign',      @Persistent_Assign,      1);
  rb_define_method(klass, 'getnamepath', @Persistent_GetNamePath, 0);
 end;

function StringList_Sort (obj : VALUE) : VALUE; cdecl;
 var
   o : TObject;
 procedure _;
  begin
   (o as TStringList).Sort;
  end;
 begin
  o := VJ(obj);
  pp_try(@_);
  Result := obj;
 end;

var
  v_dupIgnore : VALUE;
  v_dupAccept : VALUE;
  v_dupError  : VALUE;

function DY (x : TDuplicates) : VALUE; local;
 begin
  case x of
       dupIgnore :
         Result := v_dupIgnore;
       dupAccept :
         Result := v_dupAccept;
       dupError :
         Result := v_dupError;
  end;
 end;

function YD (v : VALUE) : TDuplicates; local;
 begin
  if v = v_dupIgnore
     then Result := dupIgnore
     else if v = v_dupAccept
             then Result := dupAccept
             else if v = v_dupError
                     then Result := dupError
                     else pp_invalidtype(v, 'TDuplicates');
 end;

function StringList_Duplicates (obj : VALUE) : VALUE; cdecl;
 var
   o : TObject;
   r : TDuplicates;
 procedure _;
  begin
   r := (o as TStringList).Duplicates;
  end;

 begin
  o := VJ(obj);
  pp_try(@_);
  Result := DY(r);
 end;

function StringList_set_Duplicates (obj, value : VALUE) : VALUE; cdecl;
 var
   o : TObject;
   v : TDuplicates;
 procedure _;
  begin
   (o as TStringList).Duplicates := v;
  end;
 begin
  o := VJ(obj);
  v := YD(value);
  pp_try(@_);
  Result := value;
 end;

function StringList_Sorted (obj : VALUE) : VALUE; cdecl;
 var
   o : TObject;
   r : Boolean;
 procedure _;
  begin
   r := (o as TStringList).Sorted;
  end;
 begin
  o := VJ(obj);
  pp_try(@_);
  Result := BV(r);
 end;

function StringList_set_Sorted (obj, value : VALUE) : VALUE; cdecl;
 var
   o : TObject;
   v : Boolean;
 procedure _;
  begin
   (o as TStringList).Sorted := v;
  end;
 begin
  o := VJ(obj);
  v := VB(value);
  pp_try(@_);
  Result := value;
 end;

function StringList_CaseSensitive (obj : VALUE) : VALUE; cdecl;
 var
   o : TObject;
   r : Boolean;
 procedure _;
  begin
   r := (o as TStringList).CaseSensitive;
  end;
 begin
  o := VJ(obj);
  pp_try(@_);
  Result := BV(r);
 end;

function StringList_set_CaseSensitive (obj, value : VALUE) : VALUE; cdecl;
 var
   o : TObject;
   v : Boolean;
 procedure _;
  begin
   (o as TStringList).CaseSensitive := v;
  end;
 begin
  o := VJ(obj);
  v := VB(value);
  pp_try(@_);
  Result := value;
 end;

function StringList_New (klass : VALUE) : VALUE; cdecl;
 type
   TStringListClass = class of TStringList;
 var
   c : TStringListClass;
   r : TStringList;
 procedure _;
  begin
   r := c.Create;
  end;
 begin
  c := TStringListClass(VC(klass));
  pp_try(@_);
  Result := NV(r);
 end;

procedure init_TStringList (klass : VALUE);
 begin
  rb_define_method(klass, 'sort', @StringList_Sort, 0);
  pp_define_attr(klass, 'duplicates', @StringList_Duplicates,
                                    @StringList_set_Duplicates);
  pp_define_attr(klass, 'sorted', @StringList_Sorted,
                                @StringList_set_Sorted);
  pp_define_attr(klass, 'casesensitive', @StringList_CaseSensitive,
                                       @StringList_set_CaseSensitive);
  rb_define_singleton_method(klass, 'new', @StringList_New, 0);
 end;

{ TStrings }

function Strings_Add (obj, str : VALUE) : VALUE; cdecl;
 var
   o : TObject;
   s : PChar;
   r : PtrInt;
 procedure _;
  begin
   r := (o as TStrings).Add(s);
  end;
 begin
  o := VJ(obj);
  s := VP(str);
  pp_try(@_);
  Result := IV(r);
 end;

function Strings_AddObject (obj, str, value : VALUE) : VALUE; cdecl;
 var
   o, v : TObject;
   s : PChar;
   r : PtrInt;
 procedure _;
  begin
   r := (o as TStrings).AddObject(s, v);
  end;
 begin
  o := VJ(obj);
  s := VP(str);
  v := VO(value);
  pp_try(@_);
  Result := IV(r);
 end;

function Strings_Append (obj, str : VALUE) : VALUE; cdecl;
 var
   o : TObject;
   s : PChar;
 procedure _;
  begin
   (o as TStrings).Append(s);
  end;
 begin
  o := VJ(obj);
  s := VP(str);
  pp_try(@_);
  Result := obj;
 end;

function Strings_AddStrings (obj, strings : VALUE) : VALUE; cdecl;
 var
   o, s : TObject;
 procedure _;
  begin
   (o as TStrings).AddStrings(s as TStrings);
  end;
 begin
  o := VJ(obj);
  s := VJ(strings);
  pp_try(@_);
  Result := obj;
 end;

function Strings_BeginUpdate (obj : VALUE) : VALUE; cdecl;
 var
   o : TObject;
 procedure _;
  begin
   (o as TStrings).BeginUpdate;
  end;
 begin
  o := VJ(obj);
  pp_try(@_);
  Result := obj;
 end;

function Strings_EndUpdate (obj : VALUE) : VALUE; cdecl;
 var
   o : TObject;
 procedure _;
  begin
   (o as TStrings).EndUpdate;
  end;
 begin
  o := VJ(obj);
  pp_try(@_);
  Result := obj;
 end;

function Strings_Update (obj : VALUE) : VALUE; cdecl;
 var
   o : TObject;
 procedure _bgn;
  begin
   (o as TStrings).BeginUpdate;
  end;
 procedure _end;
  begin
   TStrings(o).EndUpdate;
  end;
 begin
  o := VJ(obj);
  pp_try(@_bgn);
  Result := rb_yield(obj);
  pp_try(@_end);
 end;

function Strings_Clear (obj : VALUE) : VALUE; cdecl;
 var
   o : TObject;
 procedure _;
  begin
   (o as TStrings).Clear;
  end;
 begin
  o := VJ(obj);
  pp_try(@_);
  Result := obj;
 end;

function Strings_Delete (obj, idx : VALUE) : VALUE; cdecl;
 var
   o : TObject;
   i : PtrInt;
 procedure _;
  begin
   (o as TStrings).Delete(i);
  end;
 begin
  o := VJ(obj);
  i := VI(idx);
  pp_try(@_);
  Result := obj;
 end;

function Strings_Exchange (obj, idx1, idx2 : VALUE) : VALUE; cdecl;
 var
   o : TObject;
   i1, i2 : PtrInt;
 procedure _;
  begin
   (o as TStrings).Exchange(i1, i2);
  end;
 begin
  o := VJ(obj);
  i1 := VI(idx1);
  i2 := VI(idx2);
  pp_try(@_);
  Result := obj;
 end;

function Strings_IndexOf (obj, str : VALUE) : VALUE; cdecl;
 var
   o : TObject;
   s : PChar;
   r : PtrInt;
 procedure _;
  begin
   r := (o as TStrings).IndexOf(s);
  end;
 begin
  o := VJ(obj);
  s := VP(str);
  pp_try(@_);
  if r = -1
     then Result := Qnil
     else Result := IV(r);
 end;

function Strings_IndexOfName (obj, name : VALUE) : VALUE; cdecl;
 var
   o : TObject;
   n : PChar;
   r : PtrInt;
 procedure _;
  begin
   r := (o as TStrings).IndexOfName(n);
  end;
 begin
  o := VJ(obj);
  n := VP(name);
  pp_try(@_);
  if r = -1
     then Result := Qnil
     else Result := IV(r);
 end;

function Strings_IndexOfObject (obj, value : VALUE) : VALUE; cdecl;
 var
   o, v : TObject;
   r : PtrInt;
 procedure _;
  begin
   r := (o as TStrings).IndexOfObject(v);
  end;
 begin
  o := VJ(obj);
  v := VO(value);
  pp_try(@_);
  if r = -1
     then Result := Qnil
     else Result := IV(r);
 end;

function Strings_Insert (obj, idx, str : VALUE) : VALUE; cdecl;
 var
   o : TObject;
   i : PtrInt;
   s : PChar;
 procedure _;
  begin
   (o as TStrings).Insert(i, s);
  end;
 begin
  o := VJ(obj);
  i := VI(idx);
  s := VP(str);
  pp_try(@_);
  Result := obj;
 end;

function Strings_InsertObject (obj, idx, str, value : VALUE) : VALUE; cdecl;
 var
   o, v : TObject;
   i : PtrInt;
   s : PChar;
 procedure _;
  begin
   (o as TStrings).InsertObject(i, s, v);
  end;
 begin
  o := VJ(obj);
  i := VI(idx);
  s := VP(str);
  v := VO(value);
  pp_try(@_);
  Result := obj;
 end;

function Strings_LoadFromFile (obj, path : VALUE) : VALUE; cdecl;
 var
   o : TObject;
   p : PChar;
 procedure _;
  begin
   (o as TStrings).LoadFromFile(p);
  end;
 begin
  o := VJ(obj);
  p := VP(path);
  pp_try(@_);
  Result := obj;
 end;

function Strings_LoadFromStream (obj, stream : VALUE) : VALUE; cdecl;
 var
   o, s : TObject;
 procedure _;
  begin
   (o as TStrings).LoadFromStream(s as TStream);
  end;
 begin
  o := VJ(obj);
  s := VJ(stream);
  pp_try(@_);
  Result := obj;
 end;

function Strings_Move (obj, curidx, newidx : VALUE) : VALUE; cdecl;
 var
   o : TObject;
   ci, ni : PtrInt;
 procedure _;
  begin
   (o as TStrings).Move(ci, ni);
  end;
 begin
  o := VJ(obj);
  ci := VI(curidx);
  ni := VI(newidx);
  pp_try(@_);
  Result := obj;
 end;

function Strings_SaveToFile (obj, path : VALUE) : VALUE; cdecl;
 var
   o : TObject;
   p : PChar;
 procedure _;
  begin
   (o as TStrings).SaveToFile(p);
  end;
 begin
  o := VJ(obj);
  p := VP(path);
  pp_try(@_);
  Result := obj;
 end;

function Strings_SaveToStream (obj, stream : VALUE) : VALUE; cdecl;
 var
   o, s : TObject;
 procedure _;
  begin
   (o as TStrings).SaveToStream(s as TStream);
  end;
 begin
  o := VJ(obj);
  s := VJ(stream);
  pp_try(@_);
  Result := obj;
 end;

function Strings_ExtractName (obj, str : VALUE) : VALUE; cdecl;
 var
   o : TObject;
   s, r : PChar;
 procedure _;
  begin
   r := PChar((o as TStrings).ExtractName(s));
  end;
 begin
  o := VJ(obj);
  s := VP(str);
  pp_try(@_);
  Result := SV(r);
 end;

function Strings_Delimiter (obj : VALUE) : VALUE; cdecl;
 var
   o : TObject;
   r : array [0..1] of Char;
 procedure _;
  begin
   r[0] := (o as TStrings).Delimiter;
   r[1] := #0;
  end;
 begin
  o := VJ(obj);
  pp_try(@_);
  Result := SV(@r[0]);
 end;

function Strings_set_Delimiter (obj, value : VALUE) : VALUE; cdecl;
 var
   o : TObject;
   v : PChar;
 procedure _;
  begin
   (o as TStrings).Delimiter := v[0];
  end;
 begin
  o := VJ(obj);
  v := VP(value);
  pp_try(@_);
  Result := value;
 end;

function Strings_DelimitedText (obj : VALUE) : VALUE; cdecl;
 var
   o : TObject;
   r : PChar;
 procedure _;
  begin
   r := PChar((o as TStrings).DelimitedText);
  end;
 begin
  o := VJ(obj);
  pp_try(@_);
  Result := SV(r);
 end;

function Strings_set_DelimitedText (obj, value : VALUE) : VALUE; cdecl;
 var
   o : TObject;
   v : PChar;
 procedure _;
  begin
   (o as TStrings).DelimitedText := v;
  end;
 begin
  o := VJ(obj);
  v := VP(value);
  pp_try(@_);
  Result := value;
 end;

function Strings_StrictDelimiter (obj : VALUE) : VALUE; cdecl;
 var
   o : TObject;
   r : Boolean;
 procedure _;
  begin
   r := (o as TStrings).StrictDelimiter;
  end;
 begin
  o := VJ(obj);
  pp_try(@_);
  Result := BV(r);
 end;

function Strings_set_StrictDelimiter (obj, value : VALUE) : VALUE; cdecl;
 var
   o : TObject;
   v : Boolean;
 procedure _;
  begin
   (o as TStrings).StrictDelimiter := v;
  end;
 begin
  o := VJ(obj);
  v := VB(value);
  pp_try(@_);
  Result := value;
 end;

function Strings_QuoteChar (obj : VALUE) : VALUE; cdecl;
 var
   o : TObject;
   r : array [0..1] of Char;
 procedure _;
  begin
   r[0] := (o as TStrings).QuoteChar;
   r[1] := #0;
  end;
 begin
  o := VJ(obj);
  pp_try(@_);
  result := SV(@r[0]);
 end;

function Strings_set_QuoteChar (obj, value : VALUE) : VALUE; cdecl;
 var
   o : TObject;
   v : PChar;
 procedure _;
  begin
   (o as TStrings).QuoteChar := v[0];
  end;
 begin
  o := VJ(obj);
  v := VP(value);
  pp_try(@_);
  Result := value;
 end;

function Strings_NameValueSeparator (obj : VALUE) : VALUE; cdecl;
 var
   o : TObject;
   r : array [0..1] of Char;
 procedure _;
  begin
   r[0] := (o as TStrings).NameValueSeparator;
   r[1] := #1;
  end;
 begin
  o := VJ(obj);
  pp_try(@_);
  Result := SV(@r[0]);
 end;

function Strings_set_NameValueSeparator (obj, value : VALUE) : VALUE; cdecl;
 var
   o : TObject;
   v : PChar;
 procedure _;
  begin
   (o as TStrings).NameValueSeparator := v[0];
  end;
 begin
  o := VJ(obj);
  v := VP(value);
  pp_try(@_);
  Result := value;
 end;

function Strings_Capacity (obj : VALUE) : VALUE; cdecl;
 var
   o : TObject;
   r : PtrInt;
 procedure _;
  begin
   r := (o as TStrings).Capacity;
  end;
 begin
  o := VJ(obj);
  pp_try(@_);
  Result := IV(r);
 end;

function Strings_set_Capacity (obj, value : VALUE) : VALUE; cdecl;
 var
   o : TObject;
   v : PtrInt;
 procedure _;
  begin
   (o as TStrings).Capacity := v;
  end;
 begin
  o := VJ(obj);
  v := VI(value);
  pp_try(@_);
  Result := value;
 end;

function Strings_CommaText (obj : VALUE) : VALUE; cdecl;
 var
   o : TObject;
   r : PChar;
 procedure _;
  begin
   r := PChar((o as TStrings).CommaText);
  end;
 begin
  o := VJ(obj);
  pp_try(@_);
  Result := SV(r);
 end;

function Strings_set_CommaText (obj, value : VALUE) : VALUE; cdecl;
 var
   o : TObject;
   v : PChar;
 procedure _;
  begin
   (o as TStrings).CommaText := v;
  end;
 begin
  o := VJ(obj);
  v := VP(value);
  pp_try(@_);
  Result := value;
 end;

function Strings_Count (obj : VALUE) : VALUE; cdecl;
 var
   o : TObject;
   r : PtrInt;
 procedure _;
  begin
   r := (o as TStrings).Count;
  end;
 begin
  o := VJ(obj);
  pp_try(@_);
  Result := IV(r);
 end;

function Strings_Text (obj : VALUE) : VALUE; cdecl;
 var
   o : TObject;
   r : PChar;
 procedure _;
  begin
   r := PChar((o as TStrings).Text);
  end;
 begin
  o := VJ(obj);
  pp_try(@_);
  Result := SV(r);
 end;

function Strings_set_Text (obj, value : VALUE) : VALUE; cdecl;
 var
   o : TObject;
   v : PChar;
 procedure _;
  begin
   (o as TStrings).Text := v;
  end;
 begin
  o := VJ(obj);
  v := VP(value);
  pp_try(@_);
  Result := value;
 end;

function Strings_Get (obj, idx : VALUE) : VALUE; cdecl;
 var
   o : TObject;
   n, r : PChar;
   i : PtrInt;
 procedure _int;
  begin
   r := PChar((o as TStrings).Strings[i]);
  end;
 procedure _str;
  begin
   r := PChar((o as TStrings).Values[n]);
  end;
 begin
  o := VJ(obj);
  if FIXNUM_P(idx)
     then begin
           i := VI(idx);
           pp_try(@_int);
          end
     else begin
           n := VP(idx);
           pp_try(@_str);
          end;
  Result := SV(r);
 end;

function Strings_Set (obj, idx, value : VALUE) : VALUE; cdecl;
 var
   o : TObject;
   n, v : PChar;
   i : PtrInt;
 procedure _int;
  begin
   (o as TStrings).Strings[i] := v;
  end;
 procedure _str;
  begin
   (o as TStrings).Values[n] := v;
  end;
 begin
  o := VJ(obj);
  v := VP(value);
  if FIXNUM_P(idx)
     then begin
           i := VI(idx);
           pp_try(@_int);
          end
     else begin
           n := VP(idx);
           pp_try(@_str);
          end;
  Result := value;
 end;

function Strings_Each (obj : VALUE) : VALUE; cdecl;
 var
   o : TObject;
   i, l : PtrInt;
   s : PChar;
 procedure _cnt;
  begin
   l := (o as TStrings).Count;
  end;
 procedure _itm;
  begin
   s := PChar(TStrings(o).Strings[i]);
  end;
 begin
  if rb_block_given_p() <> 0
     then begin
           o := VJ(obj);
           pp_try(@_cnt);
           for i := 0 to l - 1 do
               begin
                pp_try(@_itm);
                rb_yield(SV(s));
               end;
          end;
  Result := obj;
 end;

function Strings_EachObject (obj : VALUE) : VALUE; cdecl;
 var
   o, v : TObject;
   i, l : PtrInt;
   s : PChar;
 procedure _cnt;
  begin
   l := (o as TStrings).Count;
  end;
 procedure _itm;
  begin
   s := PChar(TStrings(o).Strings[i]);
   v := TStrings(o).Objects[i];
  end;
 begin
  if rb_block_given_p() <> 0
     then begin
           o := VJ(obj);
           pp_try(@_cnt);
           for i := 0 to l - 1 do
               begin
                pp_try(@_itm);
                rb_yield_values(2, SV(s), OV(v));
               end;
          end;
  Result := obj;
 end;

function Strings_EachValue (obj : VALUE) : VALUE; cdecl;
 var
   o : TObject;
   i, l : PtrInt;
   n, v : PChar;
 procedure _cnt;
  begin
   l := (o as TStrings).Count;
  end;
 procedure _itm;
  begin
   n := PChar(TStrings(o).Names[i]);
   v := PChar(TStrings(o).ValueFromIndex[i]);
  end;
 begin
  if rb_block_given_p() <> 0
     then begin
           o := VJ(obj);
           pp_try(@_cnt);
           for i := 0 to l - 1 do
               begin
                pp_try(@_itm);
                rb_yield_values(2, SV(n), SV(v));
               end;
          end;
  Result := obj;
 end;

function Strings_ToHash (obj : VALUE) : VALUE; cdecl;
 var
   o : TObject;
   i, l : PtrInt;
   n, v : PChar;
 procedure _cnt;
  begin
   l := (o as TStrings).Count;
  end;
 procedure _itm;
  begin
   n := PChar(TStrings(o).Names[i]);
   v := PChar(TStrings(o).ValueFromIndex[i]);
  end;
 begin
  o := VJ(obj);
  Result := rb_hash_new();
  pp_try(@_cnt);
  for i := 0 to l - 1 do
      begin
       pp_try(@_itm);
       rb_hash_aset(Result, SV(n), SV(v));
      end;
 end;

function Strings_ObjHash (obj : VALUE) : VALUE; cdecl;
 var
   o, v : TObject;
   i, l : PtrInt;
   s : PChar;
 procedure _cnt;
  begin
   l := (o as TStrings).Count;
  end;
 procedure _itm;
  begin
   s := PChar(TStrings(o).Strings[i]);
   v := TStrings(o).Objects[i];
  end;
 begin
  o := VJ(obj);
  Result := rb_hash_new();
  pp_try(@_cnt);
  for i := 0 to l - 1 do
      begin
       pp_try(@_itm);
       rb_hash_aset(Result, SV(s), OV(v));
      end;
 end;

procedure init_TStrings (klass : VALUE);
 begin
  rb_define_method(klass, 'add',            @Strings_Add,            1);
  rb_define_method(klass, 'addobject',      @Strings_AddObject,      2);
  rb_define_method(klass, 'append',         @Strings_Append,         1);
  rb_define_method(klass, 'addstrings',     @Strings_AddStrings,     1);
{  rb_define_method(klass, 'addtext',        @Strings_AddText,        1); }
  rb_define_method(klass, 'beginupdate',    @Strings_BeginUpdate,    0);
  rb_define_method(klass, 'endupdate',      @Strings_EndUpdate,      0);
  rb_define_method(klass, 'update',         @Strings_Update,         0);
  rb_define_method(klass, 'clear',          @Strings_Clear,          0);
  rb_define_method(klass, 'delete',         @Strings_Delete,         1);
  rb_define_method(klass, 'exchange',       @Strings_Exchange,       2);
  rb_define_method(klass, 'indexof',        @Strings_IndexOf,        1);
  rb_define_method(klass, 'indexofname',    @Strings_IndexOfName,    1);
  rb_define_method(klass, 'indexofobject',  @Strings_IndexOfObject,  1);
  rb_define_method(klass, 'insert',         @Strings_Insert,         2);
  rb_define_method(klass, 'insertobject',   @Strings_InsertObject,   3);
  rb_define_method(klass, 'loadfromfile',   @Strings_LoadFromFile,   1);
  rb_define_method(klass, 'loadfromstream', @Strings_LoadFromStream, 1);
  rb_define_method(klass, 'move',           @Strings_Move,           2);
  rb_define_method(klass, 'savetofile',     @Strings_SaveToFile,     1);
  rb_define_method(klass, 'savetostream',   @Strings_SaveToStream,   1);
  rb_define_method(klass, 'extractname',    @Strings_ExtractName,    1);
  pp_define_attr(klass, 'delimiter', @Strings_Delimiter,
                                     @Strings_set_Delimiter);
  pp_define_attr(klass, 'delimitedtext', @Strings_DelimitedText,
                                         @Strings_set_DelimitedText);
  pp_define_attr(klass, 'strictdelimiter', @Strings_StrictDelimiter,
                                           @Strings_set_StrictDelimiter);
  pp_define_attr(klass, 'quotechar', @Strings_QuoteChar,
                                     @Strings_set_QuoteChar);
  pp_define_attr(klass, 'namevalueseparator', @Strings_NameValueSeparator,
                                              @Strings_set_NameValueSeparator);
  pp_define_attr(klass, 'capacity',  @Strings_Capacity,  @Strings_set_Capacity);
  pp_define_attr(klass, 'commatext', @Strings_CommaText,
                                     @Strings_set_CommaText);
  pp_define_attr(klass, 'count',     @Strings_Count,     nil);
  pp_define_attr(klass, 'text',      @Strings_Text,
                                     @Strings_set_Text);
  pp_define_braces(klass, @Strings_Get, @Strings_Set);
  rb_include_module(klass, rb_mEnumerable);
  rb_define_method(klass, 'each',        @Strings_Each,       0);
  rb_define_method(klass, 'each_object', @Strings_EachObject, 0);
  rb_define_method(klass, 'each_value',  @Strings_EachValue,  0);
  rb_define_method(klass, 'to_hash',     @Strings_ToHash,     0);
  rb_define_alias(klass, 'to_h', 'to_hash');
  rb_define_method(klass, 'object_hash', @Strings_ObjHash,    0);
  rb_define_alias(klass, 'to_s', 'text');
  rb_define_alias(klass, '<<', 'append');
 end;

procedure init;
 procedure _;
  begin
   CV(TBits);
   CV(TStringList);
   v_dupIgnore := SY('dupIgnore');
   v_dupAccept := SY('dupAccept');
   v_dupError  := SY('dupError');
  end;
 begin
  pp_protect(@_);
 end;

initialization
 pp_reg_class_hook(TBasicAction,     @init_TBasicAction);
 pp_reg_class_hook(TBasicActionLink, @init_TBasicActionLink);
 pp_reg_class_hook(TBits,            @init_TBits);
 pp_reg_class_hook(TCollection,      @init_TCollection);
 pp_reg_class_hook(TCollectionItem,  @init_TCollectionItem);
 pp_reg_class_hook(TComponent,       @init_TComponent);
 pp_reg_class_hook(TPersistent,      @init_TPersistent);
 pp_reg_class_hook(TStringList,      @init_TStringList);
 pp_reg_class_hook(TStrings,         @init_TStrings);
 pp_reg_init_hook(@init);
end.

