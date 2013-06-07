unit RubyControls;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Controls, Ruby;

implementation

function ans_owner (slf : VALUE) : VALUE; cdecl;
 var
   p : TPack;
 begin
  unpack_object(slf, p);
  try
    result := p.rb.Obj2Val((p.obj as TAnchorSide).Owner);
  except
    on e : Exception do
       p.rb.Error(e);
  end;
 end;

function anchorkind2value (rb : TRuby; ak : TAnchorKind) : VALUE; inline;
 begin
  case ak of
  akBottom :
    result := rb.Str2Sym('bottom');
  akLeft :
    result := rb.Str2Sym('left');
  akRight :
    result := rb.Str2Sym('right');
  akTop :
    result := rb.Str2Sym('top');
  else
    result := rb.Qnil;
  end;
 end;

function ans_kind (slf : VALUE) : VALUE; cdecl;
 var
   p : TPack;
 begin
  unpack_object(slf, p);
  try
    result := anchorkind2value(p.rb, (p.obj as TAnchorSide).Kind);
  except
    on e : Exception do
       p.rb.Error(e);
  end;
 end;

{$hints off}
procedure hookTAnchorSide (ruby : TRuby; cls : TClass; value : VALUE);
 begin
  ruby.DefineAttribute(value, 'owner', @ans_owner, nil);
  ruby.DefineAttribute(value, 'kind',  @ans_kind,  nil);
 end;
{$hints on}

function cnt_hide (slf : VALUE) : VALUE; cdecl;
 var
   p : TPack;
 begin
  unpack_object(slf, p);
  try
    (p.obj as TControl).Hide;
    result := slf;
  except
    on e : Exception do
       p.rb.Error(e);
  end;
 end;

function cnt_show (slf : VALUE) : VALUE; cdecl;
 var
   p : TPack;
 begin
  unpack_object(slf, p);
  try
    (p.obj as TControl).Show;
    result := slf;
  except
    on e : Exception do
       p.rb.Error(e);
  end;
 end;

function cnt_refresh (slf : VALUE) : VALUE; cdecl;
 var
   p : TPack;
 begin
  unpack_object(slf, p);
  try
    (p.obj as TControl).Refresh;
    result := slf;
  except
    on e : Exception do
       p.rb.Error(e);
  end;
 end;

function cnt_update (slf : VALUE) : VALUE; cdecl;
 var
   p : TPack;
 begin
  unpack_object(slf, p);
  try
    (p.obj as TControl).Update;
    result := slf;
  except
    on e : Exception do
       p.rb.Error(e);
  end;
 end;

function cnt_parent (slf : VALUE) : VALUE; cdecl;
 var
   p : TPack;
 begin
  unpack_object(slf, p);
  try
    result := p.rb.Obj2Val((p.obj as TControl).Parent);
  except
    on e : Exception do
       p.rb.Error(e);
  end;
 end;

function cnt_parent_set (slf : VALUE; value : VALUE) : VALUE; cdecl;
 var
   p : TPack;
 begin
  unpack_object(slf, p);
  try
    (p.obj as TControl).Parent := TWinControl(p.rb.Val2Obj(value));
    result := value;
  except
    on e : Exception do
       p.rb.Error(e);
  end;
 end;

function cnt_visible (slf : VALUE) : VALUE; cdecl;
 var
   p : TPack;
 begin
  unpack_object(slf, p);
  try
    result := p.rb.Bln2Val((p.obj as TControl).Visible);
  except
    on e : Exception do
       p.rb.Error(e);
  end;
 end;

function cnt_visible_set (slf : VALUE; value : VALUE) : VALUE; cdecl;
 var
   p : TPack;
 begin
  unpack_object(slf, p);
  try
    (p.obj as TControl).Visible := p.rb.Val2Bln(value);
    result := value;
  except
    on e : Exception do
       p.rb.Error(e);
  end;
 end;

{$hints off}
procedure hookTControl (ruby : TRuby; cls : TClass; value : VALUE);
 begin
  ruby.DefineAttribute(value, 'parent',  @cnt_parent,  @cnt_parent_set);
  ruby.DefineAttribute(value, 'visible', @cnt_visible, @cnt_visible_set, true);
  ruby.DefineMethod(value, 'show',    @cnt_show);
  ruby.DefineMethod(value, 'hide',    @cnt_hide);
  ruby.DefineMethod(value, 'refresh', @cnt_refresh);
  ruby.DefineMethod(value, 'update',  @cnt_update);
 end;
{$hints on}

initialization
 TRuby.AddRegisterClassHook(TAnchorSide, @hookTAnchorSide);
 TRuby.AddRegisterClassHook(TControl,    @hookTControl);
end.

