{$include rbopts.inc}

unit RbDialogs;

interface

uses
  Dialogs, Ruby, RbTools, RbObjects, RbClasses;

implementation

function CommonDialog_Execute (obj : VALUE) : VALUE; cdecl;
 var
   o : TObject;
   r : Boolean;
 procedure _;
  begin
   r := (o as TCommonDialog).Execute;
  end;
 begin
  o := VJ(obj);
  pp_try(@_);
  Result := BV(r);
 end;

procedure init_TCommonDialog (val : VALUE);
 begin
  rb_define_method(val, 'execute', @CommonDialog_Execute, 0);
 end;

function pp_showmessage (obj, str : VALUE) : VALUE; cdecl;
 var
   p : PChar;
 procedure _;
  begin
   Dialogs.ShowMessage(p);
  end;
 begin
  p := VP(str);
  pp_try(@_);
  Result := obj;
 end;

procedure init_Dialogs (val : VALUE);
 begin
  rb_define_module_function(val, 'showmessage', @pp_showmessage, 1);
 end;

procedure init;
 procedure _;
  begin
   CV(TOpenDialog);
   CV(TSaveDialog);
  end;
 begin
  pp_protect(@_);
 end;

// TODO: make dialogs creatable and register

initialization
 pp_reg_init_hook(@init);
 pp_reg_unit_hook('Dialogs', @init_Dialogs);
 pp_reg_class_hook(TCommonDialog, @init_TCommonDialog);
end.

