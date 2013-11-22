{$include rbopts.inc}

unit RbForms;

interface

uses
  SysUtils, Forms, Ruby, RbTools, RbObjects;

implementation

{$hints off}
function pp_screen (obj : VALUE) : VALUE; cdecl;
 begin
  Result := OV(Forms.Screen);
 end;

function pp_application (obj : VALUE) : VALUE; cdecl;
 begin
  Result := OV(Forms.Application);
 end;
{$hints on}

procedure init_Forms (val : VALUE);
 begin
  rb_define_module_function(val, 'application', @pp_application, 0);
  rb_define_module_function(val, 'screen', @pp_screen, 0);
 end;

procedure init;
 procedure _;
  begin
   CV(TApplication);
  end;
 begin
  pp_protect(@_);
 end;

initialization
 pp_reg_init_hook(@init);
 pp_reg_unit_hook('Forms', @init_Forms);
end.

