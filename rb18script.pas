unit rb18Script;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LResources, Forms, Controls, Graphics, Dialogs,
  ruby18, rb18System, rb18Classes;

type

  { TRuby18Script }

  TRuby18Script = class(TComponent)
  private
    function getActive : boolean;
    procedure setActive (v : boolean);
  protected
  public
  published
    property Active : boolean read getActive write setActive;
  end;

procedure Register;

implementation

procedure Register;
begin
  RegisterComponents('Scripts',[TRuby18Script]);
end;

{ TRuby18Script }

function TRuby18Script.getActive : boolean;
 begin
 result := rb18System.Active;
 end;

procedure TRuby18Script.setActive(v : boolean);
 begin
 rb18System.Active := v
 end;

function do_application (slf : VALUE) : VALUE; cdecl;
 begin
 result := ObjectToValue(Application);
 end;

procedure InitHook;
 begin
 rb_define_global_function('pasApplication',Pmethod(@do_application),0);
 end;

initialization
 rb18System.AddInitHook(@InitHook)
end.
