unit rb18Script;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LResources, Forms, Controls, Graphics, Dialogs,
  ruby18, rb18System, rb18Classes, rb18ScriptSource;

type

  { TRuby18Script }

  TRuby18Script = class(TComponent)
  private
    fldSource : TScriptSource;
    function getActive : boolean;
    procedure setActive (v : boolean);
  protected
  public
    procedure Execute;
  published
    property Active : boolean read getActive write setActive;
    property Source : TScriptSource read fldSource write fldSource;
  end;

procedure Register;

implementation

procedure Register;
begin
  RegisterComponents('Scripts',[TRuby18Script]);
end;

{ TScriptSource }

{ TRuby18Script }

function TRuby18Script.getActive : boolean;
 begin
 result := rb18System.Active;
 end;

procedure TRuby18Script.setActive(v : boolean);
 begin
 if self.Active <> v
    then rb18System.Active := v
 end;

procedure TRuby18Script.Execute;
 var
   oldActive : boolean;
 begin
 oldActive := self.Active;
 self.Active := true;
 rb_eval_string(pchar(fldSource.Text));
 self.Active := oldActive;
 end;

{$hints off}
function do_application (slf : VALUE) : VALUE; cdecl;
 begin
 result := ObjectToValue(Application);
 end;
{$hints on}

procedure InitHook;
 begin
 rb_define_global_function('pasApplication',Pmethod(@do_application),0);
 end;

initialization
 rb18System.AddInitHook(@InitHook)
end.
