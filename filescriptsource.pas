unit FileScriptSource;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LResources, Forms, Controls, Graphics, Dialogs,
  rb18ScriptSource;

type
  TFileScriptSource = class(TScriptSource)
  private
    { Private declarations }
  protected
    { Protected declarations }
  public
    { Public declarations }
  published
    { Published declarations }
  end;

procedure Register;

implementation

procedure Register;
begin
  {$I filescriptsource_icon.lrs}
  RegisterComponents('Scripts',[TFileScriptSource]);
end;

end.
