unit Ruby18Script;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LResources, Forms, Controls, Graphics, Dialogs;

type
  TRuby18Scipt = class(TComponent)
  private
    fldRoot : TComponent;
  protected
    { Protected declarations }
  public
    { Public declarations }
  published
    property Root : TComponent read fldRoot write fldRoot;
  end;

procedure Register;

implementation

procedure Register;
begin
  RegisterComponents('Scripts',[TRuby18Scipt]);
end;

end.
