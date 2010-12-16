unit rb18SynEditSource;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, rb18Script, SynEdit;

type

  { TRuby18SynEditSource }

  TRuby18SynEditSource = class(TRuby18CustomSource)
  private
    fldSynEdit : TCustomSynEdit;
  protected
    function getText : UTF8String; override;
  public
    constructor Create (AOwner : TComponent); override;
  published
    property SynEdit : TCustomSynEdit read fldSynEdit write fldSynEdit;
  end;

implementation

{ TRuby18SynEditSource }

function TRuby18SynEditSource.getText : UTF8String;
begin
  Result := fldSynEdit.Text;
end;

constructor TRuby18SynEditSource.Create(AOwner : TComponent);
begin
  inherited Create(AOwner);
  fldSynEdit := nil;
end;

end.

