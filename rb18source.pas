unit rb18Source;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils; 

type

  TRuby18CustomSource = class(TComponent)
  private
    function getText : UTF8String; virtual; abstract;
  public
    property Text : UTF8String read getText;
  end;

  TRuby18Source = class(TRuby18CustomSource)
  private
    fldLines : TStrings;
    function getText : UTF8String; override;
    procedure setLines(const Value : TStrings);
    procedure setText(const Value : UTF8String);
  public
    constructor Create (AOwner : TComponent); override;
  public
    property Text : UTF8String read getText write setText;
  published
    property Lines : TStrings read fldLines write setLines;
  end;

implementation

function TRuby18Source.getText : UTF8String;
begin
  result := fldLines.Text;
end;

procedure TRuby18Source.setLines(const Value : TStrings);
begin
  if Value = nil
     then fldLines.Clear
     else fldLines.Assign(Value);
end;

procedure TRuby18Source.setText(const Value : UTF8String);
begin
  fldLines.Text := Value;
end;

constructor TRuby18Source.Create(AOwner : TComponent);
begin
  inherited Create(AOwner);
  fldLines := TStringList.Create;
end;

end.

