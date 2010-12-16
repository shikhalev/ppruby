unit rb18Source;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Controls, StdCtrls;

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

  { TRuby18FileSource }

  TRuby18FileSource = class(TRuby18CustomSource)
  private
    fldFileName : UTF8String;
    function getText : UTF8String; override;
  public
    constructor Create (AOwner : TComponent); override;
  published
    property FileName : TFileName read fldFileName write fldFileName;
  end;

  { TRuby18EditSource }

  TRuby18EditSource = class(TRuby18CustomSource)
  private
    fldEdit : TCustomEdit;
    function getText : UTF8String; override;
  public
    constructor Create (AOwner : TComponent); override;
  published
    property Edit : TCustomEdit read fldEdit write fldEdit;
  end;

implementation

{ TRuby18EditSource }

function TRuby18EditSource.getText : UTF8String;
begin
  Result := fldEdit.Text;
end;

constructor TRuby18EditSource.Create(AOwner : TComponent);
begin
  inherited Create(AOwner);
  fldEdit := nil;
end;

{ TRuby18FileSource }

function TRuby18FileSource.getText : UTF8String;
begin

end;

constructor TRuby18FileSource.Create(AOwner : TComponent);
begin
  inherited Create(AOwner);
  fldFileName := '';
end;

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

