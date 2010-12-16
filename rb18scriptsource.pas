unit rb18ScriptSource;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils; 

type
  TScriptSource = class(TComponent)
  private
    function getText : UTF8String; virtual; abstract;
  public
    property Text : UTF8String read getText;
  end;

  { TMemScriptSource }

  TMemScriptSource = class(TScriptSource)
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

procedure Register;

implementation

procedure Register;
begin
  RegisterComponents('Scripts',[TMemScriptSource]);
end;

{ TMemScriptSource }

function TMemScriptSource.getText : UTF8String;
begin
  result := fldLines.Text;
end;

procedure TMemScriptSource.setLines(const Value : TStrings);
begin
  if Value = nil
     then fldLines.Clear
     else fldLines.Assign(Value);
end;

procedure TMemScriptSource.setText(const Value : UTF8String);
begin
  fldLines.Text := Value;
end;

constructor TMemScriptSource.Create(AOwner : TComponent);
begin
  inherited Create(AOwner);
  fldLines := TStringList.Create;
end;

end.

