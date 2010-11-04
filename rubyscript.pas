unit RubyScript;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LResources, Forms, Controls, Graphics, Dialogs, TextStrings, SynEdit;

type
  TScriptSource = class(TComponent)

  end;

  { TScriptLines }

  TScriptLines = class(TScriptSource)
    private
      FLines: TStrings;
      procedure SetLines(const AValue: TStrings);
    published
      constructor Create(aOwner : TComponent); override;
      property Lines : TStrings read FLines write SetLines;
  end;

  { TScriptFile }

  TScriptFile = class(TScriptSource)
    private
      FFileName: TFileName;
      procedure SetFileName(const AValue: TFileName);
    published
      property FileName : TFileName read FFileName write SetFileName;
  end;

  { TScriptMemo }

  { TScriptControl }

  TScriptControl = class(TScriptSource)
  private
    FControl: TControl;
    procedure SetControl(const AValue: TControl);
    published
      property Control : TControl read FControl write SetControl;
  end;

  { TRubyScript }

  TRubyScript = class(TComponent)
  private
    FSource: TScriptSource;
    procedure SetSource(const AValue: TScriptSource);
  published
    property Source : TScriptSource read FSource write SetSource;
  end;

procedure Register;

implementation

procedure Register;
begin
  RegisterComponents('Scripts',[TRubyScript,TScriptLines,TScriptFile,TScriptControl]);
end;

{ TScriptLines }

procedure TScriptLines.SetLines(const AValue: TStrings);
begin
  if AValue <> nil
     then FLines.Assign (AValue)
end;

constructor TScriptLines.Create(aOwner: TComponent);
begin
    inherited Create (aOwner);
    FLines := TTextStrings.Create;
end;

{ TScriptFile }

procedure TScriptFile.SetFileName(const AValue: TFileName);
begin
  if FFileName=AValue then exit;
  FFileName:=AValue;
end;

{ TRubyScript }

procedure TRubyScript.SetSource(const AValue: TScriptSource);
begin
  if FSource=AValue then exit;
  FSource:=AValue;
end;

{ TScriptControl }

procedure TScriptControl.SetControl(const AValue: TControl);
begin
  if FControl=AValue then exit;
  FControl:=AValue;
end;

end.
