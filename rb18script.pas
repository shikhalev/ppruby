unit rb18Script;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, StdCtrls, Graphics, Dialogs,
  ruby18, rb18System, rb18Classes;

type

  TRuby18CustomSource = class(TComponent)
  protected
    function getText : UTF8String; virtual; abstract;
  public
    property Text : UTF8String read getText;
  end;

  { TRuby18Script }

  TRuby18Script = class(TComponent)
  private
    fldSource : TRuby18CustomSource;
    function getActive : boolean;
    procedure setActive (v : boolean);
  protected
  public
    procedure Execute;
  published
    property Active : boolean read getActive write setActive;
    property Source : TRuby18CustomSource read fldSource write fldSource;
  end;

  TRuby18Source = class(TRuby18CustomSource)
  private
    fldLines : TStrings;
    procedure setLines(const Value : TStrings);
    procedure setText(const Value : UTF8String);
  protected
    function getText : UTF8String; override;
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
  protected
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
  protected
    function getText : UTF8String; override;
  public
    constructor Create (AOwner : TComponent); override;
  published
    property Edit : TCustomEdit read fldEdit write fldEdit;
  end;

implementation

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
   res : integer;
 begin
// oldActive := self.Active;
 self.Active := true;
 rb_eval_string_protect(pchar(fldSource.Text),@res);
 WriteLn(fldSource.Text);
// rb_eval_string(pchar(fldSource.Text));
// self.Active := oldActive;
 if res <> 0
    then raise ERuby.CreateFmt(msgRubyError, [res, 'TRuby18Script.Execute()']);
 end;

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
  result := '';
  if FileExists(fldFileName)
     then with TStringList.Create do
               try
                LoadFromFile(fldFileName);
                result := Text;
               finally
                Free;
               end;
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


{$hints off}
function do_application (slf : VALUE) : VALUE; cdecl;
 begin
 result := ObjectToValue(Application);
 end;
{$hints on}

procedure InitHook;
 begin
 rb_define_global_function('application',Pmethod(@do_application),0);
 end;

initialization
 rb18System.AddInitHook(@InitHook)
end.
