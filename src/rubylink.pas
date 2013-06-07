unit RubyLink;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LResources, Forms, Controls, Graphics, Dialogs, Ruby,
  RubyClasses, RubyControls, RubyForms, StdCtrls;

type
  TRubyVersion = (rvRuby18, rvRuby19, rvRuby20);
  TRubyVersions = set of TRubyVersion;

const
  DEFAULT_VERSION_SET = [rvRuby18, rvRuby19];

type
  TRubyLink = class(TComponent)
  private
    fldVersions : TRubyVersions;
    fldRuby : TRuby;
    fldLib : String;
    fldLink : TComponent;
    fldMemo : TCustomMemo;
    fldSource : TCustomControl;
    fldOut : TStringsOut;
    fldOnActivate, fldOnDeactivate : TNotifyEvent;
    function getActive : Boolean;
    procedure setActive(AValue : Boolean);
    procedure activate;
    procedure deactivate;
  protected
    { Protected declarations }
  public
    constructor Create(AOwner : TComponent); override;
    destructor Destroy; override;
    property Ruby : TRuby read fldRuby;
    function Execute (const code : String) : String;
    function Execute : String;
  published
    property Versions : TRubyVersions read fldVersions write fldVersions;
    property Active : Boolean read getActive write setActive;
    property LibFile : String read fldLib write fldLib;
    property Link : TComponent read fldLink write fldLink;
    property Output : TCustomMemo read fldMemo write fldMemo;
    property Source : TCustomControl read fldSource write fldSource;
    property OnActivate : TNotifyEvent read fldOnActivate write fldOnActivate;
    property OnDeactivate : TNotifyEvent read fldOnDeactivate write fldOnDeactivate;
  end;

type
  ERubyInactive = class(ERuby);

procedure Register;

implementation

procedure Register;
 begin
  {$I rubylink_icon.lrs}
  RegisterComponents('Misc', [TRubyLink]);
 end;

const
  VER_CLASSES : array [TRubyVersion] of TRubyClass = (TRuby18, TRuby19, TRuby20);
  msgRubyInactive = 'Error in %s: Ruby engine is inactive.';

{ TRubyLink }

function TRubyLink.getActive : Boolean;
 begin
  result := not (fldRuby = nil)
 end;

procedure TRubyLink.setActive(AValue : Boolean);
 begin
  if AValue <> getActive
     then if AValue
             then activate
             else deactivate;
 end;

procedure TRubyLink.activate;
 var
   v : TRubyVersion;
 begin
  for v := High(TRubyVersion) downto low(TRubyVersion) do
      if v in fldVersions
         then try
                if fldLib = ''
                   then fldRuby := VER_CLASSES[v].Auto
                   else fldRuby := VER_CLASSES[v].Create(fldLib, Name);
                if fldMemo <> nil
                   then begin
                         fldOut := TStringsOut.Create(fldRuby, fldMemo.Lines);
                         fldRuby.StdOut := fldOut.IO;
                        end;
                if fldLink = nil
                   then fldRuby['link'] := fldRuby.Obj2Val(self)
                   else fldRuby['link'] := fldRuby.Obj2Val(fldLink);
                fldRuby['form'] := fldRuby.Obj2Val(Owner);
                if fldOnActivate <> nil
                   then fldOnActivate(self);
                Exit;
              except
                fldRuby := nil;
                Continue;
              end;
 end;

procedure TRubyLink.deactivate;
 begin
  FreeAndNil(fldOut);
  FreeAndNil(fldRuby);
  if fldOnDeactivate <> nil
     then fldOnDeactivate(self);
 end;

constructor TRubyLink.Create(AOwner : TComponent);
 begin
  inherited Create(AOwner);
  fldVersions := DEFAULT_VERSION_SET;
  fldRuby := nil;
 end;

destructor TRubyLink.Destroy;
 begin
  FreeAndNil(fldOut);
  FreeAndNil(fldRuby);
  inherited Destroy;
 end;

function TRubyLink.Execute(const code : String) : String;
 begin
  if not getActive
     then raise ERubyInactive.CreateFmt(msgRubyInactive, [self.Name]);
  Screen.Cursor := crHourGlass;
  try
    with fldRuby do
         result := Val2Str(Inspect(EvalString(code)));
  finally
    Screen.Cursor := crDefault;
  end;
 end;

function TRubyLink.Execute : String;
 begin
  if not getActive
     then raise ERubyInactive.CreateFmt(msgRubyInactive, [self.Name]);
  if fldSource = nil
     then result := ''
     else result := Execute(fldSource.Caption);
 end;

end.
