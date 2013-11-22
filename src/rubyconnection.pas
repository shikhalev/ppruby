unit RubyConnection;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LResources, Forms, Controls, StdCtrls, Graphics, Dialogs,
  Ruby, RbTools, RbObjects;

type
  TRubyVersion = ( RbV19, RbV20 );
  TRubyVersions = set of TRubyVersion;
  TOutputEvent = procedure (Sender : TObject; var Line : string;
    out Cancel : Boolean) of object;

const
  ALLRUBY = [ RbV19, RbV20 ];

type

  { TRubyConnection }

  TRubyConnection = class(TComponent, IOutput)
  private
    fldLastError : VALUE;
    fldLastResult : VALUE;
    fldOnConnect : TNotifyEvent;
    fldOnDisconnect : TNotifyEvent;
    fldOnOutput : TOutputEvent;
    fldOutput : TCustomMemo;
    fldVersions : TRubyVersions;
    fldCurrent : TRubyVersion;
    function getActive : Boolean;
    function getDescription : string;
    function getLastError : VALUE;
    procedure setActive (value : Boolean);
    procedure setLastError (value : VALUE);
    procedure setVersions (value : TRubyVersions);
  protected
    function Write (str : VALUE) : VALUE;
  public
    constructor Create (o : TComponent); override;
    destructor Destroy; override;
    procedure Connect;
    procedure Connect (ver : TRubyVersions);
    procedure Disconnect;
    function Execute (const code : string) : VALUE;
    function Execute (code : TStrings) : VALUE;
    function ExecuteFile (const filename : string) : VALUE;
    property LastResult : VALUE read fldLastResult;
    property LastError : VALUE read getLastError write setLastError;
    property Description : string read getDescription;
    property Current : TRubyVersion read fldCurrent;
  published
    property Active : Boolean read getActive write setActive default False;
    property Versions : TRubyVersions read fldVersions write setVersions
                                        default ALLRUBY;
    property Output : TCustomMemo read fldOutput write fldOutput;
    property OnConnect : TNotifyEvent read fldOnConnect write fldOnConnect;
    property OnDisconnect : TNotifyEvent read fldOnDisconnect
                                        write fldOnDisconnect;
    property OnOutput : TOutputEvent read fldOnOutput write fldOnOutput;
  end;

type
  ERubyConnectionError = class(ERuby);

implementation

resourcestring
  msgCannotConnect = 'Cannot connect to Ruby!';
  msgNotConnected = 'Connection to Ruby is not active!';

const
  VER_NUMS : array [TRubyVersion] of array [0..1] of Integer = ((1, 9), (2, 0));

{ TRubyConnection }

function TRubyConnection.getActive : Boolean;
 begin
  Result := pp_ruby_active;
 end;

function TRubyConnection.getDescription : string;
 begin
  if Active
     then Result := p_ruby_description
     else Result := '<disconnected>';
 end;

function TRubyConnection.getLastError : VALUE;
 begin
  if Active
     then fldLastError := rb_errinfo();
  Result := fldLastError;
 end;

procedure TRubyConnection.setActive(value : Boolean);
 begin
  if value = Active
     then exit;
  if value
     then Connect
     else Disconnect;
 end;

procedure TRubyConnection.setLastError(value : VALUE);
 begin
  fldLastError := value;
  if Active
     then rb_set_errinfo(fldLastError);
 end;

procedure TRubyConnection.setVersions(value : TRubyVersions);
 begin
  if value = fldVersions
     then exit;
  if Active
     then Disconnect;
  fldVersions := value;
 end;

function TRubyConnection.Write (str : VALUE) : VALUE;
 var
   s : string;
   f : Boolean;
 begin
  s := pp_to_s(str);
  f := False;
  if fldOnOutput <> nil
     then fldOnOutput(self, s, f);
  if f
     then exit;
  if fldOutput <> nil
     then fldOutput.Lines.Text := fldOutput.Lines.Text + s;
  Result := Int2Val(Length(s));
 end;

constructor TRubyConnection.Create(o : TComponent);
 begin
  inherited Create(o);
  fldLastResult := Qnil;
  fldOnConnect := nil;
  fldOnDisconnect := nil;
  fldOnOutput := nil;
  fldVersions := ALLRUBY;
  fldOutput := nil;
 end;

destructor TRubyConnection.Destroy;
 begin
  if Active
     then Disconnect;
  inherited Destroy;
 end;

procedure TRubyConnection.Connect;
 begin
  Connect(fldVersions);
 end;

procedure TRubyConnection.Connect(ver : TRubyVersions);
 var
   i : TRubyVersion;
   o : VALUE;
 begin
  if Active
     then exit;
  for i := High(TRubyVersion) downto Low(TRubyVersion) do
      if i in ver
         then try
               pp_ruby_init(VER_NUMS[i]);
               o := Out2Val(self);
               p_stdout^ := o;
               p_stderr^ := o;
               fldCurrent := i;
               if fldOnConnect <> nil
                  then fldOnConnect(self);
               exit;
              except
                on ERubyLibraryError do
                   continue;
                on Exception do
                   raise;
              end;
  raise ERubyConnectionError.Create(msgCannotConnect);
 end;

procedure TRubyConnection.Disconnect;
 begin
  if not Active
     then exit;
  if fldOnDisconnect <> nil
     then fldOnDisconnect(self);
  pp_ruby_done;
 end;

function TRubyConnection.Execute(const code : string) : VALUE;
 begin
  if not Active
     then raise ERubyConnectionError.Create(msgNotConnected);
  Result := pp_eval(code);
  fldLastResult := Result;
  fldLastError := rb_errinfo();
 end;

function TRubyConnection.Execute(code : TStrings) : VALUE;
 begin
  Result := Execute(code.Text);
 end;

function TRubyConnection.ExecuteFile(const filename : string) : VALUE;
 var
   s : TStrings;
 begin
  s := TStringList.Create;
  s.LoadFromFile(filename);
  Result := Execute(s);
  s.Free;
 end;

end.
