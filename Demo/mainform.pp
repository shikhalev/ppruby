unit MainForm;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  ExtCtrls, ppRuby, ppRubyForms;

type

  { TfrmMain }

  TfrmMain = class(TForm)
    btnRun : TButton;
    memAdd : TMemo;
    memOut : TMemo;
    memScript : TMemo;
    pnlRight : TPanel;
    pnlLeft : TPanel;
    splTB : TSplitter;
    splLR : TSplitter;
    procedure btnRunClick(Sender : TObject);
  private
    { private declarations }
  public
    { public declarations }
  end; 

var
  frmMain: TfrmMain;

implementation

{$R *.lfm}

{ TfrmMain }

procedure TfrmMain.btnRunClick(Sender : TObject);
 var
   oldActive : Boolean;
 begin
  oldActive := ppRuby.Active;
  ppRuby.Active := true;
  EvalString(memScript.Lines.Text);
  ppRuby.Active := oldActive;
 end;

end.

