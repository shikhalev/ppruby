unit MainForm;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ComCtrls,
  StdCtrls, ExtCtrls, Buttons, rb18Script, ruby18dynamic, rb18System;

type

  { TfrmMain }

  TfrmMain = class(TForm)
    btnRun : TBitBtn;
    memOutput : TMemo;
    memScript : TMemo;
    pnlControl : TPanel;
    rscMain : TRuby18Script;
    rsrMain : TRuby18EditSource;
    splMain : TSplitter;
    stbMain : TStatusBar;
    procedure btnRunClick(Sender : TObject);
  private
    { private declarations }
  public
    { public declarations }
  end; 

var
  frmMain : TfrmMain;

implementation

{$R *.lfm}

{ TfrmMain }

procedure TfrmMain.btnRunClick(Sender : TObject);
 begin
 rscMain.Execute;
 end;

end.

