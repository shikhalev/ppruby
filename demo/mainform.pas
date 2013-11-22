{$codepage utf-8}

unit MainForm;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, SynMemo, SynHighlighterAny,
  Forms, Controls, Graphics, Dialogs, StdCtrls, ActnList, Menus, ComCtrls, Ruby,
  RbTools, RbObjects, RbClasses, RbForms, RbDialogs, RubyConnection, LCLIntf,
  StdActns, ExtCtrls, AboutForm;

type

  { TfrmMain }

  TfrmMain = class(TForm)
    actFileNew : TAction;
    actFileOpen : TAction;
    actFileSave : TAction;
    actFileSaveAs : TAction;
    actFileExit : TAction;
    actHelpProjectPage : TAction;
    actHelpAbout : TAction;
    actRubyExecute : TAction;
    actRubyDisconnect : TAction;
    actRubyConnect20 : TAction;
    actRubyConnect19 : TAction;
    actViewClearOutput : TAction;
    actViewOutputFont : TAction;
    actViewCodeFont : TAction;
    alsMain : TActionList;
    dlgFont : TFontDialog;
    actEditCopy : TEditCopy;
    actEditCut : TEditCut;
    actEditPaste : TEditPaste;
    actEditSelectAll : TEditSelectAll;
    actEditUndo : TEditUndo;
    ilsMain : TImageList;
    edtOutput : TMemo;
    mniEditSelectAll : TMenuItem;
    mniEditSep2 : TMenuItem;
    mniEditSep1 : TMenuItem;
    mniEditUndo : TMenuItem;
    mniHelpAbout : TMenuItem;
    mniHelpSep1 : TMenuItem;
    mniHelpProjectPage : TMenuItem;
    mniHelp : TMenuItem;
    mniRubyExecute : TMenuItem;
    mniRubySep1 : TMenuItem;
    mniRubyDisconnect : TMenuItem;
    mniRubyConnect20 : TMenuItem;
    mniRubyConnect19 : TMenuItem;
    mniRuby : TMenuItem;
    mniViewSep1 : TMenuItem;
    mniViewClearOutput : TMenuItem;
    mniViewOutputFont : TMenuItem;
    mniViewCodeFont : TMenuItem;
    mniView : TMenuItem;
    mniEditPaste : TMenuItem;
    mniEditCopy : TMenuItem;
    mniEditCut : TMenuItem;
    mniFileSep2 : TMenuItem;
    mniFileSep1 : TMenuItem;
    mniFileExit : TMenuItem;
    mniFileSaveAs : TMenuItem;
    mniFileSave : TMenuItem;
    mniFileOpen : TMenuItem;
    mniFileNew : TMenuItem;
    mniEdit : TMenuItem;
    mniFile : TMenuItem;
    mnuMain : TMainMenu;
    dlgOpen : TOpenDialog;
    rbcMain : TRubyConnection;
    dlgSave : TSaveDialog;
    splMain : TSplitter;
    stbMain : TStatusBar;
    synSimplifiedRuby : TSynAnySyn;
    edtScript : TSynMemo;
    tbrMain : TToolBar;
    tbtFileNew : TToolButton;
    tbtFileOpen : TToolButton;
    tbtFileSave : TToolButton;
    tbtFileSaveAs : TToolButton;
    tbtSep1 : TToolButton;
    tbtEditUndo : TToolButton;
    tbtEditCut : TToolButton;
    tbtEditCopy : TToolButton;
    tbtEditPaste : TToolButton;
    tbtSep2 : TToolButton;
    tbtViewClearOutput : TToolButton;
    tbtSep3 : TToolButton;
    tbtRubyExecute : TToolButton;
    tbtSep4 : TToolButton;
    tbtRubyConnect19 : TToolButton;
    tbtRubyConnect20 : TToolButton;
    tbtRubyDisconnect : TToolButton;
    tbtSep5 : TToolButton;
    tbtHelpProjectPage : TToolButton;
    tbtHelpAbout : TToolButton;
    tbtSep6 : TToolButton;
    tbtFileExit : TToolButton;
    procedure actFileExitExecute(Sender : TObject);
    procedure actFileNewExecute(Sender : TObject);
    procedure actFileOpenExecute(Sender : TObject);
    procedure actFileSaveAsExecute(Sender : TObject);
    procedure actFileSaveExecute(Sender : TObject);
    procedure actHelpAboutExecute(Sender : TObject);
    procedure actHelpProjectPageExecute(Sender : TObject);
    procedure actRubyConnect19Execute(Sender : TObject);
    procedure actRubyConnect20Execute(Sender : TObject);
    procedure actRubyDisconnectExecute(Sender : TObject);
    procedure actRubyExecuteExecute(Sender : TObject);
    procedure actViewClearOutputExecute(Sender : TObject);
    procedure actViewCodeFontExecute(Sender : TObject);
    procedure actViewOutputFontExecute(Sender : TObject);
    procedure edtScriptChange(Sender : TObject);
    procedure FormCreate(Sender : TObject);
    procedure rbcMainConnect(Sender : TObject);
    procedure rbcMainDisconnect(Sender : TObject);
  private
    fldFile : TFilename;
    function mayReset : Boolean;
    procedure updateCaption;
  public
    { public declarations }
  end;

var
  frmMain : TfrmMain;

implementation

{$R *.lfm}

const
  mainCaption = 'ppRuby Demo: ';

function FindFontName (const names : array of string) : string; local;
 var
   i : Integer;
 begin
  for i := 0 to High(names) do
      if Screen.Fonts.IndexOf(names[i]) >= 0
         then exit(names[i]);
  Result := 'default';
 end;

{ TfrmMain }

procedure TfrmMain.actFileNewExecute(Sender : TObject);
 begin
  if not mayReset
     then exit;
  edtScript.Clear;
  edtScript.Modified := False;
  fldFile := '';
  updateCaption;
 end;

procedure TfrmMain.actFileExitExecute(Sender : TObject);
 begin
  if not mayReset
     then exit;
  frmMain.Close;
 end;

procedure TfrmMain.actFileOpenExecute(Sender : TObject);
 begin
  if not mayReset
     then exit;
  if dlgOpen.Execute
     then begin
           fldFile := dlgOpen.FileName;
           edtScript.Lines.LoadFromFile(fldFile);
           edtScript.Modified := False;
           updateCaption;
          end;
 end;

procedure TfrmMain.actFileSaveAsExecute(Sender : TObject);
 begin
  if dlgSave.Execute
     then begin
           fldFile := dlgSave.FileName;
           edtScript.Lines.SaveToFile(fldFile);
           edtScript.Modified := False;
           updateCaption;
          end;
 end;

procedure TfrmMain.actFileSaveExecute(Sender : TObject);
 begin
  if fldFile = ''
     then actFileSaveAs.Execute
     else begin
           edtScript.Lines.SaveToFile(fldFile);
           edtScript.Modified := False;
           updateCaption;
          end;
 end;

procedure TfrmMain.actHelpAboutExecute(Sender : TObject);
 begin
  frmAbout.ShowModal;
 end;

procedure TfrmMain.actHelpProjectPageExecute(Sender : TObject);
 begin
  OpenURL('https://github.com/shikhalev/ppruby');
 end;

procedure TfrmMain.actRubyConnect19Execute(Sender : TObject);
 var
   flag : Boolean;
 begin
  flag := (rbcMain.Current = RbV20) or not rbcMain.Active;
  if rbcMain.Active
     then rbcMain.Disconnect;
  if flag
     then try
           rbcMain.Connect([RbV19]);
          except
            on ERubyConnectionError do
               actRubyConnect19.Enabled := False;
          end;
 end;

procedure TfrmMain.actRubyConnect20Execute(Sender : TObject);
 var
   flag : Boolean;
 begin
  flag := (rbcMain.Current = RbV19) or not rbcMain.Active;
  if rbcMain.Active
     then rbcMain.Disconnect;
  if flag
     then try
           rbcMain.Connect([RbV20]);
          except
            on ERubyConnectionError do
               actRubyConnect20.Enabled := False;
          end;
 end;

procedure TfrmMain.actRubyDisconnectExecute(Sender : TObject);
 begin
  if rbcMain.Active
     then rbcMain.Disconnect;
 end;

procedure TfrmMain.actRubyExecuteExecute(Sender : TObject);
 begin
  try
   rbcMain.Execute(edtScript.Lines);
   stbMain.Panels[1].Text := 'OK';
   stbMain.Panels[2].Text := '= ' + pp_inspect(rbcMain.LastResult);
  except
   on e : ERubyException do
      begin
       stbMain.Panels[1].Text := 'Error:';
       stbMain.Panels[2].Text := pp_inspect(e.errinfo);
       ShowMessage(e.Message);
      end;
  end;
 end;

procedure TfrmMain.actViewClearOutputExecute(Sender : TObject);
 begin
  edtOutput.Clear;
 end;

procedure TfrmMain.actViewCodeFontExecute(Sender : TObject);
 begin
  dlgFont.Font := edtScript.Font;
  if dlgFont.Execute
     then edtScript.Font := dlgFont.Font;
 end;

procedure TfrmMain.actViewOutputFontExecute(Sender : TObject);
 begin
  dlgFont.Font := edtOutput.Font;
  if dlgFont.Execute
     then edtOutput.Font := dlgFont.Font;
 end;

procedure TfrmMain.edtScriptChange(Sender : TObject);
 begin
  updateCaption;
 end;

procedure TfrmMain.FormCreate (Sender : TObject);
 begin
  edtScript.Font.Name := FindFontName(['PT Mono', 'Anonymous Pro',
                                       'Courier New', 'Monospace']);
  edtOutput.Font.Name := FindFontName(['Droid Sans Mono', 'Fixed',
                                       'Monospace']);
  edtOutput.Clear;
  updateCaption;
 end;

procedure TfrmMain.rbcMainConnect(Sender : TObject);
 begin
  stbMain.Panels[0].Text := rbcMain.Description;
  edtOutput.Lines.Text := edtOutput.Lines.text + '> ' + rbcMain.Description +
                                                        LineEnding;
  actRubyExecute.Enabled := True;
  if rbcMain.Current = RbV19
     then begin
           actRubyConnect19.Checked := True;
           actRubyConnect20.Checked := False;
           actRubyDisconnect.Checked := False;
          end
     else begin
           actRubyConnect19.Checked := False;
           actRubyConnect20.Checked := True;
           actRubyDisconnect.Checked := False;
          end;
 end;

procedure TfrmMain.rbcMainDisconnect(Sender : TObject);
 begin
  stbMain.Panels[0].Text := '<disconnected>';
  actRubyExecute.Enabled := False;
  actRubyConnect19.Checked := False;
  actRubyConnect20.Checked := False;
  actRubyDisconnect.Checked := True;
 end;

function TfrmMain.mayReset : Boolean;
 begin
  Result := False;
  if edtScript.Modified
     then case MessageDlg('Save file?', 'The edited file was modified.' +
                          ' Save it to disk?', mtConfirmation, mbYesNoCancel,
                          0) of
            mrYes :
              begin
               actFileSave.Execute;
               if edtScript.Modified
                  then exit;
              end;
            mrCancel :
              exit;
          end;
  Result := True;
 end;

procedure TfrmMain.updateCaption;
 var
   c : string;
 begin
  if fldFile = ''
     then c := mainCaption + '<untitled>.rb'
     else c := mainCaption + ExtractFileName(fldFile);
  if edtScript.Modified
     then c := c + ' *';
  frmMain.Caption := c;
 end;

end.

