unit MainForm;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, SynMemo, SynHighlighterAny,
  Forms, Controls, Graphics, Dialogs, StdCtrls, ComCtrls, ActnList, Menus,
  ExtCtrls, Ruby, RubyClasses, RubyControls, RubyForms, RubyLink;

type

  { TfrmMain }

  TfrmMain = class(TForm)
    actFileNew: TAction;
    actFileOpen: TAction;
    actFileSave: TAction;
    actFileSaveAs: TAction;
    actFileExit: TAction;
    actEditUndo: TAction;
    actEditCut: TAction;
    actEditCopy: TAction;
    actEditPaste: TAction;
    actEditSelectAll: TAction;
    actCleanOutput: TAction;
    actHelpAbout: TAction;
    actRun: TAction;
    actRuby19: TAction;
    actRuby18: TAction;
    alsMain: TActionList;
    ilsMain: TImageList;
    mnuFileNew: TMenuItem;
    mnuEditCut: TMenuItem;
    mnuEditCopy: TMenuItem;
    mnuEditPaste: TMenuItem;
    mnuSep4: TMenuItem;
    mnuEditSelectAll: TMenuItem;
    mnuHelp: TMenuItem;
    mnuRun: TMenuItem;
    mnuSep6: TMenuItem;
    mnuCleanOutput: TMenuItem;
    mnuSep7: TMenuItem;
    mnuRuby18: TMenuItem;
    mnuRuby19: TMenuItem;
    mnuHelpAbout: TMenuItem;
    mnuRuby: TMenuItem;
    mnuSep1: TMenuItem;
    mnuFileOpen: TMenuItem;
    mnuFileSave: TMenuItem;
    mnuFileSaveAs: TMenuItem;
    mnuSep2: TMenuItem;
    mnuFileExit: TMenuItem;
    mnuEditUndo: TMenuItem;
    mnuSep3: TMenuItem;
    mnuEdit: TMenuItem;
    mnuFile: TMenuItem;
    mnuMain: TMainMenu;
    memOutput: TMemo;
    dlgOpen : TOpenDialog;
    dlgSave : TSaveDialog;
    rblMain : TRubyLink;
    splMain : TSplitter;
    stbMain: TStatusBar;
    anyMain: TSynAnySyn;
    synMain: TSynMemo;
    tbnEditPaste: TToolButton;
    tbnSep3: TToolButton;
    tbnRun: TToolButton;
    tbnCleanOutput: TToolButton;
    tbnSep4: TToolButton;
    tbnRuby18: TToolButton;
    tbnRuby19: TToolButton;
    tbnSep5: TToolButton;
    tbnHelpAbout: TToolButton;
    tbnSep1: TToolButton;
    tbnEditUndo: TToolButton;
    tbnFileSave: TToolButton;
    tbnFileSaveAs: TToolButton;
    tbnSep2: TToolButton;
    tbnEditCut: TToolButton;
    tbnEditCopy: TToolButton;
    barMain: TToolBar;
    tbnFileNew: TToolButton;
    tbnFileOpen: TToolButton;
    procedure actCleanOutputExecute(Sender : TObject);
    procedure actEditCopyExecute(Sender : TObject);
    procedure actEditCutExecute(Sender : TObject);
    procedure actEditPasteExecute(Sender : TObject);
    procedure actEditSelectAllExecute(Sender : TObject);
    procedure actEditUndoExecute(Sender : TObject);
    procedure actFileExitExecute(Sender : TObject);
    procedure actFileNewExecute(Sender : TObject);
    procedure actFileOpenExecute(Sender : TObject);
    procedure actFileSaveAsExecute(Sender : TObject);
    procedure actFileSaveExecute(Sender : TObject);
    procedure actHelpAboutExecute(Sender : TObject);
    procedure actRuby18Execute(Sender : TObject);
    procedure actRuby19Execute(Sender : TObject);
    procedure actRunExecute(Sender : TObject);
    procedure rblMainActivate(Sender : TObject);
    procedure rblMainDeactivate(Sender : TObject);

  private
    fldFileName : ansistring;
  public
    { public declarations }
  end;

var
  frmMain: TfrmMain;

implementation

{$R *.lfm}

{ TfrmMain }

procedure TfrmMain.actRuby18Execute(Sender : TObject);
 begin
  if actRuby19.Checked
     then begin
           rblMain.Active := false;
           actRun.Enabled := false;
           actRuby18.Checked := false;
          end;
  if actRuby18.Checked
     then begin
          rblMain.Active := false;
          actRun.Enabled := false;
          actRuby18.Checked := false;
          end
     else try
            rblMain.Versions := [rvRuby18];
            rblMain.Active := true;
            if rblMain.Active
               then begin
                     actRun.Enabled := true;
                     actRuby18.Checked := true;
                    end
               else actRuby18.Enabled := false;
          except
            on ERuby do
               begin
                actRuby18.Enabled := false;
                raise;
               end;
          end;
 end;

procedure TfrmMain.actCleanOutputExecute(Sender : TObject);
 begin
 memOutput.Clear;
 end;

procedure TfrmMain.actEditCopyExecute(Sender : TObject);
 begin
  synMain.CopyToClipboard;
 end;

procedure TfrmMain.actEditCutExecute(Sender : TObject);
 begin
  synMain.CutToClipboard;
 end;

procedure TfrmMain.actEditPasteExecute(Sender : TObject);
 begin
  synMain.PasteFromClipboard;
 end;

procedure TfrmMain.actEditSelectAllExecute(Sender : TObject);
 begin
  synMain.SelectAll;
 end;

procedure TfrmMain.actEditUndoExecute(Sender : TObject);
 begin
  synMain.Undo;
 end;

procedure TfrmMain.actFileExitExecute(Sender : TObject);
 begin
 if synMain.Modified
    then case MessageDlg('File is modified', 'File is modified, want you save it to disk?', mtConfirmation, mbYesNoCancel, 0, mbYes) of
           mrYes :
             actFileSave.Execute;
           mrCancel :
             Exit;
         end;
  self.Close;
 end;

procedure TfrmMain.actFileNewExecute(Sender : TObject);
 begin
 if synMain.Modified
    then case MessageDlg('File is modified', 'File is modified, want you save it to disk?', mtConfirmation, mbYesNoCancel, 0, mbYes) of
           mrYes :
             actFileSave.Execute;
           mrCancel :
             Exit;
         end;
  synMain.Clear;
  synMain.Modified := false;
  self.Caption := 'ppRuby Demo';
 end;

procedure TfrmMain.actFileOpenExecute(Sender : TObject);
 begin
  if synMain.Modified
     then case MessageDlg('File is modified', 'File is modified, want you save it to disk?', mtConfirmation, mbYesNoCancel, 0, mbYes) of
            mrYes :
              actFileSave.Execute;
            mrCancel :
              Exit;
          end;
  if dlgOpen.Execute
     then begin
           fldFileName := dlgOpen.FileName;
           synMain.lines.LoadFromFile(fldFileName);
           synMain.Modified := false;
           self.Caption := 'ppRuby Demo: ' + fldFileName;
          end;
 end;

procedure TfrmMain.actFileSaveAsExecute(Sender : TObject);
 begin
  if dlgSave.Execute
     then begin
           fldFileName := dlgSave.FileName;
           synMain.Lines.SaveToFile(fldFileName);
           synMain.Modified := false;
           self.Caption := 'ppRuby Demo: ' + fldFileName;
          end;
 end;

procedure TfrmMain.actFileSaveExecute(Sender : TObject);
 begin
  if fldFileName = ''
     then actFileSaveAs.Execute
     else begin
           synMain.Lines.SaveToFile(fldFileName);
           synMain.Modified := false;
          end;
 end;

procedure TfrmMain.actHelpAboutExecute(Sender : TObject);
 begin
  //
 end;

procedure TfrmMain.actRuby19Execute(Sender : TObject);
 begin
  if actRuby18.Checked
     then begin
           rblMain.Active := false;
           actRun.Enabled := false;
           actRuby18.Checked := false;
          end;
  if actRuby19.Checked
     then begin
          rblMain.Active := false;
          actRun.Enabled := false;
          actRuby19.Checked := false;
          end
     else try
            rblMain.Versions := [rvRuby19];
            rblMain.Active := true;
            if rblMain.Active
               then begin
                     actRun.Enabled := true;
                     actRuby19.Checked := true;
                    end
               else actRuby19.Enabled := false;
          except
            on ERuby do
               begin
                actRuby19.Enabled := false;
                raise;
               end;
          end;
 end;

procedure TfrmMain.actRunExecute(Sender : TObject);
 begin
  try
    stbMain.Panels[2].Text := '= ' + rblMain.Execute;
    if rblMain.Ruby.ErrInfo = rblMain.Ruby.Qnil
       then stbMain.Panels[1].Text := 'OK'
       else stbMain.Panels[1].Text := 'Error!';
  except
    on e : ERuby do
       begin
        stbMain.Panels[2].Text := e.Message;
        stbMain.Panels[1].Text := 'Error!';
        raise;
       end;
  end;
 end;

procedure TfrmMain.rblMainActivate(Sender : TObject);
 begin
  stbMain.Panels[0].Text := rblMain.Ruby.Description;
 end;

procedure TfrmMain.rblMainDeactivate(Sender : TObject);
 begin
  stbMain.Panels[0].Text := '[inactive]';
 end;

end.

