unit MainForm;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, SynMemo, SynHighlighterAny, SynUniHighlighter,
  Forms, Controls, Graphics, Dialogs, StdCtrls, ComCtrls, ActnList, Menus,
  ExtCtrls, Ruby, RubyClasses;

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

  private
    fldRuby : TRuby;
    fldOutPut : TPascalOut;
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
         FreeAndNil(fldRuby);
         FreeAndNil(fldOutPut);
         actRuby19.Checked := false;
         actRun.Enabled := false;
         stbMain.Panels[0].Text := '[not loaded]';
         memOutput.Lines.Add('=== Unload');
         memOutput.Lines.Add('');
         end;
 if actRuby18.Checked
    then begin
         FreeAndNil(fldRuby);
         FreeAndNil(fldOutPut);
         actRuby18.Checked := false;
         actRun.Enabled := false;
         stbMain.Panels[0].Text := '[not loaded]';
         memOutput.Lines.Add('=== Unload');
         memOutput.Lines.Add('');
         end
    else try
           fldRuby := TRuby18.Auto;
           fldOutPut := TStringsOut.Create(fldRuby, memOutput.Lines);
           fldRuby.StdOut := fldOutPut.IO;
           fldRuby['frmMain'] := fldRuby.Obj2Val(self);
           actRuby18.Checked := true;
           actRun.Enabled := true;
           stbMain.Panels[0].text := fldRuby.Description;
           memOutput.Lines.Add('=== Load: ' + fldRuby.Description);
           memOutput.Lines.Add('');
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
  self.Close;
 end;

procedure TfrmMain.actFileNewExecute(Sender : TObject);
 begin
  if synMain.Modified
     then ;
 end;

procedure TfrmMain.actFileOpenExecute(Sender : TObject);
 begin
  if dlgOpen.Execute
     then begin
           fldFileName := dlgOpen.FileName;
           synMain.lines.LoadFromFile(fldFileName);
           self.Caption := 'ppRuby Demo: ' + fldFileName;
          end;
 end;

procedure TfrmMain.actFileSaveAsExecute(Sender : TObject);
 begin
  if dlgSave.Execute
     then begin
           fldFileName := dlgSave.FileName;
           synMain.Lines.SaveToFile(fldFileName);
           self.Caption := 'ppRuby Demo: ' + fldFileName;
          end;
 end;

procedure TfrmMain.actFileSaveExecute(Sender : TObject);
 begin
  if fldFileName = ''
     then actFileSaveAs.Execute
     else synMain.Lines.SaveToFile(fldFileName);
 end;

procedure TfrmMain.actHelpAboutExecute(Sender : TObject);
 begin
  //
 end;

procedure TfrmMain.actRuby19Execute(Sender : TObject);
 begin
 if actRuby18.Checked
    then begin
         FreeAndNil(fldRuby);
         FreeAndNil(fldOutPut);
         actRuby18.Checked := false;
         actRun.Enabled := false;
         stbMain.Panels[0].Text := '[not loaded]';
         memOutput.Lines.Add('=== Unload');
         memOutput.Lines.Add('');
         end;
 if actRuby19.Checked
    then begin
         FreeAndNil(fldRuby);
         FreeAndNil(fldOutPut);
         actRuby19.Checked := false;
         actRun.Enabled := false;
         stbMain.Panels[0].Text := '[not loaded]';
         memOutput.Lines.Add('=== Unload');
         memOutput.Lines.Add('');
         end
    else try
           fldRuby := TRuby19.Auto;
           fldOutPut := TStringsOut.Create(fldRuby, memOutput.Lines);
           fldRuby.StdOut := fldOutPut.IO;
           fldRuby['frmMain'] := fldRuby.Obj2Val(self);
           actRuby19.Checked := true;
           actRun.Enabled := true;
           stbMain.Panels[0].text := fldRuby.Description;
           memOutput.Lines.Add('=== Load: ' + fldRuby.Description);
           memOutput.Lines.Add('');
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
    Screen.Cursor := crHourGlass;
    try
      stbMain.Panels[2].Text := '= ' + fldRuby.Val2Str(fldRuby.Inspect(fldRuby.EvalString(synMain.Text)));
      stbMain.Panels[1].Text := 'OK';
    except
      on e : ERubyEval do
         begin
          stbMain.Panels[2].Text := e.Message;
          stbMain.Panels[1].Text := 'Error!';
          raise;
         end;
    end;
  finally
    Screen.Cursor := crDefault;
  end;
 end;

end.

