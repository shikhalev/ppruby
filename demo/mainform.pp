unit MainForm;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, SynMemo, SynHighlighterAny, Forms, Controls,
  Graphics, Dialogs, StdCtrls, ComCtrls,
  ActnList, Menus, RubyEngine;

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
    actEditDelete: TAction;
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
    mnuEditDelete: TMenuItem;
    mnuSep5: TMenuItem;
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
    procedure actRuby18Execute(Sender : TObject);
    procedure actRuby19Execute(Sender : TObject);
    procedure actRunExecute(Sender : TObject);

  private
    fldRuby : TRubyEngine;
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
         fldRuby.Destroy;
         fldRuby := nil;
         actRuby19.Checked := false;
         actRun.Enabled := false;
         end;
 if actRuby18.Checked
    then begin
         fldRuby.Destroy;
         fldRuby := nil;
         actRuby18.Checked := false;
         actRun.Enabled := false;
         end
    else begin
         fldRuby := TRuby18.Create;
         actRuby18.Checked := true;
         actRun.Enabled := true;
         end;
 end;

procedure TfrmMain.actRuby19Execute(Sender : TObject);
 begin
 if actRuby18.Checked
    then begin
         fldRuby.Destroy;
         fldRuby := nil;
         actRuby18.Checked := false;
         actRun.Enabled := false;
         end;
 if actRuby19.Checked
    then begin
         fldRuby.Destroy;
         fldRuby := nil;
         actRuby19.Checked := false;
         actRun.Enabled := false;
         end
    else begin
         fldRuby := TRuby19.Create;
         actRuby19.Checked := true;
         actRun.Enabled := true;
         end;
 end;

procedure TfrmMain.actRunExecute(Sender : TObject);
 begin
 fldRuby.Execute(synMain.Text);
 end;

end.

