unit MainForm;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, SynMemo, SynUniHighlighter, Forms, Controls,
  Graphics, Dialogs, StdCtrls, ComCtrls, ActnList, Menus, StdActns;

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
    MenuItem1: TMenuItem;
    MenuItem10: TMenuItem;
    MenuItem11: TMenuItem;
    MenuItem12: TMenuItem;
    MenuItem13: TMenuItem;
    MenuItem14: TMenuItem;
    MenuItem15: TMenuItem;
    MenuItem16: TMenuItem;
    MenuItem17: TMenuItem;
    MenuItem18: TMenuItem;
    MenuItem19: TMenuItem;
    MenuItem20: TMenuItem;
    MenuItem21: TMenuItem;
    MenuItem22: TMenuItem;
    MenuItem23: TMenuItem;
    MenuItem24: TMenuItem;
    mnuRuby: TMenuItem;
    MenuItem2: TMenuItem;
    MenuItem3: TMenuItem;
    MenuItem4: TMenuItem;
    MenuItem5: TMenuItem;
    MenuItem6: TMenuItem;
    MenuItem7: TMenuItem;
    MenuItem8: TMenuItem;
    MenuItem9: TMenuItem;
    mnuEdit: TMenuItem;
    mnuFile: TMenuItem;
    mnuMain: TMainMenu;
    memOutput: TMemo;
    stbMain: TStatusBar;
    synMain: TSynMemo;
    ToolButton10: TToolButton;
    ToolButton11: TToolButton;
    ToolButton12: TToolButton;
    ToolButton13: TToolButton;
    ToolButton14: TToolButton;
    ToolButton15: TToolButton;
    ToolButton16: TToolButton;
    ToolButton17: TToolButton;
    ToolButton18: TToolButton;
    tbnSep1: TToolButton;
    tbnEditUndo: TToolButton;
    tbnFileSave: TToolButton;
    tbnFileSaveAs: TToolButton;
    ToolButton7: TToolButton;
    ToolButton8: TToolButton;
    ToolButton9: TToolButton;
    uniMain: TSynUniSyn;
    barMain: TToolBar;
    tbnFileNew: TToolButton;
    tbnFileOpen: TToolButton;

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



end.

