unit AboutForm;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ExtCtrls,
  Buttons, StdCtrls, LCLIntf;

type

  { TfrmAbout }

  TfrmAbout = class(TForm)
    btnClose : TBitBtn;
    imgLogo : TImage;
    Label1 : TLabel;
    Label2 : TLabel;
    Label3 : TLabel;
    Label4 : TLabel;
    lblLogo : TLabel;
    StaticText1 : TStaticText;
    StaticText2 : TStaticText;
    StaticText3 : TStaticText;
    txtSite : TStaticText;
    txtLicense : TStaticText;
    procedure btnCloseClick(Sender : TObject);
    procedure lblLogoClick(Sender : TObject);
    procedure lblLogoMouseEnter(Sender : TObject);
    procedure lblLogoMouseLeave(Sender : TObject);
    procedure txtLicenseClick(Sender : TObject);
    procedure txtLicenseMouseEnter(Sender : TObject);
    procedure txtLicenseMouseLeave(Sender : TObject);
    procedure txtSiteClick(Sender : TObject);
    procedure txtSiteMouseEnter(Sender : TObject);
    procedure txtSiteMouseLeave(Sender : TObject);
  private
    { private declarations }
  public
    { public declarations }
  end;

var
  frmAbout : TfrmAbout;

implementation

{$R *.lfm}

{ TfrmAbout }

procedure TfrmAbout.btnCloseClick(Sender : TObject);
 begin
  frmAbout.Close;
 end;

procedure TfrmAbout.lblLogoClick(Sender : TObject);
 begin
  OpenURL('https://github.com/shikhalev/ppruby');
 end;

procedure TfrmAbout.lblLogoMouseEnter(Sender : TObject);
 begin
  lblLogo.Font.Underline := True;
  lblLogo.Font.Color := clBlue;
 end;

procedure TfrmAbout.lblLogoMouseLeave(Sender : TObject);
 begin
  lblLogo.Font.Underline := False;
  lblLogo.Font.Color := clNavy;
 end;

procedure TfrmAbout.txtLicenseClick(Sender : TObject);
 begin
  OpenURL('http://www.gnu.org/copyleft/gpl.html');
 end;

procedure TfrmAbout.txtLicenseMouseEnter(Sender : TObject);
 begin
  txtLicense.Font.Underline := True;
  txtLicense.Font.Color := clBlue;
 end;

procedure TfrmAbout.txtLicenseMouseLeave(Sender : TObject);
 begin
  txtLicense.Font.Underline := False;
  txtLicense.Font.Color := clDefault;
 end;

procedure TfrmAbout.txtSiteClick(Sender : TObject);
 begin
  OpenURL('https://github.com/shikhalev/ppruby');
 end;

procedure TfrmAbout.txtSiteMouseEnter(Sender : TObject);
 begin
  txtSite.Font.Underline := True;
  txtSite.Font.Color := clBlue;
 end;

procedure TfrmAbout.txtSiteMouseLeave(Sender : TObject);
 begin
  txtSite.Font.Underline := False;
  txtSite.Font.Color := clDefault;
 end;

end.

