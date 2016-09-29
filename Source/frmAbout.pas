(****************************************************************************
 *
 *            SmartClose
 *
 *            Copyright (c) 2016 Tim De Baets
 *
 ****************************************************************************
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 *
 ****************************************************************************
 *
 * About form
 *
 ****************************************************************************)

unit frmAbout;

interface

uses Windows, Messages, SysUtils, MBCSUtil, Classes, Graphics, Controls, Forms,
    StdCtrls, ExtCtrls, IconView, MyRegistry, NewDialogs;

type
  TAboutFrm = class(TForm)
    GroupBox1: TGroupBox;
    Ico: TIconView;
    btnSysInfo: TButton;
    Label1: TLabel;
    Label2: TLabel;
    Bevel1: TBevel;
    lblVersion: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    GroupBox2: TGroupBox;
    Label5: TLabel;
    Label6: TLabel;
    Label7: TLabel;
    lblWebsite: TLabel;
    Label8: TLabel;
    lblMail: TLabel;
    paCredits: TPanel;
    btnOK: TButton;
    tmrScroll: TTimer;
    lblCredits: TLabel;
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure btnOKClick(Sender: TObject);
    procedure lblMailClick(Sender: TObject);
    procedure lblWebsiteClick(Sender: TObject);
    procedure FormHide(Sender: TObject);
    procedure tmrScrollTimer(Sender: TObject);
    procedure btnSysInfoClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  AboutFrm: TAboutFrm;

implementation

uses Func, Main;

{$R *.DFM}

const
  gREGKEYSYSINFOLOC = 'SOFTWARE\Microsoft\Shared Tools Location';
  gREGVALSYSINFOLOC = 'MSINFO';
  gREGKEYSYSINFO = 'SOFTWARE\Microsoft\Shared Tools\MSINFO';
  gREGVALSYSINFO = 'PATH';
  SYSINFOEXE = 'MSINFO32.EXE';

  CreditsHeader = '============= With special thanks to: =============' + CrLf2;
  CreditsLine = '=========================================';
  CreditsEnd = CrLf + CreditsLine + CrLf + 'The end' + CrLf + CreditsLine + CrLf2;
  CreditSep = CrLf + '-----------' + CrLf;
  Credits =
      CreditsHeader + CrLf +
      'Borland for Delphi 3' + CrLf + CreditSep + CrLf +
      'Jordan Russell and Martijn Laan for the best ' + CrLf +
      'installer available: Inno Setup, and Bjørnar Henden for' + CrLf +
      'ISTool' + CrLf + CreditSep + CrLf +
      'James Holderness for his Cabinet Window Messages article' + CrLf +
      '(Undocumented Windows 95)' + CrLf + CreditSep + CrLf +
      'Brad Stowers (Delphi Free Stuff) for the' + CrLf +
      'TdfsExtProgressBar and TdfsExtListView Delphi components' + CrLf +
      CreditSep + CrLf +
      'Leonid Troyanovsky and Prasad Dabak for their' + CrLf +
      'CreateRemoteThread code' + CrLf + CreditSep + CrLf +
      'Christal for the information about processes and some of' + CrLf +
      'the processes code' + CrLf + CreditSep + CrLf +
      'Yorai Aminov for his ''obsfucator'' code' + CrLf + CreditSep + CrLf +
      'Black Viper for the information about Windows NT services' + CrLf +
      CreditSep + CrLf +
      'Jason Swager for the THugeIni Delphi component' + CrLf + CreditSep + CrLf +
      'Michael Winter for the TProcessMemMgr Delphi component' + CrLf +
      CreditSep + CrLf +
      'Ryan J. Mills for the TPage95Control Delphi component' + CrLf +
      CreditSep + CrLf +
      'Mike Lischke for the Windows XP Theme Manager Delphi' + CrLf +
      'component' + CrLf +
      CreditSep + CrLf +
      'Delphi3000.com and Clive Crocker for some of the services' + CrLf +
      'code' + CrLf +
      CreditSep + CrLf +
      'Peter Plass for the DanHint Delphi component' + CrLf + CreditSep + CrLf +
      'Meryl (Lockergnome) and Chris Becke for the information' + CrLf +
      'about Windows Explorer switches' + CrLf + CreditSep + CrLf +
      'Jim Kueneman for his PIDL code' + CrLf + CreditSep + CrLf +
      'Jonathan De Mey for beta testing SmartClose' + CrLf + CreditSep + CrLf +
      'And last but not least: you, for downloading my program' + CrLf2 +
      'Thanks!' + CrLf + CreditsEnd;


procedure TAboutFrm.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  if frmMain <> nil then
    frmMain.Show
  else
    Application.Terminate;
end;

procedure TAboutFrm.FormCreate(Sender: TObject);
var
  BaseUnitX, BaseUnitY: Integer;
begin
  SetFormFont(Self, BaseUnitX, BaseUnitY);
  //InitFormFont(Self);
  SetFontNameSize(lblCredits.Font, FormFont2, 11, FormFont2Alt, 11);
  SetFontNameSize(lblCredits.Font, FormFont2, 18, FormFont2Alt, 19);
  SetFontNameSize(lblMail.Font, FormFont, 8, '', 8);
  SetFontNameSize(lblWebsite.Font, FormFont, 8, '', 8);
  SetFontNameSize(lblCredits.Font, FormFont2, 8, FormFont2Alt, 10);
  Icon.ReleaseHandle;
  Icon.Handle := LoadIcon(HInstance, PChar(4));
  Ico.LoadFromResourceName('MAINICON', 48);
  lblVersion.Caption := 'Version ' + GetAppVer;
  lblMail.Cursor := crHand;
  lblWebsite.Cursor := crHand;
  lblCredits.Caption := Credits;
  lblCredits.Width := paCredits.Width - 4;
  lblCredits.Top := paCredits.Height - 20;
end;

procedure TAboutFrm.FormShow(Sender: TObject);
begin
  Application.Title := Caption;
  Application.Icon := Self.Icon;
  tmrScroll.Enabled := True;
end;

procedure TAboutFrm.btnOKClick(Sender: TObject);
begin
  Close;
end;

procedure TAboutFrm.lblMailClick(Sender: TObject);
begin
  ExecuteFile('mailto:BM-productions <' + lblMail.Caption + '>', '', '',
      SW_SHOWNORMAL);
end;

procedure TAboutFrm.lblWebsiteClick(Sender: TObject);
begin
  ExecuteFile(lblWebsite.Caption + '/', '', '', SW_SHOWNORMAL);
end;

procedure TAboutFrm.FormHide(Sender: TObject);
begin
  tmrScroll.Enabled := False;
end;

procedure TAboutFrm.tmrScrollTimer(Sender: TObject);
{var
  Point: TPoint;}
begin
  {if GetCursorPos(Point) then begin
    if WindowFromPoint(Point) = paCredits.Handle then
      Exit;
  end}
  paCredits.ScrollBy(0, -1);
  If lblCredits.Top + lblCredits.Height < 0 then
    lblCredits.Top := paCredits.Height;
end;

function GetSysInfoPath: String;
begin
  Result := Trim(GetKeyValue(HKEY_LOCAL_MACHINE, gREGKEYSYSINFO,
      gREGVALSYSINFO, ''));
  if Result = '' then begin
    Result := Trim(GetKeyValue(HKEY_LOCAL_MACHINE, gREGKEYSYSINFOLOC,
        gREGVALSYSINFOLOC, ''));
    if Result = '' then
      Exit;
    Result := AddBackSlash(Result) + SYSINFOEXE;
  end;
  if not FileExists(Result) then
    Result := '';
end;

procedure TAboutFrm.btnSysInfoClick(Sender: TObject);
var
  SysInfoPath: String;
begin
  SysInfoPath := GetSysInfoPath;
  if SysInfoPath = '' then
    MsgBox('System Information is not available.', bgOK, miError, '', 0)
  else
    ExecuteFile(SysInfoPath, '', '', SW_SHOWNORMAL);
end;

end.
