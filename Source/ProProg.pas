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
 * Form to add/edit a protected process
 *
 ****************************************************************************)

unit ProProg;

interface

uses Windows, Messages, SysUtils, MBCSUtil, Classes, Graphics, Controls, Forms,
    NewDialogs, ExtCtrls, StdCtrls, Buttons, Dialogs, ThemeMgr, ComboboxEx,
    ShellApi, CommCtrl, ImgList;

type
  TfrmProProg = class(TForm)
    imgProProg: TImage;
    btnOK: TButton;
    btnCancel: TButton;
    Label1: TLabel;
    Label2: TLabel;
    GroupBox1: TGroupBox;
    chkUnsafe: TCheckBox;
    chkTemp: TCheckBox;
    Label4: TLabel;
    ThemeManager1: TThemeManager;
    cmbProProg: TComboBoxEx;
    Images: TImageList;
    procedure chkUnsafeClick(Sender: TObject);
    procedure chkTempClick(Sender: TObject);
    procedure btnOKClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  frmProProg: TfrmProProg;

implementation

{$R *.DFM}

uses Func, CloseWiz;

var
  PrevName: String;

procedure TfrmProProg.chkUnsafeClick(Sender: TObject);
begin
  if chkUnsafe.Checked then begin
    chkTemp.Checked := False;
    chkTemp.Enabled := False;
  end
  else
    chkTemp.Enabled := True;
end;

procedure TfrmProProg.chkTempClick(Sender: TObject);
begin
  if chkTemp.Checked then begin
    chkUnsafe.Checked := False;
    chkUnsafe.Enabled := False;
  end
  else
    chkUnsafe.Enabled := True;
end;

procedure TfrmProProg.btnOKClick(Sender: TObject);
begin
  ModalResult := mrNone;
  if Trim(cmbProProg.Text) = '' then begin
    MsgBox('You have to enter the name of a program, either by selecting a ' +
        'currently running program from the drop-down box, or by clicking the ' +
        'button with the folder icon to browse for a program.',
        bgOK, miError, 'No program name specified', 0);
    ActiveControl := cmbProProg;
    Exit;
  end;
  if not IsValidName(cmbProProg.Text) then begin
    MsgBox('A program''s filename cannot contain any of the following characters:' +
        CrLf + SpaceString('\' + BadDirChars),
        bgOK, miError, 'Invalid program filename', 0);
    ActiveControl := cmbProProg;
    Exit
  end;
  if (ProProgs.IndexOf(cmbProProg.Text) > -1)
      and (cmbProProg.Text <> PrevName) then begin
    MsgBox('The program ' + cmbProProg.Text + ' cannot be added to the list of ' +
        'protected programs, because the list already contains a program with ' +
        'the same name.',
        bgOK, miError, 'Duplicate program error', 0);
    ActiveControl := cmbProProg;
    Exit;
  end;
  ModalResult := mrOK;
end;

procedure TfrmProProg.FormShow(Sender: TObject);
begin
  PrevName := cmbProProg.Text;
end;

procedure TfrmProProg.FormCreate(Sender: TObject);
var
  BaseUnitX, BaseUnitY: Integer;
  {hSIL: THandle;
  FileInfo: TSHFileInfo;}
begin
  SetFormFont(Self, BaseUnitX, BaseUnitY);
  //InitFormFont(Self);
  imgProProg.Picture.Icon.ReleaseHandle;
  imgProProg.Picture.Icon.Handle := LoadIcon(HInstance, PChar(18));
  ConvertTo32BitImageList(Images);
  {hSIL := SHGetFileInfo( '', 0, FileInfo, SizeOf(FileInfo),
      SHGFI_SYSICONINDEX or SHGFI_SMALLICON or SHGFI_USEFILEATTRIBUTES);
  cmbProProg.SetImageList(hSIL);}
end;

end.
