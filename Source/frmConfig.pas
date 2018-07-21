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
 * Settings form
 *
 ****************************************************************************)

unit frmConfig;

interface

uses Windows, Messages, SysUtils, MBCSUtil, Classes, Graphics, Controls, Forms,
    NewDialogs, ComCtrls95, ThemeMgr, StdCtrls, CaptionBevel, ExtCtrls, FileCtrl,
    ShlObj, ActiveX, ComboboxEx, ShellApi, MyRegistry, IconView, ComCtrls,
    EnhButtn, ImgList;

type
  TConfig = class(TForm)
    Pages: TPage95Control;
    shGeneral: TTab95Sheet;
    shCloseProgs: TTab95Sheet;
    Images: TImageList;
    btnCancel: TButton;
    btnApply: TButton;
    btnOK: TButton;
    Label1: TLabel;
    CaptionBevel2: TCaptionBevel;
    CaptionBevel1: TCaptionBevel;
    Label2: TLabel;
    btnBrowse: TButton;
    Label3: TLabel;
    btnCreate: TButton;
    Label4: TLabel;
    txtSnapPath: TEdit;
    btnSnapBrowse: TButton;
    chkAssoc: TCheckBox;
    shRestore: TTab95Sheet;
    shServices: TTab95Sheet;
    Label5: TLabel;
    cmbTarget: TComboBoxEx;
    cmbFolder: TComboBoxEx;
    imgLink: TImage;
    imgHelp: TImage;
    CaptionBevel3: TCaptionBevel;
    chkShowProgs: TCheckBox;
    chkShowProgsKill: TCheckBox;
    SmartKillBevel: TCaptionBevel;
    chkKillNonVis: TCheckBox;
    chkKillFailed: TCheckBox;
    Label6: TLabel;
    Label7: TLabel;
    chkAutoExit: TCheckBox;
    CaptionBevel5: TCaptionBevel;
    chkShowRestoreProgs: TCheckBox;
    chkProgsWait: TCheckBox;
    chkResAutoExit: TCheckBox;
    imgWiz: TImage;
    imgResWiz: TImage;
    ThemeManager1: TThemeManager;
    CaptionBevel6: TCaptionBevel;
    chkAllowScreenSaver: TCheckBox95;
    chkAllowTaskSched: TCheckBox95;
    icoSnap: TIconView;
    chkUncheckProgs: TCheckBox95;
    imgServ: TImage;
    lblServ: TLabel;
    Label8: TLabel;
    btnEditServices: TButton;
    Label9: TLabel;
    CaptionBevel7: TCaptionBevel;
    lblServices: TLabel;
    btnManageServices: TButton;
    procedure FormCreate(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure btnOKClick(Sender: TObject);
    procedure btnCancelClick(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure btnCreateClick(Sender: TObject);
    procedure btnBrowseClick(Sender: TObject);
    procedure cmbFolderExit(Sender: TObject);
    procedure btnApplyClick(Sender: TObject);
    procedure UpdateApply(Sender: TObject);
    procedure txtSnapPathExit(Sender: TObject);
    procedure btnSnapBrowseClick(Sender: TObject);
    procedure txtSnapPathEnter(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure btnManageServicesClick(Sender: TObject);
    procedure btnEditServicesClick(Sender: TObject);
  private
    procedure UpdateTab(TabIndex: Integer);
    procedure SaveTab(TabIndex: Integer);
    procedure SaveTabs;
    function CheckData: Boolean;
    function GetShortcutPath: String;
    function AddFolder(PIDL: PItemIDList; Caption: String): Integer;
    procedure SetItemIcons(Index: Integer; PIDL: PItemIDList);
    procedure WMApp(var Message: TMessage); message WM_APP;
  end;

const
  ServMgmNT = 'Services Control Panel Applet';
  ServMgm2k = 'Service Management';
  ServMgm2k2 = ServMgm2k + ' utility';

var
  Config: TConfig;

implementation

{$R *.DFM}

uses Func, Main, ExpWin, Snapshot, Service;

var
  Loading: Boolean = False;
  PrevAssoc: Boolean;
  PrevSnapPath: String;

procedure TConfig.FormCreate(Sender: TObject);
var
  Ico: TIcon;
  ImageIdx, ItemIdx: Integer;
  BaseUnitX, BaseUnitY: Integer;
  i: Integer;
begin
  SetFormFont(Self, BaseUnitX, BaseUnitY);
  //InitFormFont(Self);
  Config.Cursor := crHourGlass;
  Loading := True;
  icoSnap.LoadFromResourceID(5, 16);
  imgLink.Picture.Bitmap.LoadFromResourceName(hInstance, 'LINK');
  imgWiz.Picture.Bitmap.LoadFromResourceName(HInstance, 'WIZ16');
  imgHelp.Picture.Bitmap.LoadFromResourceName(HInstance, 'HELP');
  imgResWiz.Picture := imgWiz.Picture;
  try
    Config.Icon.ReleaseHandle;
    Config.Icon.Handle := LoadIcon(HInstance, PChar(3));
    ConvertTo32BitImageList(Images);
    cmbTarget.Images := Images;
    Ico := TIcon.Create;
    try
      Ico.Handle := LoadIcon(HInstance, 'MAINICON');
      ImageIdx := Images.AddIcon(Ico);
      shGeneral.ImageIndex := ImageIdx;
      ItemIdx := cmbTarget.Items.Add('SmartClose');
      cmbTarget.ImageIndex[ItemIdx] := ImageIdx;
      Ico.Handle := LoadIcon(HInstance, PChar(1));
      ImageIdx := Images.AddIcon(Ico);
      shCloseProgs.ImageIndex := ImageIdx;
      ItemIdx := cmbTarget.Items.Add('SmartClose - Close Programs');
      cmbTarget.ImageIndex[ItemIdx] := ImageIdx;
      Ico.Handle := LoadIcon(HInstance, PChar(2));
      ImageIdx := Images.AddIcon(Ico);
      shRestore.ImageIndex := ImageIdx;
      ItemIdx := cmbTarget.Items.Add('SmartClose - Restore Snapshot');
      cmbTarget.ImageIndex[ItemIdx] := ImageIdx;
      Ico.Handle := LoadIcon(HInstance, PChar(3));
      ImageIdx := Images.AddIcon(Ico);
      ItemIdx := cmbTarget.Items.Add('SmartClose - Settings');
      cmbTarget.ImageIndex[ItemIdx] := ImageIdx;
      Ico.Handle := LoadIcon(HInstance, PChar(4));
      ImageIdx := Images.AddIcon(Ico);
      ItemIdx := cmbTarget.Items.Add('SmartClose - About');
      cmbTarget.ImageIndex[ItemIdx] := ImageIdx;
      if IsWinNT then begin
        Ico.Handle := LoadIcon(HInstance, PChar(13));
        ImageIdx := Images.AddIcon(Ico);
        shServices.ImageIndex := ImageIdx;
        imgServ.Picture.Icon := Ico;
      end;
    finally
      Ico.Free;
    end;
    if not IsWinNT then begin
      shServices.TabVisible := False;
      shServices.Visible := False;
    end
    else begin
      lblServices.Caption := Format(lblServices.Caption,
          [IIf(Win32MajorVersion < 5, ServMgmNT, ServMgm2k2)]);
      btnManageServices.Caption := Format(btnManageServices.Caption,
          [IIf(Win32MajorVersion < 5, ServMgmNT, ServMgm2k)]);
    end;
    if not IsWinNT then begin
      SmartKillBevel.Caption := SmartKillBevel.Caption +
          ' (Currently not available on Windows 95, 98 and ME)';
      chkKillNonVis.Enabled := False;
      chkKillFailed.Enabled := False;
    end;
    for i := 0 to Pages.PageCount - 1 do begin
      if Pages.Pages[i].TabVisible then
        UpdateTab(i + 1);
    end;
    Pages.ActivePage := shGeneral;
  finally
    Config.Cursor := crDefault;
    Loading := False;
  end;
end;

procedure TConfig.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  if frmMain <> nil then
    frmMain.Show
  else
    Application.Terminate;
end;

procedure TConfig.btnOKClick(Sender: TObject);
begin
  if btnApply.Enabled then begin
    if not CheckData then
      Exit;
    SaveTabs;
  end;
  Close;
end;

procedure TConfig.btnCancelClick(Sender: TObject);
begin
  Close;
end;

procedure TConfig.FormDestroy(Sender: TObject);
var
  i: Integer;
begin
  for i := 0 to cmbFolder.Items.Count - 1 do begin
    if Assigned(cmbFolder.Items.Objects[i]) then
      CoTaskMemFree(PItemIDList(cmbFolder.Items.Objects[i]));
  end;
  cmbFolder.SetImageList(0);
end;

procedure TConfig.UpdateTab(TabIndex: Integer);
var
  Flags: Integer;
  Path: String;
  PIDL: PItemIDList;
  hSIL: THandle;
  FileInfo: TSHFileInfo;
  CloseSettings: TCloseSettings;
  RestoreSettings: TRestoreSettings;

  procedure AddShellFolder(FolderID: Integer; Caption: String);
  var
    i: Integer;
  begin
    Path := GetShellFolder(FolderID or Flags, PIDL);
    if (PIDL <> nil) and (Path <> '') and DirectoryExists(Path) then begin
      i := AddFolder(PIDL, Caption);
      if FolderID = CSIDL_DESKTOPDIRECTORY then begin
        GetShellFolder(CSIDL_DESKTOP, PIDL);
        SetItemIcons(i, PIDL);
        CoTaskMemFree(PIDL);
      end;
    end;
  end;
  
begin
  case TabIndex of
    1: begin
      hSIL := SHGetFileInfo('', 0, FileInfo, SizeOf(FileInfo),
          SHGFI_SYSICONINDEX or SHGFI_SMALLICON or SHGFI_USEFILEATTRIBUTES);
      cmbFolder.SetImageList(hSIL);
      if GetDllVersion(shell32) >= 500 then
        Flags := CSIDL_FLAG_CREATE
      else
        Flags := 0;
      AddShellFolder(CSIDL_PROGRAMS, 'Start Menu');
      AddShellFolder(CSIDL_COMMON_PROGRAMS, 'Start Menu (all users)');
      AddShellFolder(CSIDL_DESKTOPDIRECTORY, 'Desktop');
      AddShellFolder(CSIDL_COMMON_DESKTOPDIRECTORY, 'Desktop (all users)');
      Path := GetShellFolder(CSIDL_APPDATA or Flags, PIDL);
      if PIDL <> nil then begin
        CoTaskMemFree(PIDL);
        if (Path <> '') and DirectoryExists(Path) then begin
          Path := AddBackSlash(Path) + 'Microsoft\Internet Explorer\Quick Launch';
          if DirectoryExists(Path) then begin
            PIDL := PathToPIDL(Path);
            if PIDL <> nil then
              AddFolder(PIDL, 'Quick Launch');
          end;
        end;
      end;
      cmbFolder.ItemIndex := 0;
      cmbTarget.ItemIndex := 0;
      //
      PrevAssoc := CheckSnapFileAssoc;
      chkAssoc.Checked := PrevAssoc;
      PrevSnapPath := SnapPath;
      txtSnapPath.Text := PrevSnapPath;
    end;
    //
    2: begin
      LoadCloseSettings(CloseSettings);
      with CloseSettings do begin
        chkShowProgs.Checked := ShowProgs;
        chkShowProgsKill.Checked := ShowProgsTerm;
        chkAutoExit.Checked := AutoExit;
        chkKillNonVis.Checked := TerminateNonVis;
        chkKillFailed.Checked := TerminateCloseFail;
      end;
    end;
    //
    3: begin
      LoadRestoreSettings(RestoreSettings);
      with RestoreSettings do begin
        chkUncheckProgs.Checked := UncheckProgs;
        chkShowRestoreProgs.Checked := ShowProgs;
        chkProgsWait.Checked := ProgsWait;
        chkResAutoExit.Checked := AutoExit;
        chkAllowScreenSaver.Checked := AllowScreenSaver;
        if not IsWinNT then
          chkAllowTaskSched.Checked := AllowTaskSched
        else
          chkAllowTaskSched.Visible := False;
      end;
    end;
    //
    4: begin
      {Svcs := TStringList.Create;
      SvcNames := TStringList.Create;
      try
        GetServices(False, Svcs, SvcNames);
        if Svcs.Count = 0 then begin
          ShowWait(False, 'Restoring the list of default services...');
          try
            RestoreDefaultServices;
            GetServices(False, Svcs, SvcNames);
          finally
            CloseWait;
          end;
        end;
        if Svcs.Count = 0 then
          Exit;
      finally
        Svcs.Free;
        SvcNames.Free;
      end;}
    end;
  end;
end;

procedure TConfig.SaveTab(TabIndex: Integer);
var
  Path: String;
begin
  case TabIndex of
    1: begin
      if chkAssoc.Checked <> PrevAssoc then begin
        if chkAssoc.Checked then
          RegSnapFileAssoc
        else
          UnRegSnapFileAssoc;
        PrevAssoc := CheckSnapFileAssoc;
        chkAssoc.Checked := PrevAssoc;
      end;
      Path := txtSnapPath.Text;
      if AnsiCompareFileName(Path, PrevSnapPath) <> 0 then begin
        if not RemoveEmptySnapPath(SnapPath) then begin
          if MsgBox('There are still one or more System Snapshot files located in the ' +
              'previous Snapshot folder:' + CrLf2 + PrevSnapPath + CrLf2 +
              'Do you want SmartClose to remove these now?',
              bgYesNo, miQuestion, 'Remove System Snapshot(s) ?', 0) = IDYES then
            RemoveSnapFiles(Handle, False);
        end;
        SetKeyValue(Hive, RegKey, 'SnapPath', Path);
        PrevSnapPath := GetSnapPath;
        txtSnapPath.Text := PrevSnapPath;
      end;
    end;
    //
    2: begin
      with TMyRegistry.Create do try
        RootKey := Hive;
        OpenKey(RegKey, True);
        WriteBool('ShowProgs', chkShowProgs.Checked);
        WriteBool('ShowProgsTerm', chkShowProgsKill.Checked);
        WriteBool('AutoExit', chkAutoExit.Checked);
        WriteBool('TerminateNonVis', chkKillNonVis.Checked);
        WriteBool('TerminateCloseFail', chkKillFailed.Checked);
      finally
        Free;
      end;
    end;
    //
    3: begin
      with TMyRegistry.Create do try
        RootKey := Hive;
        OpenKey(RegKey, True);
        WriteBool('UncheckProgs', chkUncheckProgs.Checked);
        WriteBool('ShowRestoreProgs', chkShowRestoreProgs.Checked);
        WriteBool('ProgsWait', chkProgsWait.Checked);
        WriteBool('Res_AutoExit', chkResAutoExit.Checked);
        WriteBool('AllowSS', chkAllowScreenSaver.Checked);
        if not IsWinNT then
          WriteBool('AllowTaskSched', chkAllowTaskSched.Checked);
      finally
        Free;
      end;
    end;
  end;
end;

procedure TConfig.SaveTabs;
var
  i: Integer;
begin
  Config.Cursor := crHourGlass;
  try
    for i := 0 to Pages.PageCount - 1 do begin
      if Pages.Pages[i].TabVisible then
        SaveTab(i + 1);
    end;
  finally
    Config.Cursor := crDefault;
  end;
end;

function TConfig.CheckData: Boolean;
var
  Path: String;
  CheckDir: TCheckDirResult;
begin
  Result := False;
  txtSnapPathExit(Self);
  Path := txtSnapPath.Text;
  if Path = '' then
    Exit;
  CheckDir := CheckDirName(Path, False);
  Result := (CheckDir = cdnValid);
  if not Result then begin
    Pages.ActivePage := Pages.Pages[0];
    txtSnapPath.SetFocus;
  end;
  case CheckDir of
    cdnInvalid:
      MsgBox('You must enter a full path with drive letter for the System ' +
          'Snapshot folder, for example:' + CrLf2 + 'C:\Snapshots' + CrLf2 +
          'or a UNC path in the form:' + CrLf2 + '\\server\share',
          bgOK, miError, 'Invalid Path', 0);
    cdnBadChars:
      MsgBox('The System Snapshot folder cannot include any of the following ' +
          'characters: ' + CrLf2 + SpaceString(BadDirChars),
          bgOK, miError, 'Invalid Path', 0);
    cdnInvalidDrive:
      MsgBox('The drive or UNC share in the System Snapshot folder does not exist ' +
          'or is not accessible.',
          bgOK, miError, 'Folder Error', 0);
  end;
  if not Result then
    Exit;
  Result := False;
  if not DirectoryExists(Path) then begin
    Pages.ActivePage := Pages.Pages[0];
    txtSnapPath.SetFocus;
    if MsgBox('The new Snapshot folder:' + CrLf2 + Path + CrLf2 + 'does not exist. ' +
        'Would you like the folder to be created?',
        bgYesNo, miQuestion, 'Folder Does Not Exist', 0) = IDYES then begin
      if not MakeDir(Path) then begin
        MsgBox('SmartClose failed to create the new Snapshot folder.',
            bgOK, miError, '', 0);
      end
      else
        Result := True;
    end
  end
  else
    Result := True;
end;

procedure TConfig.SetItemIcons(Index: Integer; PIDL: PItemIDList);
var
  FileInfo: TSHFileInfo;
begin
  with cmbFolder do begin
    SHGetFileInfo(PChar(PIDL), 0, FileInfo, SizeOf(FileInfo),
        SHGFI_PIDL or SHGFI_SYSICONINDEX or SHGFI_SMALLICON);
    ImageIndex[Index] := FileInfo.iIcon;
    SHGetFileInfo(PChar(PIDL), 0, FileInfo, SizeOf(FileInfo),
        SHGFI_PIDL or SHGFI_SYSICONINDEX or SHGFI_OPENICON);
    SelectedIndex[Index] := FileInfo.iIcon;
    {SHGetFileInfo(PChar(PIDL), 0, FileInfo, SizeOf(FileInfo),
        SHGFI_PIDL or SHGFI_SYSICONINDEX or SHGFI_LINKOVERLAY);
    OverlayIndex[Index] := FileInfo.iIcon;}
  end;
end;

function TConfig.AddFolder(PIDL: PItemIDList; Caption: String): Integer;
begin
  Result := cmbFolder.Items.AddObject(Caption, TObject(PIDL));
  SetItemIcons(Result, PIDL);
end;

function TConfig.GetShortcutPath: String;
var
  PIDL: PItemIDList;
  Buffer: array[0..MAX_PATH - 1] of Char;
begin
  Result := '';
  PIDL := nil;
  if cmbFolder.ItemIndex > -1 then
    PIDL := PItemIDList(cmbFolder.Items.Objects[cmbFolder.ItemIndex]);
  if PIDL <> nil then begin
    if SHGetPathFromIDList(PIDL, Buffer) then
      Result := Buffer;
  end
  else if (PIDL = nil) and DirectoryExists(cmbFolder.Text) then
    Result := cmbFolder.Text;
end;

procedure TConfig.btnCreateClick(Sender: TObject);
const
  Num = 5;
  Names: array[0..Num - 1] of String = (
    'SmartClose',
    'SmartClose - Close Programs',
    'SmartClose - Restore Snapshot',
    'SmartClose - Settings',
    'SmartClose - About');
  Descs: array[0..Num - 1] of String = (
    'Click here to run the Close Programs or the Restore Wizard, to configure ' +
        'SmartClose or to access SmartClose help.',
    'Click here to create a system snapshot and to close all programs, among ' +
        'other options.',
    'Click here to restore a system snapshot that has been created with the ' +
        'Close Programs Wizard.',
    'Opens the SmartClose Settings window, where you can view and change various ' +
        'SmartClose settings.',
    'Click here for more information about SmartClose like version, author, ' +
        'copyright, contact info, website...');
  Params: array[0..Num - 1] of String = (
    '',
    '/closeprogs',
    '/restore',
    '/config',
    '/about');
var
  Path, Filename: String;
  Idx: Integer;
begin
  cmbFolderExit(Self);
  Path := GetShortcutPath;
  if Path = '' then begin
    if cmbFolder.Text = '' then begin
      MsgBox('Please specify the location where the shortcut should be placed, ' +
          'by selecting a location from the drop-down list, or by clicking Browse. ' +
          'You can also type an (existing) path manually.',
          bgOK, miWarning, 'No location specified', 0);
    end
    else if not DirectoryExists(cmbFolder.Text) then begin
      MsgBox('The location where you want the shortcut to be created does not ' +
          'exist. ' +
          'Please check the path and try again, select a location from the ' +
          'drop-down list or click Browse.',
          bgOK, miWarning, 'Specified location doesn''t exist', 0);
    end;
    cmbFolder.ItemIndex := 0;
    cmbFolder.SetFocus;
    Exit;
  end;
  Idx := cmbTarget.ItemIndex;
  Filename := AddBackSlash(Path) + Names[Idx] + '.lnk';
  if CreateShellLink(Filename, Descs[Idx], ParamStr(0), Params[Idx],
      '', ParamStr(0), Idx, SW_SHOWNORMAL, 0) then begin
    SHChangeNotify(SHCNE_CREATE, SHCNF_PATH, PChar(Filename), nil);
    SHChangeNotify(SHCNE_UPDATEDIR, SHCNF_PATH or SHCNF_FLUSH,
        PChar(RemoveBackslashUnlessRoot(ExtractFilePath(Filename))), nil);
    if cmbFolder.ItemIndex = -1 then begin
      cmbFolder.ItemIndex := AddFolder(PathToPIDL(Path),
          RemoveBackSlashUnlessRoot(Path));
    end;
    MsgBox('Shortcut successfully created in ' + cmbFolder.Text + '.',
        bgOK, miInfo, 'Shortcut Created', 0);
  end
  else
    MsgBox('Error while creating the shortcut.', bgOK, miError, '', 0);
end;

procedure TConfig.btnBrowseClick(Sender: TObject);
var
  PIDL: PItemIDList;
  Path: String;
begin
  cmbFolderExit(Self);
  Path := GetShortcutPath;
  PIDL := BrowseForFolder(Handle,
      'Please select the folder where you want a shortcut to be created:', Path);
  if (PIDL <> nil) and (Path <> '') then
    cmbFolder.ItemIndex := AddFolder(PIDL, RemoveBackSlashUnlessRoot(Path));
end;

procedure TConfig.cmbFolderExit(Sender: TObject);
begin
  with cmbFolder do begin
    if (ItemIndex < 0)
        or (Integer(cmbFolder.Items.Objects[cmbFolder.ItemIndex]) = 0) then
      Text := FormatDir(Text);
  end;
end;

procedure TConfig.UpdateApply(Sender: TObject);
begin
  if not Loading then
    btnApply.Enabled := True;
end;

procedure TConfig.btnApplyClick(Sender: TObject);
begin
  if not CheckData then
    Exit;
  SaveTabs;
  btnApply.Enabled := False;
end;

procedure TConfig.txtSnapPathExit(Sender: TObject);
begin
  with txtSnapPath do begin
    Text := FormatDir(Text);
    if Text = '' then
      Text := PrevSnapPath;
  end;
end;

procedure TConfig.btnSnapBrowseClick(Sender: TObject);
var
  Path: String;
  PIDL: PItemIDList;
begin
  txtSnapPathExit(Self);
  Path := txtSnapPath.Text;
  PIDL := BrowseForFolder(Handle, 'Please select a new system snapshot folder:',
      Path);
  if PIDL <> nil then begin
    CoTaskMemFree(PIDL);
    if Path <> '' then
      txtSnapPath.Text := RemoveBackSlashUnlessRoot(Path);
  end;
end;

procedure TConfig.txtSnapPathEnter(Sender: TObject);
begin
  PostMessage(Handle, WM_APP, Integer(Sender), 0);
end;

procedure TConfig.WMApp(var Message: TMessage);
begin
  if Message.wParam <> 0 then begin
    with TObject(Message.wParam) as TEdit do
      SelectAll;
  end;
end;

procedure TConfig.FormShow(Sender: TObject);
begin
  Application.Title := Caption;
  Application.Icon := Self.Icon;
end;

procedure TConfig.btnManageServicesClick(Sender: TObject);
var
  Command, Params: String;
begin
  Command := IIf(Win32MajorVersion < 5, 'rundll32.exe', 'services.msc');
  Params := IIf(Win32MajorVersion < 5,
      'shell32.dll,Control_RunDLL srvmgr.cpl Services', '');
  if not ExecuteFile(Command, Params, '', SW_SHOWNORMAL) then begin
    MsgBox('SmartClose failed to start the ' +
        IIf(Win32MajorVersion < 5, ServMgmNT, ServMgm2k2) + '.',
        bgOK, miError, '', 0);
  end;
end;

procedure TConfig.btnEditServicesClick(Sender: TObject);
var
  ServFile: String;
begin
  ServFile := GetServicesFilePath;
  if ServFile <> '' then
    ExecuteFile('notepad.exe', AddQuotes(ServFile), '', SW_SHOWNORMAL);
end;

end.
