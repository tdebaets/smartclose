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
 * Restore Wizard form
 *
 ****************************************************************************)

unit RestoreWiz;

interface

uses Windows, Messages, SysUtils, MBCSUtil, Classes, Graphics, Controls, Forms,
    NewDialogs, ExtCtrls, StdCtrls, MyRegistry, CaptionBevel, ComCtrls, ShellApi,
    CommCtrl, Menus, FileCtrl, ActiveX, ShlObj, ExtProgressBar, ThemeMgr,
    AgOpenDialog, ExtChkListView, IconView, GifImage, WinSvc, Dialogs,
    EnhListView, ExtListView, ComObj, ImgList;

type
  TfrmRestoreWiz = class(TForm)
    BackButton: TButton;
    NextButton: TButton;
    CancelButton: TButton;
    Notebook1: TNotebook;
    paWelcome: TPanel;
    WelcomeLabel1: TLabel;
    WelcomeLabel2: TLabel;
    WelcomeLabel3: TLabel;
    WelcomeLabel5: TLabel;
    chkNoIntro: TCheckBox;
    Bevel1: TBevel;
    PnlMain: TPanel;
    lblCaption: TLabel;
    lblDescription: TLabel;
    Notebook2: TNotebook;
    paFinished: TPanel;
    FinishedLabel: TLabel;
    Label9: TLabel;
    lblDetails: TLabel;
    InfoBeforeClickLabel: TLabel;
    imgSnap: TImage;
    optDefault: TRadioButton;
    optName: TRadioButton;
    lblName: TLabel;
    optFilename: TRadioButton;
    Label7: TLabel;
    txtFilename: TEdit;
    btnBrowse: TButton;
    cmbName: TComboBox;
    optLastSaved: TRadioButton;
    lblDate: TLabel;
    OpenDialog: TAgOpenDialog;
    Bevel: TBevel;
    imgSettings: TImage;
    Label1: TLabel;
    ScreenSaveBevel: TCaptionBevel;
    chkScreenSave: TCheckBox;
    IEBevel: TCaptionBevel;
    chkWindows: TCheckBox;
    lblWindows: TLabel;
    btnWindows: TButton;
    Notebook3: TNotebook;
    ServBevel: TCaptionBevel;
    chkServices: TCheckBox;
    lblServices: TLabel;
    btnServices: TButton;
    TaskBevel: TCaptionBevel;
    chkTaskSched: TCheckBox;
    imgProgs: TImage;
    Label5: TLabel;
    btnUncheckActive: TButton;
    btnCheckAll: TButton;
    btnCheckNone: TButton;
    imgsProgs: TImageList;
    lvProgs: TExtChkListView;
    mnuProgs: TPopupMenu;
    mnuDetails: TMenuItem;
    ReadyLabel: TLabel;
    txtSummary: TMemo;
    ProgressNotebook: TNotebook;
    StatusLabel: TLabel;
    lblTask: TLabel;
    lblArrow: TLabel;
    lstProgs: TListView;
    lblCurrent: TLabel;
    CurrentProgress: TdfsExtProgressBar;
    Label8: TLabel;
    TotalProgress: TdfsExtProgressBar;
    lblCount: TLabel;
    imgsStates: TImageList;
    lblDebug: TLabel;
    ThemeManager1: TThemeManager;
    icoTip: TIconView;
    imgOverlay: TImage;
    icoScreenSave: TIconView;
    icoWins: TIconView;
    icoServ: TIconView;
    icoSched: TIconView;
    icoProcess: TIconView;
    imgSmall: TImage;
    imgPicture: TImage;
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure CancelButtonClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure NextButtonClick(Sender: TObject);
    procedure BackButtonClick(Sender: TObject);
    procedure optLocationClick(Sender: TObject);
    procedure cmbNameChange(Sender: TObject);
    procedure btnBrowseClick(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    {procedure lvProgsDrawItem(Control: TWinControl; var ACanvas: TCanvas;
        Index: Integer; ARect: TRect; State: TOwnerDrawState;
        var DefaultDrawing, FullRowSelect: Boolean);}
    procedure mnuDetailsClick(Sender: TObject);
    procedure mnuProgsPopup(Sender: TObject);
    procedure lvProgsDblClick(Sender: TObject);
    procedure btnCheckAllClick(Sender: TObject);
    procedure btnCheckNoneClick(Sender: TObject);
    procedure btnUncheckActiveClick(Sender: TObject);
    procedure btnWindowsClick(Sender: TObject);
    procedure btnServicesClick(Sender: TObject);
    procedure lblDetailsClick(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure FormShow(Sender: TObject);
    //function EnumProgs(ProcObj: TResProcObj; var Dummy: Integer): Boolean;
  private
    procedure LoadSettings(Step: Integer);
    procedure LoadSummary;
    procedure LoadProgress;
    procedure Step(Num: Integer);
    function StartProcess: Boolean;
    procedure SaveSettings(Step: Integer);
    function CheckData(Step: Integer): Boolean;
    procedure NextStep(Step: Integer; Back: Boolean);
    procedure UpdateSnapFile;
    function GetSnapFile: String;
    procedure CheckOrUncheckAllProgs(Check: Boolean);
    function GetProgsCount: Integer;
    function GetWindowsCount: Integer;
    procedure SetExpWinCaption(Cnt: Integer);
    function TestWinItem(Index: Integer): Boolean;
    function GetServicesCount: Integer;
    function TestSvcItem(Index: Integer): Boolean;
    procedure SetServCaption(Cnt: Integer);
    procedure FreeProcObjs;
  public
    procedure ThdClosed(Sender: TObject);
    procedure ThdLoaded(Sender: TObject);
    procedure ParseParams;
  end;

var
  frmRestoreWiz: TfrmRestoreWiz;

implementation

uses Main, Process, ExpWin, frmResProg, frmIEWindows, frmServices, Service,
    frmLog, Restore, Func, Wait, Snapshot;

type
  TasksType = (taServices, taPrograms, taWindows, taScreensaver);

const
  TotalSteps = 7;
  WelcomeText = 'These saved system snapshots contain, for example, ' +
      'information about the programs and the screen saver that were active ' +
      'right before the Close Programs Wizard closed all these programs and ' +
      'disabled the screen saver. This wizard restores all this information ' +
      'from a selected system snapshot.';
  Captions: array[1..TotalSteps - 2] of String = (
    'Select System Snapshot Location',
    'System Snapshot Programs',
    'Set System Snapshot Options',
    'Ready to Restore System Snapshot',
    'SmartClose Process'
  );
  Descriptions: array[1..TotalSteps - 2] of String = (
    'From which location do you wish to restore a system snapshot?',
    'Which programs should be restored from the selected system snapshot?',
    'Set what SmartClose has to restore from the system snapshot',
    'SmartClose is now ready to begin restoring the system snapshot',
    'Please wait while SmartClose completes the following actions.'
  );
  DateCaptions: array[CorruptType] of String = (
    'System snapshot %s was saved on %s',
    'Specified system snapshot does not exist',
    'The file "%s" is not a system snapshot',
    'The system snapshot %s is empty or probably corrupt'
  );
  WinTypeDescs: array[TExpWindowType] of String = (
    'Explorer window',
    'Explorer window (with Folder tree)',
    'Internet Explorer window'
  );
  TaskDescs: array[0..3] of string = (
    '',
    'Launching Programs',
    'Restoring Explorer Windows',
    'Restoring Screen Saver'
  );

var
  CurStep: Integer;
  PageLoaded: array[1..TotalSteps] of Boolean;
  CurLocation: Integer;
  UseClick: Boolean;
  LastWizKey: String;
  Cancel: Boolean;
  Corrupt: CorruptType = crNormalSnap;
  LastSnapFile: String;
  Snap: TSnapshot;
  TaskLabels: TList;
  WaitCount: Integer;
  Settings: TRestoreSettings;

{$R *.DFM}

function EnumProgs(ProcObj: TResProcObj; var ProgData: Integer): Boolean;
var
  SFI: TSHFileInfo;
  Filename, FilePart: PChar;
  RetVal: Integer;
begin
  with TChkListItem(frmRestoreWiz.lvProgs.Items.Add) do begin
    ProcObj.Title := GetProcessTitle(ProcObj.LocalExe);
    Data := ProcObj;
    Caption := ProcObj.Title;
    SubItems.Add(ProcObj.CurDir);
    SubItems.Add(ProcObj.Params);
    if ExtractFileExt(ProcObj.LocalExe) = '' then
      ProcObj.LocalExe := ProcObj.LocalExe + '.exe';
    Filename := StrAlloc(MAX_PATH);
    try
      RetVal := SearchPath(nil, PChar(ProcObj.LocalExe), nil, MAX_PATH,
          Filename, FilePart);
      if RetVal > 0 then
        ProcObj.LocalExe := Filename
      else
        ProcObj.LocalExe := GetProgPathRegistry(ExtractFilename(ProcObj.LocalExe));
    finally
      StrDispose(Filename);
      Filepart := nil;
    end;
    if not FileExists(ProcObj.ExeFile) and FileExists(ProcObj.LocalExe) then
      ProcObj.ExeFile := ProcObj.LocalExe;
    SHGetFileInfo(PChar(ProcObj.ExeFile), 0, SFI, SizeOf(SFI),
        SHGFI_ICON or SHGFI_SMALLICON);
    try
      //Checked := (SFI.HIcon <> 0);
      if SFI.HIcon <> 0 then
        ImageIndex := ImageList_AddIcon(frmRestoreWiz.imgsProgs.Handle, SFI.HIcon)
      else
        ImageIndex := 0;
    finally
      DestroyIcon(SFI.hIcon);
    end;
    if ImageIndex < 0 then
      ImageIndex := 0;
    Enabled := (ImageIndex <> 0);
    Checked := Enabled;
  end;
  Result := True;
end;

function EnumExpWins(Path: TExpWindowPath; var Cnt: Integer): Boolean;
var
  PIDL: PItemIDList;
  Location: PChar;
  Flags: Integer;
  SFI: TSHFileInfo;
  ExpWin: ^TExpWindowPath;
begin
  Result := True;
  with TChkListItem(IEWindows.lvWindows.Items.Add) do begin
    SubItems.Add(WinTypeDescs[Path.WindowType]);
    Caption := Path.Location;
    New(ExpWin);
    ExpWin^ := Path;
    Data := ExpWin;
    ExpWin.PIDL := nil;
    if (Path.WindowType = wtIE) or TestPath(Path.Location, True) then begin
      Checked := True;
      Inc(Cnt);
      if Copy(Path.Location, 1, 2) = '::' then begin
        PIDL := PathToPIDL(Path.Location);
        Caption := PIDLToDisplayName(PIDL);
        if Trim(Caption) = '' then begin
          Delete;
          Dispose(ExpWin);
          Exit;
        end;
        ExpWin.PIDL := PIDL;
      end;
    end
    else
      Checked := False;
    Enabled := Checked;
    if Path.WindowType = wtIE then
      ImageIndex := 0
    else begin
      Flags := SHGFI_ICON or SHGFI_SMALLICON;
      if Checked then begin
        if Assigned(ExpWin.PIDL) then begin
          Flags := Flags or SHGFI_PIDL;
          Location := Pointer(ExpWin.PIDL);
        end
        else
          Location := PChar(Path.Location);
      end
      else begin
        Flags := Flags or SHGFI_USEFILEATTRIBUTES;
        Location := '.';
      end;
      FillChar(SFI, SizeOf(TSHFileInfo), #0);
      SHGetFileInfo(Location, 0, SFI, SizeOf(SFI), Flags);
      try
        if SFI.hIcon <> 0 then
          ImageIndex := ImageList_AddIcon(IEWindows.imgImages.Handle, SFI.HIcon)
      finally
        DestroyIcon(SFI.hIcon);
      end;
    end;
  end;
end;

function EnumServices(ServName: String; var Cnt: Integer): Boolean;
var
  Name: String;
  RetVal: Integer;
begin
  RetVal := GetServiceDisplayName(ServName, Name);
  if Trim(Name) = '' then
    Name := ServName;
  with TChkListItem(Services.lvServices.Items.Add) do begin
    Caption := Name;
    if RetVal = 1 then
      RetVal := SERVICE_STATUS_DISABLED
    else if RetVal = 2 then
      GetServiceStatus(ServName, RetVal);
    case RetVal of
      0                       : SubItems.Add('Not installed or error');
      SERVICE_STATUS_DISABLED : SubItems.Add('Disabled');
      SERVICE_STOPPED         : SubItems.Add('Stopped');
      SERVICE_START_PENDING   : SubItems.Add('Starting');
      SERVICE_STOP_PENDING    : SubItems.Add('Stopping');
      SERVICE_RUNNING,
      SERVICE_CONTINUE_PENDING,
      SERVICE_PAUSE_PENDING,
      SERVICE_PAUSED          : SubItems.Add('Running');
    end;
    Data := Pointer(RetVal);
    SubItems.Add(ServName);
    if RetVal <> SERVICE_STOPPED then
      Checked := False
    else begin
      Checked := True;
      Inc(Cnt);
    end;
    Enabled := Checked;
  end;
  Result := True;
end;

procedure TfrmRestoreWiz.LoadSettings(Step: Integer);
var
  Registry: TMyRegistry;
  SearchRec: TSearchRec;
  RetVal: Integer;
  Ico: TIcon;
begin
  UseClick := False;
  Registry := TMyRegistry.Create;
  try
    Registry.RootKey := Hive;
    Registry.OpenKey(LastWizKey, True);
    case Step of
      2: begin
        RetVal := FindFirst(SnapPath + '\*' + SnapshotExt,
            faAnyFile - faDirectory, SearchRec);
        try
          while RetVal = 0 do begin
            cmbName.Items.Add(Copy(SearchRec.Name, 1,
                Length(SearchRec.Name) - Length(SnapshotExt)));
            RetVal := FindNext(SearchRec);
          end;
        finally
          FindClose(SearchRec);
        end;
        cmbName.Text := Registry.ReadStringDef('Name', '');
        if (cmbName.Text = '') and (cmbName.Items.Count > 0) then
          cmbName.ItemIndex := 0;
        txtFilename.Text := Registry.ReadStringDef('Filename', '');
        LastSnapFile := GetKeyValue(Hive, RegKey, 'LastSnapFile', '');
        if (LastSnapFile = '') or not FileExists(LastSnapFile) then
          optLastSaved.Enabled := False;
        if not FileExists(SnapPath + '\Default' + SnapshotExt) then
          optDefault.Enabled := False;
        if cmbName.Items.Count = 0 then begin
          optName.Enabled := False;
          lblName.Enabled := False;
          cmbName.Enabled := False;
        end;
        optFilename.Checked := True;
        case Registry.ReadIntegerDef('Location', 0) of
          0: if optLastSaved.Enabled then
            optLastSaved.Checked := True;
          1: if optDefault.Enabled then
            optDefault.Checked := True;
          2: if optName.Enabled then
            optName.Checked := True;
        end;
        UpdateSnapFile;
      end;
      3: begin
        RetVal := 0;
        ShowWait(False, 'Reading programs in system snapshot...');
        try
          lvProgs.BeginUpdate;
          try
            lvProgs.Items.Clear;
            imgsProgs.Clear;
            Ico := TIcon.Create;
            try
              Ico.ReleaseHandle;
              Ico.Handle := LoadIcon(hInstance, PChar(6));
              imgsProgs.AddIcon(Ico);
            finally
              Ico.Free;
            end;
            EnumSnapProgs(@EnumProgs, RetVal);
          finally
            lvProgs.EndUpdate;
          end;
        finally
          HideWait;
        end;
        if Settings.UncheckProgs then
          btnUncheckActiveClick(Self); 
      end;
      4: begin
        // screen saver
        if GetIsScreenSaveActive then begin
          ScreenSaveBevel.Caption := '(Screen saver is already enabled)';
          chkScreenSave.Checked := False;
          chkScreenSave.Enabled := False;
        end
        else if not Settings.AllowScreenSaver
            and not Snap.HasScreenSaver then begin
          ScreenSaveBevel.Caption := '(No screen saver info in selected snapshot)';
          chkScreenSave.Checked := False;
          chkScreenSave.Enabled := False;
        end
        else begin
          ScreenSaveBevel.Caption := 'Screen Saver';
          chkScreenSave.Checked := True;
          chkScreenSave.Enabled := True;
        end;
        // explorer windows
        if not Snap.HasExpWins then begin
          IEBevel.Caption :=
              '(No Explorer or Internet Explorer windows in selected snapshot)';
          chkWindows.Checked := False;
          chkWindows.Enabled := False;
          SetExpWinCaption(-1);
          lblWindows.Enabled := False;
          btnWindows.Enabled := False;
        end
        else begin
          RetVal := 0;
          IEBevel.Caption := 'Explorer and Internet Explorer windows';
          chkWindows.Checked := True;
          chkWindows.Enabled := True;
          lblWindows.Enabled := True;
          btnWindows.Enabled := True;
          with IEWindows do begin
            lvWindows.Items.BeginUpdate;
            try
              ClearWinList;
              EnumSnapExpWins(@EnumExpWins, RetVal);
            finally
              lvWindows.Items.EndUpdate;
            end;
          end;
          SetExpWinCaption(RetVal);
        end;
        //
        if IsWinNT then begin // services
          Notebook3.PageIndex := 1;
          if not Snap.HasServices then begin
            ServBevel.Caption := '(No services in selected snapshot)';
            chkServices.Checked := False;
            chkServices.Enabled := False;
            SetServCaption(-1);
            lblServices.Enabled := False;
            btnServices.Enabled := False;
          end
          else begin
            RetVal := 0;
            ServBevel.Caption := 'Services';
            chkServices.Checked := True;
            chkServices.Enabled := True;
            lblServices.Enabled := True;
            btnServices.Enabled := True;
            with Services do begin
              lvServices.BeginUpdate;
              try
                lvServices.Items.Clear;
                EnumSnapServices(@EnumServices, RetVal);
              finally
                lvServices.EndUpdate;
              end;
            end;
            SetServCaption(RetVal);
          end;
        end
        else begin // task scheduler
          Notebook3.PageIndex := 0;
          if not SysAgent_IsAvailable then begin
            TaskBevel.Caption := '(Task Scheduler is not available)';
            chkTaskSched.Checked := False;
            chkTaskSched.Enabled := False;
          end
          else if SysAgent_GetStat = SYSAGENT_ENABLED then begin
            TaskBevel.Caption := '(Task Scheduler is already enabled)';
            chkTaskSched.Checked := False;
            chkTaskSched.Enabled := False;
          end
          else if not Settings.AllowScreenSaver and not Snap.HasTaskSched then begin
            TaskBevel.Caption := '(No Task Scheduler info in selected snapshot)';
            chkTaskSched.Checked := False;
            chkTaskSched.Enabled := False;
          end
          else begin
            TaskBevel.Caption := 'Task Scheduler';
            chkTaskSched.Checked := True;
            chkTaskSched.Enabled := True;
          end;
        end;
      end;
    end;
    PageLoaded[Step] := True;
  finally
    Registry.Free;
    UseClick := True;
  end;
end;

function TfrmRestoreWiz.GetProgsCount: Integer;
var
  i: Integer;
begin
  Result := 0;
  for i := 0 to lvProgs.Items.Count - 1 do begin
    if lvProgs.Items.Item[i].Checked then
      Inc(Result);
  end;
end;

function TfrmRestoreWiz.GetWindowsCount: Integer;
var
  i: Integer;
begin
  Result := 0;
  for i := 0 to IEWindows.lvWindows.Items.Count - 1 do begin
    if TestWinItem(i) then
      Inc(Result);
  end;
end;

function TfrmRestoreWiz.TestWinItem(Index: Integer): Boolean;
begin
  Result := IEWindows.lvWindows.Items.Item[Index].Checked
      and TChkListItem(IEWindows.lvWindows.Items[Index]).Enabled;
end;

function TfrmRestoreWiz.GetServicesCount: Integer;
var
  i: Integer;
begin
  Result := 0;
  for i := 0 to Services.lvServices.Items.Count - 1 do begin
    if TestSvcItem(i) then
      Inc(Result);
  end;
end;

function TfrmRestoreWiz.TestSvcItem(Index: Integer): Boolean;
begin
  Result := Services.lvServices.Items[Index].Checked
      and TChkListItem(Services.lvServices.Items[Index]).Enabled;
end;

procedure TfrmRestoreWiz.LoadSummary;
var
  Space: String;
  i, Cnt: Integer;
begin
  with txtSummary do begin
    Space := Format('%6s', ['']);
    Lines.BeginUpdate();
    Lines.Clear;
    if Snap.HasProgs then begin
      Cnt := 0;
      Lines.Append('Programs to launch:');
      for i := 0 to lvProgs.Items.Count - 1 do begin
        if lvProgs.Items.Item[i].Checked then begin
          Lines.Append(Space + ' ' + lvProgs.Items.Item[i].Caption);
          Inc(Cnt);
        end;
      end;
      if Cnt = 0 then
        Lines.Append(Space + '(none)');
      Lines.Append( '');
    end;
    if Snap.HasScreenSaver and chkScreenSave.Enabled then begin
      Lines.Append('Restore screen saver: ' +
          IIf(chkScreenSave.Checked, 'yes', 'no'));
      Lines.Append('');
    end;
    if Snap.HasExpWins and chkWindows.Enabled and (IEWindows <> nil) then begin
      //Cnt := 0;
      //Lines.Append('Explorer and Internet Explorer windows:');
      //Lines.Append(Space + 'Restore Explorer and Internet Explorer windows: ' +
      //    IIf(chkWindows.Checked, 'yes', 'no'));
      if chkWindows.Checked then begin
        Lines.Append('Explorer and Internet Explorer windows to restore:');
        for i := 0 to IEWindows.lvWindows.Items.Count - 1 do begin
          if TestWinItem(i) then begin
            Lines.Append(Space + ' ' + IEWindows.lvWindows.Items.Item[i].Caption);
            //Inc(Cnt);
          end;
        end;
        //if Cnt = 0 then
        //  Lines.Append(Space + Space + '(none)');
        Lines.Append('');
      end;
    end;
    if IsWinNT then begin
      if Snap.HasServices and chkServices.Enabled and (Services <> nil) then begin
        //Cnt := 0;
        //Lines.Append('Services');
        //Lines.Append(Space + 'Restart services: ' +
        //    IIf(chkServices.Checked, 'yes', 'no'));
        if chkServices.Checked then begin
          Lines.Append('Services to restart:');
          for i := 0 to Services.lvServices.Items.Count - 1 do begin
            if TestSvcItem(i) then begin
              Lines.Append(Space + ' ' + Services.lvServices.Items[i].Caption);
              //Inc(Cnt);
            end;
          end;
          //if Cnt = 0 then
          // Lines.Append(Space + Space + '(none)');
          Lines.Append('');
        end;
      end;
    end
    else begin // IsWinNT
      if Snap.HasTaskSched and chkTaskSched.Enabled then begin
        //Lines.Append('Task Scheduler:');
        Lines.Append('Re-enable Task Scheduler: ' +
            IIf(chkTaskSched.Checked, 'yes', 'no'));
        Lines.Append('');
      end;
    end;
  end;
end;

procedure TfrmRestoreWiz.LoadProgress;
var
  TaskLabel: TLabel;
  i, Y: Integer;
  Caption: String;
  Tag: Longint;
begin
  ProgressNotebook.PageIndex := 0;
  Y := lblTask.Top;
  for i := 0 to High(TaskDescs) do begin
    Caption := '';
    case TasksType(i) of
      taServices: begin
        if IsWinNT then begin
          if Snap.HasServices and chkServices.Checked
              and (GetServicesCount > 0) then
            Caption := 'Restarting Services';
        end
        else begin
          if Snap.HasTaskSched and chkTaskSched.Checked then
            Caption := 'Re-enabling Task Scheduler';
        end;
      end;
      taPrograms:
        if Snap.HasProgs and (GetProgsCount > 0) then
          Caption := TaskDescs[i];
      taScreensaver:
        if Snap.HasProgs and chkScreenSave.Checked then
          Caption := TaskDescs[i];
      taWindows:
        if Snap.HasExpWins and chkWindows.Checked and (GetWindowsCount > 0) then
          Caption := TaskDescs[i];
    end;
    if Caption <> '' then begin
      Tag := i;
      TaskLabel := CreateLabel(lblTask, Caption, Tag, Y);
      TaskLabels.Add(TaskLabel);
    end;
  end;
  if Mode = wmDebug then
    lblDebug.Visible := True;
end;

procedure TfrmRestoreWiz.Step(Num: Integer);
var
  i: Integer;
begin
  for i := 1 to Num do begin
    CurrentProgress.StepIt;
    TotalProgress.StepIt;
  end;
  Application.ProcessMessages;
end;

procedure TfrmRestoreWiz.ThdClosed(Sender: TObject);
var
  Item: TListItem;
begin
  if ProgressNotebook.PageIndex <> 1 then
    Exit;
  with Sender as TProcWaitThread do begin
    Item := lstProgs.Items.Item[Index];
    Item.MakeVisible(False);
    Item.SubItems[0] := 'Closed' {+ ' (' + IntToStr(ExitCode) + ')'};
  end;
end;

procedure TfrmRestoreWiz.ThdLoaded(Sender: TObject);
var
  Item: TListItem;
begin
  Dec(WaitCount);
  if ProgressNotebook.PageIndex <> 1 then
    Exit;
  Application.BringToFront;
  with Sender as TProcWaitThread do begin
    Item := lstProgs.Items.Item[Index];
    Item.MakeVisible(False);
    Item.SubItems[0] := 'Started';
  end;
end;

//procedure RestoreIEWindow(const URL: String; FullScreen: Boolean;
//    hMonitor: Integer);
//var
//  IEObj: InternetExplorer;
//  NullVariant: OleVariant;
//  WinPlacement: TWindowPlacement;
//  MonInfo: TMonitorInfo;
//begin
//  IEObj := CreateComObject(Class_InternetExplorer)
//      as InternetExplorer;
//  if Assigned(IEObj) then begin
//    if hMonitor <> 0 then begin
//      FillChar(MonInfo, SizeOf(MonInfo), 0);
//      MonInfo.cbSize := SizeOf(MonInfo);
//      if GetMonitorInfo(hMonitor, @MonInfo) then begin
//        SetWindowPos(IEObj.Get_HWND, 0,
//            MonInfo.rcWork.Left + 1,
//            MonInfo.rcWork.Top + 1,
//            0, 0,
//            SWP_NOSIZE or SWP_NOZORDER or SWP_FRAMECHANGED);
//      end;
//    end;
//    if FullScreen then begin
//      {FillChar(WinPlacement, SizeOf(WinPlacement), 0);
//      WinPlacement.length := SizeOf(WinPlacement);
//      WinPlacement.showCmd := SW_MAXIMIZE;
//      SetWindowPlacement(IEObj.Get_HWND, @WinPlacement);}
//      ShowWindow(IEObj.Get_HWND, SW_MAXIMIZE);
//      IEObj.FullScreen := True;
//    end;
//    IEObj.Navigate(URL, NullVariant, NullVariant, NullVariant, NullVariant);
//    IEObj.Set_Visible(True);
//    IEObj := nil;
//  end;
//end;

function TfrmRestoreWiz.StartProcess: Boolean;
var
  TotalSteps: Longint;
  Steps: array[0..High(TaskDescs)] of Longint;
  i, j: Integer;
  RetVal: Integer;
  Tmp: String;
  Item: TListItem;
  ProcObj: TResProcObj;
  Command, Params: String;
  hProc: THandle;
  ExpWinPath: PExpWindowPath;
  ShowCmd: Integer;
  //Monitors: TList;
  //hMonitor: Integer;
begin
  Cancel := False;
  Result := False;
  //Monitors := nil;
  TotalSteps := 0;
  for i := 0 to High(Steps) do
    Steps[i] := 0;

  LogFile := '';

  lblCurrent.Caption := 'Saving Wizard settings and collecting info...';
  Application.ProcessMessages;
  if Cancel then
    Exit;
  for i := 1 to Notebook2.Pages.Count do begin
    if PageLoaded[i] then begin
      SaveSettings(i);
      Log('Saving settings of wizard page ' + IntToStr(i));
    end;
  end;
  LogLine;

  //try

  Log('Calculating progress bar values');
  for i := 0 to TaskLabels.Count - 1 do begin
    case TasksType(TLabel(TaskLabels[i]).Tag) of
      taServices: begin            // task scheduler or services
        if IsWinNT then begin
          Inc(Steps[i], GetServicesCount);
          Log('Number of services to start: ' + IntToStr(Steps[i]));
        end
        else
          Inc(Steps[i], 2);
      end;
      taPrograms: begin
        Inc(Steps[i], GetProgsCount);
        Log('Number of programs to launch: ' + IntToStr(Steps[i]));
      end;
      taWindows: begin
        Inc(Steps[i], GetWindowsCount);
        Log('Number of Explorer windows to restore: ' + IntToStr(Steps[i]));
      end;
      taScreensaver:
        Inc(Steps[i], 1);
    end;
  end;
  Application.ProcessMessages;
  if Cancel then
    Exit;

  for i := 0 to High(Steps) do begin
    if Steps[i] > 0 then
      Inc(TotalSteps, Steps[i]);
  end;
  Log('Total number of steps to do: ' + IntToStr(TotalSteps));
  CurrentProgress.Position := 100;
  TotalProgress.Max := TotalSteps;
  Application.ProcessMessages;
  if Cancel then
    Exit;
  Sleep(500);
  LogLine;

  for i := 0 to TaskLabels.Count - 1 do begin
    lblArrow.Visible := True;
    lblArrow.Top := TLabel(TaskLabels[i]).Top - 1;
    TLabel(TaskLabels[i]).Font.Style := [fsBold];
    CurrentProgress.Max := Steps[i];
    CurrentProgress.Position := 0;
    Application.ProcessMessages;
    if Cancel then
      Exit;

    case TasksType(TLabel(TaskLabels[i]).Tag) of
      taServices: begin                       // task scheduler or services
        if IsWinNT then begin
          lblCurrent.Caption := 'Starting Services...';
          Application.ProcessMessages;
          if Cancel then
            Exit;
          for j := 0 to Services.lvServices.Items.Count - 1 do begin
            if not TestSvcItem(j) then
              Continue;
            if Mode <> wmDebug then begin
              Log('Starting ' + Services.lvServices.Items[j].Caption +
                  ' (' + Services.lvServices.Items[j].SubItems[1] + ')');
              RetVal := RestartService(Services.lvServices.Items[j].SubItems[1]);
              case RetVal of
                0: Tmp := '(successful; the service was started by SmartClose)';
                1: Tmp := '(there was no need to start the service as it was ' +
                    'already running or the service doesn''t exist)';
                2: Tmp := '(unsuccessful; the OpenSCManager-function failed)';
                3: Tmp := '(unsuccessful; the OpenService-function failed)';
                4: Tmp := '(unsuccessful; the StartService-function failed)';
                5: Tmp := '(unsuccessful; the starting of the service timed out)';
              end;
              Log('     Starting of service ' +
                  Services.lvServices.Items[j].Caption + ' result: ' +
                  IntToStr(RetVal) + ' ' + Tmp);
            end;
            Step(1);
            if Cancel then
              Exit;
          end;
        end
        else begin
          lblCurrent.Caption := 'Enabling the Task Scheduler...';
          Application.ProcessMessages;
          if Cancel then
            Exit;
          RetVal := SysAgent_GetStat;
          case RetVal of
            0: Tmp := '(Not running or error)';
            SYSAGENT_ENABLED: Tmp := '(Enabled)';
            SYSAGENT_DISABLED: Tmp := '(Disabled)';
            else Tmp := '(Unknown)';
          end;
          Log('Task Scheduler status: ' + IntToStr(RetVal) + ' ' + Tmp);
          if (Mode <> wmDebug) and (RetVal  = SYSAGENT_DISABLED) then begin
            RetVal := SysAgent_SetStatus(True);
            if RetVal = 0 then
              Tmp := '(Success)'
            else
              Tmp := '(Not running or error)';
            Log('Return value of ''enable Task Scheduler''-message: ' +
                IntToStr(RetVal) + ' ' + Tmp);
          end;
          Sleep(500);
          Step(2);
          if Cancel then
            Exit;
        end;
      end;
      taPrograms: begin
        lblCurrent.Caption := 'Launching programs...';
        Application.ProcessMessages;
        if Cancel then
          Exit;
        WaitCount := 0;
        lstProgs.Items.BeginUpdate;
        try
          for j := 0 to lvProgs.Items.Count - 1 do begin
            if lvProgs.Items.Item[j].Checked then
              with lstProgs.Items.Add do begin
                Caption := lvProgs.Items.Item[j].Caption;
                //SubItems.Add(lvProgs.Items.Item[j].Caption);
                SubItems.Add('');
                Data := lvProgs.Items.Item[j].Data;
                ImageIndex := -1;
              end;
            end;
        finally
          lstProgs.Items.EndUpdate;
          lstProgs.Columns[0].Width := lstProgs.Columns[0].Width + 30;
        end;
        if Settings.ShowProgs then begin
          Sleep(500);
          ProgressNotebook.PageIndex := 1;
          ActiveControl := lstProgs;
        end;
        Application.ProcessMessages;
        if Cancel then
          Exit;
        for j := 0 to lstProgs.Items.Count - 1 do begin
          Item := lstProgs.Items[j];
          EnsureVisible(Item);
          lstProgs.Selected := Item;
          Application.ProcessMessages;
          if Cancel then
            Exit;
          if Item.Data = nil then
            Continue;
          lblCount.Caption := '';
          //Item.StateIndex := 0;
          Item.ImageIndex := 0;
          Application.ProcessMessages;
          if Cancel then
            Exit;
          ProcObj := Item.Data;
          //lblCurrent.Caption := 'Launching ' + Item.SubItems[0] + '...';
          Item.SubItems[0] := 'Starting...';
          Application.ProcessMessages;
          //RetVal := -1;
          Log('Launching program ' + ProcObj.Title + ':');
          if Mode <> wmDebug then begin
            RetVal := ExecuteProgram(ProcObj.ExeFile, ProcObj.Params,
                ProcObj.CurDir, SW_SHOWMINNOACTIVE, ProcObj.hProc);
            //Application.BringToFront;
            if RetVal = ERROR_NO_ASSOCIATION then begin
              Log( '     No program associated with this file; launching it ' +
                  'with another method...');
              {if not IsFileExecutable(ProcObj.ExeFile) then begin
                Item.SubItems[0] := 'Error starting';
                Log( '     Unable to launch this file; the file is not executable');
              end
              else}
                RetVal := ExecuteProcess(ProcObj.ExeFile, ProcObj.Params,
                    ProcObj.CurDir, SW_SHOWMINNOACTIVE, ProcObj.hProc);
            end;
            Application.BringToFront;
            if RetVal <> -1 then begin
              Item.SubItems[0] := 'Error starting';
              Log( '     Starting unsuccessful; error code: ' + IntToStr(RetVal));
              Log( '     Error message: ' +
                  Format(GetShellExecErrorString(RetVal), [ProcObj.ExeFile]));
            end
            else
              Log( '     Starting successful');
          end;
          Application.ProcessMessages;
          if Cancel then
            Exit;
          if (Mode <> wmDebug) and (ProcObj.hProc <> 0) then begin
            TProcWaitThread.Create(False, ProcObj.hProc, Item.Index, 20,
                INFINITE, ThdLoaded, ThdClosed);
            Inc(WaitCount);
          end;
          //Item.StateIndex := -1;
          Item.ImageIndex := -1;
          Step(1);
          if Cancel then
            Exit;
        end;
        lstProgs.Selected := nil;
        if (Mode <> wmDebug) and Settings.ProgsWait and (WaitCount > 0) then begin
          lblCurrent.Caption := 'Waiting for programs to finish loading...';
          Application.ProcessMessages;
          while WaitCount > 0 do
            Application.ProcessMessages;
        end;
        Sleep(500);
        if Settings.ShowProgs then begin
          Sleep(500);
          ProgressNotebook.PageIndex := 0;
        end;
        Application.ProcessMessages;
        if Cancel then
          Exit;
        PulseEvent(hStopEvent);
      end;
      taWindows: begin
        lblCurrent.Caption := 'Restoring Explorer and Internet Explorer windows...';
        Application.ProcessMessages;
        if Cancel then
          Exit;
        // TODO: uncomment multimon code?
        {if not Assigned(Monitors) then
          Monitors := TList.Create;
        GetDisplayMonitors(Monitors);}
        for j := 0 to IEWindows.lvWindows.Items.Count - 1 do begin
          if not TestWinItem(j) then
            Continue;
          Item := IEWindows.lvWindows.Items.Item[j];
          Tmp := 'Restoring ' + Item.SubItems[0] + ' with location ' + Item.Caption;
          ExpWinPath := PExpWindowPath(Item.Data);
          //hMonitor := 0;
          if Assigned(ExpWinPath.PIDL) then begin
            Params := PIDLToPath(ExpWinPath.PIDL);
            Tmp := Tmp + ' (' + Params + ')';
          end
          else
            Params := Item.Caption;
          Log(Tmp);
          {if ExpWinPath.MonitorIdx < Monitors.Count then
            hMonitor := Integer(Monitors[ExpWinPath.MonitorIdx]);}
          ShowCmd := SW_MINIMIZE;
          if Item.ImageIndex = 0 then begin // IE window
            Command := 'iexplore.exe';
            {if ExpWinPath.FullScreenMode then begin
              Params := '-k ' + Params;
              ShowCmd := SW_MAXIMIZE;
            end;
            RetVal := -1;
            if Mode <> Debug then
              RestoreIEWindow(Params, ExpWinPath.FullScreenMode, hMonitor);}
          end
          else begin
            Command := 'explorer.exe';
            if Item.SubItems[0] = WinTypeDescs[wtExplorerTree] then
              Params := '/e, ' + Params;
            Params := '/n, ' + Params;
          end;
          RetVal := -1;
          if Mode <> wmDebug then begin
            RetVal := ExecuteProgram(Command, Params, '', ShowCmd, hProc);
            if RetVal = -1 then
              CloseHandle(hProc);
          end;
          Application.BringToFront;
          if RetVal <> -1 then begin
            Log( '     Restoring unsuccessful; error code: ' + IntToStr(RetVal));
            Log( '     Error message: ' +
                Format(GetShellExecErrorString(RetVal), [Command]));
          end
          else
            Log( '     Restoring successful');
          Step(1);
          if Cancel then
            Exit;
        end;
      end;
      taScreenSaver: begin
        lblCurrent.Caption := 'Enabling the screen saver...';
        Application.ProcessMessages;
        if Cancel then
          Exit;
        Log('Enabling the screen saver...');
        if Mode <> wmDebug then
          SystemParametersInfo(SPI_SETSCREENSAVEACTIVE, 1, nil, 0);
        Sleep(500);
        Step(1);
        if Cancel then
          Exit;
      end;
    end;

    if i = TaskLabels.Count - 1 then
      CancelButton.Enabled := False;
    LogLine;
    Application.ProcessMessages;
    if Cancel then
      Exit;
    Sleep(500);
    lblArrow.Visible := False;
    TLabel(TaskLabels[i]).Font.Style := [];
    Application.ProcessMessages;
    if Cancel then
      Exit;
    Sleep(250);
  end;

  lblCurrent.Caption := 'Done';
  Application.ProcessMessages;
  Sleep(500);
  Result := True;
  NextStep(CurStep + 1, False);

  {finally
    if Assigned(Monitors) then
      FreeAndNil(Monitors);
  end;}
end;

procedure TfrmRestoreWiz.SaveSettings(Step: Integer);
var
  Registry: TMyRegistry;
begin
  Registry := TMyRegistry.Create;
  try
    Registry.RootKey := Hive;
    if Registry.OpenKey(LastWizKey, True) then begin
      case Step of
        1: SetKeyBool(Hive, RegKey, 'Res_NoIntro', chkNoIntro.Checked);
        2: begin
          Registry.WriteInteger('Location', CurLocation);
          if optName.Checked then
            Registry.WriteString('Name', cmbName.Text)
          else if optFilename.Checked then
            Registry.WriteString('Filename', txtFilename.Text);
        end;
      end;
    end;
  finally
    Registry.Free;
  end;
end;

function TfrmRestoreWiz.CheckData(Step: Integer): Boolean;
var
  i: Integer;
begin
  Result := True;
  case Step of
    2: begin
      Result := False;
      //cmbName.Text := Trim(cmbName.Text);
      //txtFilename.Text := Trim(txtFilename.Text);
      if optName.Checked and (cmbName.Text = '') then begin
        MsgBox('You have to specify the name of a system snapshot or select one ' +
            'of the available names from the drop-down list.',
            bgOK, miWarning, 'No name specified', 0);
        ActiveControl := cmbName;
        Exit;
      end;
      if optName.Checked
          and not FileExists(SnapPath + '\' + cmbName.Text + SnapshotExt) then begin
        MsgBox('The system snapshot with the name you have specified does not ' +
            'exist. Please check the name and try again or choose another location.',
            bgOK, miWarning, 'System Snapshot name does not exist', 0);
        ActiveControl := cmbName;
        Exit;
      end;
      if optFilename.Checked and (txtFilename.Text = '') then begin
        MsgBox('You have to specify the filename of a system snapshot by ' +
            'clicking the "Browse"-button.',
            bgOK, miWarning, 'No filename specified', 0);
        ActiveControl := btnBrowse;
        Exit;
      end;
      case Corrupt of
        crNoFile: begin
          MsgBox('The specified system snapshot does not exist. ' +
              'Please select another system snapshot file.',
              bgOK, miWarning, 'System Snapshot does not exist', 0);
          Exit;
        end;
        crNoSnapFile: begin
          MsgBox('The selected file is not a system snapshot. ' +
              'Please select a system snapshot file.',
              bgOK, miWarning, 'No System Snapshot File', 0);
          Exit;
        end;
        crCorrupt: begin
          MsgBox('The selected system snapshot is empty or corrupt. ' +
              'Please select another system snapshot file.',
              bgOK, miWarning, 'Empty or Corrupt System Snapshot', 0);
          Exit;
        end;
      end;
      Result := True;
    end;
    4: begin
      if Snap.HasProgs then begin
        for i := 0 to lvProgs.Items.Count - 1 do begin
          if lvProgs.Items.Item[i].Checked then
            Exit;
        end;
      end;
      if chkScreenSave.Enabled and chkScreenSave.Checked then
        Exit;
      if Snap.HasExpWins and (IEWindows <> nil) then begin
        for i := 0 to IEWindows.lvWindows.Items.Count - 1 do begin
          if TestWinItem(i) then
            Exit;
        end;
      end;
      if IsWinNT then begin
        if Snap.HasServices and (Services <> nil) then begin
          for i := 0 to Services.lvServices.Items.Count - 1 do begin
            if TestSvcItem(i) then
              Exit;
          end;
        end;
      end
      else begin
        if chkTaskSched.Enabled and chkTaskSched.Checked then
          Exit;
      end;
      Result := False;
      MsgBox('SmartClose can''t continue because there is nothing to do. ' +
          'You have to select at least one program to be started or at least ' +
          'one task on this page.', bgOK, miWarning, 'Nothing to do', 0);
    end; 
  end;
end;

procedure TfrmRestoreWiz.NextStep(Step: Integer; Back: Boolean);
var
  SkipPage: Integer;
  ShowOptions: Boolean;
begin
  ShowOptions := False;
  if Step > 1 then begin
    if not CheckData(Step - 1) then
      Exit;
  end;
  if Back then
    SkipPage := -1
  else
    SkipPage := 1;
  if Step = 4 then begin
    ShowOptions := Snap.HasExpWins or Snap.HasScreenSaver;
    if IsWinNT then
      ShowOptions := ShowOptions or Snap.HasServices
    else
      ShowOptions := ShowOptions or Snap.HasTaskSched;
  end;
  if ((Step = 3) and (not Snap.HasProgs))
      or ((Step = 4) and (not ShowOptions)) then begin
    NextStep(Step + SkipPage, Back);
    Exit;
  end;
  CurStep := Step;
  if (not PageLoaded[Step]) and (Back = False) then
    LoadSettings(Step);
  if Step = 5 then
    LoadSummary
  else if Step = 6 then
    LoadProgress;
  if Step = Notebook2.Pages.Count then
    NextButton.Caption := '&Start'
  else if Step = TotalSteps then begin
    NextButton.Caption := '&Finish';
    imgPicture.Parent := paFinished;
  end
  else
    NextButton.Caption := '&Next >';
  {if (Step = 1) or (Step > Notebook2.Pages.Count) then
    BackButton.Visible := False
  else
    BackButton.Visible := True;}
  BackButton.Visible := not ((Step = 1) or (Step > Notebook2.Pages.Count));
  NextButton.Visible := (Step <> 6);
  CancelButton.Visible := (Step <> 7);
  if Step = 1 then begin
    Notebook1.PageIndex := 0;
    ActiveControl := chkNoIntro;
  end
  else if Step = Notebook2.Pages.Count + 2 then begin
    Notebook1.PageIndex := 2;
    ActiveControl := NextButton;
  end
  else begin
    lblCaption.Caption := Captions[Step - 1];
    lblDescription.Caption := Descriptions[Step - 1];
    Notebook1.PageIndex := 1;
    NoteBook2.PageIndex := Step - 2;
    case Step of
      2: begin
        if optLastSaved.Checked then
          ActiveControl := optLastSaved
        else if optDefault.Checked then
          ActiveControl := optDefault
        else if optName.Checked then
          ActiveControl := cmbName
        else if optFilename.Checked then
          ActiveControl := txtFileName
      end;
      3: ActiveControl := lvProgs;
      4: begin
        if chkScreenSave.Enabled then
          ActiveControl := chkScreenSave
        else if chkWindows.Enabled then
          ActiveControl := chkWindows
        else if IsWinNT then begin
          if chkServices.Enabled then
            ActiveControl := chkServices;
        end
        else begin
          if chkTaskSched.Enabled then
            ActiveControl := chkTaskSched;
        end;
      end;
      5: ActiveControl := NextButton;
      6: begin
        ActiveControl := CancelButton;
        StartProcess;
      end;
    end;
  end;
end;

function TfrmRestoreWiz.GetSnapFile: String;
begin
  Result := '';
  case CurLocation of
    0: Result := LastSnapFile;
    1: Result := SnapPath + '\Default' + SnapshotExt;
    2:
      if Trim(cmbName.Text) <> '' then
        Result := SnapPath + '\' + cmbName.Text + SnapshotExt;
    3:
      if Trim(txtFilename.Text) <> '' then
        Result := txtFilename.Text;
  end;
end;

procedure TfrmRestoreWiz.UpdateSnapFile;
var
  SnapFile: String;
  Date: String;
  SnapTitle: String;
  IconResID: Integer;
begin
  PageLoaded[3] := False;
  PageLoaded[4] := False;
  PageLoaded[5] := False;
  lblDate.Caption := '';
  icoTip.IconHandle := 0;
  SnapFile := GetSnapFile;
  if SnapFile = '' then
    Exit;
  Corrupt := crNormalSnap;
  if not FileExists(SnapFile) then
    Corrupt := crNoFile
  else if not FirstSnapCheck(SnapFile) then
    Corrupt := crNoSnapFile
  else begin
    OpenSnapFile(SnapFile);
    if IsCorruptedSnapFile(Snap) then
      Corrupt := crCorrupt;
  end;
  if not (Corrupt = crNoFile) then begin
    SnapTitle := ExtractFileName(SnapFile);
    SnapTitle := Copy(SnapTitle, 1,
        Length(SnapTitle) - Length(ExtractFileExt(SnapFile)));
  end;
  if Corrupt = crNormalSnap then begin
    IconResID := 14;
    Date := GetSnapDate;
    if Date = '' then
      Date := '(not available)';
  end
  else
    IconResID := 20;
  icoTip.LoadFromResourceID(IconResID, 16);
  lblDate.Caption := Format(DateCaptions[Corrupt], [SnapTitle, Date]);
end;

procedure TfrmRestoreWiz.SetExpWinCaption(Cnt: Integer);
begin
  case Cnt of
    -1:
      lblWindows.Caption := 'no Explorer or Internet Explorer windows to restore';
    {0:
      lblWindows.Caption := '0 Explorer or Internet Explorer windows to restore'}
    else begin
      chkWindows.Enabled := not (Cnt = 0);
      lblWindows.Caption := IntToStr(Cnt) +
          IIf(Cnt = 1, ' Explorer or Internet Explorer window to restore',
          ' Explorer and Internet Explorer windows to restore');
    end;
  end;
  chkWindows.Checked := (Cnt > 0);
end;

procedure TfrmRestoreWiz.SetServCaption(Cnt: Integer);
begin
  case Cnt of
    -1:
      lblServices.Caption := 'no services to restart';
    {0:
      lblServices.Caption := '0 services to restart'}
    else begin
      chkServices.Enabled := not (Cnt = 0);
      lblServices.Caption := IntToStr(Cnt) +
          IIf(Cnt = 1, ' service to restart', ' services to restart');
    end;
  end;
  chkServices.Checked := (Cnt > 0);
end;

procedure TfrmRestoreWiz.FormCloseQuery(Sender: TObject; var CanClose: Boolean);
begin
  if CurStep < TotalSteps - 1 then begin
    if MsgBox('Are you sure you want to exit the SmartClose Restore Wizard?',
        bgYesNo, miQuestion, 'Exit Wizard?', 0) = IDNO then
      CanClose := False;
  end
  else if (Notebook1.ActivePage = 'Main')
      and (Notebook2.ActivePage = 'Process') and (Cancel = False) then begin
    if MsgBox('Are you sure you want to cancel the Restore Process and exit the ' +
        'SmartClose Restore Wizard?', bgYesNo, miWarning,
        'Exit Wizard?', 0) = IDNO then
      CanClose := False
    else
      Cancel := True;
  end;
end;

procedure TfrmRestoreWiz.CancelButtonClick(Sender: TObject);
begin
  Close;
end;

procedure TfrmRestoreWiz.FormCreate(Sender: TObject);
var
  i: Integer;
  GIF: TGIFImage;
  Bmp: TBitmap;
  BaseUnitX, BaseUnitY: Integer;
begin
  SetFormFont(Self, BaseUnitX, BaseUnitY);
  //InitFormFont(Self);
  SetFontNameSize(WelcomeLabel1.Font, FormFont, 12, '', 12);
  lblCaption.Font.Style := [fsBold];
  optLastSaved.Font.Style := [fsBold];
  optDefault.Font.Style := [fsBold];
  optName.Font.Style := [fsBold];
  optFilename.Font.Style := [fsBold];
  lblCount.Font.Color := clRed;
  lblDebug.Font.Color := clRed;
  SetFontNameSize(lblDetails.Font, FormFont, 8, '', 8);
  ConvertTo32BitImageList(imgsProgs);
  WelcomeLabel3.Caption := WelcomeText;
  frmRestoreWiz.Icon.ReleaseHandle;
  frmRestoreWiz.Icon.Handle := LoadIcon(HInstance, PChar(2));
  TaskLabels := TList.Create;
  lvProgs.ReadSettings;
  for i := 1 to TotalSteps do
    PageLoaded[i] := False;
  LastWizKey := RegKey + '\LastResWiz';
  GIF := TGIFImage.Create;
  try
    {GIF.Parent := paPicture;
    GIF.Align  := alClient;
    GIF.Center := True;
    GIF.Stretch := True;}
    GIF.LoadFromResourceName(HINSTANCE, 'RestoreBig');
    imgPicture.Picture.Assign(GIF);
    //GIF := TGIFImage.Create(Self);
    {GIF.Parent := paSmallPicture;
    GIF.Align  := alClient;
    GIF.Center := True;
    //GIF.Stretch := True;}
    GIF.LoadFromResourceName(HINSTANCE, 'RestoreSmall');
    imgSmall.Picture.Assign(GIF);
  finally
    GIF.Free;
  end;
  lvProgs.SaveSettings.Root := HKEY_LOCAL_MACHINE;
  lvProgs.SaveSettings.RegistryKey := LastWizKey + '\lvProgs';
  OpenDialog.DefaultExt := SnapshotExt;
  OpenDialog.Filter := 'SmartClose System Snapshot (*' + SnapshotExt + ')|*' +
      SnapshotExt;
  imgSnap.Picture.Bitmap.LoadFromResourceName(hInstance, 'OPEN');
  imgProgs.Picture.Icon.ReleaseHandle;
  imgProgs.Picture.Icon.Handle := LoadIcon(hInstance, PChar(21));
  imgSettings.Picture.Icon.ReleaseHandle;
  imgSettings.Picture.Icon.Handle := LoadIcon(hInstance, PChar(5));
  imgOverlay.Picture.Icon.ReleaseHandle;
  imgOverlay.Picture.Icon.Handle := LoadIcon(hInstance, PChar(22));
  icoScreenSave.LoadFromResourceID(12, 16);
  icoWins.LoadFromResourceID(11, 16);
  if IsWinNT then
    icoServ.LoadFromResourceID(13, 16)
  else
    icoSched.LoadFromResourceID(14, 16);
  icoProcess.LoadFromResourceID(2, 48);
  Bmp := TBitmap.Create;
  try
    Bmp.LoadFromResourceName(hInstance, 'ARROW');
    imgsStates.AddMasked(Bmp, clFuchsia);
  finally
    Bmp.Free;
  end;
  lblDetails.Cursor := crHand;
  LoadRestoreSettings(Settings);
  chkNoIntro.Checked := Settings.NoIntro;
  PageLoaded[1] := True;
end;

procedure TfrmRestoreWiz.ParseParams;
var
  i: Integer;
  UseFile: Boolean;
begin
  Application.CreateForm(TIEWindows, IEWindows);
  if IsWinNT then
    Application.CreateForm(TServices, Services);
  UseFile := False;
  for i := 1 to ParamCount do begin
    if (ParamStr(i)[1] <> '/') and FileExists(ParamStr(i)) then begin
      UseFile := True;
      NextStep(2, False);
      UseClick := False;
      optFilename.Checked := True;
      UseClick := True;
      txtFilename.Text := ParamStr(i);
      UpdateSnapFile;
      Break;
    end;
  end;
  if UseFile then
    NextStep(3, False)
  else begin
    if chkNoIntro.Checked then
      NextStep(2, False)
    else
      NextStep(1, False);
  end;
end;

procedure TfrmRestoreWiz.NextButtonClick(Sender: TObject);
begin
  if CurStep = TotalSteps then
    Close
  else
    NextStep(CurStep + 1, False);
end;

procedure TfrmRestoreWiz.BackButtonClick(Sender: TObject);
begin
  NextStep(CurStep - 1, True);
end;

procedure TfrmRestoreWiz.optLocationClick(Sender: TObject);
var
  i: Integer;
  Control: TRadioButton;
begin
  with Notebook2.Pages.Objects[0] as TPage do begin
    for i := 0 to ControlCount - 1 do begin
      if Controls[i] is TRadioButton then begin
        Control := TRadioButton(Controls[i]);
        if Controls[i] = Sender then
          Control.Font.Style := [fsBold]
        else
          Control.Font.Style := [];
      end;
    end;
  end;
  CurLocation := (Sender as TRadioButton).Tag;
  if UseClick then begin
    if Sender = optName then
      ActiveControl := cmbName
    else if Sender = optFilename then
      btnBrowse.SetFocus;
    UpdateSnapFile;
  end;
end;

procedure TfrmRestoreWiz.cmbNameChange(Sender: TObject);
begin
  optName.Checked := True;
  UpdateSnapFile;
end;

procedure TfrmRestoreWiz.btnBrowseClick(Sender: TObject);
begin
  {OFN.Filter := 'SmartClose System Snapshots (*' + Snapshot_Ext + ')'#0'*' +
      Snapshot_Ext + #0;
  if txtFilename.Text <> '' then begin
    OFN.InitialDir := '';
    OFN.FileName := txtFilename.Text;
  end
  else
    OFN.InitialDir := SnapPath;
  OFN.Title := 'SmartClose - Restore a System Snapshot';
  OFN.DefaultExt := Copy(Snapshot_Ext, 2, Length(Snapshot_Ext) - 1);
  if OpenFilename(frmRestoreWiz.Handle, OFN) then begin
    txtFilename.Text := OFN.Filename;
    optFilename.Checked := True;
  end;}
  if txtFilename.Text <> '' then begin
    OpenDialog.InitialDir := '';
    OpenDialog.FileName := txtFilename.Text;
  end
  else
    OpenDialog.InitialDir := SnapPath;
  if OpenDialog.Execute then begin
    txtFilename.Text := OpenDialog.FileName;
    optFilename.Checked := True;
    UpdateSnapFile;
  end;
end;

procedure TfrmRestoreWiz.FormDestroy(Sender: TObject);
begin
  FreeProcObjs;
  FreeSnapIni;
end;

{procedure TfrmRestoreWiz.lvProgsDrawItem(Control: TWinControl;
    var ACanvas: TCanvas; Index: Integer; ARect: TRect;
    State: TOwnerDrawState; var DefaultDrawing, FullRowSelect: Boolean);
var
  CBRect: TRect;
  PS: TPaintStruct;
  CBDrawState: Integer;
begin
  DefaultDrawing := TRUE;  // let the componet draw it for us.
  FullRowSelect := TRUE;
  CBDrawState := DFCS_BUTTONCHECK;
  arect.Right := 0;
  CBRect.left := 0;
  CBRect.top := ARect.Top;
  CBRect.right := 16;  // adjust as desired
  CBRect.bottom := ARect.Bottom;
  BeginPaint(lvProgs.Handle, PS);
  if lvProgs.Items.Item[Index].Checked then
    CBDrawState := CBDrawState or DFCS_CHECKED;
  DrawFrameControl(ACanvas.Handle, CBRect, DFC_BUTTON, CBDrawState);
  if (lvProgs.Selected <> nil) and (lvProgs.Selected.Index = Index) then
    ACanvas.Font.Style := ACanvas.Font.Style + [fsBold, fsItalic];
end;}

procedure TfrmRestoreWiz.mnuDetailsClick(Sender: TObject);
var
  ProcObj: TResProcObj;
  SFI: TSHFileInfo;
begin
  ProcObj := lvProgs.Selected.Data;
  Application.CreateForm(TResProg, ResProg);
  with ResProg do try
    Caption := 'Program Details for ' + ProcObj.Title;
    SHGetFileInfo(PChar(ProcObj.ExeFile), 0, SFI, SizeOf(SFI),
        SHGFI_ICON or SHGFI_LARGEICON);
    imgIcon.Picture.Icon.ReleaseHandle;
    if SFI.HIcon <> 0 then
      imgIcon.Picture.Icon.Handle := SFI.HIcon
    else
      imgIcon.Picture.Icon.Handle := LoadIcon(hInstance, PChar(6));
    txtTitle.Text := ProcObj.Title;
    txtCmdLine.Text := ProcObj.CommandLine;
    txtOrgFName.Text := ProcObj.OrigExe;
    txtLocalFName.Text := ProcObj.LocalExe;
    txtArgs.Text := ProcObj.Params;
    txtCurDir.Text := ProcObj.CurDir;
    txtTitle.Hint := ProcObj.Title;
    txtCmdLine.Hint := ProcObj.CommandLine;
    txtOrgFName.Hint := ProcObj.OrigExe;
    txtLocalFName.Hint := ProcObj.LocalExe;
    txtArgs.Hint := ProcObj.Params;
    txtCurDir.Hint := ProcObj.CurDir;
    txtWarnings.Lines.BeginUpdate;
    try
      if not FileExists(ProcObj.OrigExe) then begin
        txtWarnings.Lines.Add('Warning: the original filename (' +
            ProcObj.OrigExe + ') doesn''t exist; the local filename will be used.');
        if not FileExists(ProcObj.LocalExe) then begin
          txtWarnings.Lines.Add('Error: the local filename (' +
              ProcObj.LocalExe + ') doesn''t exist. SmartClose will not be able ' +
              'to restore the program.');
        end;
      end
      else begin
        if not FileExists(ProcObj.LocalExe) then begin
          txtWarnings.Lines.Add('Warning: the local filename (' +
              ProcObj.LocalExe + ') doesn''t exist.');
        end;
      end;
      if not DirectoryExists(ProcObj.CurDir) then begin
        txtWarnings.Lines.Add('Warning: the working directory (' +
            ProcObj.CurDir + ') doesn''t exist.');
      end;
      if txtWarnings.Text = '' then
        txtWarnings.Lines.Add('(No warnings or errors.)');
    finally
      txtWarnings.Lines.EndUpdate;
    end;
    ShowModal;
  finally
    Close;
  end;
end;

procedure TfrmRestoreWiz.mnuProgsPopup(Sender: TObject);
begin
  mnuDetails.Enabled := (lvProgs.Selected <> nil);
end;

procedure TfrmRestoreWiz.FreeProcObjs;
var
  i: Integer;
  Item: TListItem;
  Obj: TObject;
begin
  for i := 0 to lvProgs.Items.Count - 1 do begin
    Item := lvProgs.Items[i];
    if Item.Data <> nil then begin
      Obj := Item.Data;
      if Obj is TResProcObj then begin
        Obj.Free;
        Item.Data := nil;
      end;
    end;
  end;
end;

procedure TfrmRestoreWiz.lvProgsDblClick(Sender: TObject);
begin
  if lvProgs.Selected <> nil then
    mnuDetailsClick(Self);
end;

procedure TfrmRestoreWiz.CheckOrUncheckAllProgs(Check: Boolean);
var
  i: Integer;
begin
  for i := 0 to lvProgs.Items.Count - 1 do begin
    if lvProgs.Items.Item[i].ImageIndex <> 0 then
      lvProgs.Items.Item[i].Checked := Check;
  end;
end;

procedure TfrmRestoreWiz.btnCheckAllClick(Sender: TObject);
begin
  CheckOrUncheckAllProgs(True);
end;

procedure TfrmRestoreWiz.btnCheckNoneClick(Sender: TObject);
begin
  CheckOrUncheckAllProgs(False);
end;

procedure TfrmRestoreWiz.btnUncheckActiveClick(Sender: TObject);
  procedure CheckProgName(Name: String);
  var
    j: Integer;
  begin
    for j := 0 to lvProgs.Items.Count - 1 do begin
      if lvProgs.Items.Item[j].Checked
          and (LowerCase(lvProgs.Items.Item[j].Caption) = LowerCase(Name)) then
        lvProgs.Items.Item[j].Checked := False;
    end;
  end;
var
  Procs: TStringList;
  i: Integer;
  Title: String;
begin
  ShowWait(False, msgProgsLoad);
  try
    Procs := TStringList.Create;
    try
      GetProcesses(Procs);
      for i := 0 to Procs.Count - 1 do begin
        Title := GetProcessTitle(TProcWinObj(Procs.Objects[i]).ExeName);
        CheckProgName(Title);
      end;
    finally
      FreeObjStrings(Procs);
    end;
  finally
    HideWait;
  end;
end;

procedure TfrmRestoreWiz.btnWindowsClick(Sender: TObject);
begin
  if IEWindows <> nil then begin
    IEWindows.ShowModal;
    SetExpWinCaption(GetWindowsCount);
  end;
end;

procedure TfrmRestoreWiz.btnServicesClick(Sender: TObject);
begin
  if Services <> nil then begin
    Services.ShowModal;
    SetServCaption(GetServicesCount);
  end;
end;

procedure TfrmRestoreWiz.lblDetailsClick(Sender: TObject);
begin
  Application.CreateForm(TLogWindow, LogWindow);
  LogWindow.Caption := 'View Restore Log';
  LogWindow.txtLog.Text := LogFile;
  LogWindow.ShowModal;
end;

procedure TfrmRestoreWiz.FormClose(Sender: TObject; var Action: TCloseAction);
var
  AutoExit: Boolean;
begin
  AutoExit := Settings.AutoExit and (CurStep = TotalSteps);
  if (frmMain <> nil) and not AutoExit then
    frmMain.Show
  else
    Application.Terminate;
end;

procedure TfrmRestoreWiz.FormShow(Sender: TObject);
begin
  Application.Title := Caption;
  Application.Icon := Self.Icon;
end;

end.
