(****************************************************************************
 *
 *            SmartClose
 *
 *            Copyright (c) 2016-2018 Tim De Baets
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
 * Close Programs Wizard form
 *
 ****************************************************************************)

unit CloseWiz;

interface

uses Windows, Messages, SysUtils, MBCSUtil, Classes, Graphics, Controls, Forms,
    NewDialogs, ExtCtrls, StdCtrls, MyRegistry, GifImage, ExtProgressBar, Menus,
    Dialogs, ThemeMgr, ExtChkListView, AgOpenDialog, IconView, ComCtrls,
    EnhListView, ExtListView, EnhButtn, NewCommCtrl, Process, ImgList;

type
  TfrmCloseWiz = class(TForm)
    Bevel1: TBevel;
    PnlMain: TPanel;
    lblCaption: TLabel;
    lblDescription: TLabel;
    BackButton: TButton;
    NextButton: TButton;
    CancelButton: TButton;
    Bevel: TBevel;
    paWelcome: TPanel;
    WelcomeLabel1: TLabel;
    WelcomeLabel2: TLabel;
    WelcomeLabel3: TLabel;
    WelcomeLabel5: TLabel;
    chkNoIntro: TCheckBox;
    Notebook1: TNotebook;
    imgsProgs: TImageList;
    SaveDialog: TAgSaveDialog;
    Notebook2: TNotebook;
    SelectComponentsLabel: TLabel;
    Label1: TLabel;
    imgTasks: TImage;
    fraDescription: TGroupBox;
    imgProtect: TImage;
    Label2: TLabel;
    Label4: TLabel;
    btnAdd: TButton;
    btnEdit: TButton;
    btnRemove: TButton;
    InfoBeforeClickLabel: TLabel;
    imgSnap: TImage;
    Label5: TLabel;
    Label6: TLabel;
    Label7: TLabel;
    optDefault: TRadioButton;
    optName: TRadioButton;
    txtName: TEdit;
    optFilename: TRadioButton;
    txtFilename: TEdit;
    btnBrowse: TButton;
    ReadyLabel: TLabel;
    txtSummary: TMemo;
    Label8: TLabel;
    TotalProgress: TdfsExtProgressBar;
    CurrentProgress: TdfsExtProgressBar;
    ProgressNotebook: TNotebook;
    StatusLabel: TLabel;
    lblTask: TLabel;
    lblArrow: TLabel;
    lstProgs: TListView;
    lblCurrent: TLabel;
    imgsStates: TImageList;
    lblCount: TLabel;
    paFinished: TPanel;
    FinishedLabel: TLabel;
    Label9: TLabel;
    lblDetails: TLabel;
    tmrCloseTime: TTimer;
    lblDebug: TLabel;
    ThemeManager1: TThemeManager;
    lvTasks: TExtChkListView;
    lblTaskDescription: TLabel;
    imgsTasks: TImageList;
    imgTask: TImage;
    lblTaskTitle: TLabel;
    imgsTasksLarge: TImageList;
    lblTip: TLabel;
    tmrCapture: TTimer;
    chkHideCritical: TCheckBox95;
    chkHideUnsafe: TCheckBox95;
    lvProgs: TExtChkListView;
    icoProcess: TIconView;
    imgPicture: TImage;
    imgSmall: TImage;
    procedure CancelButtonClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure NextButtonClick(Sender: TObject);
    procedure BackButtonClick(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure lvProgsClick(Sender: TObject);
    procedure btnAddClick(Sender: TObject);
    procedure lvProgsChange(Sender: TObject; Item: TListItem;
      Change: TItemChange);
    procedure btnEditClick(Sender: TObject);
    procedure btnRemoveClick(Sender: TObject);
    procedure optLocationClick(Sender: TObject);
    procedure btnBrowseClick(Sender: TObject);
    procedure txtNameChange(Sender: TObject);
    procedure lblDetailsClick(Sender: TObject);
    procedure tmrCloseTimeTimer(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure lvTasksHotTrack(Sender: TObject; var ItemIndex: Integer;
      SubItemIndex: Integer; Location: TPoint; var AllowSelect: Boolean);
    procedure tmrCaptureTimer(Sender: TObject);
    procedure lvTasksChange(Sender: TObject; Item: TListItem;
      Change: TItemChange);
    procedure FormDestroy(Sender: TObject);
    procedure chkHideCriticalClick(Sender: TObject);
    function lvProgsItemChecking(Sender: TObject;
      ItemIndex: Integer): Boolean;
    procedure lvProgsDblClick(Sender: TObject);
    procedure lvTasksKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure lvTasksMouseMove(Sender: TObject; Shift: TShiftState; X,
      Y: Integer);
  private
    //procedure WMCopyData(var Msg: TMessage); message WM_COPYDATA;
    procedure LoadSettings(Step: Integer);
    procedure SaveSettings(Step: Integer);
    function CheckData(Step: Integer): Boolean;
    procedure LoadSummary;
    procedure LoadProgress;
    function StartProcess: Boolean;
    procedure NextStep(Step: Integer; Back: Boolean);
    function GetSnapFile: String;
    procedure FreeProcObjs;
    procedure ShowTaskTip(Selection: Boolean);
    procedure AddProProg(Idx: Integer; MakeVis: Boolean);
    procedure RefreshProProgs;
  public
    procedure ParseParams;
    function CloseProcess(ProcObj: TProcWinObj; Item: TListItem): Boolean;
    function KillProcess(ProcObj: TProcWinObj; LibH: hModule): Boolean;
    procedure Step(Num: Integer);
    procedure ThdDone(Sender : TObject);
    procedure ThdCount(Sender : TObject);
  end;
  //TRejectFunc = procedure(ThreadID: DWORD);


{function InjectGetProcessInfo(h: HWND; tid: DWORD): Boolean; external 'scinject.dll';
procedure Reject(tid: DWORD); external 'scinject.dll';}

var
  frmCloseWiz: TfrmCloseWiz;
  ProProgs: TStringList;

implementation

uses Main, Func, ExpWin, Snapshot, ProProg, CloseThread, frmLog, Service,
    Wait, MultiMon;

const
  TotalSteps = 7;
  WelcomeText = 'This wizard will help you through the process of saving the ' +
      'current state of the system and ending all the currently running ' +
      'applications, together with other possible options ' +
      '(like closing all open Explorer and Internet Explorer windows, disabling ' +
      'the screen saver etc.).';
  WelcomeText2 = 'It is highly recommended that you run this wizard before you ' +
      'are going to execute one of the following tasks: scanning your disk(s) ' +
      'for errors, defragmenting your disk(s), burning a cd-rom, installing a ' +
      'new application, playing a game that requires a lot of memory or another ' +
      'comparable high-end task.';
  Captions: array[1..TotalSteps - 2] of String = (
    'Select Wizard Tasks',
    'Protected Programs',
    'Save Current System Status',
    'Ready To Close All Programs',
    'SmartClose Process'
  );
  Descriptions: array[1..TotalSteps - 2] of String = (
    'Which tasks should be performed by the wizard?',
    'Which programs should be protected by SmartClose?',
    'SmartClose allows you to save the current system status so you can restore it back later.',
    'SmartClose is now ready to begin closing all programs.',
    'Please wait while SmartClose completes the following actions.'
  );
  Tasks: array[0..4] of String = (
    'Close (or kill) all programs',
    'Close all (Internet) Explorer windows',
    'Disable the screen saver',
    'Disable the Windows Task Scheduler',
    'Create a System Snapshot File'
  );
  TaskDescs: array[0..4] of String = (
    'Closes all programs, except for the ones that are protected or critical ' +
        '(see next page). ' +
        'Some programs that can''t be properly closed, will be killed (you can ' +
        'disable this in SmartClose Settings).',
    'Closes all open Explorer and Internet Explorer windows. ' +
        'If you have set SmartClose to create a system snapshot (see the last ' +
        'task), the location of the windows will be saved, so all windows can be ' +
        'restored later with their original path or address.',
    'Saves the current state of the screen saver to the system snapshot (see the ' +
        'last task) and tells Windows not to start the screen saver anymore.',
    'Automatically disables the Windows Task Scheduler. When the Task Scheduler ' +
        'is disabled, it won''t start any scheduled tasks anymore.',
    'Creates a small file on your hard disk, a system snapshot, that contains ' +
        'information about the current state of your system, such as running ' +
        'programs, so it can be opened and restored again later with the ' +
        'SmartClose Restore Wizard.'
  );
  ServicesName = 'Stop Windows Services';
  ServicesDesc = 'Services are background programs that can always be running, ' +
      'even when nobody is logged on. ' +
      'An example of such a service is the Windows Task Scheduler. ' +
      'Go to SmartClose Settings to view and/or edit the list of services that ' +
      'will be stopped by SmartClose.';
  SSaverDisable = '(Note: this task is currently not available because the ' +
      'screen saver already is disabled.)';
  TaskSchedDisable = '(Note: this task is currently not available because the ' +
      'Task Scheduler is not available or already disabled.)';
  //ServiceDisable = '(Note: this task is currently not available because the ' +
  //    'list of services was not found.)';
  ProgDescs: array[0..4] of String = (
    'Closing all running programs',
    'Closing Explorer Windows',
    '',
    '',
    'Creating System Snapshot file'
  );
  {Scandisk = 'Scandskw.exe';
  Defrag = 'Defrag.exe';}
  ProtectTypeCaptions: array[TProtectType] of String = (
    'Normal',
    'Critical',
    'Unsafe',
    'Temporary'
  );

var
  LastWizKey: String;
  CurStep: Integer;
  PageLoaded: array[1..TotalSteps] of Boolean;
  CurLocation: Integer;
  UseClick: Boolean;
  TaskLabels: TList;
  //Reject: RejectType;
  Cancel: Boolean;
  Settings: TCloseSettings;
  UseKeyb: Boolean;
  WaitingThreads: TList = nil;

{$R *.DFM}

procedure TfrmCloseWiz.ThdDone(Sender: TObject);
begin
  IsWaiting := False;
end;

procedure TfrmCloseWiz.ThdCount(Sender: TObject);
begin
  with Sender as TCloseThread do
   if Seconds < CloseTimeout then begin
     lblCount.Visible := True;
     lblCount.Caption := IntToStr(Seconds);
   end;
end;

{procedure TfrmCloseWiz.WMCopyData(var Msg: TMessage);
var
  cds : PCopyDataStruct;
begin
  cds := PCopyDataStruct(Msg.LParam);
  s := PChar(cds.lpData);
  Reject(cds.dwData);
end;}

procedure UpdateProProg(Idx: Integer; Check: Boolean);
var
  Info: Word;
begin
  Info := Word(ProProgs.Objects[Idx]);
  WordRec(Info).Lo := Byte(Check);
  ProProgs.Objects[Idx] := TObject(Info);
end;

procedure TfrmCloseWiz.AddProProg(Idx: Integer; MakeVis: Boolean);
var
  Typ: TProtectType;
begin
  Typ := TProtectType(Hi(Word(ProProgs.Objects[Idx])));
  if (Typ = ptCritical) and chkHideCritical.Checked then
    Exit;
  if (Typ = ptUnsafe) and chkHideUnsafe.Checked then
    Exit;
  with TChkListItem(lvProgs.Items.Add) do begin
    Checked := LongBool(Lo(Word(ProProgs.Objects[Idx])));
    Caption := GetProcessTitle(ProProgs.Strings[Idx]);
    SubItems.Add(ProtectTypeCaptions[Typ]);
    ImageIndex := Integer(Typ);
    Enabled := (Typ <> ptCritical);
    lvProgs.Resort;
    if MakeVis then begin
      Selected := True;
      MakeVisible(False);
    end;
  end;
end;

procedure TfrmCloseWiz.RefreshProProgs;
var
  i: Integer;
begin
  with lvProgs do begin
    Items.Clear;
    Selected := nil;
    lvProgsClick(Self);
    BeginUpdate;
    for i := 0 to ProProgs.Count - 1 do
      AddProProg(i, False);
    EndUpdate;
  end;
end;

procedure TfrmCloseWiz.LoadSettings(Step: Integer);
var
  i: Integer;
  Registry: TMyRegistry;
  Item: TChkListItem;
begin
  {$IFDEF Debug}
  OutputDebugString('LoadSettings enter');
  {$ENDIF}
  UseClick := False;
  Registry := TMyRegistry.Create;
  try
    Registry.RootKey := Hive;
    Registry.OpenKey(LastWizKey, True);
    case Step of
      2: begin
        //lvTasks.Selected := nil;
        //lblTaskDescription.Caption := TaskDesc[0];
        for i := 0 to lvTasks.Items.Count - 1 do begin
          Item := TChkListItem(lvTasks.Items[i]);
          case i of
            2: begin
              if not GetIsScreenSaveActive then
                Item.Enabled := False;
            end;
            3: begin
              if IsWinNT then begin
                if GetServicesFilePath = '' then
                  Item.Enabled := False;
              end
              else begin
                if not SysAgent_IsAvailable then
                  Item.Enabled := False;
              end;
            end;
          end;
          if Item.Enabled then
           Item.Checked := Registry.ReadBoolDef('Task' + IntToStr(i + 1), True);
        end;
      end;
      3: begin
        chkHideCritical.Checked := Registry.ReadBoolDef('HideCritical', False);
        chkHideUnsafe.Checked := Registry.ReadBoolDef('HideUnsafe', False);
        ProProgs := LoadProtectedProgs(False);
        RefreshProProgs;
      end;
      4: begin
        txtName.Text := Registry.ReadStringDef('Name', '');
        txtFilename.Text := Registry.ReadStringDef('Filename', '');
        case Registry.ReadIntegerDef('Location', 0) of
          1: optName.Checked := True;
          2: optFilename.Checked := True;
          else optDefault.Checked := True;
        end;
      end;
      7: begin
        CancelButton.Enabled := True;
        {if frmMain <> nil then begin
          lblProgDesc.Visible := False;
          chkScandisk.Visible := False;
          chkDefrag.Visible := False;
        end else begin
          lblProgDesc.Visible := True;
          chkScandisk.Visible := True;
          chkDefrag.Visible := True;
          chkScandisk.Enabled := FileExists(AddBackslash(GetWinDir) + Scandisk);
          chkDefrag.Enabled := FileExists(AddBackslash(GetWinDir) + Defrag);
        end;}
      end;
    end;
    PageLoaded[Step] := True;
  finally
    Registry.Free;
    UseClick := True;
  end;
  {$IFDEF Debug}
  OutputDebugString('LoadSettings exit');
  {$ENDIF}
end;

procedure TfrmCloseWiz.SaveSettings(Step: Integer);
var
  i: Integer;
  Registry: TMyRegistry;
  Typ: TProtectType;
begin
  //CreateKey(Hive, LastWizKey);
  Registry := TMyRegistry.Create;
  try
    Registry.RootKey := Hive;
    Registry.OpenKey(LastWizKey, True);
    case Step of
      1: SetKeyBool(Hive, RegKey, 'NoIntro', chkNoIntro.Checked);
      2: begin
        for i := 1 to lvTasks.Items.Count do
          if TChkListItem(lvTasks.Items[i - 1]).Enabled then begin
            Registry.WriteBool('Task' + IntToStr(i),
                lvTasks.Items.Item[i - 1].Checked);
          end;
      end;
      3: begin
        ClearSubKeys(Hive, LastWizKey + '\Protected');
        with TMyRegistry.Create do try
          RootKey := Hive;
          for i := 0 to ProProgs.Count - 1 do begin
            Typ := TProtectType(Hi(Word(ProProgs.Objects[i])));
            if not (Typ in [ptCritical, ptTemp]) then begin
              if OpenKey(LastWizKey + '\Protected\' + ProProgs.Strings[i],
                  True) then begin
                WriteBool('Checked', LongBool(Lo(Word(ProProgs.Objects[i]))));
                WriteBool('Unsafe', Typ = ptUnsafe);
                CloseKey;
              end;
            end;
          end;
        finally
          Free;
        end;
        Registry.WriteBool('HideCritical', chkHideCritical.Checked);
        Registry.WriteBool('HideUnsafe', chkHideUnsafe.Checked);
      end;
      4: begin
        Registry.WriteInteger('Location', CurLocation);
        if optName.Checked then
          Registry.WriteString('Name', txtName.Text)
        else if optFilename.Checked then
          Registry.WriteString('Filename', txtFilename.Text);
        end;
    end;
  finally
    Registry.Free;
  end;
end;

function TfrmCloseWiz.CheckData(Step: Integer): Boolean;
var
  i, Tmp: Integer;
begin
  {$IFDEF Debug}
  OutputDebugString('CheckData enter');
  {$ENDIF}
  Result := True;
  case Step of
    2: begin
      Tmp := 0;
      for i := 0 to lvTasks.Items.Count - 1 do begin
        if lvTasks.Items.Item[i].Checked then
          Inc(Tmp);
      end;
      if Tmp = 0 then begin
        Result := False;
        MsgBox('You have to select at least one task!', bgOK, miWarning,
            'No tasks selected', 0);
        ActiveControl := lvTasks;
      end
      else if (Tmp = 1) and lvTasks.Items.Item[4].Checked then begin
        Result := False;
        MsgBox('SmartClose cannot create a System Snapshot when no other tasks ' +
            'are selected!', bgOK, miWarning, 'Tasks Warning', 0);
        ActiveControl := lvTasks;
      end;
    end;

    4: begin
      Result := False;
      txtName.Text := Trim(txtName.Text);
      txtFilename.Text := Trim(txtFilename.Text);
      if optName.Checked and (txtName.Text = '') then begin
        MsgBox('You have to specify a name.', bgOK , miWarning,
            'No name specified', 0);
        ActiveControl := txtName;
      end
      else begin
        if optName.Checked and (IsValidName(txtName.Text) = False) then begin
          MsgBox('The name of a system snapshot cannot contain any of the ' +
              'following characters:' + CrLf + '\ / : * ? " < > | ',
              bgOK, miError, 'Invalid System Snapshot name', 0);
          ActiveControl := txtName;
        end
        else begin
          if optName.Checked
              and (FileExists(SnapPath + '\' + txtName.Text + SnapshotExt)) then begin
            if MsgBox('The system snapshot file ' + txtName.Text + ' already ' +
                'exists.' + CrLf + 'Do you want to replace the existing file?',
                bgYesNo, miQuestion,
                'Replace Existing System Snapshot?', 0) = IDNO then
              ActiveControl := txtName
            else
              Result := True;
          end
          else begin
            if optFilename.Checked and (Trim(txtFilename.Text) = '') then begin
              MsgBox('You have to specify a filename by pressing the browse-button.',
                  bgOK, miWarning, 'No filename specified', 0);
              ActiveControl := btnBrowse;
            end
            else
              Result := True;
          end;
        end;
      end;
    end;
  end;
  {$IFDEF Debug}
  OutputDebugString('CheckData exit');
  {$ENDIF}
end;

procedure TfrmCloseWiz.LoadSummary;
var
  Space, Tmp: String;
  i: Integer;
begin
  with txtSummary do begin
    Space := Format('%6s', ['']);
    Lines.BeginUpdate();
    Lines.Clear;
    {Lines.Append('Warning: After you have pressed "Start", it is recommended to ' +
        'leave your computer alone while SmartClose is busy. For example, ' +
        'do not start or close any programs during the SmartClose-process, ' +
        'because otherwise, SmartClose may get confused.');
    Lines.Append('');}
    Lines.Append( 'Tasks:');
    for i := 1 to lvTasks.Items.Count do begin
      if lvTasks.Items.Item[i - 1].Checked then
        Lines.Append(Space + '- ' + lvTasks.Items.Item[i - 1].Caption);
    end;
    Lines.Append('');
    if lvTasks.Items.Item[0].Checked then begin
      Lines.Append( 'Protected Programs:');
      for i := 0 to ProProgs.Count - 1 do begin
        if LongBool(Lo(Word(ProProgs.Objects[i]))) then
          Lines.Append(Space + '- ' + GetProcessTitle(ProProgs.Strings[i]));
      end;
      Lines.Append('');
    end;
    if lvTasks.Items.Item[4].Checked then begin
      Lines.Append( 'System Snapshot');
      case CurLocation of
        0: Tmp := ': (Default)';
        1: Tmp := ' Name: ' + txtName.Text;
        2: Tmp := ' Filename: ' + txtFilename.Text;
      end;
      Lines.Append(Space + 'Location' + Tmp);
    end;
    SelStart := 0;
    SelLength := 0;
    Lines.EndUpdate;
  end;
end;

procedure TfrmCloseWiz.LoadProgress;
var
  TaskLabel: TLabel;
  i, Y: Integer;
  Caption: String;
  Tag: Longint;
begin
  ProgressNotebook.PageIndex := 0;
  Y := lblTask.Top;
  Tag := 0;
  for i := 0 to High(ProgDescs) do begin
    Caption := '';
    case i of
      2: begin
        if lvTasks.Items.Item[2].Checked and lvTasks.Items.Item[3].Checked then begin
          if IsWinNT then
            Caption := 'Disabling Screen Saver and Stopping Services'
          else
            Caption := 'Disabling Screen Saver and Task Scheduler';
          Tag := 23
        end
        else begin
          if lvTasks.Items.Item[2].Checked then begin
            Caption := 'Disabling Screen Saver';
            Tag := 2;
          end
          else begin
            if IsWinNT then
              Caption := 'Stopping Services'
            else
              Caption := 'Disabling Task Scheduler';
            Tag := 3;
          end;
        end;
        if (lvTasks.Items.Item[2].Checked = False)
            and (lvTasks.Items.Item[3].Checked = False) then
          Caption := '';
      end;
      3:
        Caption := '';
      else begin
        Tag := i;
        if lvTasks.Items.Item[i].Checked then
          Caption := ProgDescs[i];
      end;
    end;
    if Caption <> '' then begin
      TaskLabel := CreateLabel(lblTask, Caption, Tag ,Y);
      TaskLabels.Add(TaskLabel);
    end;
  end;
  if Mode = wmDebug then
    lblDebug.Visible := True;
end;

function GetInjectResultDesc(RetVal: Integer): String;
begin
  case RetVal of
    00: Result := '(successful)';
    01: Result := '(Failed to retreive a valid pointer)';
    02: Result := '(The VirtualAllocEx API-function failed)';
    03: Result := '(The WriteProcessMemory API-function failed)';
    04: Result := '(The CreateRemoteThread API-function failed)';
    05: Result := '(The ReadProcessMemory API-function failed)';
    else
      Result := '(unkown error)';
  end;
end;

procedure TfrmCloseWiz.FreeProcObjs;
var
  i: Integer;
  Item: TListItem;
  Obj: TObject;
begin
  for i := 0 to lstProgs.Items.Count - 1 do begin
    Item := lstProgs.Items[i];
    if Item.Data <> nil then begin
      Obj := Item.Data;
      if Obj is TProcWinObj then begin
        FreeAndNil(Obj);
        Item.Data := nil;
      end;
    end;
  end;
end;

function TfrmCloseWiz.CloseProcess(ProcObj: TProcWinObj;
    Item: TListItem): Boolean;
var
  Done, Count: TNotifyEvent;
begin
  Result := False;
  Log('Trying to close ' + ProcObj.DispName + ':');
  if Item <> nil then begin
    Done := frmCloseWiz.ThdDone;
    Count := frmCloseWiz.ThdCount;
  end
  else begin
    Done := frmWait.thdDone;
    Count := frmWait.thdCount;
  end;
  if EnumHwnds(ProcObj.ProcID, ehaNormalClose, Done, Count) = 1 then begin
    Result := True;
    ProcObj.Status := psClosed;
    Log('     Closing successful');
  end
  else
    Log('     Closing failed');
end;

function TfrmCloseWiz.KillProcess(ProcObj: TProcWinObj; LibH: hModule): Boolean;
var
  RetVal: Integer;
  Tmp: string;
begin
  Result := False;
  if not IsWinNT then begin
    Log('(SmartKill is currently not available on Windows 95, 98 and ME)');
    Exit;
  end;
  Log('Trying to terminate program nicely: ' + ProcObj.DispName);
  RetVal := InjectProcess(ProcObj.ProcID, True, LibH).Result;
  case RetVal of
    00: Tmp := '(successful)';
    01: Tmp := '(SmartClose failed to load a required API-function)';
    02: Tmp := '(The OpenProcessToken API-function failed)';
    03: Tmp := '(The LookupPrivilegeValue API-function failed)';
    04: Tmp := '(The first call to the AdjustTokenPrivileges API-function failed)';
    05: Tmp := '(The second call to the AdjustTokenPrivileges API-function failed)';
    06: Tmp := '(The OpenProcess API-function failed)';
    07: Tmp := '(The DuplicateHandle API-function failed)';
    08: Tmp := '(The CreateRemoteThread API-function failed)';
    09: Tmp := '(Nicely terminating the program failed, or the program isn''t responding)';
    else
      Tmp := '(unknown error)';
  end;
  {else begin
    RetVal := NiceExterminate9x(ProcObj.ProcID, LibH);
    case RetVal of
      00: Tmp := '(successful)';
      01: Tmp := '(SmartClose was unable to properly shut down the program ' +
          'because the SmartClose Injector library was not loaded)';
      02: Tmp := '(The GetProcAddress API-function failed)';
      03: Tmp := '(SmartClose was unable to get a thread ID of the program)';
      04: Tmp := '(Thread was already hooked by SCInject)';
      05: Tmp := '(The SetWindowsHookEx-function failed)';
      06: Tmp := '(The PostThreadMessage-function failed)';
      07: Tmp := '(SmartClose was unable to terminate the program because an ' +
          'unexpected error occurred)';
      else
        Tmp := '(unknown error)';
    end;
  end;}
  Log('     Attempt to nicely terminate program result: ' + IntToStr(RetVal) +
      ' ' + Tmp);
  {if Settings.BruteTerminate and (RetVal <> 00) then begin
    Log('     Nicely terminating program failed: terminating program');
    RetVal := Exterminate(ProcObj.ProcID);
    case RetVal of
      00: Tmp := '(successful)';
      01: Tmp := '(The OpenProcess API-function failed)';
      02: Tmp := '(Terminating the program failed)';
      03: Tmp := '(The program isn''t responding)';
      else
        Tmp := '(unknown error)';
    end;
    Log('     Attempt to terminate program result: ' + IntToStr(RetVal) + ' ' + Tmp);
    if RetVal = 00 then
      RetVal := 01;
  end;}
  if RetVal = 00 then begin
    Result := True;
    ProcObj.Status := psTerminated;
  end;
end;

function TfrmCloseWiz.StartProcess: Boolean;

  procedure AddProgToSnap(ProcObj: TProcWinObj);
  begin
    AddToSnapBuffer(rtProg, ProcObj.ExeName + '?' +
        ProcObj.CmdLine, ProcObj.CurDir);
  end;
  
//type
//  TInjectGetProcessInfo = function(AHandle: HWND; threadID: DWORD): Integer;
var
  i, j, k: Integer;
  Steps: array[0..3] of Longint;
  TotalSteps: Longint;
  NumProgs, NumNonClosed, NumNonVis: Integer;
  SSaverEnabled: Boolean;
  UseSystemIni: Boolean;
  RetVal: Integer;
  Svcs, SvcNames: TStringList;
  ExpWindows: TStringList;
  WBWrapper: TWebBrowserWrapper;
  ExpWinHandle: Integer;
  Path: TExpWindowPath;
  ProcList: TStringList;
  Item: TListItem;
  DllPath: string;
  LibH: hModule;
  //InjectGetProcessInfo: TInjectGetProcessInfo;
  ThreadID: Integer;
  InjectResult: TInjectResult;
  ProcObj: TProcWinObj;
  Tmp: String;
  SnapFile: String;
  CloseFail: Boolean;
  {IEFullScreen: Boolean;
  IEMonitorIdx: Integer;
  IEWinData: String;
  Monitors: TList;}
begin
  Cancel := False;
  Result := False;
  Svcs := nil;
  SvcNames := nil;
  ExpWindows := nil;
  ProcList := nil;
  LibH := 0;
  NumProgs := 0;
  NumNonClosed := 0;
  NumNonVis := 0;
  WaitingThreads := nil;
  //Monitors := nil;
  //@InjectGetProcessInfo := nil;

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

  try

    Log('Calculating progress bar values');
    for i := 0 to TaskLabels.Count - 1 do begin
      if TLabel(TaskLabels[i]).Tag in [2, 23]  then // disable screen saver
        Steps[i] := 1;
      if TLabel(TaskLabels[i]).Tag in [3, 23] then begin // disable task scheduler or stop services
        if IsWinNT then begin
          Svcs := TStringList.Create;
          SvcNames := TStringList.Create;
          GetServicesToStop(Svcs, SvcNames, False);
          if Svcs.Count = 0 then
            RestoreDefaultServices;
          GetServicesToStop(Svcs, SvcNames, True);
          Steps[i] := Steps[i] + Svcs.Count;
          Log('Number of services to stop: ' + IntToStr(Steps[i]));
        end
        else
          Steps[i] := Steps[i] + 2;
      end;
      case TLabel(TaskLabels[i]).Tag of
        0: begin         // programs
          ProcList := TStringList.Create;
          GetProcesses(ProcList);
          NumProgs := GetNumProcesses(ProcList, False);
          Steps[i] := NumProgs;
          Log('Number of running programs: ' + IntToStr(NumProgs));
        end;
        1: begin          // explorer windows
          ExpWindows := TStringList.Create;
          GetExpWindows(ExpWindows);
          Steps[i] := ExpWindows.Count;
          Log('Number of open Explorer windows: ' + IntToStr(ExpWindows.Count));
        end;
        4: Steps[i] := 3; // create system snapshot
      end;
    end;
    Application.ProcessMessages;
    if Cancel then
      Exit;

    for i := 0 to High(Steps) do begin
      if Steps[i] > 0 then
        TotalSteps := TotalSteps + Steps[i];
    end;
    Log('Total number of steps to do: ' + IntToStr(TotalSteps));
    CurrentProgress.Position := 100;
    TotalProgress.Max := TotalSteps;
    Application.ProcessMessages;
    if Cancel then
      Exit;
    Sleep(500);
    LogLine;

    InitSnapshot;
    for i := 0 to TaskLabels.Count - 1 do begin
      lblArrow.Visible := True;
      lblArrow.Top := TLabel(TaskLabels[i]).Top - 1;
      TLabel(TaskLabels[i]).Font.Style := [fsBold];
      CurrentProgress.Max := Steps[i];
      {if TLabel(TaskLabels[i]).Tag <> 3 then}
      CurrentProgress.Position := 0;
      Application.ProcessMessages;
      if Cancel then
        Exit;

      if TLabel(TaskLabels[i]).Tag in [2, 23] then begin // screen saver
        lblCurrent.Caption := 'Disabling screen saver...';
        Application.ProcessMessages;
        if Cancel then
          Exit;
        {UseSystemIni := True;
        SSaver := INIGetValue( 'system.ini', 'boot', 'SCRNSAVE.EXE', 'dummy');
        if SSaver = 'dummy' then
          Log('Current screen saver name was not found in system.ini')
        else if SSaver = '' then
          Log('Current screen saver is not set in system.ini')
        else
          Log('Current screen saver name was found in system.ini: ' + SSaver);
        if SSaver = 'dummy' then begin
          UseSystemIni := False;
          SSaver := GetKeyValue(HKEY_CURRENT_USER, 'Control Panel\Desktop',
              'SCRNSAVE.EXE', '');
          if SSaver = '' then begin
            Log('Current screen saver name was not found or is not set in ' +
                'HKEY_CURRENT_USER\Control Panel\Desktop');
          end
          else begin
            Log('Current screen saver name was found in ' +
                'HKEY_CURRENT_USER\Control Panel\Desktop: ' + SSaver);
          end;
        end;
        if SSaver <> '' then begin
          if Mode <> Debug then begin
            if UseSystemIni then
              INISetValue( 'system.ini', 'boot', 'SCRNSAVE.EXE', '');
            SetKeyValue(HKEY_CURRENT_USER, 'Control Panel\Desktop',
                'SCRNSAVE.EXE', '');
            Log('Disabling screen saver in system.ini and Registry');
          end;
          AddToBuffer(rtScreenSaver, SSaver, '');
        end;}
        SSaverEnabled := GetIsScreenSaveActive;
        Log('Screen saver is currently ' +
            IIf(SSaverEnabled, 'enabled', 'disabled'));
        if SSaverEnabled then begin
          if Mode <> wmDebug then begin
            Log('Disabling screen saver');
            SystemParametersInfo(SPI_SETSCREENSAVEACTIVE, 0, nil, 0);
          end;
          AddToSnapBuffer(rtScreenSaver, '', '');
        end;
        Sleep(500);
        Step(1);
        if Cancel then
          Exit;
      end;
      if TLabel(TaskLabels[i]).Tag in [3, 23] then begin
        if IsWinNT then begin
          lblCurrent.Caption := 'Stopping Services...';
          Application.ProcessMessages;
          if Cancel then
            Exit;
          for j := 0 to Svcs.Count - 1 do begin
            if Mode <> wmDebug then begin
              Log('Stopping ' + SvcNames[j] + ' (' + Svcs[j] + ')');
              RetVal := StopService(Svcs[j]);
              case RetVal of
                0: Tmp := '(successful; the service was stopped by SmartClose)';
                1: Tmp := '(there was no need to stop the service as it wasn''t ' +
                    'running or doesn''t allow to be stopped)';
                2: Tmp := '(unsuccessful; the OpenSCManager-function failed)';
                3: Tmp := '(unsuccessful; the OpenService-function failed)';
                4: Tmp := '(unsuccessful; the ControlService-function failed)';
                5: Tmp := '(unsuccessful; the stopping of the service timed out)';
              end;
              Log('     Stopping of service ' + Svcs[j] + ' result: ' +
                  IntToStr(RetVal) + ' ' + Tmp);
              if RetVal = 0 then
                AddToSnapBuffer(rtService, Svcs[j], '');
            end
            else
              AddToSnapBuffer(rtService, Svcs[j], '');
            Step(1);
            if Cancel then
              Exit;
          end;
          //Svcs.Free;
          //SvcNames.Free;
        end
        else begin           // task scheduler
          lblCurrent.Caption := 'Disabling the Task Scheduler...';
          Application.ProcessMessages;
          if Cancel then
            Exit;
          RetVal := SysAgent_GetStat;
          case RetVal of
            0: Tmp := '(Not running or error)';
            SYSAGENT_ENABLED: Tmp := '(Enabled)';
            SYSAGENT_DISABLED: Tmp := '(Disabled)';
            else
              Tmp := '(Unknown)';
          end;
          Log('Task Scheduler status: ' + IntToStr(RetVal) + ' ' + Tmp);
          if (Mode <> wmDebug) and (RetVal = SYSAGENT_ENABLED) then begin
            RetVal := SysAgent_SetStatus(False);
            if RetVal = 0 then begin
              Tmp := '(Success)';
              AddToSnapBuffer(rtTaskSched, '', '');
            end else
              Tmp := '(Not running or error)';
            Log('Return value of ''disable Task Scheduler''-message: ' +
                IntToStr(RetVal) + ' ' + Tmp);
          end
          else
            AddToSnapBuffer(rtTaskSched, '', '');
          Sleep(500);
          Step(2);
          if Cancel then
            Exit;
        end;
      end;
      case TLabel(TaskLabels[i]).Tag of
        0: begin // programs
          lblCurrent.Caption := 'Loading currently running programs...';
          Application.ProcessMessages;
          if Cancel then
            Exit;
          WaitingThreads := TList.Create;
          {if not IsWinNT then begin
            LibH := LoadLibrary('scinject.dll');
            @InjectGetProcessInfo := GetProcAddress(LibH, 'InjectGetProcessInfo');
            if LibH <> 0 then
              @Reject := GetProcAddress(LibH, 'Reject')
            else
              Log('Loading of SmartClose Injector DLL failed!');
          end;}
          LoadCurProgs(lcpmCloseWizProgList, ProcList);
          //ProcList.Free;
          if Settings.ShowProgs then begin
            Sleep(500);
            ProgressNotebook.PageIndex := 1;
            //ActiveControl := lstProgs;
          end;
          Application.ProcessMessages;
          if Cancel then
            Exit;
          for j := 0 to lstProgs.Items.Count - 1 do begin
            Item := lstProgs.Items[j];
            EnsureVisible(Item);
            lstProgs.Selected := Item;
            lblCurrent.Caption := 'Getting program information...';
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
            if (ProcObj.Status = psRunning)
                or (ProcObj.Status = psInvisible) then begin
              Log('Getting information for ' + ProcObj.DispName);
              {if not IsWinNT then begin
                ThreadID := GetFirstThreadID(Procobj.ProcID);
                Log('     First Thread ID: ' + IntToStr(ThreadID));
                if (ThreadID <> 0) and (@InjectGetProcessInfo <> nil) then begin
                  s := '';
                  RetVal := InjectGetProcessInfo(frmCloseWiz.Handle, ThreadID);
                  case RetVal of
                    0: Tmp := '(successful)';
                    1: Tmp := '(Thread was already hooked by SCInject)';
                    2: Tmp := '(The SetWindowsHookEx-function failed)';
                    3: Tmp := '(The PostThreadMessage-function failed)';
                    else
                      Tmp := '(unknown error)';
                  end;
                  Log('     SCInject InjectGetProcessInfo-function result: ' +
                      IntToStr(RetVal) + ' ' + Tmp);
                  Sleep(300);
                  Application.ProcessMessages;
                  if Cancel then
                    Exit;
                  ProcObj.CmdLine := s;
                  if s <> '' then begin
                    Log('     Value returned by SCInject ' +
                        InjectGetProcessInfo-function: ' + s);
                  end;
                end;
              end
              else begin     // NT}
              {if (not IsWinNT) and (LibH = 0) then
                LibH := LoadRTDLL(DLLPath);}
              InjectResult.CmdLine := '';
              InjectResult.CurDir := '';
              InjectResult := InjectProcess(ProcObj.ProcID, False, 0);
              case InjectResult.Result of
                00: Tmp := '(successful)';
                01: Tmp := '(SmartClose failed to load one or more required ' +
                    'API-functions)';
                02: Tmp := '(The OpenProcessToken API-function failed)';
                03: Tmp := '(The LookupPrivilegeValue API-function failed)';
                04: Tmp := '(The first call to the AdjustTokenPrivileges ' +
                    'API-function failed)';
                05: Tmp := '(The second call to the AdjustTokenPrivileges ' +
                    'API-function failed)';
                06: Tmp := '(The OpenProcess API-function failed)';
                07: Tmp := '(The DuplicateHandle API-function failed)';
                08: Tmp := '(SmartClose failed to retreive the ''obsfucator'')';
                09: Tmp := '(SmartClose failed to retreive the process ' +
                    'information structure)';
                10: Tmp := '(An error occurred while reading the command line ' +
                    'and/or current directory from the process information ' +
                    'structure)';
                else
                  Tmp := '(unknown error)';
              end;
              Log( '     InjectProcess-function result: ' +
                  IntToStr(InjectResult.Result) + ' ' + Tmp);
              if InjectResult.Result = 0 then begin
                Log( '     GetProcCmdLine-function result: ' +
                    IntToStr(InjectResult.CmdLineResult) + ' ' +
                    GetInjectResultDesc(InjectResult.CmdLineResult));
                if (InjectResult.CmdLineResult = 0)
                    and (InjectResult.CmdLine <> '') then begin
                  Log( '     Value returned by GetProcCmdLine: ' +
                      InjectResult.CmdLine);
                end;
                Log( '     GetProcCurDir-function result: ' +
                    IntToStr(InjectResult.CurDirResult) + ' ' +
                    GetInjectResultDesc(InjectResult.CurDirResult));
                if (InjectResult.CurDirResult = 0)
                    and (InjectResult.CurDir <> '') then begin
                  Log( '     Value returned by GetProcCurDir: ' +
                      InjectResult.CurDir);
                end;
              end;
              Application.ProcessMessages;
              if Cancel then
                Exit;
              ProcObj.CmdLine := InjectResult.CmdLine;
              ProcObj.CurDir := InjectResult.CurDir;
              //end;
              if ProcObj.CmdLine = '' then
                ProcObj.CmdLine := ProcObj.ExeName;
              if ProcObj.CurDir = '' then
                ProcObj.CurDir := ExtractFileDir(ProcObj.ExeName);
            end;
            if ProcObj.Status = psRunning then begin
              lblCurrent.Caption := 'Trying to close ' + Item.Caption + '...';
              Application.ProcessMessages;
              if Mode <> wmDebug then begin
                if CloseProcess(ProcObj, Item) then
                  Item.SubItems[0] := 'Closed'
                else
                  Inc(NumNonClosed);
                {if CloseProcess(ProcObj, True) then begin
                  Item.SubItems[0] := 'Closed';
                  AddProgToSnap(ProcObj);
                end else
                  Inc(NumProgs);}
                Application.BringToFront;
                lblCount.Visible := False;
              end
              else
                AddProgToSnap(ProcObj);
              Step(1);
              if Cancel then
                Exit;
            end
            else if ProcObj.Status = psInvisible then
              Inc(NumNonVis);
            //Item.StateIndex := -1;
            Item.ImageIndex := -1;
          end;
          lstProgs.Selected := nil;
          RedrawSystray;
          //if LibH > 32 then
          //  FreeLibrary(LibH);
          Application.ProcessMessages;
          if Cancel then
            Exit;
          if Settings.ShowProgs then begin
            Sleep(500);
            ProgressNotebook.PageIndex := 0;
          end;
          //FreeProcObjs;
          Application.ProcessMessages;
          if Cancel then
            Exit;
        end;
        1: begin  // explorer windows
          lblCurrent.Caption := 'Closing all Explorer and Internet Explorer ' +
              'windows...';
          Application.ProcessMessages;
          if Cancel then
            Exit;
          //ExpWindows := GetExplorerWindows;
          {try
            if ProcList.Count = 0 then
              GetProcs(ProcList);
          except
            ProcList := TStringList.Create;
            GetProcs(ProcList);
          end;
          for j := 0 to ProcList.Count - 1 do begin
            ProcObj := TProcWinObj(ProcList.Objects[j]);
            if ProcObj.FileNameIs('EXPLORER.EXE')
                or ProcObj.FileNameIs('IEXPLORE.EXE') then
              EnumHwnds(ProcObj.ProcID, ExpClose);
          end;}
          if ExpWindows = nil then
            ExpWindows := TStringList.Create;
          // TODO: uncomment multimon code?
          {if Monitors = nil then
            Monitors := TList.Create;
          GetDisplayMonitors(Monitors);}
          //ExpWinHandle := 0;
          for k := 0 to ExpWindows.Count - 1 do try
            ExpWinHandle := StrToIntDef(ExpWindows.Strings[k], 0);
            if ExpWinHandle = 0 then
              Continue;
            WBWrapper := ExpWindows.Objects[k] as TWebBrowserWrapper;
            {IEFullScreen := False;
            IEMonitorIdx := 0;}
            //if IEObj = nil then
            //  Continue;
            try
              //ExpWinHandle := IEObj.WebBrowser2.Get_HWND;
              // TODO: uncomment multimon code?
              {if IEObj <> nil then begin
                IEFullScreen := IEObj.WebBrowser2.FullScreen
                    or IEObj.WebBrowser2.TheaterMode;
                if IsWindow(IEObj.WebBrowser2.Get_HWND) then begin
                  IEMonitorIdx :=
                      Monitors.IndexOf(Pointer(MonitorFromWindow(IEObj.WebBrowser2.Get_HWND,
                      MONITOR_DEFAULTTOPRIMARY)));
                  if IEMonitorIdx = -1 then
                    IEMonitorIdx := 0;
                end;
              end;}
              GetExplorerWindowPath(ExpWinHandle, IsIEWindow(ExpWinHandle),
                  WBWrapper, Path);
              Application.BringToFront;
              {IEWinData := IntToStr(Integer(IEFullScreen)) + ','
                  + IntToStr(IEMonitorIdx);}
              if Mode <> wmDebug then begin
                if CloseExpWindow(WBWrapper, ExpWinHandle) then begin
                  Tmp := 'successful';
                  {AddToBuffer2(rtExpWindow, Path.Location,
                      IntToStr(Integer(Path.WindowType)), IEWinData);}
                  AddToSnapBuffer(rtExpWindow, Path.Location,
                      IntToStr(Integer(Path.WindowType)));
                end
                else
                  Tmp := 'unsuccessful';
                Log('     Closing of Explorer window ' + IntToStr(ExpWinHandle) +
                    ' was ' + Tmp);
              end
              else begin
                {AddToBuffer2(rtExpWindow, Path.Location,
                    IntToStr(Integer(Path.WindowType)), IEWinData);}
                AddToSnapBuffer(rtExpWindow, Path.Location,
                      IntToStr(Integer(Path.WindowType)));
              end;
            finally
              if WBWrapper <> nil then
                WBWrapper.Free;
            end;
            Application.ProcessMessages;
            if Cancel then
              Exit;
          finally
            Step(1);
          end;
          //ExpWindows.Free;
        end;
        4: begin  // system snapshot
          lblCurrent.Caption := 'Creating and saving System Snapshot file...';
          Application.ProcessMessages;
          if Cancel then
            Exit;
          SnapFile := GetSnapFile;
          Log('Writing system snapshot data to ' + SnapFile);
          AddSnapInfo;
          Step(1);
          if Cancel then
            Exit;
          CopySnapData(SnapFile);
          Step(2);
          if Cancel then
            Exit;
          SetKeyValue(Hive, RegKey, 'LastSnapFile', SnapFile);
        end;
      end;

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

      if lvTasks.Items.Item[4].Checked then
        k := 2
      else
        k := 1;
      if i = TaskLabels.Count - k then begin
        {if (Mode <> Debug) and (NumProgs > 0) then begin
          Log('Number of programs that haven''t closed yet: ' + IntToStr(NumProgs));
          lblCurrent.Caption := 'Waiting for programs to finish closing...';
          lblCount.Caption := '';
          Application.ProcessMessages;
          Sleep(250);
          tmrCloseTime.Tag := NumProgs * Settings.CloseTime;
          if tmrCloseTime.Tag > 0 then begin
            tmrCloseTime.Enabled := True;
            tmrCloseTimeTimer(Self);
            while tmrCloseTime.Enabled do begin
              Application.ProcessMessages;
              if Cancel then
                Exit;
            end;
          end;
          lblCount.Visible := False;
          Application.ProcessMessages;
        end;}
        if Settings.TerminateNonVis then
          Inc(NumNonClosed, NumNonVis);
        if (Mode <> wmDebug) and (NumProgs > 0) then begin
          Log('Number of programs that haven''t closed yet: ' +
              IntToStr(NumNonClosed));
          Log('Number of running, invisible programs: ' + IntToStr(NumNonVis));
          Tmp := '';
          if Settings.TerminateNonVis or Settings.TerminateCloseFail then
            Tmp := '(and possibly terminating) ';
          lblCurrent.Caption := 'Checking state of ' + Tmp +
              'programs that are still running...';
          Application.ProcessMessages;
          if Cancel then
            Exit;
          if Settings.ShowProgsTerm then begin
            Sleep(500);
            ProgressNotebook.PageIndex := 1;
            //ActiveControl := lstProgs;
          end;
          Application.ProcessMessages;
          if Cancel then
            Exit;
          for j := 0 to lstProgs.Items.Count - 1 do begin
            Item := lstProgs.Items[j];
            try
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
              //if ProcObj.Status = psClosed then
              //  Continue;
              if IsProcessClosed(ProcObj) then begin
                Item.SubItems[0] := 'Closed';
                AddProgToSnap(ProcObj);
                Continue;
              end;
              if Settings.TerminateCloseFail
                  or (Settings.TerminateNonVis
                  and (ProcObj.Status = psInvisible)) then begin  // terminate
                //lblCurrent.Caption := 'Terminating ' + Item.SubItems[0] + '...';
                Application.ProcessMessages;
                if Cancel then
                  Exit;
                if Mode <> wmDebug then begin
                  if KillProcess(ProcObj, LibH) then begin
                    Item.SubItems[0] := 'Killed';
                    AddProgToSnap(ProcObj);
                  end;
                  Application.ProcessMessages;
                  if Cancel then
                    Exit;
                end;
              end;
            finally
              //Item.StateIndex := -1;
              Item.ImageIndex := -1;
            end;
          end;
          lstProgs.Selected := nil;
          RedrawSystray;
          if Settings.ShowProgsTerm then begin
            Sleep(500);
            ProgressNotebook.PageIndex := 0;
          end;
          Application.ProcessMessages;
          if Cancel then
            Exit;
        end;
      end;
      LogLine;
      Sleep(250);

    end;


    CancelButton.Enabled := False;

    lblCurrent.Caption := 'Done';
    Application.ProcessMessages;
    Sleep(500);
    Result := True;
    NextStep(CurStep + 1, False);

  finally
    tmrCloseTime.Enabled := False;
    if Svcs <> nil then
      FreeAndNil(Svcs);
    if SvcNames <> nil then
      FreeAndNil(SvcNames);
    if ExpWindows <> nil then
      FreeAndNil(ExpWindows);
    if ProcList <> nil then
      FreeAndNil(ProcList);
    //if Monitors <> nil then
    //  FreeAndNil(Monitors);
    FreeProcObjs;
    if LibH > 32 then begin
      FreeLibrary(LibH);
      DeleteFile(DllPath);
    end;
    FreeSnapIni;
  end;
end;

function TfrmCloseWiz.GetSnapFile: String;
begin
  case CurLocation of
    0: Result := SnapPath + '\Default' + SnapshotExt;
    1: Result := SnapPath + '\' + txtName.Text + SnapshotExt;
    2: Result := txtFilename.Text;
  end;
end;

procedure TfrmCloseWiz.Step(Num: Integer);
var
  i: Integer;
begin
  for i := 1 to Num do begin
    CurrentProgress.StepIt;
    TotalProgress.StepIt;
  end;
  Application.ProcessMessages;
end;

procedure TfrmCloseWiz.NextStep(Step: Integer; Back: Boolean);
var
  SkipPage: Integer;
begin
  {$IFDEF Debug}
  OutputDebugString('NextStep enter');
  {$ENDIF}
  if Step > 1 then begin
    if CheckData(Step - 1) = False then
      Exit;
  end;
  if Back then
    SkipPage := -1
  else
    SkipPage := 1;
  if ((Step = 3) and (lvTasks.Items.Item[0].Checked = False))
      or ((Step = 4) and (lvTasks.Items.Item[4].Checked = False)) then begin
    NextStep(Step + SkipPage, Back);
    Exit;
  end;
  CurStep := Step;
  if Step = 2 then
    tmrCapture.Enabled := True
  else
    tmrCapture.Enabled := False;
  if (not PageLoaded[Step]) and (Back = False) then
    LoadSettings(Step);
  if Step = 5 then
    LoadSummary;
  if Step = 6 then
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
  else begin
    if Step = Notebook2.Pages.Count + 2 then begin
      Notebook1.PageIndex := 2;
      ActiveControl := NextButton;
    end
    else begin
      lblCaption.Caption := Captions[Step - 1];
      lblDescription.Caption := Descriptions[Step - 1];
      Notebook1.PageIndex := 1;
      NoteBook2.PageIndex := Step - 2;
      case Step of
        2: ActiveControl := lvTasks;
        3: ActiveControl := lvProgs;
        4: begin
          if optDefault.Checked then
            ActiveControl := optDefault
          else if optName.Checked then
            ActiveControl := txtName
          else if optFilename.Checked then
            ActiveControl := txtFileName
        end;
        5: ActiveControl := NextButton;
        6: begin
          ActiveControl := CancelButton;
          StartProcess;
        end;
      end;
    end;
  end;
  {$IFDEF Debug}
  OutputDebugString('NextStep exit');
  {$ENDIF}
end;

procedure TfrmCloseWiz.CancelButtonClick(Sender: TObject);
begin
  //SaveSettings(4);
  Close;
end;

procedure TfrmCloseWiz.FormCreate(Sender: TObject);
var
  GIF: TGIFImage;
  i: Integer;
  Bmp: TBitmap;
  Ico: TIcon;
  BaseUnitX, BaseUnitY: Integer;
begin
  {$IFDEF Debug}
  OutputDebugString('closewiz create enter');
  {$ENDIF}
  //InitFormFont(Self);
  SetFormFont(Self, BaseUnitX, BaseUnitY);
  SetFontNameSize(WelcomeLabel1.Font, FormFont, 12, '', 12);
  lblCaption.Font.Style := [fsBold];
  lblTaskTitle.Font.Style := [fsBold];
  optDefault.Font.Style := [fsBold];
  optName.Font.Style := [fsBold];
  optFilename.Font.Style := [fsBold];
  lblCount.Font.Color := clRed;
  lblDebug.Font.Color := clRed;
  SetFontNameSize(lblDetails.Font, FormFont, 8, '', 8);
  WelcomeLabel2.Caption := WelcomeText;
  WelcomeLabel3.Caption := WelcomeText2;
  frmCloseWiz.Icon.ReleaseHandle;
  frmCloseWiz.Icon.Handle := LoadIcon(HInstance, PChar(1));
  for i := 1 to TotalSteps do
    PageLoaded[i] := False;
  LastWizKey := RegKey + '\LastWiz';
  GIF := TGIFImage.Create;
  try
    {GIF.Parent := paPicture;
    GIF.Align  := alClient;
    GIF.Center := True;
    GIF.Stretch := True;}
    GIF.LoadFromResourceName(HINSTANCE, 'CloseBig');
    imgPicture.Picture.Assign(GIF);
    {GIF.Parent := paSmallPicture;
    GIF.Align  := alClient;
    GIF.Center := True;
    //GIF.Stretch := True;}
    GIF.LoadFromResourceName(HINSTANCE, 'CloseSmall');
    imgSmall.Picture.Assign(GIF);
  finally
    GIF.Free;
  end;
  imgTasks.Picture.Icon.ReleaseHandle;
  imgTasks.Picture.Icon.Handle := LoadIcon(HInstance, PChar(16));
  ConvertTo32BitImageList(imgsTasks);
  ConvertTo32BitImageList(imgsTasksLarge);
  ConvertTo32BitImageList(imgsProgs);
  Ico := TIcon.Create;
  try
    imgsTasks.AddIcon(Icon);
    imgsTasksLarge.AddIcon(Icon);
    Ico.Handle := LoadIcon(hInstance, PChar(11));
    imgsTasks.AddIcon(Ico);
    imgsTasksLarge.AddIcon(Ico);
    Ico.Handle := LoadIcon(hInstance, PChar(12));
    imgsTasks.AddIcon(Ico);
    imgsTasksLarge.AddIcon(Ico);
    if IsWinNT then
      Ico.Handle := LoadIcon(hInstance, PChar(13))
    else
      Ico.Handle := LoadIcon(hInstance, PChar(14));
    imgsTasks.AddIcon(Ico);
    imgsTasksLarge.AddIcon(Ico);
    Ico.Handle := LoadIcon(hInstance, PChar(5));
    imgsTasks.AddIcon(Ico);
    imgsTasksLarge.AddIcon(Ico);
  finally
    Ico.Free;
  end;
  imgProtect.Picture.Icon.ReleaseHandle;
  imgProtect.Picture.Icon.Handle := LoadIcon(HInstance, PChar(17));
  Ico := TIcon.Create;
  try
    Ico.Handle := LoadIcon(hInstance, PChar(6));
    imgsProgs.AddIcon(Ico);
    Ico.Handle := LoadIcon(hInstance, PChar(19));
    imgsProgs.AddIcon(Ico);
    Ico.Handle := LoadIcon(hInstance, PChar(20));
    imgsProgs.AddIcon(Ico);
    Ico.Handle := LoadIcon(hInstance, PChar(7));
    imgsProgs.AddIcon(Ico);
  finally
    Ico.Free;
  end;
  imgSnap.Picture.Icon.ReleaseHandle;
  imgSnap.Picture.Icon.Handle := LoadIcon(HInstance, PChar(5));
  icoProcess.LoadFromResourceID(1, 48);
  Bmp := TBitmap.Create;
  try
    Bmp.LoadFromResourceName(hInstance, 'ARROW');
    imgsStates.AddMasked(Bmp, clFuchsia);
  finally
    Bmp.Free;
  end;
  lvProgs.SaveSettings.RegistryKey := LastWizKey + '\lvProgs';
  TaskLabels := TList.Create;
  LoadCloseSettings(Settings);
  chkNoIntro.Checked := Settings.NoIntro;
  PageLoaded[1] := True;
  SaveDialog.DefaultExt := SnapshotExt;
  SaveDialog.Filter := 'SmartClose System Snapshot (*' + SnapshotExt + ')|*' +
      SnapshotExt;
  lblDetails.Cursor := crHand;
  {$IFDEF Debug}
  OutputDebugString('closewiz create exit');
  {$ENDIF}
end;

procedure TfrmCloseWiz.ParseParams;
var
  i: Integer;
begin
  for i := 0 to High(Tasks) do begin
    with lvTasks.Items.Add do begin
      if (i = 3) and IsWinNT then
        Caption := ServicesName
      else
        Caption := Tasks[i];
      ImageIndex := i;
    end;
  end;
  if chkNoIntro.Checked then
    NextStep(2, False)
  else
    NextStep(1, False);
end;

procedure TfrmCloseWiz.NextButtonClick(Sender: TObject);
begin
  if CurStep = TotalSteps then
    Close
  else
    NextStep(CurStep + 1, False);
end;

procedure TfrmCloseWiz.BackButtonClick(Sender: TObject);
begin
  NextStep(CurStep - 1, True);
end;

procedure TfrmCloseWiz.FormCloseQuery(Sender: TObject; var CanClose: Boolean);
begin
  if CurStep < TotalSteps - 1 then begin
    if MsgBox('Are you sure you want to exit the SmartClose Close Programs ' +
        'Wizard?', bgYesNo, miQuestion, 'Exit Wizard?', 0) = IDNO then
      CanClose := False;
  end
  else if (Notebook1.ActivePage = 'Main') and (Notebook2.ActivePage = 'Process')
      and (Cancel = False) then begin
    if MsgBox('Are you sure you want to cancel the Close Programs Process and ' +
        'exit the SmartClose Close Programs Wizard?', bgYesNo, miWarning,
        'Exit Wizard?', 0) = IDNO then
      CanClose := False
    else
      Cancel := True;
  end
  {else begin
    if chkScandisk.Checked then
      ExecuteFile(AddBackslash(GetWindir) + Scandisk, '', GetWinDir, SW_SHOWNORMAL)
    else if chkDefrag.Checked then
      ExecuteFile(AddBackslash(GetWindir) + Defrag, '', GetWinDir, SW_SHOWNORMAL);
  end;}
end;

procedure TfrmCloseWiz.FormClose(Sender: TObject; var Action: TCloseAction);
var
  AutoExit: Boolean;
begin
  FreeAndNil(WaitingThreads);
  //ReleaseMutex(CloseMutexHandle);
  AutoExit := Settings.AutoExit and (CurStep = TotalSteps);
  if (frmMain <> nil) and not AutoExit then
    frmMain.Show
  else
    Application.Terminate;
end;

procedure TfrmCloseWiz.lvProgsClick(Sender: TObject);
begin
  if (lvProgs.Selected = nil)
      or (TProtectType(lvProgs.Selected.ImageIndex) = ptCritical) then begin
    btnEdit.Enabled := False;
    btnRemove.Enabled := False;
  end
  else begin
    btnEdit.Enabled := True;
    btnRemove.Enabled := True;
  end;
end;

procedure TfrmCloseWiz.btnAddClick(Sender: TObject);
var
  Typ: TProtectType;
begin
  Application.CreateForm(TfrmProProg, frmProProg);
  frmProProg.Caption := 'Add New Protected Program';
  LoadCurProgs(lcpmProProgCombobox, nil);
  if frmProProg.ShowModal = mrOK then begin
    if frmProProg.chkUnsafe.Checked then
      Typ := ptUnsafe
    else if frmProProg.chkTemp.Checked then
      Typ := ptTemp
    else
      Typ := ptNormal;
    AddProProg(AddProtectedProg(ProProgs, frmProProg.cmbProProg.Text, Typ,
        True), True);
    ActiveControl := lvProgs;
  end;
end;

procedure TfrmCloseWiz.lvProgsChange(Sender: TObject; Item: TListItem;
    Change: TItemChange);
begin
  lvProgsClick(Self);
end;

procedure TfrmCloseWiz.btnEditClick(Sender: TObject);
var
  Typ: TProtectType;
begin
  if TProtectType(lvProgs.Selected.ImageIndex) = ptUnsafe then begin
    if MsgBox('It is not recommended to edit the selected protected program ' +
        'because this entry is marked as unsafe. ' +
        'This means that closing or killing this program may cause your system ' +
        'to become unstable and/or crash.' + CrLf +
        'Are you sure you want to continue?', bgYesNo, miWarning,
        'Unsafe program warning', 0) = IDNO then
      Exit;
  end;
  Application.CreateForm(TfrmProProg, frmProProg);
  frmProProg.Caption := 'Edit Protected Program';
  LoadCurProgs(lcpmProProgCombobox, nil);
  frmProProg.cmbProProg.Text := lvProgs.Selected.Caption;
  frmProProg.chkUnsafe.Checked :=
      (TProtectType(lvProgs.Selected.ImageIndex) = ptUnsafe);
  frmProProg.chkTemp.Checked :=
      (TProtectType(lvProgs.Selected.ImageIndex) = ptTemp);
  if frmProProg.ShowModal = mrOK then begin
    ProProgs.Delete(ProProgs.IndexOf(lvProgs.Selected.Caption));
    lvProgs.Selected.Delete;
    if frmProProg.chkUnsafe.Checked then
      Typ := ptUnsafe
    else if frmProProg.chkTemp.Checked then
      Typ := ptTemp
    else
      Typ := ptNormal;
    AddProProg(AddProtectedProg(ProProgs, frmProProg.cmbProProg.Text, Typ,
        True), True);
    ActiveControl := lvProgs;
  end;
end;

procedure TfrmCloseWiz.btnRemoveClick(Sender: TObject);
var
  Warning: String;
  Style: TPJMsgDlgIconKind;
begin
  if TProtectType(lvProgs.Selected.ImageIndex) = ptUnsafe then begin
    Style := miWarning;
    Warning := 'Note: it is not recommended to remove the selected protected ' +
        'program because it is marked as unsafe. ' +
        'This means that closing or killing the program may cause your system ' +
        'to become unstable and/or crash.' + CrLf2;
  end
  else begin
    Style := miQuestion;
    Warning := '';
  end;
  if MsgBox(Warning + 'Are you sure you want to remove the entry ' +
      lvProgs.Selected.Caption + ' from the list of protected programs?',
      bgYesNo, Style, '', 0) = IDYES then begin
    ProProgs.Delete(ProProgs.IndexOf(lvProgs.Selected.Caption));
    lvProgs.Selected.Delete;
  end;
  ActiveControl := lvProgs;
end;

procedure TfrmCloseWiz.optLocationClick(Sender: TObject);
var
  i: Integer;
  Control: TRadioButton;
begin
  with Notebook2.Pages.Objects[2] as TPage do begin
    for i := 0 to ControlCount - 1 do
      if Controls[i] is TRadioButton then begin
        Control := TRadioButton(Controls[i]);
        if Controls[i] = Sender then
          Control.Font.Style := [fsBold]
        else
          Control.Font.Style := [];
    end;
  end;
  CurLocation := (Sender as TRadioButton).Tag;
  if UseClick then begin
    if Sender = optName then
      ActiveControl := txtName
    else if Sender = optFilename then
      btnBrowse.SetFocus;
  end;
end;

procedure TfrmCloseWiz.btnBrowseClick(Sender: TObject);
begin
  if txtFilename.Text <> '' then begin
    SaveDialog.InitialDir := '';
    SaveDialog.FileName := txtFilename.Text;
  end
  else
    SaveDialog.InitialDir := SnapPath;
  if SaveDialog.Execute then begin
    txtFilename.Text := SaveDialog.FileName;
    optFilename.Checked := True;
  end;
end;

procedure TfrmCloseWiz.txtNameChange(Sender: TObject);
begin
  optName.Checked := True;
end;

procedure TfrmCloseWiz.lblDetailsClick(Sender: TObject);
begin
  Application.CreateForm(TLogWindow, LogWindow);
  LogWindow.Caption := 'View Close Programs Log';
  LogWindow.txtLog.Text := LogFile;
  LogWindow.ShowModal;
end;

{procedure TfrmCloseWiz.chkScanDiskClick(Sender: TObject);
begin
  if UseClick then begin
    UseClick := False;
    //if chkDefrag.Checked then
    //  chkDefrag.Checked := False;
    UseClick := True;
  end;
end;}

{procedure TfrmCloseWiz.chkDefragClick(Sender: TObject);
begin
  if UseClick then begin
    UseClick := False;
    //if chkScandisk.Checked then
    //  chkScandisk.Checked := False;
    UseClick := True;
  end;
end;}

procedure TfrmCloseWiz.tmrCloseTimeTimer(Sender: TObject);
begin
  tmrCloseTime.Tag := tmrCloseTime.Tag - 1;
  if tmrCloseTime.Tag = 0 then
    tmrCloseTime.Enabled := False;
  lblCount.Visible := True;
  lblCount.Caption := IntToStr(tmrCloseTime.Tag);
end;

procedure TfrmCloseWiz.FormShow(Sender: TObject);
begin
  {$IFDEF Debug}
  OutputDebugString('closewiz show enter');
  {$ENDIF}
  Application.Title := Caption;
  Application.Icon := Self.Icon;
  {$IFDEF Debug}
  OutputDebugString('closewiz show exit');
  {$ENDIF}
end;

procedure TfrmCloseWiz.ShowTaskTip(Selection: Boolean);
var
  Idx: Integer;
  Text: String;
begin
  if Selection and (lvTasks.Selected <> nil) then begin
    Text := '';
    Idx := lvTasks.Selected.Index;
    if not TChkListItem(lvTasks.Selected).Enabled then
      case Idx of
        2: // screen saver
          Text := SSaverDisable;
        3: // task scheduler (or services)
          Text := IIf(IsWinNT, '' {ServiceDisable}, TaskSchedDisable);
      end;
    lblTip.Visible := False;
    imgsTasksLarge.GetIcon(Idx, imgTask.Picture.Icon);
    lblTaskTitle.Caption := lvTasks.Items[Idx].Caption;
    lblTaskDescription.Caption :=
        IIf((Idx = 3) and IsWinNT, ServicesDesc, TaskDescs[Idx]) +
        IIf(Text = '', '', CrLf + Text);
    lblTaskDescription.Visible := True;
    lblTaskTitle.Visible := True;
    imgTask.Visible := True;
  end
  else begin
    if lblTip.Visible then
      Exit;
    lblTaskDescription.Visible := False;
    lblTaskTitle.Visible := False;
    imgTask.Visible := False;
    lblTaskDescription.Caption := '';
    lblTaskTitle.Caption := '';
    imgTask.Picture := nil;
    lblTip.Visible := True;
  end;
end;

procedure TfrmCloseWiz.lvTasksHotTrack(Sender: TObject;
    var ItemIndex: Integer; SubItemIndex: Integer; Location: TPoint;
    var AllowSelect: Boolean);
begin
  if lvTasks.GetItemAt(Location.X, Location.Y) = nil then
    ShowTaskTip(False)
  {else if lvTasks.Selected = lvTasks.Items[ItemIndex] then
    ShowTaskTip(True);}
end;

procedure TfrmCloseWiz.tmrCaptureTimer(Sender: TObject);
var
  Point: TPoint;
begin
  if not GetCursorPos(Point) then
    Exit;
  if not UseKeyb and (WindowFromPoint(Point) <> lvTasks.Handle) then
    ShowTaskTip(False);
end;

procedure TfrmCloseWiz.lvTasksChange(Sender: TObject; Item: TListItem;
    Change: TItemChange);
var
  Pos: TPoint;
begin
  if Change = ctState then begin
    if UseKeyb then
      ShowTaskTip(True)
    else begin
      GetCursorPos(Pos);
      Pos := lvTasks.ScreenToClient(Pos);
      if (lvTasks.GetItemAt(Pos.X, Pos.Y) = Item)
          and (lvTasks.Selected = Item) then
        ShowTaskTip(True);
    end;
  end;
end;

procedure TfrmCloseWiz.FormDestroy(Sender: TObject);
begin
  FreeAndNil(TaskLabels);
  FreeAndNil(ProProgs);
end;

procedure TfrmCloseWiz.chkHideCriticalClick(Sender: TObject);
begin
  if UseClick then
    RefreshProProgs;
end;

function TfrmCloseWiz.lvProgsItemChecking(Sender: TObject;
    ItemIndex: Integer): Boolean;
var
  Item: TListItem;
begin
  Result := True;
  Item := lvProgs.Items[ItemIndex];
  if Item <> nil then begin
    if Item.Checked and (TProtectType(Item.ImageIndex) = ptUnsafe) then begin
      Result := False;
      if MsgBox('It is not recommended to disable the selected protected program ' +
          'because it is marked as unsafe. ' +
          'This means that closing or killing the program may cause your system ' +
          'to become unstable and/or crash.' + CrLf +
          'Are you sure you want to continue?', bgYesNo, miWarning,
          'Unsafe program warning', 0) = IDYES then
        Result := True;
    end;
    if Result then
      UpdateProProg(ProProgs.IndexOf(Item.Caption), not Item.Checked);
  end;
end;

procedure TfrmCloseWiz.lvProgsDblClick(Sender: TObject);
begin
  if btnEdit.Enabled then
    btnEditClick(Self);
end;

procedure TfrmCloseWiz.lvTasksKeyDown(Sender: TObject; var Key: Word;
    Shift: TShiftState);
begin
  if Key in [VK_PRIOR, VK_NEXT, VK_END, VK_HOME, VK_UP, VK_DOWN] then begin
    UseKeyb := True;
    //ShowTaskTip(True);
  end;
end;

procedure TfrmCloseWiz.lvTasksMouseMove(Sender: TObject;
    Shift: TShiftState; X, Y: Integer);
begin
  UseKeyb := False;
end;

end.
