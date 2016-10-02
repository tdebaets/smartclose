;****************************************************************************
;*
;*            SmartClose
;*
;*            Copyright (c) 2016 Tim De Baets
;*
;****************************************************************************
;*
;* Licensed under the Apache License, Version 2.0 (the "License");
;* you may not use this file except in compliance with the License.
;* You may obtain a copy of the License at
;*
;*     http://www.apache.org/licenses/LICENSE-2.0
;*
;* Unless required by applicable law or agreed to in writing, software
;* distributed under the License is distributed on an "AS IS" BASIS,
;* WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
;* See the License for the specific language governing permissions and
;* limitations under the License.
;*
;****************************************************************************
;*
;* Inno Setup install script
;*
;****************************************************************************

[Setup]
; NOTE: The value of AppId uniquely identifies this application.
; Do not use the same AppId value in installers for other applications.
AppID=SmartClose.{{7F22CBCB-92B5-4F5D-9A34-BB690215BEF2}
AppName=SmartClose
AppVerName=SmartClose 1.3
AppVersion=1.3
AppPublisher=BM-productions
AppPublisherURL={cm:URL}
AppSupportURL={cm:URL}
AppUpdatesURL={cm:URL}
DefaultDirName={pf}\SmartClose
DisableDirPage=auto
DefaultGroupName=SmartClose
AllowNoIcons=true
OutputDir=..\Output
OutputBaseFilename=SmartClose-1.3
AppCopyright=Copyright © 2010 BM-productions - All rights reserved
ChangesAssociations=false
AppMutex=SmartClose
DisableStartupPrompt=true
UninstallDisplayIcon={app}\SmartClose.exe
ShowTasksTreeLines=false
InfoBeforeFile=InfoBefore.rtf
Compression=lzma
PrivilegesRequired=admin
UsePreviousUserInfo=false
VersionInfoVersion=1.3
VersionInfoTextVersion=1.3
ShowLanguageDialog=no
LanguageDetectionMethod=none
InternalCompressLevel=normal
SolidCompression=false
WizardImageFile=compiler:WizModernImage-IS.bmp
WizardSmallImageFile=compiler:WizModernSmallImage-IS.bmp
DisableProgramGroupPage=auto
DisableWelcomePage=false
AlwaysShowDirOnReadyPage=true

[Tasks]
; Name: fileassoc; Description: &Associate system snapshot files (.scss) with SmartClose
Name: desktopicon; Description: {cm:CreateDesktopIcon}; GroupDescription: {cm:AdditionalIcons}; Flags: unchecked
Name: quicklaunchicon; Description: {cm:CreateQuickLaunchIcon}; GroupDescription: {cm:AdditionalIcons}; Flags: unchecked

[Files]
Source: "..\Output\SmartClose.exe"; DestDir: {app}; Flags: replacesameversion
Source: "..\LICENSE"; DestDir: "{app}"; Flags: ignoreversion
Source: "PSAPI.DLL"; DestDir: {sys}; Flags: restartreplace sharedfile uninsneveruninstall; OnlyBelowVersion: 1,5.0

[Dirs]
Name: {userappdata}\SmartClose; Tasks: 
Name: {userappdata}\SmartClose\Snapshots; Tasks: 

[Icons]
Name: "{group}\SmartClose   "; Filename: {app}\SmartClose.exe; IconIndex: 0; IconFilename: {app}\SmartClose.exe; Comment: {cm:MainComment}
Name: {group}\SmartClose - Close Programs; Filename: {app}\SmartClose.exe; IconIndex: 1; IconFilename: {app}\SmartClose.exe; Parameters: /closeprogs; Comment: {cm:CloseProgsComment}
Name: {group}\SmartClose - Restore Snapshot; Filename: {app}\SmartClose.exe; IconIndex: 2; IconFilename: {app}\SmartClose.exe; Parameters: /restore; Comment: {cm:RestoreComment}
Name: {group}\SmartClose - Settings; Filename: {app}\SmartClose.exe; IconIndex: 3; IconFilename: {app}\SmartClose.exe; Parameters: /config; Comment: {cm:SettingsComment}
Name: {group}\{cm:UninstallProgram,SmartClose}; Filename: {uninstallexe}; Comment: {cm:UninstallComment}
Name: {commondesktop}\SmartClose; Filename: {app}\SmartClose.exe; Tasks: desktopicon; IconIndex: 0; Comment: {cm:MainComment}
Name: {userappdata}\Microsoft\Internet Explorer\Quick Launch\SmartClose; Filename: {app}\SmartClose.exe; Tasks: quicklaunchicon; IconIndex: 0; Comment: {cm:MainComment}

[Run]
Filename: {app}\SmartClose.exe; Parameters: /UNASSOC; StatusMsg: Removing association of system snapshot files...; Check: RemoveAssoc
Filename: {app}\SmartClose.exe; Parameters: /ASSOC; StatusMsg: Associating system snapshot files with SmartClose...; Check: ShouldAssoc
;Filename: {app}\SmartClose.exe; Description: &Configure SmartClose now; Flags: postinstall skipifsilent; Parameters: /config
Filename: {app}\SmartClose.exe; Flags: nowait postinstall skipifsilent runascurrentuser; Description: {cm:LaunchNow}

[Registry]
Root: HKLM; Subkey: Software\BM-productions; ValueType: none; Flags: uninsdeletekeyifempty
Root: HKLM; Subkey: Software\BM-productions\SmartClose; Flags: uninsdeletekey; ValueType: none
; General settings:
Root: HKLM; Subkey: Software\BM-productions\SmartClose; ValueType: string; ValueName: InstallPath; ValueData: {app}
Root: HKLM; Subkey: Software\BM-productions\SmartClose; ValueType: string; ValueName: Version
Root: HKLM; SubKey: Software\BM-productions\SmartClose; ValueType: string; ValueName: SnapPath; ValueData: {userappdata}\SmartClose\Snapshots; Flags: createvalueifdoesntexist
; Close Programs:
Root: HKLM; SubKey: SOFTWARE\BM-productions\SmartClose; ValueType: dword; ValueName: NoIntro; ValueData: 0; Flags: createvalueifdoesntexist
Root: HKLM; SubKey: SOFTWARE\BM-productions\SmartClose; ValueType: dword; ValueName: ShowProgs; ValueData: 1; Flags: createvalueifdoesntexist
Root: HKLM; SubKey: SOFTWARE\BM-productions\SmartClose; ValueType: dword; ValueName: ShowProgsTerm; ValueData: 0; Flags: createvalueifdoesntexist
Root: HKLM; SubKey: SOFTWARE\BM-productions\SmartClose; ValueType: dword; ValueName: TerminateNonVis; ValueData: 1; Flags: createvalueifdoesntexist
Root: HKLM; SubKey: SOFTWARE\BM-productions\SmartClose; ValueType: dword; ValueName: TerminateCloseFail; ValueData: 0; Flags: createvalueifdoesntexist
Root: HKLM; SubKey: SOFTWARE\BM-productions\SmartClose; ValueType: dword; ValueName: AutoExit; ValueData: 0; Flags: createvalueifdoesntexist
; Root: HKLM; SubKey: SOFTWARE\BM-productions\SmartClose; ValueType: dword; ValueName: ProgCloseTime; ValueData: 1; Flags: createvalueifdoesntexist
; Restore:
Root: HKLM; SubKey: SOFTWARE\BM-productions\SmartClose; ValueType: dword; ValueName: Res_NoIntro; ValueData: 0; Flags: createvalueifdoesntexist
Root: HKLM; SubKey: SOFTWARE\BM-productions\SmartClose; ValueType: dword; ValueName: Res_AutoExit; ValueData: 0; Flags: createvalueifdoesntexist
Root: HKLM; SubKey: SOFTWARE\BM-productions\SmartClose; ValueType: dword; ValueName: ShowRestoreProgs; ValueData: 1; Flags: createvalueifdoesntexist
Root: HKLM; SubKey: SOFTWARE\BM-productions\SmartClose; ValueType: dword; ValueName: ProgsWait; ValueData: 1; Flags: createvalueifdoesntexist
Root: HKLM; SubKey: SOFTWARE\BM-productions\SmartClose; ValueType: dword; ValueName: UncheckProgs; ValueData: 0; Flags: createvalueifdoesntexist
Root: HKLM; SubKey: SOFTWARE\BM-productions\SmartClose; ValueType: dword; ValueName: AllowSS; ValueData: 0; Flags: createvalueifdoesntexist
Root: HKLM; SubKey: SOFTWARE\BM-productions\SmartClose; ValueType: dword; ValueName: AllowTaskSched; ValueData: 0; Flags: createvalueifdoesntexist

[UninstallRun]
Filename: {app}\SmartClose.exe; Parameters: /RemoveSnapPath; RunOnceId: RemoveSnapPath
Filename: {app}\SmartClose.exe; Parameters: /UNASSOC; RunOnceId: RemoveAssoc

[Messages]
ConfirmUninstall=Are you sure you want to completely remove %1 and all of its components?%n%nNote: It is not recommended to uninstall %1 before installing a newer version. If you do so, you will lose all your settings.

[UninstallDelete]
Name: {app}\Services.ini; Type: files
Name: {reg:HKLM\Software\BM-productions\SmartClose,SnapPath|{userappdata}\SmartClose\Snapshots}; Type: dirifempty

[CustomMessages]
URL=http://www.bm-productions.tk/
MainComment=Click here to run the Close Programs or the Restore Wizard, or to configure SmartClose.
CloseProgsComment=Click here to create a system snapshot and to close all running programs.
RestoreComment=Click here to restore a system snapshot that has been created with the Close Programs Wizard.
SettingsComment=Opens the SmartClose Settings window, that allows you to view and change all of SmartClose's settings.
UninstallComment=This will remove SmartClose from your system.
LaunchNow=&Launch SmartClose now
AssocCaption=Setup Association Options
AssocCaption1=Should Setup associate System Snapshots with SmartClose?
AssocDesc=SmartClose can save the current state of the system to a 'system snapshot' file before closing all programs and disabling anything. These system snapshot files have the ".scss" extension and can be associated with the SmartClose Restore Wizard, so they can be restored quickly when you open such a file in Windows Explorer.%nDo you want Setup to do this now?
AssocCheck=&Associate system snapshot files (*.scss) with SmartClose
AssocMemoTitle=Setup Association Options:
AssocMemo=Associate system snapshot files with SmartClose
IEURL=http://www.microsoft.com/windows/ie/downloads
ShlWapiWarning=Setup has detected that you don't have Internet Explorer version 4.0 or higher installed. SmartClose requires at least Internet Explorer 4.0 to function properly.%nDo you want to download and install a newer version of Internet Explorer now?%n%nChoose "Yes" to cancel Setup and go to the Internet Explorer download location (%1), "No" to cancel Setup, or "Cancel" to ignore this message and continue with installing SmartClose.%n

[Code]
var
  AssocScss: Boolean;
  chkAssoc: TCheckBox;

const
  AssocScssName = 'AssocScss';
  ReqShlWapiVersion = $40000 + 71; // requires version 4.71

function InitializeSetup: Boolean;
var
  VerMS, VerLS: Cardinal;
  ErrCode: Integer;
begin
  Result := True;
  GetVersionNumbers(ExpandConstant('{sys}') + '\shlwapi.dll', VerMS, VerLS);
  if VerMS < ReqShlWapiVersion then begin
    case MsgBox(ExpandConstant('{cm:ShlWapiWarning,{cm:IEURL}}'), mbError,
        MB_YESNOCANCEL) of
      IDYES: begin
        ShellExec('open', ExpandConstant('{cm:IEURL}'), '', '', SW_SHOWNORMAL,
            ewNoWait, ErrCode);
        //InstShellExec(ExpandConstant('{cm:IEURL}'), '', '', SW_SHOWNORMAL, ErrCode);
        Result := False;
      end;
      IDNO:
        Result := False;
      else
        Result := True;
    end;
  end;
  AssocScss := (GetPreviousData(AssocScssName, '1') = '1');
end;

procedure InitializeWizard;
var
  AssocPage: TWizardPage;
  lbl: TLabel;
begin
  AssocPage := CreateCustomPage(wpSelectProgramGroup,
      ExpandConstant('{cm:AssocCaption}'), ExpandConstant('{cm:AssocCaption1}'));
  { lbl }
  lbl := TLabel.Create(AssocPage);
  lbl.Left := 0;
  lbl.Top := 0;
  lbl.Width := AssocPage.SurfaceWidth;
  lbl.Height := ScaleY(70);
  lbl.AutoSize := False;
  lbl.Caption := ExpandConstant('{cm:AssocDesc}');
  lbl.WordWrap := True;
  lbl.Parent := AssocPage.Surface;
  { chkAssoc }
  chkAssoc := TCheckBox.Create(AssocPage);
  chkAssoc.Left := 0;
  chkAssoc.Top := lbl.Top + lbl.Height + ScaleY(10);
  chkAssoc.Width := ScaleX(313);
  chkAssoc.Height := ScaleY(17);
  chkAssoc.Caption := ExpandConstant('{cm:AssocCheck}');
  chkAssoc.Checked := AssocScss;
  chkAssoc.TabOrder := 0;
  chkAssoc.Parent := AssocPage.Surface;
end;

function RemoveAssoc: Boolean;
begin
  Result := False;
  //if (Pos('fileassoc', WizardSelectedTasks(False)) = 0) and Reinstall then
  if not chkAssoc.Checked and (WizardForm.PrevAppDir <> '') then
    Result := True;
end;

procedure RegisterPreviousData(PreviousDataKey: Integer);
var
  ValueData: String;
begin
  if chkAssoc.Checked then
    ValueData := '1'
  else
    ValueData := '0';
  SetPreviousData(PreviousDataKey, AssocScssName, ValueData);
end;

//function ScriptDlgPages(CurPage: Integer; BackClicked: Boolean): Boolean;
//var
//  Next: Boolean;
//  lbl: TLabel;
//  chkAssoc: TCheckBox;
//begin
//  if (not BackClicked and (CurPage = wpSelectProgramGroup)) or
//      (BackClicked and (CurPage = wpSelectTasks)) then begin
//    ScriptDlgPageOpen();
//    ScriptDlgPageSetCaption(ExpandConstant('{cm:AssocCaption}'));
//    ScriptDlgPageSetSubCaption2('');
//    ScriptDlgPageSetSubCaption1(ExpandConstant('{cm:AssocCaption1}'));
//    ScriptDlgPageClearCustom();
//
//    { lbl }
//    lbl := TLabel.Create(WizardForm.ScriptDlgPanel);
//    lbl.Parent := WizardForm.ScriptDlgPanel;
//    lbl.Left := 0;
//    lbl.Top := 0;
//    lbl.Width := 411;
//    lbl.Height := 69;
//    lbl.AutoSize := False;
//    lbl.Caption := ExpandConstant('{cm:AssocDesc}');
//    lbl.WordWrap := True;
//
//    { chkAssoc }
//    chkAssoc := TCheckBox.Create(WizardForm.ScriptDlgPanel);
//    chkAssoc.Parent := WizardForm.ScriptDlgPanel;
//    chkAssoc.Left := 0;
//    chkAssoc.Top := 80;
//    chkAssoc.Width := 313;
//    chkAssoc.Height := 17;
//    chkAssoc.Caption := ExpandConstant('{cm:AssocCheck}');
//    chkAssoc.Checked := AssocScss;
//    //chkAssoc.State := cbChecked;
//    chkAssoc.TabOrder := 0;
//
//    Next := ScriptDlgPageProcessCustom();
//    AssocScss := chkAssoc.Checked;
//    if not BackClicked then
//      Result := Next
//    else
//      Result := not Next;
//    ScriptDlgPageClose(not Result);
//  end else
//    Result := True;
//end;
//
//function NextButtonClick(CurPage: Integer): Boolean;
//begin
//  Result := ScriptDlgPages(CurPage, False);
//end;
//
//function BackButtonClick(CurPage: Integer): Boolean;
//begin
//  Result := ScriptDlgPages(CurPage, True);
//end;

function ShouldAssoc: Boolean;
begin
  Result := chkAssoc.Checked;
end;

function UpdateReadyMemo(Space, NewLine, MemoUserInfoInfo, MemoDirInfo,
    MemoTypeInfo, MemoComponentsInfo, MemoGroupInfo, 
    MemoTasksInfo: String): String;
var
  S: String;
begin
  S := '';
  if MemoDirInfo <> '' then begin
    S := S + MemoDirInfo + NewLine;
    S := S + NewLine;
  end;
  if MemoGroupInfo <> '' then begin
    S := S + MemoGroupInfo + NewLine;
    S := S + NewLine;
  end;
  if chkAssoc.Checked then begin
    S := S + ExpandConstant('{cm:AssocMemoTitle}') + NewLine;
    S := S + Space + ExpandConstant('{cm:AssocMemo}') + NewLine;
    S := S + NewLine;
  end;
  if MemoTasksInfo <> '' then
    S := S + MemoTasksInfo + NewLine;
  Result := S;
end;
