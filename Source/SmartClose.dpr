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
 * Main project file
 *
 ****************************************************************************)

program SmartClose;

uses
  Forms,
  Windows,
  SysUtils,
  Messages,
  MyRegistry,
  NewDialogs,
  Main in 'Main.pas' {frmMain},
  Func in 'Func.pas',
  ProProg in 'ProProg.pas' {frmProProg},
  Wait in 'Wait.pas' {frmWait},
  Snapshot in 'Snapshot.pas',
  frmLog in 'frmLog.pas' {LogWindow},
  RestoreWiz in 'RestoreWiz.pas' {frmRestoreWiz},
  Restore in 'Restore.pas',
  Process in 'Process.pas',
  ExpWin in 'ExpWin.pas',
  frmResProg in 'frmResProg.pas' {ResProg},
  frmIEWindows in 'frmIEWindows.pas' {IEWindows},
  frmServices in 'frmServices.pas' {Services},
  Service in 'Service.pas',
  CMDUtil in 'CMDUtil.pas',
  frmConfig in 'frmConfig.pas' {Config},
  frmAbout in 'frmAbout.pas' {AboutFrm},
  CloseThread in 'CloseThread.pas',
  CloseWiz in 'CloseWiz.pas' {frmCloseWiz};

{$R *.RES}
{$R GIFRes32.res}
{$R XPAndUACManifest.res}

{procedure ActivatePrevInstance(hWnd: Integer);
var
  SWFlag: Integer;
  Foreground: Integer;
  CurThreadID: Integer;
  NextThreadID: Integer;
begin
  if IsIconic(hWnd) then
    SWFlag := SW_RESTORE
  else
    SWFlag := SW_SHOW;
  ShowWindow(hWnd, SWFlag);
  Foreground := GetForegroundWindow;
  if hWnd <> Foreground then begin
    CurThreadID := GetWindowThreadProcessId(Foreground, nil);
    NextThreadID := GetWindowThreadProcessId(hWnd, nil);
    if CurThreadID <> NextThreadID then begin
      AttachThreadInput(CurThreadID, NextThreadID, True);
      BringWindowToTop(hWnd);
      SetForegroundWindow(hWnd);
      AttachThreadInput(CurThreadID, NextThreadID, False);
    end else begin
      BringWindowToTop(hWnd);
      SetForegroundWindow(hWnd);
    end;
  end;
end;}

function GetAnimation: Boolean;
var
  Info: TAnimationInfo;
begin
  Info.cbSize := SizeOf(TAnimationInfo);
  if SystemParametersInfo(SPI_GETANIMATION, SizeOf(Info), @Info, 0) then
    Result := Info.iMinAnimate <> 0
  else
    Result := False;
end;

procedure SetAnimation(Value: Boolean);
var
  Info: TAnimationInfo;
begin
  Info.cbSize := SizeOf(TAnimationInfo);
  BOOL(Info.iMinAnimate) := Value;
  SystemParametersInfo(SPI_SETANIMATION, SizeOf(Info), @Info, 0);
end;

procedure ShowWinNoAnimate(Handle: HWnd; CmdShow: Integer);
var
  Animation: Boolean;
begin
  Animation := GetAnimation;
  if Animation then
    SetAnimation(False);
  ShowWindow(Handle, CmdShow);
  if Animation then
    SetAnimation(True);
end;

function EnumWinProc(H: HWND; AppHandle: HWND): Bool; stdcall;
begin
  Result := True;
  if (GetClass(H) = Application.ClassName) and (AnsiPos( 'SmartClose', WindowText(H)) > 0)
     and (H <> AppHandle) then begin
    SetForeGroundWindow(H);
    if IsIconic(H) then begin
      SetActiveWindow(H);
      ShowWinNoAnimate(H, SW_RESTORE);
      //RestoreTopMosts;
      if Screen.ActiveControl <> nil then
        Windows.SetFocus(Screen.ActiveControl.Handle);
    end;
    Result := False;
  end;
end;



var
  i: Integer;
  //Wnd: HWND;
  CmdTool: Boolean;
  hWizard: Integer;
  ShlWapiVer: Integer;
begin
  CmdTool := False;
  Application.Initialize;
  Application.Title := 'SmartClose';
  Application.HintHidePause := 30000;

  MutexHandle := CreateMutex(NIL, TRUE, 'SmartClose');
  if GetLastError = ERROR_ALREADY_EXISTS then begin
    CloseHandle(MutexHandle);
    {Wnd := GetWindow(GetDesktopWindow, GW_CHILD);
    while Wnd <> 0 do begin
      if (WindowText(Wnd) = Application.Title) and (GetClass(Wnd) = Application.ClassName)
        and (Wnd <> Application.Handle) then
        Break;
      Wnd := GetWindow(Wnd, GW_HWNDNEXT)
    end;}
    EnumWindows(@EnumWinProc, Application.Handle);
    //MsgBox( 'SmartClose is already running!', bgOK, miWarning, 'Warning', 0);
    Application.Terminate;
  end;

  if Win32Platform = VER_PLATFORM_WIN32s then begin
    MsgBox( 'SmartClose will not run on Win32s.', bgOK, miError, 'Not Compatible', 0);
    Exit;
  end;

  for i := 1 to ParamCount do begin
    if LowerCase(ParamStr(i)) = '/assoc' then begin
      RegSnapFileAssoc;
      Exit;
    end;
    if LowerCase(ParamStr(i)) = '/unassoc' then begin
      UnRegSnapFileAssoc;
      Exit;
    end;
    if LowerCase(ParamStr(i)) = '/removesnappath' then begin
      {hWizard := StrToIntDef(ParamStr(i + 1), 0);
      if not IsWindow(hWizard) then}
      hWizard := FindWindow( '#32770', 'SmartClose Uninstall');
      //simplemsg(IntToStr(hwizard), '');
      EnableWindow(hWizard, False);
      try
        if RemoveEmptySnapPath(SnapPath) then
          Exit;
        if MessageBox(hWizard, 'There are still one or more System Snapshot ' +
            'files located in the SmartClose snapshot folder.' + CrLf +
            'Do you want Setup to remove them now?',
            'Remove System Snapshot(s) ?',
            MB_YESNO or MB_ICONQUESTION or MB_APPLMODAL or MB_SETFOREGROUND) = IDYES then
          RemoveSnapFiles(hWizard, True);
        Exit;
      finally
        EnableWindow(hWizard, True);
      end;
    end;
  end;

  ShlWapiVer := GetDllVersion( 'shlwapi.dll');
  if ShlWapiVer < 470 then begin
    MsgBox( 'SmartClose requires at least version 3 of Internet Explorer to be installed.',
        bgOK, miError, 'Internet Explorer Version Error', 0);
    Exit;
  end;
  if (ShlWapiVer < 471) and not GetKeyBool(Hive, RegKey, 'NoIEVerWarning', False) then begin
    if MessageBox(Application.Handle,
        'You don''t have Internet Explorer version 4.0 or higher installed. ' +
        'It is not recommended to continue because SmartClose wasn''t designed ' +
        'to run on systems with lower Internet Explorer versions.' +
        CrLf + 'Are you sure you want to continue?' + CrLf2 +
        'Warning: if you do continue, SmartClose may cause unpredictable ' +
        'behaviour. Some features, such as the closing of Explorer windows will not work. ' +
        'It''s also possible that SmartClose will display errors or even crash.',
        'Internet Explorer Version Error', MB_YESNO or MB_ICONWARNING or MB_DEFBUTTON2) = IDNO then
      Exit;
  end;

  try
    if AnsiPos('/debug', CMDLine) > 0 then
      Mode := wmDebug;
    for i := 1 to ParamCount do begin
      if LowerCase(ParamStr(i)) = '/close' then
        CmdTool := CmdLineClose(False)
      else if CmdTool and (LowerCase(ParamStr(i)) = '/kill') then
        CmdTool := CmdLineClose(True);
      if LowerCase(ParamStr(i)) = '/closeprogs' then begin
        ShowCloseWiz;
        Exit;
      end else if LowerCase(ParamStr(i)) = '/restore' then begin
        ShowRestoreWiz;
        Exit;
      end else if LowerCase(ParamStr(i)) = '/config' then begin
        ShowConfig;
        Exit;
      end else if LowerCase(ParamStr(i)) = '/about' then begin
        ShowAbout;
        Exit;
      end;
    end;
    {$IFDEF Debug}
    outputdebugstring('end init');
    {$ENDIF}
    if CmdTool then begin
      try
        if AnsiPos( '/noresult', CMDLine) = 0 then
          CmdLineResult
      finally
        frmWait.Close;
        frmWait.Free;
      end;
    end
    else
      Application.CreateForm(TfrmMain, frmMain);
  finally
    {$IFDEF Debug}
    outputdebugstring('run');
    {$ENDIF}
    Application.Run;
  end;

end.
