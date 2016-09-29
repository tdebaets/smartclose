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
 * Process-related functions
 *
 ****************************************************************************)

unit Process;

interface

uses Windows, Messages, SysUtils, MBCSUtil, MyRegistry, Classes, ComCtrls,
    TLHelp32, Forms, Controls, NewDialogs, ShellApi, Graphics, CommCtrl;

type
  //TWindows = array[0..4096] of Integer;
  TInjectResult = record
    Result: Integer;
    CmdLineResult: Integer;
    CmdLine: String;
    CurDirResult: Integer;
    CurDir: String;
  end;
  TEnumhWndsAction = (ehaGetCount, ehaNormalClose);
  TProcObjStat = (psRunning, psProtected, psInvisible, psClosed, psTerminated);
  TProtectType = (ptNormal, ptCritical, ptUnsafe, ptTemp);

type
  TProcWinObj = class(TObject)
  private
    fProcID: DWORD;   // process ID
    fExeName: String; // executable filename
    fDispName: String;
    fCmdLine: String;
    fCurDir: String;
    fStatus: TProcObjStat;
  public
    constructor Create(ProcID: DWORD; const ExeName: String);
    function FileNameIs(const S: String): Boolean;
  published
    property ExeName: String read fExeName;
    property DispName: String read fDispName;
    property ProcID: DWORD read fProcID;
    property CmdLine: String read fCmdLine write fCmdLine;
    property CurDir: String read fCurDir write fCurDir;
    property Status: TProcObjStat read fStatus write fStatus;
  end;

const
  CloseTimeout = 20;
  SYSAGENT_ENABLED = 4;
  SYSAGENT_DISABLED = 7;

procedure GetProcesses(Processes: TStrings);
function AddProtectedProg(ProProgs: TStrings; Filename: String;
    Typ: TProtectType; Enabled: Boolean): Integer;
function LoadProtectedProgs(OnlyCritical: Boolean): TStringList;

type
  TLoadCurProgsMode = (lcpmProProgCombobox, lcpmCloseWizProgList);
  
procedure LoadCurProgs(Mode: TLoadCurProgsMode; Processes: TStringList);
function EnumhWnds(ProcID: DWORD; Action: TEnumHwndsAction;
    OnDone, OnCount: TNotifyEvent): Integer;
function GetProcessTitle(Filename: String): String;
function GetNumProcesses(ProcList: TStrings; IncludeProtected: Boolean): Cardinal;
function IsProtectedProcess(Name: String): Boolean;
function IsProcessClosed(ProcObj: TProcWinObj): Boolean;
function SysAgent_GetStat: DWORD;
function SysAgent_SetStatus(Enable: Boolean): DWORD;
function SysAgent_IsAvailable: Boolean;
function GetIsScreenSaveActive: LongBool;
//function LoadRTDLL(var DLLPath: String): HMODULE;
function InjectProcess(PID: DWORD; Terminate: Boolean;
    hInjectLib: HMODULE): TInjectResult;
function KillProcess(PID: DWORD): Integer;
procedure RedrawSystray;

// The NewState parameter isn't optional in the original AdjustTokenPrivileges
// declaration
function AdjustTokenPrivileges2(TokenHandle: THandle; DisableAllPrivileges: BOOL;
    const NewState: TTokenPrivileges; BufferLength: DWORD;
    PreviousState: PTokenPrivileges; var ReturnLength: DWORD): BOOL; stdcall;

type
  TProcWaitThread = class(TThread)
  private
    FhProc: THandle;
    FIndex: Integer;
    FSecs: Integer;
    FCloseTimeout: Cardinal;
    FExitCode: Cardinal;
    FOnLoaded: TNotifyEvent;
    FRetVal: Integer;
    FData: Pointer;
    procedure SyncLoaded;
  public
    constructor Create(Suspended: Boolean; hProc: THandle; Index: Integer;
        Seconds: Cardinal; CloseTimeout: Cardinal;
        OnLoaded, OnClosed: TNotifyEvent);
    procedure Execute; override;
    property Index: Integer read FIndex;
    property ExitCode: Cardinal read FExitCode;
    property RetVal: Integer read FRetVal;
    property Data: Pointer read FData write FData;
  end;

var
  hStopEvent: THandle;

implementation

uses Func, CloseWiz, CloseThread, ProProg, Processes;

const
  SYSAGENT_GETSTATUS = WM_USER + 200;
  SYSAGENT_ENABLE = WM_USER + 203;
  SYSAGENT_DISABLE = WM_USER + 202;

type
  PHINST = ^HINST;
  PCmdLine = ^TCmdLine;
  TCmdLine = array[0..2000] of Char;
  TGetCommandLine = function: PCmdLine; stdcall;
  TInjectCmdLine = packed record
    GetCommandLine: TGetCommandLine;
    CmdLine: TCmdLine;
    InjectInfoCode: array[0..100] of Byte;
  end;
  PInjectCmdLine = ^TInjectCmdLine;
  TGetCurrentDir = function(nSize: DWord; lpDirectory: PChar): DWord; stdcall;
  TInjectCurDir = packed record
    GetCurrentDir: TGetCurrentDir;
    CurrentDir: array[0..MAX_PATH] of Char;
    InjectInfoCode: array[0..100] of Byte;
  end;
  PInjectCurDir = ^TInjectCurDir;
  TEnvironmentDatabase = packed record
    Environment: PChar;
    unl: DWORD;
    CmdLine: PChar;
    CurrDirectory: PChar;
  end;
  PEnvironmentDatabase = ^TEnvironmentDatabase;
  TProcessDatabase = packed record
    Typ: DWORD;                       // 00h KERNEL32 object type (5)
    cReference: DWORD;                // 04h Number of references to process
    un1: DWORD;                       // 08h
    someEvent: DWORD;                 // 0Ch An event object
    TerminationStatus: DWORD;         // 10h Returned by GetExitCodeProcess
    un2: DWORD;                       // 14h
    DefaultHeap: DWORD;               // 18h Address of the process heap
    MemoryContext: DWORD;             // 1Ch pointer to the process's context
    flags: DWORD;                     // 20h
    pPSP: DWORD;                      // 24h Linear address of PSP?
     PSPSelector: WORD;               // 28h
     MTEIndex: WORD;                  // 2Ah
     cThreads: WORD;                  // 2Ch
     cNotTermThreads: WORD;           // 2Eh
     un3: WORD;                       // 30h
     cRing0Threads: WORD;             // 32h number of ring 0 threads
    HeapHandle: THandle;              // 34h Heap to allocate handle tables out of
                                      // This seems to always be the KERNEL32 heap
    W16TDB: DWORD;                    // 38h Win16 Task Database selector
    MemMapFiles: DWORD;               // 3Ch memory mapped file list (?)
    pEDB: PEnvironmentDatabase;       // 40h Pointer to Environment Database
  end;
  PProcessDatabase = ^TProcessDatabase;

var
  Obsfucator: Cardinal = 0;
  xCreateRemoteThread: function(hProcess: THandle; lpThreadAttributes: Pointer;
      dwStackSize: DWORD; lpStartAddress: Pointer; lpParameter: Pointer;
      dwCreationFlags: DWORD; lpThreadId: Pointer): DWORD; stdcall = nil;
  xVirtualAllocEx: function(hProcess: THandle; lpAddress: Pointer; dwSize,
      flAllocationType: DWORD; flProtect: DWORD): Pointer; stdcall = nil;
  xVirtualFreeEx: function(hProcess: THandle; lpAddress: Pointer; dwSize,
      dwFreeType: DWORD): Pointer; stdcall = nil;
  FuncLoaded: Boolean = False;

function AdjustTokenPrivileges2; external advapi32 name 'AdjustTokenPrivileges';

constructor TProcWinObj.Create(ProcID: DWORD; const ExeName: String);
begin
  Inherited Create;
  fExeName := LowerCase(ExeName);
  fDispName := ExtractFileName(fExeName);
  fProcID := ProcID;
end;

function TProcWinObj.FileNameIs(const S: String): Boolean;
begin
  Result := (AnsiCompareText(fDispName, S) = 0);
end;

function AddProtectedProg(ProProgs: TStrings; Filename: String;
    Typ: TProtectType; Enabled: Boolean): Integer;
var
  Info: Word;
begin
  WordRec(Info).Lo := Byte(Enabled);
  WordRec(Info).Hi := Byte(Typ);
  Result := ProProgs.AddObject(Filename, TObject(Info));
end;

function LoadProtectedProgs(OnlyCritical: Boolean): TStringList;
  procedure AddItem(Filename: String; Typ: TProtectType; Enabled: Boolean);
  begin
    AddProtectedProg(Result, FileName, Typ, Enabled);
  end;
const
  Critical: array[0..2] of String = ('EXPLORER.EXE', 'IEXPLORE.EXE', 'SYSTRAY.EXE');
  Critical9x: array[0..2] of String = ('KERNEL32.DLL', 'RNAAPP.EXE' , 'MSTASK.EXE');
  CriticalNT: array[0..7] of String = ('LSASS.EXE', 'SERVICES.EXE',
      'WINLOGON.EXE', 'SVCHOST.EXE', 'SMSS.EXE', 'CSRSS.EXE', 'WININIT.EXE',
      'LOGONUI.EXE');
var
  i: Integer;
  Keys: TStringList;
  Typ: TProtectType;
begin
  // TODO: check for special system PIDs: 0, 4, 8
  Result := TStringList.Create;
  Result.Sorted := True;
  AddItem(ExtractFileName(ParamStr(0)), ptCritical, True);
  for i := 0 to High(Critical) do
    AddItem(Critical[i], ptCritical, True);
  if IsWinNT then begin
    for i := 0 to High(CriticalNT) do
      AddItem(CriticalNT[i], ptCritical, True);
  end
  else begin
    for i := 0 to High(Critical9x) do
      AddItem(Critical9x[i], ptCritical, True);
  end;
  if not OnlyCritical then begin
    with TMyRegistry.Create do try
      RootKey := Hive;
      if OpenKey(RegKey + '\LastWiz\Protected', False) then begin
        Keys := TStringList.Create;
        try
          GetKeyNames(Keys);
          CloseKey;
          for i := 0 to Keys.Count - 1 do begin
            if OpenKey(RegKey + '\LastWiz\Protected\' + Keys[i], False) then begin
              if ReadBoolDef('Unsafe', False) then
                Typ := ptUnsafe
              else
                Typ := ptNormal;
              AddItem(Keys[i], Typ, ReadBoolDef('Checked', False));
              CloseKey;
            end;
          end;
        finally
          Keys.Free;
        end;
      end;
    finally
      Free;
    end;
  end;
end;

function ListWinProc(hWnd: HWND; hWnds: TList): BOOL; stdcall;
begin
  Result := True;
  if IsWindowVisible(hWnd) then
    hWnds.Insert(0, Pointer(hWnd))
  else
    hWnds.Add(Pointer(hWnd));
end;

function CloseWindowPhase2(hWnd: HWND): Boolean;
var
  MsgResult: DWORD;
begin
  Result := True;
  if not IsWindow(hWnd) then
    Exit;
  SendMessageTimeout(hWnd, WM_SYSCOMMAND, SC_CLOSE, 0, SMTO_ABORTIFHUNG, 100,
      MsgResult);
  Sleep(100);
  if not IsWindow(hWnd) then
    Exit;
  SendMessageTimeout(hWnd, WM_ENDSESSION, Integer(True),
      Integer(ENDSESSION_LOGOFF), SMTO_ABORTIFHUNG, 100, MsgResult);
  Sleep(100);
  if not IsWindow(hWnd) then
    Exit;
  Result := False;
end;

function EnumhWnds(ProcID: DWORD; Action: TEnumHwndsAction;
    OnDone, OnCount: TNotifyEvent): Integer;
var
  PID: DWORD;
  hWnds: TList;
  i: Integer;
  Windows: TList;
  hWin: HWND;
  AllWindowsClosed: Boolean;
begin
  Result := 0;
  hWnds := TList.Create;
  Windows := TList.Create;
  try
    try
      EnumWindows(@ListWinProc, Integer(hWnds));
      {hWin := GetWindow(GetDesktopWindow, GW_CHILD);
      while hWin <> 0 do begin
        GetWindowThreadProcessID(hWin, @PID);
        if ProcID = PID then begin;
          Windows[Result] := hWin;
          Inc(Result);
        end;
        hWin := GetWindow(hWin, GW_HWNDNEXT);
      end;}
      for i := 0 to hWnds.Count - 1 do begin
        GetWindowThreadProcessID(Integer(hWnds.Items[i]), @PID);
        if ProcID = PID then begin;
          Windows.Add(hWnds.Items[i]);
          Inc(Result);
        end;
      end;
    finally
      hWnds.Free;
    end;
    if Action = ehaNormalClose then begin
      if Result > 0 then begin
        for i := 0 to Result - 1 do begin
          hWin := Integer(Windows.Items[i]);
          if (hWin = 0) or not IsWindow(hWin) then
            Continue;
          IsWaiting := True;
          TCloseThread.Create(hWin, ProcID, CloseTimeout, OnDone, OnCount, False);
          while IsWaiting do
            Application.ProcessMessages;
          Sleep(500);
          //CloseWindow(hWin, False);
          if CloseWindowPhase2(hWin) then begin
            Log('     Closing of window with handle ' + IntToStr(hWin) +
                ' succeeded')
          end
          else begin
            Log('     Closing of window with handle ' + IntToStr(hWin) +
                ' failed');
          end;
        end;
        Sleep(500);
        AllWindowsClosed := True;
        for i := 0 to Result - 1 do begin
          if IsWindow(Integer(Windows.Items[i])) then begin
            AllWindowsClosed := False;
            Break;
          end;
        end;
        if AllWindowsClosed then
          Result := 1
        else
          Result := 0;
//        if OpenProcess(PROCESS_QUERY_INFORMATION or PROCESS_VM_READ, False,
//            ProcID) = 0 then
//          Result := 1;
      end
      else
        Log('     No windows that belong to this program were found');
    end;
  finally
    Windows.Free;
  end;
  {else if Action = ExpClose then begin
    Wins := 0;
    Done := 0;
    if Result = 0 then
      Exit;
    for i := 0 to Result - 1 do begin
      if not IsWindow(Window[i]) then
        Continue;
      if not IsExplorerWindow(Window[i]) then
        Continue;
      Inc(Wins);
      ExplorerWindowPath(Window[i], IsIEWindow(Window[i]), Path);
      if Mode = Debug then
        AddToBuffer(ExpWindow, Path.Location, IntToStr(Integer(Path.WindowType)))
      else
      begin
        Waiting := True;
        TCloThread.Create(Window[i], ProcID, 10, frmCloseWiz.thdDone, nil, True);
        while Waiting do
          Application.ProcessMessages;
        Sleep(100);
        if not IsWindow(Window[i]) then begin
          Inc(Done);
          Tmp := 'successful';
          AddToBuffer(ExpWindow, Path.Location, IntToStr(Integer(Path.WindowType)));
        end
        else
          Tmp := 'unsuccessful';
        Log('     Closing of Explorer window ' + IntToStr(Window[i]) + ' was ' + Tmp);
      end;
      frmCloseWiz.Step;
    end;
    Application.ProcessMessages;
    if Wins = Done then
      Result := 0
    else
      Result := 1;
  end;}
end;

procedure GetProcesses(Processes: TStrings);

  procedure GetListOfProcs9x;
  type
    TCreateToolhelp32Snapshot = function(dwFlags, th32ProcessID: DWORD): THandle;
        stdcall;
    TProcess32First = function(hSnapshot: THandle;
        var lppe: TProcessEntry32): BOOL; stdcall;
    TProcess32Next = function(hSnapshot: THandle;
        var lppe: TProcessEntry32): BOOL; stdcall;
  var
    PI32: TProcessEntry32;
    hSnap: THandle;
    hKernel: HMODULE;
    CreateToolhelp32Snapshot: TCreateToolhelp32Snapshot;
    Process32First: TProcess32First;
    Process32Next: TProcess32Next;
  begin
    hKernel := GetModuleHandle(kernel32);
    @CreateToolhelp32Snapshot := GetProcAddress(hKernel,
        'CreateToolhelp32Snapshot');
    @Process32First := GetProcAddress(hKernel, 'Process32First');
    @Process32Next := GetProcAddress(hKernel, 'Process32Next');
    if (@Process32First = nil) or (@Process32Next = nil) then
      Exit;
    hSnap := CreateToolHelp32Snapshot(TH32CS_SNAPPROCESS, 0);
    if hSnap = 0 then
      Exit;
    try
      PI32.dwSize := SizeOf(PI32);
      if Process32First(hSnap, PI32) then begin
        repeat
          with PI32 do begin
            if th32ProcessID <> 0 then begin
              Processes.AddObject(ExtractFileName(szExefile),
                  TProcWinObj.Create(th32ProcessID, szExefile));
              //SL.AddObject(IntToHex(th32ProcessID, 8),
              //    TProcWinObj.Create(th32ProcessID, szExefile));
            end;
          end;
        until not Process32Next(hSnap, PI32);
      end;
    finally
      CloseHandle(hSnap);
    end;
  end;

  procedure GetListOfProcsNT;
    procedure LoadPSAPIFunc(hPSAPI: HMODULE; const S: String; var P: Pointer);
    const
      NotFoundMsg = 'Function %s not found in PSAPI.DLL';
    begin
      P := GetProcAddress(hPSAPI, PChar(S));
      if P = nil then
        raise Exception.CreateFmt(NotFoundMsg, [S]);
    end;
  const
    MaxPIDCount = 65536;
  type
    TEnumProcessModules = function(hProcess: THandle; moduleList: PHINST;
        cb: Integer; var cbNeeded: Integer): Boolean; stdcall;
    TEnumProcesses = function(pidList: PInteger; cb: Integer;
        var cbNeeded: Integer): Boolean; stdcall;
    TGetModuleFileNameExA = function(hProcess: THandle; module: HINST;
        FileName: PChar; size: Integer): Integer; stdcall;
  var
    EnumProcesses: TEnumProcesses;
    EnumProcessModules: TEnumProcessModules;
    GetModuleFileNameExA: TGetModuleFileNameExA;
    hPSAPI: HMODULE;
    PID, i, cbNeeded, PIDCount{, ModuleCount}: Integer;
    PIDList: PInteger;
    ModuleList: PHINST;
    hProc: THandle;
    //Name: array[0..MAX_PATH - 1] of Char;
    FileName: String;
  begin
    PIDList := nil;
    ModuleList := nil;
    hPSAPI := LoadLibrary('psapi.dll');
    if hPSAPI = 0 then
      raise Exception.Create('PSAPI.DLL: ' + SysErrorMessage(GetLastError));
    LoadPSAPIFunc(hPSAPI, 'EnumProcessModules', @EnumProcessModules);
    LoadPSAPIFunc(hPSAPI, 'EnumProcesses', @EnumProcesses);
    LoadPSAPIFunc(hPSAPI, 'GetModuleFileNameExA', @GetModuleFileNameExA);
    ReallocMem(PIDList, MaxPIDCount);
    if not EnumProcesses(PIDList, MaxPIDCount, cbNeeded) then
      cbNeeded := 0;
    ReallocMem(PIDList, cbNeeded);
    PIDCount := cbNeeded div SizeOf(Integer);
    // Walk the list
    for i := 0 to Pred(PIDCount) do begin
      // Get PID
      PID := PInteger(PChar(PIDList) + i * SizeOf(Integer))^;
      if PID <> 0 then begin
        // Get Handle
        hProc := OpenProcess(PROCESS_QUERY_INFORMATION or PROCESS_VM_READ,
            False, PID);
        try
          if GetProcessFilename(PID, hProc, Filename) = 0 then begin
            Processes.AddObject(ExtractFileName(FileName),
                TProcWinObj.Create(PID, FileName));
          end;
          {// Get the Module list, if fact we only need the first module but ... who cares?
          //ModuleCount := 0;
          if hProc <> 0 then begin
            ReallocMem(ModuleList, 32);
            if not EnumProcessModules(hProc, ModuleList, 32, cbNeeded) then
              cbNeeded := 0;
            ReallocMem(ModuleList, cbNeeded);
            //ModuleCount := cbNeeded div SizeOf(HINST);
          end
          else
            ReallocMem(ModuleList, 0);
          // Get first Module instance
          if Assigned(ModuleList) then begin
            hInst := PHINST(PChar(ModuleList))^;
            if GetModuleFileNameExA(hProc, hInst, Name, SizeOf(Name)) <> 0 then begin
              FileName := StrPas(Name);
              SL.AddObject(ExtractFileName(FileName),
                  TProcWinObj.Create(PID, FileName));
              //SL.AddObject(IntToHex(PID, 8), TProcWinObj.Create(PID, FileName));
            end;
          end;}
        finally
          CloseHandle(hProc);
        end;
      end;
    end;
    // Free the lists
    ReallocMem(PIDList, 0);
    ReallocMem(ModuleList, 0);
  end;

begin
  Processes.Clear;
  // TODO: always use toolhelp as it gives better results
  if IsWinNT { and (Win32MajorVersion < 5)} then
    GetListOfProcsNT
  else
    GetListOfProcs9x;
end;

function GetProcessTitle(Filename: String): String;
begin
  if AnsiPos( '\', Filename) > 0 then
    Result := ExtractFileName(Filename)
  else
    Result := Filename;
  Result := UpperCase(Result[1]) + LowerCase(Copy(Result, 2, Length(Result) - 1));
end;

// TODO: remove Mode and split into 2 separate functions
procedure LoadCurProgs(Mode: TLoadCurProgsMode; Processes: TStringList);
var
  ProcList: TStringList;
  i, Idx: Longint;
  Tmp: String;
  Title: String;
  ProcObj: TProcWinObj;
  FileInfo: TSHFileInfo;
  Ico: TIcon;
begin
  Screen.Cursor := crHourGlass;
  case Mode of
    lcpmProProgCombobox: begin
      ShowWait(False, msgProgsLoad);
      frmProProg.Images.Clear;
      frmProProg.cmbProProg.Clear;
      Ico := TIcon.Create;
      try
        Ico.ReleaseHandle;
        Ico.Handle := LoadIcon(hInstance, PChar(6));
        frmProProg.Images.AddIcon(Ico);
      finally
        Ico.Free;
      end;
    end;
    lcpmCloseWizProgList:
      frmCloseWiz.lstProgs.Items.BeginUpdate;
  end;
  if Processes = nil then
    ProcList := TStringList.Create
  else
    ProcList := Processes;
  try
    ProcList.Sorted := True;
    if Processes = nil then
      GetProcesses(ProcList);
    for i := 0 to ProcList.Count - 1 do begin
      ProcObj := TProcWinObj(ProcList.Objects[i]);
      Title := GetProcessTitle(ProcObj.fExeName);
      case Mode of
        lcpmProProgCombobox: begin
          Idx := frmProProg.cmbProProg.Items.Add(Title);
          SHGetFileInfo(PChar(ProcObj.fExename), 0, FileInfo, SizeOf(FileInfo),
              SHGFI_ICON or SHGFI_SMALLICON);
          try
            if FileInfo.HIcon <> 0 then begin
              frmProProg.cmbProProg.ImageIndex[Idx] :=
                  ImageList_AddIcon(frmProProg.Images.Handle, FileInfo.HIcon);
            end
            else
              frmProProg.cmbProProg.ImageIndex[Idx] := 0;
          finally
            DestroyIcon(FileInfo.hIcon);
          end;
        end;
        lcpmCloseWizProgList: begin
          with frmCloseWiz.lstProgs.Items.Add do begin
            Tmp := 'Running';
            ProcObj.Status := psRunning;
            if IsProtectedProcess(Title) then begin
              Tmp := Tmp + '; Protected';
              ProcObj.Status := psProtected;
              Data := nil;
            end
            else begin
              if EnumHwnds(ProcObj.fProcID, ehaGetCount, nil, nil) = 0 then begin
                Tmp := Tmp + ' (invisible)';
                ProcObj.Status := psInvisible;
              end;
              Data := Pointer(ProcObj);
            end;
            Caption := Title;
            SubItems.Add(Tmp);
            ImageIndex := -1;
          end;
        end;
      end;
    end;
  finally
    if Processes = nil then
      FreeObjStrings(ProcList);
    case Mode of
      lcpmProProgCombobox:
        HideWait;
      lcpmCloseWizProgList: begin
        frmCloseWiz.lstProgs.Items.EndUpdate;
        frmCloseWiz.lstProgs.Columns[0].Width :=
            frmCloseWiz.lstProgs.Columns[0].Width + 30;
      end;
    end;
    Screen.Cursor := 0;
  end;
end;

function GetNumProcesses(ProcList: TStrings; IncludeProtected: Boolean): Cardinal;
var
  i: Cardinal;
  ProcObj: TProcWinObj;
  ProcTitle: String;
begin
  Result := 0;
  for i := 0 to ProcList.Count - 1 do begin
    if IncludeProtected then
      Inc(Result)
    else begin
      ProcObj := TProcWinObj(ProcList.Objects[i]);
      ProcTitle := GetProcessTitle(ProcObj.fExeName);
      if not IsProtectedProcess(ProcTitle)
          and (EnumHwnds(ProcObj.fProcID, ehaGetCount, nil, nil) > 0) then
        Inc(Result);
    end;
  end;
end;

function IsProtectedProcess(Name: String): Boolean;
var
  i: Longint;
begin
  Result := False;
  i := ProProgs.IndexOf(Name);
  if (i > -1) and LongBool(Lo(Word(ProProgs.Objects[i]))) then
    Result := True;
end;

function IsProcessClosed(ProcObj: TProcWinObj): Boolean;
var
  hProc: THandle;
  ExitCode: Cardinal;
begin
  // TODO: use waitthread for process exit detection
  hProc := OpenProcess(PROCESS_QUERY_INFORMATION, False, ProcObj.ProcID);
  if hProc <> 0 then try
    if GetExitCodeProcess(hProc, ExitCode) then
      Result := (ExitCode <> STILL_ACTIVE)
    else
      Result := True;
  finally
    CloseHandle(hProc);
  end
  else begin
    // If the error code is other than ERROR_INVALID_PARAMETER then it means
    // that the process exists but we are not able to open it.
    Result := (GetLastError = ERROR_INVALID_PARAMETER);
  end;
  if Result then begin
    ProcObj.Status := psClosed;
    Log('Closing of ' + ProcObj.DispName + ' successful');
  end;
end;

function SysAgent_GetStat: DWORD;
var
  Wnd: HWND;
begin
  Result := 0;
  Wnd := FindWindow('SAGEWINDOWCLASS', 'SYSTEM AGENT COM WINDOW');
  if Wnd = 0 then
    Exit;
  SendMessageTimeOut(Wnd, SYSAGENT_GETSTATUS, 0, 0, SMTO_ABORTIFHUNG, 100,
      Result);
end;

function SysAgent_SetStatus(Enable: Boolean): DWORD;
var
  Wnd: HWND;
  Msg: Integer;
begin
  Result := 1;
  Wnd := FindWindow('SAGEWINDOWCLASS', 'SYSTEM AGENT COM WINDOW');
  if Wnd = 0 then
    Exit;
  if Enable then
    Msg := SYSAGENT_ENABLE
  else
    Msg := SYSAGENT_DISABLE;
  SendMessageTimeOut(Wnd, Msg, 0, 0, SMTO_ABORTifHUNG, 100, Result);
end;

function SysAgent_IsAvailable: Boolean;
begin
  Result := (FindWindow('SAGEWINDOWCLASS', 'SYSTEM AGENT COM WINDOW') <> 0);
end;

function GetIsScreenSaveActive: LongBool;
begin
  if not SystemParametersInfo(SPI_GETSCREENSAVEACTIVE, 0, @Result, 0) then
    Result := False;
end;

function GetObsfucator: Cardinal;
  function TestObsf(Obsfucator: Cardinal): Boolean;
  begin
    // TODO: bad practice to use IsBadReadPtr - remove this!
    Result := not IsBadReadPtr(Pointer(GetCurrentProcessID xor Obsfucator),
        SizeOf(Integer));
  end;
var
  PID: Integer;
  TID: Integer;
begin
  PID := GetCurrentProcessID;
  asm
    MOV  EAX, FS:[030h]
    XOR  EAX, PID
    MOV  Result, EAX
  end;
  if TestObsf(Result) then
    Exit;
  TID := GetCurrentThreadID;
  asm
    MOV  EAX, FS:[18h]
    SUB  EAX, 10h
    XOR  EAX, [TID]
    MOV  [Result], EAX
  end;
  if not TestObsf(Result) then
    Result := 0;
end;

function SetPrivilege(hToken: THandle; Privilege: PChar;
    bEnablePrivilege: BOOL): Integer;
var
  tp: TTokenPrivileges;
  luid: TLargeInteger;
  tpPrevious: TTokenPrivileges;
  cbPrevious: DWORD;
begin
  Result := 0;
  if not IsWinNT then
    Exit;
  cbPrevious := SizeOf(TTokenPrivileges);
  Result := 03;
  if not LookupPrivilegeValue(nil, Privilege, luid) then
    Exit;
  //
  // first pass.  get current privilege setting
  //
  tp.PrivilegeCount := 1;
  tp.Privileges[0].Luid := luid;
  tp.Privileges[0].Attributes := 0;
  Result := 04;
  AdjustTokenPrivileges(hToken, FALSE, tp, SizeOf(TTokenPrivileges), tpPrevious,
      cbPrevious);
  if GetLastError <> ERROR_SUCCESS then
    Exit;
  //
  // second pass.  set privilege based on previous setting
  //
  tpPrevious.PrivilegeCount := 1;
  tpPrevious.Privileges[0].Luid := luid;
  with tpPrevious.Privileges[0] do begin
    if bEnablePrivilege then
      Attributes := Attributes or SE_PRIVILEGE_ENABLED
    else
      Attributes := Attributes and (not SE_PRIVILEGE_ENABLED);
  end;
  Result := 05;
  AdjustTokenPrivileges2(hToken, FALSE, tpPrevious, cbPrevious, nil, cbPrevious);
  if GetLastError <> ERROR_SUCCESS then
    Exit;
  Result := 00;
end;

function WaitFlash(T: THandle): Boolean;
var
  Count: Integer;
begin
  Result := (WaitForSingleObject(T, 500) <> WAIT_TIMEOUT);
  if not Result then begin
    Count := 0;
    repeat
      Inc(Count);
      Result := (WaitForSingleObject(T, 500) <> WAIT_TIMEOUT);
    until Result or (Count > 9);
  end;
end;

function RemoteCurDirFunc(pInjectInfo: PInjectCurDir): DWORD; stdcall;
begin
  Result := PInjectInfo^.GetCurrentDir(SizeOf(PInjectInfo^.CurrentDir),
      @pInjectInfo^.CurrentDir[0]);
end;

function GetProcCurDir(hProc: THandle; var CurDir: String): Integer;
var
  InjectInfo: TInjectCurDir;
  pTarget: PInjectCurDir;
  BytesRead, BytesWritten: DWORD;
  hThread: THandle;
  ThreadId: DWORD;
  pCode: ^Byte;
  i: Integer;
begin
  // TODO: remove exception handler
  try
    pCode := Addr(RemoteCurDirFunc);
    for i := 0 to SizeOf(InjectInfo.InjectInfoCode) - 1 do begin
      InjectInfo.InjectInfoCode[i] := Byte(pCode^);
      Inc(pCode);
    end;
    InjectInfo.GetCurrentDir :=
        GetProcAddress(GetModuleHandle(kernel32), 'GetCurrentDirectoryA');
  except
    Result := 1;
    Exit;
  end;
  pTarget := nil;
  try
    pTarget := xVirtualAllocEx(hProc, nil, SizeOf(InjectInfo), MEM_COMMIT,
        PAGE_EXECUTE_READWRITE);
    Result := 2;
    if pTarget = nil then
      Exit;
    Result := 3;
    if not WriteProcessMemory(hProc, pTarget, @InjectInfo, SizeOf(InjectInfo),
        BytesWritten) then
      Exit;
    Result := 4;
    hThread := xCreateRemoteThread(hProc, nil, 0,
        Addr(pTarget^.InjectInfoCode[0]), pTarget, 0, @ThreadId);
    if hThread = 0 then
      Exit;
    WaitForSingleObject(hThread, 5000);
    CloseHandle(hThread);
    Result := 5;
    if not ReadProcessMemory(hProc, pTarget, @InjectInfo, SizeOf(InjectInfo),
        BytesRead) then
      Exit;
    CurDir := InjectInfo.CurrentDir;
    Result := 0;
  finally
    // TODO:: needs testing
    if (hProc <> 0) and Assigned(pTarget) then
      xVirtualFreeEx(hProc, pTarget, 0, MEM_RELEASE);
  end;
end;

function RemoteCmdLineFunc(pInjectCmdLine: PInjectCmdLine): DWORD; stdcall;
begin
  pInjectCmdLine^.CmdLine := pInjectCmdLine^.GetCommandLine^;
  Result := 0;
end;

function GetProcCmdLine(hProc: THandle; var CmdLine: String): Integer;
var
  InjectInfo: TInjectCmdLine;
  pTarget: PInjectCmdLine;
  BytesRead, BytesWritten: DWORD;
  hThread: THandle;
  ThreadId: DWORD;
  pCode: ^Byte;
  i: Integer;
begin
  // TODO: remove exception handler
  try
    pCode := Addr(RemoteCmdLineFunc);
    for i := 0 to SizeOf(InjectInfo.InjectInfoCode) - 1 do begin
      InjectInfo.InjectInfoCode[i] := Byte(pCode^);
      Inc(pCode);
    end;
    InjectInfo.GetCommandLine :=
        GetProcAddress(GetModuleHandle(kernel32), 'GetCommandLineA');
  except
    Result := 1;
    Exit;
  end;
  pTarget := nil;
  try
    pTarget := xVirtualAllocEx(hProc, nil, SizeOf(InjectInfo), MEM_COMMIT,
        PAGE_EXECUTE_READWRITE);
    Result := 2;
    if pTarget = nil then
      Exit;
    Result := 3;
    if not WriteProcessMemory(hProc, pTarget, @InjectInfo, SizeOf(InjectInfo),
        BytesWritten) then
      Exit;
    Result := 4;
    hThread := xCreateRemoteThread(hProc, nil, 0,
        Addr(pTarget^.InjectInfoCode[0]), pTarget, 0, @ThreadId);
    if hThread = 0 then
      Exit;
    WaitForSingleObject(hThread, 5000);
    CloseHandle(hThread);
    Result := 5;
    if not ReadProcessMemory(hProc, pTarget, @InjectInfo, SizeOf(InjectInfo),
        BytesRead) then
      Exit;
    //if not ReadProcessMemory(hProc, InjectInfo.CmdLine, Str,
    //    StrLen(Str) * SizeOf(Char), BytesRead) then
    //  Exit;
    CmdLine := InjectInfo.CmdLine;
    Result := 0;
  finally
    // TODO: needs testing
    if (hProc <> 0) and Assigned(pTarget) then
      xVirtualFreeEx(hProc, pTarget, 0, MEM_RELEASE);
  end;
end;

function LoadProcFunc: Boolean;
var
  hKernel: HMODULE;
begin
  Result := False;
  if not FuncLoaded then begin
    FuncLoaded := True;
    hKernel := GetModuleHandle(kernel32);
    if hKernel <> 0 then begin
      @xCreateRemoteThread := GetProcAddress(hKernel, 'CreateRemoteThread');
      @xVirtualAllocEx := GetProcAddress(hKernel, 'VirtualAllocEx');
      @xVirtualFreeEx := GetProcAddress(hKernel, 'VirtualFreeEx');
    end;
  end;
  if (@xCreateRemoteThread = nil)
      or (@xVirtualAllocEx = nil)
      or (@xVirtualFreeEx = nil) then
    Exit;
  Result := True;
end;

{function LoadRTDLL(var DLLPath: String): HMODULE;
begin
  DLLPath := GenerateUniqueName(GetTempDir, '.tmp');
  ResourceToFile('ELIRT.DLL', DLLPath);
  Result := LoadLibrary(PChar(DLLPath));
  if Result = 0 then begin
    Log('Loading of SmartClose Remote DLL failed!');
    DeleteFile(DllPath);
    Result := -1;
  end;
end;}

function InjectProcess(PID: DWORD; Terminate: Boolean;
    hInjectLib: HMODULE): TInjectResult;
const
  SE_DEBUG_NAME = 'SeDebugPrivilege';
var
  hProc, hProcDup: THandle;
  hRT, hKernel: HMODULE;
  ExitProcessProc: procedure(uExitCode: UINT); stdcall;
  hToken: THandle;
  TID: DWORD;
  PDB: PProcessDatabase;
begin
  // TODO: close opened process handles (hProc/hProcDup) again
  //@xCreateRemoteThread := nil;
  hKernel := 0;
  Result.Result := 01;
  if Terminate then begin
    hKernel := GetModuleHandle(kernel32);
    if @xCreateRemoteThread = nil then begin
      if IsWinNT then
        @xCreateRemoteThread := GetProcAddress(hKernel, 'CreateRemoteThread')
      {else
        @xCreateRemoteThread := GetProcAddress(LibH, 'xCreateRemoteThread');}
    end;
    if @xCreateRemoteThread = nil then
      Exit;
  end
  else if IsWinNT then begin
    if not LoadProcFunc then
      Exit;
  end;
  if IsWinNT then begin
    if not OpenProcessToken(GetCurrentProcess,
        TOKEN_ADJUST_PRIVILEGES or TOKEN_QUERY, hToken) then begin
      Result.Result := 02;
      Exit;
    end;
  end;
  try
    if IsWinNT then begin
      Result.Result := SetPrivilege(hToken, SE_DEBUG_NAME, True);
      if Result.Result <> 00 then
        Exit;
    end;
    try
      Result.Result := 06;
      hProc := OpenProcess(PROCESS_ALL_ACCESS, False, PID);
      if hProc = 0 then
        Exit;
      hProcDup := INVALID_HANDLE_VALUE;
      TID := 0;
      Result.Result := 07;
      if IsWinNT then begin
        if not DuplicateHandle(GetCurrentProcess, hProc, GetCurrentProcess,
            @hProcDup, PROCESS_ALL_ACCESS, FALSE, 0) then
          Exit;
      end
      else
        hProcDup := hProc;
      if hProcDup = 0 then
        Exit;
      if Terminate then begin
        ExitProcessProc := GetProcAddress(hKernel, 'ExitProcess');
        Result.Result := 08;
        if @ExitProcessProc = nil then
          Exit;
        hRT := xCreateRemoteThread(hProcDup, nil, 0, @ExitProcessProc, nil, 0, @TID);
        if hRT = 0 then
          Exit
        else try
          if not WaitFlash(hRT) then
            Result.Result := 09
          else
            Result.Result := 00;
        finally
          CloseHandle(hRT);
        end;
      end
      else begin
        if IsWinNT then begin
          Result.Result := 0;
          Result.CmdLineResult := GetProcCmdLine(hProcDup, Result.CmdLine);
          Result.CurDirResult := GetProcCurDir(hProcDup, Result.CurDir);
        end
        else begin
          Result.Result := 08;
          if Obsfucator = 0 then
            Exit;
          Result.Result := 09;
          // TODO: remove exception handler
          try
            PDB := Pointer((PID xor Obsfucator));
          except
            Exit;
          end;
          if PDB = nil then
            Exit;
          Result.Result := 10;
          // TODO: remove exception handler
          try
            Result.CmdLine := PDB.pEDB.CmdLine;
            Result.CmdLineResult := 0;
            Result.CurDir := PDB.pEDB.CurrDirectory;
            Result.CurDirResult := 0;
          except
            Exit;
          end;
          Result.Result := 0;
        end;
      end;
    finally
      if IsWinNT then
        SetPrivilege(hToken, SE_DEBUG_NAME, False);
    end;
  finally
    if IsWinNT then
      CloseHandle(hToken);
  end;
end;

function KillProcess(PID: DWORD): Integer;
var
  hProc: THandle;
begin
  hProc := OpenProcess(PROCESS_ALL_ACCESS, False, PID);
  if hProc = 0 then
    Result := 01
  else if not TerminateProcess(hProc, 0) then
    Result := 02
  else if not WaitFlash(hProc) then
    Result := 03
  else
    Result := 00;
end;

procedure RedrawSystray;
var
  WasPt: TPoint;
  X, Y: Integer;
  hTray: HWND;
  TrayRect: TRect;
begin
  GetCursorPos(WasPt);
  try
    hTray :=
        FindWindowEx(FindWindowEx(GetDesktopWindow, 0, 'Shell_TrayWnd', nil),
        0, 'TrayNotifyWnd', nil);
    if hTray = 0 then
      Exit;
    GetWindowRect(hTray, TrayRect);
    Y := TrayRect.Top;
    while Y < TrayRect.Bottom do begin
      X := TrayRect.Left;
      while X < TrayRect.Right do begin
        Sleep(2);
        SetCursorPos(X, Y);
        Inc(X, 8);
      end;
      Inc(Y, 8);
    end;
    InvalidateRect(hTray, nil, True);
    UpdateWindow(hTray);
  finally
    SetCursorPos(WasPt.X, WasPt.Y);
  end;
end;

constructor TProcWaitThread.Create(Suspended: Boolean; hProc: THandle;
    Index: Integer; Seconds: Cardinal; CloseTimeout: Cardinal;
    OnLoaded, OnClosed: TNotifyEvent);
begin
  inherited Create(Suspended);
  FreeOnTerminate := True;
  FhProc := hProc;
  FIndex := Index;
  FSecs := Seconds;
  FCloseTimeout := CloseTimeout;
  FOnLoaded := OnLoaded;
  OnTerminate := OnClosed;
  FExitCode := 0;
end;

procedure TProcWaitThread.SyncLoaded;
begin
  if Assigned(FOnLoaded) then
    FOnLoaded(Self);
end;

procedure TProcWaitThread.Execute;
var
  Objs: array[0..1] of THandle;
begin
  Objs[0] := FhProc;
  Objs[1] := hStopEvent;
  FRetVal := WaitForInputIdle(FhProc, FSecs * 1000);
  Synchronize(SyncLoaded);
  FRetVal := WaitForMultipleObjects(2, @Objs, False, FCloseTimeout);
  GetExitCodeProcess(FhProc, FExitCode);
  CloseHandle(FhProc);
end;

{function GetFirstThreadID(ProcID: Integer): Integer;
var
  hSnap: THandle;
  ThreadEntry: TThreadEntry32;
begin
  Result := 0;
  hSnap := CreateToolHelp32Snapshot(TH32CS_SNAPTHREAD, 0);
  if hSnap = 0 then
    Exit;
  try
    ThreadEntry.dwSize := SizeOf(ThreadEntry);
    if Thread32First(hSnap, ThreadEntry) then begin
      repeat
        if ThreadEntry.th32OwnerProcessID = ProcID then begin
          Result := ThreadEntry.th32ThreadID;
          Exit;
        end;
      until not Thread32Next(hSnap, ThreadEntry);
    end;
  finally
    CloseHandle(hSnap);
  end;
end;}

{function SmartKillProcess(PID: DWORD; LibModule: HMODULE): Integer;
type
  TInjectAndTerminate = function(ThreadID: DWORD): Integer;
var
  hProc: THandle;
  dwTID: DWORD;
  InjectAndTerminate: TInjectAndTerminate;
begin
  Result := 0;
  if IsWinNT then
    Exit;
  if LibModule > 32 then begin
    @InjectAndTerminate := GetProcAddress(LibModule, 'InjectAndTerminate');
    if @InjectAndTerminate <> nil then begin
      dwTID := GetFirstThreadID(PID);
      if dwTID <> 0 then begin
        Result := InjectAndTerminate(dwTID);
        if Result = 00 then begin
          Result := 7;
          Reject(dwTID);
          hProc := OpenProcess(PROCESS_ALL_ACCESS, False, PID);
          if (hProc = 0) or WaitFlash(hProc) then begin
            Result := 00;
            Exit;
          end;
        end;
      end
      else
        Result := 03;
    end
    else
      Result := 02;
  end
  else
    Result := 01;
end;}

initialization
  if Win32Platform = VER_PLATFORM_WIN32_WINDOWS then
    Obsfucator := GetObsfucator;
  hStopEvent := CreateEvent(nil, True, False,
      'SmartClose ProcWaitThread Stop Event ' +
      '{B39624AA-7B74-40E5-80F4-8DBCD22A22F2}');

finalization
  CloseHandle(hStopEvent);

end.
