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
 * (Internet) Explorer window-related functions
 *
 ****************************************************************************)

unit ExpWin;

interface

uses Windows, Messages, Classes, SysUtils, MBCSUtil, MyRegistry, ActiveX,
    ShlObj, ShlObj2, ShellApi, ComObj, FileCtrl, CommCtrl, NewDialogs, Forms,
    ShDocVw_TLB, Common2;

type
  TExpWindowType = (wtExplorer, wtExplorerTree, wtIE);

type
  TExpWindowPath = record
    WindowType: TExpWindowType;
    Location: String;
    // TODO: uncomment?
    //FullScreenMode: Boolean;
    //MonitorIdx: Integer;
    PIDL: PItemIDList;
  end;
  PExpWindowPath = ^TExpWindowPath;

type
  TWebBrowserWrapper = class(TObject)
  public
    WebBrowser2: IWebBrowser2;
    constructor Create(WB2: IWebBrowser2);
  end;

function IsExplorerWindow(hWnd: HWND): Boolean;
function IsIEWindow(hWnd: HWND): Boolean;
procedure GetExpWindows(ExpWins: TStrings);
procedure GetExplorerWindowPath(Handle: HWND; IsIEWindow: Boolean;
    WBWrapper: TWebBrowserWrapper; var Path: TExpWindowPath);
//function GetIEWindowPath(hWnd: HWND; var Path: TExpWindowPath): Integer;
function CloseExpWindow(WBWrapper: TWebBrowserWrapper; hWnd: HWND): Boolean;
function TestPath(Path: String; OnlyDirs: Boolean): Boolean;
function PathToPIDL(APath: String): PItemIDList;
function PIDLToDisplayName(PIDL: PItemIDList): String;
function PIDLToPath(PIDL: PItemIDList): String;
//function GetExplorerWindows: TWindows;

implementation

uses CloseWiz, Func, CloseThread;

const
  CWM_GETPATH = WM_USER + 12;

var
  //ShowWarning: Boolean = True;
  Desktop: IShellFolder;
  hShell32: HMODULE;
  FuncLoaded: Boolean = False;
  IsWin95: Boolean = False;
  SHFreeShared: function(hMemory: THandle; PID: DWORD): Boolean; stdcall = nil;
  SHLockShared: function(hMemory: THandle; PID: DWORD): Pointer; stdcall = nil;
  SHUnlockShared: function(Buf: Pointer): Boolean; stdcall = nil;
  ILClone: function(pidl: PItemIDList): PItemIDList; stdcall = nil;
  ILGlobalFree: function(pidl: PItemIDList): LongBool; stdcall = nil;

constructor TWebBrowserWrapper.Create(WB2: IWebBrowser2);
begin
  WebBrowser2 := WB2;
end;

function PIDLToString(IDList: PItemIDList): String;

  function NextPIDL(IDList: PItemIDList): PItemIDList;
  begin
    Result := IDList;
    Inc(PChar(Result), IDList^.mkid.cb);
  end;

var
  Dummy: Byte;
  GUID: TGUID;
begin
  try
    Dummy := 2; // trick to access abID's elements past declared size
    CopyMemory(@GUID, Addr(IDList^.mkid.abID[Dummy]), SizeOf(GUID));
    Result := GUIDToString(GUID);
    // Get Next PIDL, if any
    IDList := NextPIDL(IDList);
    if IDList^.mkid.cb <> 0 then
      Result := Result + '\::' + PIDLToString(IDList);;
  except
    on Exception do
      Result := 'ERROR';
  end;
end;

function PIDLToPath(PIDL: PItemIDList): String;
var
  Name: TStrRet;
  Path: String;
begin
  SHGetDesktopFolder(Desktop);
  if not Assigned(Desktop) then
    Exit;
  Desktop.GetDisplayNameOf(PIDL, SHGDN_FORPARSING or SHGDN_INCLUDE_NONFILESYS,
      Name);
  //Result := GetDisplayName(PIDL, False, False, False, True);
  {case Name.uType of
    STRRET_WSTR:
      Result := Name.pOleStr;
    STRRET_OFFSET:
      Result := Name.pStr;
    STRRET_CSTR:
      Result := Name.cStr;
  end;}
  Result := StrRetToString(Name, PIDL);
  Path := Result;
  if not TestPath(Result, True) then begin
    Result := PIDLToString(PIDL);
    if Trim(Result) = '' then
      Exit;
    Result := '::' + Result;
    if not TestPath(Result, True) then
      Result := Path;
  end;
end;

procedure ParsePIDL(PIDL: PItemIDList; var Path: TExpWindowPath);
begin
  // TODO: remove exception handler
  try
    try
      Path.Location := PIDLToPath(PIDL);
    finally
      CoTaskMemFree(PIDL);
    end;
  except
    on Exception do
      Exit;
  end;
end;

function GetExplorerWindowType9xAndNT(hWnd: HWND): TExpWindowType;

  function GetDesktopName: String;
  var
    PIDL: PItemIDList;
  begin
    Result := '';
    SHGetSpecialFolderLocation(0, CSIDL_DESKTOP, PIDL);
    Result := PIDLToDisplayName(PIDL);
  end;
  
  function FindTreeProc(hWnd: Cardinal; hTree: PHWND): BOOL; stdcall;
  begin
    Result := True;
    if IsWindowVisible(hWnd) then begin
      if (lstrcmpi(PChar(GetClass(hWnd)), WC_TREEVIEW) = 0)
          and (TreeGetRootCaption(hWnd) = GetDesktopName) then begin
        hTree^ := hWnd;
        Result := False;
      end;
    end;
  end;
  
var
  Handle, TreeHandle: Cardinal;
begin
  TreeHandle := 0;
  Result := wtExplorer;
  if FindWindowEx(hWnd, 0, WC_TREEVIEW, nil) <> 0 then begin
    Result := wtExplorerTree;
    Exit;
  end;
  Handle := FindWindowEx(hWnd, 0, 'BaseBar', '');
  if Handle <> 0 then begin
    Handle := FindWindowEx(Handle, 0, REBARCLASSNAME, '');
    if Handle <> 0 then begin
      TreeHandle := FindWindowEx(Handle, 0, WC_TREEVIEW, '');
      if TreeHandle = 0 then begin
        //Handle := FindWindowEx(Handle, 0, TreeClass, 'Folders');
        EnumChildWindows(Handle, @FindTreeProc, Integer(@TreeHandle));
      end;
      if TreeHandle <> 0 then begin
        if IsWindowVisible(TreeHandle) then
          Result := wtExplorerTree;
      end;
    end;
  end;
end;

function LoadMemFunc: Boolean;
begin
  Result := False;
  if not FuncLoaded then begin
    FuncLoaded := True;
    hShell32 := LoadLibrary(shell32);
    if hShell32 <> 0 then begin
      LoadFuncOrd(hShell32, @ILClone, 18);
      LoadFuncOrd(hShell32, @ILGlobalFree, 156);
      LoadFuncOrd(hShell32, @SHLockShared, 521);
      LoadFuncOrd(hShell32, @SHFreeShared, 523);
      LoadFuncOrd(hShell32, @SHUnlockShared, 522);
    end;
    if not Assigned(SHLockShared) or not Assigned(SHFreeShared)
        or not Assigned(SHUnlockShared) then
      IsWin95 := True;
  end;
  if not Assigned(ILClone) then
    Exit;
  if not Assigned(ILGlobalFree) then
    Exit;
  Result := True;
end;

function GetExplorerWindowPath9xAndNT(hWnd: HWND;
    var Path: TExpWindowPath): Integer;

  function ProcessPIDL(PIDL: PItemIDList): Integer;
  begin
    Result := 3;
    // TODO: remove exception handler
    try
      PIDL := ILClone(PIDL);
    except
      Exit;
    end;
    if PIDL = nil then
      Exit;
    Result := 4;
    ParsePIDL(PIDL, Path);
    if Path.Location = '' then
      Exit;
    if not TestPath(Path.Location, True) then
      Path.WindowType := wtIE;
    Result := 0;
  end;
  
var
  PID: Integer;
  hMem: THandle;
  pv: Pointer;
  RetPIDL: PItemIDList;
begin
  //Win95 := False;
  Path.Location := '';
  Result := 1;
  if not LoadMemFunc then
    Exit;
  if IsWin95 then
    PID := 0
  else
    PID := GetCurrentProcessID;
  hMem := 0;
  SendMessageTimeOut(hWnd, CWM_GETPATH, PID, 0, SMTO_ABORTIFHUNG, 1000, hMem);
  Result := 2;
  if hMem = 0 then
    Exit;
  if not IsWin95 then try
    // NT
    Result := 3;
    pv := SHLockShared(hMem, PID);
    if pv = nil then
      Exit;
    try
      // TODO: remove exception handler
      try
        Result := ProcessPIDL(PItemIDList(pv));
      except
        Exit;
      end;
    finally
      SHUnlockShared(pv);
    end;
  finally
    SHFreeShared(hMem, PID);
  end
  else begin
    // Win 9x
    Result := 3;
    // TODO: remove exception handler
    try
      RetPIDL := PItemIDList(hMem);
    except
      Exit;
    end;
    if RetPIDL = nil then
      Exit;
    try
      Result := ProcessPIDL(RetPIDL);
    finally
      ILGlobalFree(RetPIDL);
    end;
  end;
end;

function GetExplorerWindowPathAndType(hExpWin: HWND; WebBrowser2: IWebBrowser2;
    var Path: TExpWindowPath): Integer;
var
  ServiceProvider: IServiceProvider;
  ShellBrowser: IShellBrowser;
  ShellView: IShellView;
  FolderView: IFolderView;
  PersistFolder2: IPersistFolder2;
  PIDL: PItemIDList;
  hTree: HWND;
begin
  Path.WindowType := wtExplorer;
  Result := 1;
  if not Succeeded(WebBrowser2.QueryInterface(IServiceProvider,
      ServiceProvider)) then
    Exit;
  Result := 2;
  if not Succeeded(ServiceProvider.QueryService(SID_STopLevelBrowser,
      IShellBrowser, ShellBrowser)) then
    Exit;
  if Succeeded(ShellBrowser.GetControlWindow(FCW_TREE, hTree))
      and (hTree <> 0) then
    Path.WindowType := wtExplorerTree;
  Result := 3;
  if not Succeeded(ShellBrowser.QueryActiveShellView(ShellView)) then
    Exit;
  Result := 4;
  if not Succeeded(ShellView.QueryInterface(IFolderView, FolderView)) then
    Exit;
  Result := 5;
  if not Succeeded(FolderView.GetFolder(IPersistFolder2, PersistFolder2)) then
    Exit;
  Result := 6;
  if not Succeeded(PersistFolder2.GetCurFolder(PIDL)) then
    Exit;
  if not Assigned(PIDL) then
    Exit;
  Result := 7;
  ParsePIDL(PIDL, Path); // PIDL gets freed here
  if Path.Location = '' then
    Exit;
  if not TestPath(Path.Location, True) then
    Path.WindowType := wtIE;
  Result := 0;
end;

procedure GetExplorerWindowPath(Handle: HWND; IsIEWindow: Boolean;
    WBWrapper: TWebBrowserWrapper; var Path: TExpWindowPath);
var
  RetVal: Integer;
  Tmp: String;
  WinXPOrHigher: Boolean;
begin
  if IsIEWindow then begin
    Log('Internet Explorer window handle: ' + IntToStr(Handle));
    //RetVal := GetIEWindowPath(Handle, Path);
    Path.Location := '';
    if Assigned(WBWrapper.WebBrowser2) then try
      Path.Location := WBWrapper.WebBrowser2.Get_LocationURL;
    except
      on EOleException do
        // nothing
    end;
    Path.WindowType := wtIE;
    if Trim(Path.Location) = '' then begin
      Tmp := 'unsuccessful; SmartClose couldn''t get the current location, ' +
          'possibly because the location of the IE window is a normal folder; ' +
          'treating IE window as an Explorer window...'
    end
    else begin
      Tmp := 'successful; SmartClose was able to retrieve the current location ' +
          'of the IE window';
    end;
    Log('     "GetIEWindowPath" function result: ' + Tmp);
    {case RetVal of
      2: Tmp := '(unsuccessful; SmartClose was able to retrieve the location URL ' +
          'with Active Accessibility, but the URL was empty)';
      3: Tmp := '(unsuccessful; unable to load oleacc.dll, Active Accessibility ' +
        'is possibly not installed)';
      4: Tmp := '(unsuccessful; unable to load a required function, possibly wrong ' +
          'or incorrectly installed version of Active Accessibility)';
      5: Tmp := '(unsuccessful; the SendMessageTimeOut API-function failed)';
      {0: Tmp := '(successful; SmartClose was able to retrieve the current ' +
          'location from the Address-bar)';
      2: Tmp := '(unsuccessful; SmartClose found the Address-bar but was not ' +
          'able to read its contents)';
      else
        Tmp := '(unknown error)';
    end;}
    if Trim(Path.Location) = '' then
      GetExplorerWindowPath(Handle, False, WBWrapper, Path)
    else
      Log('     "GetIEWindowPath" function return value: ' + Path.Location);
  end
  else begin
    Path.Location := '';
    Log('Explorer window handle: ' + IntToStr(Handle));
    WinXPOrHigher := (Win32MajorVersion > 5)
        or ((Win32MajorVersion = 5) and (Win32MinorVersion >= 1));
    if WinXPOrHigher and Assigned(WBWrapper.WebBrowser2) then begin
      // use IWebBrowser2 on WinXP and higher
      RetVal := GetExplorerWindowPathAndType(Handle, WBWrapper.WebBrowser2, Path);
      case RetVal of
        0: Tmp := '(successful; SmartClose was able to retrieve a valid PIDL ' +
            'from the current path)';
        1: Tmp := '(unsuccessful; SmartClose failed to retrieve the ' +
            'IServiceProvider interface)';
        2: Tmp := '(unsuccessful; the QueryService method failed)';
        3: Tmp := '(unsuccessful; the QueryActiveShellView method failed)';
        4: Tmp := '(unsuccessful; SmartClose failed to retrieve the ' +
            'IShellFolder interface)';
        5: Tmp := '(unsuccessful; the GetFolder method failed)';
        6: Tmp := '(unsuccessful; the GetCurFolder method failed)';
        7: Tmp := '(unsuccessful; SmartClose failed to convert the returned ' +
            'PIDL to a string (path))';
        else
          Tmp := '(unknown error)';
      end;
    end
    else begin
      Path.WindowType := GetExplorerWindowType9xAndNT(Handle);
      RetVal := GetExplorerWindowPath9xAndNT(Handle, Path);
      case RetVal of
        0: Tmp := '(successful; SmartClose was able to retrieve a valid PIDL ' +
            'from the current path)';
        1: Tmp := '(unsuccessful; SmartClose failed to load one or more of the ' +
            'required API functions)';
        2: Tmp := '(unsuccessful; the "Get path"-message that was sent to the ' +
            'Explorer window failed)';
        3: Tmp := '(unsuccessful; SmartClose failed to convert the returned ' +
            'value to a PIDL)';
        4: Tmp := '(unsuccessful; SmartClose failed to convert the returned ' +
            'PIDL to a string (path))';
        {0: Tmp := '(successful; SmartClose was able to retrieve a qualified ' +
            'path from the Address bar)';
        1: Tmp := '(successful; SmartClose was able to retrieve a qualified ' +
            'path with Active Accessibility)';
        2: Tmp := '(successful; SmartClose was able to retrieve a display name ' +
            'because the current folder is a special (namespace) folder)';
        3: Tmp := '(unsuccessful; SmartClose found the Address-bar but was not ' +
            'able to read its contents)';
        4: Tmp := '(unsuccessful; SmartClose was able to retrieve the location ' +
            'URL with Active Accessibility, but the URL was in an unrecognizeable ' +
            'format, possibly a web page)';
        5: Tmp := '(unsuccessful; Active Accessibility error: possibly Web View ' +
            'is disabled, or wrong OS or IE version)';}
        else
          Tmp := '(unknown error)';
      end;
    end;
    Log('     "GetExplorerWindowPath" function result: ' +
        IntToStr(RetVal) + ' ' + Tmp);
    if Path.Location <> '' then
      Log('     "GetExplorerWindowPath" function return value: ' + Path.Location);
  end;
end;

function IsExplorerWindow(hWnd: HWND): Boolean;
var
  Buffer: array[0..MAX_PATH] of Char;
begin
  GetClassName(hWnd, Buffer, MAX_PATH);
  Result := True;
  if StrComp(Buffer, 'ExploreWClass') = 0 then
    Exit;
  if StrComp(Buffer, 'CabinetWClass') = 0 then
    Exit;
  //if StrComp(Buffer, 'IEFrame') = 0 then
  //  Exit;
  Result := False;
end;

function IsIEWindow(hWnd: HWND): Boolean;
var
  Buffer: array[0..MAX_PATH] of Char;
begin
  GetClassName(hWnd, Buffer, MAX_PATH);
  Result := True;
  if StrComp(Buffer, 'IEFrame') = 0 then
    Exit;
  Result := False;
end;

function ExpWinProc(hWnd: HWND; ExpWins: TStrings): BOOL; stdcall;
begin
  Result := True;
  if IsExplorerWindow(hWnd) and (ExpWins.IndexOf(IntToStr(hWnd)) = -1) then
    ExpWins.Add(IntToStr(hWnd));
end;

procedure GetExpWindowsNT95(ExpWins: TStrings);
begin
  EnumWindows(@ExpWinProc, Integer(ExpWins));
end;

procedure GetExpWindows(ExpWins: TStrings);
var
  Unknown: IUnknown;
  Result: HResult;
  i: Integer;
  ShellWin: ShellWindows;
  IE: IWebbrowser2;
begin
  ExpWins.Clear;
  //EnumWindows(@ExpWinProc, Integer(hWnds));
  try
    Result := GetActiveObject(Class_ShellWindows, nil, Unknown);
    if (Result = MK_E_UNAVAILABLE) then
      ShellWin := CreateComObject(Class_ShellWindows) as ShellWindows
    else begin
      { make sure no other error occurred during GetActiveObject }
      OleCheck(Result);
      OleCheck(Unknown.QueryInterface(IShellWindows, ShellWin));
    end;
    if ShellWin = nil then
      Exit;
    for i := 0 to ShellWin.Count - 1 do begin
      // TODO: use queryinterface for better error handling
      IE := ShellWin.Item(i) as IWebbrowser2;
      if IE <> nil then
        ExpWins.AddObject(IntToStr(IE.Get_HWND), TWebBrowserWrapper.Create(IE));
    end
  except
    // TODO: make handler less general
    on Exception do begin
      MsgBox('Error while retrieving the list of (Internet) Explorer windows. ' +
          'No Explorer windows will be closed.',
          bgOK, miError, '', 0);
    end;
  end;
  if (Win32MajorVersion < 5) and ((Win32MinorVersion < 10) or IsWinNT) then
    GetExpWindowsNT95(ExpWins);
end;

{function GetIEFromHWND(WHandle: HWND; var IE: IWebbrowser2): HRESULT;
type
  TObjectFromLResult = function(LRESULT: lResult; const IID: TIID;
      WPARAM: wParam; out pObject): HRESULT; stdcall;
var
  hInst: Integer;
  lRes: Integer;
  MSG: Integer;
  pDoc: IHTMLDocument2;
  ObjectFromLresult: TObjectFromLresult;
begin
  Result := 3;
  hInst := LoadLibrary('Oleacc.dll');
  if hInst = 0 then
    Exit;
  try
    Result := 4;
    @ObjectFromLresult := GetProcAddress(hInst, 'ObjectFromLresult');
    if @ObjectFromLresult <> nil then begin
      MSG := RegisterWindowMessage('WM_HTML_GETOBJECT');
      Result := 5;
      if not LongBool(SendMessageTimeOut(WHandle, MSG, 0, 0, SMTO_ABORTIFHUNG,
          10000, lRes)) then
        Exit;
      Result := ObjectFromLresult(lRes, IHTMLDocument2, 0, pDoc);
      if Result = S_OK then begin
        (pDoc.parentWindow as IServiceprovider).QueryService(IWebbrowserApp,
            IWebbrowser2, IE);
      end;
    end;
  finally
    FreeLibrary(hInst);
  end;
end;}

{function GetExplorerWindowPath(hWnd: HWND; var Path: ExpWindowPath): Integer;
var
  hAddrBar, hIE: Integer;
  IE: IWebbrowser2;
begin
  Result := 0;
  Path.IsSpecial := False;
  Path.ShowTree := False;
  if StrToIntDef(FindWindowLike(hWnd, 'SysTreeView32'), 0) > 0 then
    Path.ShowTree := True;

  hAddrBar := StrToIntDef(FindWindowLike(hWnd, 'ComboBoxEx32'), 0);
  hAddrBar := FindWindowEx(hAddrBar, 0, 'ComboBox', nil);
  hAddrBar := FindWindowEx(hAddrBar, 0, 'Edit', nil);
  if hAddrBar > 0 then begin
    Path.Path := WindowText(hAddrBar);
    if (Path.Path <> '') and not DirectoryExists(Path.Path) then
      Result := 2
    else if Path.Path = '' then
      Result := 3;
  end
  else
    Result := 1;

  if Result = 1 then begin // no address bar
    hIE := FindWindowEx(hWnd, 0, 'SHELLDLL_DefView', nil);
    hIE := FindWindowEx(hIE, 0, 'Internet Explorer_Server', nil);
    if GetIEFromHwnd(hIE, IE) = 0 then begin
      if Copy(IE.LocationURL, 1, 8) = 'file:///' then begin
        Path.Path := Copy(IE.LocationURL, 9, Length(IE.LocationURL) - 8);
        ConvertPercentStr(Path.Path);
      end else if IE.LocationName <> '' then begin
        Path.Path := IE.LocationName;
        Result := 2;
      end else
        Result := 4;
    end
    else begin
      if ShowWarning then
        ShowWarning := not GetKeyBool(Hive, RegKey, 'NoExplorerWarning');
      if ShowWarning then begin
        if SpecialMsgBox('SmartClose was unable to retrieve the active path of ' +
            'one or more Explorer windows because the Address bar is disabled ' +
            'and because one (or both) of the following conditions is met:' + CrLf +
            '- ''Web View'' is disabled in one or more Explorer windows' + CrLf +
            '- You are currently running Windows 95 or NT or you don''t have ' +
            'Internet Explorer 4.0 or higher installed' + CrLf + CrLf +
            'To let SmartClose successfully retrieve the active path of ' +
            'Explorer windows, you can either enable the Address bar ' +
            '(View > Toolbars > Address bar), or turn on Web View if you have ' +
            'a compatible operating system and Internet Explorer version.',
            miWarning, 'Explorer Window Warning', 0) then
          SetKeyBool(Hive, RegKey, 'NoExplorerWarning', True);
        ShowWarning := False;
      end;
      Result := 5;
    end;

  end;

  if Result = 2 then // special folder
    Path.IsSpecial := True;

end;}

function PIDLToDisplayName(PIDL: PItemIDList): String;
var
  Name: TStrRet;
begin
  SHGetDesktopFolder(Desktop);
  if not Assigned(Desktop) then
    Exit;
  Desktop.GetDisplayNameOf(PIDL, SHGDN_NORMAL, Name);
  Result := StrRetToString(Name, PIDL);
end;

function PathToPIDL(APath: String): PItemIDList;
var
  pchEaten, dwAttributes: ULONG;
begin
  Result := nil;
  SHGetDesktopFolder(Desktop);
  dwAttributes := 0;
  if not Assigned(Desktop) then
    Exit;
  Desktop.ParseDisplayName(0, nil, StringToOleStr(APath), pchEaten, Result,
      dwAttributes);
end;

function TestPath(Path: String; OnlyDirs: Boolean): Boolean;
var
  PIDL: PItemIDList;
begin
  Result := False;
  if Copy(Path, 1, 2) = '::' then begin
    PIDL := PathToPIDL(Path);
    if not Assigned(PIDL) then
      Exit;
    CoTaskMemFree(PIDL);
  end
  else begin
    if OnlyDirs then begin
      if not DirectoryExists(Path) then
        Exit;
    end
    else begin
      if Trim(Path) = '' then
        Exit;
    end;
  end;
  Result := True;
end;

{function GetIEWindowPath(hWnd: HWND; var Path: TExpWindowPath): Integer;
var
  //WorkerClass: String;
  //hAddrBar: Integer;
  hIE: Integer;
  IExpl: IWebbrowser2;
begin
  Path.Location := '';
  Path.WindowType := wtIE;
  {WorkerClass := IIf(IsWinNT, 'WorkerW', 'WorkerA');
  hAddrBar := FindWindowEx(hWnd, 0, PChar(WorkerClass), nil);     // WorkerA/WorkerW
  if hAddrBar <> 0 then
    hAddrBar := FindWindowEx(hAddrBar, 0, 'ReBarWindow32', nil);  // rebar
  if hAddrBar <> 0 then
    hAddrBar := FindWindowEx(hAddrBar, 0, 'ComboBoxEx32', nil);   // comboboxex
  if hAddrBar <> 0 then
    hAddrBar := FindWindowEx(hAddrBar, 0, 'ComboBox', nil);       // combobox
  if hAddrBar <> 0 then
    hAddrBar := FindWindowEx(hAddrBar, 0, 'Edit', nil);           // edit
  if hAddrBar <> 0 then begin
    Path.Location := WindowText(hAddrBar);
    if Path.Location = '' then
      Result := 2
    else
      Result := 0;
  end
  else
    Result := 1;

  Result := 1;
  hIE := FindWindowEx(hWnd, 0, 'Shell DocObject View', nil);
  if hIE <> 0 then
    hIE := FindWindowEx(hIE, 0, 'Internet Explorer_Server', nil);
  if hIE = 0 then
    Exit;
  SetForeGroundWindow(hWnd);
  SetActiveWindow(hWnd);
  Result := GetIEFromHwnd(hIE, IExpl);
  if Result = 0 then begin
    Result := 2;
    if IExpl.LocationURL = '' then
      Exit;
    Path.Location := IExpl.LocationURL;
    Result := 0;
  end
  else begin
    if Result = 3 then begin
      if ShowWarning then
        ShowWarning := not GetKeyBool(Hive, RegKey, 'NoIEWarning', False);
      if ShowWarning then begin
        if SpecialMsgBox('SmartClose was unable to retrieve the current location ' +
            'of one or more Internet Explorer windows because you don''t have ' +
            'Active Accessibility installed on this computer.' + CrLf +
            'SmartClose requires Active Accessibility to be able to retrieve the ' +
            'active path of Internet Explorer windows.' + CrLf +
            '(Choose Help for more information about how to install Active ' +
            'Accessibility.)',
            miWarning, 'Internet Explorer Window Warning', 1) then
          SetKeyBool(Hive, RegKey, 'NoIEWarning', True);
        ShowWarning := False;
      end;
    end;
  end;
end;}

function CloseExpWindow(WBWrapper: TWebBrowserWrapper; hWnd: HWND): Boolean;
var
  IE: IWebBrowser2;
  hIEWin: Cardinal;
  i: Integer;
  PID: Integer;
begin
  if WBWrapper = nil then begin
    {SetLastError(0);
    SendMessageTimeout(H, WM_GETTEXTLENGTH, 0, 0, SMTO_ABORTifHUNG, 100, D);
    if GetLastError = 0 then begin
      if SetForegroundWindow(hWnd) then
        BringWindowToTop(hWnd);
    end;
    SetLastError(0);
    SendMessageTimeout(H, WM_DESTROY, 0, 0, SMTO_ABORTifHUNG, 100, D);
    Sleep(100);
    SendMessageTimeout(H, WM_NCDESTROY, 0, 0, SMTO_ABORTifHUNG, 100, D);
    Sleep(100);
    SendMessageTimeOut(H, WM_CLOSE, 0, 0, SMTO_ABORTifHUNG, 30, D);
    Result := not IsWindow(hWnd);}

    Result := False;
    PID := 0;
    hIEWin := hWnd;
    if not IsWindow(hIEWin) then
      Exit;
    if not IsExplorerWindow(hIEWin) then
      Exit;
    GetWindowThreadProcessID(hIEWin, @PID);
    if PID = 0 then
      Exit;
    IsWaiting := True;
    TCloseThread.Create(hIEWin, PID, 10, frmCloseWiz.thdDone, nil, True);
    while IsWaiting do
      Application.ProcessMessages;
    Sleep(500);
  end
  else begin
    Result := False;
    try
      IE := WBWrapper.WebBrowser2;
      hIEWin := IE.Get_HWND;
      IE.Quit;
      IE := nil;
    except
      // TODO: make handler less general
      on Exception do
        Exit;
    end;
    for i := 1 to 200 do begin
      Application.ProcessMessages;
      if not IsWindow(hIEWin) then begin
        //Log('     Explorer window closed in ' + IntToStr(i * 100) +
        //    ' milliseconds.');
        Break;
      end;
      Sleep(100);
    end;
  end;
  Result := not IsWindow(hIEWin);
end;

{function GetExplorerWindows: TWindows;
var
  hWnd: HWND;
  Cnt: Longint;
begin
  Cnt := 0;
  hWnd := GetWindow(GetDesktopWindow, GW_CHILD);
  while hWnd <> 0 do begin
    if IsExplorerWindow(hWnd) then begin
      Result[Cnt] := hWnd;
      Inc(Cnt);
    end;
    hWnd := GetWindow(Wnd, GW_HWNDNEXT);
  end;
end;}

initialization
  //ShowWarning := True;

finalization
  if hShell32 <> 0 then
    FreeLibrary(hShell32);

end.
