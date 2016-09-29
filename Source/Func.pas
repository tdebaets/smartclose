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
 * Various utility functions
 *
 ****************************************************************************)

unit Func;

interface

uses Windows, Messages, Forms, NewDialogs, SysUtils, MBCSUtil, MyRegistry,
    Classes, FileCtrl, ShellApi, ActiveX, ShlObj, ComObj, ComCtrls, CommCtrl,
    Controls, StdCtrls, Graphics, Consts;

type
  TWizardMode = (wmNormal, wmDebug, wmSilent, wmSetDefault);
  TCheckDirResult = (cdnValid, cdnInvalid, cdnBadChars, cdnInvalidDrive);
  {TOFN = record
    Filter: string;
    Filename: string;
    InitialDir: string;
    Title: string;
    DefaultExt: string;
  end;}
  TCloseSettings = record
    NoIntro: Boolean;
    ShowProgs: Boolean;
    ShowProgsTerm: Boolean;
    //CloseTime: Integer;
    TerminateNonVis: Boolean;
    TerminateCloseFail: Boolean;
    AutoExit: Boolean;
  end;
  TRestoreSettings = record
    NoIntro: Boolean;
    UncheckProgs: Boolean;
    ShowProgs: Boolean;
    //ShowProgsWait: Boolean;
    ProgsWait: Boolean;
    AutoExit: Boolean;
    AllowScreenSaver: Boolean;
    AllowTaskSched: Boolean;
  end;

const
  AppVer = '1.3';
  Hive = HKEY_LOCAL_MACHINE;
  CrLf = #13#10;
  CrLf2 = #13#10#13#10;
  crHand = 90;
  RegKey = 'Software\BM-productions\SmartClose';
  CSIDL_STARTMENU = $000B;
  CSIDL_COMMON_STARTMENU = $0016;
  CSIDL_DESKTOPDIRECTORY = $0010;
  CSIDL_COMMON_DESKTOPDIRECTORY = $0019;
  CSIDL_APPDATA = $001A;
  CSIDL_LOCAL_APPDATA = $001C;
  CSIDL_FLAG_CREATE = $8000;
  BadDirChars = '/:*?"<>|';
  msgProgsLoad = 'Loading all currently running programs...';

  FormFont = 'Tahoma';
  FormFont2 = 'Verdana';
  FormFont2Alt = 'Arial';

  OrigBaseUnitX = 6;
  OrigBaseUnitY = 13;

var
  Mode: TWizardMode;
  IsWinNT: Boolean;
  LogFile: String;
  MutexHandle: THandle;

procedure ShowCloseWiz;
procedure ShowRestoreWiz;
procedure ShowConfig;
procedure ShowAbout;
procedure ShowCancel(Show: Boolean);

function GetAppVer: string;
function IsValidName(Name: String): Boolean;
function INIGetGroups(Path: String): String;
function INIGetValue(Path: String; Group: String; Key: String;
    Default: String): String;
procedure INISetValue(Path: String; Group: String; Key: String; Value: String);
function GetClass(Wnd: HWND): String;
function FindWindowLike(hWndStart: HWND; Classname: String): String;
function WindowText(Wnd: HWND): String;
function SpecialMsgBox(Prompt: String; miStyle: TPJMsgDlgIconKind;
    Caption: String; Context: Integer): Boolean;
//procedure WriteDebug(Text: String);
procedure Log(Text: String);
procedure LogLine;
function GetTempDir: string;
function RemoveBackslashUnlessRoot(const S: String): string;
function GenerateUniqueName(Path: String; const Extension: String): String;
function BoolToInt(Val: Boolean): Integer;
function AddQuotes(const S: String): String;
function GetExeFile(CommandLine: String): String;
function GetParams(CommandLine: String): String;
function AddBackslash(const S: String): String;
function ExecuteFile(const FileName, Params, DefaultDir: string;
    ShowCmd: Integer): Boolean;
function GetWinDir: String;
function IIf(Condition: Boolean; TrueVal, FalseVal: String): String;
procedure LoadFuncOrd(LibH: Integer; var P: Pointer; Index: Integer);
//function OpenFilename(Handle: Integer; var OpenFN: TOFN): Boolean;
function ConvertPercentStr(var S: String): Boolean;
function TreeGetRootCaption(Wnd: HWND): String;
procedure LoadCloseSettings(var Settings: TCloseSettings);
procedure LoadRestoreSettings(var Settings: TRestoreSettings);
function StrRetToString(const Ret: TStrRet; IDL: PItemIDList): String;
function StrToDateTime(const aString: String): TDateTime;
function IsFileNameValid(const FileName: String): Boolean;
function IsDirectoryValid(Dir: String): Boolean;
procedure ResourceToFile(ResName, Filename: String);
function CreateLabel(const BaseControl: TControl; const ACaption: String;
    const ATag: Longint; var Y: Integer): TLabel;
procedure EnsureVisible(Item: TListItem);
procedure ShowWait(ShowCancel: Boolean; Status: String);
procedure HideWait;
procedure CloseWait;
function GetProgPathRegistry(Prog: String): String;
function LoadResStr(Instance: HInst; ID: Integer): String;
procedure RegSnapFileAssoc;
procedure UnRegSnapFileAssoc;
function CheckSnapFileAssoc: Boolean;
function GetDllVersion(Name: String): Integer;
function CreateShellLink(const Filename, Description, ShortcutTo, Parameters,
    WorkingDir, IconFilename: String; const IconIndex, ShowCmd: Integer;
    const HotKey: Word): Boolean;
function GetShellFolder(const FolderID: Integer; var PIDL: PItemIDList): String;
function BrowseForFolder(Handle: Integer; Title: String;
    var Path: String): PItemIDList;
function MakeDir(Dir: String): Boolean;
function FormatDir(Path: String): String;
function CheckDirName(Path: String; AllowRoot: Boolean): TCheckDirResult;
function SpaceString(const S: String): String;
procedure ConvertTo32BitImageList(const ImageList: TImageList);
//procedure InitFormFont(Form: TForm);
function SetFontNameSize(const AFont: TFont; const AName: String;
    const ASize: Integer; const AFallbackName: String;
    const AFallbackSize: Integer): Boolean;
procedure SetFormFont(const Form: TForm; var BaseUnitX, BaseUnitY: Integer);
function FormatMulti(Num: Integer; Noun: String): String;
procedure FreeObjStrings(S: TStrings);
function WrapText(S: String; MaxLen: Integer; CommentStr: String): String;
procedure FreeAndNil(var Obj);

implementation

uses Main, CloseWiz, RestoreWiz, frmConfig, frmAbout, Wait, uProcessMemMgr,
    Snapshot;

{type
  PFindWindow = ^TFindWindow;
  TFindWindow = record
    Parent: Integer;
    ClassName: string;
    hWnd: Integer;
  end;}

const
  ID_FOLDERLABEL = $443;
  ID_FORMAT = $470;
  ID_FILETEXT = $480;
  ID_INFO = WM_USER + 1;
  ClassRoot = HKEY_CLASSES_ROOT;
  DefTypeName = 'SmartCloseSnapshot';
  TypeDesc = 'SmartClose System Snapshot';
  CmdName = 'SmartClose';
  CmdDesc = '&Restore with SmartClose';
  IconIdx = 5;
  CmdParams = '/restore "%1"';
  //OFNFilter = 'SmartClose System Snapshots (*.s3)'#0'*.s3'#0;
  IDC_HAND = 32649;

var
  Hook: HHOOK;
  MsgBoxResult: Boolean;
  ChkWnd: Integer;
  //OldDialogProc: Pointer;
  Shlwapi: THandle = 0;
  FuncLoaded: Boolean = False;
  StrRetToStrW: function(str: PStrRet; pidl: PItemIDList; var Name: PWideChar):
      HRESULT; stdcall = nil;
  {MainMutexHandle: THandle;
  CloseMutexHandle: THandle;
  RestoreMutexHandle: THandle;
  ConfigMutexHandle: THandle;}

procedure ShowCloseWiz;
begin
  {$IFDEF Debug}
  OutputDebugString('showclosewiz enter');
  {$ENDIF}
  //CloseMutexHandle := CreateMutex(NIL, TRUE, 'SmartCloseWiz');
  //if GetLastError <> ERROR_ALREADY_EXISTS then begin
    Application.CreateForm(TfrmCloseWiz, frmCloseWiz);
    frmCloseWiz.ParseParams;
    frmCloseWiz.Show;
  //end;
  {$IFDEF Debug}
  OutputDebugString('showclosewiz exit');
  {$ENDIF}
end;

procedure ShowRestoreWiz;
begin
  Application.CreateForm(TfrmRestoreWiz, frmRestoreWiz);
  frmRestoreWiz.ParseParams;
  frmRestoreWiz.Show;
end;

procedure ShowConfig;
begin
  {ConfigMutexHandle := CreateMutex(NIL, TRUE, 'SmartCloseConfig');
  try
    if GetLastError <> ERROR_ALREADY_EXISTS then}
      //Application.CreateForm(TfrmConfig, frmConfig);
  {finally
    ReleaseMutex(ConfigMutexHandle)
  end;}
  Application.CreateForm(TConfig, Config);
  Config.Show;
end;

procedure ShowAbout;
begin
  Application.CreateForm(TAboutFrm, AboutFrm);
  AboutFrm.Show;
end;

procedure ShowCancel(Show: Boolean);
begin
  if Show then begin
    frmWait.Height := frmWait.ScalePixelsY(107);
    frmWait.btnCancel.Visible := True;
  end
  else begin
    frmWait.Height := frmWait.ScalePixelsY(91);
    frmWait.btnCancel.Visible := False;
  end;
end;

function GetAppVer: String;
var
  RegVersion: string;
  {VHandle : DWORD;
  VSize   : DWORD;
  VBuffer : String;
  Str: String;}
begin
  Result := AppVer;
  RegVersion := GetKeyValue(Hive, RegKey, 'Version', '');
  if Trim(RegVersion) <> '' then
    Result := Result + ' ' + RegVersion;
  {VSize := GetFileVersionInfoSize(PChar(ParamStr(0)), VHandle);
  if VSize > 0 then begin
    SetLength(VBuffer, VSize);
    GetFileVersionInfo(PChar(ParamStr(0)), VHandle, VSize, PChar(VBuffer));
    VerQueryValue(PChar(VBuffer), PChar('\StringFileInfo\040904E4\FileVersion'),
        Pointer(Str), VSize);
    Result := StrPas(PChar(Str));
    Result := Copy(Result, 1, 3)
  end;}
end;

function IsValidName(Name: String): Boolean;
var
  i: Longint;
begin
  Result := False;
  if Trim(Name) = '' then
    Exit;
  if Name[1] in [' ', '.'] then
    Exit;
  if Name[Length(Name)] in [' ', '.'] then
    Exit;
  if LastDelimiter(BadDirChars + '\', Name) <> 0 then
    Exit;
  for i := 1 to Length(Name) do begin
    // Check for invalid characters
    if Name[i] in [Chr(0)..Chr(31)] then
      Exit;
  end;
  Result := True;
end;

function RemoveExtraBackslashes(const S: String): String;
var
  I: Integer;
begin
  Result := S;
  I := 1;
  while I < Length(Result) do begin
    if IsLeadByte(Result[I]) then
      Inc(I, 2)
    else if (Result[I] = '\') and (Result[I+1] = '\') then
      Delete(Result, I+1, 1)
    else
      Inc(I);
  end;
end;

function FormatDir(Path: String): String;
var
  IsUNCPath: Boolean;
begin
  Path := Trim(Path);
  IsUNCPath := (Length(Path) >= 2) and (Path[1] = '\') and (Path[2] = '\');
  if not IsUNCPath then
    Path := RemoveExtraBackslashes(Path)
  else
    Path := '\\' + RemoveExtraBackslashes(Copy(Path, 3, Maxint));
  Result := RemoveBackslashUnlessRoot(Path);
end;

function CheckDirName(Path: String; AllowRoot: Boolean): TCheckDirResult;
var
  IsUNCPath: Boolean;
  I: Integer;
  RootPath: String;
begin
  Result := cdnInvalid;
  IsUNCPath := (Length(Path) >= 2) and (Path[1] = '\') and (Path[2] = '\');
  if not IsUNCPath then begin
    if not AllowRoot then
      I := 4
    else
      I := 3;
    if (Length(Path) < I) or not (UpCase(Path[1]) in ['A'..'Z']) or
        (Path[2] <> ':') or (Path[3] <> '\') then
      Exit;
  end
  else begin
    if StrScan(@Path[3], '\') = nil then
      Exit;
  end;
  Result := cdnBadChars;
  if LastDelimiter(BadDirChars, Copy(Path, 3, Maxint)) <> 0 then
    Exit;
  Result := cdnInvalidDrive;
  RootPath := AddBackslash(ExtractFileDrive(Path));
  if not DirectoryExists(RootPath) then
    Exit;
  Result := cdnValid;
end;

function INIGetGroups(Path: String): String;
var
  x: LongInt;
  sBuff: String;
begin
  SetLength(sBuff, 512);
  x := GetPrivateProfileString(nil, nil, '', PChar(sBuff), Length(sBuff),
      PChar(Path));
  Result := Trim(Copy(sBuff, 1, x));
end;

function INIGetValue(Path: String; Group: String; Key: String;
    Default: String): String;
var
  x: LongInt;
  sBuff: String;
begin
  SetLength(sBuff, 512);
  x := GetPrivateProfileString(PChar(Group), PChar(Key), PChar(Default),
      PChar(sBuff), Length(sBuff), PChar(Path));
  Result := Trim(Copy(sBuff, 1, x));
end;

procedure INISetValue(Path: String; Group: String; Key: String; Value: String);
begin
  WritePrivateProfileString(PChar(Group), PChar(Key), PChar(Value), PChar(Path));
end;

function GetClass(Wnd: HWND): String;
begin
  Result := StringOfChar(' ', MAX_PATH);
  GetClassName(Wnd, PChar(Result), MAX_PATH);
  Result := Trim(Result);
end;

function FindWindowLike(hWndStart: HWND; Classname: String): String;
var
  Wnd: Integer;
begin
  // TODO: use EnumWindows/EnumChildWindows instead to prevent infinite loop (see Windows SDK)
  Wnd := GetWindow(hWndStart, GW_CHILD);
  while Wnd > 0 do begin
    Result := FindWindowLike(Wnd, ClassName);
    if GetClass(Wnd) = Classname then begin
      Result := IntToStr(Wnd);
      Exit;
    end;
    Wnd := GetWindow(Wnd, GW_HWNDNEXT);
  end;
end;

{function EnumChildProc(H: HWnd; Data: Pointer): Bool; stdcall;
var
  PFW: PFindWindow;
begin
  Result := True;
  PFW := PFindWindow(Data);
  if (GetParent(H) = PFW.Parent) and (GetClass(H) = PFW.ClassName) then begin
    PFW.hWnd := H;
    Result := False;
  end;
end;}

{function FindChildWindow(hWnd: Integer; ChildClass: String): Integer;
var
  TFW: TFindWindow;
begin
  TFW.Parent := hWnd;
  TFW.ClassName := ChildClass;
  TFW.hWnd := 0;
  EnumChildWindows(hWnd, @EnumChildProc, Longint(@TFW));
  Result := TFW.hWnd;
end;}

function WindowText(Wnd: HWND): String;
var
  TextLen: Longint;
begin
  TextLen := SendMessage(Wnd, WM_GETTEXTLENGTH, 0, 0);
  Result := StringOfChar(#0, TextLen);
  SendMessage(Wnd, WM_GETTEXT, TextLen + 1, Integer(Pointer(Result)));
  {Result := StringOfChar(' ', MAX_PATH);
  GetWindowText(Wnd, PChar(Result), MAX_PATH);
  Result := Trim(Result);}
end;

function MsgBoxHook(nCode: Integer; wParam: WPARAM;
    lParam: LPARAM): Integer; stdcall;
var
  pcwp: PCWPStruct;
  hStatic: Integer;
  Static: TRect;
  Font: HFONT;
  hButton: Integer;
  Button: TRect;
  Dialog: TRect;
begin
  Result := CallNextHookEx(Hook, nCode, wParam, lParam);
  if nCode >= 0 then begin
    pcwp := PCWPStruct(lParam);
    case pcwp.message of
      WM_INITDIALOG: begin
        hStatic := FindWindowEx(pcwp.hwnd, 0, 'Static', nil);
        hStatic := GetWindow(hStatic, GW_HWNDNEXT);
        Font := SendMessage(hStatic, WM_GETFONT, 0, 0);
        GetWindowRect(hStatic, Static);
        ScreenToClient(pcwp.hwnd, Static.TopLeft);
        ScreenToClient(pcwp.hwnd, Static.BottomRight);
        hButton := FindWindowEx(pcwp.hwnd, 0, 'Button', nil);
        GetWindowRect(hButton, Button);
        ScreenToClient(pcwp.hwnd, Button.TopLeft);
        ScreenToClient(pcwp.hwnd, Button.BottomRight);
        SetWindowPos(hButton, 0, Button.Left, Static.Bottom + 40, 0, 0,
            SWP_NOSIZE or SWP_NOZORDER);
        hButton := GetWindow(hButton, GW_HWNDNEXT);
        if hButton <> 0 then begin
          GetWindowRect(hButton, Button);
          ScreenToClient(pcwp.hwnd, Button.TopLeft);
          ScreenToClient(pcwp.hwnd, Button.BottomRight);
          SetWindowPos(hButton, 0, Button.Left, Static.Bottom + 40, 0, 0,
              SWP_NOSIZE or SWP_NOZORDER);
        end;
        //Font := frmMain.Font.Handle;
        ChkWnd := CreateWindowEx(0, 'Button',
            '&Don''t display this warning message again',
            WS_CHILD or WS_TABSTOP or BS_AUTOCHECKBOX,
            Static.Left, Static.Bottom + 10, 400, 15, pcwp.hwnd, 0, hInstance,
            nil);
        GetWindowRect(pcwp.hwnd, Dialog);
        SetWindowPos(pcwp.hwnd, 0, 0, 0, Dialog.Right - Dialog.Left,
            Static.Bottom + 40 + Button.Bottom - Button.Top + 40,
            SWP_NOMOVE or SWP_NOZORDER);
        SendMessage(ChkWnd, WM_SETFONT, Font, 0);
        SendMessage(ChkWnd, BM_SETCHECK, Integer(True), 0);
        ShowWindow(ChkWnd, SW_SHOWNORMAL);
      end;
      WM_DESTROY: begin
        if GetClass(pcwp.hwnd) <> '#32770' then
          Exit;
        MsgBoxResult := (SendMessage(ChkWnd, BM_GETCHECK, 0, 0) = 1);
      end;
    end;
  end;
end;

function SpecialMsgBox(Prompt: String; miStyle: TPJMsgDlgIconKind;
    Caption: String; Context: Integer): Boolean;
begin
  MsgBoxResult := True;
  Hook := SetWindowsHookEx(WH_CALLWNDPROC, @MsgBoxHook, 0, GetCurrentThreadId);
  MsgBox(Prompt, bgOK, miStyle, Caption, Context);
  UnhookWindowsHookEx(Hook);
  Result := MsgBoxResult;
end;

{procedure WriteDebug(Text: String);
var
  F: TextFile;
begin
  AssignFile(F, 'SmartCloseDebug.txt');
  if FileExists('SmartCloseDebug.txt') then
    Append(F)
  else
    Rewrite(F);
  Writeln(F, TimeToStr(Time) + ' ' + Text);
  CloseFile(F);
end;}

{procedure WriteDebug(Text: String);
begin
  OutputDebugString(PChar(Text));
end;}

{procedure WriteDebug(Text: String);
var
  FName: string;
  F: TextFile;
begin
  FName := ExtractFilePath(ParamStr(0)) + 'SmartCloseDebug.txt';
  AssignFile(F, FName);
  if FileExists(FName) then
    Append(F)
  else
    Rewrite(F);
  Writeln(F, TimeToStr(Time) + ' ' + Text);
  CloseFile(F);
end;}

procedure Log(Text: String);
begin
  //WriteDebug(Text);
  LogFile := LogFile + TimeToStr(Time) + '     ' + Text + CrLf;
end;

procedure LogLine;
begin
  LogFile := LogFile + CrLf;
end;

function RemoveBackslashUnlessRoot(const S: String): String;
{ Removes the trailing backslash from the string, if one exists and does
  not specify a root directory of a drive (i.e. "C:\"}
var
  L: Integer;
begin
  Result := S;
  L := Length(Result);
  if L < 2 then
    Exit;
  if (AnsiLastChar(Result)^ = '\') and
     ((Result[L-1] <> ':') or (ByteType(Result, L-1) <> mbSingleByte)) then
    SetLength(Result, L-1);
end;

function InternalGetFileAttr(const Name: String): Integer;
var
  OldErrorMode: UINT;
begin
  OldErrorMode := SetErrorMode(SEM_FAILCRITICALERRORS);  { Prevent "Network Error" boxes }
  try
    Result := GetFileAttributes(PChar(RemoveBackslashUnlessRoot(Name)));
  finally
    SetErrorMode (OldErrorMode);
  end;
end;

function AdjustLength(var S: String; const Res: Cardinal): Boolean;
{ Returns True if successful. Returns False if buffer wasn't large enough,
  and called AdjustLength to resize it. }
begin
  Result := Integer(Res) < Length(S);
  SetLength(S, Res);
end;

function GetEnv(const EnvVar: String): String;
{ Gets the value of the specified environment variable. (Just like TP's GetEnv) }
var
  Res: DWORD;
begin
  SetLength(Result, 255);
  repeat
    Res := GetEnvironmentVariable(PChar(EnvVar), PChar(Result), Length(Result));
    if Res = 0 then begin
      Result := '';
      Break;
    end;
  until AdjustLength(Result, Res);
end;

function AddBackslash(const S: String): String;
{ Adds a trailing backslash to the string, if one wasn't there already.
  But if S is an empty string, the function returns an empty string. }
begin
  Result := S;
  if (Result <> '') and (AnsiLastChar(Result)^ <> '\') then
    Result := Result + '\';
end;

function GetWinDir: String;
{ Returns fully qualified path of the Windows directory. Only includes a
  trailing backslash if the Windows directory is the root directory. }
var
  Buf: array[0..MAX_PATH-1] of Char;
begin
  GetWindowsDirectory(Buf, SizeOf(Buf));
  Result := StrPas(Buf);
end;

function GetTempDir: String;
begin
  Result := GetEnv('TMP');
  if (Result = '') or not DirectoryExists(Result) then
    Result := GetEnv('TEMP');
  if (Result = '') or not DirectoryExists(Result) then
    Result := GetWinDir;
  Result := AddBackslash(ExpandFileName(Result));
end;

function FileOrDirExists(const Name: String): Boolean;
{ Returns True if the specified directory or file name exists. The specified
  name may include a trailing backslash. }
begin
  Result := InternalGetFileAttr(Name) <> -1;
end;

function GenerateUniqueName(Path: String; const Extension: String): String;
  function IntToBase32(Number: Longint): String;
  const
    Table: array[0..31] of Char = '0123456789ABCDEFGHIJKLMNOPQRSTUV';
  var
    I: Integer;
  begin
    Result := '';
    for I := 0 to 4 do begin
      Insert(Table[Number and 31], Result, 1);
      Number := Number shr 5;
    end;
  end;
var
  Rand, RandOrig: Longint;
begin
  Path := AddBackslash(Path);
  RandOrig := Random($2000000);
  Rand := RandOrig;
  repeat
    Inc(Rand);
    if Rand > $1FFFFFF then
      Rand := 0;
    { Generate a random name }
    Result := Path + 'sc_' + IntToBase32(Rand) + Extension;
  until not FileOrDirExists(Result);
end;

function BoolToInt(Val: Boolean): Integer;
begin
  if Val then
    Result := 1
  else
    Result := 0;
end;

function AddQuotes(const S: String): String;
begin
  Result := Trim(S);
  if Result <> '' then begin
    if ((Result[1] <> '"') or (AnsiLastChar(Result)^ <> '"')) then
      Result := '"' + Result + '"';
  end;
end;

function GetExeFile(CommandLine: String): String;
var
  Pos: Longint;
begin
  if CommandLine = '' then
    Exit;
  Result := CommandLine;
  if FileExists(Result) then
    Exit;
  if Result[1] = '"' then begin
    Delete(Result, 1, 1);
    Pos := AnsiPos('"', Result);
  end
  else begin
    Pos := AnsiPos(' ', Result);
    if Pos = 0 then
      Pos := Length(Result) + 1;
  end;
  if Pos > 0 then
    Result := Copy(Result, 1, Pos - 1);
  Result := Trim(Result);
end;

function GetParams(CommandLine: String): String;
var
  Pos: Longint;
begin
  if CommandLine = '' then
    Exit;
  Pos := 0;
  if CommandLine[1] = '"' then begin
    Delete(CommandLine, 1, 1);
    Pos := AnsiPos('"', CommandLine) + 1
  end
  else if FileExists(CommandLine) = False then
    Pos := AnsiPos(' ', CommandLine);
  if Pos > 0 then
    Result := Copy(CommandLine, Pos, Length(CommandLine) - Pos + 1);
  Result := Trim(Result);
end;

{function DoEnvSubst(S: String): String;
var
  Len: Integer;
begin
  Result := '';
  if Trim(S) = '' then
    Exit;
  Len := ExpandEnvironmentStrings(PChar(S), nil, 0);
  SetLength(Result, Len);
  ExpandEnvironmentStrings(PChar(S), PChar(Result), Len);
end;}

function DoEnvSubst(const Value: String): String;
var
  Buf: array[0..1023] of Char;
  Res: DWORD;
begin
  Res := ExpandEnvironmentStrings(PChar(Value), Buf, 1024);
  if Res = 0 then
    Result := Value
  else
    Result := Trim(Buf);
end;

function GetProgPathRegistry(Prog: String): String;
begin
  Result := DoEnvSubst(GetKeyValue(HKEY_LOCAL_MACHINE,
      'SOFTWARE\Microsoft\Windows\CurrentVersion\App Paths\' + Prog, '', ''));
  if Trim(Result) = '' then
    Result := Prog;
end;

function ExecuteFile(const FileName, Params, DefaultDir: String;
    ShowCmd: Integer): Boolean;
var
  Sei: TShellExecuteInfo;
begin
  ZeroMemory(@Sei, SizeOf(Sei));
  Sei.cbSize := SizeOf(Sei);
  Sei.fMask := SEE_MASK_DOENVSUBST {or SEE_MASK_NOCLOSEPROCESS}
      or SEE_MASK_FLAG_NO_UI;
  Sei.Wnd := 0;
  Sei.lpFile := PChar(FileName);
  Sei.lpParameters := PChar(Params);
  Sei.lpDirectory := PChar(DefaultDir);
  Sei.nShow := ShowCmd;
  Result := ShellExecuteEx(@Sei);
  //CloseHandle(Sei.hProcess);
  //Result := ShellExecute(0, nil, PChar(FileName), PChar(Params),
  //  PChar(DefaultDir), ShowCmd) > 32;
end;

function TreeGetRootCaption(Wnd: HWND): String;
var
  MM: TProcessMemMgr;
  Item: hTreeItem;
  TVItem: TTVItem;
  // following are pointers into the possibly foreign process
  PrTVItem: PTVItem;
  PrText: PChar;
begin
  MM := CreateProcessMemMgrForWnd(Wnd);
  try
    PrTVItem := MM.AllocMem(SizeOf(TTVItem));
    PrText := MM.AllocMem(MAX_PATH);
    TVItem.mask := TVIF_TEXT;
    TVItem.pszText := PrText;
    TVItem.cchTextMax := MAX_PATH;
    Item := TreeView_GetRoot(Wnd);
    TVItem.hItem := Item;
    MM.Write(TVItem, PrTVItem, SizeOf(TTVItem));
    TreeView_GetItem(Wnd, PrTVItem^);
    Result := MM.ReadStr(PrText);
  finally
    MM.Free;
  end;
end;

function IIf(Condition: Boolean; TrueVal, FalseVal: String): String;
begin
  if Condition then
    Result := TrueVal
  else
    Result := FalseVal;
end;

procedure LoadFuncOrd(LibH: Integer; var P: Pointer; Index: Integer);
const
  NotFoundMsg = 'Function with ordinal %d not found';
begin
  P := GetProcAddress(LibH, PChar(Index));
  //if P = nil then Raise(Exception.CreateFmt(nf, [index]));
end;

function StrToIntArray(const aString: String; var aIntegers;
    aMaxCount: Integer): Integer;
const
  csDigits = ['0'..'9'];
var
  p: PInteger;
  pos: Integer;
  start: Integer;
begin
  FillChar(aIntegers, aMaxCount * SizeOf(Integer), 0);
  p := PInteger(@aIntegers);
  pos := 1;
  Result := 0;
  while (pos <= Length(aString)) and (Result < aMaxCount) do begin
    while (pos <= Length(aString))
        and not (aString[pos] in (csDigits + ['-'])) do
      Inc(pos);
    start := pos;
    if (pos <= Length(aString)) and (aString[pos] = '-') then
      Inc(pos);
    while (pos <= Length(aString)) and (aString[pos] in csDigits) do
      Inc(pos);
    if pos > start then begin
      p^ := StrToInt(System.Copy(aString, start, pos - start));
      Inc(p);
      Inc(Result);
    end;
  end;
end;

function StrToDateTime(const aString: String): TDateTime;
var
  a: array[1..6] of Integer;

  procedure AdjustHour(i: Integer);
  begin
    if System.Pos('PM', UpperCase(aString)) > 0 then
      Inc(a[i], 12);
  end;

begin
  StrToIntArray(aString, a, 6);
  if System.Pos('/', aString) > 0 then begin
    AdjustHour(4);
    Result := EncodeDate(a[3], a[1], a[2]) + EncodeTime(a[4], a[5], a[6], 0);
  end
  else begin
    AdjustHour(1);
    Result := EncodeTime(a[1], a[2], a[3], 0);
  end;
end;

function IsFileNameValid(const FileName: String): Boolean;
var
  i: Integer;
begin
  Result := False;
  if Trim(FileName) = '' then
    Exit;
  if FileName[1] in [' ', '.'] then
    Exit;
  if FileName[Length(FileName)] in [' ', '.'] then
    Exit;
  for i := 1 to Length(FileName) do begin
    // Check for invalid characters
    if FileName[i] in
      [Chr(0)..Chr(31), {'\', '/', ':',} '*', '?', '"', '<', '>', '|'] then
    Exit;
   end;
  Result := True;
end;

function IsDirectoryValid(Dir: String): Boolean;
const
  BadDirChars = '/*?"<>|';
begin
  Result := False;
  Dir := Trim(Dir);
  if Dir = '' then
    Exit;
  if Length(Dir) < 3 then
    Exit;
  if LastDelimiter(BadDirChars, Copy(Dir, 4, Maxint)) <> 0 then
    Exit;
  Result := True;
end;

{function GetFilePath(Handle: HWND): String;
var
  Path: array[0..MAX_PATH] of Char;
begin
  SendMessage(Handle, CDM_GETFILEPATH, SizeOf(Path), Integer(@Path));
  Result := StrPas(Path);
end;}

function ConvertPercentStr(var S: String): Boolean;
{ Expands all %-encoded characters in the string (see RFC 2396). Returns True
  if all were successfully expanded. }
var
  I, C, E: Integer;
  N: String;
begin
  Result := True;
  I := 1;
  while I <= Length(S) do begin
    if S[I] = '%' then begin
      N := Copy(S, I, 3);
      if Length(N) <> 3 then begin
        Result := False;
        Break;
      end;
      N[1] := '$';
      Val(N, C, E);
      if E <> 0 then begin
        Result := False;
        Break;
      end;
      { delete the two numbers following '%', and replace '%' with the character }
      Delete(S, I+1, 2);
      S[I] := Chr(C);
    end;
    if S[I] = '/' then
      S[I] := '\';
    Inc (I);
  end;
end;

procedure FreeAndNil(var Obj);
var
  P: TObject;
begin
  P := TObject(Obj);
  TObject(Obj) := nil;  // clear the reference before destroying the object
  P.Free;
end;

function StrRetToString(const Ret: TStrRet; IDL: PItemIDList): String;
var
  p: PWideChar;
begin
  Result := '';
  if not FuncLoaded then begin
    FuncLoaded := True;
    Shlwapi := LoadLibrary('shlwapi.dll');
    if Shlwapi <> 0 then
      @StrRetToStrW := GetProcAddress(Shlwapi, 'StrRetToStrW');
  end;
  if @StrRetToStrW <> nil then begin
    StrRetToStrW(@Ret, IDL, p);
    try
      Result := p;
    finally
      CoTaskMemFree(p);
    end;
  end
  else begin
    case Ret.uType of
      STRRET_OFFSET:
        Result := PChar(UINT(IDL) + Ret.uOffset);
      STRRET_CSTR:
        Result := Ret.cStr;
      STRRET_WSTR: begin
        Result := WideCharToString(Ret.pOleStr);
        CoTaskMemFree(Ret.pOleStr);
      end;
    end;
  end;
end;

procedure ResourceToFile(ResName, Filename: String);
var
  RS: TResourceStream;
  FS: TFileStream;
begin
  RS := TResourceStream.Create(hInstance, ResName, RT_RCDATA);
  try
    FS := TFileStream.Create(Filename, fmCreate or fmShareDenyWrite);
    try
      FS.CopyFrom(RS, soFromBeginning);
    finally
      FS.Free;
    end;
  finally
    RS.Free;
  end;
end;

function CreateLabel(const BaseControl: TControl; const ACaption: String;
    const ATag: Longint; var Y: Integer): TLabel;
begin
  Result := TLabel.Create(BaseControl.Owner);
  with Result do begin
    Parent := BaseControl.Parent;
    AutoSize := False;
    Caption := ACaption;
    Left := BaseControl.Left;
    Top := Y;
    Width := BaseControl.Width;
    Height := BaseControl.Height;
    Tag := ATag;
    Y := Y + 25;
  end;
end;

procedure EnsureVisible(Item: TListItem);
var
  Cnt: Integer;
begin
  Cnt := 3;
  while Item.Index + Cnt >= Item.Owner.Count do begin
    Cnt := Cnt - 1;
    if Cnt = 0 then
      Break;
  end;
  Item.Owner[Item.Index + Cnt].MakeVisible(False);
end;

function LoadResStr(Instance: HInst; ID: Integer): String;
var
  Buffer: array[Byte] of Char;
begin
  SetString(Result, Buffer, LoadString(Instance, ID, Buffer, SizeOf(Buffer)));
end;

function GetDllVersion(Name: String): Integer;
type
  PDllVersionInfo = ^TDllVersionInfo;
  TDllVersionInfo = record
    cbSize          : DWORD;
    dwMajorVersion  : DWORD;
    dwMinorVersion  : DWORD;
    dwBuildNumber   : DWORD;
    dwPlatformID    : DWORD;
  end;
  TDllGetVersion = function(pdvi: PDllVersionInfo): HRESULT; stdcall;
var
  LibH: HMODULE;
  DllGetVersion: TDllGetVersion;
  DVI: TDllVersionInfo;
begin
  Result := 0;
  LibH := LoadLibrary(PChar(Name));
  if LibH = 0 then
    Exit;
  try
    @DllGetVersion := GetProcAddress(LibH, 'DllGetVersion');
    if @DllGetVersion = nil then
      Exit;
    FillChar(DVI, SizeOf(DVI), 0);
    DVI.cbSize := SizeOf(DVI);
    if DllGetVersion(@DVI) = NOERROR then
      Result := DVI.dwMajorVersion * 100 + DVI.dwMinorVersion;
  finally
    FreeLibrary(LibH);
  end;
end;

function GetShellFolder(const FolderID: Integer; var PIDL: PItemIDList): String;
var
  Buffer: array[0..MAX_PATH-1] of Char;
begin
  Result := '';
  PIDL := nil;
  if SUCCEEDED(SHGetSpecialFolderLocation(0, FolderID, PIDL)) then begin
    if SHGetPathFromIDList(PIDL, Buffer) then
      Result := Buffer;
  // TODO: uncomment and clone PIDL instead of passing ownership
  {finally
    CoTaskMemFree(pidl);}
  end;
end;

function BrowseCallback(Wnd: HWND; MessageID: UINT;
    Param, Data: LPARAM): Integer; stdcall;
var
  Buffer: array[0..MAX_PATH] of Char;
  FsDir: Boolean;
begin
  case MessageID of
    BFFM_INITIALIZED: begin
      if (Data <> 0) and DirectoryExists(PChar(Data)) then begin
        SendMessage(Wnd, BFFM_SETSELECTION, Integer(True), Data);
        //CoTaskMemFree(Pointer(Data));
      end;
    end;
    BFFM_SELCHANGED: begin
      FsDir := False;
      if SHGetPathFromIDList(Pointer(Param), Buffer) then begin
        if DirectoryExists(Buffer) then
          FsDir := True;
      end;
      SendMessage(Wnd, BFFM_ENABLEOK, 0, Integer(FsDir));
    end;
  end;
  Result := 0;
end;

function BrowseForFolder(Handle: Integer; Title: String;
    var Path: String): PItemIDList;
const
  BIF_EDITBOX = $10;
  BIF_NEWDIALOGSTYLE = $40;
var
  BrowseInfo: TBrowseInfo;
  DisplayName: array[0..MAX_PATH] of Char;
begin
  FillChar(BrowseInfo, SizeOf(BrowseInfo), 0);
  BrowseInfo.hwndOwner := Handle;
  BrowseInfo.pidlRoot := nil;
  BrowseInfo.pszDisplayName := @DisplayName[0];
  BrowseInfo.lpszTitle := PChar(Title);
  BrowseInfo.ulFlags := BIF_RETURNONLYFSDIRS or BIF_EDITBOX or BIF_NEWDIALOGSTYLE;
  BrowseInfo.lpfn := BrowseCallback;
  BrowseInfo.lParam := Integer(PChar(Path));
  BrowseInfo.iImage := 0;
  Result := SHBrowseForFolder(BrowseInfo);
  Path := '';
  //CoTaskMemFree(SelPIDL);
  if Result = nil then
    Exit;
  //try
    if SHGetPathFromIDList(Result, DisplayName) then
      Path := DisplayName;
  {finally
    CoTaskMemFree(PIDL)
  end;}
end;

procedure LoadCloseSettings(var Settings: TCloseSettings);
var
  Registry: TMyRegistry;
begin
  {$IFDEF Debug}
  OutputDebugString('LoadCloseSettings enter');
  {$ENDIF}
  Registry := TMyRegistry.Create;
  try
    Registry.RootKey := Hive;
    Registry.OpenKey(RegKey, True);
    with Settings do begin
      NoIntro := Registry.ReadBoolDef('NoIntro', False);
      ShowProgs := Registry.ReadBoolDef('ShowProgs', True);
      ShowProgsTerm := Registry.ReadBoolDef('ShowProgsTerm', False);
      //CloseTime := Registry.ReadIntegerDef('ProgCloseTime', 1);
      TerminateNonVis := Registry.ReadBoolDef('TerminateNonVis', True);
      TerminateCloseFail := Registry.ReadBoolDef('TerminateCloseFail', False);
      AutoExit := Registry.ReadBoolDef('AutoExit', True);
    end;
  finally
    Registry.Free;
  end;
  {$IFDEF Debug}
  OutputDebugString('LoadCloseSettings exit');
  {$ENDIF}
end;

procedure LoadRestoreSettings(var Settings: TRestoreSettings);
var
  Registry: TMyRegistry;
begin
  Registry := TMyRegistry.Create;
  try
    Registry.RootKey := Hive;
    Registry.OpenKey(RegKey, True);
    with Settings do begin
      UncheckProgs := Registry.ReadBoolDef('UncheckProgs', False);
      NoIntro := Registry.ReadBoolDef('Res_NoIntro', False);
      ShowProgs := Registry.ReadBoolDef('ShowRestoreProgs', True);
      //ShowProgsWait := Registry.ReadBoolDef('ShowProgsWait', False);
      ProgsWait := Registry.ReadBoolDef('ProgsWait', True);
      AutoExit := Registry.ReadBoolDef('Res_AutoExit', True);
      AllowScreenSaver := Registry.ReadBoolDef('AllowSS', False);
      AllowTaskSched := Registry.ReadBoolDef('AllowTaskSched', False);
    end;
  finally
    Registry.Free;
  end;
end;

procedure ShowWait(ShowCancel: Boolean; Status: String);
begin
  WaitCancel := False;
  if frmWait = nil then
    Application.CreateForm(TfrmWait, frmWait);
  if ShowCancel then begin
    frmWait.Height := frmWait.ScalePixelsY(119);
    frmWait.btnCancel.Visible := True;
  end
  else begin
    frmWait.Height := frmWait.ScalePixelsY(100);
    frmWait.btnCancel.Visible := False;
  end;
  frmWait.lblStatus.Caption := Status;
  frmWait.Show;
  Application.ProcessMessages;
end;

procedure HideWait;
begin
  frmWait.Hide;
  {frmWait.Release;
  frmWait := nil;}
end;

procedure CloseWait;
begin
  with frmWait do begin
    Hide;
    Close;
    Free;
  end;
  frmWait := nil;
end;

function RegDeleteKeyIncludingSubkeys(const Key: HKEY; const Name: String): Longint;
{ Deletes the specified key and all subkeys.
  Returns ERROR_SUCCESS if the key was successfully deleted. }
var
  H: HKEY;
  KeyName: String;
  KeyNameCount, MaxCount: DWORD;
  FT: TFileTime;
  I: Integer;
begin
  if Win32Platform = VER_PLATFORM_WIN32_NT then begin
    if RegOpenKeyEx(Key, PChar(Name), 0,
        KEY_ENUMERATE_SUB_KEYS or KEY_QUERY_VALUE, H) = ERROR_SUCCESS then begin
      if RegQueryInfoKey(H, nil, nil, nil, nil, @MaxCount, nil, nil, nil, nil,
          nil, nil) = ERROR_SUCCESS then begin
        if MaxCount < 1 then
          MaxCount := 1;
        SetLength(KeyName, MaxCount);
        I := 0;
        while True do begin
          KeyNameCount := MaxCount + 1;
          if RegEnumKeyEx(H, I, PChar(KeyName), KeyNameCount, nil, nil, nil,
              @FT) <> ERROR_SUCCESS then
            Break;
          if RegDeleteKeyIncludingSubkeys(H, PChar(KeyName)) <> ERROR_SUCCESS then
            Inc (I);
        end;
      end;
      RegCloseKey (H);
    end;
  end;
  Result := RegDeleteKey(Key, PChar(Name));
end;

function RegDeleteKeyIfEmpty(const RootKey: HKEY; const SubkeyName: String): Longint;
{ Deletes the specified subkey if it has no subkeys or values.
  Returns ERROR_SUCCESS if the key was successful deleted, ERROR_DIR_NOT_EMPTY
  if it was not deleted because it contained subkeys or values, or possibly
  some other Win32 error code. }
var
  K: HKEY;
  NumSubkeys, NumValues: DWORD;
begin
  Result := RegOpenKeyEx(RootKey, PChar(SubkeyName), 0, KEY_QUERY_VALUE, K);
  if Result <> ERROR_SUCCESS then
    Exit;
  Result := RegQueryInfoKey(K, nil, nil, nil, @NumSubkeys, nil, nil,
      @NumValues, nil, nil, nil, nil);
  RegCloseKey(K);
  if Result <> ERROR_SUCCESS then
    Exit;
  if (NumSubkeys = 0) and (NumValues = 0) then
    Result := RegDeleteKey(RootKey, PChar(SubkeyName))
  else
    Result := ERROR_DIR_NOT_EMPTY;
end;

function RegKeyExists(const RootKey: Cardinal; const SubKeyName: String): Boolean;
var
  K: HKEY;
begin
  Result := False;
  if RegOpenKeyEx(RootKey, PChar(SubkeyName), 0, KEY_QUERY_VALUE,
      K) = ERROR_SUCCESS then begin
    RegCloseKey(K);
    Result := True;
  end;
end;

procedure RegSnapFileAssoc;

  procedure SetClassKeyValue(const Subkey: String; const Data: String);
    procedure Check(const Res: Longint);
    begin
      if Res <> ERROR_SUCCESS then begin
        raise Exception.CreateFmt('Error creating file association:'#13#10'%d - %s',
            [Res, SysErrorMessage(Res)]);
      end;
    end;
  var
    K: HKEY;
    Disp: DWORD;
  begin
    Check(RegCreateKeyEx(ClassRoot, PChar(Subkey), 0, nil, 0, KEY_SET_VALUE, nil,
        K, @Disp));
    try
      Check(RegSetValueEx(K, nil, 0, REG_SZ, PChar(Data), Length(Data)+1));
    finally
      RegCloseKey(K);
    end;
  end;
  
var
  ExeName: String;
  TypeName: String;
begin
  ExeName := ParamStr(0);
  TypeName := GetKeyValue(ClassRoot, SnapshotExt, '', '');
  if Trim(TypeName) = '' then begin
    TypeName := DefTypeName;
    SetClassKeyValue(SnapshotExt, TypeName);
  end;
  SetClassKeyValue(TypeName, TypeDesc);
  SetClassKeyValue(TypeName + '\DefaultIcon', ExeName + ',' + IntToStr(IconIdx));
  SetClassKeyValue(TypeName + '\shell', CmdName);
  SetClassKeyValue(TypeName + '\shell\' + CmdName + '\command',
      '"' + ExeName + '" ' + CmdParams);
  SetClassKeyValue(TypeName + '\shell\' + CmdName, CmdDesc);
  SHChangeNotify(SHCNE_ASSOCCHANGED, SHCNF_IDLIST, nil, nil);
end;

function ClassKeyValueEquals(const Subkey: String; const Data: String): Boolean;
var
  S: String;
begin
  Result := False;
  S := GetKeyValue(ClassRoot, SubKey, '', '');
  if (S <> '') and (AnsiCompareFileName(Data, S) = 0) then
    Result := True;
end;

procedure UnRegSnapFileAssoc;

  function GetKeyNumSubkeysValues(const Subkey: String;
      var NumSubkeys, NumValues: DWORD): Boolean;
  var
    K: HKEY;
  begin
    Result := False;
    if RegOpenKeyEx(ClassRoot, PChar(Subkey), 0, KEY_QUERY_VALUE,
        K) = ERROR_SUCCESS then begin
      Result := (RegQueryInfoKey(K, nil, nil, nil, @NumSubkeys, nil, nil,
          @NumValues, nil, nil, nil, nil) = ERROR_SUCCESS);
      RegCloseKey(K);
    end;
  end;

  procedure DeleteDefaultValue(const Subkey: String);
  var
    K: HKEY;
  begin
    if RegOpenKeyEx(ClassRoot, PChar(Subkey), 0, KEY_SET_VALUE,
        K) = ERROR_SUCCESS then begin
      RegDeleteValue(K, '');
      RegCloseKey(K);
    end;
  end;
  
var
  Reg: TMyRegistry;
  TypeName: String;
  ExeName: String;
  NoKey: Boolean;
  NumSubkeys, NumValues: DWORD;
begin
  NoKey := False;
  TypeName := DefTypeName;
  Reg := TMyRegistry.Create;
  try
    Reg.RootKey := ClassRoot;
    if not Reg.OpenKey(SnapshotExt, False) then
      NoKey := True;
    try
      if not RegKeyExists(ClassRoot, TypeName) then begin
        if NoKey then
          Exit;
        TypeName := Reg.ReadStringDef('', '');
        if Trim(TypeName) = '' then
          Exit;
      end;
    finally
      Reg.CloseKey;
    end;
    ExeName := ParamStr(0);
    if RegKeyExists(ClassRoot, TypeName) then begin
      if ClassKeyValueEquals(TypeName + '\DefaultIcon',
          ExeName + ',' + IntToStr(IconIdx)) then
        RegDeleteKeyIncludingSubkeys(ClassRoot, TypeName + '\DefaultIcon');
      if ClassKeyValueEquals(TypeName + '\shell\' + CmdName + '\command',
          '"' + ExeName + '" ' + CmdParams) then
        RegDeleteKeyIncludingSubkeys(ClassRoot, TypeName + '\shell\' + CmdName);
      if ClassKeyValueEquals(TypeName + '\shell', CmdName) then
        DeleteDefaultValue(TypeName + '\shell');
      RegDeleteKeyIfEmpty(ClassRoot, TypeName + '\shell');
      if ClassKeyValueEquals(TypeName, TypeDesc) and
          GetKeyNumSubkeysValues(TypeName, NumSubkeys, NumValues) and
          (NumSubkeys = 0) and (NumValues <= 1) then
        RegDeleteKey(ClassRoot, PChar(TypeName));
    end;
    if not RegKeyExists(ClassRoot, TypeName) and
        ClassKeyValueEquals(SnapshotExt, TypeName) then
      DeleteDefaultValue(SnapshotExt);
    RegDeleteKeyIfEmpty(ClassRoot, SnapshotExt);
  finally
    Reg.Free;
  end;
  SHChangeNotify(SHCNE_ASSOCCHANGED, SHCNF_IDLIST, nil, nil);
end;

function CheckSnapFileAssoc: Boolean;
var
  Reg: TMyRegistry;
  TypeName: String;
  DefAction: String;
  ExeName: String;
begin
  Result := False;
  TypeName := DefTypeName;
  Reg := TMyRegistry.Create;
  try
    Reg.RootKey := ClassRoot;
    if not Reg.OpenKey(SnapshotExt, False) then
      Exit;
    try
      if not RegKeyExists(ClassRoot, TypeName) then begin
        TypeName := Reg.ReadStringDef('', '');
        if Trim(TypeName) = '' then
          Exit;
      end;
    finally
      Reg.CloseKey;
    end;
    ExeName := ParamStr(0);
    if not Reg.OpenKey(TypeName + '\shell', False) then
      Exit;
    try
      DefAction := Reg.ReadStringDef('', '');
      if Trim(DefAction) = '' then
        DefAction := 'open';
      if not RegKeyExists(ClassRoot, TypeName + '\shell\' + DefAction) then
        Exit;
    finally
      Reg.CloseKey;
    end;
    if not ClassKeyValueEquals(TypeName + '\shell\' + DefAction + '\command',
        '"' + ExeName + '" ' + CmdParams) then
      Exit;
    Result := True;
  finally
    Reg.Free;
  end;
end;

function CreateShellLink(const Filename, Description, ShortcutTo, Parameters,
    WorkingDir, IconFilename: String; const IconIndex, ShowCmd: Integer;
    const HotKey: Word): Boolean;
var
  Obj: IUnknown;
  SL: IShellLink;
  PF: IPersistFile;
  WideFilename: WideString;
begin
  Obj := CreateComObject(CLSID_ShellLink);
  SL := Obj as IShellLink;
  SL.SetPath(PChar(ShortcutTo));
  SL.SetArguments(PChar(Parameters));
  if WorkingDir <> '' then
    SL.SetWorkingDirectory(PChar(WorkingDir));
  if IconFilename <> '' then
    SL.SetIconLocation(PChar(IconFilename), IconIndex);
  SL.SetShowCmd(ShowCmd);
  if Description <> '' then
    SL.SetDescription(PChar(Description));
  if HotKey <> 0 then
    SL.SetHotKey(HotKey);
  PF := Obj as IPersistFile;
  WideFilename := Filename;
  Result := SUCCEEDED(PF.Save(PWideChar(WideFilename), True));
end;

function MakeDir(Dir: String): Boolean;
begin
  Result := CreateDirectory(PChar(Dir), nil);
  if Result then begin
    SHChangeNotify(SHCNE_MKDIR, SHCNF_PATH, PChar(Dir), nil);
    SHChangeNotify(SHCNE_UPDATEDIR, SHCNF_PATH or SHCNF_FLUSH,
        PChar(RemoveBackslashUnlessRoot(ExtractFilePath(Dir))), nil);
  end;
end;

function SpaceString(const S: String): String;
var
  I: Integer;
begin
  Result := '';
  for I := 1 to Length(S) do begin
    if S[I] = ' ' then
      Continue;
    if Result <> '' then
      Result := Result + ' ';
    Result := Result + S[I];
  end;
end;

procedure ConvertTo32BitImageList(const ImageList: TImageList);
const
  Mask: array[Boolean] of Longint = (0, ILC_MASK);
var
  TempList: TImageList;
begin
  if Assigned(ImageList) then begin
    TempList := TImageList.Create(nil);
    try
      TempList.Assign(ImageList);
      with ImageList do begin
        Handle := ImageList_Create(Width, Height, ILC_COLOR32 or Mask[Masked],
            0, AllocBy);
        if not HandleAllocated then
          raise EInvalidOperation.Create(SInvalidImageList);
      end;
      Imagelist.AddImages(TempList);
    finally
      TempList.Free;
    end;
  end;
end;

function FontExistsCallback(const lplf: TLogFont; const lptm: TTextMetric;
    dwType: DWORD; lpData: LPARAM): Integer; stdcall;
begin
  Boolean(Pointer(lpData)^) := True;
  Result := 1;
end;

function FontExists(const FaceName: String): Boolean;
var
  DC: HDC;
begin
  Result := False;
  DC := GetDC(0);
  try
    EnumFonts(DC, PChar(FaceName), @FontExistsCallback, @Result);
  finally
    ReleaseDC(0, DC);
  end;
end;

function GetPreferredUIFont: String;
{ Gets the preferred UI font. Returns Microsoft Sans Serif, or MS Sans Serif
  if it doesn't exist.
  Microsoft Sans Serif (which is available on Windows 2000 and later) has two
  advantages over MS Sans Serif:
  1) On Windows XP, it can display password dots in edit boxes.
  2) In my tests on Japanese XP, Microsoft Sans Serif can display Japanese
     characters (MS Sans Serif cannot). }
begin
  if FontExists('Microsoft Sans Serif') then
    Result := 'Microsoft Sans Serif'
  else
    Result := 'MS Sans Serif';
end;


function SetFontNameSize(const AFont: TFont; const AName: String;
    const ASize: Integer; const AFallbackName: String;
    const AFallbackSize: Integer): Boolean;
{ Returns True if AName <> '' and it used AName as the font name,
  False otherwise. }

  function SizeToHeight(const S: Integer): Integer;
  begin
    Result := MulDiv(-S, Screen.PixelsPerInch, 72);
  end;

begin
  Result := False;
  if AName <> '' then begin
    if FontExists(AName) then begin
      AFont.Name := AName;
      AFont.Height := SizeToHeight(ASize);
      Result := True;
      Exit;
    end;
    { Note: AFallbackName is not used if the user specified an empty string for
      AName because in that case they want the default font used always }
    if (AFallbackName <> '') and FontExists(AFallbackName) then begin
      AFont.Name := AFallbackName;
      AFont.Height := SizeToHeight(AFallbackSize);
      Exit;
    end;
  end;
  AFont.Name := GetPreferredUIFont;
  AFont.Height := SizeToHeight(AFallbackSize);
end;

type
  TFormAccess = class(TForm);

procedure NewChangeScale(const Ctl: TControl; const XM, XD, YM, YD: Integer);
var
  X, Y, W, H: Integer;
begin
  X := MulDiv(Ctl.Left, XM, XD);
  Y := MulDiv(Ctl.Top, YM, YD);
  if not(csFixedWidth in Ctl.ControlStyle) then
    W := MulDiv(Ctl.Left + Ctl.Width, XM, XD) - X
  else
    W := Ctl.Width;
  if not(csFixedHeight in Ctl.ControlStyle) then
    H := MulDiv(Ctl.Top + Ctl.Height, YM, YD) - Y
  else
    H := Ctl.Height;
  Ctl.SetBounds(X, Y, W, H);
end;

procedure NewScaleControls(const Ctl: TWinControl; const XM, XD, YM, YD: Integer);
{ This is like TControl.ScaleControls, except it allows the width and height
  to be scaled independently }
var
  I: Integer;
  C: TControl;
begin
  for I := 0 to Ctl.ControlCount - 1 do begin
    C := Ctl.Controls[I];
    if C is TWinControl then begin
      TWinControl(C).DisableAlign;
      try
        NewScaleControls(TWinControl(C), XM, XD, YM, YD);
        NewChangeScale(C, XM, XD, YM, YD);
      finally
        TWinControl(C).EnableAlign;
      end;
    end
    else
      NewChangeScale(C, XM, XD, YM, YD);
  end;
end;

procedure SetFormFont(const Form: TForm; var BaseUnitX, BaseUnitY: Integer);
var
  Size: TSize;
  TM: TTextMetric;
  W, H: Integer;
  R: TRect;
begin
  SetFontNameSize(Form.Font, FormFont, 8, '', 8);

  { Based on code from Q145994: }
  GetTextExtentPoint(Form.Canvas.Handle,
    'ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz', 52, Size);
  BaseUnitX := (Size.cx div 26 + 1) div 2;
  GetTextMetrics(Form.Canvas.Handle, TM);
  BaseUnitY := TM.tmHeight;

  if (BaseUnitX <> OrigBaseUnitX) or (BaseUnitY <> OrigBaseUnitY) then begin
    { Loosely based on scaling code from TForm.ReadState: }
    NewScaleControls(Form, BaseUnitX, OrigBaseUnitX, BaseUnitY, OrigBaseUnitY);
    R := Form.ClientRect;
    W := MulDiv(R.Right, BaseUnitX, OrigBaseUnitX);
    H := MulDiv(R.Bottom, BaseUnitY, OrigBaseUnitY);
    Form.SetBounds(Form.Left, Form.Top, W + (Form.Width - R.Right),
        H + (Form.Height - R.Bottom));
  end;
end;

{procedure InitFormFont(Form: TForm);
begin
  if DefFontData.Charset = SHIFTJIS_CHARSET then
    Form.Font.Handle := 0  // revert to default Japanese font (requires D3+)
  else if FontExists('Tahoma') then
    Form.Font.Name := 'Tahoma'
  else if FontExists('MS Shell Dlg') then
    Form.Font.Name := 'MS Shell Dlg'
  else
    Form.Font.Name := 'MS Sans Serif';
end;}

function FormatMulti(Num: Integer; Noun: String): String;
begin
  Result := Noun;
  if (Num = 0) or (Num > 1) then
    Result := Result + 's';
end;

procedure FreeObjStrings(S: TStrings);
var
  i: Integer;
begin
  if S = nil then
    Exit;
  for i := 0 to S.Count - 1 do begin
    if S.Objects[i] = nil then
      Continue;
    S.Objects[i].Free;
    S.Objects[i] := nil;
  end;
  S.Free;
end;

function StrReplace(S: String; S1, S2: String): String;
  function NewPos(Substr: String; S: String; StartPos: Integer): Integer;
  begin
    if StartPos > 1 then
      Delete(S, 1, StartPos - 1);
    Result := Pos(Substr, S);
    if Result > 0 then
      Inc(Result, StartPos - 1);
  end;
var
  Tmp: Integer;
begin
  Tmp := 1;
  Result := S;
  repeat
    Tmp := NewPos(S1, Result, Tmp);
    if Tmp > 0 then begin
      Result := Copy(Result, 1, Tmp - 1) + S2 +
          Copy(Result, Tmp + Length(S1), Length(Result) - Tmp - Length(S1));
      Inc(Tmp, Length(S2));
    end
    else
      Break;
  until False;
end;

function WrapText(S: String; MaxLen: Integer; CommentStr: String): String;
var
  i, CurLen, LastSpace: Integer;
begin
  Result := StrReplace(S, CrLf, CrLf + CommentStr);
  Result := CommentStr + Result;
  i := 1;
  CurLen := 0;
  LastSpace := 1;
  if Length(Result) <= MaxLen then
    Exit;
  while i <= Length(Result) do begin
    Inc(CurLen);
    if Result[i] = ' ' then
      LastSpace := i;
    if Result[i] in [#13, #10] then begin
      CurLen := 0;
      //Inc(i);
    end;
    if CurLen > MaxLen then begin
      Result := Copy(Result, 1, LastSpace - 1) + CrLf + CommentStr +
          TrimLeft(Copy(Result, LastSpace + 1, Length(Result) - LastSpace + 1));
      CurLen := 0;
      i := LastSpace;
    end;
    Inc(i);
  end;
end;

{procedure UpdateButton(Handle: HWND);
var
  Path: String;
  ValidFile: Boolean;
begin
  Path := GetFilePath(Handle);
  ValidFile := FileExists(Path) and (GetFileAttributes(PChar(Path)) <> $FFFFFFFF);
  if ValidFile
      and (Copy(Path, Length(Path) - Length(Snapshot_Ext) + 1,
      Length(Snapshot_Ext)) = Snapshot_Ext) then begin
    //SendDlgItemMessage(hDialog, ID_INFO, WM_ENABLE, 1, 0);
    EnableWindow(GetDlgItem(Handle, ID_INFO), True);
  end
  else begin
    //SendDlgItemMessage(hDialog, ID_INFO, WM_ENABLE, 0, 0);
    EnableWindow(GetDlgItem(Handle, ID_INFO), False);
  end;
end;}

{function DialogWndProc(hWnd: HWND; uMsg: UINT; wParam: WPARAM;
    lParam: LPARAM): Integer; stdcall;
var
  Path: String;
  OFNotify: TOFNotify;
begin
  case uMsg of
    WM_COMMAND: begin
      if (HIWORD(wParam) = BN_CLICKED) and (LOWORD(wParam) = ID_INFO) then begin
        Path := GetFilePath(hWnd);
        if FileExists(Path) then begin
          {if IsCorrupted then begin
            MsgBox('SmartClose cannot continue because the selected ' +
                'System Snapshot is empty or corrupt.', bgOK, miError,
                'Empty or Corrupt System Snapshot', 0);
          end
          else begin
            Application.CreateForm(TfrmSnapshot, frmSnapshot);
            frmSnapshot.ShowModal;
          end;
        end
        else
          EnableWindow(GetDlgItem(hWnd, ID_INFO), False);
      end
      else if (HIWORD(wParam) = EN_CHANGE) and (LOWORD(wParam) = ID_FILETEXT) then begin
        OFNotify.hdr.code := CDN_SELCHANGE;
        SendMessage(hWnd, WM_NOTIFY, 0, Integer(@OFNotify));
        UpdateButton(hWnd);
      end;
    end;
  end;
  Result := CallWindowProc(OldDialogProc, hwnd, uMsg, wParam, lParam);
end;}

{function DialogHook(Wnd: HWnd; Msg: UINT; WParam: WPARAM;
    LParam: LPARAM): UINT; stdcall;
var
  hDialog: Integer;
  rLabel: TRect;
  rFType: TRect;
  Font: hFont;
  BtnWnd: Integer;
begin
  Result := 0;
  hDialog := GetParent(Wnd);
  case Msg of
    WM_INITDIALOG: begin
      GetWindowRect(GetDlgItem(hDialog, ID_FOLDERLABEL), rLabel);
      ScreenToClient(hDialog, rLabel.TopLeft);
      GetWindowRect(GetDlgItem(hDialog, ID_FORMAT), rFType);
      ScreenToClient(hDialog, rFType.BottomRight);
      Font := SendMessage(GetDlgItem(hDialog, ID_FOLDERLABEL), WM_GETFONT, 0, 0);
      BtnWnd := CreateWindowEx(WS_EX_NOPARENTNOTIFY, 'Button',
          '&More information...', WS_CHILD or WS_TABSTOP or WS_GROUP,
          rLabel.Left, rFType.Bottom + 10, 95, 23, hDialog, ID_INFO, hInstance,
          nil);
      SendMessage(BtnWnd, WM_SETFONT, Font, 0);
      ShowWindow(BtnWnd, SW_SHOWNORMAL);
      OldDialogProc := Pointer(SetWindowLong(hDialog, GWL_WNDPROC,
          Integer(@DialogWndProc)));
    end;
    WM_DESTROY:
      SetWindowLong(hDialog, GWL_WNDPROC, Integer(OldDialogProc));
  end;
  if ((Msg = WM_NOTIFY) and (POFNotify(LParam)^.hdr.code = CDN_SELCHANGE)) then
    UpdateButton(hDialog);
end;}

{function OpenFilename(Handle: Integer; var OpenFN: TOFN): Boolean;
//var
  //buffer: array[0..MAX_PATH] of Char;
begin
  //ZeroMemory(@buffer, sizeof(buffer));
  ofn.lStructSize := SizeOf(TOpenFileName);
  ofn.hWndOwner := Handle;
  ofn.hInstance := hInstance;
  ofn.lpstrFilter := PChar(OpenFN.Filter);
  OpenFN.Filename := OpenFN.Filename + Space(MAX_PATH);
  OpenFN.Filename := Copy(OpenFN.Filename, 1, MAX_PATH);
  ofn.lpstrFile := PChar(OpenFN.Filename);
  ofn.nMaxFile := MAX_PATH;
  ofn.lpstrInitialDir := PChar(OpenFN.InitialDir);
  ofn.lpstrTitle := PChar(OpenFN.Title);
  ofn.lpstrDefExt := PChar(OpenFN.DefaultExt);
  ofn.Flags := OFN_FILEMUSTEXIST or OFN_PATHMUSTEXIST or OFN_EXPLORER
      or OFN_HIDEREADONLY or OFN_ENABLEHOOK;
  ofn.lpfnHook := DialogHook;
  Result := GetOpenFileName(ofn);
  if Result then
    OpenFN.Filename := Trim(ofn.lpstrFile);
end;}

procedure LoadHandCursor;
var
  Cur: HCURSOR;
begin
  Cur := LoadCursor(0, PChar(IDC_HAND));
  if Cur = 0 then
    Cur := LoadCursor(hInstance, 'HAND');
  Screen.Cursors[crHand] := Cur;
end;

initialization
  IsWinNT := (Win32Platform = VER_PLATFORM_WIN32_NT);
  LoadHandCursor;

finalization
  if Shlwapi <> 0 then
    FreeLibrary(Shlwapi);

end.
