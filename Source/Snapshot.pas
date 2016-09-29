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
 * System snapshot-related code
 *
 ****************************************************************************)

unit Snapshot;

interface

uses ExpWin, SysUtils, MBCSUtil, Windows, Classes, NewDialogs, FileCtrl,
    HugeIni, MyRegistry, ShellApi, ShlObj, Common2;

const
  SnapshotExt = '.scss';

var
  SnapPath: String;

type
  RestoreType = (rtScreenSaver, rtService, rtTaskSched, rtExpWindow, rtProg);
  CorruptType = (crNormalSnap, crNoFile, crNoSnapFile, crCorrupt);

type
  TSnapshot = record
    HasProgs: Boolean;
    HasExpWins: Boolean;
    HasScreenSaver: Boolean;
    HasServices: Boolean;
    HasTaskSched: Boolean;
  end;

type
  // TODO: close hProc handle in destructor if not 0
  TResProcObj = class(TObject)
  public
    Title: String;
    CommandLine: String;
    ExeFile: String;
    OrigExe: String;
    LocalExe: String;
    Params: String;
    CurDir: String;
    hProc: THandle;
  end;

type
  TEnumProgsProc = function(Prog: TResProcObj; var Data: Integer): Boolean;
  TEnumExpWinProc = function(Path: TExpWindowPath; var Data: Integer): Boolean;
  TEnumServiceProc = function(ServName: String; var Data: Integer): Boolean;

procedure InitSnapshot;
procedure AddToSnapBuffer2(Typ: RestoreType; Name: String; Extra, Extra2: String);
procedure AddToSnapBuffer(Typ: RestoreType; Name: String; Extra: String);
procedure FreeSnapIni;
function GetSnapPath: String;
procedure AddSnapInfo;
procedure CopySnapData(Filename: String);
procedure OpenSnapFile(Filename: String);
function FirstSnapCheck(Filename: String): Boolean;
function IsCorruptedSnapFile(var Snap: TSnapshot): Boolean;
function GetSnapDate: String;
procedure EnumSnapProgs(EnumProc: TEnumProgsProc; var Data: Integer);
procedure EnumSnapExpWins(EnumProc: TEnumExpWinProc; var Data: Integer);
procedure EnumSnapServices(EnumProc: TEnumServiceProc; var Data: Integer);
function RemoveEmptySnapPath(Path: String): Boolean;
procedure RemoveSnapFiles(hWnd: HWND; Setup: Boolean);

implementation

uses Func;

const
  InfoSection = 'Info';
  SvcSection = 'Services';
  ExpWinSection = 'ExplorerWindows';
  ProgSection = 'Programs';
  Comment =
      ';This system snapshot file was created by SmartClose version %s' +
          CrLf +
      ';WARNING: Do not make any modifications to this file as this may cause an ' +
          CrLf +
      ';error message or even a crash when SmartClose attempts to restore it ' +
          'again later.';
  MaxFilesize = 500; // in KB

var
  TempIni: THugeIni = nil;
  SnapFile: THugeIni = nil;
  SSaver: Boolean = False;
  SysAgent: Boolean = False;
  Counters: array[0..2] of Longint = (0, 0, 0);

procedure InitSnapshot;
var
  i: Integer;
begin
  TempIni := THugeIni.Create('');
  TempIni.Delay := True;
  for i := 0 to High(Counters) do
    Counters[i] := 0;
end;

procedure AddToSnapBuffer(Typ: RestoreType; Name: String; Extra: String);
begin
  AddToSnapBuffer2(Typ, Name, Extra, '');
end;

procedure AddToSnapBuffer2(Typ: RestoreType; Name: String; Extra, Extra2: String);
var
  Pos: Integer;
  Tmp: String;
begin
  if TempIni = nil then
    Exit;
  case Typ of
    rtScreenSaver: SSaver := True;
    rtService: begin
      //INISetValue(TempFile, 'Services', IntToStr(Cnt[0]), Name);
      TempIni.WriteString(SvcSection, IntToStr(Counters[0]), Name);
      Inc(Counters[0]);
    end;
    rtTaskSched: SysAgent := True;
    rtExpWindow: begin
      //INISetValue(TempFile, ExpWinSection, IntToStr(Cnt[1]),
      //    Extra + ',' + AddQuotes(Name));
      TempIni.WriteString(ExpWinSection, IntToStr(Counters[1]),
          Extra + ',' + AddQuotes(Name) {+ ',' + Extra2}); // TODO: uncomment multimon support?
      Inc(Counters[1]);
    end;
    rtProg: begin
      Pos := AnsiPos('?', Name);
      if Pos > 0 then begin
        Tmp := Copy(Name, Pos + 1, Length(Name) - Pos);    // Cmdline
        Name := Copy(Name, 1, Pos - 1);                    // Exefile
      end;
      //INISetValue(TempFile, ProgSection, IntToStr(Cnt[2]),
      //    AddQuotes(Extra) + ',' + Name);
      TempIni.WriteString(ProgSection, IntToStr(Counters[2]),
          AddQuotes(Extra) + ',' + AddQuotes(Name) + ',' + Tmp);
      Inc(Counters[2]);
    end;
  end;
end;

procedure FreeSnapIni;
begin
  if TempIni <> nil then
    FreeAndNil(TempIni);
  if SnapFile <> nil then
    FreeAndNil(SnapFile);
end;

{procedure CreateSnapshot(Filename: String);
var
  F: TextFile;
begin
  try
    AssignFile(F, Filename);
    Rewrite(F);
  except
    on E: Exception do begin
      MsgBox('Error while creating the System Snapshot file. ' +
          'SmartClose was not able to create a System Snapshot file' +
          CrLf + CrLf + 'Error message:' + CrLf + E.Message,
          bgOK, miError, 'File create error', 0);
      Exit;
    end;
  end;
  Write(F, Format(Comment, [AppVer]));
  Writeln(F);
  Writeln(F);
  CloseFile(F);
end;}

procedure AddSnapInfo;
begin
  SnapFile := THugeIni.Create('');
  SnapFile.Delay := True;
  //INISetValue(Filename, InfoSection, 'Version', AppVer);
  SnapFile.WriteString(InfoSection, 'Version', AppVer);
  DateSeparator := '/';
  TimeSeparator := ':';
  //INISetValue(Filename, InfoSection, 'CreateDate',
  //    FormatDateTime('MM/DD/YY HH:MM:SS', Now));
  SnapFile.WriteString(InfoSection, 'CreateDate',
      FormatDateTime('mm/dd/yyyy hh:nn:ss', Now));
  if SSaver then
    SnapFile.WriteBool(InfoSection, 'SSaver', True);
  if SysAgent then
    SnapFile.WriteBool(InfoSection, 'SysAgent', True);
end;

procedure CopySnapData(Filename: String);
var
  F1, F2: String;
  F: TextFile;
begin
  if TempIni = nil then
    Exit;
  if SnapFile = nil then
    Exit;
  F1 := SnapFile.WriteToString;
  F2 := TempIni.WriteToString;
  FreeAndNil(SnapFile);
  FreeAndNil(TempIni);
  try
    AssignFile(F, Filename);
    try
      Rewrite(F);
      Write(F, Format(Comment, [GetAppVer]));
      Writeln(F);
      Writeln(F);
      Write(F, F1);
      Write(F, F2);
    finally
      CloseFile(F);
    end;
  except
    on E: Exception do begin
      MsgBox('Error while creating the system snapshot file. ' +
          'SmartClose was not able to create a system snapshot file' + CrLf2 +
          'Error message:' + CrLf + E.Message,
          bgOK, miError, 'File create error', 0);
      Exit;
    end;
  end;
end;

procedure OpenSnapFile(Filename: String);
begin
  if SnapFile <> nil then
    FreeAndNil(SnapFile);
  SnapFile := THugeIni.Create(Filename);
end;

function CheckScreenSaver: Boolean;
{var
  ScreenSaver: String;}
begin
  Result := SnapFile.ReadBool(InfoSection, 'SSaver', False);
  {Result := 1;
  ScreenSaver := SnapFile.ReadString(InfoSection, 'SSaver', '');
  if ScreenSaver = '' then
    Exit;
  Result := 2;
  if not FileExists(ScreenSaver) then
    Exit;
  Result := 0;}
end;

function CheckSysAgent: Boolean;
begin
  Result := SnapFile.ReadBool(InfoSection, 'SysAgent', False);
end;

function CheckServiceLine(Name: String): Boolean;
begin
  Result := False;
  if Name = '' then
    Exit;
  if Length(Name) > 256 then
    Exit;
  if AnsiPos('/', Name) > 0 then
    Exit;
  if AnsiPos('\', Name) > 0 then
    Exit;
  Result := True;
end;

procedure EnumSnapServices(EnumProc: TEnumServiceProc; var Data: Integer);
var
  Name: String;
  Cnt: Longint;
begin
  Cnt := 0;
  Name := SnapFile.ReadString(SvcSection, '0', '');
  while Name <> '' do begin
    Name := Trim(Name);
    if CheckServiceLine(Name) then begin
      if not EnumProc(Name, Data) then
        Break;
    end;
    Inc(Cnt);
    Name := SnapFile.ReadString(SvcSection, IntToStr(Cnt), '');
  end;
end;

function CheckExpLine(Line: String; var Path: TExpWindowPath): Boolean;
var
  WindowType: Integer;
  pTmp: PChar;
  Tmp: String;
  //Extra: TStringList;
begin
  Result := False;
  if Length(Line) < 7 then
    Exit;
  if Line[2] <> ',' then
    Exit;
  WindowType := StrToIntDef(Line[1], 99);
  if (WindowType < 0) or (WindowType > 2) then
    Exit;
  Path.WindowType := TExpWindowType(WindowType);
  // TODO: uncomment?
  //Path.FullScreenMode := False;
  //Path.MonitorIdx := 0;
  Delete(Line, 1, 2);
  pTmp := PChar(Line);
  if not (Line[1] = '"') {or not (AnsiLastChar(Line)^ = '"')} then
    Exit;
  Tmp := Trim(AnsiExtractQuotedStr(pTmp, '"'));
  if Length(Tmp) < 3 then
    Exit;
  if (Path.WindowType <> wtIE) and (Copy(Tmp, 1, 2) <> '::') then begin
    {if Trim(ExtractFilePath(Line)) = '' then
      Exit;
    if Trim(ExtractFilename(Line)) = '' then
      Exit;}
    if CheckDirName(Tmp, True) <> cdnValid then
      Exit;
  end;
  Path.Location := Tmp;
  Result := True;
  {if Length(pTmp) > 1 then begin
    Tmp := pTmp;
    Delete(Tmp, 1, 1);
    Extra := TStringList.Create;
    try
      Split(Tmp, ',', Extra);
      if Extra.Count > 0 then
        Path.FullScreenMode := LongBool(StrToIntDef(Extra[0], 0));
      if Extra.Count > 1 then
        Path.MonitorIdx := StrToIntDef(Extra[1], 0);
    finally
      FreeAndNil(Extra);
    end;
  end;}
end;

procedure EnumSnapExpWins(EnumProc: TEnumExpWinProc; var Data: Integer);
var
  Line: String;
  Cnt: Longint;
  Path: TExpWindowPath;
begin
  Cnt := 0;
  Line := SnapFile.ReadString(ExpWinSection, '0', '');
  while Line <> '' do begin
    Line := Trim(Line);
    if CheckExpLine(Line, Path) then begin
      if not EnumProc(Path, Data) then
        Break;
    end;
    Inc(Cnt);
    Line := SnapFile.ReadString(ExpWinSection, IntToStr(Cnt), '');
  end;
end;

function CheckProgLine(Line: String; ProcObj: TResProcObj): Boolean;
var
  Pos: Integer;
  Tmp: String;
  FilePath, FileName: String;
  i: Integer;
begin
  Result := False;
  if Length(Line) < {7} 12 then
    Exit;
  for i := 1 to 2 do begin
    if Line[1] <> '"' then
      Exit;
    Delete(Line, 1, 1);
    Pos := AnsiPos('"', Line);
    if Pos = 0 then
      Exit;
    // TODO: possible error here if Line is exactly Pos chars long
    if Line[Pos + 1] <> ',' then
      Exit;
    Tmp := Copy(Line, 1, Pos - 1);
    case i of
      1: begin
        if CheckDirName(Tmp, True) = cdnValid then
          ProcObj.CurDir := Tmp
        else
          ProcObj.CurDir := '';
      end;
      2: begin
        if (CheckDirName(ExtractFilePath(Tmp), True) <> cdnValid) or
            (not IsValidName(ExtractFileName(Tmp))) then
          Exit;
        ProcObj.ExeFile := Tmp;
        ProcObj.OrigExe := Tmp;
        ProcObj.LocalExe := ExtractFilename(Tmp);
      end;
    end;
    Delete(Line, 1, Pos + 1);
  end;
  //Line := Copy(Line, Pos + 2, Length(Line) - Pos - 1);
  Line := Trim(Line);
  if Line = '' then
    Exit;
  Tmp := GetExeFile(Line);
  FilePath := ExtractFilePath(Tmp);
  if FilePath <> '' then begin
    if CheckDirName(FilePath, True) <> cdnValid then
      Exit;
  end;
  FileName := ExtractFileName(Tmp);
  if (FileName = '') or not IsValidName(FileName) then
    Exit;
  //ProcObj.OrigExe := Tmp;
  //ProcObj.ExeFile := Tmp;
  ProcObj.CommandLine := Line;
  if Line[1] = '"' then
    Delete(Line, 1, Length(Tmp) + 2)
  else
    Delete(Line, 1, Length(Tmp));
  ProcObj.Params := Trim(Line);
  Result := True;
end;

procedure EnumSnapProgs(EnumProc: TEnumProgsProc; var Data: Integer);
var
  Line: String;
  Cnt: Longint;
  ProcObj: TResProcObj;
begin
  Cnt := 0;
  Line := SnapFile.ReadString(ProgSection, '0', '');
  while Line <> '' do begin
    Line := Trim(Line);
    ProcObj := TResProcObj.Create;
    if CheckProgLine(Line, ProcObj) then begin
      if not EnumProc(ProcObj, Data) then
        Break;
    end
    else
      ProcObj.Free;
    Inc(Cnt);
    Line := SnapFile.ReadString(ProgSection, IntToStr(Cnt), '');
  end;
end;

function IsCorruptedSnapFile(var Snap: TSnapshot): Boolean;
  function CountServicesProc(ServName: String; var Data: Integer): Boolean;
  begin
    Inc(Data);
    Result := False;
  end;
  function CountExpWinsProc(Path: TExpWindowPath; var Data: Integer): Boolean;
  begin
    Inc(Data);
    Result := False;
  end;
  function CountProgsProc(ProcObj: TResProcObj; var Data: Integer): Boolean;
  begin
    ProcObj.Free;
    Inc(Data);
    Result := False;
  end;
var
  Cnt, Tmp: Longint;
begin
  Result := True;
  if SnapFile = nil then
    Exit;
  Cnt := 0;
  Tmp := 0;
  Snap.HasProgs := False;
  Snap.HasExpWins := False;
  Snap.HasScreenSaver := False;
  Snap.HasServices := False;
  Snap.HasTaskSched := False;
  if AnsiPos('.', SnapFile.ReadString(InfoSection, 'Version', '')) = 0 then
    Exit;
  if CheckScreenSaver then begin
    Snap.HasScreenSaver := True;
    Inc(Cnt);
  end;
  if IsWinNT then begin
    Tmp := 0;
    EnumSnapServices(@CountServicesProc, Tmp);
    if Tmp > 0 then
      Snap.HasServices := True;
    Inc(Cnt, Tmp);
  end
  else if CheckSysAgent then begin
    Inc(Cnt);
    Snap.HasTaskSched := True;
  end;
  Tmp := 0;
  EnumSnapExpWins(@CountExpWinsProc, Tmp);
  if Tmp > 0 then
    Snap.HasExpWins := True;
  Inc(Cnt, Tmp);
  Tmp := 0;
  EnumSnapProgs(@CountProgsProc, Tmp);
  if Tmp > 0 then
    Snap.HasProgs := True;
  Inc(Cnt, Tmp);
  if Cnt = 0 then
    Exit;
  Result := False;
end;

function FirstSnapCheck(Filename: String): Boolean;
var
  TFS: TFileStream;
  Str: String;
begin
  Result := False;
  try
    TFS := TFileStream.Create(Filename, fmOpenRead);
    try
      if TFS.Size > MaxFilesize * 1024 then
        Exit;
      SetLength(Str, TFS.Size);
      TFS.Read(Str[1], TFS.Size);
      if AnsiPos('[' + InfoSection + ']', Str) = 0 then
        Exit;
      Result := True;
    finally
      TFS.Free;
    end;
  except
    on E: Exception do
      Exit;
  end;
end;

function GetSnapDate: String;
var
  Str: String;
begin
  Result := '';
  if SnapFile = nil then
    Exit;
  Str := SnapFile.ReadString(InfoSection, 'CreateDate', '');
  if Str = '' then
    Exit;
  try
    Result := FormatDateTime('dddddd t', Func.StrToDateTime(Str));
  except
    on E: Exception do
      Exit;
  end;
end;

function GetSnapPath: String;
const
  SnapPath1 = 'SmartClose';
  SnapPath2 = 'Snapshots';
begin
  Result := GetKeyValue(Hive, RegKey, 'SnapPath', '');
  if (Result = '') or not DirectoryExists(Result) then begin
    //Result := ExtractFileDir(ParamStr(0)) + '\Snapshots';
    // TODO: improve code
    Result := AddBackSlash(GetShellFolderByCSIDL(CSIDL_APPDATA, True)) + SnapPath1;
    if not DirectoryExists(Result) then
      MakeDir(Result);
    Result := AddBackSlash(Result) + SnapPath2;
    if not DirectoryExists(Result) then
      MakeDir(Result);
    SetKeyValue(Hive, RegKey, 'SnapPath', Result);
  end;
  SnapPath := Result;
end;

function RemoveEmptySnapPath(Path: String): Boolean;
var
  SearchRec: TSearchRec;
begin
  Result := (FindFirst(AddBackSlash(Path) + '*' + SnapshotExt,
      faAnyFile - faDirectory, SearchRec) <> 0);
  SysUtils.FindClose(SearchRec);
  if Result then begin
    SetCurrentDirectory(PChar(GetWinDir));
    if RemoveDirectory(PChar(Path)) then
      SHChangeNotify(SHCNE_RMDIR, SHCNF_PATH, PChar(Path), nil)
  end;
end;

procedure RemoveSnapFiles(hWnd: HWND; Setup: Boolean);
const
  ManualDelMsg = 'If you still want the snapshot folder to be removed, ' +
      'you will have to manually delete the following folder:' + CrLf2 + '%s';
var
  Status: Integer;
  SearchRec: TSearchRec;
  SHFOS: TSHFileOpStruct;
  SHFrom: array[0..MAX_PATH + 2] of Char;
  OtherFilesFound: Boolean;
  NoSnapDelMsg: String;
begin
  NoSnapDelMsg := IIf(Setup, 'Setup', 'SmartClose') +
      ' will not be able to remove the Snapshot folder.';
  SHFrom := '';
  if not IsWindow(hWnd) then
    hWnd := 0;
  FillChar(SHFOS, SizeOf(SHFOS), 0);
  FillChar(SHFrom, SizeOf(SHFrom), 0);
  if Length(SnapPath) > MAX_PATH then
    Exit;
  with SHFOS do begin
    Wnd := hWnd;
    wFunc := FO_DELETE;
    StrPCopy(SHFrom, AddBackSlash(SnapPath) + '*' + SnapshotExt + #0#0);
    pFrom := @SHFrom;
    pTo := PChar(#0 + #0);
    fFlags := FOF_FILESONLY or FOF_NOCONFIRMATION;
    hNameMappings := nil;
  end;
  SHFileOperation(SHFOS);
  if SHFOS.fAnyOperationsAborted then begin
    MessageBox(hWnd,
        PChar('Removing the system snapshot files was canceled. ' +
        NoSnapDelMsg + CrLf2 + Format(ManualDelMsg, [SnapPath])),
        'Removing Canceled',
        MB_OK or MB_ICONWARNING or MB_APPLMODAL or MB_SETFOREGROUND);
    Exit;
  end;
  OtherFilesFound := False;
  Status := FindFirst(AddBackSlash(SnapPath) + '*', faAnyFile, SearchRec);
  try
    while Status = 0 do begin
      if (SearchRec.Name <> '.') and (SearchRec.Name <> '..') then begin
        OtherFilesFound := True;
        Break;
      end;
      Status := FindNext(SearchRec);
    end;
  finally
    SysUtils.FindClose(SearchRec);
  end;
  if OtherFilesFound then begin
    MessageBox(hWnd,
        PChar('There are still files and/or subfolders in the snapshot folder. ' +
        NoSnapDelMsg + CrLf2 + Format(ManualDelMsg, [SnapPath])),
        'Snapshot Folder isn''t Empty',
        MB_OK or MB_ICONWARNING or MB_APPLMODAL or MB_SETFOREGROUND);
    Exit;
  end;
  SetCurrentDirectory(PChar(GetWinDir));
  if RemoveDirectory(PChar(SnapPath)) then
    SHChangeNotify(SHCNE_RMDIR, SHCNF_PATH, PChar(SnapPath), nil);
end;

initialization
  GetSnapPath;

finalization
  FreeSnapIni;

end.
