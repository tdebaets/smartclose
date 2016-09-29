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
 * Snapshot restore-related functions
 *
 ****************************************************************************)

unit Restore;

interface

uses Windows, Classes, SysUtils, MBCSUtil, ShlObj, ShellApi;

function ExecuteProgram(const FileName, Params, DefaultDir: String;
    ShowCmd: Integer; var hProc: THandle): Integer; overload;
function ExecuteProgram(const FileName, Params, DefaultDir: String;
    ShowCmd: Integer; hMonitor: Integer; var hProc: THandle): Integer; overload;
function ExecuteProcess(const FileName, Params, DefaultDir: String;
    ShowCmd: Integer; var hProc: THandle): Integer;
function GetShellExecErrorString(Error: Integer): String;
function IsFileExecutable(const Filename: String): Boolean;

implementation

uses Func;

function IsFileExecutable(const Filename: String): Boolean;
var
  SFI: TSHFileInfo;
begin
  Result := (SHGetFileInfo(PChar(Filename), 0, SFI, SizeOf(TSHFileInfo),
      SHGFI_EXETYPE) <> 0);
end;

function ExecuteProgram(const FileName, Params, DefaultDir: String;
    ShowCmd: Integer; var hProc: THandle): Integer;
begin
  Result := ExecuteProgram(Filename, Params, DefaultDir, ShowCmd, 0, hProc);
end;

const
  SEE_MASK_HMONITOR = $00200000;

function ExecuteProgram(const FileName, Params, DefaultDir: String;
    ShowCmd: Integer; hMonitor: Integer; var hProc: THandle): Integer;
var
  SEI: TShellExecuteInfo;
begin
  Result := -1;
  ZeroMemory(@SEI, SizeOf(SEI));
  SEI.cbSize := SizeOf(SEI);
  SEI.fMask := SEE_MASK_DOENVSUBST or SEE_MASK_FLAG_NO_UI
      or SEE_MASK_NOCLOSEPROCESS;
  if hMonitor <> 0 then begin
    SEI.fMask := SEI.fMask or SEE_MASK_HMONITOR;
    SEI.hIcon := hMonitor;
  end;
  SEI.Wnd := 0;
  SEI.lpFile := PChar(FileName);
  SEI.lpParameters := PChar(Params);
  SEI.lpDirectory := PChar(DefaultDir);
  SEI.nShow := ShowCmd;
  SetLastError(0);
  if not ShellExecuteEx(@SEI) then
    Result := GetLastError;
  hProc := SEI.hProcess;
end;

function ExecuteProcess(const FileName, Params, DefaultDir: String;
    ShowCmd: Integer; var hProc: THandle): Integer;
var
  SUInfo: TStartupInfo;
  ProcInfo: TProcessInformation;
  CmdLine: String;
begin
  Result := -1;
  CmdLine := '"' + Filename + '" ' + Params;
  FillChar(ProcInfo, SizeOf(TProcessInformation), #0);
  FillChar(SUInfo, SizeOf(SUInfo), #0);
  with SUInfo do begin
    cb := SizeOf(SUInfo);
    dwFlags := STARTF_USESHOWWINDOW;
    wShowWindow := ShowCmd;
  end;
  SetLastError(0);
  if CreateProcess(nil, PChar(CmdLine), nil, nil, False,
      CREATE_NEW_CONSOLE, nil, PChar(DefaultDir), SUInfo, ProcInfo) then begin
    CloseHandle(ProcInfo.hThread);
    hProc := ProcInfo.hProcess;
  end
  else
    Result := GetLastError;
end;

function GetShellExecErrorString(Error: Integer): String;
var
  //LibH: HMODULE;
  Buf: array[Byte] of Char;
  //ResNum: Integer;
begin
  {LibH := LoadLibrary(shell32);
  if LibH = 0 then begin
    if Error = 2 then begin
      Result := 'Cannot find the file %s (or one of the parts of it). Check if ' +
          'the path and filename are correct and if all the required libraries ' +
          'are available.';
    end
    else begin
      if FormatMessage(FORMAT_MESSAGE_FROM_SYSTEM, nil, Error,
          LOCALE_USER_DEFAULT, Buf, SizeOf(Buf), nil) <> 0 then
        Result := Buf;
    end;
  end
  else try
    case Error of
      0:                             ResNum := 8448;    //0
      ERROR_FILE_NOT_FOUND:          ResNum := 8449;    //2
      ERROR_PATH_NOT_FOUND:          ResNum := 8450;    //3
      ERROR_TOO_MANY_OPEN_FILES:     ResNum := 8451;    //4
      ERROR_ACCESS_DENIED:           ResNum := 8452;    //5
      ERROR_NOT_ENOUGH_MEMORY:       ResNum := 8448;    //8
      ERROR_BAD_FORMAT:              ResNum := 8461;    //11
      ERROR_OUTOFMEMORY:             ResNum := 8448;    //14
      {SE_ERR_SHARE:                  ResNum := 8457;    //26
      SE_ERR_ASSOCINCOMPLETE:        ResNum := 8458;    //27
      SE_ERR_DDETIMEOUT:             ResNum := 8459;    //28
      SE_ERR_DDEFAIL:                ResNum := 8459;    //29
      SE_ERR_DDEBUSY:                ResNum := 8459;    //30
      SE_ERR_NOASSOC:                ResNum := 8460;    //31
      //ERROR_SHARING_VIOLATION:       ResNum := 8457;    //32
      //SE_ERR_DLLNOTFOUND:            ResNum := 8456;    //32
      182, 188..202:                 ResNum := 8462;
      1150:                          ResNum := 8453;    //1150
      1151:                          ResNum := 8454;    //1151
      1152:                          ResNum := 8455;    //1152
      1154:                          ResNum := 8456;    //1154
      ERROR_NO_ASSOCIATION:          ResNum := 8460;
      ERROR_DDE_FAIL:                ResNum := 8459;
      //ERROR_DLL_NOT_FOUND:           ResNum := 8456;    //1157
      else                           ResNum := 0;
    end;
    if ResNum = 0 then begin}
      if FormatMessage(FORMAT_MESSAGE_FROM_SYSTEM, nil, Error,
          LOCALE_USER_DEFAULT, Buf, SizeOf(Buf), nil) <> 0 then
        Result := Buf;
    {end
    else
      Result := LoadResStr(LibH, ResNum);
  finally
    FreeLibrary(LibH);
  end;}
  if Trim(Result) = '' then
    Result := 'Unexpected error opening file. Error code: ' + IntToStr(Error);
end;

function ExecutePIDL(PIDL: PItemIDList): Boolean;
// Open the selected virtual folder
var
  ExeInfo: TShellExecuteInfo;
begin
  FillChar(ExeInfo, SizeOf(ExeInfo), 0);
  with ExeInfo do begin
    cbSize := SizeOf(ExeInfo);
    fMask := SEE_MASK_IDLIST;
    lpVerb := PChar('open');
    nShow := SW_RESTORE;
    lpIdList := PIDL;
  end;
  Result := ShellExecuteEx(@ExeInfo);
end;

end.
