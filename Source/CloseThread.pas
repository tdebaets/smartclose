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
 * Thread that closes a process and waits for the process to close
 *
 ****************************************************************************)

unit CloseThread;

interface

uses Windows, Classes;

type
  TCloseThread = class(TThread)
  private
    FhWnd: HWND;
    FPID: DWORD;
    FResult: DWORD;
    FSeconds: DWORD;
    FOnNotify: TNotifyEvent;
    FIsExplorer: Boolean;
    procedure Notify;
  public
    constructor Create(hWnd: HWND; PID, Seconds: DWORD;
        OnReturn, OnNotify: TNotifyEvent; IsExplorer: Boolean);
    procedure Execute; override;
    property Result: DWORD read FResult;
    property Seconds: DWORD read FSeconds;
  end;

var
  hStopEvent: THandle;
  IsWaiting: Boolean;

implementation

uses Messages;

type
  TSMTOThread = class(TThread)
  private
    FhWnd: HWND;
    FTime: DWORD;
    FIsExplorer: Boolean;
  public
    constructor Create(hWnd: HWND; Time: DWORD; IsExplorer: Boolean);
    procedure Execute; override;
  end;


constructor TCloseThread.Create(hWnd: HWND; PID, Seconds: DWORD;
    OnReturn, OnNotify: TNotifyEvent; IsExplorer: Boolean);
begin
  Inherited Create(False);
  FreeOnTerminate := True;
  FhWnd := hWnd;
  FPID := PID;
  FSeconds := Seconds;
  OnTerminate := OnReturn;
  FOnNotify := OnNotify;
  FIsExplorer := IsExplorer;
end;

procedure TCloseThread.Notify;
begin
  if Assigned(FOnNotify) then
    FOnNotify(Self);
end;

procedure TCloseThread.Execute;
var
  Objs: array[0..2] of Integer;
begin
  Objs[0] := OpenProcess(Windows.SYNCHRONIZE, False, FPID);
  Objs[1] := hStopEvent;
  Objs[2] := TSMTOThread.Create(FhWnd, FSeconds * 1000, FIsExplorer).Handle;
  Synchronize(Notify);
  repeat
    FResult := WaitForMultipleObjects(3, @Objs, False, 1000);
    if FResult <> WAIT_TIMEOUT then
      Exit;
    Dec(FSeconds);
    Synchronize(Notify);
  until FSeconds = 0;
end;

{ TSMTOThread }

constructor TSMTOThread.Create(hWnd: HWND; Time: DWORD; IsExplorer: Boolean);
begin
  Inherited Create(False);
  FreeOnTerminate := True;
  FhWnd := hWnd;
  FTime := Time;
  FIsExplorer := IsExplorer;
end;

procedure TSMTOThread.Execute;
var
  MsgResult: DWORD;
begin
  SetLastError(0);
  SendMessageTimeout(FhWnd, WM_GETTEXTLENGTH, 0, 0, SMTO_ABORTIFHUNG, 100,
      MsgResult);
  if GetLastError = 0 then begin
    if SetForegroundWindow(FhWnd) then
      BringWindowToTop(FhWnd);
  end;
  SetLastError(0);
  SendMessageTimeOut(FhWnd, WM_CLOSE, 0, 0, SMTO_ABORTIFHUNG, FTime, MsgResult);
  if FIsExplorer then begin
    SendMessageTimeout(FhWnd, WM_DESTROY, 0, 0, SMTO_ABORTIFHUNG, 100, MsgResult);
    Sleep(100);
    SendMessageTimeout(FhWnd, WM_NCDESTROY, 0, 0, SMTO_ABORTIFHUNG, 100, MsgResult);
    Sleep(100);
  end;
end;

initialization
  hStopEvent := CreateEvent(nil, True, False,
      'SmartClose CloseThread Stop Event {9F9DB9EB-97DE-4D6A-8A30-E6117F8207A3}');

finalization
  CloseHandle(hStopEvent);
  
end.
