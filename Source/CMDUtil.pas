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
 * Command-line utilities
 *
 ****************************************************************************)

unit CMDUtil;

interface

uses Windows, Classes, Forms, SysUtils, MBCSUtil;

function CmdLineClose(Kill: Boolean): Boolean;
procedure CmdLineResult;

implementation

uses Func, Wait, Process, Controls, CloseWiz;

var
  NumClosedProgs: Integer = 0;
  NumKilledProgs: Integer = 0;

function IsProcInList(Name: String; ProcID: Integer; List: TStrings): Boolean;
var
  i: Longint;
  PID: Integer;
begin
  Result := True;
  for i := 0 to List.Count - 1 do begin
    PID := StrToIntDef(List.Strings[i], 0);
    if (ProcID <> 0) and (PID <> 0) then begin
      if ProcID = PID then
        Exit;
    end
    else if (AnsiUpperCase(List.Strings[i]) = AnsiUpperCase(Name)) then
      Exit;
  end;
  Result := False;
end;

{procedure FreeProcObjs(Strings: TStrings);
var
  i: Integer;
  Obj: TObject;
begin
  for i := 0 to Strings.Count - 1 do begin
    Obj := Strings.Objects[i];
    if Obj <> nil then begin
      if Obj.ClassName = 'TProcWinObj' then
        Obj.Free;
    end;
  end;
end;}

function CmdLineClose(Kill: Boolean): Boolean;
var
  i: Integer;
  CloseList: TStringList;
  ProcList: TStringList;
  ProcObj: TProcWinObj;
  ProProgs: TStringList;
  hLib: HMODULE;
  DllPath: String;
begin
  Result := False;
  ProcList := nil;
  ProProgs := nil;
  hLib := 0;
  CloseList := TStringList.Create;
  try
    for i := 1 to ParamCount do begin
      if ParamStr(i)[1] <> '/' then
        CloseList.Add(ParamStr(i));
    end;
    if CloseList.Count = 0 then
      Exit;
    ShowWait(True, msgProgsLoad);
    try
      ProcList := TStringList.Create;
      GetProcesses(ProcList);
      ProProgs := LoadProtectedProgs(True);
      Application.ProcessMessages;
      if WaitCancel then
        Exit;
      for i := 0 to ProcList.Count - 1 do begin
        ProcObj := TProcWinObj(ProcList.Objects[i]);
        if IsProcInList(ProcObj.DispName, 0, ProProgs) then
          Continue;
        if not Kill and (ProcObj.Status = psRunning) then begin
          if not IsProcInList(ProcObj.DispName, ProcObj.ProcID, CloseList) then
            Continue;
          frmWait.lblStatus.Caption := 'Trying to close ' + ProcObj.DispName +
              '...';
          Application.ProcessMessages;
          if WaitCancel then
            Exit;
          if frmCloseWiz.CloseProcess(ProcObj, nil) then
            Inc(NumClosedProgs);
          Application.BringToFront;
          frmWait.lblCount.Visible := False;
          Application.ProcessMessages;
          if WaitCancel then
            Exit;
        end;
        if Kill and (ProcObj.Status in [psRunning, psInvisible]) then begin
          if not IsProcInList(ProcObj.DispName, ProcObj.ProcID, CloseList) then
            Continue;
          Application.ProcessMessages;
          if WaitCancel then
            Exit;
          if ProcObj.Status = psClosed then
            Continue;
          if IsProcessClosed(ProcObj) then begin
            Inc(NumClosedProgs);
            Continue;
          end;
          {if not IsWinNT and (hLib = 0) then
            hLib := LoadRTDLL(DLLPath);}
          frmWait.lblStatus.Caption := 'Terminating ' + ProcObj.DispName + '...';
          Application.ProcessMessages;
          if WaitCancel then
            Exit;
          if frmCloseWiz.KillProcess(ProcObj, hLib) then
            Inc(NumKilledProgs);
          Application.BringToFront;
          Application.ProcessMessages;
          if WaitCancel then
            Exit;
        end;
      end;
      Result := True;
      RedrawSystray;
    finally
      if ProcList <> nil then
        FreeObjStrings(ProcList);
      if ProProgs <> nil then
        FreeAndNil(ProProgs);
      if hLib <> 0 then begin
        FreeLibrary(hLib);
        DeleteFile(DllPath);
      end;
      HideWait;
    end;
  finally
    FreeAndNil(CloseList);
  end;
end;

procedure CmdLineResult;
begin
  if frmWait = nil then
    Application.CreateForm(TfrmWait, frmWait);
  frmWait.Height := frmWait.ScalePixelsY(118);
  frmWait.btnCancel.Visible := True;
  frmWait.lblDetails.Visible := True;
  frmWait.lblTitle.Caption := 'Done';
  frmWait.lblStatus.Caption := IntToStr(NumClosedProgs) + ' ' +
      FormatMulti(NumClosedProgs, 'program') + ' closed' +
      IIf(NumKilledProgs = 0, '', ' and ' + IntToStr(NumKilledProgs) + ' ' +
      FormatMulti(NumKilledProgs, 'program') + ' killed') + '.';
  frmWait.btnCancel.Caption := 'Close';
  frmWait.btnCancel.ModalResult := mrOK;
  frmWait.ShowModal;
  {frmWait.lblDetails.Visible := False;
  frmWait.lblTitle.Caption := 'Please wait...';
  frmWait.btnCancel.Caption := 'Cancel';
  frmWait.btnCancel.ModalResult := mrNone;}
end;

end.
