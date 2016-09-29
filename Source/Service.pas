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
 * System service-related functions
 *
 ****************************************************************************)

unit Service;

interface

uses Windows, SysUtils, MBCSUtil, Classes, WinSvc, MyRegistry, Forms, NewDialogs,
    HugeIni, Common2;

const
  SERVICE_STATUS_DISABLED = $FF;

function GetServicesFilePath: String;
procedure GetServicesToStop(Svcs, DisplayNames: TStrings; CheckStoppable: Boolean);
function RestoreDefaultServices: Boolean;
function StopService(Name: String): Integer;
function RestartService(Name: String): Integer;
function GetServiceDisplayName(Name: String; var DisplayName: String): Integer;
function IsServiceStartable(Name: String): Boolean;
function GetServiceStatus(Name: String; var State: Integer): Integer;

implementation

uses Func;

type
  TReadOnlyHugeINI = class(THugeIni)
  private
    FReadOnly: Boolean;
  public
    property ReadOnly: Boolean read FReadOnly write FReadOnly;
    procedure WriteFile; override;
  end;

const
  ServiceCount = 20;
  DefaultServices: array[0..ServiceCount - 1, 0..2] of String = (
      ('Alerter',       '',
          'This service notifies users of administrative alerts.'),
      ('wuauserv',      'Automatic Update',
          'Periodically checks the Windows Update site for any new (critical) ' +
          'Windows updates available for download, and allows you to install them.'),
      ('BITS',          'Background Intelligent Transfer Service',
          'This service allows Windows to resume an interrupted download when ' +
          'you log back in. It is used, for example, by Windows Update.'), 
      ('ClipSrv',       'ClipBook',
          'Allows you to store information (cut/paste) in a remote clipboard ' +
          'that is shared with other computers.'),
      {('ERSvc',         'Error Reporting Service',
          'Used to send information to Microsoft about application errors that ' +
          'have occurred.'),}
      {('FAX',           'Fax Service',
          'This service allows you to send and receive faxes.'),}
      ('helpsvc',       'Help and Support',
          'This service is required to access Microsoft Help and Support'),
      {('HidServ',       'Human Interface Device Access',
          'Some devices may require this service to function properly, such as ' +
          'scanners with function buttons, remote controls and keyboards with ' +
          'volume or play controls.'),}
      ('cisvc',         'Indexing Service',
          'Indexes the contents and properties of files on hard drive while ' +
          'the computer is idle.'),
      ('PolicyAgent',   'IPSEC Services',
          'This service is used to provide some additional security on IP addresses.'),
      ('Messenger',     '',
          'Sends messages between clients and servers. No messages will be sent ' +
          'if this service is stopped.' + CrLf +
          'Note: not related to Windows or MSN Messenger.'),
      ('mnmsrvc',       'NetMeeting Remote Desktop Sharing',
          'Enables an authorized user to remotely access your computer through ' +
          'NetMeeting.'),
      ('NetDDE',        'Network DDE',
          'Provides Dynamic Data Exchange between programs through a network'),
      ('NetDDEdsdm',    'Network DDE DSDM',
          'Manages Dynamic Data Exchange between programs through a network'),
      ('SysmonLog',     'Performance Logs and Alerts',
          'Collects performance data at a scheduled time and writes this data to ' +
          'a log or triggers an alert. No performance data will be collected if ' +
          'this service is stopped.'), 
      {('WmdmPmSp',      'Portable Media Serial Number',
          'This service retrieves the serial number from a portable music player ' +
          'connected to the computer.'),}
      ('RDSessMgr',     'Remote Desktop Help Session Manager',
          'Manages and controls Remote Assistance.'),
      ('RemoteRegistry','Remote Registry Service',
          'This service can give remote users access to your registry.'),
      {('seclogon',      'Secondary Logon',
          'Allows a limited user account to start an application with higher ' +
          'privileges, such as the Administrator account or another user, by ' +
          'using the "Run As..." option.'),}
      {('SCardSvr',      'Smart Card',
          'Offers access to a Smart Card in a connected Smart Card reader, ' +
          'mostly used for authentication. No Smart Cards can be read if this ' +
          'service is stopped.'),
      ('SCardDrv',      'Smart Card Helper',
          'Enables support for connected Smart Card readers that are not Plug ' +
          'and Play.'),}
      {('SSDPSRV',       'SSDP Discovery Service',
          'This service enables the detection of Universal Plug and Play devices ' +
          'on the home network.'),}
      {('srservice',     'System Restore Service',
          'Automatically creates restore points to roll back to in case of a ' +
          'problem, either when installing a program or driver, or by schedule. ' +
          'No restore points will be created if this service is stopped.'),}
      ('Schedule',      'Task Scheduler',
          'Allows a user to schedule and configure automated tasks. None of the ' +
          'scheduled tasks will be executed if this service is stopped.'),
      {('UPS',           'Uninterruptible Power Supply',
          'Manages an Uninterruptible Power Supply (UPS) that is connected to ' +
          'the computer.'),}
      {('UPNPhost',      'Universal Plug and Play Device Host',
          'This service supports hosting of Universal Plug and Play devices.'),}
      ('uploadmgr',     'Upload Manager',
          'This service manages file transfers between clients and servers on ' +
          'the network.'),
      ('W32Time',       'Windows Time',
          'Automatically synchronizes the date and time with a time server on ' +
          'the internet or a network. The date and time will not be synchronized ' +
          'if this service is stopped.'),
      ('InteractiveLogon','',
          'This service is part of Windows XP PowerToys and is not required to ' +
          'be running.'),
      ('MDM',           'Machine Debug Manager',
          'Supports debugging in Microsoft Office components or Microsoft ' +
          'Visual Studio applications.'),
      ('NVSvc',         'NVIDIA Driver Helper Service',
          'This service is part of the nVidia Drivers, but not needed to be ' +
          'running.')
    );

const
  InfoHeader = CrLf +
      '; SMARTCLOSE SERVICES FILE'
      + CrLf2 +
      '; This file contains a list of all the services that can be stopped by' + CrLf +
      '; SmartClose.'
      + CrLf2 +
      '; Warning: some services are very important and should never be stopped. Do not' + CrLf +
      '; edit this list unless you know what you''re doing!'
      + CrLf2 +
      '; To restore the default list of services, delete this file. SmartClose will' + CrLf +
      '; automatically re-create the file the next time it''s needed.';

  InfoSectionInfo =
      '; General Info section'
      + CrLf2 +
      '; This section contains some general info about this file that might be useful.';
  InfoSection = 'Info';
  VersionInfo =
      '; Version of SmartClose that created this file:';
  Version = 'Version';
  DateInfo =
      '; Date SmartClose created this file at:';
  CreateDate = 'CreateDate';

  ServiceSectionInfo =
      '; Service Section'
      + CrLf2 +
      '; This section contains the actual list of services. Each entry has the' + CrLf +
      '; following format:' + CrLf +
      '; <service name>=<display name>' + CrLf +
      '; The service name is required. Display name will only be used when the service' + CrLf +
      '; doesn''t exist, otherwise, SmartClose will use the display name of the' + CrLf +
      '; installed service itself.' + CrLf +
      '; After each entry, there is also a short description of the purpose' + CrLf +
      '; of the service.' + CrLf +
      '; If you want to temporarily exclude a service from being stopped by SmartClose,' + CrLf +
      '; comment (precede) the line of the service with a semicolon and a space.';
  ServiceSection = 'Services';

  IniComment = '; ';
  
const
  SERVICE_DISABLED = $00000004;
  //ServKey = RegKey + '\Services';
  ServiceFilename = 'Services.ini';

procedure TReadOnlyHugeINI.WriteFile;
begin
  if not FReadOnly then
    inherited WriteFile;
end;

function GetServiceStatus(Name: String; var State: Integer): Integer;
var
  hService: SC_HANDLE;
  hSCManager: SC_HANDLE;
  Status: TServiceStatus;
begin
  Result := 0;
  State := 0;
  // TODO: only request required access rights instead of SC_MANAGER_ALL_ACCESS
  hSCManager := OpenSCManager(nil, nil, SC_MANAGER_ALL_ACCESS);
  if hSCManager = 0 then
    Exit;
  try
    hService := OpenService(hSCManager, PChar(Name), SERVICE_QUERY_STATUS);
    try
      if QueryServiceStatus(hService, Status) then begin
        State := Status.dwCurrentState;
        Result := Status.dwControlsAccepted;
      end;
    finally
      CloseServiceHandle(hService);
    end;
  finally
    CloseServiceHandle(hSCManager);
  end;
end;

function IsServiceStoppable(Name: String): Boolean;
var
  Status, Controls: Integer;
begin
  Result := True;
  Controls := GetServiceStatus(Name, Status);
  if (Controls and SERVICE_ACCEPT_STOP = 0) then
    Result := False
  else if (Status <> SERVICE_RUNNING) and (Status <> SERVICE_CONTINUE_PENDING)
      and (Status <> SERVICE_PAUSE_PENDING) and (Status <> SERVICE_PAUSED) then
    Result := False;
end;

function IsServiceStartable(Name: String): Boolean;
var
  Status: Integer;
begin
  Result := True;
  GetServiceStatus(Name, Status);
  if not (Status = SERVICE_STOPPED) then
    Result := False;
end;

function GetServicesFilePath: String;
begin
  Result := AddBackSlash(ExtractFilePath(ParamStr(0))) + ServiceFilename;
  if not FileExists(Result) then begin
    if not RestoreDefaultServices then
      Result := '';
  end;
end;

function RestoreDefaultServices: Boolean;
var
  Path: String;
  F: TextFile;
  i: Integer;
begin
  Result := True;
  Path := AddBackSlash(ExtractFilePath(ParamStr(0))) + ServiceFilename;
  try
    AssignFile(F, Path);
    try
      Rewrite(F);
      Writeln(F, InfoHeader);
      Writeln(F);
      Writeln(F);
      Writeln(F, InfoSectionInfo);
      Writeln(F);
      Writeln(F, '[' + InfoSection + ']');
      Writeln(F);
      Writeln(F, VersionInfo);
      Writeln(F, Version + '=' + AppVer);
      Writeln(F);
      Writeln(F, DateInfo);
      Writeln(F, CreateDate + '=' + FormatDateTime('mm/dd/yyyy hh:nn:ss', Now));
      Writeln(F);
      Writeln(F);
      Writeln(F, ServiceSectionInfo);
      Writeln(F);
      Writeln(F, '[' + ServiceSection + ']');
      for i := 0 to ServiceCount - 1 do begin
        Writeln(F);
        Writeln(F, DefaultServices[i, 0] + '=' + DefaultServices[i, 1]);
        Writeln(F, WrapText(DefaultServices[i, 2], 80, IniComment));
      end;
    finally
      CloseFile(F);
    end;
  // TODO: make handler less general
  except
    on E: Exception do begin
      MsgBox('Error while creating the Services file (' + Path + ').' + CrLf2 +
          'Error message:' + CrLf + E.Message,
          bgOK, miError, 'File creation error', 0);
      Result := False;
      Exit;
    end;
  end;
end;

procedure GetServicesToStop(Svcs, DisplayNames: TStrings; CheckStoppable: Boolean);
var
  ServicesFile: String;
  Services: TReadOnlyHugeINI;
  i: Integer;
  Tmp: TStringList;
  DispName: String;
begin
  Svcs.Clear;
  DisplayNames.Clear;
  ServicesFile := GetServicesFilePath;
  if ServicesFile = '' then
    Exit;
  Services := TReadOnlyHugeINI.Create(ServicesFile);
  Tmp := TStringList.Create;
  try
    Services.ReadOnly := True;
    Services.ReadSection(ServiceSection, Tmp);
    if Tmp.Count = 0 then
      Exit;
    for i := 0 to Tmp.Count - 1 do begin
      if Trim(Tmp[i]) = '' then
        Continue;
      if CheckStoppable and not IsServiceStoppable(Tmp[i]) then
        Continue;
      DispName := Services.ReadString(ServiceSection, Tmp[i], '');
      if Trim(DispName) = '' then
        DispName := Tmp[i];
      Svcs.Add(Tmp[i]);
      DisplayNames.Add(DispName);
    end;
  finally
    Services.Free;
    Tmp.Free;
  end;
end;

{procedure GetServicesToStop(OnlyStoppable: Boolean; Svcs, DisplayNames: TStrings);
var
  F: TextFile;
  Line: String;
  Pos: Integer;
  Name: String;
begin
  if GetServicesFilePath = '' then
    Exit;
  Svcs.Clear;
  DisplayNames.Clear;
  AssignFile(F, GetServicesFilePath);
  Reset(F);
  while not Eof(F) do begin
    Readln(F, Line);
    Pos := AnsiPos(#9, Line);
    if Pos = 0 then
      Pos := Length(Line) + 1;
    Name := Copy(Line, 1, Pos - 1);
    if OnlyStoppable and not IsServiceStoppable(Name) then
      Continue;
    Svcs.Add(Name);
    Delete(Line, 1, Length(Name) + 1);
    if Line = '' then
      Line := Name;
    DisplayNames.Add(Line);
  end;
end;}

{procedure RestoreDefaultServices;
var
  Reg: TMyRegistry;
  i: Integer;
  DispName: String;
begin
  Reg := TMyRegistry.Create;
  try
    Reg.RootKey := Hive;
    for i := 0 to ServCount - 1 do begin
      if not Reg.OpenKey(ServKey + '\' + DefaultServices[i, 0], True) then
        Continue;
      try
        Reg.WriteBool('Enabled', True);
        Reg.WriteBool('Default', True);
        DispName := DefaultServices[i, 1];
        if Trim(DispName) <> '' then
          Reg.WriteString('DisplayName', DefaultServices[i, 1]);
      finally
        Reg.CloseKey;
      end;
    end;
  finally
    Reg.Free;
  end;
end;}

{procedure GetServicesToStop(OnlyStoppable: Boolean; Svcs, DisplayNames: TStrings);
var
  Reg: TMyRegistry;
  i: Integer;
  Keys: TStringList;
  DispName: String;
begin
  Svcs.Clear;
  DisplayNames.Clear;
  Keys := TStringList.Create;
  Reg := TMyRegistry.Create;
  try
    Reg.RootKey := Hive;
    if not Reg.OpenKey(ServKey, True) then
      Exit;
    try
      if not Reg.HasSubKeys then
        Exit;
      Reg.GetKeyNames(Keys);
    finally
      Reg.CloseKey;
    end;
    for i := 0 to Keys.Count - 1 do begin
      if not Reg.OpenKey(ServKey + '\' + Keys[i], False) then
        Continue;
      try
        if not Reg.ReadBoolDef('Enabled', False) then
          Continue;
        Svcs.Add(Keys[i]);
        DispName := Reg.ReadStringDef('DisplayName', '');
        if Trim(DispName) = '' then
          DispName := Keys[i];
        DisplayNames.Add(DispName);
      finally
        Reg.CloseKey;
      end;
    end;
  finally
    Reg.Free;
    Keys.Free;
  end;
end;}

function StopService(Name: String): Integer;
var
  hService: SC_HANDLE;
  hSCManager: SC_HANDLE;
  Status: TServiceStatus;
  i: Integer;
begin
  Result := 1;
  if not IsServiceStoppable(Name) then
    Exit;
  Result := 2;
  // TODO: only request required access rights instead of SC_MANAGER_ALL_ACCESS
  hSCManager := OpenSCManager(nil, nil, SC_MANAGER_ALL_ACCESS);
  if hSCManager = 0 then
    Exit;
  try
    Result := 3;
    hService := OpenService(hSCManager, PChar(Name),
        SERVICE_STOP or SERVICE_QUERY_STATUS);
    if hService = 0 then
      Exit;
    try
      Result := 4;
      if not ControlService(hService, SERVICE_CONTROL_STOP, Status) then
        Exit;
      Sleep(500);
      Result := 5;
      for i := 1 to 20 do begin
        QueryServiceStatus(hService, Status);
        Application.ProcessMessages;
        if Status.dwCurrentState = SERVICE_STOPPED then begin
          Result := 0;
          Exit;
        end
        else
          Sleep(500);
      end;
    finally
      CloseServiceHandle(hService);
    end;
  finally
    CloseServiceHandle(hSCManager);
  end;
end;

function RestartService(Name: String): Integer;
var
  hService: SC_HANDLE;
  hSCManager: SC_HANDLE;
  Status: TServiceStatus;
  Args: PChar;
  i: Integer;
begin
  Result := 1;
  if not IsServiceStartable(Name) then
    Exit;
  Result := 2;
  // TODO: only request required access rights instead of SC_MANAGER_ALL_ACCESS
  hSCManager := OpenSCManager(nil, nil, SC_MANAGER_ALL_ACCESS);
  if hSCManager = 0 then
    Exit;
  try
    Result := 3;
    hService := OpenService(hSCManager, PChar(Name),
        SERVICE_START or SERVICE_QUERY_STATUS);
    if hService = 0 then
      Exit;
    try
      Result := 4;
      Args := nil;
      if not StartService(hService, 0, Args) then
        Exit;
      Sleep(500);
      Result := 5;
      for i := 1 to 20 do begin
        QueryServiceStatus(hService, Status);
        Application.ProcessMessages;
        if Status.dwCurrentState = SERVICE_RUNNING then begin
          Result := 0;
          Exit;
        end
        else
          Sleep(500);
      end;
    finally
      CloseServiceHandle(hService);
    end;
  finally
    CloseServiceHandle(hSCManager);
  end;
end;

function GetServiceDisplayName(Name: String; var DisplayName: String): Integer;
var
  hSCManager: SC_HANDLE;
  hService: SC_HANDLE;
  ServConfig: PQueryServiceConfig;
  Bytes: Cardinal;
begin
  Result := 0;
  // TODO: only request required access rights instead of SC_MANAGER_ALL_ACCESS
  hSCManager := OpenSCManager(nil, nil, SC_MANAGER_ALL_ACCESS);
  if hSCManager = 0 then
    Exit;
  try
    hService := OpenService(hSCManager, PChar(Name), SERVICE_QUERY_CONFIG);
    if hService = 0 then
      Exit;
    try
      Bytes := 0;
      QueryServiceConfigAllowNil(hService, nil, 0, Bytes);
      GetMem(ServConfig, Bytes);
      try
        if QueryServiceConfigAllowNil(hService, ServConfig, Bytes,
            Bytes) then begin
          DisplayName := ServConfig.lpDisplayName;
          if ServConfig.dwStartType = SERVICE_DISABLED then
            Result := 1
          else
            Result := 2;
        end;
      finally
        FreeMem(ServConfig);
      end;
    finally
      CloseServiceHandle(hService);
    end;
  finally
    CloseServiceHandle(hSCManager);
  end;
end;

end.
