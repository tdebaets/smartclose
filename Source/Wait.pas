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
 * Wait form
 *
 ****************************************************************************)

unit Wait;

interface

uses Windows, Messages, Classes, Graphics, Controls, Forms, ExtCtrls, StdCtrls,
    SysUtils, MBCSUtil, ThemeMgr;

type
  TfrmWait = class(TForm)
    imgIcon: TImage;
    lblTitle: TLabel;
    lblStatus: TLabel;
    btnCancel: TButton;
    lblCount: TLabel;
    lblDetails: TLabel;
    ThemeManager1: TThemeManager;
    procedure FormCreate(Sender: TObject);
    procedure btnCancelClick(Sender: TObject);
    procedure lblDetailsClick(Sender: TObject);
  private
    { Private declarations }
  public
    BaseUnitX, BaseUnitY: Integer;
    procedure ThdCount(Sender: TObject);
    procedure ThdDone(Sender: TObject);
    function ScalePixelsX(const N: Integer): Integer;
    function ScalePixelsY(const N: Integer): Integer;
  end;

var
  frmWait: TfrmWait;
  WaitCancel: Boolean;

implementation

{$R *.DFM}

uses Func, Process, CloseThread, frmLog;

procedure TfrmWait.ThdCount(Sender: TObject);
begin
  with Sender as TCloseThread do begin
    if Seconds < CloseTimeout then begin
      frmWait.lblCount.Visible := True;
      frmWait.lblCount.Caption := IntToStr(Seconds);
    end;
  end;
end;

procedure TfrmWait.ThdDone(Sender: TObject);
begin
  IsWaiting := False;
end;

procedure TfrmWait.FormCreate(Sender: TObject);
begin
  SetFormFont(Self, BaseUnitX, BaseUnitY);
  //InitFormFont(Self);
  lblTitle.Font.Style := [fsBold];
  lblCount.Font.Color := clRed;
  SetFontNameSize(lblDetails.Font, FormFont, 8, '', 8);
  lblDetails.Cursor := crHand;
  imgIcon.Picture.Icon.ReleaseHandle;
  imgIcon.Picture.Icon.Handle := LoadImage(HInstance, 'MAINICON', IMAGE_ICON,
      32, 32, LR_DEFAULTCOLOR);
end;

procedure TfrmWait.btnCancelClick(Sender: TObject);
begin
  btnCancel.Enabled := False;
  WaitCancel := True;
end;

procedure TfrmWait.lblDetailsClick(Sender: TObject);
begin
  Application.CreateForm(TLogWindow, LogWindow);
  LogWindow.Caption := 'View Command Line Log';
  LogWindow.txtLog.Text := LogFile;
  LogWindow.ShowModal;
  LogWindow.Free;
end;

function TfrmWait.ScalePixelsX(const N: Integer): Integer;
begin
  Result := MulDiv(N, BaseUnitX, OrigBaseUnitX);
end;

function TfrmWait.ScalePixelsY(const N: Integer): Integer;
begin
  Result := MulDiv(N, BaseUnitY, OrigBaseUnitY);
end;

end.
