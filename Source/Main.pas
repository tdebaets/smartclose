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
 * Main form
 *
 ****************************************************************************)

unit Main;

interface

uses Windows, Messages, Classes, Graphics, Controls, Forms, NewDialogs,
    ExtCtrls, StdCtrls, Buttons, ComCtrls, GifImage, Danhint, ThemeMgr,
    SysUtils, MBCSUtil, ImgList, ToolWin;

type
  TfrmMain = class(TForm)
    paMain: TPanel;
    GroupBox: TGroupBox;
    img: TImage;
    DanHint: TDanHint;
    Status: TStatusBar;
    Label1: TLabel;
    ThemeManager1: TThemeManager;
    Images: TImageList;
    HotImages: TImageList;
    tbMain: TToolBar;
    tbCloseProgs: TToolButton;
    tbRestoreProgs: TToolButton;
    tbSettings: TToolButton;
    tbInfo: TToolButton;
    tbClose: TToolButton;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure tbCloseProgsClick(Sender: TObject);
    procedure tbRestoreProgsClick(Sender: TObject);
    procedure tbSettingsClick(Sender: TObject);
    procedure FormHide(Sender: TObject);
    procedure tbInfoClick(Sender: TObject);
    procedure tbCloseClick(Sender: TObject);
  private
    procedure tbMainWndProc(var Message: TMessage);
    function LoadGlyph(ResName: string): Integer;
  public
    BaseUnitX, BaseUnitY: Integer;
    function ScalePixelsX(const N: Integer): Integer;
    function ScalePixelsY(const N: Integer): Integer;
    procedure DoShowHint(var HintStr: string; var CanShow: Boolean;
        var HintInfo: THintInfo);
  end;

var
  frmMain: TfrmMain;

implementation

uses Func, Snapshot, Process, Restore;

{$R *.DFM}

var
  OldtbMainWndProc: TWndMethod;

function TfrmMain.LoadGlyph(ResName: String): Integer;
var
  Bmp: TBitmap;
begin
  Bmp := TBitmap.Create;
  try
    Bmp.LoadFromResourceName(HInstance, ResName);
    Result := Images.AddMasked(Bmp, clFuchsia);
    Bmp.LoadFromResourceName(HInstance, ResName + 'HOT');
    HotImages.InsertMasked(Result, Bmp, clFuchsia);
  finally
    Bmp.Free;
  end;
end;

procedure TfrmMain.FormCreate(Sender: TObject);
var
  GIF: TGIFImage;
begin
  {$IFDEF Debug}
  outputdebugstring('main create enter');
  {$ENDIF}
  //InitFormFont(Self);
  SetFormFont(Self, BaseUnitX, BaseUnitY);
  DanHint.HintFont := Font;
  OldtbMainWndProc := tbMain.WindowProc;
  tbMain.WindowProc := tbMainWndProc;
  GIF := TGIFImage.Create;
  try
    //GIF.Center := True;
    GIF.LoadFromResourceName(HINSTANCE, 'WELCOME');
    img.Picture.Assign(GIF);
    //img.Picture.Bitmap := GIF.Bitmap;
  finally
    GIF.Free;
  end;
  {with img.Picture.Bitmap.Canvas.Font do begin
    Name := 'Tahoma';
    Size := 19;
  end;}
  Status.Panels.Items[0].Text := 'SmartClose version ' + GetAppVer;
  Application.OnShowHint := DoShowHint;
  tbMain.ButtonWidth := ScalePixelsX(300);
  tbMain.Indent := (tbMain.Width - tbCloseProgs.Width) div 2;
  tbCloseProgs.ImageIndex := LoadGlyph('TB_CLOSEPROGS');
  tbRestoreProgs.ImageIndex := LoadGlyph( 'TB_RESTORE');
  tbSettings.ImageIndex := LoadGlyph('TB_CONFIG');
  tbInfo.ImageIndex := LoadGlyph('TB_ABOUT');
  tbClose.ImageIndex := LoadGlyph('TB_EXIT');
  {$IFDEF Debug}
  outputdebugstring('main create exit');
  {$ENDIF}
end;

procedure TfrmMain.FormDestroy(Sender: TObject);
begin
  //ReleaseMutex(MainMutexHandle);
end;

procedure TfrmMain.FormShow(Sender: TObject);
begin
  {$IFDEF Debug}
  outputdebugstring('main show enter');
  {$ENDIF}
  Application.Title := 'SmartClose';
  Application.Icon.ReleaseHandle;
  Application.Icon.Handle := LoadIcon(HInstance, 'MAINICON');
  DanHint.HintActive := True;
  {$IFDEF Debug}
  outputdebugstring('main show exit');
  {$ENDIF}
end;

procedure TfrmMain.tbMainWndProc(var Message: TMessage);
var
  Control: TControl;
begin
  if Message.Msg = WM_LBUTTONUP then begin
    Control := tbMain.ControlAtPos(Point(LoWord(Message.lParam),
        HiWord(Message.lParam)), False);
    if (Control <> nil) and (Control is TToolButton) then begin
      (Control as TToolButton).Click;
      tbMain.Perform(WM_MOUSEMOVE, 0, 0);
      Exit;
    end;
  end;
  {if Message.Msg = WM_COMMAND then
    case Message.wParamLo of
      0: tbCloseProgsClick(Self);
      1: tbRestoreProgsClick(Self);
      2: tbSettingsClick(Self);
      3: tbInfoClick(Self);
      4: Application.Terminate;
    end;}
  OldtbMainWndProc(Message);
end;

procedure TfrmMain.FormKeyDown(Sender: TObject; var Key: Word;
    Shift: TShiftState);
begin
  case Key of
    VK_ESCAPE: Application.Terminate;
    VK_F5: tbCloseProgsClick(nil);
    VK_F6: tbRestoreProgsClick(nil);
    VK_F7: tbSettingsClick(nil);
    VK_F8: tbInfoClick(nil);
  end;
end;

procedure TfrmMain.DoShowHint(var HintStr: String; var CanShow: Boolean;
    var HintInfo: THintInfo);
var
  Pos: TPoint;
begin
  with HintInfo do begin
    if HintControl.ClassNameIs('TToolButton') then begin
      Pos.x := HintControl.Left;
      Pos.y := HintControl.Top + HintControl.Height;
      Pos := HintControl.Parent.ClientToScreen(Pos);
      HintData := Pointer(Pos.y);
      //HintStr := HintControl.Parent.ClassName;
    end;
  end;
end;

procedure TfrmMain.tbCloseProgsClick(Sender: TObject);
begin
  frmMain.Hide;
  Mode := wmNormal;
  ShowCloseWiz;
end;

procedure TfrmMain.tbRestoreProgsClick(Sender: TObject);
begin
  frmMain.Hide;
  Mode := wmNormal;
  ShowRestoreWiz;
end;

procedure TfrmMain.tbSettingsClick(Sender: TObject);
begin
  frmMain.Hide;
  ShowConfig;
end;

procedure TfrmMain.FormHide(Sender: TObject);
begin
  DanHint.HintActive := False;
end;

procedure TfrmMain.tbInfoClick(Sender: TObject);
begin
  frmMain.Hide;
  ShowAbout;
end;

function TfrmMain.ScalePixelsX(const N: Integer): Integer;
begin
  Result := MulDiv(N, BaseUnitX, OrigBaseUnitX);
end;

function TfrmMain.ScalePixelsY(const N: Integer): Integer;
begin
  Result := MulDiv(N, BaseUnitY, OrigBaseUnitY);
end;

procedure TfrmMain.tbCloseClick(Sender: TObject);
begin
  Application.Terminate;
end;

end.
