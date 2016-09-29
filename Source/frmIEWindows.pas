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
 * Form to list (Internet) Explorer windows to restore
 *
 ****************************************************************************)

unit frmIEWindows;

interface

uses Windows, Messages, Classes, Graphics, Controls, Forms, NewDialogs,
    ComCtrls, StdCtrls, ExtCtrls, ActiveX, ThemeMgr, ExtChkListView,
    EnhListView, ExtListView, ImgList;

type
  TIEWindows = class(TForm)
    imgImages: TImageList;
    Label1: TLabel;
    Button1: TButton;
    Bevel1: TBevel;
    ThemeManager1: TThemeManager;
    lvWindows: TExtChkListView;
    {procedure lvWindowsKeyPress(Sender: TObject; var Key: Char);
    procedure lvWindowsMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);}
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
  private
    { Private declarations }
  public
    procedure ClearWinList;
  end;

var
  IEWindows: TIEWindows;

implementation

uses Func, ExpWin;

{$R *.DFM}

{procedure TIEWindows.lvWindowsKeyPress(Sender: TObject; var Key: Char);
var
  Point: TPoint;
begin
  Point := lvWindows.Selected.GetPosition;
  if Key = ' ' then
    lvWindowsMouseDown(Self, mbLeft, [ssLeft], Point.x, Point.y);
end;

procedure TIEWindows.lvWindowsMouseDown(Sender: TObject; Button: TMouseButton;
    Shift: TShiftState; X, Y: Integer);
var
  Item: TListItem;
begin
  Item := lvWindows.GetItemAt(X, Y);
  if Item <> nil then begin
    if (Integer(Item.Data) = 0) and Item.Checked then begin
      Item.Checked := False;
      MsgBox('This Explorer window cannot be restored because the location of ' +
          'the window doesn''t exist on this system.',
          bgOK, miWarning, 'Explorer Window Location doesn''t exist', 0);
    end;
  end;
end;}

procedure TIEWindows.FormCreate(Sender: TObject);
var
  BaseUnitX, BaseUnitY: Integer;
begin
  SetFormFont(Self, BaseUnitX, BaseUnitY);
  //InitFormFont(Self);
  //imgImages.Overlay(3, 0);
end;

procedure TIEWindows.FormDestroy(Sender: TObject);
begin
  ClearWinList;
end;

procedure TIEWindows.ClearWinList;
var
  i: Integer;
  Ico: TIcon;
  ExpWinPath: PExpWindowPath;
begin
  for i := 0 to lvWindows.Items.Count - 1 do begin
    ExpWinPath := PExpWindowPath(lvWindows.Items.Item[i].Data);
    if Assigned(ExpWinPath) then begin
      if Assigned(ExpWinPath.PIDL) then
        CoTaskMemFree(ExpWinPath.PIDL);
      Dispose(ExpWinPath);
    end;
  end;
  lvWindows.Items.Clear;
  imgImages.Clear;
  Ico := TIcon.Create;
  try
    Ico.ReleaseHandle;
    Ico.Handle := LoadIcon(hInstance, PChar(23));
    imgImages.AddIcon(Ico);
  finally
    Ico.Free;
  end;
end;

end.
