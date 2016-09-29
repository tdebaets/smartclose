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
 * Form to list system services to restore
 *
 ****************************************************************************)

unit frmServices;

interface

uses Windows, Messages, Classes, Graphics, Controls, Forms, StdCtrls, ExtCtrls,
    ThemeMgr, ComCtrls, ExtChkListView, NewCommCtrl, WinSvc, EnhListView,
    ExtListView;

type
  TServices = class(TForm)
    Label1: TLabel;
    Bevel1: TBevel;
    Button1: TButton;
    ThemeManager1: TThemeManager;
    lvServices: TExtChkListView;
    procedure lstServicesDrawItem2(Control: TWinControl; Index: Integer;
      ACanvas: TCanvas; Rect: TRect; State: TOwnerDrawState);
    procedure FormCreate(Sender: TObject);
    function lvServicesItemPrePaint(Control: TWinControl;
      var NMLVCD: tagNMLVCUSTOMDRAW; var FontChanged: Boolean): Boolean;
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Services: TServices;

implementation

uses Service, Func;

{$R *.DFM}

procedure TServices.lstServicesDrawItem2(Control: TWinControl;
    Index: Integer; ACanvas: TCanvas; Rect: TRect; State: TOwnerDrawState);
begin
  {if Integer(lstServices.ItemObject[Index]) = 1 then
    ACanvas.Font.Color := clRed;}
end;

procedure TServices.FormCreate(Sender: TObject);
var
  BaseUnitX, BaseUnitY: Integer;
begin
  SetFormFont(Self, BaseUnitX, BaseUnitY);
  //InitFormFont(Self);
end;

function TServices.lvServicesItemPrePaint(Control: TWinControl;
  var NMLVCD: tagNMLVCUSTOMDRAW; var FontChanged: Boolean): Boolean;
begin
  Result := True;
  with NMLVCD do begin
    if Integer(lvServices.Items[nmcd.dwItemSpec].Data)
        in [0, SERVICE_STATUS_DISABLED] then begin
      clrText := ColorToRGB(clRed);
      FontChanged := True;
    end;
  end;
end;

end.
