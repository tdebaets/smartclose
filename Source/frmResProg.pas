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
 * Form that shows details about a process to restore
 *
 ****************************************************************************)

unit frmResProg;

interface

uses Windows, Messages, Classes, Graphics, Controls, Forms, ExtCtrls, StdCtrls,
    ThemeMgr;

type
  TResProg = class(TForm)
    imgIcon: TImage;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    Label6: TLabel;
    txtCurDir: TEdit;
    txtWarnings: TMemo;
    txtTitle: TEdit;
    txtCmdLine: TEdit;
    txtOrgFName: TEdit;
    txtLocalFName: TEdit;
    txtArgs: TEdit;
    Bevel1: TBevel;
    btnOK: TButton;
    ThemeManager1: TThemeManager;
    procedure FormCreate(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  ResProg: TResProg;

implementation

uses Func;

{$R *.DFM}

procedure TResProg.FormCreate(Sender: TObject);
var
  BaseUnitX, BaseUnitY: Integer;
begin
  SetFormFont(Self, BaseUnitX, BaseUnitY);
  //InitFormFont(Self);
  ActiveControl := btnOK;
end;


end.
