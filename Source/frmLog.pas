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
 * Log form
 *
 ****************************************************************************)

unit frmLog;

interface

uses Classes, Graphics, Controls, Forms, StdCtrls, ThemeMgr;

type
  TLogWindow = class(TForm)
    txtLog: TMemo;
    ThemeManager1: TThemeManager;
    procedure FormCreate(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  LogWindow: TLogWindow;

implementation

{$R *.DFM}

uses Func;

procedure TLogWindow.FormCreate(Sender: TObject);
var
  BaseUnitX, BaseUnitY: Integer;
begin
  SetFormFont(Self, BaseUnitX, BaseUnitY);
  //InitFormFont(Self);
  SetFontNameSize(txtLog.Font, FormFont2, 10, FormFont2Alt, 10);
end;

end.
