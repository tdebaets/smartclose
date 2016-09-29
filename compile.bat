@echo off

rem **************************************************************************
rem *
rem *            SmartClose
rem *
rem *            Copyright (c) 2016 Tim De Baets
rem *
rem **************************************************************************
rem *
rem * Licensed under the Apache License, Version 2.0 (the "License");
rem * you may not use this file except in compliance with the License.
rem * You may obtain a copy of the License at
rem *
rem *     http://www.apache.org/licenses/LICENSE-2.0
rem *
rem * Unless required by applicable law or agreed to in writing, software
rem * distributed under the License is distributed on an "AS IS" BASIS,
rem * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
rem * See the License for the specific language governing permissions and
rem * limitations under the License.
rem *
rem **************************************************************************
rem *
rem * Compile script
rem *
rem **************************************************************************

setlocal

set CFGFILE=
set OLDCFGFILE=

rem  Quiet compile / Build all / Output warnings
set DCC32OPTS=-Q -B -W

rem Generate unique number for temporary file renames
set RND=%RANDOM%

rem  Retrieve user-specific settings from file

if exist userprefs.bat goto userprefsfound
:userprefserror
echo userprefs.bat is missing or incomplete. It needs to be created with
echo the following lines, adjusted for your system:
echo.
echo   set DELPHIROOT=c:\delphi4              [Path to Delphi 4 (or later)]
goto failed2

:userprefsfound
set DELPHIROOT=
call .\userprefs.bat
if "%DELPHIROOT%"=="" goto userprefserror

for /F %%i in ('dir /b /a "common\*" 2^>NUL') do (
    rem common submodule folder not empty, ok
    goto common_ok
)

echo The common subdirectory was not found or is still empty.
echo Did you run postclone.bat yet?
goto failed2

:common_ok

set COMMONDIR=..\common
set LIB_PATH=%COMMONDIR%\Delphi\LibFixed;%DELPHIROOT%\lib;%COMMONDIR%\Delphi\LibUser;%COMMONDIR%\Delphi\Imports

rem -------------------------------------------------------------------------

rem  Compile each project separately because it seems Delphi carries some
rem  settings (e.g. $APPTYPE) between projects if multiple projects are
rem  specified on the command line.

rem  Always use 'master' .cfg file when compiling from the command line to
rem  prevent user options from hiding compile failures in official builds.
rem  Temporarily rename any user-generated .cfg file during compilation.

cd Source
if errorlevel 1 goto failed

echo - SmartClose.dpr

rem  Rename user-generated .cfg file if it exists
if not exist SmartClose.cfg goto smartclose
ren SmartClose.cfg SmartClose.cfg.%RND%
if errorlevel 1 goto failed
set OLDCFGFILE=SmartClose.cfg

:smartclose
ren SmartClose.cfg.main SmartClose.cfg
if errorlevel 1 goto failed
set CFGFILE=SmartClose.cfg
"%DELPHIROOT%\bin\dcc32.exe" %DCC32OPTS% %1 ^
    -U"%LIB_PATH%" ^
    -R"%DELPHIROOT%\lib" ^
    SmartClose.dpr
if errorlevel 1 goto failed
ren %CFGFILE% %CFGFILE%.main
set CFGFILE=
if not "%OLDCFGFILE%"=="" ren %OLDCFGFILE%.%RND% %OLDCFGFILE%
set OLDCFGFILE=

echo Success!
cd ..
goto exit

:failed
if not "%CFGFILE%"=="" ren %CFGFILE% %CFGFILE%.main
if not "%OLDCFGFILE%"=="" ren %OLDCFGFILE%.%RND% %OLDCFGFILE%
echo *** FAILED ***
cd ..
:failed2
exit /b 1

:exit
