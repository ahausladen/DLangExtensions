@echo off
SETLOCAL

git submodule update --init

call clean.bat
call rsvars.bat

msbuild /nologo /t:Build DLangExtensionGroup.groupproj
if ERRORLEVEL 1 goto Error

goto Leave
:: ===========================================
:Error
pause

:Leave

ENDLOCAL