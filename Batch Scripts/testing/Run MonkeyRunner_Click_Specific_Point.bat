@echo off

:: Deal with unresponsive adb
adb kill-server
adb start-server
echo.

set iterations=%~1
set frequency=%~2

:: Get user input:
if "%iterations%"=="" (set /p iterations="Enter how many times to run the script: " %=%)

if "%frequency%"=="" (set /p frequency="Enter interaction frequency in milliseconds: " %=%)

set currentDir=%cd:\=\%
"%ANDROID_HOME%\tools\monkeyrunner.bat" "%currentDir%\MonkeyRunner_Click_Specific_Point.py" -i %iterations% -f %frequency%