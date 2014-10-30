@echo off

adb kill-server
adb start-server

set currentDir=%cd:\=\%
"%ANDROID_HOME%\tools\monkeyrunner.bat" "%currentDir%\MonkeyRunner_Random_Clicks.py"