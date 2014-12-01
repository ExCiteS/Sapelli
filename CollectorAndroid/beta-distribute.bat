@echo OFF

echo === Make sure to put correct/current release notes in beta-notes.txt ===
pause

echo ON

cd ..

call mvn clean install

cd CollectorAndroid

call mvn deploy -Prelease

pause