call mvn versions:set -DnewVersion=%1

call mvn versions:commit

cd CollectorAndroid

call mvn android:manifest-update

cd ..