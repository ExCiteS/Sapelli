call mvn versions:set -DnewVersion=%1

call mvn versions:commit

cd CollectorAndroid

call mvn build-helper:parse-version android:manifest-merger

cd ..