# Release instructions

1. Edit or create [CollectorPackager/CHANGELOG.md](CHANGELOG.md) to list changes since last release.
2. Edit the version in the [CollectorPackager/build.gradle](build.gradle) to the correct version e.g. 1.0.0
3. Ensure that the CollectorPackager builds correctly by running: `gradlew CollectorPackager:assembleRelease`
4. Commit the changes on git and tag the commit with the version.
5. Push to github, including tags
6. Create a new release on github based on the tag and attach the exe to it (take it from the dir: `CollectorPackager\build\launch4j\Sapelli Packager VERSION.exe`)
7. Edit the version in CollectorPackager/build.gradle to the correct `1.0.0-SNAPSHOT` (!!!)
8. Commit to git (message: _Changed version to 1.0.0-SNAPSHOT to open development of v1.0.X+1_)
9. Push to github
