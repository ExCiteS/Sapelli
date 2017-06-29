# Release instructions

1. Edit or create [CollectorPackager/CHANGELOG.md](CHANGELOG.md) to list changes since last release.
2. Edit the version in the [CollectorPackager/build.gradle](build.gradle) to the correct version e.g. 1.0.0
3. Ensure that the CollectorPackager builds correctly by running: `gradlew CollectorPackager:build`
4. Commit the changes on git and tag the commit with the version.
5. Push to github, including tags.
6. Release the new version using the `gradlew CollectorPackager:archiveRelease` task.
7. Create a new release on github based on the tag and attach the archive to it (take it from the dir: `CollectorPackager\build\Sapelli Packager VERSION.zip`)
8. Edit the version in CollectorPackager/build.gradle to the correct `1.0.0-SNAPSHOT` (!!!)
9. Commit to git (message: _Changed version to 1.0.0-SNAPSHOT to open development of v1.0.X+1_)
10. Push to github
