# CollectorPackager

Simple Java application to create and validate Sapelli Collector projects.



There are two ways to run the CollectorPackager:

* As a jar from the command line
* As an executable (.exe) on Windows



## Run as a Jar
To run use:
```shell
java -jar "Sapelli Packager v1.x.x.jar"
```



## Gradle Commands



### List Tasks:

```
gradlew -q -b CollectorPackager\build.gradle tasks
```



### Clean:
```
gradlew CollectorPackager:clean
```



### Build:

The build.gradle uses the **Gradle shadowJar** plugin to assemble the application and all it's dependencies into a single ***"fat"*** jar.

To build the "fat jar" run:

```
gradlew CollectorPackager:shadowJar
```


which outputs the jar in:
```
..\CollectorPackager\build\libs\Sapelli Packager v1.x.x.jar
```
(You can copy the jar and run it anywhere there is a Java 8+ JDK. It contains all the dependencies it needs so you don't need to install any dependencies on the target machine).



### Run:

Because the application plugin is being used, you may directly run the application:

```
gradlew CollectorPackager:run
```



### Dependencies:

Displays a report of the project dependencies that are up-to-date, exceed the latest version found, have upgrades, or failed to be resolved. 
```
gradlew CollectorPackager:dependencyUpdates
```



### Create .exe:

Sapelli Packager has the gradle-launch4j that generates an executable (exe) out of the compiled fat jar. To create the exe, run:

```
gradlew CollectorPackager:createExe
```

This will place the exe in:
```
..\CollectorPackager\build\launch4j\Sapelli Packager v1.x.x.exe
```


### Prepare archive for GitHub:

This task will create a zip archive, containing the latest version of the packager for Windows and Unix operating systems (an executable exe and a jar respectively). The task will firstly run **clean**, **build**, **shadowJar** and **createExe**, so there is no need to run any of those before that. The task will ensure that the version is a release and not a **SNAPSHOT** version.

```
gradlew CollectorPackager:archiveRelease
```

This will place the the archives under the `\build` directory:

```
..\CollectorPackager\build\Sapelli Packager v1.x.x-Unix.zip
..\CollectorPackager\build\Sapelli Packager v1.x.x-Windows.zip
```

