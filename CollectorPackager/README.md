# CollectorPackager

Simple Java application to create and validate Sapelli Collector projects.



There are two ways to run the CollectorPackager:

* As a jar from the command line
* As an executable (.exe) on Windows



## Run as a Jar
To run use:
```shell
java -jar sapelli-collector-cmdln-VERSION-with-dependencies.jar
```



## Gradle Commands

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
..\CollectorPackager\build\libs\sapelli-collector-packager-VERSION-with-dependencies.jar
```
and will also push the `jar` to the Local Maven Repository. In Windows this is usually under:
```
C:\Users\USER_NAME\.m2\repository\uk\ac\ucl\excites\sapelli-collector-packager\VERSION
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
..\CollectorPackager\build\launch4j\Sapelli Packager VERSION.exe
```