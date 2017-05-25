# CollectorCmdLn

Simple command-line interface to load/verify Sapelli Collector projects.

## Arguments
To run use:
```
java -jar sapelli-collector-cmdln-VERSION-with-dependencies.jar <args>
```


### Help

To print all the arguments that the **CollectorCmdLn** accepts:
```
java -jar sapelli-collector-cmdln-VERSION-with-dependencies.jar -help
```

Currently, the arguments that the CollectorCmdLn accepts are:

```
usage: To use CollectorCmdLn, run the jar wiht the folowing arguments:
-geokey            Produce 'sapelli_project_info' (JSON) for geokey_sapelli
-help              Print list of arguments
-json              Produce JSON output
-load <sap_file>   Sapelli project (*.sap) to load
-p <arg>           Sapelli working directory
```

## Gradle Commands

Clean project:
```
gradlew CollectorCmdLn:clean
```

Build project:
```
gradlew CollectorCmdLn:install
```

which outputs the jar in:
```
..\CollectorCmdLn\build\libs\sapelli-collector-cmdln-VERSION-with-dependencies.jar
```
and will also push the `jar` to the Local Maven Repository. In Windows this is usually under:
```
C:\Users\USER_NAME\.m2\repository\uk\ac\ucl\excites\sapelli-collector-cmdln\VERSION
```