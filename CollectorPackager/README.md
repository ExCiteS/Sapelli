# CollectorPackager

Simple Java application to create and validate Sapelli Collector projects.

## Arguments
To run use:
```
java -jar sapelli-collector-cmdln-VERSION-with-dependencies.jar <args>
```


## Gradle Commands

Clean project:
```
gradlew CollectorPackager:clean
```

Build project:
```
gradlew CollectorPackager:install
```

which outputs the jar in:
```
..\CollectorPackager\build\libs\sapelli-collector-packager-VERSION-with-dependencies.jar
```
and will also push the `jar` to the Local Maven Repository. In Windows this is usually under:
```
C:\Users\USER_NAME\.m2\repository\uk\ac\ucl\excites\sapelli-collector-packager\VERSION
```