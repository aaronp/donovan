# Donovan

This project declares some data types for working with javascript paths and expressions.




## Usage


```
libraryDependencies += "com.github.aaronp" %%% "donovan" % "0.0.1"
```

in maven:

```
<dependency>
  <groupId>com.github.aaronp</groupId>
  <artifactId>donovan_2.12</artifactId>
  <version>0.0.1</version>
</dependency>
```


## Building
To build a new release:

```
sbt release
sbt publishSigned
sbt sonatypeRelease
```