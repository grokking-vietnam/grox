# Prerequisites

## sdkman
`sdkman` là công cụ all-in-one cho phép thiết lập và quản lý phiên bản của nhiều sdk.

Cài đặt `sdkman` tại https://sdkman.io/install.

Kiểm tra việc cài đặt thành công bằng:
```
$ sdk version

SDKMAN 5.13.1
```

Chuyển phiên bản của sdk, ví dụ `scala`, trong terminal hiện tại:
```
$ sdk use scala 3.1.0
```

## java
```
$ sdk install java 11.0.12-open
```

## scala
```
$ sdk install scala 3.1.0
```

## sbt
`sbt` là công cụ để build, test và run Scala project.
```
$ sdk install sbt 1.6.1
```
Tìm hiểu thêm về `sbt` tại https://www.scala-sbt.org/learn.html.

# Build, test and run

Tại đường dẫn của project, khởi chạy `sbt` shell:

```
$ sbt
```

Trong sbt shell, compile/test/run bằng:
```
sbt> compiler/compile  
sbt> compiler/test
sbt> compiler/run
```

## (Optional) Build faster with `bloop`
`bloop` là ứng dụng chạy nền có nhiệm vụ tối ưu quá trình build cho IDE, build tool hay script.

Cài đặt `bloop` tại https://scalacenter.github.io/bloop/setup#sbt.

Sau khi cài đặt hoàn tất, bật bloop server bằng lệnh:
```
$ bloop server
Attempting a connection to the server...
Resolving ch.epfl.scala:bloop-frontend_2.12:1.4.12...
Starting bloop server at 127.0.0.1:8212...
```

Tại đường dẫn của project, xuất build từ `sbt` đến `bloop`:
```
$ sbt bloopInstall
... 
[success] Generated .bloop/root.json
[success] Generated .bloop/compiler.json
[success] Generated .bloop/root-test.json
[success] Generated .bloop/compiler-test.json
[success] Total time: 2 s, completed Jan 22, 2022, 1:47:58 PM

```

Lấy danh sách các sub-project:

```
$ bloop projects
compiler
compiler-test
root
root-test
```


Compile:
```
$ bloop compile compiler
```

Test:
```
$ bloop test compiler
```