# Prerequisites
* **Java 17 SDK**
* **sbt version >= 1.6.2**


Dưới đây là hướng dẫn cài đặt các công cụ trên bằng [sdkman](https://sdkman.io), ngoài ra bạn cũng có thể tự tải xuồng dùng homebrew trên Mac hoặc apt trên Linux.

## sdkman
`sdkman` là công cụ all-in-one cho phép thiết lập và quản lý phiên bản của nhiều sdk. Cài đặt tại https://sdkman.io/install

## java sdk
```
$ sdk install java 17.0.2-oracle
```

## sbt
```
$ sdk install sbt 1.6.1
```

# Development

## IDE

Sử dụng `Intelij + Scala plugin` hoặc `VSCode/Vim + Metals`.

## Linting

```
$ sbt prepare
```

## Test

```
$ sbt test
```

Build

```
$ sbt build
```

Test một file duy nhất (ví dụ: ScannerTest.scala):
```
$ sbt "testOnly *ScannerTest"
```

## Compile

```
$ sbt compile
```

## Run

### Jvm

```

$ sbt cliJVM/run
```

### Javascript

```

$ sbt cliJS/fastLinkJS
# Scala version 3.1.2. Có thể phải update Scala version nếu cần.
$ node ./cli/js/target/scala-3.1.2/cli-fastopt/main.js
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

Lấy danh sách projects:

```
$ bloop projects
compiler
compiler-test
root
root-test
```


Compile:

```
$ bloop compile root
```

Test:

```

$ bloop test root
```

Test một file duy nhất:

```
$ bloop test compilerJVM -o grox.ScannerTest
```

## Docusaurus

Trang [Grox](https://grokking-vietnam.github.io/grox/) được build bằng [Docusaurus V2](https://docusaurus.io/)

Để thêm 1 nội dung mới trên trang Grox, bạn sẽ phải tạo 1 nội dung markdown trong thư mục `docs`.

Nếu nội dung mới cần hiển thị ở Sidebar, bạn cần chỉnh sửa file `website/sidebars.js`

Test locally:

```bash
sbt --client docs/mdoc
cd website
yarn install
yarn start
```