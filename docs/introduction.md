# Introduction

* [Mục tiêu](#mục-tiêu)
* [Crafting Interpreters](#crafting-interpreters)
* [Cách hoạt động](#cách-hoạt-động)

## Mục tiêu

* Tìm hiểu về interpreter/compiler
  * Các lý thuyết và khái niệm về cần thiết
  * Implement interpreter cho một programming language
  * Cách thiết kế một programming languages

* Functional Programming
  * Sử dụng một pure functional programming language để viết interpreter
  * Tìm hiểu sâu hơn về các advanced FP techniques/concepts (Monad, parser combinator)

* Thực hành coding best practices
  * Testing
  * Github workflow/PR/Code review
  * CI (CD???)

## Crafting Interpreters

Team sẽ sử dụng quyển [Crafting Interpreters](http://craftinginterpreters.com).

- Free
- Có nhiều recommendation
- Cân bằng hợp lý giữa lý thuyết và thực hành
- Code bằng Java/C nên dễ đọc hiểu

### Nội dung

Quyển [Crafting Interpreters](http://craftinginterpreters.com) sẽ implement 2 interpreters cho ngôn ngữ [Lox](book/1-lox-language.md).

Interpreter đầu tiên

  * Sử dụng Java
  * Focus vào các concepts, techniques của interpreter/compiler
  * Giúp người đọc hiểu rõ về programming language (design, behaviours)

Interpreter thứ hai

  * Sử dụng C
  * Implement một số data structures cần thiết (vd: dynamic array, hash table).
  * Tối ưu hoá tốc độ bằng cách compile thành bytecode và viết một VM để thực thi trên runtime.

## Cách hoạt động

- Mỗi người trong team sẽ chia nhau làm từng chương
- Sau khi đọc xong thì sẽ code, rồi tạo PR trên github
- Các thành viên khác sẽ review/discuss trong PR đó
- Sau đó thì sẽ viết report & present cho grokking lab team
- Mọi người có thể làm việc song song với nhau (miễn là có sự thoả thuận về code interface)

### Code & Tools

- [Scala 3](https://docs.scala-lang.org/scala3/new-in-scala3.html)
- [Typelevel Stack](https://typelevel.org/)
- Github workflow/PR/Code review

### Team

[![Team](https://contrib.rocks/image?repo=grokking-vietnam/grox)](https://github.com/grokking-vietnam/grox/graphs/contributors)
