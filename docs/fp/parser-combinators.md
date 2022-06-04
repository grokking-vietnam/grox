# Parser Combinators

## Introduction

Trong bài này, mình sẽ trình bày về parser combinators một kỹ thuật thường được sử dụng trong Functional Programming khi giải quyết các bài toán parsing. Nếu chúng ta coi Parser là một function có input là String và output là một structured data; thì parser combinator là một higher-order function nhận một hoặc nhiều parser và kết hợp chúng lại thành một parser mới. Ý tưởng ở đây là chúng ta có thể dùng các parser đơn giản và gộp chúng lại bằng các parser combinator để giải quyết các bài toán phức tạp hơn.

Một số ví dụ bài toán về parsing như: compiler cần parse source code thành [Abstract Syntax Tree/AST](https://en.wikipedia.org/wiki/Abstract_syntax_tree) hoặc một chuỗi các token (nếu compiler tách riêng [scanning](../book/3-scanning.md)); json parser sẽ parser json string thành các data class.

Sau đây mình sẽ sử dụng ngôn ngữ [Scala 3](https://docs.scala-lang.org/scala3/getting-started.html) và thư viện [cats-parse](https://github.com/typelevel/cats-parse) để giải thích về parser combinator nhưng trước hết hãy bắt đầu bằng việc khám phá type của parser[^1].

## Parser representation

Như chúng ta đã nói ở trên, parser là một function nhận String và trả về một kết quả mà chúng ta đang mong đợi. Từ đó chúng ta có thể định nghĩa parser như sau:

```scala
type Parser[A] = String => A
```

Type như vậy thì đơn giản nhưng chưa đủ. Điều đầu tiên chúng ta thấy là không phải lúc nào cũng có thể parse thành công. Khi input không đúng cú pháp thì parser phải trả về lỗi tương ứng. Và trong thế giới của Functional Programming thì chúng ta phải thể hiện điều đó khi khai báo kiểu cho parser. Sau đây là định nghĩa mới của parser bằng cách sử dụng Either [^either]

```scala
type Parser[A] = String => Either[ParseError, A]
```

Điều tiếp theo là như chúng ta đã nói ở trên về parser combinator, chúng ta không muốn viết một cái parser phức tạp một lần duy nhất, mà chúng ta muốn viết những parser đơn giản, cơ bản rồi kết hợp chúng lại với nhau. Ví dụ như khi parse một biểu thức toán học đơn giản như sau `(3 + 5) * 4` chúng ta sẽ viết một cái parser cho dấu `(` rồi một parser khác cho số tự nhiên, rồi các phép toán ... Do đó parser của chúng ta không nên tiêu thụ hoàn toàn String input mà chỉ nên sử dụng một phần và trả về phần còn lại cho các parser tiếp theo tiêu thụ. Từ đó chúng ta có một định nghĩa mà chúng ta có thể thoã mãn như sau:

```scala
type Parser[A] = String => Either[Parser.Error, (String, A)]
```

Trong thực tế thì Parser được định nghĩa phức tạp hơn một xíu như sau:

```scala
sealed abstract class Parser[+A] {
  final def parse(str: String): Either[Parser.Error, (String, A)]

  // Attempt to parse all of the input `str` into an `A` value.
  final def parseAll(str: String): Either[Parser.Error, A]
}

```

## Parser with cats

Sau khi đã có định nghĩa của parser, chúng ta sẽ khám cách sử dụng parser và parser combinator bằng library [cats-parse](https://github.com/typelevel/cats-parse) - một trong những parser combinators library cho ngôn ngữ Scala. Các phần sau đa phần được lấy trực tiếp từ [cats-parse](https://github.com/typelevel/cats-parse#readme) với một số chỉnh sửa.


### Simple Parsers

Library cats-parse cung cấp một tập hợp các parser cơ bản, để tạo thành các building block cho bất cứ parser phức tạp nào.


Đầu tiên là `Parser.anyChar`, là một parser luôn luôn trả về ký tự đầu tiên của chuỗi input (fail trong trường hợp đầu vào là một empty string).

```scala
val p: Parser[Char] = Parser.anyChar

p.parse("t")
// Either[Error, Tuple2[String, Char]] = Right((,t))
p.parse("")
// Either[Error, Tuple2[String, Char]] = Left(Error(0,NonEmptyList(InRange(0,,))))
p.parse("two")
// Either[Error, Tuple2[String, Char]] = Right((wo,t))
```

`Parser.string` là parser mà nó sẽ parse thành công nếu string input bắt đầu với giá trị của `str`. Chú ý rằng `Parser.string` sẽ trả về một parser có type là `Parser[Unit]`, điều đó có nghĩa là nó sẽ trả về `Unit` nếu thành công.

```scala
val p: Parser[Unit] = Parser.string("hello")

p.parse("hello")
// Either[Error, Tuple2[String, Unit]] = Right((,()))
p.parse("hell")
// Either[Error, Tuple2[String, Unit]] = Left(Error(0,NonEmptyList(OneOfStr(0,List(hello)))))
p.parse("hello world")
// Either[Error, Tuple2[String, Unit]] = Right(( world ,hello))
```

`sp` tương tự như `Parser.anyChar` nhưng chỉ đúng khi ký tự đầu tiên là ký tự khoảng trắng.

```scala
import cats.parse.Rfc5234.sp

sp.parse(" ")
// Either[Error, Tuple2[String, Unit]] = Right((,()))
sp.parse("o_o")
// Either[Error, Tuple2[String, Unit]] = Left(Error(0,NonEmptyList(InRange(0, , ))))

```

`alpha` tương tự như `Parser.anyChar` nhưng chỉ đúng khi ký tự đầu tiên là ký tự alphabet.

```scala
import cats.parse.Rfc5234.alpha
alpha.parse("z")
// Either[Error, Tuple2[String, Char]] = Right((,z))
alpha.parse("3")
// Either[Error, Tuple2[String, Char]] = Left(Error(0,NonEmptyList(InRange(0,A,Z), InRange(0,a,z))))
```

`digit` tương tự như `Parser.alpha` nhưng chỉ đúng khi ký tự đầu tiên là ký tự từ 0-9

```scala
import cats.parse.Rfc5234.digit

digit.parse("3")
// Either[Error, Tuple2[String, Char]] = Right((,3))
digit.parse("z")
//  Either[Error, Tuple2[String, Char]] = Left(Error(0,NonEmptyList(InRange(0,0,9))))
```

`Parser.charIn` nhận một string đầu vào và trả về một parser mà nó sẽ parse thành công nếu ký tự đầu tiên là một character trong string đầu vào.

```scala
val charIn = Parser.charIn("123456789") // tương đương với digit
charIn.parse("3")
// Either[Error, Tuple2[String, Char]] = Right((,3))
```


### Mapping Output


Đầu ra của parser có thể được xử lý bằng `map` function.

```scala
case class CharWrapper(value: Char)

val p: Parser[CharWrapper] = Parser.anyChar.map(char => CharWrapper(char))

p.parse("t")
// Right((,CharWrapper(t)))
```

Library cung cấp sẵn một số hàm cho để mapping sang type `String` và `Unit` dễ dàng hơn

```scala
/* String */

val p2: Parser[String] = digit.map((c: Char) => c.toString)
// tương đương với
val p3: Parser[String] = digit.string

p3.parse("1")
// Either[Error, Tuple2[String, String]] = Right((,1))

/* Unit */

val p4: Parser[Unit] = digit.map(_ => ())
// tương đương với
val p5: Parser[Unit] = digit.void

p5.parse("1")
// Either[Error, Tuple2[String, Unit]] = Right((,()))
```

*Chú ý*: `string` sẽ trả về input mà parser đã sử dụng, và bỏ qua output.

### Combinging parsers

Các parser có thể kết hợp với nhau bằng các operator sau:

- `~` - `product` - Tiếp tục parsing parser thứ 2 nếu parser đầu tiên thành công;
- `<*` - `productL` - Như hàm `product` nhưng bỏ qua kết quả của parser thứ 2;
- `*>` - `productR` - Như hàm `product` nhưng bỏ qua kết quả của parser đầu tiên;
- `suroundedBy` - tương đương với `border *> parsingResult <* border`;
- `between` - tương đương với `border1 *> parsingResult <* border2`;
- `|` - `orElse` - Parser thành công nếu một trong hai parser đầu vào thành công;

```scala
import cats.parse.Rfc5234.{sp, alpha, digit}
import cats.parse.Parser

/* Product */

// product
val p1: Parser[(Char, Unit)] = alpha ~ sp

p1.parse("t")
// Either[Error, Tuple2[String, Tuple2[Char, Unit]]] = Left(Error(1,NonEmptyList(InRange(1, , ))))
p1.parse("t ")
// Either[Error, Tuple2[String, Tuple2[Char, Unit]]] = Right((,(t,())))

/* productL, productR */

// Tương tự như `p1` nhưng đầu ra sẽ có type là `Parser[Char]` thay vì `Tuple2`
// vì <* sẽ bỏ quả kết quả của parser phía bên trái
val p2: Parser[Char] = alpha <* sp

p2.parse("t")
// Either[Error, Tuple2[String, Char]] = Left(Error(1,NonEmptyList(InRange(1, , ))))
p2.parse("t ")
// Either[Error, Tuple2[String, Char]] = Right((,t))

// Chú ý nếu muốn bỏ qua kết quả của alpha thì chuyển mũi tên
val p21: Parser[Unit] = alpha *> sp

/* surroundedBy */

val p4: Parser[Char] = sp *> alpha <* sp
val p5: Parser[Char] = alpha.surroundedBy(sp)

p4.parse(" a ")
// Either[Error, Tuple2[String, Char]] = Right((,a))
p5.parse(" a ")
// Either[Error, Tuple2[String, Char]] = Right((,a))

/* between */

val p6: Parser[Char] = sp *> alpha <* digit
val p7: Parser[Char] = alpha.between(sp, digit)

p6.parse(" a1")
// Either[Error, Tuple2[String, Char]] = Right((,a))
p7.parse(" a1")
// Either[Error, Tuple2[String, Char]] = Right((,a))

/* OrElse */

val p3: Parser[AnyVal] = alpha | sp

p3.parse("t")
// Either[Error, Tuple2[String, AnyVal]] = Right((,t))
p3.parse(" ")
// Either[Error, Tuple2[String, AnyVal]] = Right((,()))
```


### Repeating parsers

`cats-parse` cung cấp 2 function để chúng ta biến một Parser[A] thành Paser[List[A]] đó là `rep` và `rep0`. Với `rep` parser cần phải parse thành công ít nhất một phần tử, còn `rep0` thì có thể cho ra một List rỗng.

```scala
val number: Parser[NonEmptyList[Char]] = digit.rep
val numberOrNone: Parser0[List[Char]] = digit.rep0

number.parse("73")
// Either[Error, Tuple2[String, NonEmptyList[Char]]] = Right((,NonEmptyList(7, 3)))
number.parse("")
// Either[Error, Tuple2[String, NonEmptyList[Char]]] = Left(Error(0,NonEmptyList(InRange(0,0,9))))
numberOrNone.parse("")
// Either[Error, Tuple2[String, List[Char]]] = Right((,List()))
numberOrNone.parse("73")
// Either[Error, Tuple2[String, List[Char]]] = Right((,List(7, 3)))
```

**Chú ý**: type của number và numberOrNone là khác nhau. `Parser` type luôn luôn trả về non-empty ouput, còn `Parser0` thì có thể empty(trong trường hợp thành công).

`rep` và `rep0` có thể kết hợp với `string` function mà chúng ta đã nhắc đến ở trên.

```scala
val word1 = alpha.rep.map((l: NonEmptyList[Char]) => l.toList.mkString)
val word2 = alpha.rep.string
val word2 = alpha.repAs[String]

word1.parse("bla")
// Either[Error, Tuple2[String, String]] = Right((,bla))
```

3 parser ở trên hoàn toàn giống nhau về mặt kết quả, nhưng 2 parser sau sẽ tối ưu hơn vì chúng không phải tạo ra List trung gian

#### Parsers with empty output

Có một số parser không bao giờ trả về kết quả và type của chúng sẽ là `Parser0`. Chúng ta có thể chuyển type `Parser` về `Parser0` bằng `rep0` hoặc `?` aka `optional`.


```scala
val p: Parser[String] = (alpha.rep <* sp.?).rep.string

p.parse("hello world")
// Either[Error, Tuple2[String, String]] = Right((,hello world))
```

### Error Handling

Như chúng ta đã nói ở phần đầu tiên, một parser nếu parse không thành công thì sẽ phải trả về lỗi. Có 2 kiểu lỗi:
- *epsilon failure*: lỗi mà parser chưa sử dụng bất kỳ một character nào
- *arresting failure*: lỗi mà parser đã sử dụng ít nhất một character

Về mặt implementation `Parser.Error` có định nghĩa như sau:
```scala
final case class Error(failedAtOffset: Int, expected: NonEmptyList[Expectation])
```

Nếu `failedAtOffset == 0` thì đó là `epsilon failure` và `arresting failure` trong trường hợp còn lại.

```scala
val p1: Parser[Char] = alpha
val p2: Parser[Char] = sp *> alpha

// epsilon failure
p1.parse("123")
// Either[Error, Tuple2[String, Char]] = Left(Error(0,NonEmptyList(InRange(0,A,Z), InRange(0,a,z))))

// arresting failure
p2.parse(" 1")
// Either[Error, Tuple2[String, Char]] = Left(Error(1,NonEmptyList(InRange(1,A,Z), InRange(1,a,z))))
```

Chúng ta cần phân biệt hai loại lỗi này vì, loại đầu tiên cho chúng ta biết parser không khớp với input đầu vào ngay khi bắt đầu parse, và loại thứ 2 xảy ra trong quá trình parse.

### Backtrack

Backtrack là một function giúp chúng ta chuyển *arresting failure* thành *epsilon failure*. Nó cũng giúp tua lại offset của input về trước khi parser bắt đầu. Đây là một function cực kỳ hữu dụng khi chúng ta muốn kết hợp nhiều parser lại với nhau.

```scala
val p1 = sp *> digit <* sp // _digit_
val p2 = sp *> digit // _digit

p1.parse(" 1") // (1)
// Left(Error(2,NonEmptyList(InRange(2, , ))))

(p1 | p2 ).parse(" 1") // (2)
// Either[Error, Tuple2[String, Char]] = Left(Error(2,NonEmptyList(InRange(2, , ))))

(p1.backtrack | p2 ).parse(" 1") (3)
// Either[Error, Tuple2[String, Char]] = Right((,1))
```

Parser(2) trả về lỗi bởi vì p1 trả về *arresting failure* và operator `|` chỉ có thể phục hồi với *epsilon failure*.

Parser(3) parse thành công vì `backtrack` chuyển *arresting failure* thành *epsilon failure* nên sau khi thất bại với `p1.backtrack` (3) sẽ tiếp tục với `p2` và thành công.


### Soft

Có hiệu ứng tương tự như `backtrack` với `|`, nhưng với operator `~` và nó cho phép chúng ta tiếp tục parsing khi toán tử bên phải trả về `epsilon failure`. Nó rất hữu ích trong trường hợp chúng ta không biết chính xác output mà chúng ta cần trước khi quá trình parsing kết thúc. Như ví dụ dưới đây chúng ta parse đầu vào cho một search engine. Input có thể có dạng `key:value` hoặc chỉ mỗi `value`

```scala
val searchWord = (alpha.rep.string ~ sp.?).rep.string
val fieldValue = alpha.rep.string ~ pchar(':')

val fieldValueSoft = alpha.rep.string.soft ~ pchar(':')

val p1 = fieldValue.? ~ searchWord
val p2 = fieldValueSoft.? ~ searchWord

p2.parse("title:The Wind Has Risen") (1)
// Right((,(Some((title,())),The Wind Has Risen)))
p2.parse("The Wind Has Risen") // (2)
// Right((,(None,The Wind Has Risen)))
p1.parse("The Wind Has Risen") // (3)
// Left(Error(3,NonEmptyList(InRange(3,:,:))))
```

Parser `p2` parse thành công ở (2) trong khi `p1` thất bại ở (3) bởi vì `fieldValueSoft` trả về *epsilon failure* với sự giúp đỡ của `soft` từ đó `p2` có thể tiếp tục parsing với `searchWord` trong khi `p1` trả về lỗi vì nó nhận được *arresting failure* từ `failValue` parser.

## Resources

- [Parser Combinators Walkthrough](https://hasura.io/blog/parser-combinators-walkthrough)
- [cats-parse](https://github.com/typelevel/cats-parse)
- [Parser Combinators tutorial in Haskell](https://tgdwyer.github.io/parsercombinators/)

[^1]: Trong phần đầu mình sẽ sử dụng pseudocode, implmentation trong thực tế sẽ tương tự nhưng phức tạp hơn.
[^either]: `Either[E, A]` được định ngĩa như sau(phiên bản sơ lược):

    ```scala
      enum Either[E, A] {
        case Left(value: E)
        case Right(value: A)
      }
    ```

    là kiểu dữ liệu mà nó đại diện cho 2 khả năng hoặc là Left với giá trị của type E hoặc là Right với giá trị của type A. Left thường được dùng để biểu thị trường hợp lỗi, và Right cho trường hợp thành công.
