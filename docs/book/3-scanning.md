# Scanning


* [Introduction](#introduction)
* [Lexemes and Tokens](#lexemes-and-tokens)
* [Error Handling](#error-handling)
* [Regular language](#regular-language)
* [Implementation](#implementation)
* [Resources](#resources)


## Introduction

Là bước đầu tiên của bất cứ compiler hoặc interpreter nào. Scanning hay còn được gọi là lexing hoặc lexical analysis. Một scanner (hoặc lexer) nhận một string và chia nhỏ nó thành một chuỗi của token. Token có thể coi như "từ" và "dấu câu" được dùng để tạo nên ngữ pháp của một ngôn ngữ.


Fun fact: **Lexical** comes from the Greek root **lex**, meaning **word**

## Lexemes and Tokens

Đây là một dòng code với ngôn ngữ Lox

```
var language = "lox";
```

Ở đây, `var` là từ khoá(keyword) để định nghĩa một biến(variable) mới. Ba ký tự `v-a-r` có một ý nghĩa nhất định ở đây. Và đó cũng là nhiệm vụ chính của scanner - duyệt chuỗi ký tự và trả nhóm chúng lại thành những từ có nghĩa. Mỗi một nhóm ký tự này được gọi là một `lexeme`. Lexeme tương ứng với các raw string trong source code. Lexeme có thể chỉ gồm một ký tự: `{`, `;` hoặc nhiều ký tự như number `123`, string `"hi!"`. Tương ứng với ví dụ bên trên, lexeme sẽ giống như ảnh phía dưới.

![lexeme](https://i.imgur.com/A2NMsRL.png)


Trong source file có nhiều kí tự không mang nhiều ý nghĩa như whitespace hoặc comment. Scanner thông thường sẽ bỏ qua những ký tự này.

Trong quá trình scanning, ngoài việc nhóm các ký tự thành lexeme, thì scanner còn thu thập thêm các thông tin hữu ích khác. Chúng ta gộp lexeme và những thông tin đó lại thành một Token.

`Token = lexeme + additional information`

Các thông tin đó có thể bao gồm:

- Token type: Khi scanner nhận ra một lexeme thì nó sẽ lưu lại kiểu của lexeme đó luôn. Chúng ta có một số loại token như sau: Keyword, operator, literal (number/string/identifier).
- Literal value: giá trị cho các token có kiểu literal
- Location information: Việc lưu lại vị trí của mỗi lexeme trong mỗi token sẽ giúp chúng ta có khả năng hiển thị cho người dùng vị trí của lỗi nếu xảy ra.

## Error Handling

Scanner còn có nhiệm vụ report lỗi cho người dùng. Thông thường vấn đề này hay bị bỏ qua trên lý thuyết nhưng rất quan trọng cho người dùng trên thực tế.

## Regular language

Mỗi ngôn ngữ lập trình đều có một bộ quy tắc (set of rule) riêng để xác định một chuỗi ký tự có phải là một lexeme hợp lệ hay không? Bộ quy tắc đó được gọi là lexical grammar. Lox language cũng như đa phần các ngôn ngữ lập trình khác, chúng ta có thể dùng regular expression để nhận ra tất cả các lexeme hợp lệ. Các ngôn ngữ này được gọi là regular language. Một số tool như [Lex](http://dinosaur.compilertools.net/lex/) hoặc [Flex](https://github.com/westes/flex) có thể generate scanner bằng regular expression.

Func fact: Lex was created by Mike Lesk and Eric Schmidt - who was executive chairman of Google.


### Implementation

Đứng trước bài toán scanning/parsing thông thường chúng ta có những lựa chọn như sau:

* Parser có sẵn: dành cho các format phổ biến như Json, xml.
* Dùng `String.split`: Thường chỉ được dùng cho những trường hợp đơn giản.
* `Regular expression`: Được sử dụng khi không cần kết quả chính xác 100%. Thông thường các parser sử dụng regex thì không ổn định, hay bị lỗi (cả false positives và false negatives)
* Parser generator: Một số tool như [Yacc](https://en.wikipedia.org/wiki/Yacc) hay [ANTLR](https://en.wikipedia.org/wiki/ANTLR) có thể generate source code của parser dựa vào lexical grammar. Cái này được sử dụng rộng rãi nhưng việc set up khá là phiền toái.
* Recursive descent parser (top down parser) style: parser kiểu này sẽ duyệt từ string input từ đầu đến cuối, cố gắng đoán xem chuỗi ký tự hiện tại có phải token hay không? Nếu đúng thì chuỗi ký tự đó sẽ được thêm vào token list sau rồi tiếp tục quá trình đó đến khi gặp EOF. Đây là phương án mà [Crafting Interpreter] chọn để implement. Các bạn có thể xem source của Scanner ở [đây](https://github.com/munificent/craftinginterpreters/blob/master/java/com/craftinginterpreters/lox/Scanner.java).
* Parser combinators: Cũng là một dạng top down parser nhưng được phổ biến trong giới functional programming. Đây cũng là phương án mà nhóm chọn để implement. Mọi người có thể xem kỹ hơn về nó ở [đây](../fp/parser-combinators.md)


## Resources

- [Scanning chapter](http://craftinginterpreters.com/scanning.html)
- [fastparser](https://www.lihaoyi.com/post/EasyParsingwithParserCombinators.html)
