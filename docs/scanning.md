# Scanning

* [Scanning](#scanning)
    * [Introduction](#introduction)
    * [Lexeme and Token](#lexeme-and-token)
    * [Regular language](#regular-language)
* [Implementation](#implementation)
    * [Parser Combinator](#parser-combinator)
* [Resources](#resources)

## Scanning

### Introduction

Scanning hay còn được gọi là lexing hoặc lexical analysis. Một scanner (hoặc lexer) nhận một string và chia nhỏ nó thành một chuỗi của token. Tokens có thể coi như "từ" và dấu câu, dùng để tạo nên ngữ pháp của một ngôn ngữ. Token có thể chỉ gồm một ký tự: `{`, `;` hoặc nhiều ký tự như number `123`, string ("hi!").

Trong source file có nhiều kí tự không mang nhiều ý nghĩa như whitespace hoặc comment. Scanner thông thường sẽ bỏ qua những ký tự này.

![token example](https://i.imgur.com/ETU1ZVC.png)

Fun fact: **Lexical** comes from the Greek root **lex**, meaning **word**

### Error Handling

Scanner còn có nhiệm vụ report lỗi cho người dùng. Thông thường vấn đề này hay bị bỏ qua trên lý thuyết nhưng rất quan trọng cho người dùng trên thực tế.


### Lexeme and Token

Nhiệm vụ của việc scanning là duyệt string input đầu vào và nhóm những ký tự mà đại diện cho một cái gì đó. Mỗi một nhóm ký tự này được gọi là một `lexeme`. Lexeme tương ứng với các raw string trong source code. Trong quá trình scanning, nhóm các ký tự thành lexeme, thì scanner còn thu thập thêm các thông tin hữu ích khác. Chúng ta gộp lexeme và những thông tin đó lại thành một Token.

`Token = lexem + additional information`

Các thông tin đó có thể bao gồm:

- Token type: Khi scanner nhận ra một lexeme thì nó sẽ lưu lại kiểu của lexeme đó luôn. Chúng ta có một số loại token như sau: Keyword, operator, literal (number/string).
- Location information: Việc lưu lại vị trí của mỗi lexeme trong mỗi token sẽ giúp chúng ta có khả năng hiển thị cho người dùng vị trí của lỗi nếu xảy ra.

### Regular language

Mỗi ngôn ngữ lập trình đều có một bộ quy tắc (set of rule) riêng để xác định một chuỗi ký tự có phải là một lexeme hợp lệ hay không? Bộ quy tắc đó được gọi là lexical grammar. Lox language cũng như đa phần các ngôn ngữ lập trình khác, chúng ta có thể dùng regular expression để nhận ra tất cả các lexeme hợp lệ. Các ngôn ngữ này được gọi là regular language. Một số tool như Lex hoặc Flex có thể generate scanner bằng regular expression.

## Implementation


Đứng trước bài toán scanning thông thường chúng ta có những lựa chọn như sau:

* Parser có sẵn: dành cho các format phổ biến như Json, xml.
* Dùng `String.split`: Thường chỉ được dùng cho những trường hợp đơn giản.
* `Regular expression`: Được sử dụng khi không cần kết quả chính xác 100%. Thông thường các parser sử dụng regex thì không ổn định, hay bị lỗi (cả false positives và false negatives)
* Parser generator: Một số tool như [Yacc](https://en.wikipedia.org/wiki/Yacc) hay [ANTLR](https://en.wikipedia.org/wiki/ANTLR) có thể generate source code của parser dựa vào lexical grammar. Cái này được sử dụng rộng rãi nhưng việc set up khá là phiền toái.
* Recursive descent parser (top down parser) style: parser kiểu này sẽ duyệt từ string input từ đầu đến cuối, cố gắng đoán xem chuỗi ký tự hiện tại có phải token hay không? Nếu đúng thì chuỗi ký tự đó sẽ được thêm vào token list sau rồi tiếp tục quá trình đó đến khi gặp EOF.


```Java
  List<Token> scanTokens() {
    while (!isAtEnd()) {
      // We are at the beginning of the next lexeme.
      start = current;
      scanToken();
    }

    tokens.add(new Token(EOF, "", null, line));
    return tokens;
  }

  private void scanToken() {
    char c = advance(); get the next character
    switch (c) {
      case '(': addToken(LEFT_PAREN); break;
      case ')': addToken(RIGHT_PAREN); break;
      case '!':
        addToken(match('=') ? BANG_EQUAL : BANG);
        break;
      case '/':
        if (match('/')) {
          // A comment goes until the end of the line.
          while (peek() != '\n' && !isAtEnd()) advance();
        } else {
          addToken(SLASH);
        }
        break;
    }
  }
```

### Parser combinator

Lựa chọn tối ưu dành cho FP.

## Resources

- [Scanning chapter](http://craftinginterpreters.com/scanning.html)
- [fastparser](https://www.lihaoyi.com/post/EasyParsingwithParserCombinators.html)
