Ở phần trước, chúng ta đã thử biến đổi source code từ dạng raw sang một list tokens. Hôm nay, chúng ta sẽ tiếp tục biểu diễn list token này sang 1 dạng phức tạp và đầy đủ hơn.

Đầu tiên, chúng ta sẽ bắt đầu với 1 ví dụ đơn giản, làm thế nào để tính biểu thức sau:

```
1 + 2 * 3 - 4
```

Chúng ta đều biết quy tắc giải bài toán này đó là "nhân chia trước, cộng trừ sau". Trên máy tính, nó có thể được biểu diễn bằng cấu trúc cây. Trong đó, số là các node lá và toán tử là các node trong. 

![](https://craftinginterpreters.com/image/representing-code/tree-evaluate.png)

Để tính được giá trị của biểu thức, chúng ta sẽ phải tính giá trị của các cây con trước. Tức là chúng ta phải thực hiện duyệt cây (tree traversal), trong ví dụ này chúng ta sẽ thực hiện post-order traversal.

Chúng ta thấy là nếu dùng cấu trúc cây để biểu diễn biểu thức trên thì việc tính toán trông khá dễ dàng. Vậy 1 cách trực quan nhất, liệu chúng ta có thể biểu diễn grammar của ngôn ngữ lập trình dưới dạng dạng cây hay không?

## Context-Free Grammars

Ở bài trước, chúng ta cũng đã sử dụng lexical grammar để định nghĩa các tokens. Với Scanner thì cách làm này rất hiệu quả nhưng không đủ để handle các biểu thức lồng nhau rất phức tạp.

Vì vậy, chúng ta sẽ cần đến **context-free grammar (CFG)**.  Công cụ mạnh nhất trong các [ngôn ngữ hình thức](https://vi.wikipedia.org/wiki/Ng%C3%B4n_ng%E1%BB%AF_h%C3%ACnh_th%E1%BB%A9c)

> Một **ngôn ngữ hình thức** (_formal language_) được định nghĩa là một tập các chuỗi (_string_) được xây dựng dựa trên một [bảng chữ cái](https://vi.wikipedia.org/wiki/B%E1%BA%A3ng_ch%E1%BB%AF_c%C3%A1i "Bảng chữ cái") (_alphabet_), và chúng được ràng buộc bởi các [luật](https://vi.wikipedia.org/wiki/Lu%E1%BA%ADt "Luật") (_rule_) hoặc [văn phạm](https://vi.wikipedia.org/wiki/V%C4%83n_ph%E1%BA%A1m "Văn phạm") (_grammar_) đã được định nghĩa trước.
> 
> Wikipedia


Bảng chữ cái ở đây có thể lấy từ ngôn ngữ tự nhiên hoặc tự định nghĩa

| Terminology                   |    | Lexical grammar | Syntactic grammar |
|-------------------------------|----|-----------------|-------------------|
| The “alphabet” is . . .       | →  | Characters      | Tokens            |
| A “string” is . . .           | →  | Lexeme or token | Expression        |
| It’s implemented by the . . . | →  | Scanner         | Parser            |

## Rules for grammars

Khi chúng ta đã định nghĩa được các quy tắc cho ngôn ngữ của mìnhh thì chúng ta có thể dùng rule để tạo ra các strings (hay ở đây chính là viết code). Rules trong trường hợp này gọi là **productions** và chúng _produce_ ra các strings.

Mỗi production trong CFG có phần đầu (head) - tên của nó và phần thân (body) mô tả nó sẽ generate ra cái gì. Ở dạng cơ bản nhất thì, phần thân chỉ bao gồm một danh sách các kí hiệu (Symbols). Các kí hiệu có 2 kiểu:

- **Terminal** là một chữ cái trong bảng chữ cái. Chúng ta có thể nghĩ nó như là một literal. Trong syntactic grammar nó là các token chúng ta đã có được từ Scanner.
- **Nonterminal** dùng để chỉ đến 1 rule trong gammar của chúng ta.

Chỉ còn 1 điều cuối cùng cần phải lưu ý:  Chúng ta có thể có nhiều rules có cùng tên. Khi chúng ta gặp một nonterminal với 1 tên nào đó chúng ta được phép chọn 1 rule bất kì cho nó.

Để biểu diễn được 1 cách cụ thể hơn, chúng ta cần một cách để viết ra các production rules này. Người ta đã tìm cách thực hiện điều này từ hàng ngàn năm về trước và cho đến khi John Backus và công ty của ông cần nó cho ngôn ngữ ALGOL 58 và cuối cùng tạo ra **Backus-Naur form (BNF)**. Và kể từ đó, hầu hết mọi người để sử dụng BNF (hoặc biến đổi của BNF).

```
breakfast  → protein "with" breakfast "on the side" ;
breakfast  → protein ;
breakfast  → bread ;

protein    → crispiness "crispy" "bacon" ;
protein    → "sausage" ;
protein    → cooked "eggs" ;

crispiness → "really" ;
crispiness → "really" crispiness ;

cooked     → "scrambled" ;
cooked     → "poached" ;
cooked     → "fried" ;

bread      → "toast" ;
bread      → "biscuits" ;
bread      → "English muffin" ;
```

Từ đó chúng ta có thể viết ra các strings dựa trên BNF ở trên:

![](https://craftinginterpreters.com/image/representing-code/breakfast.png)

## A Grammar for Lox expressions

Syntatic Grammar phức tạp hơn Lexical Grammar rất là nhiều, vì vậy rất khó để trình bày được toàn bộ SG trong 1 lần giống như cách chúng ta làm ở phần Scanner. Vì vậy, chúng ta sẽ nghiền ngẫm từng tập con của ngôn ngữ và thêm syntax mới trong các chương tiếp theo. Hiện tại, chúng ta sẽ chỉ quan tâm đến 1 expression:

```
expression     → literal
               | unary
               | binary
               | grouping ;

literal        → NUMBER | STRING | "true" | "false" | "nil" ;
grouping       → "(" expression ")" ;
unary          → ( "-" | "!" ) expression ;
binary         → expression operator expression ;
operator       → "==" | "!=" | "<" | "<=" | ">" | ">="
               | "+"  | "-"  | "*" | "/" ;
```

Trên đây là syntax để chúng ta có thể biểu diễn được 1 expression giống như sau:

```scala
1 - (2 * 3) < 4 == false
```

## Implementing Syntax Trees

Dễ thấy, grammar của chúng ta có thể được biểu diễn bằng cấu trúc cây. Bởi vì cấu trúc này được dùng để biểu diễn syntax cho ngôn ngữ grox nên nó được gọi là **syntax tree.**

Với scala 3 chúng ta có thể biểu diễn một binary như sau:

```scala
enum Expr:
  case Binary(left: Expr, operator: Token, right: Expr)
```