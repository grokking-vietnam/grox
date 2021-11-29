# Map of the territory

Giới thiệu về các khái niệm căn bản, các giai đoạn thực thi của compiler.

* [The Parts of a Language](#the-parts-of-a-language)
    * [Scanning/Lexing](#scanning)
    * [Parsing](#parsing)
    * [Static analysis](#static-analysis)
    * [Intermediate representations](#intermediate-representation)
    * [Optimization](#optimization)
    * [Code Generation](#code-generation)
    * [Virtual Machine](#virtual-machine)
    * [Runtime](#runtime)
* [Shortcut and alternative routes](#shortcut-and-alternative-routes)
    * [Single-pass compilers](#single-pass-compilers)
    * [Tree-walk interpreters](#tree-walk-interpreters)
    * [Transpiler](#transpiler)
    * [Just-in-time compilation](#just-in-time-compilation)
* [Compilers vs interpreters](#compilers-vs-interpreters)

![The Map](https://i.imgur.com/875BQl5.png)


## The Parts of a Language

### Scanning

Còn được gọi là lexing hoặc lexical analysis. Một scanner (hoặc lexer) nhận một string và chia nhỏ nó thành một chuỗi của token.

Tokens có thể coi như "từ" và dấu câu, được dùng để tạo nên ngữ pháp của một ngôn ngữ.
![ex1](https://i.imgur.com/ETU1ZVC.png)

Fun fact: **Lexical** comes from the Greek root **lex**, meaning **word**

### Parsing

Parsers có đầu vào là một chuỗi token và trả về một cấu trúc dữ liệu kiểu cây thường được gọi là parse tree hoặc abstract syntax tree(AST) hoặc syntax trees. Thêm vào đó parsers còn có nhiệm vụ thông báo các lỗi ngữ pháp (syntax error) cho người dùng.

Đối chiếu với các ngôn ngữ nói thì, scanner sẽ chia nhỏ một đoạn văn thành từng từ, dấu câu; còn parser sẽ dựa vào đó mà phân tích ngữ pháp cả đoạn văn.

![AST](https://i.imgur.com/26k6LOJ.png)

### Static analysis

Dựa vào ASTs từ giai đoạn trước, phần này sẽ làm những công việc sau:

- **binding/resolution**: với mỗi định danh(identifier; vd: tên biến hoặc tên hàm), chúng ta cần tìm nơi nó định nghĩa, nơi nó được sử dụng; phạm vi(scope) hợp lệ của nó.
- **type checking**: nếu một ngôn ngữ thuộc dạng staticallly typed; khi khai báo một biến bất kỳ, chúng ta phải xác định được type của biến đó. Và report type error khi cần thiết.
- Sau khi phân tích xong, chúng ta sẽ lưu lại những thông tin cần thiết để phục vụ cho các giai đoạn tiếp theo. Có 3 cách lưu chính như sau:
    1. Chèn ngay vào ASTs dưới dạng *attributes*
    2. Lưu vào [Symbol Table](https://www.geeksforgeeks.org/symbol-table-compiler/); thông thường dành cho identifiers và giá trị của chúng.
    3. Chuyển hẳn sang một cấu trúc AST mới có nhiều thông tin hơn.

#### Notes

3 giai đoạn phía trên được gọi là phần front end của compiler. Các phần được trình bày tiếp theo được gọi là back end.

### Intermediate Representation

Có thể tưởng tượng compiler như một pipeline, gồm 2 giai đoạn (phase) lớn (front end và back end) và nhiều giai đoạn nhỏ. Output của phase này sẽ là input của phase kia.

Giai đoạn front end tập trung chủ yếu vào phần source code. Giai đoạn back end tập trung vào phần chạy chương trình (final architecture where the program will run). Ở giữa 2 giai đoạn đó thì code được lưu dưới dạng *intermediate representation(IR)*. IR sẽ không gắn chặt với input (vd: source code) hay output (vd: binary code) của compiler. Có thể coi IR như một interface giữa 2 ngôn ngữ đó.

Nhờ vào IR, mà chúng ta có thể support nhiều runtime một cách dễ dàng: dùng chung 1 front end, và mỗi runtime khác nhau sẽ có backend của riêng nó.

Ví dụ như Kotlin, Scala đều có thể chạy trên 3 runtime khác nhau: JVM, Javascript, Native. Hoặc [GCC](https://en.wikipedia.org/wiki/GNU_Compiler_Collection) có thể biên dịch hàng tá ngôn ngữ và có thể target trên dưới 100 architectures khác nhau.

IR có 4 styles chính:

- control flow graph
- static single-assignment
- continuation-passing style
- three-address code

### Optimization

Sau khi có đủ hiểu biết về chương trình đang được compile, thì chúng ta có thể tối ưu nó bằng cách thay chương trình này bằng một chương trình khác có cùng ngữ nghĩa(same semantics - same input same output) nhưng hiệu quả hơn.

Ví dụ đơn giản nhất là **constant folding**, nếu một biểu thức luôn luôn cho ra một kết quả duy nhất; thì chúng ta có thể dùng kết quả nhận được để thay thế cho biểu thức đó trong quá trình compile.

```pennyArea = 3.14159 * (0.75 / 2) * (0.75 / 2);```

Có thể thay thế bằng

```pennyArea = 0.4417860938;```

Optimization là một nhánh lớn trong mảng programming language, nhưng sẽ không được đi sâu vào trong quyển sách này. Sau đây là một số keyword để tìm hiểu thêm về nó: “constant propagation”, “common subexpression elimination”, “loop invariant code motion”, “global value numbering”, “strength reduction”, “scalar replacement of aggregates”, “dead code elimination”, “loop unrolling”.

### Code generation

Bước cuối cùng, sau khi optimization hoàn thành, chuyển IR sang một dạng code mà máy có thể chạy được. Bước này còn gọi là **Code Generation** (hoặc **code gen**); Code ở đây là code dành cho máy (vd: dạng assembly hoặc byte code trên virtual machine), không phải code dành *người* đọc và viết.

Chúng ta có một số lựa chọn cho **code** ở đây:

- CPU instruction: Code có thể chạy cực nhanh, nhưng phức tạp và tốn rất nhiều công để viết.
- Bytecode chạy trên virtual machine (được gọi là bytecode vì mỗi instruction đều có độ dài là một byte): Có thể support nhiều cpu architecture dễ dàng hơn nhưng lại phải maintain virtual machine và tốc độ có thể không nhanh bằng cách trước. (Chúng ta sẽ chọn phương án này)
- Cũng là bytecode nhưng thay vì dùng virtual machine, chúng ta sẽ viết một mini-compiler cho mỗi một architecture để chuyển bytecode sang native code. Hay nói cách khác là chúng ta sử dụng bytecode như một IR.

### Virtual machine

**Virtual machine** (hoặc chính xác hơn là language virtual machines, gọi tắt là VM) là một chương trình có chạy một dạng byte code ở runtime. Code chạy trên VM sẽ chậm hơn trên native code nhưng công việc của compiler sẽ đơn giản hơn. Trong phần 2 của quyển sách này thì cũng ta sẽ implement một VM bằng C và nó sẽ có thể chạy trên tất cả các platfrom hỗ trợ C compiler.

### Runtime

Ở đây chúng ta đã có một chương trình mà người dùng có thể sử dụng. Bước cuối cùng chạy chương trình đó. Nếu chúng ta compiled thành native code, thì operating system sẽ tải chương trình và chạy nó, nếu compiled thành bytecode thì chúng ta cần khởi động VM rồi tải chương trình trên cái VM đó.

Trong cả 2 trường hợp đó chúng ta vẫn cần phải một số services chạy ngầm để nhứ:
- Quản lý memory/gabarge collector
- Nếu language hỗ trợ runtim checking (vd: `instance of`), chúng ta cần nắm được type của từng object trong cả quá trình thực thi.

Tất cả những thứ đó được thực hiện trong lúc chạy chương trình nên được gọi là runtime.

Ở những ngôn ngữ được compile hoàn toàn thành native code, thì runtime được đính kèm luôn vào chương trình (vd như Golang).

Nếu một ngôn ngữ được chạy trong một interpreter hoặc VM thì runtime là một phần của interpreter/VM. Đa phần các ngôn ngữ được implement dưới dạng này: Java, Python, Javascript.

## Shortcut and alternative routes

Phần trên đã mô tả tất cả các bước mà chúng ta có thể phải implement. Rất nhiều ngôn ngữ được viết với đầy đủ các bước đó, nhưng cũng có một số con đường tắt và thay thế.

### Single-pass compilers

Có một số compiler gộp parsing, analysis và code generation lại làm một, thành ra output code được trực tiếp tạo ra từ parser. Chúng không sử dụng ASTs cũng như IRs trong quá trình biên dịch. Chúng được gọi là **single-pass compilers**.

Những **single-pass compilers** này sẽ có nhiều giới hạn:
- Không có một cấu trúc dữ liệu trung gian để lưu lại global information
- Không thể quay lại các phần trước của chương trình. Nghĩa là khi compiler thấy một biểu thức nào đó thì nó phải có khả năng biên dịch biểu thức đó ngay.

Pascal và C có chung những cái điểm yếu này, đó là lý do tại sao Pascal yêu cầu phải khai báo kiểu của biến ở đầu file; cũng như với C ta không thể gọi một function trước khi nó được khai báo (trừ khi ta khai báo function đó với forward declaration).

### Tree-walk interpreters

Là những ngôn ngữ được thực thi sau khi có AST (có thể có thêm một ít static analysis). Để thực thi chương trình, intepreter duyệt từng node của AST, thay thế dần dần các node bằng kết quả tính được đến tận cùng. Đây cũng là cách chúng ta implement interpreter đầu tiên.

Notes: Những phiên bản đầu tiên của Ruby được implement dưới dạng này.

### Transpiler

Khá là phổ biến hiện nay, đặc biệt với javascript là ngôn ngữ mục tiêu. Transpiler là một compiler biên dịch code từ ngôn ngữ này sang một ngôn ngữ khác, thường từ bậc cao (higher level) sang bậc thấp hơn (lower level).

Phần front end của những compiler này thì giống hệt các compiler khác. Sau đó tuỳ thuộc vào sự khác nhau giữa hai ngôn ngữ, implementation của back end có thể rất khác nhau. Từ đơn giản chuyển thẳng từ AST sang target language hoặc bao gồm tất cả các bước kể trên.

### Just-in-time compilation

Cách nhanh nhất để thực thi code đó là biên dịch nó thành native code. Nhưng khi biên dịch thì chúng ta không biết được nó sẽ được chạy với kiểu architecture nào. Just-in-time (JIT) là một kỹ thuật cao cấp được để tối ưu hoá tốc độc của chương trình trong trường hợp này. JIT được sự dụng bởi Hotspot Java Virtual Machine (JVM), Microsoft's Common Language Runtime (CLR) và đa số các trình biên dịch của Javascript.

Khi chương trình được tải lên - từ source trong trường hợp của Javascript hay từ bytecode như của JVM/CLR - thì JIT sẽ compile nó thành native code ngay lúc đó.

## Compilers vs Interpreters

- Compiling (biên dịch) là quá trình chuyển đổi từ một ngôn ngữ lập trình sang một dạng mới: bytecode hoặc native code hoặc một ngôn ngữ khác.
- Compiler thì chỉ có nhiệm vụ biên dịch
- Intepreter thì có khả năng thực thi chường trình từ source code.

![venn](https://i.imgur.com/LLvek2X.png)
