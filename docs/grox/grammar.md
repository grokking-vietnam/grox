# Grox Grammar

# Expression
Grox hỗ trợ expression cho các kiểu dữ liệu:
- Boolean
- Double 

> **FYI**
> Expression grammar của Grox có sự khác biệt với [Lox](http://craftinginterpreters.com/representing-code.html#a-grammar-for-lox-expressions). Lox là ngôn ngữ dynamic-typing và compiler của Lox cho phép nhiều kiểu dữ liệu xuất hiện trong 1 expression, và lỗi chỉ xuất hiện tại runtime khi quá trình evaluate diễn ra. Ví dụ expression `false + true * "abcde"` được compile thành công nhưng sẽ gây lỗi khi chạy chương trình.
> 
> Mặt khác, Grox hướng đến static-typing, đồng nghĩa với việc compiler biết kiểu dữ liệu của expression trong quá trình compile, đồng thời đảm bảo expression có giá trị nếu được compile thành công.

**Expression grammar**
```
expr                -> bool_expr | double_expr | grouping
bool_expr           -> "true" | "false" 
                    | not
                    | logic
                    | comparison                       
not                 -> "!" bool_expr
logic               -> bool_expr ("and" | "or") bool_expr 
comparison          -> expr "<" "<=" "==" ">=" ">" expr  

double_expr         -> Double 
                    | negate 
                    | binary                    
negate              -> "-" double_expr
binary              -> double_expr ("+" | "-" | "*" | "/") double_expr 

grouping            -> "(" expr ")"
```