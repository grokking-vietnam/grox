package grox

trait Callable[F[_]]:
    def arity(): Int;
    def call(stmtExecutor: StmtExecutor[F], args: List[LiteralType]): LiteralType;
