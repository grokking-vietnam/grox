package grox

import cats.*

object Function:
    def instance[F[_]: MonadThrow](name: Token[Span], params: List[Token[Span]], body: List[Stmt]): Callable[F] =
        new Callable[F]:
            def arity(): Int = ???
            def call(stmtExecutor: StmtExecutor, args: List[LiteralType]): LiteralType = ???


        