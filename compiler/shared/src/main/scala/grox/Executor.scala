package grox

import cats.effect.implicits.*
import cats.effect.kernel.{Resource, Sync}
import cats.effect.std.Console
import cats.syntax.all.*
import cats.{Applicative, MonadThrow}

trait Executor[F[_]]:
  def scan(str: String): F[List[Token[Span]]]
  def parse(str: String): F[Expr]
  def evaluate(str: String): F[LiteralType]
  def execute(str: String): F[Unit]

// todo add StmtExecutor
object Executor:

  def instance[F[_]: MonadThrow](
    using scanner: Scanner[F],
    parser: Parser[F],
    interpreter: Interpreter[F],
    executor: StmtExecutor[F],
  ): Executor[F] =
    new Executor[F]:
      val env = Environment()
      def scan(str: String): F[List[Token[Span]]] = scanner.scan(str)

      def parse(str: String): F[Expr] =
        for
          tokens <- scanner.scan(str)
          expr <- parser.parseExpr(tokens)
        yield expr

      def evaluate(str: String): F[LiteralType] =
        for
          tokens <- scanner.scan(str)
          expr <- parser.parseExpr(tokens)
          result <- interpreter.evaluate(env, expr)
        yield result

      def execute(str: String): F[Unit] =
        for
          tokens <- scanner.scan(str)
          _ = println(s"TOKENS: $tokens")
          stmts <- parser.parse(tokens)
          _ <- executor.execute(stmts)
        yield ()

  def module[F[_]: MonadThrow: Sync: Console]: Resource[F, Executor[F]] =

    val unit = Applicative[F].unit.toResource
    given Scanner[F] = Scanner.instance[F]
    given Parser[F] = Parser.instance[F]
    given Interpreter[F] = Interpreter.instance[F]
    for
      given Env[F] <- Env.instance[F](Environment()).toResource
      _ <- unit
      given StmtExecutor[F] = StmtExecutor.instance[F]
    yield instance[F]
