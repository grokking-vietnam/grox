package grox

import cats.effect.implicits.*
import cats.effect.kernel.{Resource, Sync}
import cats.effect.std.Console
import cats.syntax.all.*
import cats.{Applicative, MonadThrow}

import scribe.Scribe
import fs2.Stream
import fs2.Pull

trait Executor[F[_]]:
  def scan(str: String): F[List[Token[Span]]]
  def parse(str: String): F[Expr]
  def evaluate(str: String): F[LiteralType]
  def execute(str: String): Stream[F, LiteralType]

object Executor:

  def instance[F[_]: MonadThrow: Scribe](
    using scanner: Scanner[F],
    parser: Parser[F],
    interpreter: Interpreter[F],
    executor: StmtExecutor[F],
  ): Executor[F] =
    new Executor[F]:
      def scan(str: String): F[List[Token[Span]]] = scanner.scan(str)

      def parse(str: String): F[Expr] =
        for
          tokens <- scanner.scan(str)
          _ <- Scribe[F].info(s"Tokens $tokens")
          expr <- parser.parseExpr(tokens)
        yield expr

      def evaluate(str: String): F[LiteralType] =
        for
          tokens <- scanner.scan(str)
          _ <- Scribe[F].info(s"Tokens $tokens")
          expr <- parser.parseExpr(tokens)
          _ <- Scribe[F].info(s"Expr $expr")
          result <- interpreter.evaluate(State(), expr)
        yield result

      def execute(str: String): Stream[F, LiteralType] =
        val stmts = for
          tokens <- scanner.scan(str)
          stmts <- parser.parse(tokens)
        yield stmts
        Stream.eval(stmts).flatMap(xs => executor.execute(xs))

  def module[F[_]: MonadThrow: Sync: Scribe]: Resource[F, Executor[F]] =
    given Scanner[F] = Scanner.instance[F]
    given Parser[F] = Parser.instance[F]
    given Interpreter[F] = Interpreter.instance[F]
    for
      given Env[F] <- Env.instance[F](State()).toResource
      given StmtExecutor[F] = StmtExecutor.instance[F]
    yield instance[F]
