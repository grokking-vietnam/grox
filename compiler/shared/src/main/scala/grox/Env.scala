package grox

import cats.MonadThrow
import cats.effect.kernel.Ref
import cats.effect.std.Console
import cats.syntax.all.*

// todo move Environment => State
//
trait Env[F[_]]:
  def define(name: String, value: LiteralType): F[Unit]
  def assign(name: String, value: LiteralType): F[Unit]
  def get(name: String): F[LiteralType]
  def state: F[Environment]
  def startBlock(): F[Unit]
  def endBlock(): F[Unit]

object Env:

  def instance[F[_]: MonadThrow: Ref.Make](s: Environment): F[Env[F]] = Ref[F].of(s).map { ref =>
    new Env {
      def define(name: String, value: LiteralType): F[Unit] = ref.update(s => s.define(name, value))
      def assign(name: String, value: LiteralType): F[Unit] =
        for
          s <- ref.get
          ss <- s.assign(name, value).liftTo[F]
          _ <- ref.set(ss)
        yield ()
      def get(name: String): F[LiteralType] = ref.get.map(_.get(name).liftTo[F]).flatten
      def state: F[Environment] = ref.get
      def startBlock(): F[Unit] = ref.update(s => Environment(Map.empty, Some(s)))
      def endBlock(): F[Unit] = ref.update(_.enclosing.getOrElse(Environment()))
    }
  }

// Todo rename => Interpreter
// rename Interpreter => Expr evaluator or something
trait StmtExecutor[F[_]]:
  def execute[A](stmt: List[Stmt[A]]): F[Unit]

// Each block need to add new Environment and use the current env as enclosing
// => we have multiple versions of env
object StmtExecutor:
  import Stmt.*

  def instance[F[_]: MonadThrow: Console](
    using env: Env[F],
    interpreter: Interpreter[F],
  ): StmtExecutor[F] =
    new StmtExecutor {

      private def executeStmt[A](stmt: Stmt[A]): F[Unit] =
        stmt match
          case Block(stmts) =>
            for
              _ <- env.startBlock()
              _ <- execute(stmts)
              _ <- env.endBlock()
            yield ()

          case Expression(expr) => ???

          case Print(expr) =>
            for
              state <- env.state
              result <- interpreter.evaluate(state, expr)
              _ <- Console[F].println(result)
            yield ()

          case Var(name, init) =>
            for
              state <- env.state
              result <- init.map(interpreter.evaluate(state, _)).sequence
              _ <- env.define(name.lexeme, result.getOrElse(()))
            yield ()

          case While(cond, body) => ???
          case Assign(name, value) =>
            for
              state <- env.state
              result <- interpreter.evaluate(state, value)
              _ <- env.assign(name, result)
            yield ()

      def execute[A](stmts: List[Stmt[A]]): F[Unit] = stmts.traverse_(x => executeStmt(x))
    }
