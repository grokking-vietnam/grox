package grox

import cats.effect.kernel.Ref
import cats.effect.std.Console
import cats.syntax.all.*
import cats.{Monad, MonadThrow}
import fs2.Stream

type Output = LiteralType
type StmtOutput[F[_]] = F[(LiteralType, Stream[F, Output])]

object StmtOutput:
  def empty[F[_]]() = ((), Stream.empty[F])

trait StmtExecutor1[F[_]]:
  def execute(stmts: List[Stmt]): F[Unit]
  def execute(stmt: Stmt): F[LiteralType]

trait StmtExecutor[F[_]]:
  def execute(stmts: List[Stmt]): F[Unit]
  def execute(stmt: Stmt): F[LiteralType]

  def execute1(stmt: Stmt): StmtOutput[F]
  def execute1(stmts: List[Stmt]): F[Stream[F, Output]]

object StmtExecutor:
  import Stmt.*
  import LiteralType.*

  def instance[F[_]: MonadThrow: Console](
    using env: Env[F],
    interpreter: Interpreter[F],
  ): StmtExecutor[F] =
    new StmtExecutor:

      def execute(stmt: Stmt): F[LiteralType] =
        stmt match

          case Block(stmts) =>
            for
              _ <- env.startBlock()
              _ <- execute(stmts)
              _ <- env.endBlock()
            yield ()

          case Expression(expr) =>
            for
              state <- env.state
              result <- interpreter.evaluate(state, expr)
            yield result

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

          case Assign(name, value) =>
            for
              state <- env.state
              result <- interpreter.evaluate(state, value)
              _ <- env.assign(name, result)
            yield ()

          case While(cond, body) =>
            val conditionStmt =
              for
                state <- env.state
                r <- interpreter.evaluate(state, cond)
              yield r.isTruthy
            val bodyStmt = execute(body)
            Monad[F].whileM_(conditionStmt)(bodyStmt).widen

          case If(cond, thenBranch, elseBranch) =>
            for
              state <- env.state
              result <- interpreter.evaluate(state, cond)
              _ <-
                if result.isTruthy then execute(thenBranch)
                else elseBranch.fold(Monad[F].unit.widen)(eb => execute(eb))
            yield ()

          case Function(name, params, body) => ???

      def execute(stmts: List[Stmt]): F[Unit] = stmts.traverse_(execute)

      def execute1(stmt: Stmt): StmtOutput[F] =
        stmt match

          case Block(stmts) =>
            for
              _ <- env.startBlock()
              result <- execute1(stmts)
              _ <- env.endBlock()
            yield ((), result)

          case Expression(expr) =>
            for
              state <- env.state
              result <- interpreter.evaluate(state, expr)
            yield (result, Stream.empty)

          case Print(expr) =>
            for
              state <- env.state
              result <- interpreter.evaluate(state, expr)
            yield (result, Stream.eval(Monad[F].pure(result)))

          case Var(name, init) =>
            for
              state <- env.state
              result <- init.map(interpreter.evaluate(state, _)).sequence
              _ <- env.define(name.lexeme, result.getOrElse(()))
            yield ((), Stream.empty)

          case Assign(name, value) =>
            for
              state <- env.state
              result <- interpreter.evaluate(state, value)
              _ <- env.assign(name, result)
            yield StmtOutput.empty[F]()

          case While(cond, body) =>
            val conditionStmt =
              for
                state <- env.state
                r <- interpreter.evaluate(state, cond)
              yield r.isTruthy
            val bodyStmt: StmtOutput[F] = execute1(body)
            StmtOutput.empty[F]()
            ???
            // Monad[F].whileM(conditionStmt)(bodyStmt)

          case If(cond, thenBranch, elseBranch) =>
            for
              state <- env.state
              result <- interpreter.evaluate(state, cond)
              _ <-
                if result.isTruthy then execute(thenBranch)
                else elseBranch.fold(Monad[F].unit.widen)(eb => execute(eb))
            yield ()
            ???

      def execute1(stmts: List[Stmt]): F[Stream[F, Output]] = ???
