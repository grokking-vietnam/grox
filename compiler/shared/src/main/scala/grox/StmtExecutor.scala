package grox

import cats.effect.kernel.Ref
import cats.effect.std.Console
import cats.syntax.all.*
import cats.{Monad, MonadThrow}

trait StmtExecutor[F[_]]:
  def execute(stmts: List[Stmt]): F[Unit]
  def execute(stmt: Stmt): F[LiteralType]

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
              value <-
                if result.isTruthy then execute(thenBranch)
                else elseBranch.fold(Monad[F].unit.widen)(eb => execute(eb))
            yield value

          case Function(name, params, body) => ???

      def execute(stmts: List[Stmt]): F[Unit] = stmts.traverse_(execute)
