package grox

import cats.effect.kernel.Ref
import cats.effect.std.Console
import cats.syntax.all.*
import cats.{Monad, MonadThrow}

trait StmtExecutor[F[_]]:
  def execute(stmt: List[Stmt]): F[Unit]

object StmtExecutor:
  import Stmt.*

  extension (value: LiteralType)

    def isTruthy: Boolean =
      value match
        case _: Unit    => false
        case v: Boolean => v
        case _          => true

  def instance[F[_]: MonadThrow: Console](
    using env: Env[F],
    interpreter: Interpreter[F],
  ): StmtExecutor[F] =
    new StmtExecutor:

      private def executeStmt(stmt: Stmt): F[Unit] =
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
              _ = interpreter.evaluate(state, expr)
            yield ()

          case Print(expr) =>
            // println(s"EXECUTING PRINT $expr")
            for
              state <- env.state
              // _ = println(s"ENV PRINT $expr")
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
            val c =
              for
                state <- env.state
                r <- interpreter.evaluate(state, cond)
              yield r.isTruthy
            val b = executeStmt(body)
            Monad[F].whileM_(c)(b)

          case If(cond, thenBranch, elseBranch) =>
            for
              state <- env.state
              result <- interpreter.evaluate(state, cond)
              _ <-
                if result.isTruthy then executeStmt(thenBranch)
                else elseBranch.fold(Monad[F].unit)(eb => executeStmt(eb))
            yield ()

          case Function(name, params, body) => ???

      def execute(stmts: List[Stmt]): F[Unit] = stmts.traverse_(executeStmt)
