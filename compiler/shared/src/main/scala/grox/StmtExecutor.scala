package grox

import cats.effect.kernel.Ref
import cats.effect.std.Console
import cats.syntax.all.*
import cats.{Monad, MonadThrow}
import fs2.Stream
import fs2.Pull

type Output = LiteralType

object StmtOutput:
  def empty[F[_]]() = ((), Stream.empty[F])

trait StmtExecutor[F[_]]:
  def execute(stmt: Stmt): Pull[F, Output, Output]
  def execute(stmts: List[Stmt]): Stream[F, Output]

object StmtExecutor:
  import Stmt.*
  import LiteralType.*

  def instance[F[_]: MonadThrow](
    using env: Env[F],
    interpreter: Interpreter[F],
  ): StmtExecutor[F] =
    new StmtExecutor:

      def execute(stmts: List[Stmt]): Stream[F, Output] =
        executePull(stmts).stream

      def executePull(stmts: List[Stmt]): Pull[F, Output, Unit] =
        stmts.traverse_(execute)

      def execute(stmt: Stmt): Pull[F, Output, Output] =
        stmt match
          case Print(expr) =>
            val o = for
              state <- env.state
              output <- interpreter.evaluate(state, expr)
            yield output
            Pull.eval(o).flatMap(Pull.output1)

          case Expression(expr) =>
            val o = for
              state <- env.state
              result <- interpreter.evaluate(state, expr)
            yield result
            Pull.eval(o)

          case Var(name, init) =>
            val o = for
              state <- env.state
              result <- init.map(interpreter.evaluate(state, _)).sequence
              _ <- env.define(name.lexeme, result.getOrElse(()))
            yield ()
            Pull.eval(o)

          case Assign(name, value) =>
            val o = for
              state <- env.state
              result <- interpreter.evaluate(state, value)
              _ <- env.assign(name, result)
            yield ()
            Pull.eval(o)

          case While(cond, body) =>
            val o =
              for
                state <- env.state
                r <- interpreter.evaluate(state, cond)
              yield r.isTruthy
            val conditionStmt = Pull.eval(o)
            val bodyStmt: Pull[F, Output, Output] = execute(body)
            Monad[Pull[F, Output, *]].whileM_(conditionStmt)(bodyStmt).widen

          case If(cond, thenBranch, elseBranch) =>
            val b = for
              state <- env.state
              result <- interpreter.evaluate(state, cond)
            yield result.isTruthy
            Pull.eval(b).flatMap(x =>
                if(x) then execute(thenBranch)
                else elseBranch.fold(Pull.done)(eb => execute(eb)))

          case Block(stmts) =>
            Pull.bracketCase(Pull.eval(env.startBlock()), (_) => executePull(stmts), (_, _) => Pull.eval(env.endBlock()))

          case Function(name, params, body) => ???
