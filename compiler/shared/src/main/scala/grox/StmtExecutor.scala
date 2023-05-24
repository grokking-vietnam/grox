package grox

import cats.effect.kernel.Ref
import cats.syntax.all.*
import cats.{Monad, MonadThrow}

import fs2.{Pull, Stream}

type Output = LiteralType

trait StmtExecutor[F[_]]:
  def execute(stmt: Stmt): Pull[F, Output, Output]
  def execute(stmts: List[Stmt]): Stream[F, Output]

object StmtExecutor:
  import Stmt.*
  import LiteralType.*

  def instance[F[_]: MonadThrow](
    using env: Env[F],
    interpreter: Interpreter[F],
  ): StmtExecutor[F] = new:

    def executePull(stmts: List[Stmt]): Pull[F, Output, Unit] = stmts.traverse_(execute)
    def execute(stmts: List[Stmt]): Stream[F, Output] = executePull(stmts).stream

    def execute(stmt: Stmt): Pull[F, Output, Output] = stmt match

      case Expression(expr) =>
        val result = interpreter.evaluate(expr)
        Pull.eval(result)

      case Print(expr) =>
        val output = interpreter.evaluate(expr)
        Pull.eval(output).flatMap(Pull.output1)

      case Var(name, init) =>
        val output =
          for
            result <- init.map(interpreter.evaluate(_)).sequence
            _ <- env.define(name.lexeme, result.getOrElse(()))
          yield ()
        Pull.eval(output)

      case Assign(name, value) =>
        val output =
          for
            result <- interpreter.evaluate(value)
            _ <- env.assign(name, result)
          yield ()
        Pull.eval(output)

      case While(cond, body) =>
        val output = interpreter.evaluate(cond).map(_.isTruthy)
        val conditionStmt = Pull.eval(output)
        val bodyStmt = execute(body)
        Monad[Pull[F, Output, *]].whileM_(conditionStmt)(bodyStmt).widen

      case If(cond, thenBranch, elseBranch) =>
        val condOuput = interpreter.evaluate(cond).map(_.isTruthy)
        Pull
          .eval(condOuput)
          .flatMap(x =>
            if x then execute(thenBranch)
            else elseBranch.fold(Pull.done)(eb => execute(eb))
          )

      case Block(stmts) => Pull.bracketCase(
          Pull.eval(env.startBlock()),
          _ => executePull(stmts),
          (_, _) => Pull.eval(env.endBlock()),
        )
