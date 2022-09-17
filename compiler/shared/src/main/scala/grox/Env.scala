package grox

import cats.MonadThrow
import cats.effect.kernel.Ref
import cats.syntax.all.*

trait Env[F[_]]:
  def define(name: String, value: LiteralType): F[Unit]
  def assign(name: String, value: LiteralType): F[Unit]
  def get(name: String): F[LiteralType]
  def state: F[State]
  def startBlock(): F[Unit]
  def endBlock(): F[Unit]

object Env:

  def instance[F[_]: MonadThrow: Ref.Make](s: State): F[Env[F]] = Ref[F].of(s).map { ref =>
    new Env:
      // we allowed redeclaration of variables
      def define(name: String, value: LiteralType): F[Unit] = ref.update(s => s.define(name, value))
      def assign(name: String, value: LiteralType): F[Unit] =
        for
          s <- ref.get
          ss <- s.assign(name, value).liftTo[F]
          _ <- ref.set(ss)
        yield ()
      def get(name: String): F[LiteralType] = ref.get.map(_.get(name).liftTo[F]).flatten
      def state: F[State] = ref.get
      def startBlock(): F[Unit] = ref.update(s => State(Map.empty, Some(s)))
      def endBlock(): F[Unit] = ref.update(_.enclosing.getOrElse(State()))
  }
