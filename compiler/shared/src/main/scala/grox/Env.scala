package grox

import scala.util.control.NoStackTrace
import cats.MonadThrow
import cats.effect.kernel.Ref
import cats.syntax.all.*

case class EnvError(msg: String) extends NoStackTrace

trait Env[F[_]]:
  def define(name: String, value: LiteralType): F[Unit]
  def assign(name: String, value: LiteralType): F[Unit]
  def get(name: String): F[LiteralType]
  def state: F[State]
  def startBlock(): F[Unit]
  def endBlock(): F[Unit]

object Env:

  def instance[F[_]: MonadThrow: Ref.Make](s: State): F[Env[F]] = Ref[F].of(s).map { ref =>
    new:
      // we allowed redeclaration of variables
      def define(name: String, value: LiteralType): F[Unit] = ref.update(s => s.define(name, value))
      def assign(name: String, value: LiteralType): F[Unit] =
        for
          (s, set) <- ref.access
          ss <- s.assign(name, value).liftTo[F]
          success <- set(ss)
          _ <-
            if success then MonadThrow[F].unit
            else MonadThrow[F].raiseError(EnvError(s"Cannot update program state."))
        yield ()
      def get(name: String): F[LiteralType] = ref.get.map(_.get(name).liftTo[F]).flatten
      def state: F[State] = ref.get
      def startBlock(): F[Unit] = ref.update(s => State(Map.empty, Some(s)))
      def endBlock(): F[Unit] = ref.update(_.enclosing.getOrElse(State()))
  }
