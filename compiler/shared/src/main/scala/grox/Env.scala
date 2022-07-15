package grox

import cats.syntax.all.*
import cats.MonadThrow
import cats.effect.kernel.Ref

// todo move Enviroment => State
trait Env[F[_]]:
  def define(name: String, value: LiteralType): F[Environment]
  def get(name: String): F[LiteralType]
  def assign(name: String, value: LiteralType): F[Environment]

object Env:

  def instance[F[_]: MonadThrow: Ref.Make]: F[Env[F]] = Ref[F].of(Environment()).map { ref =>
    new Env {
      def define(name: String, value: LiteralType): F[Environment] = ???
      def get(name: String): F[LiteralType] = ???
      def assign(name: String, value: LiteralType): F[Environment] = ???
    }
  }
