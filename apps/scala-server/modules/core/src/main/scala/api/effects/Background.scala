package api.effects

import scala.concurrent.duration.FiniteDuration

import cats.effect._
import cats.effect.std.Supervisor
import cats.syntax.all._

trait Background[F[_]] {
  def schedule[A](fa: F[A], duration: FiniteDuration): F[Unit]
}

object Background {
  implicit def bgInstance[F[_]: Background](implicit S: Supervisor[F], T: Temporal[F]): Background[F] =
    new Background[F] {
      override def schedule[A](fa: F[A], duration: FiniteDuration): F[Unit] =
        S.supervise(T.sleep(duration) *> fa).void
    }
}
