package api.services

import scala.concurrent.duration._

import api.domain.healthcheck._

import cats.effect._
import cats.effect.implicits._
import cats.syntax.all._
import dev.profunktor.redis4cats.RedisCommands
import skunk._
import skunk.codec.all._
import skunk.implicits._

trait HealthCheck[F[_]] {
  def status: F[AppStatus]
}

object HealthCheck {
  def make[F[_]: Temporal](
      postgres: Resource[F, Session[F]],
      redis: RedisCommands[F, String, String]
                          ): HealthCheck[F] =
    new HealthCheck[F] {
      val q: Query[Void, Int] =
        sql"select pid FROM pg_stat_activity".query(int4)
    }
}
