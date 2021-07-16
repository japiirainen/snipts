package api.modules

import api.effects.GenUUID
import api.services._
import cats.effect._
import dev.profunktor.redis4cats.RedisCommands
import skunk.Session

object Services {
  def make[F[_]: GenUUID: Temporal](
      redis: RedisCommands[F, String, String],
      postgres: Resource[F, Session[F]]
  ): Services[F] = {
    new Services[F](
      healthCheck = HealthCheck.make[F](postgres, redis)
    ) {}
  }
}

sealed abstract class Services[F[_]] private (
    val healthCheck: HealthCheck[F]
)
