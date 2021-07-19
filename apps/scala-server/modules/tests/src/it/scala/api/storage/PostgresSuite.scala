package api.storage

import api.config.types.PasswordSalt
import api.generators._
import api.services._
import cats.effect._
import cats.implicits._
import eu.timepit.refined.auto._
import eu.timepit.refined.cats._
import natchez.Trace.Implicits.noop
import skunk._
import skunk.implicits._
import suite.ResourceSuite

object PostgresSuite extends ResourceSuite {

  val salt: PasswordSalt = PasswordSalt("123")

  val flushTables: List[Command[Void]] =
    List("users", "languages", "categories", "snippets", "links").map { table =>
      sql"DELETE FROM #$table".command
    }

  type Res = Resource[IO, Session[IO]]

  override def sharedResource: Resource[IO, Res] =
    Session.pooled[IO](
      host = "localhost",
      port = 5432,
      user = "postgres",
      password = Some("my-password"),
      database = "snipts",
      max = 10
    )
      .beforeAll {
        _.use { s =>
          flushTables.traverse_(s.execute)
        }
      }

  test("Users") { postgres =>
    val gen = for {
      u <- userNameGen
      p <- encryptedPasswordGen
    } yield u -> p

    forall(gen) {
      case (username, password) =>
        val u = Users.make[IO](postgres)
        for {
          d <- u.create(username, password)
          x <- u.find(username)
          z <- u.create(username, password).attempt
        } yield expect.all(x.count(_.id === d) === 1, z.isLeft)
    }
  }
}