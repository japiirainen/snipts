package api

import cats.effect._
import cats.effect.std.Console

object Main extends IOApp.Simple {
  val console = Console.make[IO]
  override def run: IO[Unit] =
    for {
      _ <- console.println("hello from Main.scala")
    } yield ()
}
