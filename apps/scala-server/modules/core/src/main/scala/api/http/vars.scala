package api.http

import java.util.UUID

import api.domain.link.LinkId
import api.domain.snippet.SnippetId
import cats.implicits._

object vars {
  protected class UUIDVar[A](f: UUID => A) {
    def unapply(str: String): Option[A] =
      Either.catchNonFatal(f(UUID.fromString(str))).toOption
  }

  object SnippetIdVar extends UUIDVar(SnippetId.apply)
  object LinkIdVar    extends UUIDVar(LinkId.apply)
}
