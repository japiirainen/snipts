package api.domain

import java.util.UUID

import api.domain.snippet._
import api.optics.uuid
import derevo.cats._
import derevo.circe.magnolia.{ decoder, encoder }
import derevo.derive
import eu.timepit.refined.auto._
import eu.timepit.refined.cats._
import eu.timepit.refined.types.string.NonEmptyString
import io.circe.refined._
import io.estatico.newtype.macros.newtype

object link {
  @derive(encoder, decoder, eqv, show, uuid)
  @newtype
  case class LinkId(value: UUID)

  @derive(decoder, encoder, eqv, show)
  @newtype
  case class LinkUrl(value: String)

  @derive(decoder, encoder, eqv, show)
  case class Link(
      uuid: LinkId,
      url: LinkUrl,
      snippetId: SnippetId
  )

  // ---- create link ----

  @derive(decoder, encoder, show)
  @newtype
  case class LinkUrlParam(value: NonEmptyString)

  case class CreateLinkParam(
      url: LinkUrlParam,
      snippetId: SnippetId
  ) {
    def toDomain: CreateLink =
      CreateLink(
        LinkUrl(url.value),
        snippetId
      )
  }

  case class CreateLink(
      url: LinkUrl,
      snippetId: SnippetId
  )
}
