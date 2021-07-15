package api.domain

import java.util.UUID

import api.domain.category._
import api.domain.language._
import api.optics.uuid
import derevo.cats._
import derevo.circe.magnolia._
import derevo.derive
import eu.timepit.refined.api.Refined
import eu.timepit.refined.auto._
import eu.timepit.refined.cats._
import eu.timepit.refined.string.{ Uuid }
import eu.timepit.refined.types.string.NonEmptyString
import io.circe.refined._
import io.estatico.newtype.macros.newtype

object snippet {
  @derive(decoder, encoder, keyDecoder, keyEncoder, eqv, show, uuid)
  @newtype
  case class SnippetId(value: UUID)

  @derive(decoder, encoder, eqv, show)
  @newtype
  case class SnippetName(value: String)

  @derive(decoder, encoder, eqv, show)
  @newtype
  case class SnippetDescription(value: String)

  @derive(decoder, encoder, eqv, show)
  @newtype
  case class SnippetContent(value: String)

  @derive(decoder, encoder, eqv, show)
  case class Snippet(
      uuid: SnippetId,
      name: SnippetName,
      description: SnippetDescription,
      content: SnippetContent,
      language: Language,
      category: Category
  )

  // ---- create snippet ----

  @derive(decoder, encoder, show)
  @newtype
  case class SnippetNameParam(value: NonEmptyString)

  @derive(decoder, encoder, show)
  @newtype
  case class SnippetDescriptionParam(value: NonEmptyString)

  @derive(decoder, encoder, show)
  @newtype
  case class SnippetContentParam(value: NonEmptyString)

  case class CreateSnippetParam(
      name: SnippetNameParam,
      description: SnippetDescriptionParam,
      content: SnippetContentParam,
      languageId: LanguageId,
      categoryId: CategoryId
  ) {
    def toDomain: CreateSnippet =
      CreateSnippet(
        SnippetName(name.value),
        SnippetDescription(description.value),
        SnippetContent(content.value),
        languageId,
        categoryId
      )
  }

  case class CreateSnippet(
      name: SnippetName,
      description: SnippetDescription,
      content: SnippetContent,
      languageId: LanguageId,
      categoryId: CategoryId
  )

  // ---- update item ----

  @derive(decoder, encoder)
  @newtype
  case class SnippetIdParam(value: String Refined Uuid)

  @derive(decoder, encoder)
  case class UpdateSnippetParam(
      id: SnippetIdParam,
      content: SnippetContentParam
  ) {
    def toDomain: UpdateSnippet =
      UpdateSnippet(
        SnippetId(UUID.fromString(id.value)),
        SnippetContent(content.value)
      )
  }

  @derive(decoder, encoder)
  case class UpdateSnippet(
      id: SnippetId,
      content: SnippetContent
  )
}
