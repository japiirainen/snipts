package api.domain

import java.util.UUID

import scala.util.control.NoStackTrace

import api.ext.http4s.queryParam
import api.ext.http4s.refined._
import api.optics.uuid
import derevo.cats._
import derevo.circe.magnolia.{ decoder, encoder }
import derevo.derive
import eu.timepit.refined.auto._
import eu.timepit.refined.cats._
import eu.timepit.refined.types.string.NonEmptyString
import io.circe.refined._
import io.circe.{ Decoder, Encoder }
import io.estatico.newtype.macros.newtype

object language {
  @derive(decoder, encoder, eqv, show, uuid)
  @newtype
  case class LanguageId(value: UUID)

  @derive(decoder, encoder, eqv, show)
  @newtype
  case class LanguageName(value: String) {
    def toLanguage(languageId: LanguageId): Language =
      Language(languageId, this)
  }

  @derive(decoder, encoder, eqv, show)
  case class Language(uuid: LanguageId, name: LanguageName)

  @derive(queryParam, show)
  @newtype
  case class LanguageParam(value: NonEmptyString) {
    def toDomain: LanguageName = LanguageName(value.toLowerCase.capitalize)
  }

  object LanguageParam {
    implicit val jsonEncoder: Encoder[LanguageParam] =
      Encoder.forProduct1("name")(_.value)

    implicit val jsonDecoder: Decoder[LanguageParam] =
      Decoder.forProduct1("name")(LanguageParam.apply)
  }

  @derive(decoder, encoder)
  case class InvalidLanguage(value: String) extends NoStackTrace
}
