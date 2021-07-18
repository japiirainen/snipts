package api

import java.util.UUID

import api.domain.auth._
import api.domain.language._
import api.domain.category._
import api.domain.snippet._
import api.domain.link._
import api.http.auth.users._

import eu.timepit.refined.api.Refined
import eu.timepit.refined.scalacheck.string._
import eu.timepit.refined.string.ValidBigDecimal
import eu.timepit.refined.types.string.NonEmptyString
import org.scalacheck.Arbitrary.arbitrary
import org.scalacheck.Gen

object generators {
  val nonEmptyString: Gen[String] =
    Gen
      .chooseNum(21, 40)
      .flatMap { n =>
        Gen.buildableOfN[String, Char](n, Gen.alphaChar)
      }

  def nesGen[A](f: String => A): Gen[A] =
    nonEmptyString.map(f)

  def idGen[A](f: String => A): Gen[A] =
    Gen.uuid.map(f)

  val categoryIdGen: Gen[CategoryId] =
    idGen(CategoryId.apply)

  val categoryNameGen: Gen[CategoryName] =
    nesGen(CategoryName.apply)

  val snippetIdGen: Gen[SnippetId] =
    idGen(SnippetId.apply)

  val snippetNameGen: Gen[SnippetName] =
    nesGen(SnippetName.apply)

  val snippetDescriptionGen: Gen[SnippetDescription] =
    nesGen(SnippetDescription.apply)

  val snippetContentGen: Gen[SnippetContent] =
    nesGen(SnippetContent.apply)

  val userIdGen: Gen[UserId] =
    idGen(UserId.apply)

  val orderIdGen: Gen[OrderId] =
    idGen(OrderId.apply)

  val categoryGen: Gen[Category] =
    for {
      i <- categoryIdGen
      n <- categoryNameGen
    } yield Category(i, n)
}
