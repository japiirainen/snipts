package api

import java.util.UUID

import api.domain.auth._
import api.domain.category._
import api.domain.language._
import api.domain.link._
import api.domain.snippet._
import api.http.auth.users._
import eu.timepit.refined.scalacheck.string._
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

  def idGen[A](f: UUID => A): Gen[A] =
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

  val languageIdGen: Gen[LanguageId] =
    idGen(LanguageId.apply)

  val languageNameGen: Gen[LanguageName] =
    nesGen(LanguageName.apply)

  val userIdGen: Gen[UserId] =
    idGen(UserId.apply)

  val userNameGen: Gen[UserName] =
    nesGen(UserName.apply)

  val passwordGen: Gen[Password] =
    nesGen(Password.apply)

  val encryptedPasswordGen: Gen[EncryptedPassword] =
    nesGen(EncryptedPassword.apply)

  val linkIdGen: Gen[LinkId] =
    idGen(LinkId.apply)

  val linkUrlGen: Gen[LinkUrl] =
    nesGen(LinkUrl.apply)

  val languageGen: Gen[Language] =
    for {
      i <- languageIdGen
      n <- languageNameGen
    } yield Language(i, n)

  val categoryGen: Gen[Category] =
    for {
      i <- categoryIdGen
      n <- categoryNameGen
    } yield Category(i, n)

  val snippetGen: Gen[Snippet] =
    for {
      i <- snippetIdGen
      n <- snippetNameGen
      d <- snippetDescriptionGen
      c <- snippetContentGen
      l <- languageGen
      cat <- categoryGen
    } yield Snippet(i, n, d, c, l, cat)


  val linkGen: Gen[Link] =
    for {
        i <- linkIdGen
        u <- linkUrlGen
        sid <- snippetIdGen
    } yield Link(i, u, sid)


  val userGen: Gen[User] =
    for {
      i <- userIdGen
      n <- userNameGen
    } yield User(i, n)

  val adminUserGen: Gen[AdminUser] =
    userGen.map(AdminUser(_))

  val commonUserGen: Gen[CommonUser] =
    userGen.map(CommonUser(_))

  val languageParamGen: Gen[LanguageParam] =
    arbitrary[NonEmptyString].map(LanguageParam(_))
}
