package api.sql

import api.domain.auth._
import api.domain.category._
import api.domain.language._
import api.domain.link._
import api.domain.snippet._
import skunk._
import skunk.codec.all._

object codecs {
  val snippetId: Codec[SnippetId]                   = uuid.imap[SnippetId](SnippetId(_))(_.value)
  val snippetName: Codec[SnippetName]               = varchar.imap[SnippetName](SnippetName(_))(_.value)
  val snippetDescription: Codec[SnippetDescription] = varchar.imap[SnippetDescription](SnippetDescription(_))(_.value)

  val categoryId: Codec[CategoryId]     = uuid.imap[CategoryId](CategoryId(_))(_.value)
  val categoryName: Codec[CategoryName] = varchar.imap[CategoryName](CategoryName(_))(_.value)

  val languageId: Codec[LanguageId]     = uuid.imap[LanguageId](LanguageId(_))(_.value)
  val languageName: Codec[LanguageName] = varchar.imap[LanguageName](LanguageName(_))(_.value)

  val linkId: Codec[LinkId]   = uuid.imap[LinkId](LinkId(_))(_.value)
  val linkUrl: Codec[LinkUrl] = varchar.imap[LinkUrl](LinkUrl(_))(_.value)

  val userId: Codec[UserId]     = uuid.imap[UserId](UserId(_))(_.value)
  val userName: Codec[UserName] = varchar.imap[UserName](UserName(_))(_.value)

  val encPassword: Codec[EncryptedPassword] = varchar.imap[EncryptedPassword](EncryptedPassword(_))(_.value)
}
