package api.services

import api.domain.language._

trait Languages[F[_]] {
  def findAll: F[List[Language]]
  def create(name: LanguageName): F[LanguageId]
}
