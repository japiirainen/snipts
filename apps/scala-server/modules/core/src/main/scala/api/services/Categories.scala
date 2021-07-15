package api.services

// import api.domain.ID
import api.domain.category._
// import api.effects.GenUUID
// import api.sql.codecs._

trait Categories[F[_]] {
  def findAll: F[List[Category]]
  def create(name: CategoryName): F[CategoryId]
}
