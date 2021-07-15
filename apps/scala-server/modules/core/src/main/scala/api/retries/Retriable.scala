package api.retries

import derevo.cats.show
import derevo.derive

@derive(show)
sealed trait Retriable

object Retriable {
  case object GithubImport extends Retriable
}
