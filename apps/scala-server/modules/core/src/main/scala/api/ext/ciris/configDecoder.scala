package api.ext.ciris

import _root_.ciris.ConfigDecoder
import api.ext.derevo.Derive

object configDecoder extends Derive[Decoder.Id]

object Decoder {
  type Id[A] = ConfigDecoder[String, A]
}
