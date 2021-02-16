module App.Data.Comment where

import Prelude
import App.Data.PreciseDateTime as PDT
import App.Data.Profile (Author)
import App.Data.Profile as Profile
import App.Data.Username (Username)
import Data.Codec.Argonaut (JsonCodec)
import Data.Codec.Argonaut as CA
import Data.Codec.Argonaut.Record as CAR
import Data.Maybe (Maybe)
import Data.Newtype (class Newtype)
import Data.PreciseDateTime (PreciseDateTime)
import Data.Profunctor (wrapIso)

newtype CommentId
  = CommentId Int

derive instance newtypeCommentId :: Newtype CommentId _

derive instance eqCommentId :: Eq CommentId

derive instance ordCommentId :: Ord CommentId

type Comment
  = { id :: CommentId
    , createdOn :: PreciseDateTime
    , body :: String
    , author :: Author
    }

codec :: Maybe Username -> JsonCodec Comment
codec mbUsername =
  CAR.object "Comment"
    { id: wrapIso CommentId CA.int
    , createdOn: PDT.codec
    , body: CA.string
    , author: Profile.authorCodec mbUsername
    }
