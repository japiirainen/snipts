module App.Data.Profile where

import Prelude
import App.Data.Email (Email)
import App.Data.Email as Email
import App.Data.Username (Username)
import App.Data.Username as Username
import Data.Codec (mapCodec)
import Data.Codec.Argonaut (JsonCodec, JsonDecodeError)
import Data.Codec.Argonaut as CA
import Data.Codec.Argonaut.Compat as CAC
import Data.Codec.Argonaut.Record as CAR
import Data.Either (Either)
import Data.Maybe (Maybe(..))

data Relation
  = Following
  | NotFollowing
  | You

derive instance eqRelation :: Eq Relation

type ProfileRep row
  = ( username :: Username
    , bio :: Maybe String
    | row
    )

-- | The `Profile` type consists only of three core fields: the username, biography
type Profile
  = { | ProfileRep () }

type ProfileWithEmail
  = { | ProfileRep ( email :: Email ) }

type ProfileWithEmailPassword
  = { | ProfileRep ( email :: Email, password :: Maybe String ) }

-- | The `Author` type extends the `Profile` fields with `Relation` type.
type Author
  = { | ProfileRep ( relation :: Relation ) }

profileCodec :: JsonCodec Profile
profileCodec =
  CAR.object "Profile"
    { username: Username.codec
    , bio: CAC.maybe CA.string
    }

profileWithEmailCodec :: JsonCodec ProfileWithEmail
profileWithEmailCodec =
  CAR.object "Profile"
    { username: Username.codec
    , email: Email.codec
    , bio: CAC.maybe CA.string
    }

profileWithEmailPasswordCodec :: JsonCodec ProfileWithEmailPassword
profileWithEmailPasswordCodec =
  CAR.object "Profile"
    { username: Username.codec
    , email: Email.codec
    , password: CAC.maybe CA.string
    , bio: CAC.maybe CA.string
    }

authorCodec :: Maybe Username -> JsonCodec Author
authorCodec mbUsername = mapCodec to from codec
  where
  codec =
    CAR.object "Author"
      { username: Username.codec
      , bio: CAC.maybe CA.string
      , following: CA.boolean
      }

  to :: { | ProfileRep ( following :: Boolean ) } -> Either JsonDecodeError Author
  to { username, bio, following } =
    pure do
      let
        mkAuthor = { username, bio, relation: _ }
      mkAuthor case mbUsername of
        Just user
          | user == username -> You
        _
          | following -> Following
        _ -> NotFollowing

  from :: Author -> { | ProfileRep ( following :: Boolean ) }
  from { username, bio, relation } = do
    let
      mkAuthorJson = { username, bio, following: _ }
    mkAuthorJson case relation of
      Following -> true
      _ -> false
