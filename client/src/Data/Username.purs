module App.Data.Username
  ( Username
  , parse
  , toString
  , codec
  ) where

import Prelude
import Data.Codec.Argonaut (JsonCodec)
import Data.Codec.Argonaut as CA
import Data.Maybe (Maybe(..))
import Data.Newtype (class Newtype)
import Data.Profunctor (wrapIso)

-- | We'll represent usernames as a newtype wrapper around a string. Newtypes have no performance
-- | overhead, so they're the fastest way to add a documenting type to a primitive type like
-- | `String`.
newtype Username
  = Username String

derive instance eqUsername :: Eq Username

derive instance ordUsername :: Ord Username

derive instance newTypeUsername :: Newtype Username _

codec :: JsonCodec Username
codec = wrapIso Username CA.string

parse :: String -> Maybe Username
parse "" = Nothing

-- | This function requires a string to pass some validation before being considered a valid
-- | `Username`. For now, we'll just enforce a username is non-empty, but we might introduce more
-- | sophisticated validation later on.
parse str = Just (Username str)

-- | While we don't want to be able to write or manipulate a `Username` without passing validation,
-- | we should still be able to read the string inside. Providing this function makes `Username`
-- | a read-only type.
toString :: Username -> String
toString (Username str) = str
