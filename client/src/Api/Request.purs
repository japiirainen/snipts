module App.Api.Request
  ( Token
  , BaseURL(..)
  , RequestMethod(..)
  , RequestOptions(..)
  , defaultRequest
  , RegisterFields(..)
  , LoginFields(..)
  , login
  , register
  , readToken
  , writeToken
  , removeToken
  ) where

import Prelude
import Affjax (Request, printError, request)
import Affjax.RequestBody as RB
import Affjax.RequestHeader (RequestHeader(..))
import Affjax.ResponseFormat as RF
import App.Api.Endpoint (Endpoint(..), endpointCodec)
import App.Data.Email (Email)
import App.Data.Email as Email
import App.Data.Profile (Profile)
import App.Data.Profile as Profile
import App.Data.Username (Username)
import App.Data.Username as Username
import Data.Argonaut.Core (Json)
import Data.Bifunctor (lmap)
import Data.Codec as Codec
import Data.Codec.Argonaut (JsonCodec, JsonDecodeError, printJsonDecodeError)
import Data.Codec.Argonaut as CA
import Data.Codec.Argonaut.Record as CAR
import Data.Either (Either(..))
import Data.HTTP.Method (Method(..))
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..))
import Effect (Effect)
import Effect.Aff.Class (class MonadAff, liftAff)
import Routing.Duplex (print)
import Web.HTML (window)
import Web.HTML.Window (localStorage)
import Web.Storage.Storage (getItem, removeItem, setItem)

newtype Token
  = Token String

derive instance eqToken :: Eq Token

derive instance ordToken :: Ord Token

instance showToken :: Show Token where
  show (Token _) = "Token {- token -}"

newtype BaseURL
  = BaseURL String

data RequestMethod
  = Get
  | Post (Maybe Json)
  | Put (Maybe Json)
  | Delete

type RequestOptions
  = { endpoint :: Endpoint
    , method :: RequestMethod
    }

defaultRequest :: BaseURL -> Maybe Token -> RequestOptions -> Request Json
defaultRequest (BaseURL baseUrl) auth { endpoint, method } =
  { method: Left method
  , url: baseUrl <> print endpointCodec endpoint
  , headers:
      case auth of
        Nothing -> []
        Just (Token t) -> [ RequestHeader "Authorization" $ "Bearer " <> t ]
  , content: RB.json <$> body
  , username: Nothing
  , password: Nothing
  , withCredentials: false
  , responseFormat: RF.json
  }
  where
  Tuple method body = case method of
    Get -> Tuple GET Nothing
    Post b -> Tuple POST b
    Put b -> Tuple PUT b
    Delete -> Tuple DELETE Nothing

type RegisterFields
  = { email :: Email
    , password :: String
    , username :: Username
    }

registerCodec :: JsonCodec RegisterFields
registerCodec =
  CAR.object "registerfields"
    { email: Email.codec
    , password: CA.string
    , username: Username.codec
    }

type LoginFields
  = { email :: Email
    , password :: String
    }

loginCodec :: JsonCodec LoginFields
loginCodec =
  CAR.object "LoginFields"
    { email: Email.codec
    , password: CA.string
    }

login :: forall m. MonadAff m => BaseURL -> LoginFields -> m (Either String (Tuple Token Profile))
login baseUrl fields =
  let
    method = Post $ Just $ Codec.encode (CAR.object "User" { user: loginCodec }) { user: fields }
  in
    requestUser baseUrl { endpoint: Login, method }

register :: forall m. MonadAff m => BaseURL -> RegisterFields -> m (Either String (Tuple Token Profile))
register baseUrl fields =
  let
    method = Post $ Just $ Codec.encode (CAR.object "User" { user: registerCodec }) { user: fields }
  in
    requestUser baseUrl { endpoint: Users, method }

requestUser :: forall m. MonadAff m => BaseURL -> RequestOptions -> m (Either String (Tuple Token Profile))
requestUser baseUrl opts = do
  res <- liftAff $ request $ defaultRequest baseUrl Nothing opts
  case res of
    Left e -> pure $ Left $ printError e
    Right v -> pure $ lmap printJsonDecodeError $ decodeAuthProfile =<< Codec.decode (CAR.object "User" { user: CA.json }) v.body

decodeAuthProfile :: { user :: Json } -> Either JsonDecodeError (Tuple Token Profile)
decodeAuthProfile { user } = do
  { token } <- Codec.decode (CAR.object "Token" { token: tokenCodec }) user
  profile <- Codec.decode Profile.profileCodec user
  pure (Tuple token profile)
  where
  tokenCodec = CA.prismaticCodec (Just <<< Token) (\(Token t) -> t) CA.string

tokenKey = "token" :: String

readToken :: Effect (Maybe Token)
readToken = do
  str <- getItem tokenKey =<< localStorage =<< window
  pure $ map Token str

writeToken :: Token -> Effect Unit
writeToken (Token str) = setItem tokenKey str =<< localStorage =<< window

removeToken :: Effect Unit
removeToken = removeItem tokenKey =<< localStorage =<< window
