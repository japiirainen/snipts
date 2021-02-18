module App.Api.Utils where

import Prelude
import Affjax (request)
import App.Api.Request (BaseURL, RequestOptions, Token, defaultRequest, readToken, writeToken)
import App.Capability.LogMessages (class LogMessages, logError)
import App.Capability.Now (class Now)
import App.Data.Profile (Profile)
import App.Data.Username (Username)
import App.Env (UserEnv)
import Control.Monad.Reader (class MonadAsk, ask, asks)
import Data.Argonaut.Core (Json)
import Data.Bifunctor (rmap)
import Data.Codec.Argonaut (JsonCodec, printJsonDecodeError)
import Data.Codec.Argonaut as CA
import Data.Either (Either(..), hush)
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..))
import Effect.Aff.Bus as Bus
import Effect.Aff.Class (class MonadAff, liftAff)
import Effect.Class (class MonadEffect, liftEffect)
import Effect.Ref as Ref

mkRequest ::
  forall m r.
  MonadAff m =>
  MonadAsk { baseUrl :: BaseURL | r } m =>
  RequestOptions ->
  m (Maybe Json)
mkRequest opts = do
  { baseUrl } <- ask
  response <- liftAff $ request $ defaultRequest baseUrl Nothing opts
  pure $ hush $ rmap _.body response

mkAuthRequest ::
  forall m r.
  MonadAff m =>
  MonadAsk { baseUrl :: BaseURL | r } m =>
  RequestOptions ->
  m (Maybe Json)
mkAuthRequest opts = do
  { baseUrl } <- ask
  token <- liftEffect readToken
  response <- liftAff $ request $ defaultRequest baseUrl token opts
  pure $ hush $ rmap _.body response

authenticate ::
  forall m a r.
  MonadAff m =>
  MonadAsk { baseUrl :: BaseURL, userEnv :: UserEnv | r } m =>
  LogMessages m =>
  Now m =>
  (BaseURL -> a -> m (Either String (Tuple Token Profile))) ->
  a ->
  m (Maybe Profile)
authenticate req fields = do
  { baseUrl, userEnv } <- ask
  req baseUrl fields
    >>= case _ of
        Left err -> logError err *> pure Nothing
        Right (Tuple token profile) -> do
          liftEffect do
            writeToken token
            Ref.write (Just profile) userEnv.currentUser
          -- any time we write to the current user ref, we should also broadcast the change
          liftAff $ Bus.write (Just profile) userEnv.userBus
          pure (Just profile)

decode :: forall m a. LogMessages m => Now m => JsonCodec a -> Maybe Json -> m (Maybe a)
decode _ Nothing = logError "Response malformed" *> pure Nothing

decode codec (Just json) = case CA.decode codec json of
  Left err -> logError (printJsonDecodeError err) *> pure Nothing
  Right response -> pure (Just response)

decodeWithUser ::
  forall m a r.
  MonadEffect m =>
  MonadAsk { userEnv :: UserEnv | r } m =>
  LogMessages m =>
  Now m =>
  (Maybe Username -> JsonCodec a) ->
  Maybe Json ->
  m (Maybe a)
decodeWithUser codec json = do
  maybeProfile <- (liftEffect <<< Ref.read) =<< asks _.userEnv.currentUser
  decode (codec (_.username <$> maybeProfile)) json
