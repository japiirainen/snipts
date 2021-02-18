module App.AppM where

import Prelude
import App.Api.Endpoint (Endpoint(..), noArticleParams)
import Conduit.Api.Request (RequestMethod(..))
import Conduit.Api.Request as Request
import App.Api.Utils (authenticate, decode, decodeWithUser, mkAuthRequest, mkRequest)
import App.Capability.LogMessages (class LogMessages)
import App.Capability.Navigate (class Navigate, navigate)
import App.Capability.Now (class Now)
import App.Capability.Resource.Article (class ManageArticle)
import App.Capability.Resource.Comment (class ManageComment)
import App.Capability.Resource.Tag (class ManageTag)
import App.Capability.Resource.User (class ManageUser)
import App.Data.Article as Article
import App.Data.Comment as Comment
import App.Data.Log as Log
import App.Data.Profile as Profile
import App.Data.Route as Route
import App.Env (Env, LogLevel(..))
import Control.Monad.Reader.Trans (class MonadAsk, ReaderT, ask, asks, runReaderT)
import Control.Parallel (class Parallel, parallel, sequential)
import Data.Codec.Argonaut as CA
import Data.Codec.Argonaut as Codec
import Data.Codec.Argonaut.Record as CAR
import Data.Maybe (Maybe(..))
import Effect.Aff (Aff, ParAff)
import Effect.Aff.Bus as Bus
import Effect.Aff.Class (class MonadAff, liftAff)
import Effect.Class (class MonadEffect, liftEffect)
import Effect.Console as Console
import Effect.Now as Now
import Effect.Ref as Ref
import Routing.Duplex (print)
import Routing.Hash (setHash)
import Type.Equality (class TypeEquals, from)

newtype AppM a
  = AppM (ReaderT Env Aff a)

runAppM :: Env -> AppM ~> Aff
runAppM env (AppM m) = runReaderT m env

derive newtype instance functorAppM :: Functor AppM

derive newtype instance applyAppM :: Apply AppM

derive newtype instance bindAppM :: Bind AppM

derive newtype instance monadAppM :: Monad AppM

derive newtype instance monadEffectAppM :: MonadEffect AppM

derive newtype instance monadAffAppM :: MonadAff AppM

instance monadAskAppM :: TypeEquals e Env => MonadAsk e AppM where
  ask = AppM $ asks from

newtype ParAppM a
  = ParAppM (ReaderT Env ParAff a)

derive newtype instance functorParAppM :: Functor ParAppM

derive newtype instance applyParAppM :: Apply ParAppM

derive newtype instance applicativeParAppM :: Applicative ParAppM

instance parallelAppM :: Parallel ParAppM AppM where
  parallel (AppM readerT) = ParAppM (parallel readerT)
  sequential (ParAppM readerT) = AppM (sequential readerT)

instance nowAppM :: Now AppM where
  now = liftEffect Now.now
  nowDate = liftEffect Now.nowDate
  nowTime = liftEffect Now.nowTime
  nowDateTime = liftEffect Now.nowDateTime

instance navigateAppM :: Navigate AppM where
  navigate = liftEffect <<< setHash <<< print Route.routeCodec
  logout = do
    { currentUser, userBus } <- asks _.userEnv
    liftEffect do
      Ref.write Nothing currentUser
      Request.removeToken
    liftAff do
      Bus.write Nothing userBus
    navigate Route.Home
