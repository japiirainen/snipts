module App.AppM where

import Prelude
import App.Api.Endpoint (Endpoint(..), noSnippetParams)
import App.Api.Request (RequestMethod(..))
import App.Api.Request as Request
import App.Api.Utils (authenticate, decode, decodeWithUser, mkAuthRequest, mkRequest)
import App.Capability.LogMessages (class LogMessages)
import App.Capability.Navigate (class Navigate, navigate)
import App.Capability.Now (class Now)
import App.Capability.Resource.Comment (class ManageComment)
import App.Capability.Resource.Snippet (class ManageSnippet)
import App.Capability.Resource.Tag (class ManageTag)
import App.Capability.Resource.User (class ManageUser)
import App.Data.Comment as Comment
import App.Data.Log as Log
import App.Data.Profile as Profile
import App.Data.Route as Route
import App.Data.Snippet as Snippet
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

derive newtype instance applicativeAppM :: Applicative AppM

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

instance logMessagesAppM :: LogMessages AppM where
  logMessage log = do
    env <- ask
    liftEffect case env.logLevel, Log.reason log of
      Prod, Log.Debug -> pure unit
      _, _ -> Console.log $ Log.message log

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

-- | Operations for managin users
instance manageUserAppM :: ManageUser AppM where
  loginUser = authenticate Request.login
  registerUser = authenticate Request.register
  getCurrentUser = do
    mbJson <- mkAuthRequest { endpoint: User, method: Get }
    map (map _.user) $ decode (CAR.object "User" { user: Profile.profileWithEmailCodec }) mbJson
  getAuthor username = do
    mbJson <- mkAuthRequest { endpoint: Profiles username, method: Get }
    map (map _.profile) $ decodeWithUser (\u -> CAR.object "Profile" { profile: Profile.authorCodec u }) mbJson
  updateUser fields = do
    void $ mkAuthRequest { endpoint: User, method: Put (Just (Codec.encode Profile.profileWithEmailPasswordCodec fields)) }
  followUser username = do
    mbJson <- mkAuthRequest { endpoint: Follow username, method: Post Nothing }
    map (map _.profile) $ decodeWithUser (\u -> CAR.object "Profile" { profile: Profile.authorCodec u }) mbJson
  unfollowUser username = do
    mbJson <- mkAuthRequest { endpoint: Follow username, method: Delete }
    map (map _.profile) $ decodeWithUser (\u -> CAR.object "Profile" { profile: Profile.authorCodec u }) mbJson

-- | Operations for managing tags
instance manageTagAppM :: ManageTag AppM where
  getAllTags = do
    mbJson <- mkRequest { endpoint: Tags, method: Get }
    map (map _.tags) $ decode (CAR.object "Tags" { tags: CA.array CA.string }) mbJson

-- | Operations for managing comments
instance manageCommentAppM :: ManageComment AppM where
  getComments slug = do
    mbJson <- mkRequest { endpoint: Comments slug, method: Get }
    map (map _.comments)
      $ decodeWithUser (\u -> CAR.object "Comments" { comments: CA.array (Comment.codec u) }) mbJson
  createComment slug body =
    let
      method = Post $ Just $ Codec.encode (CAR.object "CommentBody" { body: CA.string }) { body }
    in
      void $ mkAuthRequest { endpoint: Comments slug, method }
  deleteComment slug id = void $ mkAuthRequest { endpoint: Comment slug id, method: Delete }

-- | Operations for managing snippets
instance manageSnippetAppM :: ManageSnippet AppM where
  getSnippet slug = do
    mbJson <- mkRequest { endpoint: Snippet slug, method: Get }
    map (map _.snippet)
      $ decodeWithUser (\u -> CAR.object "Snippet" { snippet: Snippet.snippetWithMetadataCodec u }) mbJson
  getSnippets fields =
    mkRequest { endpoint: Snippets fields, method: Get }
      >>= decodeWithUser Snippet.snippetsWithMetaDataCodec
  createSnippet snippet = do
    let
      codec = CAR.object "Snippet" { snippet: Snippet.snippetCodec }

      method = Post $ Just $ Codec.encode codec { snippet }
    mbJson <- mkAuthRequest { endpoint: Snippets noSnippetParams, method }
    map (map _.snippet)
      $ decodeWithUser (\u -> CAR.object "Snippet" { snippet: Snippet.snippetWithMetadataCodec u }) mbJson
  updateSnippet slug snippet = do
    let
      codec = CAR.object "Snippet" { snippet: Snippet.snippetCodec }

      method = Put $ Just $ Codec.encode codec { snippet }
    mbJson <- mkAuthRequest { endpoint: Snippet slug, method }
    map (map _.snippet) $ decodeWithUser (\u -> CAR.object "Snippet" { snippet: Snippet.snippetWithMetadataCodec u }) mbJson
  deleteSnippet slug = void $ mkAuthRequest { endpoint: Snippet slug, method: Delete }
  likeSnippet slug = do
    mbJson <- mkAuthRequest { endpoint: Like slug, method: Post Nothing }
    map (map _.snippet) $ decodeWithUser (\u -> CAR.object "Snippet" { snippet: Snippet.snippetWithMetadataCodec u }) mbJson
  unlikeSnippet slug = do
    mbJson <- mkAuthRequest { endpoint: Like slug, method: Delete }
    map (map _.snippet) $ decodeWithUser (\u -> CAR.object "Snippet" { snippet: Snippet.snippetWithMetadataCodec u }) mbJson
  getCurrentUserFeed params =
    mkAuthRequest { endpoint: Feed params, method: Get }
      >>= decodeWithUser Snippet.snippetsWithMetaDataCodec
