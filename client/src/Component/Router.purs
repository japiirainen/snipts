module App.Component.Router where

import Prelude
import App.Capability.LogMessages (class LogMessages)
import App.Capability.Navigate (class Navigate, navigate)
import App.Capability.Now (class Now)
import App.Capability.Resource.Comment (class ManageComment)
import App.Capability.Resource.Snippet (class ManageSnippet)
import App.Capability.Resource.Tag (class ManageTag)
import App.Capability.Resource.User (class ManageUser)
import App.Component.Utils (OpaqueSlot)
import App.Data.Profile (Profile)
import App.Data.Route (Route(..), routeCodec)
import App.Env (UserEnv)
import App.Page.Home as Home
import Component.HOC.Connect (WithCurrentUser)
import Component.HOC.Connect as Connect
import Control.Monad.Reader (class MonadAsk)
import Data.Either (hush)
import Data.Foldable (elem)
import Data.Maybe (Maybe(..), fromMaybe, isJust)
import Data.Symbol (SProxy(..))
import Effect.Aff.Class (class MonadAff)
import Halogen (liftEffect)
import Halogen as H
import Halogen.HTML as HH
import Routing.Duplex as RD
import Routing.Hash (getHash)
