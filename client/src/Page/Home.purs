module App.Page.Home where

import Prelude
import App.Api.Endpoint (ArticleParams, Pagination, noArticleParams)
import App.Capability.Navigate (class Navigate)
import App.Capability.Resource.Snippet (class ManageSnippet, getCurrentUserFeed, getSnippets)
import App.Capability.Resource.Tag (class ManageTag, getAllTags)
import App.Component.HTML.SnippetList (articleList, renderPagination)
import App.Component.HTML.Footer (footer)
import App.Component.HTML.Header (header)
import App.Component.HTML.Utils (css, maybeElem, whenElem)
import App.Component.Part.LikeButton (favorite, unfavorite)
import App.Data.PaginatedArray (PaginatedArray)
import App.Data.Profile (Profile)
import App.Data.Route (Route(..))
import App.Data.Snippet (SnippetWithMetaData)
import App.Env (UserEnv)
import Component.HOC.Connect as Connect
import Control.Monad.Reader (class MonadAsk)
import Data.Lens (Traversal')
import Data.Lens.Index (ix)
import Data.Lens.Record (prop)
import Data.Maybe (Maybe(..), isJust, isNothing)
import Data.Monoid (guard)
import Data.Symbol (SProxy(..))
import Effect.Aff.Class (class MonadAff)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Network.RemoteData (RemoteData(..), _Success, fromMaybe, toMaybe)
import Web.Event.Event (preventDefault)
import Web.UIEvent.MouseEvent (MouseEvent, toEvent)
