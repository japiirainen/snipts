module App.Capability.Resource.Snippet where

import Prelude
import App.Api.Endpoint (Pagination, SnippetParams)
import App.Data.PaginatedArray (PaginatedArray)
import App.Data.Snippet (Snippet, SnippetWithMetaData)
import Data.Maybe (Maybe)
import Halogen (HalogenM, lift)
import Slug (Slug)

class
  Monad m <= ManageSnippet m where
  getSnippet :: Slug -> m (Maybe SnippetWithMetaData)
  getSnippets :: SnippetParams -> m (Maybe (PaginatedArray SnippetWithMetaData))
  createSnippet :: Snippet -> m (Maybe SnippetWithMetaData)
  updateSnippet :: Slug -> Snippet -> m (Maybe SnippetWithMetaData)
  deleteSnippet :: Slug -> m Unit
  likeSnippet :: Slug -> m (Maybe SnippetWithMetaData)
  unlikeSnippet :: Slug -> m (Maybe SnippetWithMetaData)
  getCurrentUserFeed :: Pagination -> m (Maybe (PaginatedArray SnippetWithMetaData))

instance manageSnippetHalogenM :: ManageSnippet m => ManageSnippet (HalogenM st act slots msg m) where
  getSnippet = lift <<< getSnippet
  getSnippets = lift <<< getSnippets
  createSnippet = lift <<< createSnippet
  updateSnippet s = lift <<< updateSnippet s
  deleteSnippet = lift <<< deleteSnippet
  likeSnippet = lift <<< likeSnippet
  unlikeSnippet = lift <<< unlikeSnippet
  getCurrentUserFeed = lift <<< getCurrentUserFeed
