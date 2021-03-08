{-# LANGUAGE DeriveAnyClass #-}

module Lib.Core.Snippet
  ( Snippet (..),
  )
where

import Lib.Core.Id (Id)
import Lib.Core.SnippetContent (SnippetContent)
import Lib.Core.SnippetDescription (SnippetDescription)
import Lib.Core.SnippetTitle (SnippetTitle)
import Lib.Core.User (User)

-- | Data type representing row in the @snippets@ table.
data Snippet = Snippet
  { snippetId :: !(Id Snippet),
    title :: !SnippetTitle,
    author :: !(Id User),
    description :: !SnippetDescription,
    content :: !SnippetContent
  }
  deriving stock (Generic, Show, Eq)
  deriving anyclass (FromRow)
  deriving (FromJSON, ToJSON)
