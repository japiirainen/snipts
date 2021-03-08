-- | Newtype wrapper around SnippetTitle address.
module Lib.Core.SnippetTitle
  ( SnippetTitle (..),
  )
where

-- | Newtype for snippetTitle address.
newtype SnippetTitle = SnippetTitle
  { unSnippetTitle :: Text
  }
  deriving stock (Show, Generic)
  deriving newtype (Eq, Ord, Hashable, FromField, ToField, FromJSON, ToJSON)
