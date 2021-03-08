-- | Newtype wrapper around SnippetContent address.
module Lib.Core.SnippetContent
  ( SnippetContent (..),
  )
where

-- | Newtype for snippetContent address.
newtype SnippetContent = SnippetContent
  { unSnippetContent :: Text
  }
  deriving stock (Show, Generic)
  deriving newtype (Eq, Ord, Hashable, FromField, ToField, FromJSON, ToJSON)