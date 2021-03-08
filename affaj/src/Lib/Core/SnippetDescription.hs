-- | Newtype wrapper around SnippetDescription address.
module Lib.Core.SnippetDescription
  ( SnippetDescription (..),
  )
where

-- | Newtype for snippetDescription address.
newtype SnippetDescription = SnippetDescription
  { unSnippetDescription :: Text
  }
  deriving stock (Show, Generic)
  deriving newtype (Eq, Ord, Hashable, FromField, ToField, FromJSON, ToJSON)