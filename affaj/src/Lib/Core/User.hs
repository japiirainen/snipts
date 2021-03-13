{-# LANGUAGE DeriveAnyClass #-}

module Lib.Core.User
  ( User (..),
    NewUser (..),
  )
where

import Lib.Core.Email (Email)
import Lib.Core.Id (Id)
import Lib.Core.Password (PasswordHash)

-- | Data type representing row in the @users@ table.
data User = User
  { userId :: !(Id User),
    userName :: !Text,
    userEmail :: !Email,
    userHash :: !PasswordHash
  }
  deriving stock (Generic, Show, Eq)
  deriving anyclass (FromRow)
  deriving (FromJSON, ToJSON)

-- | Data type representing a new user.
data NewUser = NewUser
  { newUserName :: !Text,
    newUserEmail :: !Email,
    password :: !Text
  }
  deriving (Generic, Show, Eq)
  deriving anyclass (ToRow)
  deriving (FromJSON, ToJSON)