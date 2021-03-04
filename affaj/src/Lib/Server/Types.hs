-- | This module introduce aliases to use for @servant-generic@ types and functions writing.
module Lib.Server.Types
  ( AppServer,
    ToApi,
  )
where

import Lib.App (App)
import Servant.API.Generic (ToServantApi)
import Servant.Server.Generic (AsServerT)

type AppServer = AsServerT App

type ToApi (site :: Type -> Type) = ToServantApi site
