module App.Env where

import Prelude
import App.Data.Profile (Profile)
import App.Api.Request (BaseURL)
import Data.Maybe (Maybe)
import Effect.Aff.Bus (BusRW)
import Effect.Ref (Ref)

type Env
  = { logLevel :: LogLevel
    , baseUrl :: BaseURL
    , userEnv :: UserEnv
    }

data LogLevel
  = Dev
  | Prod

derive instance eqLogLevel :: Eq LogLevel

derive instance ordLogLevel :: Ord LogLevel

type UserEnv
  = { currentUser :: Ref (Maybe Profile)
    , userBus :: BusRW (Maybe Profile)
    }
