module Main where

import Prelude
import App.Button as Button
import Effect (Effect)
import Halogen.Aff as HA
import Halogen.VDom.Driver (runUI)
import Affjax (request, printError)
import App.Api.Endpoint (Endpoint(..))
import Conduit.Api.Request (BaseURL(..), RequestMethod(..), defaultRequest, readToken)
import App.AppM (runAppM)
import App.Component.Router as Router
import App.Data.Profile as Profile
import App.Data.Route (routeCodec)
import App.Env (Env, LogLevel(..))
import Data.Bifunctor (lmap)
import Data.Codec as Codec
import Data.Codec.Argonaut (printJsonDecodeError)
import Data.Codec.Argonaut as CA
import Data.Codec.Argonaut.Record as CAR
import Data.Either (Either(..), hush)
import Data.Maybe (Maybe(..))
import Data.Traversable (traverse_)
import Effect (Effect)
import Effect.Aff (Aff, launchAff_)
import Effect.Aff.Bus as Bus
import Effect.Ref as Ref
import Halogen (liftEffect)
import Halogen as H
import Halogen.Aff as HA
import Halogen.HTML as HH
import Halogen.VDom.Driver (runUI)
import Routing.Duplex (parse)
import Routing.Hash (matchesWith)

main :: Effect Unit
main =
  HA.runHalogenAff do
    body <- HA.awaitBody
    runUI Button.component unit body
