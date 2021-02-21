module App.Component.Utils where

import Prelude
import Control.Monad.Rec.Class (forever)
import Effect.Aff (error, forkAff, killFiber)
import Effect.Aff.Bus as Bus
import Effect.Aff.Class (class MonadAff)
import Halogen as H
import Halogen.Query.EventSource as ES

type OpaqueSlot slot
  = forall query. H.Slot query Void slot

busEventSource :: forall m r act. MonadAff m => Bus.BusR' r act -> ES.EventSource m act
busEventSource bus =
  ES.affEventSource \emitter -> do
    fiber <- forkAff $ forever $ ES.emit emitter =<< Bus.read bus
    pure (ES.Finalizer (killFiber (error "Event source closed") fiber))
