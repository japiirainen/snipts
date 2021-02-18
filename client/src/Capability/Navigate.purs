-- | A capability representing the ability to move the user from place to place in the application.
-- | Currently, the production monad implements hash-based routing, but that could easily be replaced
-- | with another method (pushState, for example) without breaking any code outside of `Main`.
module App.Capability.Navigate where

import Prelude
import Control.Monad.Trans.Class (lift)
import App.Data.Route (Route)
import Halogen (HalogenM)

class
  Monad m <= Navigate m where
  navigate :: Route -> m Unit
  logout :: m Unit

instance navigateHalogenM :: Navigate m => Navigate (HalogenM st act slots msg m) where
  navigate = lift <<< navigate
  logout = lift logout
