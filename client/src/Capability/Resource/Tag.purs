module App.Capability.Resource.Tag where

import Prelude
import Data.Maybe (Maybe)
import Halogen (HalogenM, lift)

class
  Monad m <= ManageTag m where
  getAllTags :: m (Maybe (Array String))

instance manageTagHalogemM :: ManageTag m => ManageTag (HalogenM st act cs msg m) where
  getAllTags = lift getAllTags
