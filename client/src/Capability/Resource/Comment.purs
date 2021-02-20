module App.Capability.Resource.Comment where

import Prelude
import App.Data.Comment (Comment, CommentId)
import Data.Maybe (Maybe)
import Halogen (HalogenM, lift)
import Slug (Slug)

class
  Monad m <= ManageComment m where
  getComments :: Slug -> m (Maybe (Array Comment))
  createComment :: Slug -> String -> m Unit
  deleteComment :: Slug -> CommentId -> m Unit

instance manageCommentHalogenM :: ManageComment m => ManageComment (HalogenM st act cs msg m) where
  getComments = lift <<< getComments
  createComment s = lift <<< createComment s
  deleteComment s = lift <<< deleteComment s
