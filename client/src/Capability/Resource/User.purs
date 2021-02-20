module App.Capability.Resource.User where

import Prelude
import App.Api.Request (LoginFields, RegisterFields)
import App.Data.Email (Email)
import App.Data.Profile (Author, Profile, ProfileRep, ProfileWithEmail)
import App.Data.Username (Username)
import Data.Maybe (Maybe)
import Halogen (HalogenM, lift)

type UpdateProfileFields
  = { email :: Email
    , password :: Maybe String
    | ProfileRep ()
    }

class
  Monad m <= ManageUser m where
  loginUser :: LoginFields -> m (Maybe Profile)
  registerUser :: RegisterFields -> m (Maybe Profile)
  getCurrentUser :: m (Maybe ProfileWithEmail)
  getAuthor :: Username -> m (Maybe Author)
  updateUser :: UpdateProfileFields -> m Unit
  followUser :: Username -> m (Maybe Author)
  unfollowUser :: Username -> m (Maybe Author)

instance manageUserHalogenM :: ManageUser m => ManageUser (HalogenM st act slots msg m) where
  loginUser = lift <<< loginUser
  registerUser = lift <<< registerUser
  getCurrentUser = lift getCurrentUser
  getAuthor = lift <<< getAuthor
  updateUser = lift <<< updateUser
  followUser = lift <<< followUser
  unfollowUser = lift <<< unfollowUser
