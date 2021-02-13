module App.Data.Profile where

import App.Data.Username (Username)

type ProfileRep row
  = ( username :: Username
    | row
    )

type Profile
  = { | ProfileRep () }
