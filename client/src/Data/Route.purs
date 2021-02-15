-- | We can represent valid locations in our application with a simple sum type. This will cause
-- | any invalid routes to fail at compile-time.
module App.Data.Route where

import Prelude hiding ((/))
import App.Data.Username (Username)
import App.Data.Username as Username
import Data.Either (note)
import Data.Generic.Rep (class Generic)
import Routing.Duplex (RouteDuplex', as, root, segment)
import Routing.Duplex.Generic (noArgs, sum)
import Routing.Duplex.Generic.Syntax ((/))
import Slug (Slug)
import Slug as Slug

-- | This is the representation of all application routes as a sum type.
data Route
  = Home
  | Login
  | Register
  | Settings
  | Editor
  | EditSnippet Slug
  | ViewSnippet Slug
  | Profile Username
  | Favorites Username

derive instance genericRoute :: Generic Route _

derive instance eqRoute :: Eq Route

derive instance ordRoute :: Ord Route

routeCodec :: RouteDuplex' Route
routeCodec =
  root
    $ sum
        { "Home": noArgs
        , "Login": "login" / noArgs
        , "Register": "register" / noArgs
        , "Settings": "settings" / noArgs
        , "Editor": "editor" / noArgs
        , "EditSnippet": "editor" / slug segment
        , "ViewSnippet": "snippet" / slug segment
        , "Profile": "profile" / uname segment
        , "Favorites": "profile" / uname segment / "favorites"
        }

-- | This combinator transforms a codec over `String` into one that operates on the `Slug` type.
slug :: RouteDuplex' String -> RouteDuplex' Slug
slug = as Slug.toString (Slug.parse >>> note "Bad Slug")

-- | This combinator transforms a codec over `String` into one that operates on the `Username` type.
uname :: RouteDuplex' String -> RouteDuplex' Username
uname = as Username.toString (Username.parse >>> note "Bad username")
