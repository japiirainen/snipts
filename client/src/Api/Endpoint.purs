module App.Api.Endpoint where

import Prelude hiding ((/))
import App.Data.Comment (CommentId)
import App.Data.Route (slug, uname)
import App.Data.Username (Username)
import Data.Generic.Rep (class Generic)
import Data.Lens.Iso.Newtype (_Newtype)
import Data.Maybe (Maybe(..))
import Routing.Duplex (RouteDuplex', int, optional, prefix, root, segment, string)
import Routing.Duplex.Generic (noArgs, sum)
import Routing.Duplex.Generic.Syntax ((/), (?))
import Slug (Slug)

type PaginationRep
  = ( limit :: Maybe Int
    , offset :: Maybe Int
    )

type Pagination
  = { | PaginationRep }

type SnippetParams
  = { tag :: Maybe String
    , author :: Maybe Username
    , liked :: Maybe Username
    | PaginationRep
    }

noSnippetParams :: SnippetParams
noSnippetParams =
  { tag: Nothing
  , author: Nothing
  , liked: Nothing
  , offset: Nothing
  , limit: Nothing
  }

data Endpoint
  = Login
  | User
  | Users
  | Follow Username
  | Snippet Slug
  | Comment Slug CommentId
  | Comments Slug
  | Like Slug
  | Snippets SnippetParams
  | Profiles Username
  | Feed Pagination
  | Tags

derive instance genericEndpoint :: Generic Endpoint _

endpointCodec :: RouteDuplex' Endpoint
endpointCodec =
  root $ prefix "api"
    $ sum
        { "Login": "users" / "login" / noArgs
        , "User": "user" / noArgs
        , "Users": "users" / noArgs
        , "Follow": "profiles" / uname segment / "follow"
        , "Snippet": "snippets" / slug segment
        , "Comment": "articles" / slug segment / "comments" / commentId
        , "Comments": "articles" / slug segment / "comments"
        , "Like": "like" / slug segment / "like"
        -- automatically create query parameters
        , "Snippets":
            "snippets"
              ? { tag: optional <<< string
                , author: optional <<< uname
                , liked: optional <<< uname
                , offset: optional <<< int
                , limit: optional <<< int
                }
        , "Profiles": "profiles" / uname segment
        , "Feed":
            "snippets" / "feed"
              ? { offset: optional <<< int
                , limit: optional <<< int
                }
        , "Tags": "tags" / noArgs
        }

commentId :: RouteDuplex' CommentId
commentId = _Newtype (int segment)
