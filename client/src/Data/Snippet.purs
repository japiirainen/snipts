module App.Data.Snippet where

import App.Data.PaginatedArray (PaginatedArray)
import App.Data.PreciseDateTime as PDT
import App.Data.Profile (Author)
import App.Data.Profile as Profile
import App.Data.Username (Username)
import Data.Codec ((>~>))
import Data.Codec.Argonaut (JsonCodec)
import Data.Codec.Argonaut as CA
import Data.Codec.Argonaut.Migration as CAM
import Data.Codec.Argonaut.Record as CAR
import Data.Maybe (Maybe)
import Data.PreciseDateTime (PreciseDateTime)
import Slug (Slug)
import Slug as Slug
import Type.Row (type (+))

type SnippetRep row
  = ( title :: String
    , description :: String
    , content :: String
    | row
    )

type SnippetMetaDataRep row
  = ( createdOn :: PreciseDateTime
    , author :: Author
    , liked :: Boolean
    , likesCount :: Int
    | row
    )

type Snippet
  = { | SnippetRep () }

snippetCodec :: JsonCodec Snippet
snippetCodec =
  CAR.object "Snippet"
    { title: CA.string
    , description: CA.string
    , content: CA.string
    }

type SnippetWithMetaData
  = { | SnippetRep + SnippetMetaDataRep () }

snippetWithMetadataCodec :: Maybe Username -> JsonCodec SnippetWithMetaData
snippetWithMetadataCodec mbUsername =
  CAR.object "SnippetWithMetaData"
    { title: CA.string
    , content: CA.string
    , description: CA.string
    , createdOn: PDT.codec
    , liked: CA.boolean
    , likesCount: CA.int
    , author: Profile.authorCodec mbUsername
    }

snippetsWithMetaDataCodec :: Maybe Username -> JsonCodec (PaginatedArray SnippetWithMetaData)
snippetsWithMetaDataCodec mbUsername =
  CAM.renameField "snippets" "body" >~> CAM.renameField "snippetsCount" "total"
    >~> codec
  where
  codec =
    CAR.object "PaginatedArray SnippetWithMetaData"
      { body: CA.array (snippetWithMetadataCodec mbUsername)
      , total: CA.int
      }
