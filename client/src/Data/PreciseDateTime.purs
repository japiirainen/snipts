module App.Data.PreciseDateTime where

import Prelude
import Data.Codec.Argonaut (JsonCodec)
import Data.Codec.Argonaut as CA
import Data.Formatter.DateTime (FormatterCommand(..), format)
import Data.List (fromFoldable)
import Data.Newtype (unwrap)
import Data.PreciseDateTime (PreciseDateTime)
import Data.PreciseDateTime as PDT
import Data.RFC3339String (RFC3339String(..))

codec :: JsonCodec PreciseDateTime
codec = CA.prismaticCodec from to CA.string
  where
  from = PDT.fromRFC3339String <<< RFC3339String

  to = unwrap <<< PDT.toRFC3339String

-- | Display a human-readable version of the precise datetime, as described in the Conduit spec
-- |
-- | Example: "Wed Nov 5, 1999"-- | Display a human-readable version of the precise datetime, as described in the Conduit spec
toDisplayWeekName :: PreciseDateTime -> String
toDisplayWeekName = PDT.toDateTimeLossy >>> format dateFormatter
  where
  dateFormatter =
    fromFoldable
      [ MonthFull
      , Placeholder " "
      , DayOfMonth
      , Placeholder ", "
      , YearFull
      ]

-- | An alternate way to display a human-readable version of the precise datetime
-- |
-- | Example: "November 5, 1999"
toDisplayMonthDayYear :: PreciseDateTime -> String
toDisplayMonthDayYear = PDT.toDateTimeLossy >>> format dateFormatter
  where
  dateFormatter =
    fromFoldable
      [ MonthFull
      , Placeholder " "
      , DayOfMonth
      , Placeholder ", "
      , YearFull
      ]
