-- | This module defines a type to represent well-formed logs which can be written to an external
-- | logging service.
module App.Data.Log (LogReason(..), message, reason, Log, mkLog) where

import Prelude
import App.Capability.Now (class Now, nowDateTime)
import Data.DateTime (DateTime)
import Data.Either (either)
import Data.Foldable (fold)
import Data.Formatter.DateTime (formatDateTime)

data LogReason
  = Debug
  | Info
  | Warn
  | Error

derive instance eqLogReason :: Eq LogReason

derive instance ordLogReason :: Ord LogReason

newtype Log
  = Log
  { reason :: LogReason
  , timestamp :: DateTime
  , message :: String
  }

derive instance eqLog :: Eq Log

-- | We have been careful to prevent creation of the `Log` type outside this module, but we should
-- | still be able to read the fields within. In other words, the type is read-only.
-- |
-- | This helper function retrieves the well-formed message from a `Log`.
message :: Log -> String
message (Log { message: m }) = m

-- | This helper function retrieves the reason a log was produced from a `Log`.
reason :: Log -> LogReason
reason (Log { reason: r }) = r

mkLog :: ∀ m. Now m => LogReason -> String -> m Log
mkLog logReason inputMessage = do
  now <- nowDateTime
  let
    headerWith start = fold [ "[", start, ": ", formatTimestamp now, "]\n", inputMessage ]

    formattedLog =
      headerWith case logReason of
        Debug -> "DEBUG"
        Info -> "WARNING"
        Warn -> "WARNING"
        Error -> "ERROR"
  pure $ Log { reason: logReason, timestamp: now, message: formattedLog }
  where
  formatTimestamp =
    either (const "(failed to assign time)") identity
      <<< formatDateTime "yyyy-dd-mm hh:mm:ss a"
