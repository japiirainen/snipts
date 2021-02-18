-- | A capability representing the ability to save status information to some output, which could
-- | be the console, an external service like Splunk, or to the file system for testing purposes.
-- | The implementation can be freely swapped out without changing any application code besides
-- | the application monad, `Conduit.AppM`.
module App.Capability.LogMessages where

import Prelude
import App.Capability.Now (class Now)
import App.Data.Log (Log, LogReason(..), mkLog)
import Control.Monad.Trans.Class (lift)
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Halogen (HalogenM)

class
  Monad m <= LogMessages m where
  logMessage :: Log -> m Unit

-- | This instance lets us avoid having to use `lift` when we use these functions in a component.
instance logMessagesHalogenM :: LogMessages m => LogMessages (HalogenM st act slots msg m) where
  logMessage = lift <<< logMessage

-- | Log a message to given a particular `LogType`
log :: forall m. LogMessages m => Now m => LogReason -> String -> m Unit
log reason = logMessage <=< mkLog reason

-- | Log message for debugging purposes
logDebug :: forall m. LogMessages m => Now m => String -> m Unit
logDebug = log Debug

-- | Log a message to convey non-error information
logInfo :: forall m. LogMessages m => Now m => String -> m Unit
logInfo = log Info

-- | Log a message as a warning
logWarn :: forall m. LogMessages m => Now m => String -> m Unit
logWarn = log Warn

-- | Log a message as an error
logError :: forall m. LogMessages m => Now m => String -> m Unit
logError = log Error

-- | Hush a monadic action by logging the error, leaving it open why the error is being logged
logHush :: forall m a. LogMessages m => Now m => LogReason -> m (Either String a) -> m (Maybe a)
logHush reason action =
  action
    >>= case _ of
        Left e -> case reason of
          Debug -> logDebug e *> pure Nothing
          Info -> logInfo e *> pure Nothing
          Warn -> logWarn e *> pure Nothing
          Error -> logError e *> pure Nothing
        Right v -> pure $ Just v

-- | Hush a monadic action by loggin the error in debug mode
debugHush :: forall m a. LogMessages m => Now m => m (Either String a) -> m (Maybe a)
debugHush = logHush Debug
