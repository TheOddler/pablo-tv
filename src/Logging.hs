module Logging where

import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Trans.Class (MonadTrans (..))
import Control.Monad.Trans.Reader (ReaderT (..), ask)
import Data.ByteString qualified as BS
import Data.ByteString.UTF8 qualified as BS
import UnliftIO (MonadUnliftIO)
import Yesod.WebSockets (WebSocketsT)

data LogLevel
  = Debug
  | Info
  | Warning
  | Error
  deriving (Eq, Ord)

class Logger m where
  putLogBS :: LogLevel -> BS.ByteString -> m ()

putLog :: (Logger m) => LogLevel -> String -> m ()
putLog level = putLogBS level . BS.fromString

logLevelColour :: LogLevel -> BS.ByteString
logLevelColour = \case
  Debug -> "\ESC[34m" -- Blue
  Info -> "\ESC[36m" -- Cyan
  Warning -> "\ESC[33m" -- Yellow
  Error -> "\ESC[31m" -- Red

defaultColour :: BS.ByteString
defaultColour = "\ESC[0m"

-- | Use putStr from the bytestring module as that seems to be concurrency safe?
-- Don't use putStrLn as that isn't. Instead append the newline, even though that requires more memory.
putLogIO :: (MonadIO m) => LogLevel -> BS.ByteString -> m ()
putLogIO level msg =
  liftIO . BS.putStr $
    mconcat
      [ defaultColour,
        BS.fromString "➫ ", -- This ensure correct encoding of the arrow
        logLevelColour level,
        msg,
        defaultColour,
        "\n"
      ]

newtype LoggerT m a = LoggerT {unLoggerT :: ReaderT (LogLevel -> BS.ByteString -> m ()) m a}
  deriving (Functor, Applicative, Monad, MonadIO, MonadUnliftIO)

instance (Monad m) => Logger (LoggerT m) where
  putLogBS lvl msg = LoggerT $ do
    logFunc <- ask
    lift $ logFunc lvl msg

instance (Monad m, Logger m) => Logger (WebSocketsT m) where
  putLogBS lvl msg = do
    lift $ putLogBS lvl msg

runLoggerT :: (MonadIO m) => LoggerT m a -> m a
runLoggerT loggerT = runReaderT (unLoggerT loggerT) putLogIO
