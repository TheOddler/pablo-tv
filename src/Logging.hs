module Logging where

import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.ByteString qualified as BS
import Data.ByteString.UTF8 qualified as BS

data LogLevel
  = Debug
  | Info
  | Warning
  | Error

putLog :: (MonadIO m) => LogLevel -> String -> m ()
putLog level msg =
  -- Use putStr from the bytestring module as that seems to be concurrency safe?
  -- Don't use putStrLn as that isn't. Instead append the newline, even though that requires more memory.
  liftIO . BS.putStr . BS.fromString $
    concat
      [ "\ESC[0m🭺🭺🭺\n",
        case level of
          Debug -> "\ESC[34m" -- Blue
          Info -> "\ESC[36m" -- Cyan
          Warning -> "\ESC[33m" -- Yellow
          Error -> "\ESC[31m", -- Red
        msg,
        "\ESC[0m\n🭷🭷🭷\n"
      ]
