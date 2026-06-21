{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module LibMain where

import Actions
  ( OverwritePreviouslyWatched (..),
    markDirOrFileAsWatched,
    mkInputDevice,
  )
import Control.Applicative (asum)
import Control.Exception (throwIO)
import Control.Monad (when)
import Control.Monad.IO.Class (MonadIO (..))
import Control.Monad.Trans.Reader (ask)
import Data.List.Extra (intercalate, stripPrefix)
import Data.Map.Strict qualified as Map
import Data.Maybe (fromMaybe, mapMaybe)
import Data.Text qualified as T
import Data.Time.Clock.POSIX (getPOSIXTime)
import Directory
  ( getMemoryFileDir,
    loadRootsFromDisk,
    memoryFileName,
    saveRootsToDisk,
  )
import Directory.Directories
  ( DirectoryName (..),
    RootDirectories (..),
    RootDirectoryData (..),
    RootDirectoryLocation (..),
    rootDirectoryLocationToAbsPath,
  )
import Directory.Files
  ( VideoFileName (..),
  )
import Directory.Paths
  ( VideoFilePath (..),
  )
import Env (ServerEnv (..))
import GHC.Conc (TVar, atomically, newTVarIO, writeTVar)
import GHC.Data.Maybe (firstJustsM)
import GHC.Exception (errorCallException)
import IsDevelopment (isDevelopment)
import Logging (LogLevel (..), Logger, mkLogSlidingWindow, putLog)
import Mpris (MediaPlayer, getFirstMediaPlayer, mediaListener)
import PVar (PVar, modifyPVar_, newPVar)
import SafeIO (SafeIO, catchAny, unsafePinkyPromiseThisIsSafe)
import Samba (MountResult, SmbServer (..), SmbShare (..), mount)
import Server qualified as Servant
import System.Directory (XdgDirectory (..), createDirectoryIfMissing, doesDirectoryExist, getCurrentDirectory, getXdgDirectory, renameFile)
import System.Environment (getArgs)
import System.FilePath (pathSeparator, (</>))
import System.IO (BufferMode (..), hSetBuffering, stderr, stdout)
import System.Process (callProcess)
import TVState (startingTVState)
import Transformers (LoggerT (..), SafeIOT (..), runLoggerT)
import Util
  ( logOnErrorIO,
    showT,
    unsnocNE,
  )
import Util.DirPath (absPathQQ, relPathQQ)
import Util.TextWithoutSeparator (splitAtSeparatorNE)

mountAllSambaShares :: (SafeIO m, Logger m) => RootDirectories -> m ()
mountAllSambaShares roots = do
  let sambaShares =
        mapMaybe
          ( \case
              RootRelToHome _ -> Nothing
              RootAbsPath _ -> Nothing
              RootSamba srv shr -> Just (srv, shr)
          )
          (Map.keys roots.unRootDirectories)
  results <- mapM (uncurry mount) sambaShares
  let showResult :: ((SmbServer, SmbShare), MountResult) -> String
      showResult ((SmbServer srv, SmbShare shr), result) =
        srv ++ "/" ++ shr ++ ": " ++ show result
  putLog Info $
    "Mounted sambas:\n\t"
      ++ intercalate "\t\n" (showResult <$> zip sambaShares results)

mediaListenerHandler :: (SafeIO m, Logger m) => PVar RootDirectories -> FilePath -> m ()
mediaListenerHandler rootDirsPVar absFilePath = do
  modifyPVar_ rootDirsPVar ("Media listener handler: " <> showT absFilePath) $ \roots -> do
    let tryRoot rootLoc = do
          rootAbsPath <- rootDirectoryLocationToAbsPath rootLoc
          let mRest = stripPrefix (rootAbsPath ++ [pathSeparator]) absFilePath
          case mRest of
            Nothing -> pure Nothing
            Just rest -> do
              let pathPieces = splitAtSeparatorNE $ T.pack rest
              let (dirNames', fileName) = unsnocNE pathPieces
              let dirNames = DirectoryName <$> dirNames'
              -- We'll pretend that this filename is always a video file, if not we just won't find the file and not update it, so that's fine too.
              let videoFileName = VideoFileName fileName
              pure $ Just $ VideoFilePath rootLoc dirNames videoFileName
    mPath <- firstJustsM $ tryRoot <$> Map.keys roots.unRootDirectories
    case mPath of
      Nothing -> do
        putLog Info $ "Path did not match any known files, so didn't mark any file as watched: " ++ absFilePath
        pure roots
      Just path -> do
        putLog Info $ "Marking file as watched: " ++ show path
        markDirOrFileAsWatched OverwritePreviouslyWatched (Right path) roots
  saveRootsToDisk rootDirsPVar

playerListenerHandler :: (MonadIO m) => TVar (Maybe MediaPlayer) -> MediaPlayer -> m ()
playerListenerHandler lastActivePlayerTVar playerName = do
  liftIO $ atomically $ writeTVar lastActivePlayerTVar $ Just playerName

main :: IO ()
main = do
  args <- getArgs
  let minLogLevel =
        if "--log-debug" `elem` args
          then Debug
          else Info
  let logBuffering =
        -- By default Haskell does block buffering, but I want line buffering by default
        if "--log-block-buffering" `elem` args
          then BlockBuffering Nothing
          else LineBuffering

  -- Read the static folder path
  let staticArg = "--frontend="
  frontendDir <- case asum (stripPrefix staticArg <$> args) of
    Nothing -> throwIO . errorCallException $ "Missing " ++ staticArg ++ " argument"
    Just "" -> throwIO . errorCallException $ "Empty " ++ staticArg ++ " argument"
    Just path -> do
      exists <- doesDirectoryExist path
      when (not exists) $ do
        curDir <- getCurrentDirectory
        throwIO . errorCallException $
          "Given static folder (" ++ path ++ ") does not exist. Server was started in: " ++ curDir
      pure path

  logWindow <- mkLogSlidingWindow 1000
  runLoggerT logWindow minLogLevel . runSafeIOT $ do
    logOnErrorIO Error ("setting log buffering stdout to " ++ show logBuffering) $
      hSetBuffering stdout logBuffering
    logOnErrorIO Error ("setting log buffering stdout to " ++ show logBuffering) $
      hSetBuffering stderr logBuffering

    putLog Info $ "Terminal arguments: " ++ show args
    putLog Info $ "Min log level: " ++ show minLogLevel

    -- Log some info
    let port = 8080
    let url = "http://localhost:" ++ show port
    putLog Info $ "Running on port " ++ show port ++ " - " ++ url
    putLog Info $ "Development mode: " ++ show isDevelopment

    memoryFileDir <- getMemoryFileDir
    putLog Info $ "Memory file(s) directory: " ++ memoryFileDir
    logOnErrorIO Error ("Creating memory file(s) directory " ++ memoryFileDir) $
      createDirectoryIfMissing True memoryFileDir

    -- Move the memory file to the new location if needed.
    -- TODO: Remove this again once I no longer need to support the old location
    old <- unsafePinkyPromiseThisIsSafe $ (</> memoryFileName) <$> getXdgDirectory XdgData ""
    new <- (</> memoryFileName) <$> getMemoryFileDir
    renameFile old new `catchAny` \_ -> pure ()

    mRootDirs <- loadRootsFromDisk
    let emptyRootData = RootDirectoryData Map.empty Map.empty
    let rootDirsDefault =
          RootDirectories $
            Map.fromList
              [ (RootRelToHome [relPathQQ|Videos|], emptyRootData),
                (RootAbsPath [absPathQQ|/home/pablo/Downloads/Torrents|], emptyRootData),
                -- TODO: I'll have to add an interface somewhere to add these
                (RootSamba (SmbServer "192.168.0.99") (SmbShare "videos"), emptyRootData)
              ]
    let rootDirs = fromMaybe rootDirsDefault mRootDirs
    rootDirsPVar <- newPVar rootDirs

    -- Open samba shares. TODO: Make some way of checking and re-mounting the broken ones at runtime
    mountAllSambaShares rootDirs

    -- Get the log func
    (logFunc, _) <- SafeIOT $ LoggerT ask

    -- Create input device
    inputDevice <- mkInputDevice

    -- Create empty tv state
    tvState <- liftIO $ newTVarIO startingTVState

    -- Get Mpris stuff
    player <- getFirstMediaPlayer
    lastActivePlayerTVar <- liftIO $ newTVarIO player

    -- Get start time
    now <- unsafePinkyPromiseThisIsSafe getPOSIXTime

    -- The thread for the app
    let app =
          ServerEnv
            { envPort = port,
              envLogFunc = logFunc,
              envLogSlidingWindow = logWindow,
              envInputDevice = inputDevice,
              envStartTime = now,
              envTVState = tvState,
              envLastActivePlayer = lastActivePlayerTVar,
              envRootDirs = rootDirsPVar,
              envFrontend = frontendDir
            }
    let servantAppThread = Servant.main app

    -- The thread that'll be listening for files being played, and marking them as watched
    -- This function returns instantly, but in the background it's still running, so no need to race
    _ <-
      mediaListener
        (mediaListenerHandler rootDirsPVar)
        (playerListenerHandler lastActivePlayerTVar)

    -- Only open the browser automatically in production because it's annoying in
    -- development as it opens a new tab every time the server restarts.
    -- Do this as late as possible, because if we're too fast the server won't actually respond yet.
    when (not isDevelopment) . logOnErrorIO Warning "opening pablo-tv home" $
      callProcess "xdg-open" [url]

    putLog Info "Starting server..."
    liftIO servantAppThread

    putLog Debug "Server quit."
