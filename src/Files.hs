{-# LANGUAGE TemplateHaskell #-}

module Files
  ( DirectoryInfo (..),
    DirectoryKind (..),
    parseDirectory,
    niceFileNameT,
    -- Exports for testing
    guessDirectoryInfo,
    isVideoFile,
  )
where

import Algorithms.NaturalSort qualified as Natural
import Autodocodec
  ( HasCodec (codec),
    object,
    optionalField,
    requiredField,
    stringConstCodec,
    (.=),
  )
import Autodocodec.Codec (optionalFieldWithDefaultWith)
import Autodocodec.Yaml (eitherDecodeYamlViaCodec, encodeYamlViaCodec)
import Control.Applicative ((<|>))
import Data.ByteString qualified as BS
import Data.ByteString.Char8 qualified as BS8
import Data.Char (toLower)
import Data.List (sortBy)
import Data.List.NonEmpty (group, nonEmpty)
import Data.List.NonEmpty qualified as NE
import Data.Maybe (fromMaybe, isJust, mapMaybe)
import Data.Text (Text, breakOn, replace, strip)
import Data.Text qualified as T
import GHC.Data.Maybe (firstJusts, firstJustsM, listToMaybe, orElse)
import GHC.Exts (sortWith)
import GHC.Generics (Generic)
import Path
  ( Abs,
    Dir,
    File,
    Path,
    Rel,
    addExtension,
    dirname,
    fileExtension,
    filename,
    fromAbsDir,
    fromAbsFile,
    fromRelDir,
    fromRelFile,
    mkRelFile,
    parent,
    parseRelFile,
    splitExtension,
    toFilePath,
    (</>),
  )
import Path.IO (listDirRel, renameFile)
import System.FilePath (combine, dropTrailingPathSeparator)
import TVDB (TVDBData (..), TVDBType (..), getInfoFromTVDB)
import Text.Read (readMaybe)
import Text.Regex.TDFA ((=~))

data DirectoryInfo = DirectoryInfo
  { directoryInfoKind :: DirectoryKind,
    directoryInfoTitle :: Text,
    directoryInfoYear :: Maybe Int,
    directoryInfoDescription :: Maybe Text,
    directoryInfoImdb :: Maybe Text,
    directoryInfoTvdb :: Maybe Text,
    directoryInfoTmdb :: Maybe Text,
    directoryInfoForceUpdate :: Maybe Bool
  }
  deriving (Generic, Show, Eq)

instance HasCodec DirectoryInfo where
  codec =
    object "DirectoryInfo" $
      DirectoryInfo
        <$> requiredField "kind" "Is this a series or a movie?" .= directoryInfoKind
        <*> requiredField "title" "The title of this series or movie" .= directoryInfoTitle
        <*> optionalField' "year" "The year this series or movie was released" .= directoryInfoYear
        <*> optionalField' "description" "The description of the series or movie" .= directoryInfoDescription
        <*> optionalField' "imdb" "The IMDB ID" .= directoryInfoImdb
        <*> optionalField' "tvdb" "The TVDB ID" .= directoryInfoTvdb
        <*> optionalField' "tmdb" "The TMDB ID" .= directoryInfoTmdb
        <*> optionalField "force-update" "This forces the system to try and download more information again. This can be used for when some of the data was wrong, you can remove the incorrect data, fill in what you know, set this flag to true, and it'll use the info you gave it to search for the rest." .= directoryInfoForceUpdate
    where
      -- We use this instead of `optionalFieldOrNull` because this also writes a `null` when the field is `Nothing`
      -- that way we get `null` values in new files and they serve as better templates, but we can still
      -- remove the field from the file and they'll parse just fine.
      optionalField' key = optionalFieldWithDefaultWith key codec Nothing

data DirectoryKind
  = DirectoryKindMovie
  | DirectoryKindSeries
  deriving (Generic, Show, Eq)

instance HasCodec DirectoryKind where
  codec =
    stringConstCodec $
      (DirectoryKindMovie, "movie")
        NE.:| [ (DirectoryKindSeries, "series")
              ]

-- | The main function, gives a directory, tries to see if there's existing info,
-- and if not tries to guess it and saves it.
parseDirectory :: BS.ByteString -> Path Abs Dir -> IO (Maybe DirectoryInfo, [(Text, Path Rel File)], [(Text, Path Rel Dir)])
parseDirectory tvdbToken dir = do
  -- Read files and directories
  (dirNames, fileNames) <- listDirRel dir
  let files = smartPathSort $ filter isVideoFile fileNames
  let directories = smartPathSort dirNames

  let filesWithNames = map (\f -> (niceFileNameT f, f)) files
      directoriesWithNames = map (\d -> (niceDirNameT d, d)) directories
      -- Guess some info in case there's no info file
      mInfoGuess = guessDirectoryInfo dir files directories

  -- See if there's an info file
  mInfoFilePath <- tryGetFile (== $(mkRelFile "info.yaml")) dir
  mInfoFile <- case mInfoFilePath of
    Nothing -> pure Nothing
    Just infoFilePath -> do
      -- If there is, try and decode it
      decodedOrError <- eitherDecodeYamlViaCodec <$> BS.readFile (fromAbsFile infoFilePath)
      case decodedOrError of
        Right info -> pure $ Just (infoFilePath, info)
        Left err -> do
          -- If there's an error, move it to a backup file, we'll guess info and create a new one later
          putStrLn $ "Failed to decode info file: " ++ show err
          newName <- addExtension ".backup" infoFilePath
          renameFile infoFilePath newName
          pure Nothing

  let doUpdateWith rootDir startingInfo = do
        let tvdbType = case directoryInfoKind startingInfo of
              DirectoryKindMovie -> TVDBTypeMovie
              DirectoryKindSeries -> TVDBTypeSeries
        (usedInfo, mTVDBData) <- do
          let getInfoForYear = getInfoFromTVDB tvdbToken (directoryInfoTitle startingInfo) tvdbType
          attemptWithYear <- getInfoForYear startingInfo.directoryInfoYear
          case attemptWithYear of
            Just d -> pure (startingInfo, Just d)
            -- Fallback to not using the year in case we guessed it wrong.
            -- I had this happen when a series had a special episode that had a year in the name, but that was the year
            -- the special was made, not the series.
            Nothing | isJust startingInfo.directoryInfoYear -> do
              attemptWithoutYear <- getInfoForYear Nothing
              pure (startingInfo {directoryInfoYear = Nothing}, attemptWithoutYear)
            Nothing -> pure (startingInfo, Nothing)

        -- If we're doing a force update, that means someone corrected it manually, so we do not want to overwrite
        let keepOriginal = fromMaybe False usedInfo.directoryInfoForceUpdate
        let select infoField tvdbValue =
              if keepOriginal && isJust (infoField usedInfo)
                then infoField usedInfo
                else tvdbValue

        let extendedInfo = case mTVDBData of
              Nothing ->
                usedInfo
                  { -- Even when we don't find anything, remove this flag so we don't keep trying
                    directoryInfoForceUpdate = Nothing
                  }
              Just tvdbData ->
                DirectoryInfo
                  { directoryInfoKind = usedInfo.directoryInfoKind,
                    directoryInfoTitle = usedInfo.directoryInfoTitle,
                    directoryInfoYear = select directoryInfoYear (tvdbData.tvdbDataYear <|> usedInfo.directoryInfoYear),
                    directoryInfoDescription = select directoryInfoDescription tvdbData.tvdbDataDescription,
                    directoryInfoImdb = select directoryInfoImdb tvdbData.tvdbDataImdb,
                    directoryInfoTvdb = select directoryInfoTvdb (Just tvdbData.tvdbDataId),
                    directoryInfoTmdb = select directoryInfoTmdb tvdbData.tvdbDataTmdb,
                    directoryInfoForceUpdate = Nothing
                  }
        case tvdbDataImage =<< mTVDBData of
          Nothing -> pure ()
          Just (contentType, image) -> do
            let extension =
                  if BS.isPrefixOf "image/" contentType
                    then Just $ BS.drop 6 contentType
                    else Nothing
                fallbackName = $(mkRelFile "poster.jpg")
                name = fromMaybe fallbackName $ do
                  ext <- extension
                  parseRelFile $ BS8.unpack $ "poster." <> ext

            let path = rootDir </> name
            BS.writeFile (fromAbsFile path) image

        BS.writeFile (combine (fromAbsDir rootDir) "info.yaml") (encodeYamlViaCodec extendedInfo)
        pure (Just extendedInfo, filesWithNames, directoriesWithNames)

  case (mInfoFile, mInfoGuess) of
    (Just (root, info), _)
      | info.directoryInfoForceUpdate == Just True ->
          doUpdateWith (parent root) info
    (Just (_, info), _) -> pure (Just info, filesWithNames, directoriesWithNames)
    (Nothing, Nothing) -> pure (Nothing, filesWithNames, directoriesWithNames)
    (Nothing, Just (rootDir, info)) -> doUpdateWith rootDir info

-- | This tries to guess some information based on the directory and file names.
-- It also returns what it thinks is the root directory of this series or movie.
guessDirectoryInfo :: Path a Dir -> [Path Rel File] -> [Path Rel Dir] -> Maybe (Path a Dir, DirectoryInfo)
guessDirectoryInfo dir files directories =
  let videoFiles :: [Path Rel File]
      videoFiles = smartPathSort $ filter isVideoFile files

      isSeriesDir = any (isJust . seasonFromDir) directories

      mSeasonFromDir = seasonFromDir dir
      mSeasonFromFiles = seasonFromFiles videoFiles
      mSeason = mSeasonFromDir <|> mSeasonFromFiles

      simpleInfo kind name =
        DirectoryInfo
          { directoryInfoKind = kind,
            directoryInfoTitle = name,
            directoryInfoYear = yearFromDir dir <|> yearFromFiles videoFiles,
            directoryInfoDescription = Nothing,
            directoryInfoImdb = Nothing,
            directoryInfoTvdb = Nothing,
            directoryInfoTmdb = Nothing,
            directoryInfoForceUpdate = Nothing
          }
   in case (mSeason, nonEmpty videoFiles, nonEmpty directories) of
        (Just _season, Just _actualFiles, _) ->
          let relevantDir =
                if isJust mSeasonFromDir
                  then parent dir
                  else dir
           in Just
                ( relevantDir,
                  (simpleInfo DirectoryKindSeries $ titleFromDir relevantDir)
                    { directoryInfoYear = yearFromDir relevantDir <|> yearFromFiles videoFiles
                    }
                )
        (_, _, _)
          | isSeriesDir ->
              Just
                ( dir,
                  simpleInfo DirectoryKindSeries $ titleFromDir dir
                )
        (_, Just _actualFiles, _) ->
          Just
            ( dir,
              simpleInfo DirectoryKindMovie $ titleFromDir dir
            )
        _ -> Nothing

-- Some helpers

readInt :: String -> Maybe Int
readInt = readMaybe

tryRegex :: Path x y -> ([String] -> Maybe a) -> String -> Maybe a
tryRegex source resultParser regex =
  let res :: (String, String, String, [String])
      res = toFilePath source =~ regex
      (_, _, _, matches) = res
   in resultParser matches

expect1Int :: [String] -> Maybe Int
expect1Int = \case
  [a] ->
    readInt a
  _ -> Nothing

expect2Ints :: [String] -> Maybe (Int, Int)
expect2Ints = \case
  [a, b] ->
    (,) <$> readInt a <*> readInt b
  _ -> Nothing

expect3Ints :: [String] -> Maybe (Int, Int, Int)
expect3Ints = \case
  [a, b, c] ->
    (,,) <$> readInt a <*> readInt b <*> readInt c
  _ -> Nothing

seasonFromDir :: Path a Dir -> Maybe Int
seasonFromDir dir =
  tryRegex dir expect1Int "[Ss]eason ([0-9]+)"
    <|> tryRegex dir expect1Int "[Ss]eries ([0-9]+)"
    <|> tryRegex dir expect1Int "[Ss]eizoen ([0-9]+)"

seasonFromFiles :: [Path a File] -> Maybe Int
seasonFromFiles files =
  case mSeason of
    Just season -> Just season
    Nothing -> if looseEpisodesFound then Just 1 else Nothing
  where
    mSeason = NE.head <$> listToMaybe (sortWith NE.length $ group (mapMaybe seasonFromFile files))
    looseEpisodesFound = any (isJust . snd . episodeInfoFromFile) files

    seasonFromFile :: Path a File -> Maybe Int
    seasonFromFile file =
      tryRegex file expect1Int "[Ss]eason ([0-9]+)"
        <|> tryRegex file expect1Int "[Ss]eries ([0-9]+)"
        <|> tryRegex file expect1Int "[Ss]eizoen ([0-9]+)"
        <|> tryRegex file expect1Int "[Ss]([0-9]+)[Ee][0-9]+"
        <|> tryRegex file expect1Int "([0-9]+)[Xx][0-9]+"

episodeInfoFromFile :: Path a File -> (Maybe Int, Maybe (Either Int (Int, Int)))
episodeInfoFromFile file =
  let double :: Maybe (Int, Int, Int)
      double =
        tryRegex file expect3Ints "[Ss]([0-9]+)[Ee]([0-9]+)-[Ee]([0-9]+)"

      seasonAndEp :: Maybe (Int, Int)
      seasonAndEp =
        tryRegex file expect2Ints "[Ss]([0-9]+)[Ee]([0-9]+)"
          <|> tryRegex file expect2Ints "([0-9]+)[Xx]([0-9]+)"

      epOnly :: Maybe Int
      epOnly =
        tryRegex file expect1Int "[Ee]pisode ([0-9]+)"
          <|> tryRegex file expect1Int "[Aa]flevering ([0-9]+)"
   in case double of
        Just (s, a, b) -> (Just s, Just $ Right (a, b))
        Nothing -> case seasonAndEp of
          Just (s, a) -> (Just s, Just $ Left a)
          Nothing -> case epOnly of
            Just a -> (Nothing, Just $ Left a)
            Nothing -> (Nothing, Nothing)

yearRegex :: String
yearRegex = "((19|20)[0-9][0-9])"

yearFromDir :: Path a Dir -> Maybe Int
yearFromDir dir =
  -- Take fst because the regex parses doesn't support non-capturing groups
  -- so we capture two, but only use the first, and ignore the inner group
  fst <$> tryRegex (dirname dir) expect2Ints yearRegex

yearFromFiles :: [Path a File] -> Maybe Int
yearFromFiles files =
  firstJusts (try <$> files)
  where
    try file = fst <$> tryRegex file expect2Ints yearRegex

titleFromDir :: Path a Dir -> Text
titleFromDir folder = strip . fst $ breakOn "(" (niceDirNameT folder)

tryGetFile :: (Path Rel File -> Bool) -> Path Abs Dir -> IO (Maybe (Path Abs File))
tryGetFile predicate startDir =
  -- Look up to X levels deep
  firstJustsM $ tryGetFile' <$> take 3 (iterate parent startDir)
  where
    tryGetFile' dir = do
      (_dirNames, fileNames) <- listDirRel dir
      let matchingFileNames = filter predicate fileNames
      pure $ case listToMaybe $ smartPathSort matchingFileNames of
        Nothing -> Nothing
        Just foundFile -> Just (dir </> foundFile)

niceDirNameT :: Path a Dir -> Text
niceDirNameT = T.pack . dropTrailingPathSeparator . fromRelDir . dirname

niceFileNameT :: Path a File -> Text
niceFileNameT file =
  let withoutExt = (fst <$> splitExtension file) `orElse` file
      name = fromRelFile $ filename withoutExt
   in replace "." " " $ T.pack name

isVideoFile :: Path b File -> Bool
isVideoFile file =
  case fileExtension file of
    Just ext -> ext `elem` [".mp4", ".mkv", ".avi", ".webm"]
    Nothing -> False

-- | Sorts the paths, taking into account numbers properly
smartPathSort :: [Path Rel a] -> [Path Rel a]
smartPathSort = sortBy sorting
  where
    sorting a b =
      Natural.compare
        (toLower <$> toFilePath a)
        (toLower <$> toFilePath b)
