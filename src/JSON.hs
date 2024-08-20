module JSON where

import Data.Aeson (Options (..), defaultOptions)
import Data.Char (toLower)

prefixedDefaultOptions :: Int -> Options
prefixedDefaultOptions n =
  defaultOptions
    { fieldLabelModifier = lowerFirst . drop n,
      constructorTagModifier = lowerFirst . drop n
    }
  where
    lowerFirst "" = ""
    lowerFirst (x : xs) = toLower x : xs
