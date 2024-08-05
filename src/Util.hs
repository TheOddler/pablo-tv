{-# LANGUAGE CPP #-}

module Util where

import Data.Default (def)
import Language.Haskell.TH.Syntax (Exp, Q)
import Yesod.Default.Util (widgetFileNoReload, widgetFileReload)

-- | This should really not be used anywhere, as the dev and prod code should be the same.
-- The only exception are some Yesod functions to automatically reload templates in development,
-- as that considerably speeds up development, so it's worth the tradeoff there.
{- ORMOLU_DISABLE -}
isDevelopment :: Bool
isDevelopment =
#ifdef DEVELOPMENT
  True
#else
  False
#endif
{- ORMOLU_ENABLE -}

widgetFile :: String -> Q Exp
widgetFile =
  if isDevelopment
    then widgetFileReload settings
    else widgetFileNoReload settings
  where
    settings = def
