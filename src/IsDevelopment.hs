{-# LANGUAGE CPP #-}

module IsDevelopment where

import Language.Haskell.TH.Syntax (Exp, Q)

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
