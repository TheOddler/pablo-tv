{-# OPTIONS_GHC -Wno-orphans #-}

module Orphanage () where

instance MonadFail (Either String) where
  fail = Left
