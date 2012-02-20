{-# LANGUAGE OverloadedStrings #-}

-----------------------------------------------------------------------------
-- |
-- Module      : Database.HXournal.IDMap.Client.Config
-- Copyright   : (c) 2012 Ian-Woo Kim
--
-- License     : BSD3
-- Maintainer  : Ian-Woo Kim <ianwookim@gmail.com>
-- Stability   : experimental
-- Portability : GHC
--
-----------------------------------------------------------------------------

module Database.HXournal.IDMap.Client.Config where

import Control.Applicative
import Data.Configurator as C
import Data.Configurator.Types

data HXournalIDMapClientConfiguration = HXournalIDMapClientConfiguration { 
  hxournalIDMapServerURL :: String,
  hxournalIDMapClientURL :: String, 
  hxournalIDMapCurlPath :: FilePath
} deriving (Show)

getHXournalIDMapClientConfiguration :: Config -> IO (Maybe HXournalIDMapClientConfiguration)
getHXournalIDMapClientConfiguration config = do  
  s <- C.lookup config "server" 
  c <- C.lookup config "client" 
  curl <- C.lookup config "curl"
  return  (HXournalIDMapClientConfiguration  <$> s <*> c <*> curl )
