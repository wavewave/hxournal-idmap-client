{-# LANGUAGE OverloadedStrings #-}

module Database.HXournal.IDMap.Client.Config where

import Control.Applicative
import Data.Configurator as C
import Data.Configurator.Types

data HXournalIDMapClientConfiguration = HXournalIDMapClientConfiguration { 
  hxournalIDMapServerURL :: String,
  hxournalIDMapClientURL :: String
} deriving (Show)

getHXournalIDMapClientConfiguration :: Config -> IO (Maybe HXournalIDMapClientConfiguration)
getHXournalIDMapClientConfiguration config = do  
  s <- C.lookup config "server" 
  c <- C.lookup config "client" 
  return  (HXournalIDMapClientConfiguration  <$> s <*> c )
