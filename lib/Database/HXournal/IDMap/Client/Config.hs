{-# LANGUAGE OverloadedStrings #-}

module Database.HXournal.IDMap.Client.Config where

import Control.Applicative
import Data.Configurator as C
import Data.Configurator.Types

data Hxournal-idmapClientConfiguration = Hxournal-idmapClientConfiguration { 
  hxournal-idmapServerURL :: String,
  hxournal-idmapClientURL :: String
} deriving (Show)

getHxournal-idmapClientConfiguration :: Config -> IO (Maybe Hxournal-idmapClientConfiguration)
getHxournal-idmapClientConfiguration config = do  
  s <- C.lookup config "server" 
  c <- C.lookup config "client" 
  return  (Hxournal-idmapClientConfiguration  <$> s <*> c )
