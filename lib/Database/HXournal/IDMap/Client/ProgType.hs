{-# LANGUAGE DeriveDataTypeable #-}

module Database.HXournal.IDMap.Client.ProgType where 

import System.FilePath
import System.Console.CmdArgs hiding (name)

data HXournalIDMapClient = Create { config :: FilePath, modulename :: String }
                  | Get    { config :: FilePath, name :: String } 
                  | Put    { config :: FilePath, name :: FilePath, modulename :: String } 
                  | Delete { config :: FilePath, name :: String } 
                  | GetList { config :: FilePath } 
              deriving (Show,Data,Typeable)

create :: HXournalIDMapClient
create = Create { config = "test.conf"
                , modulename = "" &= typ "MODULENAME" &= argPos 0
                }

get :: HXournalIDMapClient 
get = Get { config = "test.conf" 
          , name = "" &= typ "NAME" &= argPos 0 
          } 

put :: HXournalIDMapClient 
put = Put { config = "test.conf"
          , name = "" &= typ "NAME" &= argPos 0 
          , modulename = "" &= typ "NAME" &= argPos 1
          }

delete :: HXournalIDMapClient 
delete = Delete { config = "test.conf"
                , name = "" &= typ "NAME" &= argPos 0 
                }

getlist :: HXournalIDMapClient 
getlist = GetList { config = "test.conf" } 

mode = modes [ create, get, put, delete, getlist ]

