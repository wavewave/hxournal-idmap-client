{-# LANGUAGE DeriveDataTypeable #-}

module Database.HXournal.IDMap.Client.ProgType where 

import System.FilePath
import System.Console.CmdArgs hiding (name)

data Hxournal-idmap_client = Create { config :: FilePath, modulename :: String }
                  | Get    { config :: FilePath, name :: String } 
                  | Put    { config :: FilePath, name :: FilePath, modulename :: String } 
                  | Delete { config :: FilePath, name :: String } 
                  | GetList { config :: FilePath } 
              deriving (Show,Data,Typeable)

create :: Hxournal-idmap_client
create = Create { config = "test.conf"
                , modulename = "" &= typ "MODULENAME" &= argPos 0
                }

get :: Hxournal-idmap_client 
get = Get { config = "test.conf" 
          , name = "" &= typ "NAME" &= argPos 0 
          } 

put :: Hxournal-idmap_client 
put = Put { config = "test.conf"
          , name = "" &= typ "NAME" &= argPos 0 
          , modulename = "" &= typ "NAME" &= argPos 1
          }

delete :: Hxournal-idmap_client 
delete = Delete { config = "test.conf"
                , name = "" &= typ "NAME" &= argPos 0 
                }

getlist :: Hxournal-idmap_client 
getlist = GetList { config = "test.conf" } 

mode = modes [ create, get, put, delete, getlist ]

