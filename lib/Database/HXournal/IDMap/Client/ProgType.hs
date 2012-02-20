{-# LANGUAGE DeriveDataTypeable #-}

module Database.HXournal.IDMap.Client.ProgType where 

import System.FilePath
import System.Console.CmdArgs hiding (name)

data HXournalIDMapClient = 
                    Test { config :: FilePath } 
                  | Create { config :: FilePath, modulename :: String }
                  | Get    { config :: FilePath, name :: String } 
                  | Put    { config :: FilePath, name :: FilePath, modulename :: String } 
                  | Delete { config :: FilePath, name :: String } 
                  | GetList { config :: FilePath } 
                  | CreateWithFile { config :: FilePath, filename :: FilePath } 
                  | GetListWithTime { config :: FilePath, time1 :: String, time2 :: String }  
              deriving (Show,Data,Typeable)


-- | 

test :: HXournalIDMapClient 
test = Test { config = "test.conf" } 

-- | 

get :: HXournalIDMapClient 
get = Get { config = "test.conf" 
          , name = "" &= typ "NAME" &= argPos 0 
          } 

-- | 

getlist :: HXournalIDMapClient 
getlist = GetList { config = "test.conf" } 

-- | 

getlistwithtime :: HXournalIDMapClient
getlistwithtime = GetListWithTime { config = "test.conf"
                                  , time1 = "" &= typ "TIMESTART" &= argPos 0
                                  , time2 = "" &= typ "TIMEEND" &= argPos 1 
                                  }

-- |

createwithfile :: HXournalIDMapClient 
createwithfile = CreateWithFile { config = "test.conf"
                                , filename = "" &= typ "FILENAME" &= argPos 0 } 

-- | 

mode = modes [ test, get, getlist, createwithfile, getlistwithtime ]

{- create, -} 
 {-  put, delete, -}

{- create :: HXournalIDMapClient
create = Create { config = "test.conf"
                , modulename = "" &= typ "MODULENAME" &= argPos 0
                } -}

{-
put :: HXournalIDMapClient 
put = Put { config = "test.conf"
          , name = "" &= typ "NAME" &= argPos 0 
          , modulename = "" &= typ "NAME" &= argPos 1
          }

delete :: HXournalIDMapClient 
delete = Delete { config = "test.conf"
                , name = "" &= typ "NAME" &= argPos 0 
                }
-}
