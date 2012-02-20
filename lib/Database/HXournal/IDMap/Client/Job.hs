{-# LANGUAGE OverloadedStrings #-}

-----------------------------------------------------------------------------
-- |
-- Module      : Database.HXournal.IDMap.Client.Job
-- Copyright   : (c) 2012 Ian-Woo Kim
--
-- License     : BSD3
-- Maintainer  : Ian-Woo Kim <ianwookim@gmail.com>
-- Stability   : experimental
-- Portability : GHC
--
-----------------------------------------------------------------------------

module Database.HXournal.IDMap.Client.Job where


import System.Directory 
import System.FilePath
import System.Posix.Files 
import System.Process

import Network.HTTP.Types hiding (statusCode)
import Network.HTTP.Enumerator
import qualified Data.ByteString.Char8 as SC


import Data.UUID
import Data.UUID.V5
import qualified Data.ByteString as B
import Data.Time.Clock
import Data.Time.Clock.POSIX

import Database.HXournal.Store.Config
import Database.HXournal.Store.Job

import Database.HXournal.IDMap.Type

import Database.HXournal.IDMap.Client.Config
import Database.HXournal.IDMap.Client.Communication.Common




-- | 

nextUUID :: HXournalIDMapClientConfiguration -> IO UUID
nextUUID mc = do 
  let c = hxournalIDMapClientURL mc 
  t <- getCurrentTime 
  return . generateNamed namespaceURL . B.unpack . SC.pack $ c ++ "/" ++ show t 



-- | 

test :: HXournalIDMapClientConfiguration 
          -> IO () 
test mc = do 
  putStrLn "test"
  uuid <- nextUUID mc
  let fname ="test"
  let url = hxournalIDMapServerURL mc 
  utctime <- getCurrentTime 
  let npages = 0 
  let info = HXournalIDMapInfo { hxournal_idmap_uuid = uuid 
                               , hxournal_idmap_name = fname 
                               , hxournal_idmap_creationtime = utctime
                               , hxournal_idmap_currentversion = 0
                               , hxournal_idmap_numofpages = npages 
                               } 
  response <- postJsonWithFile (url </> "test") info "test.txt"
  putStrLn $ show response 


{-  cwd <- getCurrentDirectory
  let fname = cwd </> fname'
  let url = hxournalIDMapServerURL mc 
  b <- doesFileExist fname 
  if not b 
    then error "no such file"
    else do
      npages <- startAdd (toString uuid) fname 
      fstatus <- getFileStatus fname  
      let etime = modificationTime fstatus 
          utctime = posixSecondsToUTCTime (realToFrac etime)
-}


-- | 

createWithFile :: HXournalIDMapClientConfiguration 
                    -> FilePath
                    -> IO () 
createWithFile mc fname = do 
  let url = hxournalIDMapServerURL mc 
      curl = hxournalIDMapCurlPath mc

  r <- curlFilePost mc "test" fname 

  putStrLn (show r )

  
  

-- | 

get :: HXournalIDMapClientConfiguration -> String -> IO () 
get mc idee = do 
  putStrLn $"get " ++ idee
  let url = hxournalIDMapServerURL mc 
  r <- jsonFromServer url ("hxournalidmap" </> idee) methodGet
  putStrLn $ show r 

-- | 

getList :: HXournalIDMapClientConfiguration -> IO () 
getList mc = do 
  putStrLn "getlist: "
  let url = hxournalIDMapServerURL mc 
  r <- jsonFromServer url ("listhxournalidmap") methodGet
  putStrLn $ show r 

-- |

getListWithTime :: HXournalIDMapClientConfiguration -> String -> String -> IO ()
getListWithTime mc t1 t2 = do 
  putStrLn "getlistwithtime"
  let url = hxournalIDMapServerURL mc 
  r <- jsonFromServer url ("listhxournalidmap" </> t1 </> t2) methodGet
  putStrLn $ show r 
  
