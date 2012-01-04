module Database.HXournal.IDMap.Client.Command where

import Database.HXournal.IDMap.Client.ProgType
import Database.HXournal.IDMap.Client.Job
import Database.HXournal.IDMap.Client.Config
import Data.Configurator

commandLineProcess :: HXournalIDMapClient -> IO ()
commandLineProcess (Create cfg mn) = do 
  putStrLn "create called"
  mc <- getHXournalIDMapClientConfiguration =<< load [Required cfg] 
  maybe (error "cannot parse config") (\x -> startCreate x mn Nothing) mc
commandLineProcess (Get cfg n) = do 
  putStrLn "get called"
  mc <- getHXournalIDMapClientConfiguration =<< load [Required cfg] 
  maybe (error "cannot parse config") (flip startGet n) mc 
{-
commandLineProcess (Put cfg n mn) = do 
  putStrLn "put called"
  mc <- getHXournalIDMapClientConfiguration =<< load [Required cfg] 
  maybe (error "cannot parse config") (\c-> startPut c n mn) mc
commandLineProcess (Delete cfg n) = do 
  putStrLn "delete called"
  mc <- getHXournalIDMapClientConfiguration =<< load [Required cfg] 
  maybe (error "cannot parse config") (flip startDelete n) mc
-}
commandLineProcess (GetList cfg) = do 
  putStrLn "getlist called"
  mc <- getHXournalIDMapClientConfiguration =<< load [Required cfg] 
  maybe (error "cannot parse config") startGetList mc
commandLineProcess (CreateWithFile cfg fn) = do 
  putStrLn "create called"
  mc <- getHXournalIDMapClientConfiguration =<< load [Required cfg] 
  maybe (error "cannot parse config") (\x -> startCreateWithFile x fn) mc
