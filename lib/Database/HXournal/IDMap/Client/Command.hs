-----------------------------------------------------------------------------
-- |
-- Module      : Database.HXournal.IDMap.Client.Communication.Common
-- Copyright   : (c) 2012 Ian-Woo Kim
--
-- License     : BSD3
-- Maintainer  : Ian-Woo Kim <ianwookim@gmail.com>
-- Stability   : experimental
-- Portability : GHC
--
-----------------------------------------------------------------------------

module Database.HXournal.IDMap.Client.Command where

import Database.HXournal.IDMap.Client.ProgType hiding (test,get)
import Database.HXournal.IDMap.Client.Job
import Database.HXournal.IDMap.Client.Config
import Data.Configurator

commandLineProcess :: HXournalIDMapClient -> IO ()
commandLineProcess (Test cfg) = do 
  putStrLn "test called"
  mc <- getHXournalIDMapClientConfiguration =<< load [Required cfg] 
  maybe (error "cannot parse config") test mc 
commandLineProcess (Get cfg n) = do 
  putStrLn "get called"
  mc <- getHXournalIDMapClientConfiguration =<< load [Required cfg] 
  maybe (error "cannot parse config") (flip get n) mc 
commandLineProcess (GetList cfg) = do 
  putStrLn "getlist called"
  mc <- getHXournalIDMapClientConfiguration =<< load [Required cfg] 
  maybe (error "cannot parse config") getList mc
commandLineProcess (CreateWithFile cfg fn) = do 
  putStrLn "create called"
  mc <- getHXournalIDMapClientConfiguration =<< load [Required cfg] 
  maybe (error "cannot parse config") (\x -> createWithFile x fn) mc
commandLineProcess (GetListWithTime cfg t1 t2) = do 
  putStrLn "getlist called"
  mc <- getHXournalIDMapClientConfiguration =<< load [Required cfg] 
  maybe (error "cannot parse config") (\x -> getListWithTime x t1 t2) mc





{-
commandLineProcess (Create cfg mn) = do 
  putStrLn "create called"
  mc <- getHXournalIDMapClientConfiguration =<< load [Required cfg] 
  maybe (error "cannot parse config") (\x -> startCreate x mn Nothing) mc
-}
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
