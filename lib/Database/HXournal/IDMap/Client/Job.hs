{-# LANGUAGE OverloadedStrings #-}

module Database.HXournal.IDMap.Client.Job where

import Debug.Trace

import qualified Data.ByteString.Lazy.Char8 as C
import qualified Data.ByteString.Char8 as SC
import Data.Aeson.Types
import Data.Aeson.Encode as E
import Data.Aeson.Parser
import qualified Data.Attoparsec as A

import Network.HTTP.Types hiding (statusCode)
import Network.HTTP.Enumerator

import System.Directory 
import System.FilePath
import Unsafe.Coerce

import Database.HXournal.IDMap.Client.Config
import Database.HXournal.IDMap.Type
import Data.UUID
import Data.UUID.V5
import qualified Data.ByteString as B
import Data.Time.Clock

type Url = String 

nextUUID :: HXournalIDMapClientConfiguration -> IO UUID
nextUUID mc = do 
  let c = hxournalIDMapClientURL mc 
  t <- getCurrentTime 
  return . generateNamed namespaceURL . B.unpack . SC.pack $ c ++ "/" ++ show t 

startCreate :: HXournalIDMapClientConfiguration -> String -> IO () 
startCreate mc name = do 
  putStrLn "job started"
  cwd <- getCurrentDirectory
  let url = hxournalIDMapServerURL mc 
  uuid <- nextUUID mc
  let info = HXournalIDMapInfo { hxournal_idmap_uuid = uuid , hxournal_idmap_name = name } 
  response <- hxournalIDMapToServer url ("uploadhxournalIDMap") methodPost info
  putStrLn $ show response 


startGet :: HXournalIDMapClientConfiguration -> String -> IO () 
startGet mc idee = do 
  putStrLn $"get " ++ idee
  let url = hxournalIDMapServerURL mc 
  r <- jsonFromServer url ("hxournalIDMap" </> idee) methodGet
  putStrLn $ show r 


startPut :: HXournalIDMapClientConfiguration 
         -> String  -- ^ hxournalIDMap idee
         -> String  -- ^ hxournalIDMap name 
         -> IO () 
startPut mc idee name = do 
  putStrLn "job started"
  cwd <- getCurrentDirectory
  let url = hxournalIDMapServerURL mc 
      info = case fromString idee of 
               Nothing -> error "strange in startPut" 
               Just idee' -> HXournalIDMapInfo { hxournal_idmap_uuid = idee', hxournal_idmap_name = name }
  response <- hxournalIDMapToServer url ("hxournalidmap" </> idee) methodPut info
  putStrLn $ show response 


startDelete :: HXournalIDMapClientConfiguration -> String -> IO () 
startDelete mc idee = do 
  putStrLn "job started"
  let url = hxournalIDMapServerURL mc 
  r <- jsonFromServer url ("hxournalidmap" </> idee) methodDelete
  putStrLn $ show r 


startGetList :: HXournalIDMapClientConfiguration -> IO () 
startGetList mc = do 
  putStrLn "getlist: "
  let url = hxournalIDMapServerURL mc 
  r <- jsonFromServer url ("listhxournalidmap") methodGet
  putStrLn $ show r 


jsonFromServer :: Url -> String -> Method -> IO (Either String (Result Value))
jsonFromServer url api mthd = do 
  request <- parseUrl (url </> api)
  withManager $ \manager -> do
    let requestjson = request { 
          method = mthd,
          requestHeaders = [ ("Accept", "application/json; charset=utf-8") ] } 
    r <- httpLbs requestjson manager 
    if statusCode r == 200 
      then return . parseJson . SC.concat . C.toChunks . responseBody $ r
      else return (Left $ "status code : " ++ show (statusCode r)) 

hxournalIDMapToServer :: Url -> String -> Method -> HXournalIDMapInfo -> IO (Either String (Result Value))
hxournalIDMapToServer url api mthd mi = do 
  request <- parseUrl (url </> api)
  withManager $ \manager -> do
    let mijson = E.encode (toJSON mi)
        myrequestbody = RequestBodyLBS mijson 
    let requestjson = request 
          { method = mthd
          , requestHeaders = [ ("Accept", "application/json; charset=utf-8") ]
          , requestBody = myrequestbody } 
    r <- httpLbs requestjson manager 
    if statusCode r == 200 
      then return . parseJson . SC.concat . C.toChunks . responseBody $ r
      else return (Left $ "status code : " ++ show (statusCode r)) 

parseJson :: (FromJSON a) => SC.ByteString -> Either String (Result a)
parseJson bs =
  let resultjson = trace (SC.unpack bs) $ A.parse json bs 
  in case resultjson of 
       (A.Done rest rjson) -> return (parse parseJSON rjson)
       _                 -> Left "parseJson" 