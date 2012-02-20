{-# LANGUAGE OverloadedStrings #-}

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

module Database.HXournal.IDMap.Client.Communication.Common where

import qualified Data.ByteString.Lazy.Char8 as C
import qualified Data.ByteString.Char8 as SC
import Network.HTTP.Types hiding (statusCode)
import Network.HTTP.Enumerator
import Data.Aeson.Types
import Data.Aeson.Parser
import Blaze.ByteString.Builder
import System.FilePath
import System.Process

import qualified Data.Attoparsec as A
import Data.Aeson.Encode as E

import Debug.Trace
import Database.HXournal.IDMap.Type
import Database.HXournal.IDMap.Client.Communication.Multipart
import Database.HXournal.IDMap.Client.Config

-- | 

type Url = String 

-- |

postStuff :: Url -> RequestHeaders -> RequestBody IO -> IO (Either String (Result Value))
postStuff url rqheaders rqbody = do 
    request <- parseUrl url 
    withManager $ \manager -> do
      let requestjson = request 
            { method = methodPost 
            , requestHeaders = rqheaders --  [ ("Accept", "application/json; charset=utf-8") ]
            , requestBody = rqbody }  --  myrequestbody } 
      case rqbody of 
        RequestBodyLBS test ->  putStrLn $ (C.unpack test) 
        _ -> return ()
      r <- httpLbs requestjson manager 
      let rpbody = responseBody r  
      putStrLn $ "rpbody = " ++ show rpbody 
      if statusCode r == 200 
        then return . parseJson . SC.concat . C.toChunks . responseBody $ r
        else return (Left $ "status code : " ++ show (statusCode r))  
      -- return (Left "error")

-- | 

postJson :: (ToJSON a) => Url -> a -> IO (Either String (Result Value))
postJson url obj = postStuff url [("Accept", "application/json; charset=utf-8")] myrequestbody  
  where objjson = E.encode (toJSON obj)
        myrequestbody = RequestBodyLBS objjson

-- | 

postJsonWithFile :: (ToJSON a) => Url -> a -> FilePath -> IO (Either String (Result Value))
postJsonWithFile url obj fp = do 
    objtext <- C.readFile fp 
    let combined = toLazyByteString (multipart "aAbBcC" 
                     [ OnePartJson "json" objjson
                     , OnePartTextFile "f2" "test.txt" objtext])
        myrequestbody = RequestBodyLBS combined
    postStuff url headers myrequestbody
  where headers = [ ("Accept", "application/json; charset=utf-8")
                  , ("Content-Type", "multipart/form-data; boundary=AaB03x")
                  ] 
        objjson = E.encode (toJSON obj)




-- | 
{-
  request <- parseUrl url 
  withManager $ \manager -> do
    let objjson = E.encode (toJSON obj)
        myrequestbody = RequestBodyLBS objjson
    let requestjson = request 
          { method = methodPost 
          , requestHeaders = [ ("Accept", "application/json; charset=utf-8") ]
          , requestBody = myrequestbody } 
    putStrLn $ show objjson
    r <- httpLbs requestjson manager 
    let rbody = responseBody r  
    print rbody 
    if statusCode r == 200 
      then return . parseJson . SC.concat . C.toChunks . responseBody $ r
      else return (Left $ "status code : " ++ show (statusCode r)) 

-}

{-

  request <- parseUrl url 
  withManager $ \manager -> do
    let requestjson = request 
          { method = methodPost 
          , requestHeaders = [ ("Accept", "application/json; charset=utf-8") ]
          , requestBody = myrequestbody } 
    putStrLn $ show objjson
    r <- httpLbs requestjson manager 
    let rbody = responseBody r  
    print rbody 
    if statusCode r == 200 
      then return . parseJson . SC.concat . C.toChunks . responseBody $ r
      else return (Left $ "status code : " ++ show (statusCode r)) 

-}

-- | 

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
    putStrLn $ show mijson
    r <- httpLbs requestjson manager 
    if statusCode r == 200 
      then return . parseJson . SC.concat . C.toChunks . responseBody $ r
      else return (Left $ "status code : " ++ show (statusCode r)) 

-- |

parseJson :: (FromJSON a) => SC.ByteString -> Either String (Result a)
parseJson bs =
  let resultjson = trace (SC.unpack bs) $ A.parse json bs 
  in case resultjson of 
       (A.Done rest rjson) -> return (parse parseJSON rjson)
       _                 -> Left "parseJson" 

-- | 

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


-- |

curlFilePost :: HXournalIDMapClientConfiguration 
             -> Url 
             -> FilePath 
             -> IO (Either String (Result Value))
curlFilePost mc cmd fp = do 
  let url = hxournalIDMapServerURL mc 
      curl = hxournalIDMapCurlPath mc
  --readProcess :: FilePath -> [String] -> String -> IO String
  sout <- readProcess curl [ "--form", "f2=@" ++ fp, "--form"
                           , "press=\"Submit\""
                           , "-H", "Accept: application/json" 
                           , url </> cmd ] ""
  putStrLn $ " sout = " ++ sout
  return (parseJson . SC.pack  $ sout)
