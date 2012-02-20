{-# LANGUAGE OverloadedStrings #-}

-----------------------------------------------------------------------------
-- |
-- Module      : Database.HXournal.IDMap.Client.Communication.Multipart
-- Copyright   : (c) 2012 Ian-Woo Kim
--
-- License     : BSD3
-- Maintainer  : Ian-Woo Kim <ianwookim@gmail.com>
-- Stability   : experimental
-- Portability : GHC
--
-----------------------------------------------------------------------------

module Database.HXournal.IDMap.Client.Communication.Multipart where

import Blaze.ByteString.Builder
-- import Blaze.ByteString.Builder.Char.Utf8 as BC
import Data.ByteString.Lazy hiding (map)
import Data.ByteString.Lazy.Char8 hiding (map)
import Data.Monoid  

-- | 
-- 
-- class ToBuilder a where
--   toBuilder :: a -> Builder 
--   toContentType :: a -> ContentType 

-- | 

infixl 4 <>

(<>) :: Monoid a => a -> a -> a 
(<>) = mappend 


-- | 

startbdry :: ByteString -> Builder 
startbdry boundary = fromLazyByteString "--" 
                    <> fromLazyByteString boundary 
                    <> fromLazyByteString "\r\n"

-- | 

endbdry :: ByteString -> Builder 
endbdry boundary = fromLazyByteString "--" 
                   <> fromLazyByteString boundary 
                   <> fromLazyByteString "--"
                   <> fromLazyByteString "\r\n"


-- | 

contentDisposition :: ByteString -> Builder 
contentDisposition name = fromLazyByteString "Content-Disposition: form-data; name=\""
                          <> fromLazyByteString (name `append` "\"") 
                          <> lineBreak

contentDispositionFile :: ByteString -> ByteString -> Builder 
contentDispositionFile name fname = 
    fromLazyByteString 
      ("Content-Disposition: form-data; name=\"" 
       <> name 
       <> "\"; filename=\""
       <> fname
       <> "\"")
    <> lineBreak


-- | 

data ContentType = TextPlain 
                 | ImageGif 
                 | ApplicationXoj
                 | ApplicationJson
                 deriving (Show,Eq,Ord,Enum)

-- | 

contentType :: ContentType -> Builder 
contentType typ = ctypeheader <> fromLazyByteString (ctype typ) <> lineBreak
  where ctypeheader = fromLazyByteString "Content-Type: " 
        ctype TextPlain = "text/plain"
        ctype ImageGif = "image/gif"
        ctype ApplicationXoj = "application/xoj"
        ctype ApplicationJson = "application/json"



-- | 

lineBreak :: Builder 
lineBreak = fromLazyByteString "\r\n"


-- | 

data OnePart = OnePartTextFile 
               { fieldName :: ByteString
               , fileName :: ByteString
               , fileeContent :: ByteString
               }
             | OnePartText ByteString ByteString 
             | OnePartGif ByteString ByteString 
             | OnePartJson ByteString ByteString

-- | 

toBuilder :: OnePart -> Builder 
toBuilder opart = ctypedispos
                  <> ctypeheader 
                  <> lineBreak 
                  <> fromLazyByteString cnt
                  <> lineBreak
  where (ctypedispos,ctypeheader,cnt) = case opart of 
          OnePartTextFile name fname bstr -> 
            (contentDispositionFile name fname, contentType TextPlain, bstr)
          OnePartText name bstr -> (contentDisposition name,contentType TextPlain,bstr)
          OnePartGif name bstr -> (contentDisposition name,contentType ImageGif ,bstr)
          OnePartJson name bstr -> (contentDisposition name,contentType ApplicationJson,bstr)

-- |

onepart :: ByteString -> OnePart -> Builder
onepart boundary content = 
  startbdry boundary <> toBuilder content

-- |

multipart :: ByteString -> [OnePart] -> Builder 
multipart boundary xs = 
  mconcat (map (onepart boundary) xs)
  <> endbdry boundary 


