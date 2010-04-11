{-# OPTIONS_GHC -XTypeSynonymInstances -XMultiParamTypeClasses  #-}
module CarbonCopy.HeadersStorage (Storage, fileStorage, hdrAdd, hdrExists ) where

import Data.ByteString.Lazy.Char8 as BStr
import CarbonCopy.MailHeaders
import System.IO
import Prelude as P

crlf = BStr.pack "\n"

data Storage headerT  = HeadersStorage {
    exists :: headerT -> IO (Bool),
    add :: headerT -> IO ()
}

hdrExists :: Storage headerT -> headerT -> IO (Bool)
hdrExists HeadersStorage { exists = myExists } = myExists

hdrAdd :: Storage headerT -> headerT -> IO ()
hdrAdd HeadersStorage { add = myAdd } = myAdd

fileStorage :: FilePath -> Storage StrHeader
fileStorage path = HeadersStorage {
    exists = existsInFile path ,
    add =   \hdr -> do 
                BStr.appendFile path . BStr.pack . value $ hdr 
                BStr.appendFile path crlf
}

existsInFile :: FilePath -> StrHeader -> IO (Bool)
existsInFile path hdr = do 
                handle <- openFile path ReadMode 
                found <- fmap (P.elem hdrValue . BStr.lines) $ BStr.hGetContents handle
                found `seq` hClose handle
                return found
                    where
                        hdrValue = BStr.pack . value $ hdr
