{-# OPTIONS_GHC -XTypeSynonymInstances -XMultiParamTypeClasses  #-}
module CarbonCopy.ThreadBuilder (
    headerVal,
    saveMatchingChain,
    Chain(..),
    getChain,
    MsgidChain) where

import Control.Monad
import CarbonCopy.MailHeaders
import CarbonCopy.HeadersStorage
import Data.Maybe
import Data.ByteString.Char8 as BStr
import Text.ParserCombinators.ReadP as R

data Chain keyT valueT = Chain { current, previous :: Header keyT valueT}

type MsgidChain = Chain String String

headerVal :: String -> ReadP StrHeader
headerVal name = do 
            R.string name
            R.skipSpaces
            R.char ':'
            R.skipSpaces
            R.char '<'
            xs <- R.many R.get
            R.char '>'
            return (Header name xs)


getChain :: ByteString -> Maybe MsgidChain
getChain content = getChain' headers
    where 
        headers = extractHeaders content ( (headerVal msg_id_hdr) +++ (headerVal in_reply_to_hdr) )
        getChain' (h1:h2:[]) =Just chain
            where
                chain | name h1 == msg_id_hdr = Chain {current = h1, previous = h2 }
                      | otherwise = Chain {current = h2, previous = h1 }
        getChain' _          = Nothing


saveMatchingChain :: Storage (Header keyT valueT) -> Chain keyT valueT -> IO (Bool)
saveMatchingChain storage 
                  ( Chain { current=myCurrent, previous=myPrevious } ) = do
    prevHdrExists <- storage `hdrExists` myPrevious
    currHdrExists <- storage `hdrExists` myCurrent
    when (prevHdrExists && not currHdrExists) $ storage `hdrAdd` myCurrent
    return ( prevHdrExists || currHdrExists )
