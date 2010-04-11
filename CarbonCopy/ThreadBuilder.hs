{-# OPTIONS_GHC -XTypeSynonymInstances -XMultiParamTypeClasses  #-}
module CarbonCopy.ThreadBuilder (
    headerVal,
    saveMatchingChain,
    Chain(..),
    getChain,
    MsgidChain) where

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
getChain content = case headers of
                        (h1:h2:[]) -> Just chain
                            where
                                chain | name h1 == msg_id_hdr = Chain {current = h1, previous = h2 }
                                      | otherwise = Chain {current = h2, previous = h1 }
                        _          -> Nothing
    where 
        headers = extractHeaders content ( (headerVal msg_id_hdr) +++ (headerVal in_reply_to_hdr) )


saveMatchingChain :: Storage (Header keyT valueT) -> Chain keyT valueT -> IO (Bool)
saveMatchingChain storage 
                  ( Chain { current=myCurrent, previous=myPrevious } ) = do
    prevHdrExists <- hdrExists storage myPrevious
    currHdrExists <- hdrExists storage myCurrent
    if prevHdrExists && not currHdrExists
        then do hdrAdd storage myCurrent 
                return (True)
        else return ( currHdrExists )
