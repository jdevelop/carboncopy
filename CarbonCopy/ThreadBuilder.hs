{-# OPTIONS_GHC -XTypeSynonymInstances -XMultiParamTypeClasses  #-}
module CarbonCopy.ThreadBuilder (
    headerVal,
    saveMatchingChain,
    Chain(..),
    prepareChain,
    matchFromHeader,
    MsgidChain) where

import Control.Monad
import CarbonCopy.MailHeaders
import CarbonCopy.HeadersStorage
import Data.Maybe
import Data.ByteString.Char8 as BStr
import Text.ParserCombinators.ReadP as R
import Data.List as L
import Prelude as P

data Chain keyT valueT = Chain { current, previous :: Header keyT valueT} | Root { current :: Header keyT valueT }

type MsgidChain = Chain String String


saveMatchingChain :: Storage (Header keyT valueT) -> Chain keyT valueT -> IO (Bool)
saveMatchingChain storage 
                  ( Chain { current=myCurrent, previous=myPrevious } ) = do
    prevHdrExists <- storage `hdrExists` myPrevious
    currHdrExists <- storage `hdrExists` myCurrent
    when (prevHdrExists && not currHdrExists) $ storage `hdrAdd` myCurrent
    return ( prevHdrExists || currHdrExists )
saveMatchingChain storage _ = return False

findHeaderByName :: String -> [StrHeader] -> Maybe (StrHeader)
findHeaderByName hdrName = L.find ( (hdrName ==) . name )

prepareChain :: [StrHeader] -> Maybe (MsgidChain)
prepareChain hdrs = processNextHeader $ findHeaderByName in_reply_to_hdr hdrs
    where 
        processNextHeader (Just inReplyToHdr) = do
            msgIdHdr <- findHeaderByName msg_id_hdr hdrs
            Just (Chain msgIdHdr inReplyToHdr)
        processNextHeader Nothing = do
            msgIdHdr <- findHeaderByName msg_id_hdr hdrs
            Just (Root msgIdHdr)

matchFromHeader :: ByteString -> String -> ( Maybe MsgidChain, Bool )
matchFromHeader content email = ( chain, hdrsMatchFound )
    where 
        headers = visitHeader content
        hdrsMatchFound = or $ P.map hdrMatch headers
        chain = prepareChain headers
        hdrMatch (Header name value) = name == from_hdr && email `L.isInfixOf` value

visitHeader :: ByteString -> [StrHeader]
visitHeader = flip extractHeaders ( 
                                    headerValue from_hdr +++ 
                                    headerVal msg_id_hdr +++ 
                                    headerVal in_reply_to_hdr 
                                  )

headerValue :: String -> ReadP StrHeader
headerValue hdrName = do
            R.string hdrName
            R.skipSpaces
            R.char ':'
            R.skipSpaces
            xs <- R.many R.get
            R.satisfy ( == '\n' )
            return (Header hdrName xs)

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
