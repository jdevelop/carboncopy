module CarbonCopy.StorageInit (
    storageInit,
    matchFromHeader
    ) where

import Prelude as P
import Control.Monad
import Data.Maybe
import Data.List as L
import CarbonCopy.EmailStorage
import CarbonCopy.MailHeaders
import CarbonCopy.HeadersStorage
import CarbonCopy.ThreadBuilder as TB
import Text.ParserCombinators.ReadP as R
import Data.ByteString.Char8 as BStr

storageInit :: String -> FilePath -> Storage StrHeader -> IO ()
storageInit email rootDir storage = visitEmailsRecursively rootDir ( processHeader email storage )


processHeader :: String -> Storage StrHeader -> EmailHandler
processHeader email storage = \content -> do
    let (chain, hdrsMatchFound) = matchFromHeader content email
    case chain of
        (Just myChain)  -> if hdrsMatchFound
                                then do
                                    let fromMsgId = current myChain
                                    fromAlreadyExistsInStorage <- hdrExists storage fromMsgId
                                    when (not fromAlreadyExistsInStorage) $ hdrAdd storage fromMsgId
                                else saveMatchingChain storage myChain >> return ()
        otherwise       -> return ()
            

matchFromHeader :: ByteString -> String -> ( Maybe MsgidChain, Bool )
matchFromHeader content email = ( chain, hdrsMatchFound )
    where 
        headers = visitHeader email content
        hdrsMatchFound = P.foldr hdrMatch False $ headers
        chain = prepareChain headers
        hdrMatch (Header name value) acc | name == from_hdr && email `L.isInfixOf` value = True
                                         | otherwise = acc


findHeaderByName :: String -> [StrHeader] -> Maybe (StrHeader)
findHeaderByName hdrName = L.find ( (hdrName ==) . name )

prepareChain :: [StrHeader] -> Maybe (MsgidChain)
prepareChain hdrs = findHeaderByName msg_id_hdr hdrs >>= \msgIdHdr ->
                    findHeaderByName in_reply_to_hdr hdrs >>= \inReplyToHdr ->
                    Just (Chain msgIdHdr inReplyToHdr)


visitHeader :: String -> ByteString -> [StrHeader]
visitHeader email msgData = extractHeaders msgData ( 
                                    headerValue from_hdr +++ 
                                    TB.headerVal msg_id_hdr +++ 
                                    TB.headerVal in_reply_to_hdr 
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
