module CarbonCopy.StorageInit (
    storageInit,
    matchFromHeader
    ) where

import Prelude as P
import Control.Monad
import Control.Monad.IfElse
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
processHeader email storage content = processHeader' chain
    where
        (chain, hdrsMatchFound) = matchFromHeader content email
        processHeader' (Just myChain) = processHeader'' hdrsMatchFound
            where
                fromMsgId = current myChain
                processHeader'' True = unlessM (storage `hdrExists` fromMsgId) $ storage `hdrAdd` fromMsgId
                processHeader'' False = return ()
        processHeader' _ = return ()
            

matchFromHeader :: ByteString -> String -> ( Maybe MsgidChain, Bool )
matchFromHeader content email = ( chain, hdrsMatchFound )
    where 
        headers = visitHeader content
        hdrsMatchFound = or $ P.map hdrMatch headers
        chain = prepareChain headers
        hdrMatch (Header name value) = name == from_hdr && email `L.isInfixOf` value


findHeaderByName :: String -> [StrHeader] -> Maybe (StrHeader)
findHeaderByName hdrName = L.find ( (hdrName ==) . name )

prepareChain :: [StrHeader] -> Maybe (MsgidChain)
prepareChain hdrs = do
                    msgIdHdr <- findHeaderByName msg_id_hdr hdrs
                    inReplyToHdr <- findHeaderByName in_reply_to_hdr hdrs
                    Just (Chain msgIdHdr inReplyToHdr)


visitHeader :: ByteString -> [StrHeader]
visitHeader = flip extractHeaders ( 
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
