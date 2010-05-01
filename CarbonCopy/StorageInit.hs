module CarbonCopy.StorageInit (
    storageInit
    ) where

import Control.Monad.IfElse
import Data.Maybe
import CarbonCopy.EmailStorage
import CarbonCopy.MailHeaders
import CarbonCopy.HeadersStorage
import CarbonCopy.ThreadBuilder

storageInit :: String -> FilePath -> Storage StrHeader -> IO ()
storageInit email rootDir storage = visitEmailsRecursively rootDir ( processHeader email storage )


processHeader :: String -> Storage StrHeader -> EmailHandler
processHeader email storage content = processHeader' chain
    where
        (chain, hdrsMatchFound) = matchFromHeader content email
        processHeader' (Just (Chain {current=fromMsgId, previous=inReplyTo})) = processHeader'' hdrsMatchFound
            where
                processHeader'' True = unlessM (storage `hdrExists` fromMsgId) $ storage `hdrAdd` fromMsgId
                processHeader'' False = whenM (storage `hdrExists` inReplyTo) $ 
                                            unlessM (storage `hdrExists` fromMsgId) $ 
                                                storage `hdrAdd` fromMsgId
        processHeader' (Just (Root {current=fromMsgId})) = processHeader'' hdrsMatchFound
            where
                processHeader'' True = unlessM (storage `hdrExists` fromMsgId) $ storage `hdrAdd` fromMsgId
                processHeader'' False = return ()
        processHeader' _ = return ()
