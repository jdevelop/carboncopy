{-# LANGUAGE TypeSynonymInstances #-}
module CarbonCopy.MailHeaders (
    Header(..),
    StrHeader,
    HeaderMatcher,
    extractHeaders,
    msg_id_hdr,
    from_hdr,
    in_reply_to_hdr
    ) where

import Prelude as P
import Data.Char
import Data.ByteString.Char8 as BStr hiding (concatMap, takeWhile)
import Text.ParserCombinators.ReadP as R

data (Eq keyT) => Header keyT valueT = Header { name :: keyT, value :: valueT } deriving Eq

type StrHeader = Header String String

instance Show StrHeader where
    show (Header name value) = show name ++ ":" ++ show value

type HeaderMatcher = ReadP StrHeader

msg_id_hdr      =   "message-id"
in_reply_to_hdr =   "in-reply-to"
from_hdr        =   "from"

extractHeaders :: ByteString -> HeaderMatcher -> [StrHeader]
extractHeaders src matcher = concatMap parse . takeWhile ( /= BStr.empty) $ lines
                             where
                                lines = BStr.lines src
                                parse l = P.map fst $ readP_to_S matcher line
                                    where line = BStr.unpack ( BStr.map toLower l ) ++ "\n"
