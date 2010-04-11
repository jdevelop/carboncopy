module CarbonCopy.Configuration (
    Configuration, 
    loadConfiguration, 
    defaultConfiguration,
    getPath, 
    getEmail) where

import Data.ByteString.Char8 as BStr
import Text.ParserCombinators.ReadP as R
import Data.Char
import Data.Maybe
import Data.List

import Prelude as P

data Option = Path String | Email String | Unparsed String

data Configuration = Configuration [Option]


defaultConfiguration :: String -> Configuration
defaultConfiguration email = Configuration [Path ".ccheader", Email email]

loadConfiguration :: ByteString -> Configuration
loadConfiguration content = Configuration ( P.foldl (parseLine) [] $ BStr.lines content )
    where 
        parseLine :: [Option] -> ByteString -> [Option]
        parseLine acc line = case parsedLine of
                                    [(nv@(name,value),_)]   -> handleNv nv
                                    [(nv@(name,value),_),_] -> handleNv nv
                                    unparsed                -> (Unparsed unpackedLine):acc
                             where  handleNv (name,value) =
                                        case name of
                                            "cc_header_file"    -> (Path value):acc
                                            "originator_email"  -> (Email value):acc
                                            _                   -> acc
                                    unpackedLine = BStr.unpack line
                                    parsedLine = readP_to_S extractNameValue unpackedLine

extractNameValue :: ReadP (String, String)
extractNameValue = do
    name <- munch ( not . (\c -> isSpace c || c `P.elem` "#="))
    skipSpaces
    char '='
    skipSpaces
    value <- munch (\_ -> True)
    skipSpaces
    return (name, value)


getPath :: Configuration -> Maybe String
getPath (Configuration xs) = listToMaybe [value | Path value <- xs]

getEmail :: Configuration -> Maybe String
getEmail (Configuration xs) = listToMaybe [email | Email email <- xs]

instance Show Configuration where
    show (Configuration xs) = P.foldl ( flip ((++) . (++ "\n") . show) ) "" xs

instance Show Option where
    show (Email value) = "email => " ++ value
    show (Path value) = "path => " ++ value
    show (Unparsed line) = "unparsed => " ++ line
