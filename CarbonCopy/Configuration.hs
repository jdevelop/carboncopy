module CarbonCopy.Configuration (
    Configuration, 
    loadConfiguration, 
    defaultConfiguration,
    getPath, 
    getEmail) where

import Data.ByteString.Char8 as BStr
import Text.ParserCombinators.ReadP as R
import Data.Char
import Data.List

data Option = Path String | Email String | Unparsed String

data Configuration = Configuration [Option]


defaultConfiguration :: String -> Configuration
defaultConfiguration email = Configuration [Path ".ccheader", Email email]

loadConfiguration :: ByteString -> Configuration
loadConfiguration content = Configuration ( Prelude.foldl (parseLine) [] $ BStr.lines content )
    where 
        parseLine :: [Option] -> ByteString -> [Option]
        parseLine acc line = let parsed = readP_to_S extractNameValue $ unpack line
                                in case parsed of
                                    [(nv@(name,value),_)]   -> handleNv nv
                                    [(nv@(name,value),_),_] -> handleNv nv
                                    unparsed                -> (Unparsed $ BStr.unpack line):acc
                             where handleNv (name,value) =
                                    case name of
                                        "cc_header_file"    -> (Path value):acc
                                        "originator_email"  -> (Email value):acc
                                        _                   -> acc

extractNameValue :: ReadP (String, String)
extractNameValue = do
    name <- munch ( not . (\c -> isSpace c || c `Prelude.elem` "#="))
    skipSpaces
    char '='
    skipSpaces
    value <- munch (\_ -> True)
    skipSpaces
    return (name, value)


getPath :: Configuration -> Maybe String
getPath (Configuration xs) = extractPath xs
                             where extractPath [] = Nothing
                                   extractPath ((Path value):xs') = Just value
                                   extractPath (_:xs') = extractPath xs'

getEmail :: Configuration -> Maybe String
getEmail (Configuration xs) = extractEmail xs
                               where extractEmail [] = Nothing
                                     extractEmail ((Email value):xs') = Just value
                                     extractEmail (_:xs') = extractEmail xs'

instance Show Configuration where
    show (Configuration xs) = Prelude.foldl ( flip ((++) . (++ "\n") . show) ) "" xs

instance Show Option where
    show (Email value) = "email => " ++ value
    show (Path value) = "path => " ++ value
    show (Unparsed line) = "unparsed => " ++ line
