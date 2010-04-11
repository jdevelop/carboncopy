module Main(main) where

import CarbonCopy.StorageInit
import CarbonCopy.MailHeaders
import CarbonCopy.HeadersStorage
import CarbonCopy.ThreadBuilder
import CarbonCopy.Configuration

import System
import System.Exit
import System.IO.HVFS
import System.FilePath.Posix

import Data.ByteString.Char8 as BStr
import Data.Maybe

import Monad

emptyStr = BStr.pack ""

main = do
    home <- getEnv "HOME"
    args <- getArgs
    let configFileName = home </> ".ccrc"
    configFileExists <- fileExists configFileName
    unless configFileExists $ error $ "Configuration file " ++ configFileName ++ " not found"
    configuration <- fmap loadConfiguration $ BStr.readFile configFileName
    let storageFileName = home </> ( fromJust . getPath $ configuration )
        storage = fileStorage storageFileName
        email = fromJust . getEmail $ configuration
    case args of
        ("init":folders) -> do 
            let foldersLength = Prelude.length folders
            storageFileExists <- fileExists storageFileName
            unless storageFileExists $ BStr.writeFile storageFileName emptyStr
            mapM_ (initMailFolder storage email foldersLength ) $ Prelude.zip folders [1..]
        [] -> BStr.getContents >>= handleEmail storage email
        otherwise -> error "Wrong arguments"


fileExists :: FilePath -> IO (Bool)
fileExists = vDoesFileExist SystemFS

initMailFolder :: Storage StrHeader -> String -> Int -> (FilePath , Int) -> IO ()
initMailFolder storage email count (mailStorage, idx) = do
    Prelude.putStrLn $ "Processing storage " ++ show idx ++ " of " ++ show count ++ " at " ++ mailStorage
    storageInit email mailStorage storage

handleEmail :: Storage StrHeader -> String -> ByteString -> IO ()
handleEmail storage email content = do
    let ( chain, hdrsMatchFound ) = matchFromHeader content email
    case chain of
        (Just chain)    -> do 
                                messageMatches <- saveMatchingChain storage chain
                                if  hdrsMatchFound || messageMatches
                                    then do 
                                            unless hdrsMatchFound $ hdrAdd storage $ current chain
                                            System.exitWith ExitSuccess
                                    else do System.exitWith $ ExitFailure 1
        otherwise       -> System.exitWith $ ExitFailure 2
