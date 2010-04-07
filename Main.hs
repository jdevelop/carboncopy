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

emptyStr = BStr.pack ""

main = do
    home <- getEnv "HOME"
    args <- getArgs
    let configFileName = getRelFileName home ".ccrc"
    configFileExists <- fileExists configFileName
    if not configFileExists
        then error $ "Configuration file " ++ configFileName ++ " not found"
        else return ()
    configuration <- BStr.readFile configFileName >>= return . loadConfiguration
    let storageFileName = getRelFileName home $ fromJust . getPath $ configuration
        storage = fileStorage storageFileName
        email = fromJust . getEmail $ configuration
    case args of
        ("init":folders) -> do 
            let foldersLength = Prelude.length folders
            storageFileExists <- fileExists storageFileName
            if not storageFileExists
                then BStr.writeFile storageFileName emptyStr
                else return ()
            mapM_ (initMailFolder storage email foldersLength ) $ Prelude.zip folders [1..]
        [] -> BStr.getContents >>= handleEmail storage email
        otherwise -> error "Wrong arguments"


fileExists :: FilePath -> IO (Bool)
fileExists = vDoesFileExist SystemFS

getRelFileName :: FilePath -> FilePath -> FilePath
getRelFileName = (</>)

initMailFolder :: Storage StrHeader -> String -> Int -> (FilePath , Int) -> IO ()
initMailFolder storage email count (mailStorage, idx) = 
    ( Prelude.putStrLn $ "Processing storage " ++ show idx ++ " at " ++ mailStorage ) 
    >> storageInit email mailStorage storage

handleEmail :: Storage StrHeader -> String -> ByteString -> IO ()
handleEmail storage email content = do
    let ( chain, hdrsMatchFound ) = matchFromHeader content email
    case chain of
        (Just chain)    -> do 
                                messageMatches <- saveMatchingChain storage chain
                                if  hdrsMatchFound || messageMatches
                                    then do 
                                            if (hdrsMatchFound) 
                                                then hdrAdd storage $ current chain
                                                else return ()
                                            System.exitWith ExitSuccess
                                    else do System.exitWith $ ExitFailure 1
        otherwise       -> System.exitWith $ ExitFailure 2
