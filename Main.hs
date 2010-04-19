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

import Control.Monad.IfElse
import Control.Monad

emptyStr = BStr.pack ""

data CCState = Ok | NotFound | NoChain deriving ( Enum )

main = do
    home <- getEnv "HOME"
    args <- getArgs
    let configFileName = home </> ".ccrc"
    unlessM ( fileExists configFileName) $ error $ "Configuration file " ++ configFileName ++ " not found"
    configuration <- fmap loadConfiguration $ BStr.readFile configFileName
    let storageFileName = home </> ( fromJust . getPath $ configuration )
        storage = fileStorage storageFileName
        email = fromJust . getEmail $ configuration
    case args of
        ("init":folders) -> do 
            let foldersLength = Prelude.length folders
            unlessM (fileExists storageFileName) $ BStr.writeFile storageFileName emptyStr
            mapM_ (initMailFolder storage email foldersLength ) $ Prelude.zip folders [1..]
        [] -> BStr.getContents >>= handleEmail storage email >>= processCCState
        otherwise -> error "Wrong arguments"


processCCState :: CCState -> IO ()
processCCState = System.exitWith . ExitFailure . fromEnum


fileExists :: FilePath -> IO (Bool)
fileExists = vDoesFileExist SystemFS

initMailFolder :: Storage StrHeader -> String -> Int -> (FilePath , Int) -> IO ()
initMailFolder storage email count (mailStorage, idx) = do
    Prelude.putStrLn $ "Processing storage " ++ show idx ++ " of " ++ show count ++ " at " ++ mailStorage
    storageInit email mailStorage storage

handleEmail :: Storage StrHeader -> String -> ByteString -> IO ( CCState )
handleEmail storage email content = handleEmail' chain
    where ( chain, ownerEmail ) = matchFromHeader content email
          handleEmail' (Just chain) = handleEmail'' ownerEmail
             where
                  handleEmail'' True = do hdrAdd storage . current $ chain    
                                          System.exitWith ExitSuccess
                  handleEmail'' _ = do existsInStorage <- saveMatchingChain storage chain
                                       if existsInStorage
                                          then return ( Ok )
                                          else return ( NotFound )
          handleEmail' _ =  return ( NoChain )
