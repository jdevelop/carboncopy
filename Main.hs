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

import Prelude as P

emptyStr = BStr.pack ""

data CCState = Ok | NotFound | NoChain deriving ( Enum )

data Opts = Opts { email, storageFileName :: String, storage :: Storage StrHeader }

main = do
    opts <- prepareConfig
    getArgs >>= processArgs opts


processArgs :: Opts -> [String] -> IO ()
processArgs opts args = 
    case args of
        ("init":folders)    -> initPassedFolders folders
        []                  -> handlePassedEmail
        _                   -> usage
    where
        email'           = email opts
        storage'         = storage opts
        storageFileName' = storageFileName opts
        initPassedFolders folders = do
            let foldersLength = P.length folders
            unlessM (fileExists storageFileName' ) $ BStr.writeFile storageFileName' emptyStr
            mapM_ (initMailFolder storage' email' foldersLength ) $ P.zip folders [1..]
        handlePassedEmail = BStr.getContents >>= handleEmail storage' email' >>= processCCState
            


prepareConfig :: IO Opts
prepareConfig = do
    home <- getEnv "HOME"
    let configFileName = home </> ".ccrc"
    unlessM ( fileExists configFileName) $ error $ "Configuration file " ++ configFileName ++ " not found"
    configuration <- fmap loadConfiguration $ BStr.readFile configFileName
    let storageFileName = home </> ( fromJust . getPath $ configuration )
        storage = fileStorage storageFileName
        email = fromJust . getEmail $ configuration
    return ( Opts email storageFileName storage )


usage :: IO ()
usage = do
    progName <- getProgName
    P.putStrLn $ "Usage " ++ progName ++ " [init maildir1 maildir2 maildir3 ...] [ < content ]"
    P.putStrLn "\twhere 'init' will initialize header index with messages from given maildirs"
    P.putStrLn "\twith no arguments it will read message from stdin and attempt to recognize headers within it"
    P.putStrLn "\n\nExit codes:"
    P.putStrLn "\t0\t- reply to known thread was found, header was added to the storage"
    P.putStrLn "\t1\t- current email does not contain either e-mail address from configuration and is not a reply to a known thread"
    P.putStrLn "\t2\t- no message headers were recognized"


processCCState :: CCState -> IO ()
processCCState state | stateCode == 0 = System.exitWith ExitSuccess
                     | otherwise      = System.exitWith $ ExitFailure stateCode
    where
        stateCode = fromEnum state


fileExists :: FilePath -> IO Bool
fileExists = vDoesFileExist SystemFS

initMailFolder :: Storage StrHeader -> String -> Int -> (FilePath , Int) -> IO ()
initMailFolder storage email count (mailStorage, idx) = do
    P.putStrLn $ "Processing storage " ++ show idx ++ " of " ++ show count ++ " at " ++ mailStorage
    storageInit email mailStorage storage

handleEmail :: Storage StrHeader -> String -> ByteString -> IO CCState
handleEmail storage email content = handleEmail' chain
    where ( chain, ownerEmail ) = matchFromHeader content email
          handleEmail' (Just chain) = handleEmail'' ownerEmail
             where
                  handleEmail'' True = do hdrAdd storage . current $ chain    
                                          System.exitWith ExitSuccess
                  handleEmail'' _ = do existsInStorage <- saveMatchingChain storage chain
                                       if existsInStorage
                                          then return Ok
                                          else return NotFound
          handleEmail' _ =  return NoChain
