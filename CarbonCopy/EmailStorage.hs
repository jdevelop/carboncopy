module CarbonCopy.EmailStorage (EmailHandler, visitEmailsRecursively) where

import Control.Monad
import System.IO.HVFS
import System.FilePath.Posix
import Data.ByteString.Char8 as BStr

import Prelude as P

type EmailHandler = ByteString -> IO ()

(?:) :: IO (Bool) -> ( IO (), IO () ) -> IO ()
(?:) cond (actL,actR) = cond >>= \condExp -> if condExp then actL else actR

(?&) :: IO (Bool) -> IO () -> IO ()
(?&) cond act = cond ?: ( act, return () )

visitEmailsRecursively :: FilePath -> EmailHandler -> IO ()
visitEmailsRecursively dir emailHandler =
    vGetDirectoryContents SystemFS dir >>= mapM_ processDirectory . P.filter (not . flip P.elem [".",".."] )
        where 
            processDirectory _path = fileExists ?: 
                                        ( BStr.readFile filePath >>= emailHandler, 
                                          dirExists ?& visitEmailsRecursively newDir emailHandler
                                        )
                where
                    filePath = dir </> _path
                    fileExists = vDoesFileExist SystemFS filePath
                    dirExists = vDoesDirectoryExist SystemFS filePath
                    newDir = addTrailingPathSeparator filePath
