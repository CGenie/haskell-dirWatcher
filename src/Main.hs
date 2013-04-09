{-# LANGUAGE CPP, TemplateHaskell #-}
-----------------------------------------------------------------------------
--
-- Module      :  Main
-- Copyright   :
-- License     :  AllRightsReserved
--
-- Maintainer  :
-- Stability   :
-- Portability :
--
-- |
--
-----------------------------------------------------------------------------

-- TODO: config file parsing instead of hardcoded values

module Main (
    main
) where

import Data.List (stripPrefix)
import Data.List.Split (splitOn)
import Data.String.Utils

import Control.Concurrent (threadDelay)
import Control.Monad (unless, liftM, filterM)
import Control.Monad.State

import System.Directory
import System.Posix (getFileStatus, fileSize)
import System.Cmd
import System.Exit (exitFailure)

--import qualified Data.ConfigFile as ConfigFile
--import Data.Either.Utils


watchedDirectory = "/home/przemek/Dropbox/dirWatcher"
refreshInterval = 5  -- in seconds
actionCommands = ["lpr %(file)s", "rm %(file)s"]

data SystemType = Windows | Linux
     deriving Eq

data WatchedFile = WatchedFile {
                               path :: String
                              ,size :: Integer
                   } deriving (Eq, Show)

data WatchedFilesState = WatchedFilesState {
                         watchedFiles :: [WatchedFile]
                   } deriving (Show)

type AppState = StateT WatchedFilesState IO

currentSystem = Linux

dirSeparator | currentSystem == Linux = "/"
             | currentSystem == Windows = "\\"


main = do
         let state = WatchedFilesState {watchedFiles = []}
               
         runLoop state


runLoop = evalStateT loop

loop :: AppState ()
loop = do
        dc <- liftIO $ listFiles watchedDirectory
        watchedFilesState <- get
        let (actionFiles, newFiles, modifiedFiles) = computeWatchedFiles (watchedFiles watchedFilesState) dc
        liftIO $ mapM performAction actionFiles
        liftIO $ mapM newFile newFiles
        liftIO $ mapM modifiedFile modifiedFiles
        put (watchedFilesState {watchedFiles = newFiles ++ modifiedFiles})
        liftIO $ print "-------------------"
        liftIO $ threadDelay (refreshInterval * 1000000)
        loop


performAction wf = do
        print ("Performing action on " ++ path wf)
        --removeFile (path wf)
        mapM_ (\command -> system (replace "%(file)s" (path wf) command)) actionCommands

newFile wf = do
        print ("New file " ++ path wf)

modifiedFile wf = do
        print ("Modified file " ++ path wf)


computeWatchedFiles watchedFiles dc = (actionFiles, newFiles, modifiedFiles)
        where
            actionFilesFilter wf = wf `elem` watchedFiles
            actionFiles = filter actionFilesFilter dc
            newFilesFilter = not . (\wf -> (path wf) `elem` (map path watchedFiles))
            newFiles = filter newFilesFilter dc
            modifiedFilesFilter wf = and [(path wf) `elem` (map path watchedFiles),
                                          (not . actionFilesFilter) wf]
            modifiedFiles = filter modifiedFilesFilter dc


createDirectoryIfNotExists dir = do
        dirExists <- doesDirectoryExist dir
        if dirExists
            then return ()
            else createDirectory dir

special "." = True
special ".." = True
special _ = False

getFileSize path = do
                 stat <- getFileStatus path
                 return $ fileSize stat

makeFullPaths dir dirlist = map (\dd -> dir ++ dirSeparator ++ dd) dirlist

directoryExistsAndNotSpecial dir = do
        de <- doesDirectoryExist dir
        return $ and [de, not $ special dname]
        where
            dname = last $ splitOn dirSeparator dir

listSpecificDirectoryContent dir content = do
        createDirectoryIfNotExists dir
        dc <- getDirectoryContents dir
        filtered <- filterM existenceFunction (makeFullPaths dir dc)
        fileSizes <- mapM getFileSize filtered
        return $ map (\(path, size) -> WatchedFile {path = path, size = toInteger size})
                     (zip filtered fileSizes)
        where
            existenceFunction = case content of
                        "directory" -> directoryExistsAndNotSpecial
                        "file"      -> doesFileExist

listDirectories dir = listSpecificDirectoryContent dir "directory"
listFiles dir = listSpecificDirectoryContent dir "file"

