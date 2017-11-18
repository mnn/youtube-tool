{-# LANGUAGE TypeOperators #-}

{-|
Module     : Main
Description: CLI tool for easy downloading and opening of videos from YouTube (presumes presence of youtube-dl and kde-open).
Copyright  : monnef
License    : GPL-3
Stability  : experimental
-}

module Main where

import Lib
import System.IO (hGetContents, hFlush, stdout)
import System.Process (createProcess, StdStream(CreatePipe), cwd, std_out, std_err, proc, waitForProcess)
import System.Exit (ExitCode(ExitFailure, ExitSuccess))
import Control.Monad (when)
import Data.List (isInfixOf, isPrefixOf)
import Text.Regex.PCRE.String (compile, regexec, compBlank, execBlank)
import Data.Either.Utils (fromRight)
import System.Directory (doesFileExist, getHomeDirectory)
import System.FilePath.Posix (takeBaseName, takeExtension)
import Data.Function ((&))
import System.Hclip (getClipboard)

infixl 1:
(<&>) :: Functor f => f a -> (a -> b) -> f b
(<&>) = flip fmap

runCmd :: String -> [String] -> Bool -> String -> IO ([String], [String], ExitCode)
runCmd cmd args printOutput targetDir = do
  (_, Just hout, Just herr, jHandle) <-
    createProcess (proc cmd args)
      { cwd = Just targetDir
      , std_out = CreatePipe
      , std_err = CreatePipe
      }

  out <- hGetContents hout
  when printOutput $ putStrLn out
  
  err <- hGetContents herr
  when printOutput $ putStrLn err
  
  exitCode <- waitForProcess jHandle
  return (lines out, lines err, exitCode)

runCmdInDownloadDir :: String -> [String] -> Bool -> IO ([String], [String], ExitCode)
runCmdInDownloadDir cmd args printOutput = do
  dir <- downloadDir
  runCmd cmd args printOutput dir

-- not reliable, fails when video is already downloaded
parseFileNameFromOutputAndOpenDownloadedVideo :: [String] -> IO ()
parseFileNameFromOutputAndOpenDownloadedVideo out = do
  let matchingLines = filter (isInfixOf "Merging formats into") out
  when (length matchingLines /= 1) $ error $ "unexpected count of matching lines: " ++ show matchingLines
  regex <- fromRight <$> compile compBlank execBlank "\"([^\"]+)\""
  rawRegexpRes <- regexec regex (head matchingLines)
  name <- case rawRegexpRes of 
           Left a -> error $ "Failed to extract a file name: " ++ show a
           Right Nothing -> error $ "Failed to extract a file name (not found): " ++ show matchingLines
           Right (Just (_, _, _, matches)) -> do
             when (length matches < 1) $ error $ "Too few regexp matches: " ++ show matches
             putStrLn $ "matches: " ++ show matches
             return $ head matches
  openVideo name

downloadVideo :: String -> String -> IO ([String], [String], ExitCode)
downloadVideo link dir = do
  (out, err, exitCode) <- runCmd "youtube-dl" ["--no-playlist", link] True dir
  case exitCode of ExitFailure errCode -> error $ "Error " ++ show errCode ++ " occured. Error output:\n" ++ unlines err
                   ExitSuccess -> return (out, err, exitCode)

getVideoFilename :: String -> String -> IO String
getVideoFilename link dir = do
  (out, err, exitCode) <- runCmd "youtube-dl" ["--no-playlist", "--get-filename", link] False dir
  case exitCode of ExitFailure errCode ->
                     error $ "Error " ++ show errCode ++ " during getting filename occured. Error output:\n" ++ unlines err
                   ExitSuccess -> onRunSuccess $ head out
  where
    onRunSuccess :: String -> IO String
    onRunSuccess returnedFileName = do
      returnedFileExists <- doesFileExist returnedFileName
      if returnedFileExists then return returnedFileName
                            else tryOtherExtensions returnedFileName
    tryOtherExtensions :: String -> IO String
    tryOtherExtensions origName = do
      let otherExtensions = ["webm", "mkv", "avi", "mp4"]
      isExistingList <- mapM mapFn otherExtensions :: IO [(Bool, String)]
      let onlyExisting = isExistingList & filter fst & map snd :: [String]
      when (null onlyExisting) $ error $ "Newly created file not found even when trying other extensions." ++ show isExistingList
      return $ head onlyExisting
      where
        baseName = takeBaseName origName
        mapFn ext = do
          let newName = dir ++ "/" ++ baseName ++ "." ++ ext
          exists <- doesFileExist newName 
          return (exists, newName)

openVideo :: String -> IO ()
openVideo name = do
  putStrLn $ "opening file: " ++ show name
  _ <- runCmdInDownloadDir "kde-open" [name] True
  return ()

homeDir :: IO String
homeDir = getHomeDirectory

downloadDirSuffix :: String
downloadDirSuffix = "/Downloads/yt"

downloadDir :: IO String
downloadDir =  homeDir <&> (++ downloadDirSuffix)

putStrAndFlush :: String -> IO ()
putStrAndFlush x = do
  putStr x
  hFlush stdout

printQueryAndReadLine :: IO String
printQueryAndReadLine = do
  putStrAndFlush "link: "
  getLine

looksLikeAYouTubeLink :: String -> Bool
looksLikeAYouTubeLink x = (isPrefixOf "http" x) && (isInfixOf "youtube" x)

handleQueryAndFileOpen :: String -> String -> IO ()
handleQueryAndFileOpen link dir = do
  fileName <- getVideoFilename link dir
  putStrAndFlush $ "Open downloaded video " ++ fileName ++ "? (y/n) [y]: "
  openAnswer <- getLine
  when (openAnswer == "y" || openAnswer == "") $ do
    -- parseFileNameFromOutputAndOpenDownloadedVideo out
    putStrLn $ "Opening file: \"" ++ fileName ++ "\""
    openVideo fileName

main :: IO ()
main = do
  putStrLn "YouTube download utility by *monnef* (rewritten in Haskell from Clojure)"

  clipboard <- getClipboard
  link <- if looksLikeAYouTubeLink clipboard
    then do
      putStrLn $ "Using URL from clipboard - " ++ clipboard
      return clipboard 
    else printQueryAndReadLine

  dir <- downloadDir

  (out, err, exitCode) <- downloadVideo link dir
  handleQueryAndFileOpen link dir
