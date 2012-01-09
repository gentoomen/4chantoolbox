-- Duplicate file checker
-- Finds duplicate files based on MD5
--
-- Author: rfw
--
-- This file has been placed in the public domain.

import Control.Monad            (foldM)
import qualified Data.Hash.MD5  as MD5
import Data.List                (intercalate, nub)
import qualified Data.Map       as Map
import Data.Maybe               (fromMaybe)
import System.Environment       (getArgs)
import System.Directory         (getDirectoryContents)
import System.FilePath          (combine)
import System.IO                (withBinaryFile, hGetContents, IOMode(ReadMode))
import System.IO.PlafCompat     (getFileStatus, isDirectory)

type MD5HashMap = Map.Map Integer [FilePath]

-- hash a single file and return the MD5 sum
hashFile :: String -> IO Integer
hashFile fileName =
    withBinaryFile fileName ReadMode $ \handle -> do
        contents <- hGetContents handle
        return $! MD5.md5i $ MD5.Str contents

-- hash a directory and put them into an MD5 hash map
hashDirectory :: MD5HashMap -> FilePath -> IO MD5HashMap
hashDirectory hashes dirName = do
    contents <- getDirectoryContents dirName
    statuses <- mapM getFileStatus contents
    let files = map fst $ filter (not . isDirectory . snd) (zip contents statuses)
    -- now hash the files and throw them into the map
    foldM hashAndPut hashes files
  where
    hashAndPut hashes fileName = do
        hash <- hashFile fileName
        let fn = combine dirName fileName
        let existing = fromMaybe [] $ Map.lookup hash hashes
        return $ Map.insert hash (fn:existing) hashes

-- hash multiple directories
hashDirectories :: MD5HashMap -> [FilePath] -> IO MD5HashMap
hashDirectories = foldM hashDirectory

-- generate a summary of duplicates
generateSummary :: MD5HashMap -> IO ()
generateSummary hashes = mapM_ summarize (Map.toList hashes)
  where
    summarize (_, [x]) = return ()
    summarize (hash, fileNames) =
        putStrLn $ intercalate ", " fileNames ++ " have the same hash."

main :: IO ()
main = do
    argv <- getArgs
    let dirs = nub argv
    if dirs == [] 
        then fail "At least one directory should be specified."
        else hashDirectories Map.empty dirs >>= generateSummary
