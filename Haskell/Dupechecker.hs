-- Duplicate file checker
-- Finds duplicate files based on MD5
--
-- Author: rfw
--
-- This file has been placed in the public domain.

import Control.Monad (foldM)
import qualified Data.Hash.MD5 as MD5
import Data.List (nub)
import qualified Data.Map as Map
import Data.Maybe (fromMaybe)
import Data.String.Utils (join)
import System (getArgs)
import System.Directory (getDirectoryContents)
import System.FilePath (combine)
import System.IO (withBinaryFile, hGetContents, IOMode(ReadMode))
import System.IO.PlafCompat (getFileStatus, isDirectory)
import Text.Printf (printf)

type MD5HashMap = Map.Map Integer [FilePath]

-- hash a single file and return the MD5 sum
hashFile :: String -> IO Integer
hashFile fileName = do
    withBinaryFile fileName ReadMode $ \handle -> do
        contents <- hGetContents handle
        return $! MD5.md5i $ MD5.Str contents

-- hash a directory and put them into an MD5 hash map
hashDirectory :: MD5HashMap -> FilePath -> IO MD5HashMap
hashDirectory hashes dirName = do
    let banner = printf  "Scanning: %s" dirName
    putStrLn banner
    putStrLn $ [ '=' | _ <- [1..length banner] ]

    contents <- getDirectoryContents dirName
    statuses <- mapM getFileStatus contents

    let files = map fst $ filter (not . isDirectory . snd) (zip contents statuses)

    -- now hash the files and throw them into the map
    foldM hashAndPut hashes files

    where
        hashAndPut hashes fileName = do
            hash <- hashFile fileName
            let fn = combine dirName fileName
            existing <- maybe (return []) (notify hash fn) $ Map.lookup hash hashes
            return $ Map.insert hash (fn:existing) hashes

        notify hash fileName existing = do
            putStrLn $ printf "%32x: %s, %s" hash fileName (head existing)
            return existing

-- hash multiple directories
hashDirectories :: MD5HashMap -> [FilePath] -> IO MD5HashMap
hashDirectories hashes dirs =
    foldM hashDirectory hashes dirs

-- generate a summary of duplicates
generateSummary :: MD5HashMap -> IO ()
generateSummary hashes = do
    putStrLn ""
    putStrLn "Summary"
    putStrLn "======="
    mapM_ summarize $ Map.toList hashes

    where
        summarize (_, [x]) = return ()
        summarize (hash, fileNames) = do
            putStrLn $ printf "%32x: %s" hash (join ", " fileNames)

main :: IO ()
main = do
    argv <- getArgs
    let dirs = nub argv

    if dirs == [] then
        putStrLn "ERROR: At least one directory should be specified."
    else
        hashDirectories Map.empty dirs >>= generateSummary
