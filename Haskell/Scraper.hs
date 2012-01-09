-- Copyright (C) 2011 John Anthony
-- 
-- This program is free software: you can redistribute it and/or modify
-- it under the terms of the GNU General Public License as published by
-- the Free Software Foundation, either version 3 of the License, or
-- (at your option) any later version.
-- 
-- This program is distributed in the hope that it will be useful,
-- but WITHOUT ANY WARRANTY; without even the implied warranty of
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
-- GNU General Public License for more details.
-- 
-- You should have received a copy of the GNU General Public License
-- along with this program.  If not, see <http://www.gnu.org/licenses/>.

-- Depends on, from Hackage:
-- * curl
-- * regex-tdfa
-- The rest should be distributed with the Haskell Platform.
{-# LANGUAGE DeriveDataTypeable #-}

import Control.Monad    (unless, forM_)
import Data.List        (nub)
import Data.ByteString as B (writeFile)
import Network.Curl
import Network.Curl.Easy
import Text.Regex.TDFA
import System.Environment (getArgs)
import System.Directory
import System.Console.CmdArgs
import Download

getImage :: String -> String -> IO Bool
getImage s o = 
    doesFileExist filename >>= \x ->
        if x
            then
                return False
            else do
                Right a <- openURI s
                B.writeFile filename a
                return True
  where 
    filename = (o ++ "/" ++ (s =~ "[0-9]{13}.[A-Za-z0-9]+"))

data Args = Args
    { output    :: String
    , uri       :: String
    , quiet     :: Bool
    } deriving (Show, Data, Typeable)

defaultArgs = Args
    { output    = "./" 
        &= help "Where to download the images."     
        &= typ "DIR"
    , uri       = "boards.4chan.org/g/" 
        &= help "The URI to scrape images from."
        &= typ "URI"
    , quiet     = False                 
        &= help "If specified, nothing is printed."
    }   &= summary "4chan image scraper alpha beater."

main = do
    args        <- cmdArgs defaultArgs
    curl        <- initialize
    (cc, body)  <- curlGetString (uri args) []
    let images = concat $ nub $ show body =~ "http://images.4chan.org/[A-Za-z0-9]+/src/[0-9]{13}.[A-Za-z0-9]+"
    if (cc /= CurlOK)
        then putStrLn $ "Error: " ++ show cc ++ ". Exiting."
        else if quiet args
                then withCurlDo $
                    forM_ images (\a -> getImage a (output args))
                else withCurlDo $ do
                    putStrLn $ "Page retrieved. " ++ show (length images) ++ " images found."
                    forM_ images $ \a -> do
                            got <- getImage a (output args)
                            if got
                                then putStrLn $ "Downloaded " ++ show a
                                else putStrLn $ "Skipped    " ++ show a
