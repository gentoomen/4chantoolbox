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
import System.Console.CmdArgs
import System.IO
import Download

getImage :: String -> String -> IO ()
getImage s o = do 
    Right a <- openURI s
    B.writeFile (o ++ "/" ++ (s =~ "[0-9]{13}.[A-Za-z0-9]+")) a

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
        &= help "The URI to scrape images from."    &= typ "URI"
    , quiet     = False                 
        &= help "If specified, nothing is printed."
    }   &= summary "4chan image scraper alpha beater."

main :: IO ()
main = withCurlDo $ do
    args <- cmdArgs defaultArgs
    curl <- initialize
    bodyText <- curlGetString (uri args) []
    let images = nub $ show bodyText =~ "http://images.4chan.org/[A-Za-z0-9]+/src/[0-9]{13}.[A-Za-z0-9]+"
    unless (quiet args) $ putStrLn $ "Page retrieved. " ++ show (length images) ++ " images found."
    forM_ (concat images) $
        \a -> do 
            getImage a (output args)
            unless (quiet args) $ putStrLn $ "Downloaded " ++ show a ++ 
                if output args == "./"
                    then "."
                    else "to" ++ show (output args) ++ "."