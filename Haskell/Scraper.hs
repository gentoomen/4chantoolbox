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

import Control.Monad (unless, forM_)
import Data.List (nub)
import qualified Data.ByteString.Internal as B
import qualified Data.ByteString          as B
import Data.IORef
import Foreign
import Foreign.ForeignPtr
import Network.Curl
import Network.Curl.Easy
import Text.Regex.TDFA
import System.Environment (getArgs)
import System.Console.CmdArgs
import System.IO

-- pasted from Network.Curl.Download, by Don Stewart 
data P = P !(Ptr Word8) !Int
newtype URL = URL String

-- memcpy chunks of data into our bytestring.
writer :: ((Ptr Word8, Int) -> IO ()) -> WriteFunction
writer f src sz nelems _ = do
    let n' = sz * nelems
    f (castPtr src, fromIntegral n')
    return n'

gather :: IORef P -> WriteFunction
gather r = writer $ \(src, m) -> do
    P dest n <- readIORef r
    dest' <- reallocBytes dest (n + m)
    B.memcpy (dest' `plusPtr` n) src (fromIntegral m)
    writeIORef r (P dest' (n + m))

parseURL :: String -> Maybe URL
parseURL s = Just (URL s) -- no parsing

getFile :: URL -> [CurlOption] -> IO (Either String B.ByteString)
getFile (URL url) flags = do
    h <- initialize
    let start = 1024
    buf  <- mallocBytes start
    ref  <- newIORef (P buf 0)
    setopt h (CurlFailOnError True)
    setDefaultSSLOpts h url
    setopt h (CurlURL url)
    setopt h (CurlWriteFunction (gather ref))
    mapM_ (setopt h) flags
    rc        <- perform h
    P buf' sz <- readIORef ref
    if rc /= CurlOK
        then do
            free buf'
            return $ Left (show rc)
        else do
            fp <- newForeignPtr finalizerFree buf'
            return (Right $! B.fromForeignPtr fp 0 (fromIntegral sz))

openURI :: String -> IO (Either String B.ByteString)
openURI s = case parseURL s of
    Nothing  -> return $ Left $ "Malformed url: "++ s
    Just url -> do
        e <- getFile url []
        return $ case e of
             Left err   -> Left $ "Failed to connect: " ++ err
             Right src  -> Right src
-- end snippets from Network.Curl.Download

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
    { output    = "./"                  &= help "Where to download the images."     &= typ "DIR"
    , uri       = "boards.4chan.org/g/" &= help "The URI to scrape images from."    &= typ "URI"
    , quiet     = False                 &= help "If specified, nothing is printed."
    } &= summary "4chan image scraper alpha beater."

main :: IO ()
main = withCurlDo $ do
    args <- cmdArgs defaultArgs
    curl <- initialize
    bodyText <- curlGetString (uri args) []
    let images = nub $ show bodyText =~ "http://images.4chan.org/[A-Za-z0-9]+/src/[0-9]{13}.[A-Za-z0-9]+" :: [[String]]
    unless (quiet args) $ putStrLn $ "Page retrieved. " ++ show (length images) ++ " images found."
    forM_ (concat images) $
        \a -> do 
            getImage a (output args)
            unless (quiet args) $ putStrLn $ "Downloaded " ++ show a ++ 
                if output args == "./"
                    then "."
                    else "to" ++ show (output args) ++ "."