module Download (openURI) where

--------------------------------------------------------------------
--
-- Module 	 : Download
-- Copyright : (c) Don Stewart
-- License   : BSD3
--
-- A snippet from Network.Curl.Download, a binding to Curl.
--
--------------------------------------------------------------------

import qualified Data.ByteString.Internal as B
import qualified Data.ByteString          as B
import Data.IORef
import Network.Curl
import Foreign
import Foreign.ForeignPtr
import System.IO

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