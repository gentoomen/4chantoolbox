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


-- Note: To run this you will obviously need the relevant packages from hackage
-- or your distro.

import Data.List ( nub )
import System.IO
import Network.Curl
import Network.Curl.Easy
import Text.Regex.TDFA
import System.Environment ( getArgs )

writeToFile handle string = do
    hPutStr handle string

getImage s = do
    curl <- initialize
    putStr $ "Getting image: " ++ show s
    outFile <- openFile (filePath s) WriteMode
    setopt curl (CurlWriteFunction $ easyWriter (writeToFile outFile) )
    setopt curl (CurlURL s)
    hSetBinaryMode outFile True
    perform curl
    hClose outFile
    putStr " ... Done!\n"
  where
    filePath s = s =~ "[0-9]{13}.[A-Za-z0-9]+"

main = withCurlDo $ do
    args <- getArgs
    curl <- initialize
    bodyText <- curlGetString (last args) []
    let images = nub $ (show bodyText =~ "http://images.4chan.org/[A-Za-z0-9]+/src/[0-9]{13}.[A-Za-z0-9]+")
    putStr $ "Page retrieved. " ++ show (length images) ++ " images found.\n"
    mapM_ getImage (concat images)
