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

import List
import System.IO
import Network.Curl
import Network.Curl.Easy
import Text.Regex.TDFA
import Text.Printf
import System ( getArgs )


getImage s = do
    printf "Getting image: %s " s
    outFile <- openFile (filePath s) WriteMode
    hSetBinaryMode outFile True
    (_, imageStr) <- curlGetString s []
    hPutStr outFile imageStr
    printf " ... Done!\n"
  where
    filePath s = s =~ "[0-9]{13}.[A-Za-z0-9]+"

main = withCurlDo $ do
    args <- getArgs
    curl <- initialize
    bodyText <- curlGetString (last args) []
    let images = nub $ (show bodyText =~ "http://images.4chan.org/[A-Za-z0-9]+/src/[0-9]{13}.[A-Za-z0-9]+")::[[String]]
    mapM_ getImage (concat images)
