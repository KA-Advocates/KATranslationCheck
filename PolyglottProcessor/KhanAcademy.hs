{-# LANGUAGE OverloadedStrings #-}
-- module KhanAcademy where

import Network.HTTP.Conduit
import Data.Default
import Text.Regex.TDFA.ByteString.Lazy
import Text.Regex.TDFA
import Data.Maybe
import Data.ByteString.Lazy (ByteString)
import qualified Data.ByteString.Lazy as LBS
import System.IO.Unsafe (unsafeDupablePerformIO)

parseCrowdinLanguages :: ByteString -> IO ()
parseCrowdinLanguages lbs =
    let rgx = "https?://[a-z0-9]*\\.cloudfront\\.net/images/flags/([^\\.]+)\\.png" :: ByteString
        res = lbs =~ rgx :: [[ByteString]]
        f [_, lang] = Just lang
        f _ = Nothing
    in mapMaybe f res

listCrowdinLanguages :: Manager -> IO [ByteString]
listCrowdinLanguages mgr = do
    request <- parseUrl "https://crowdin.com/project/khanacademy"
    res <- httpLbs request mgr
    let resLBS = responseBody res
    parseCrowdinLanguages $ resLBS
    return undefined

main :: IO ()
main = do
    -- 60 timeout
    manager <- newManager $ tlsManagerSettings {managerResponseTimeout = Just 60000000}
    print =<< listCrowdinLanguages manager