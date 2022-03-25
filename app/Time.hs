module Time (getServerTime) where

import Data.Time.Clock ( getCurrentTime )
import Data.Time.Format ( defaultTimeLocale, formatTime )

-- Get Current server string time
-- format is based on sepc found here:
-- https://developer.mozilla.org/en-US/docs/Web/HTTP/Headers/Date
getServerTime :: IO String
getServerTime = do
    formatTime defaultTimeLocale "%a, %d %b %Y %H:%M:%S GMT" <$> getCurrentTime