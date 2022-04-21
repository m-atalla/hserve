module FileSystem where

import qualified Config (root)
import System.IO (withFile, IOMode (ReadMode), hFileSize)
import Control.Exception ( try, SomeException )
import HTTPTypes (Path, Request(..))

-- joins paths of a directory
-- sort of a safe (++) for file paths that avoids duplicated '/'
(+/+) :: Path -> Path -> Path
(+/+) a b =
    if last a == '/' && head b == '/' then
        init a ++ "/" ++ tail b
    else
        a ++ b

-- Get File length
fileLength :: FilePath -> IO (Maybe Integer)
fileLength filePath = do
    size <- try $ withFile filePath ReadMode hFileSize :: IO (Either SomeException Integer)
    case size of
        Left _ -> return Nothing
        Right n -> return $ Just n


resolvePath :: Request -> String
resolvePath req = case reqPath of
    []  -> error "Invalid Path"
    "/" -> Config.root +/+ "index.html"
    _   -> Config.root +/+ reqPath
  where
    reqPath = path req