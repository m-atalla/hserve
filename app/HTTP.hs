module HTTP where
import System.IO (withFile, IOMode (ReadMode), hFileSize)
import Control.Exception ( try,  SomeException )


-- Type aliases
type Path = String
type Extension = String
type StatusCode = Int


-- Unsupported methods: POST, PUT, PATCH, DELETE, etc..
data Method = GET | NOT_IMPLEMENTED
    deriving (Eq, Show)

data Request = Request {
    method :: Method,
    path :: Path,
    ver :: String
}

-- Utility, used for debugging so far
instance Show Request where
    show request =
        "method: " ++ show (method request) ++
        "\npath:" ++ path request ++
        "\nver:" ++  ver request

data HeaderField = HeaderField {
    key :: String,
    value :: String
}

instance Show HeaderField where
    show header = key header ++ ": " ++ value header ++ "\r\n"

data Response = Response {
    version :: String,
    status :: Int,
    statusStr :: String,
    hFields :: [HeaderField]
}
instance Show Response where
    show response =
        -- Response Status
        joinH ([version, show . status, statusStr] <*> [response])
        -- Optional Headers
        -- HeaderField derive Show in their own way as well that ends in CRLF
        ++ concat [show h | h <- hFields response] 
        ++ "\r\n" -- CRLF end of response headers

resolveMethod :: String -> Method
resolveMethod sm
    | sm == "GET" = GET
    | otherwise   = NOT_IMPLEMENTED

parseRequestHead :: String -> Request
parseRequestHead raw = eat $ words raw
    where
        eat request = case request of 
            (m:p:v:_)   -> Request (resolveMethod m) p v
            _           -> error "Invalid HTTP Request"

resolvePath :: Request -> String
resolvePath req = case reqPath of [] -> error "Invalid Path"
                                  "/" -> "resource/index.html"
                                  reqPath -> "resource/" +/+ reqPath
    where reqPath = path req

statusMsg :: StatusCode -> String
statusMsg code
    | code == 200 = "OK"
    | code == 404 = "Not Found"
    | code == 405 = "Method Not Allowed"
    | otherwise = "Unavailable"

evalMethod :: Method -> StatusCode
evalMethod m = if m == GET then 200 else 405

evalPath :: Path -> StatusCode -> Path
evalPath p code = if code == 200 then p else "resource/405.html"

-- concats a string with a space and ends line with CRLF
joinH :: [String] -> String
joinH = foldr (\x acc-> x ++ " " ++ acc) "\r\n"


-- Response optional headers
resOHeaders :: Path -> Maybe Integer -> String -> [HeaderField]
resOHeaders path len dateTime=
    [
        HeaderField "Server" "Hserve/0.2",
        HeaderField "Content-Type" $ contentType path,
        HeaderField "Content-Length" $ maybe "0" show len,
        HeaderField "Date" dateTime
    ]

-- Get File length
-- TODO: should this be in new `FS` module?
fileLength :: FilePath -> IO (Maybe Integer)
fileLength path = do
    size <- try $ withFile path ReadMode hFileSize :: IO (Either SomeException Integer)
    case size of
        Left _ -> return Nothing
        Right n -> return $ Just n

contentType :: Path -> String
contentType p = contentTypeGen $ splitOn p '.'

contentTypeGen :: Extension -> String
contentTypeGen ext = case ext of
                                "html" -> "text/html"
                                "png"  -> "image/png"
                                "ico"  -> "image/vnd.microsoft.icon"
                                "jpg"  -> "image/jpeg"
                                "jpeg" -> "image/jpeg"
                                "css"  -> "text/css"
                                "js"   -> "text/javascript"
                                "gif"  -> "image/gif"
                                "json" -> "application/json"
                                -- Default case is a text/html 
                                -- which is the content type of error pages.
                                _      -> "text/html"

-- Splits a string using a character
-- The resulting string DOES NOT include the split character
splitOn :: String -> Char -> String
splitOn []     _  = []
splitOn (x:xs) ch = if x == ch then xs else splitOn xs ch


-- joins paths of a directory
-- sort of a safe (++) for file paths that avoids duplicated '/'
(+/+) :: Path -> Path -> Path
(+/+) a b =
    if last a == '/' && head b == '/' then
        init a ++ "/" ++ tail b
    else
        a ++ b
