module HTTP where
import qualified Config ( root )
import FileSystem ( (+/+) )
import HTTPTypes

resolveMethod :: String -> Method
resolveMethod sm
    | sm == "GET" = GET
    | otherwise   = NOT_IMPLEMENTED

parseRequestHead :: String -> Request
parseRequestHead raw = eat $ words raw
    where
        -- parse request words using the `Request` type constructor
        eat request = case request of 
            (m:p:v:_)   -> Request (resolveMethod m) p v
            _           -> headParsingError request 

headParsingError :: [String] -> a
headParsingError request = error $ "Invalid HTTP init line.\n\
                                    \Expected pattern: '<Method> <Path> <Version>'.\n\
                                    \Received: " ++ concat request

statusMsg :: StatusCode -> String
statusMsg code
    | code == 200 = "OK"
    | code == 404 = "Not Found"
    | code == 405 = "Method Not Allowed"
    | otherwise = "Unavailable"

evalMethod :: Method -> StatusCode
evalMethod m = if m == GET then 200 else 405

evalPath :: Path -> StatusCode -> Path
evalPath p code = if code == 200 then p else Config.root +/+ "/405.html"



-- Response optional headers
resOHeaders :: Path -> Maybe Integer -> String -> [HeaderField]
resOHeaders filePath len dateTime=
    [
        HeaderField "Server" "Hserve/0.2",
        HeaderField "Content-Type" $ contentType filePath,
        HeaderField "Content-Length" $ maybe "0" show len,
        HeaderField "Date" dateTime
    ]


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
