module HTTP where

-- Unsupported methods: POST, PUT, PATCH, DELETE
data Method = GET | NOT_IMPLEMENTED 
    deriving Eq

instance Show Method where
    show m = if m == GET then "GET" else "NOT_IMPLEMENTED"

type Path = String

data Request = Request {
    method :: Method,
    path :: Path,
    ver :: String
}

-- Used for debugging
instance Show Request where
    show request = 
        "method: " ++ show (method request) ++ 
        "\npath:" ++ path request ++
        "\nver:" ++  ver request
        -- ["\n" ++ (key header) ++ ": " ++ (value header) | header <- (headers request)]

data Header = Header {
    key :: String,
    value :: String
}

data Response = Response {
    version :: String,
    status :: Int,
    statusStr :: String
}


resolveMethod :: String -> Method
resolveMethod sm
    | sm == "GET" = GET
    | otherwise   = NOT_IMPLEMENTED

parseRequestHead :: String -> Request
parseRequestHead raw = eat $ words raw
    where 
        eat []        = error "Invalid HTTP Request"
        eat (m:p:v:_) = Request (resolveMethod m) p v

resolvePath :: Request -> String
resolvePath req = case (reqPath) of [] -> error "Invalid Path"
                                    "/" -> "resource/index.html"
                                    reqPath -> "resource/" ++ reqPath
    where reqPath = path req
                                     