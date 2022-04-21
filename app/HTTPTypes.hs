module HTTPTypes where

-- Type aliases
type Path = String
type Extension = String
type StatusCode = Int

-- Request
data Request = Request {
    method :: Method,
    path :: Path,
    ver :: String
}

-- Unsupported methods: POST, PUT, PATCH, DELETE, etc..
data Method = GET | NOT_IMPLEMENTED
    deriving (Eq, Show)

-- Utility, used for debugging so far
instance Show Request where
    show request =
        "method: " ++ show (method request) ++
        "\npath:" ++ path request ++
        "\nver:" ++  ver request

-- Response 
data Response = Response {
    version :: String,
    status :: Int,
    statusStr :: String,
    hFields :: [HeaderField]
}

data HeaderField = HeaderField {
    key :: String,
    value :: String
}

instance Show Response where
    show response =
        -- Response Status
        joinH ([version, show . status, statusStr] <*> [response])
        -- Optional Headers
        -- HeaderField derive Show in their own way as well that ends in CRLF
        ++ concat [show h | h <- hFields response] 
        ++ "\r\n" -- CRLF end of response headers

instance Show HeaderField where
    show header = key header ++ ": " ++ value header ++ "\r\n"

-- concats a string with a space and ends line with CRLF
joinH :: [String] -> String
joinH = foldr (\x acc-> x ++ " " ++ acc) "\r\n"