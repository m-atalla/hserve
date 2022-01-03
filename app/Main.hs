module Main where
import Network



import System.IO

data Person = Person { name :: String,
                       age :: Int}
main = do
    content <- readFile "app/index.html"
    s <- listenOn (PortNumber 8080)
    (h, hostName, portNum) <- Network.accept s
    putStrLn $ "Connection from " ++ hostName ++ ":" ++ show portNum
    hPutStrLn h "HTTP/1.1 200 OK\r\n\r\n"
    hPutStr h content
    hClose h
