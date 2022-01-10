module Main where
import Network ( accept, PortNumber, Socket )
import Network.Socket

import System.IO

main :: IO ()
main = do
    sock <- createSocket 8080
    handleRequest sock

createSocket :: PortNumber -> IO Socket 
createSocket port = do
    sock <- socket AF_INET Stream defaultProtocol
    setSocketOption sock ReuseAddr 1  
    bind sock (SockAddrInet port iNADDR_ANY)
    listen sock 2
    return sock


handleRequest :: Socket -> IO ()
handleRequest sock = do
    content <- readFile "app/index.html"
    (h, hostName, portNum) <- Network.accept sock
    putStrLn $ "Connection from " ++ hostName ++ ":" ++ show portNum
    hPutStrLn h "HTTP/1.1 200 OK\r\n\r\n"
    hPutStr h content
    hClose h
    handleRequest sock