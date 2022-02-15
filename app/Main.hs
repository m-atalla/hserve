module Main (main) where

import Control.Concurrent (forkFinally)
import qualified Control.Exception as E
import Control.Monad (forever, void)
import qualified Data.ByteString as S
import qualified Data.ByteString.Char8 as C
import Network.Socket
import Network.Socket.ByteString(recv, sendAll)
import HTTP
import System.Directory (doesFileExist)
import System.IO (withFile, IOMode (ReadMode), hFileSize)
import Data.Word (Word16)


host :: HostName
host = "127.0.0.1"

port :: ServiceName
port = "3000"

serviceURL :: HostName -> ServiceName -> String
serviceURL h p = "http://" ++ h ++ ":" ++ p

main :: IO ()
main =
    putStrLn ("Starting server on on:" ++ serviceURL host port) >>
    runTCPServer (Just host) port hserve
  where
    hserve s = do
        msg <- recv s 1024
        let rawReq = unpackRequest msg
        let req = parseRequestHead . head $ rawReq
        let path = resolvePath req
        pathExist <- doesFileExist path
        if pathExist then do
            let code = evalMethod $ method req

            let resPath = evalPath path code

            -- Content length (impure operation)
            len <- fileLength resPath

            -- Response body (resource being requested)
            resource <- C.readFile resPath

            -- constructing Response
            let res = Response (ver req) code (statusMsg code) (resOHeaders path len)


            sendAll s $ packResponse (show res) resource
        else
            C.readFile "resource/404.html" >>= \c -> sendAll s $ packResponse "HTTP/1.1 404 NOT FOUND\r\n\r\n" c
        close s

-- Since ByteString is an instance of Semigroup it
-- is able to concat with another ByteString using (<>) function
packResponse :: String -> C.ByteString -> C.ByteString
packResponse res content = C.pack res <> content


unpackRequest :: C.ByteString -> [String]
unpackRequest msg = lines $ C.unpack msg


-- Runs "server" handlers in parallel
runTCPServer :: Maybe HostName -> ServiceName -> (Socket -> IO a) -> IO a
runTCPServer mhost port server = withSocketsDo $ do
    addr <- resolve
    E.bracket (open addr) close loop
  where
    resolve = do
        let hints = defaultHints {
                addrFlags = [AI_PASSIVE],
                addrSocketType = Stream
            }
        head <$> getAddrInfo (Just hints) mhost (Just port)
    open addr = E.bracketOnError (openSocket addr) close $ \sock -> do
        setSocketOption sock ReuseAddr 1
        withFdSocket sock setCloseOnExecIfNeeded
        bind sock $ addrAddress addr
        listen sock 1024
        return sock
    loop sock = forever $ E.bracketOnError (accept sock) (close . fst)
        $ \(conn, _peer) -> void $
            -- 'forkFinally' alone is unlikely to fail thus leaking @conn@,
            -- but 'E.bracketOnError' above will be necessary if some
            -- non-atomic setups (e.g. spawning a subprocess to handle
            -- @conn@) before proper cleanup of @conn@ is your case
            forkFinally (server conn) (const $ close conn) 