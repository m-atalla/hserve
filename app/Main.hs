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
import GHC.Conc
import qualified Time 

type ThreadResult = TVar Int

host :: HostName
host = "127.0.0.1"

port :: ServiceName
port = "3000"

serviceURL :: HostName -> ServiceName -> String
serviceURL h p = "http://" ++ h ++ ":" ++ p

main :: IO ()
main = do
    putStrLn ("Starting server on on:" ++ serviceURL host port)
    responseTVar <- newTVarIO 0
    runTCPServer (Just host) port hserve responseTVar
  where
    hserve skt result = do
        -- receiving request as bytestring
        msg <- recv skt 1024
        let reqList = unpackRequest msg

        -- parse first line of the request
        -- (e.g. "GET / HTTP/1.1")
        let req = parseRequestHead . head $ reqList

        let path = resolvePath req

        serverDateTime <- Time.getServerTime 

        pathExist <- doesFileExist path

        -- TODO: Error 404 error handling shouldn't be as explicit as it is right now?
        if pathExist then do
            let code = evalMethod $ method req

            let resPath = evalPath path code

            -- Content length (impure operation)
            len <- fileLength resPath

            -- Response body (resource being requested)
            resource <- C.readFile resPath

            -- constructing Response
            let optionalHeaders = resOHeaders path len serverDateTime 
            let res = Response (ver req) code (statusMsg code) optionalHeaders

            sendAll skt $ packResponse (show res) resource
        else
            -- TODO: add optional headers to 404
            sendNotFoundResponse skt

        -- atomically allows performing STM actions inside IO actions
        atomically $ commitUpdate result
        responseCounter <- readTVarIO result

        putStr $ "\r" ++ show responseCounter

        close skt


sendNotFoundResponse :: Socket -> IO ()
sendNotFoundResponse skt = do
    let resPath = "resource/404.html"
    c <- C.readFile resPath
    sendAll skt $ packResponse "HTTP/1.1 404 NOT FOUND\r\n\r\n" c

-- Since ByteString is an instance of Semigroup it
-- is able to concat with another ByteString using (<>) function
packResponse :: String -> C.ByteString -> C.ByteString
packResponse res content = C.pack res <> content


unpackRequest :: C.ByteString -> [String]
unpackRequest msg = lines $ C.unpack msg


-- Gets a TVar int
commitUpdate :: ThreadResult -> STM ()
commitUpdate result = do
    instances <- readTVar result
    writeTVar result (succ instances)

-- Runs "server" handlers in parallel
runTCPServer :: Maybe HostName -> ServiceName -> (Socket -> ThreadResult -> IO a) -> ThreadResult -> IO a
runTCPServer mhost port server threadResult = withSocketsDo $ do
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
            forkFinally (server conn threadResult) (const $ close conn)
