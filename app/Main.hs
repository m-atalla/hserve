module Main (main) where

import Control.Concurrent (forkFinally, threadDelay)
import qualified Control.Exception as E
import Control.Monad (forever, void, forM_)
import qualified Data.ByteString as S
import qualified Data.ByteString.Char8 as C
import Network.Socket
import Network.Socket.ByteString(recv, sendAll)
import HTTP
import System.Directory (doesFileExist)
import System.IO (withFile, IOMode (ReadMode), hFileSize)
import GHC.Conc


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
    hserve s result = do
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
            sendNotFoundResponse s

        -- atomically allows performing STM actions inside IO actions
        atomically $ commitUpdate result
        responseCounter <- atomically $ readTVar result

        putStr $ "\r" ++ show responseCounter

        close s


sendNotFoundResponse :: Socket -> IO ()
sendNotFoundResponse socket = C.readFile "resource/404.html" >>=
    \c -> sendAll socket $ packResponse "HTTP/1.1 404 NOT FOUND\r\n\r\n" c

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