module Config (host, port, root) where

type HostName = String
type ServiceName = String
type RootPath = String

host :: HostName
host = "127.0.0.1"

port :: ServiceName
port = "3000"

root :: RootPath 
root = "resource/"
