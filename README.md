To run the program:
```bash
    cabal run
```
The server will start at **http://localhost:3000**

To run the program in interactive mode (ghci with all the modules and libraries loaded):
```bash
    cabal repl
```

## Possible errors
If port 3000 is already being used on your machine, please change the server port in Main.hs
```haskell
    -- replace the second argument with a valid port number
    main = runTCPServer (Just "127.0.0.1") "<NEW_PORT>" hserve
```

## Required library: defined in hserve.cabal
- base [4.16.0.0](https://hackage.haskell.org/package/base)
- network [3.1.2.7](https://hackage.haskell.org/package/network)
- bytestring [0.11.2.0](https://hackage.haskell.org/package/bytestring)
- directory [1.3.7.0](https://hackage.haskell.org/package/directory)
