To compile targets in defined in `hserve.cabal` file:
```bash
    cabal build
```

To run the server:
```bash
    cabal run
```
The server will start at **http://localhost:3000**

To run the program in interactive mode (ghci with all the modules and libraries loaded):
```bash
    cabal repl
```

## Possible errors
If port 3000 is already being used on your machine, please change the server port in Config.hs
```haskell
    port = "<NEW_PORT>"
```

## Required library: defined in hserve.cabal
- base [4.16.0.0](https://hackage.haskell.org/package/base)
- time
- network [3.1.2.7](https://hackage.haskell.org/package/network)
- bytestring [0.11.2.0](https://hackage.haskell.org/package/bytestring)
- directory [1.3.7.0](https://hackage.haskell.org/package/directory)
