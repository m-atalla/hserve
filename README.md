## Setup
You will need cabal and ghc installed on your system to run this project. I don't use windows as a daily driver but I found on the easiest way to get ghc and cabal running is through chocolatey a package manager for windows.

- Follow the installation guide found [here](https://chocolatey.org/install#individual)

```powershell
    # in admin powershell
    Set-ExecutionPolicy Bypass -Scope Process
```
choco installation
```powershell
Set-ExecutionPolicy Bypass -Scope Process -Force; [System.Net.ServicePointManager]::SecurityProtocol = [System.Net.ServicePointManager]::SecurityProtocol -bor 3072; iex ((New-Object System.Net.WebClient).DownloadString('https://community.chocolatey.org/install.ps1'))
```

- Install ghc and cabal by running this command found here
```powershell
    choco install ghc
```

To run the program:
```powershell
    cabal run
```
The server will start at **http://localhost:3000**

To run the program in interactive mode (ghci with all the modules and libraries loaded):
```powershell
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
