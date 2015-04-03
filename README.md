# diagramming
This project aims to be something similar to processing.org except using haskell.
Something anyone can download and run to very quickly start making graphical code for fun and profit without needing to wrestle with cabal and ghc installation.

## Building
```bash
  # The usual cabal dance
  cabal sandbox init
  cabal configure
  cabal build

  # On OSX: To package it up as an OSX app bundle:
  runghc Setup configure
  sudo runghc Setup build # Needs sudo to scan for dynamic system libs to package up
  # Then run it with:
  open dist/build/diagramming.app
```
