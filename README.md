# Linear Programming in Haskell using the HiGHS toolkit

This is a Haskell library that helps create and solve linear programs.

## Example

The following simple example is a translation [from Wikipedia](https://en.wikipedia.org/wiki/Linear_programming#Example):

```haskell
import Numeric.LinearProgramming

main = do
    let objective = (4 *: "x1") +: (3 *: "x2")
        constraints = [ (1 *: "x1") +: (1 *: "x2") `boundedBy` (0 `to` 10)
                      , (3 *: "x1") +: (6 *: "x2") `boundedBy` (0 `to` 48)
                      , (4 *: "x1") +: (2 *: "x2") `boundedBy` (0 `to` 32)
                      ] 
        bounds = [ ("x1", from 0)
                 , ("x2", from 0) 
                 ]

        lp = linearProgram Maximize objective constraints bounds
        sol = solve lp
    
    print sol
```

## Building

**At the moment, only Linux is supported.**

`highs` is a Haskell library which binds to the [HiGHS](https://github.com/ERGO-Code/HiGHS) programming toolkit. For convenience,
HiGHS is included in this repository as a third-party submodule.

To build HiGHS, you can use the `install_highs.sh` script. Then, you can build the `highs` library using `cabal`:

```bash
# Ensure that HiGHS is cloned
git submodule update --init

# Build and install HiGHS
chmod +x ./install_highs.sh
sudo ./install.sh

# Build the Haskell library
cabal build
```