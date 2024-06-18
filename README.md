# Linear Programming in Haskell using the HiGHS toolkit

This is a Haskell library that helps create and solve linear programs using the [HiGHS](https://github.com/ERGO-Code/HiGHS) programming toolkit.

## Example

The following simple example is a translation [from Wikipedia](https://en.wikipedia.org/wiki/Linear_programming#Example):

```haskell
import qualified Data.Map.Strict as Map
import qualified Data.Vector as Vector
import           Numeric.LinearProgramming

main = do
    let 
        -- The objective function is a linear function, composed of coefficients (4 and 3) 
        -- and variables ("x1" and "x2").
        -- We use `*:` function to create terms, and `+:` to add them.
        objective = (4 *: "x1") +: (3 *: "x2")

        -- Constraints are linear functions bounded by values.
        -- We create the linear functions using *: and +:, 
        -- and bound their values using `boundedBy` and one of:
        --  * `from`: value bounded to the left;
        --  * `upto`: value bounded to the right;
        --  * `to`  : value in a range
        constraints = Vector.fromList 
                      [ (1 *: "x1") +: (1 *: "x2") `boundedBy` (0 `to` 10)
                      , (3 *: "x1") +: (6 *: "x2") `boundedBy` (0 `to` 48)
                      , (4 *: "x1") +: (2 *: "x2") `boundedBy` (0 `to` 32)
                      ]
        
        -- Bounds are constraints on the values of specific variables 
        bounds = Map.fromList 
                 [ ("x1", from 0)
                 , ("x2", from 0) 
                 ]

        lp = linearProgram Maximize -- `Maximize` or `Minimize` the objective 
                           objective 
                           constraints 
                           bounds

    print (solve lp)
```

## Building

**At the moment, only Linux is supported.**

For convenience, HiGHS is included in this repository as a third-party submodule.

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