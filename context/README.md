# [context][]

[![Version badge][]][version]

🚧 This README is under construction and could use some love. 🚧

`context` provides thread-indexed storage around arbitrary context
values. The interface supports nesting context values per thread, and at
any point, the calling thread may ask for their current context.

## Synopsis

```haskell
{-# LANGUAGE BlockArguments #-}

import qualified Context

data Thing = Thing
  { stuff :: Int
  }

main :: IO ()
main = do
  Context.withEmptyStore \store -> do
    Context.use store Thing { stuff = 1 } do
      Context.use store Thing { stuff = 2 } do
        thing2 <- Context.mine store
        -- ...
      number1 <- Context.mines store stuff
```

See the Haddocks for more info on the library.

[context]: https://github.com/jship/context/context
[Version badge]: https://img.shields.io/hackage/v/context?color=brightgreen&label=version&logo=haskell
[version]: https://hackage.haskell.org/package/context
