# Change log

## 0.2.0.2

* Link test suite with `-threaded` RTS option

## 0.2.0.1

* Backwards compatibility with GHC 8.4 (@pbrisbin)

## 0.2.0.0

* Lift signatures from `IO` to `MonadIO`, `MonadThrow` and `MonadMask`
* Type parameter order is now explicit via `ScopedTypeVariables`
* `WithRes` has new monad type parameter
* Dependency bump: `context-0.2.0.0`

## 0.1.0.0

* Initial release
