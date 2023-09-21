# Change log

## 0.2.0.2

* Link test suite with `-threaded` RTS option

## 0.2.0.1

* Backwards compatibility with GHC 8.4 (@pbrisbin)

## 0.2.0.0

* Lift signatures from `IO` to `MonadIO`, `MonadThrow` and `MonadMask`
* Type parameter order is now explicit via `ScopedTypeVariables`
* Add `withAdjusted` convenience function
* Re-export `Context.View` module from `Context.Implicit`

## 0.1.1.1

* Correct Haddocks

## 0.1.1.0

* Add `Context.View` module, which includes the `View` type + `view`, `viewMay`,
  and `toView` functions
* Re-export `Context.View` from `Context`

## 0.1.0.0

* Initial release
