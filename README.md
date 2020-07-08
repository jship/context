# [context][]

[![Build badge][]][build]
[![Version badge: context][]][version: context]
[![Version badge: context-wai-middleware][]][version: context-wai-middleware]
[![Version badge: context-http-client][]][version: context-http-client]

🚧 This README is under construction and could use some love. 🚧

This repo houses the core `context` library and libraries built on top
of `context`. The `context` package provides thread-indexed storage
around arbitrary context values. The interface supports nesting context
values per thread, and at any point, the calling thread may ask for
their current context.

See the Haddocks of each package for more info on the libraries.

[context]: https://github.com/jship/context
[Build badge]: https://img.shields.io/travis/jship/context?logo=travis
[build]: https://travis-ci.org/jship/context
[Version badge: context]: https://img.shields.io/hackage/v/context?color=brightgreen&label=context&logo=haskell
[version: context]: https://hackage.haskell.org/package/context
[Version badge: context-wai-middleware]: https://img.shields.io/hackage/v/context-wai-middleware?color=brightgreen&label=context-wai-middleware&logo=haskell
[version: context-wai-middleware]: https://hackage.haskell.org/package/context-wai-middleware
[Version badge: context-http-client]: https://img.shields.io/hackage/v/context-http-client?color=brightgreen&label=context-http-client&logo=haskell
[version: context-http-client]: https://hackage.haskell.org/package/context-http-client
