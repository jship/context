# [context][]

[![Build badge][]][build]

This repo houses the core `context` library and libraries built on top
of `context`. The `context` package provides thread-indexed storage
around arbitrary context values. The interface supports nesting context
values per thread, and at any point, the calling thread may ask for
their current context.

See the Haddocks of each package for more info on the libraries.

[context]: https://github.com/jship/context
[Build badge]: https://github.com/jship/context/workflows/ci/badge.svg
[build]: https://github.com/jship/context
