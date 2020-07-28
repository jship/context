---
title: Context Is Everything\*
author: Jason Shipman | jship
patat:
  wrap: true
  margins:
    left: 10
    right: 10
  incrementalLists: true
...

# Context Is Everything\*

## Outline

<!-- Set terminal width x height to 100x32, and remember to ENHANCE!!!  -->

Throughout this talk, we'll explore the `context` library and we'll aim
to answer the following questions:

- 🔎 What is it?
- 🧰 How can we use it?
- 🔧 How does it work? (during Q&A?)

# 🔎 What is it?

## High-level gist

The `context` package provides:

- **Thread-indexed storage** around arbitrary context values 🧵 + 🔑 + 📦
- **Nesting** context values per thread ⤵️
- **Context retrieval** for the calling thread 🙋‍♀️
- **Context propagation** to "child" threads 🧵💌🧵

. . .

Let's dive in on each one of these points.

## Thread-indexed storage 🧵 + 🔑 + 📦

**Thread-indexed storage** means we have a shared place that can be read
from and written to by the threads in our application.

. . .

The `context` package provides an opaque type for this:

```haskell
data Store ctx
```

. . .

The `Store` is parametrically polymorphic, so we can store context
values of whatever specific type we're interested in.

. . .

We can acquire an **empty**  `Store` value via `withEmptyStore`:

```haskell
withEmptyStore :: (Store ctx -> IO a) -> IO a
```

## Thread-indexed storage 🧵 + 🔑 + 📦

We can register context in a `Store` on behalf of the calling thread via
`use`, **for the duration of the provided action**:

```haskell
use :: Store ctx -> ctx -> IO a -> IO a
```

. . .

Let's start a running example:

. . .

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
      -- ...
```

## Nesting context values per thread ⤵️

Context values can be **nested**, in that we can override a thread's
current context in a subsequent inner action. When that inner action
completes, the thread's current context **reverts to the value
registered before that action**.

. . .

Continuing our example, with the preamble omitted for brevity:

. . .

```haskell
data Thing = Thing
  { stuff :: Int
  }

main :: IO ()
main = do
  Context.withEmptyStore \store -> do
    Context.use store Thing { stuff = 1 } do
      Context.use store Thing { stuff = 2 } do
        -- ...
```

## Context retrieval 🙋‍♀️

The calling thread can ask for its registered context via `mine` and friends:

```haskell
mine  :: Store ctx -> IO ctx
mines :: Store ctx -> (ctx -> a) -> IO a
```

. . .

Continuing our example:

. . .

```haskell
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
      -- ...
```

## Context retrieval 🙋‍♀️

What happens if we try to retrieve context before any has been registered?

. . .

```haskell
data Thing = Thing
  { stuff :: Int
  }

main :: IO ()
main = do
  Context.withEmptyStore \store -> do
    thingy <- Context.mine store  -- Git rekt!!!
    Context.use store Thing { stuff = 1 } do
      Context.use store Thing { stuff = 2 } do
        thing2 <- Context.mine store
        -- ...
      number1 <- Context.mines store stuff
      -- ...
```

. . .

ANSWER! A `NotFoundException` is thrown and the app go 💥.

## Context retrieval 🙋‍♀️

We can avoid the app exploding into flames in a few ways.

. . .

One option (besides catching the exception) is to leverage friends of
`mine` and `mines`:

```haskell
mineMay  :: Store ctx -> IO (Maybe ctx)
minesMay :: Store ctx -> (ctx -> a) -> IO (Maybe a)
```

. . .

These functions are guaranteed to never throw a `NotFoundException`, but
the onus is on us to handle the `Nothing` case.

## Context retrieval 🙋‍♀️

We couuuuuuld do that and go full `Maybe` with things, but:

- It ain't no fun
- We often have some base context applicable to all threads
- Really though, it ain't no fun

. . .

Enter `withNonEmptyStore`:

```haskell
data Thing = Thing
  { stuff :: Int
  }

main :: IO ()
main = do
  Context.withNonEmptyStore Thing { stuff = 0 } \store -> do
    thing0 <- Context.mine store  -- Did not git rekt!!!
    Context.use store Thing { stuff = 1 } do
      Context.use store Thing { stuff = 2 } do
        thing2 <- Context.mine store
        -- ...
      number1 <- Context.mines store stuff
      -- ...
```

## Context retrieval 🙋‍♀️

Acquiring a **non-empty store** requires specifying a context value to
use as a default when the calling thread has no registered context.

```haskell
withNonEmptyStore :: ctx -> (Store ctx -> IO a) -> IO a
```

. . .

`mine` and `mines` are guaranteed to never throw a `NotFoundException`
when applied to a **non-empty** `Store`. 🥳

## Context propagation 🧵💌🧵

So far, we haven't seen any multi-threaded business.

. . .

What happens if we fork a thread from a "parent" thread that has a
registered context, and from the "child" thread we ask for context?

. . .

```haskell
import qualified Control.Concurrent as Concurrent

data Thing = Thing
  { stuff :: Int
  }

main :: IO ()
main = do
  Context.withEmptyStore \store -> do
    Context.use store Thing { stuff = 1 } do
      threadId <- Concurrent.forkIO do
        isItThing1QuestionMark <- Context.mine store
        -- ...
```

. . .

ANSWER! A `NotFoundException` is thrown and the app go 💥.

## Context propagation 🧵💌🧵

But we avoided 💥-ing before via `withNonEmptyStore`. Let's try that out 
here too:

. . .

```haskell
data Thing = Thing
  { stuff :: Int
  }

main :: IO ()
main = do
  Context.withNonEmptyStore Thing { stuff = 0 } \store -> do
    Context.use store Thing { stuff = 1 } do
      threadId <- Concurrent.forkIO do
        thing0 <- Context.mine store
        -- ...
```

. . .

The default context in a **non-empty** store is just that: a default
value to use in the case when the calling thread has no registered
context.

## Context propagation 🧵💌🧵

What if we put a lot of effort into establishing context for one thread
and we wanted that thread's "child" threads to have this
context too?

. . .

One option is put in the ol' functional elbow grease of passing values
to functions:

```haskell
data Thing = Thing
  { stuff :: Int
  }

main :: IO ()
main = do
  Context.withNonEmptyStore Thing { stuff = 0 } \store -> do
    let thing1 = Thing { stuff = 1 } -- M A X I M U M  E F F O R T
    Context.use store thing1 do
      threadId <- Concurrent.forkIO do
        Context.use store thing1 do
          thing1' <- Context.mine store
          -- ...
```

## Context propagation 🧵💌🧵

Should we have to think about manually propagating context though?

. . .

ANSWER! Nope!!! \*

. . .

```haskell
data Thing = Thing
  { stuff :: Int
  }

main :: IO ()
main = do
  Context.withNonEmptyStore Thing { stuff = 0 } \store -> do
    Context.use store Thing { stuff = 1 } do
      threadId <- Context.forkIO do  -- sneaky sneaky!
        thing1 <- Context.mine store
        -- ...
```

. . .

`Context.Concurrent` is a drop-in replacement for `Control.Concurrent`
and is re-exported from `Context`.

. . .

\* We may wish to not propagate context at all depending on our use case

. . .

We'll see a real case for explicitly choosing to not propagate context later.

# 🧰 How can we use it?

## Using `context`

The examples we've seen up til now have been... contrived. 🥺

. . .

For the rest of the talk, we'll see how we can put `context` to use in
real-world stuff:

- Hierarchical logging
- Resource-sharing
- `servant` tidiness

. . .

The fun is about to begin! 😎

## Hierarchical logging

Let's build a logging system that allows us to "name" sections of code:

. . .

We want output like this:

```
main: Neat-o program is starting up
main.init: Acquiring resources
main.init.db: Building connection pool
main.init.db: Connection pool built
main.init: Resources acquired
main.app: Doing app stuff
main: Neat-o program is exiting
```

. . .

Naming a chunk of code should "inherit" the name of the enclosing
chunk.

. . .

This is **hierarchical logging**, and features along these lines are
available in some popular libraries, like `Log4j`.

## Hierarchical logging

Fortunately for us, building a simple hierarchical logging system will
be a cakewalk with `context`.

. . .

We'll start with everyone's favorite part of Haskell talks: the preamble.

```haskell
import Data.Text (Text)
import Prelude hiding (log)
import qualified Context
import qualified Data.Text.IO as Text.IO
```

## Hierarchical logging

Our `Logger` type will be a newtype wrapper around a `Store`:

```haskell
newtype Logger = Logger { store :: Context.Store LogFunc }

type LogFunc = Text -> IO ()
```

. . .

We can acquire a logger via `withStdoutLogger`:

```haskell
withStdoutLogger :: Text -> (Logger -> IO a) -> IO a
withStdoutLogger rootName f = do
  Context.withNonEmptyStore logFunc \store -> do
    f Logger { store }
  where
  logFunc text = Text.IO.putStrLn $ rootName <> text
```

. . .

So we we delegate to `text` for actually writing to `stdout`, but we
prefix all messages with the `rootName`.

## Hierarchical logging

Now let's write a function to log a message with our `Logger`:

```haskell
log :: Logger -> Text -> IO ()
log Logger { store } text = do
  logFunc <- Context.mine store
  logFunc $ ": " <> text
```

. . .

Nothing fancy here. We ask for the calling thread's logging function and
call it with the `Text` to log. We are careful to prefix the message
with a colon, to visually delineate between the name and the message.

. . .

So far, using our logging interface would look like this:

```haskell
main :: IO ()
main = do
  withStdoutLogger "main" \logger -> do
    -- Logs "main: Neat-o program is starting up"
    log logger "Neat-o program is starting up"

    -- Logs "main: Neat-o program is exiting"
    log logger "Neat-o program is exiting"
```

## Hierarchical logging

Now we'll build the hierarchical bit:

```haskell
named :: Logger -> Text -> IO a -> IO a
named Logger { store } name action = do
  Context.adjust store toNamed action
  where
  toNamed :: LogFunc -> LogFunc
  toNamed logFunc = \text -> logFunc $ "." <> name <> text
```

. . .

The above uses a new `context` function we haven't seen yet:

```haskell
adjust :: Store ctx -> (ctx -> ctx) -> IO a -> IO a
adjust store f action = do
  adjustedContext <- mines store f
  use store adjustedContext action
```

. . .

`adjust` is a convenience function for building a new context value from
the currently-registered value.

## Hierarchical logging

And that's it! We now have a simple, hierarchical logging interface.

. . .

Let's write a program with it to produce the following logs:

```
main: Neat-o program is starting up
main.init: Acquiring resources
main.init.db: Building connection pool
main.init.db: Connection pool built
main.init: Resources acquired
main.app: Doing app stuff
main.app.thread: Doing app stuff on a separate thread
main: Neat-o program is exiting
```

## Hierarchical logging

Tada! 🎊

```haskell
main :: IO ()
main = do
  withStdoutLogger "main" \logger -> do
    log logger "Neat-o program is starting up"
    named logger "init" do
      log logger "Acquiring resources"
      named logger "db" do
        log logger "Building connection pool"
        -- ... Do DB pool things
        log logger "Connection pool built"
      log logger "Resources acquired"
    named logger "app" do
      log logger "Doing app stuff"
      named logger "thread" do
        threadDone <- Context.newEmptyMVar
        _threadId <- Context.forkIO do
          log logger "Doing app stuff on a separate thread"
          -- ... Do stuff
          Context.putMVar threadDone ()
        Context.takeMVar threadDone
    log logger "Neat-o program is exiting"
```

## Resource-sharing

Next up on how we can use `context`: resource-sharing.

. . .

But we'll need to build up a case for wanting resource-sharing in the
first place.

. . .

Let's get the imports out of the way:

```haskell
import Data.Pool (Pool)
import Data.Text (Text)
import Database.PostgreSQL.Simple (Connection)
import Prelude
import qualified Data.Pool as Pool
import qualified Database.PostgreSQL.Config as PG
```

## Resource-sharing

Imagine we have a small DB interface for users and talks:

```haskell
data User = User { userName :: Text }

newtype UserKey = UserKey { unUserKey :: Text }

addUser :: Pool Connection -> User -> IO UserKey
addUser connPool user = do
  Pool.withResource connPool \conn ->
    pure $ UserKey "stub"

data Talk = Talk
  { talkTitle :: Text
  , talkSpeaker :: UserKey
  }

newtype TalkKey = TalkKey { unTalkKey :: Text }

addTalk :: Pool Connection -> Talk -> IO TalkKey
addTalk connPool talk = do
  Pool.withResource connPool \conn ->
    pure $ TalkKey "stub"
```

## Resource-sharing

We can use this DB interface like this:

```haskell
main :: IO ()
main = do
  pgConf <- loadPGConfig
  connPool <- fmap PG.unPGPool $ PG.createPGPool pgConf

  userKey <- addUser connPool User { userName = "SPJ" }
  talkKey <- addTalk connPool Talk
    { talkTitle = "Elastic sheet-defined functions"
    , talkSpeaker = userKey
    }

  -- ...
```

. . .

But is there a problem?

. . .

ANSWER! There is no way for us to share the same connection across the
calls to `addUser` and `addTalk` (without some really annoying hacks,
that is).

## Resource-sharing

One possible solution would be to add some internal functions that do
the actual interfacing with the DB, taking in an explicit `Connection`:

```haskell
addUser :: Pool Connection -> User -> IO UserKey
addUser connPool user = do
  Pool.withResource connPool \conn ->
    addUser' conn user

addUser' :: Connection -> User -> IO UserKey
addUser' conn user = -- ...

addTalk :: Pool Connection -> Talk -> IO TalkKey
addTalk connPool talk = do
  Pool.withResource connPool \conn ->
    addTalk' conn talk

addTalk' :: Connection -> Talk -> IO TalkKey
addTalk' conn talk = -- ...
```

. . .

When adding a user or adding a talk in isolation, we can use the
higher-level `addUser` and `addTalk`, but when we want to share the
connection, perhaps in a transaction, we can use `addUser'` and
`addTalk'`.

## Resource-sharing

But we're Haskellers! Another solution would be to abstract over the
`Pool`:

```haskell
addUser :: (forall r. (Connection -> IO r) -> IO r) -> User -> IO UserKey
addUser withConn user = do
  withConn \conn ->
    pure $ UserKey "stub"

addTalk :: (forall r. (Connection -> IO r) -> IO r) -> Talk -> IO TalkKey
addTalk withConn talk = do
  withConn \conn ->
    pure $ TalkKey "stub"
```

## Resource-sharing

This is a nice solution in that it empowers the **caller** to share the
connection if they choose to, and we don't have to maintain two sets of
DB functions.

. . .

Here is our updated running example where a connection is not shared:

```haskell
main :: IO ()
main = do
  -- ...
  let withConn = Pool.withResource connPool
  userKey <- addUser withConn User { userName = "SPJ" }
  talkKey <- addTalk withConn Talk
    { talkTitle = "Elastic sheet-defined functions"
    , talkSpeaker = userKey
    }
  -- ...
```

## Resource-sharing

And here is that same example, but this time we as the **caller** are
choosing to share the same connection:

```haskell
main' :: IO ()
main' = do
  -- ...
  let withConn = Pool.withResource connPool
  withConn \conn -> do
    userKey <- addUser ($ conn) User { userName = "SPJ" }
    talkKey <- addTalk ($ conn) Talk
      { talkTitle = "Elastic sheet-defined functions"
      , talkSpeaker = userKey
      }
    -- ...
```

## Resource-sharing

It would seem that we have solved our problem without the use of
`context`.

. . .

What gives?

. . .

Often, we close over the connection-acquiring function so that we don't
have to pass it around everywhere:

```haskell
data UserDB = UserDB
  { addUser :: User -> IO UserKey
  , findUser :: UserKey -> IO User
  -- ...
  }

mkUserDB :: (forall r. (Connection -> IO r) -> IO r) -> UserDB
mkUserDB withConn =
  UserDB
    { addUser = \user -> do
        withConn \conn -> do
          pure $ UserKey "stub"
    , findUser = -- ...
    -- ...
    }
```

## Resource-sharing

And similarly for the `Talk` stuff:

```haskell
data TalkDB = TalkDB
  { addTalk :: Talk -> IO TalkKey
  , findTalk :: TalkKey -> IO Talk
  -- ...
  }

mkTalkDB :: (forall r. (Connection -> IO r) -> IO r) -> TalkDB
mkTalkDB withConn =
  TalkDB
    { addTalk = \talk -> do
        withConn \conn -> do
          pure $ TalkKey "stub"
    , findTalk = -- ...
    -- ...
    }
```

## Resource-sharing

But now we're back to square one and cannot share the same connection
across our `UserDB` and `TalkDB` components:

```haskell
main :: IO ()
main = do
  -- ...
  let withConn = Pool.withResource connPool
  let userDB = mkUserDB withConn
  let talkDB = mkTalkDB withConn
  userKey <- addUser userDB User { userName = "SPJ" }
  talkKey <- addTalk talkDB Talk
    { talkTitle = "Elastic sheet-defined functions"
    , talkSpeaker = userKey
    }
  -- ...
```

. . .

The connection-acquiring function is fixed at startup. 😭

## Resource-sharing

Never fear, `context` is here!

. . .

Let's build a general resource `Provider` type, that uses the approach
we took to abstract over the `Pool`'s resource-acquiring function as a
guiding principle:

. . .

```haskell
-- | An opaque resource provider.
newtype Provider res = Provider
  { store :: Context.Store (WithRes res)
  }

-- | Helper newtype.
newtype WithRes res = WithRes (forall r. (res -> IO r) -> IO r)

withProvider
  :: (forall r. (res -> IO r) -> IO r)
  -> (Provider res -> IO a)
  -> IO a
withProvider withRes f = do
  Context.withStore Context.noPropagation (Just (WithRes withRes)) \store -> do
    f Provider { store }
```

## Resource-sharing

And the core operations we can apply to our `Provider`:

```haskell
withResource :: Provider res -> (res -> IO a) -> IO a
withResource Provider { store } f = do
  WithRes withRes <- Context.mine store
  withRes f

shareResource :: Provider res -> res -> IO a -> IO a
shareResource Provider { store } resource action = do
  Context.use store (WithRes ($ resource)) action
```

. . .

And we're done!

## Resource-sharing

Let's fix our example:

```haskell
mkUserDB :: Provider Connection -> UserDB
mkUserDB connProvider =
  UserDB
    { addUser = \user -> do
        withResource connProvider \conn -> do
          pure $ UserKey "stub"
    , findUser = -- ...
    }

mkTalkDB :: Provider Connection -> TalkDB
mkTalkDB connProvider =
  TalkDB
    { addTalk = \talk -> do
        withResource connProvider \conn -> do
          pure $ TalkKey "stub"
    , findTalk = -- ...
    }
```

## Resource-sharing

And now `main`:

```haskell
main :: IO ()
main = do
  -- ...
  let withConn = Pool.withResource connPool
  withProvider withConn \connProvider -> do
    let userDB = mkUserDB connProvider
    let talkDB = mkTalkDB connProvider
    userKey <- addUser userDB User { userName = "SPJ" }
    talkKey <- addTalk talkDB Talk
      { talkTitle = "Elastic sheet-defined functions"
      , talkSpeaker = userKey
      }
    -- ...
```

In the above, `addUser` and `addTalk` are each grabbing fresh
`Connection`s.

## Resource-sharing

But now we have restored the power as the **caller** to explicitly share
a connection when needed:

```haskell
main :: IO ()
main = do
  -- ...
  let withConn = Pool.withResource connPool
  withProvider withConn \connProvider -> do
    let userDB = mkUserDB connProvider
    let talkDB = mkTalkDB connProvider
    withResource connProvider \conn -> do
      shareResource connProvider conn do
        userKey <- addUser userDB User { userName = "SPJ" }
        talkKey <- addTalk talkDB Talk
          { talkTitle = "Elastic sheet-defined functions"
          , talkSpeaker = userKey
          }
        -- ...
```

🥳

. . .

What about `ReaderT` + `local`? 🤔

## `servant` tidiness

Let's suppose we're tasked with building out some CRUD endpoints, so we
get this `servant` API going:

```haskell
haskellLoveAPI :: Proxy HaskellLoveAPI
haskellLoveAPI = Proxy

type HaskellLoveAPI = ToServantApi HaskellLoveRoutes

data HaskellLoveRoutes route = HaskellLoveRoutes
  { newAttendee :: route :-
     Summary "Create a Haskell Love attendee"
       :> "user" :> ReqBody '[JSON] User :> Post '[JSON] UserKey

  , getAttendee :: route :-
     Summary "Retrieve a Haskell Love attendee"
       :> "user" :> Capture "user-id" UserKey :> Get '[JSON] User

  , newTalk :: route :-
     Summary "Create a Haskell Love talk"
       :> "talk" :> ReqBody '[JSON] Talk :> Post '[JSON] TalkKey

  -- ... and many more endpoints
  } deriving stock (Generic)
```

## `servant` tidiness

We also build out a corresponding `IO` interface:

```haskell
data Service = Service
  { newAttendee :: User -> IO UserKey
  , getAttendee :: UserKey -> IO User
  , newTalk :: Talk -> IO TalkKey

  -- ... and many more functions
  }
```

. . .

We can hoist an implementation of this interface up into a `servant`
server according to the API definition. The hoisting code has been
omitted for brevity.

## `servant` tidiness

Things are going well, but then we realize our endpoints are
wide-open!

. . .

No authentication, no authorization, no nothing!

. . .

What to do?

. . .

😱

## `servant` tidiness

One option would be to roll up the sleeves and update all our endpoints
to take in an `Authorization` header:

```haskell
type AuthHeader = Header' '[Required, Strict] "Authorization" JWTBytes

data HaskellLoveRoutes route = HaskellLoveRoutes
  { newAttendee :: route :-
     Summary "Create a Haskell Love attendee"
       :> AuthHeader :> "user" :> ReqBody '[JSON] User :> Post '[JSON] UserKey

  , getAttendee :: route :-
     Summary "Retrieve a Haskell Love attendee"
       :> AuthHeader :> "user" :> Capture "user-id" UserKey :> Get '[JSON] User

  , newTalk :: route :-
     Summary "Create a Haskell Love talk"
       :> AuthHeader :> "talk" :> ReqBody '[JSON] Talk :> Post '[JSON] TalkKey

  -- ... and many more endpoints
  } deriving stock (Generic)
```

## `servant` tidiness

We'd then update all the functions in our `IO` interface to take in the
authorization header bytes too:

```haskell
data Service = Service
  { newAttendee :: JWTBytes -> User -> IO UserKey
  , getAttendee :: JWTBytes -> UserKey -> IO User
  , newTalk :: JWTBytes -> Talk -> IO TalkKey

  -- ... and many more functions
  }
```

. . .

But this is kind of sad...

. . .

This `JWTBytes` value is something we require all our endpoints to
have. If every single endpoint requires it, it sure seems a shame to
have to explicitly deal with it in our `servant` API and our `IO`
interface.

. . .

On top of all that, we should be able to **authenticate** the request
somewhere higher up the server stack. Otherwise, every one of our
endpoints is going to be responsible for remembering to authenticate
the request. Ideally, our endpoints would only have to worry about
**authorizing** the request.

## `servant` tidiness

`wai` has a partial solution for this problem: `Middleware` + `Vault`

```haskell
type Middleware = Application -> Application
type Application =
       Request -> (Response -> IO ResponseReceived) -> IO ResponseReceived

vaultersGonnaVault :: Vault.Key JWTClaims -> Middleware
vaultersGonnaVault vaultKey app = \request sendResponse -> do
  let authenticationFailure =
        sendResponse
          $ responseLBS status401 [("Content-Type", "text/plain")] "Nice try!"
  case lookup hAuthorization $ requestHeaders request of
    Nothing -> authenticationFailure
    Just jwtBytes -> do
      jwtClaims <- authenticate authenticationFailure jwtBytes
      let vault' = Vault.insert vaultKey jwtClaims $ vault request
      let request' = request { vault = vault' }
      app request' sendResponse

authenticate :: IO a -> ByteString -> IO JWTClaims
authenticate onFailure jwtBytes = -- ... actually do the authentication!
```

## `servant` tidiness

The `Middleware` + `Vault` approach solves one of our problems:
**authenticating** the request higher up the server stack and letting us
pass the resulting user info to our endpoints.

. . .

But to use this technique, we still need to update every endpoint in our
`servant` API **and** every function in our `IO` interface to have a
`Vault` as a parameter... 👎 

. . .

```haskell
data HaskellLoveRoutes route = HaskellLoveRoutes
  { newAttendee :: route :-
     Summary "Create a Haskell Love attendee"
       :> Vault :> "user" :> ReqBody '[JSON] User :> Post '[JSON] UserKey

  , getAttendee :: route :-
     Summary "Retrieve a Haskell Love attendee"
       :> Vault :> "user" :> Capture "user-id" UserKey :> Get '[JSON] User

  , newTalk :: route :-
     Summary "Create a Haskell Love talk"
       :> Vault :> "talk" :> ReqBody '[JSON] Talk :> Post '[JSON] TalkKey

  -- ... and many more endpoints
  } deriving stock (Generic)
```

## `servant` tidiness

And here's the updated `IO` interface:

```haskell
data Service = Service
  { newAttendee :: Vault -> User -> IO UserKey
  , getAttendee :: Vault -> UserKey -> IO User
  , newTalk :: Vault -> Talk -> IO TalkKey

  -- ... and many more functions
  }
```

. . .

In both this `Vault` case and the previous explicit `JWTClaims` case,
it is sad to have to marry our service interface to concerns of a
specific implementation of an HTTP server.

. . .

What if we want to use the same `Service` interface to define a DB-only
implementation? Or maybe an HTTP client? An in-memory server for use in
testing?

. . .

The interface itself shouldn't need to be concerned with
implementation-specific things like a `Vault` from a WAI `Request`, or
the `JWTClaims` in the explicit case.

## `servant` tidiness

It's probably no surprise at this point, but `context` can help.

. . .

We'll use an approach similar to the `Middleware` + `Vault` example:

```haskell
addJWTClaims :: Context.Store JWTClaims -> Middleware
addJWTClaims jwtClaimsStore app = \request sendResponse -> do
  let authenticationFailure =
        sendResponse
          $ responseLBS status401 [("Content-Type", "text/plain")] "Nice try!"
  case lookup hAuthorization $ requestHeaders request of
    Nothing -> authenticationFailure
    Just jwtBytes -> do
      jwtClaims <- authenticate authenticationFailure jwtBytes
      Context.use jwtClaimsStore jwtClaims do
        app request sendResponse
```

## `servant` tidiness

Using this `Middleware`, our `servant` API and `IO` interface signatures
**go totally untouched**, yet every one of our endpoints now implicitly
has access to the `JWTClaims` for the corresponding user requesting into
our server:

. . .

```haskell
mkIOServer :: Context.Store JWTClaims -> Service
mkIOServer jwtClaimsStore =
  Service
    { newAttendee = \attendee -> do
        requestingJWTClaims <- Context.mine jwtClaimsStore
        -- ... do stuff

      -- ... and so on
    }
```

🥳

## End

```
 ______________________
< See you in the Q&A!! >
 ----------------------
    \                                  ___-------___
     \                             _-~~             ~~-_
      \                         _-~                    /~-_
             /^\__/^\         /~  \                   /    \
           /|  O|| O|        /      \_______________/        \
          | |___||__|      /       /                \          \
          |          \    /      /                    \          \
          |   (_______) /______/                        \_________ \
          |         / /         \                      /            \
           \         \^\\         \                  /               \     /
             \         ||           \______________/      _-_       //\__//
               \       ||------_-~~-_ ------------- \ --/~   ~\    || __/
                 ~-----||====/~     |==================|       |/~~~~~
                  (_(__/  ./     /                    \_\      \.
                         (_(___/                         \_____)_)
```

# 🔧 How does it work?

## How it works according to Mr. Lightyear

```
            _._                           _._
           ||||                           ||||
           ||||_           ___           _||||
           |  ||        .-'___`-.        ||  |
           \   /      .' .'_ _'. '.      \   /
           /~~|       | (| b d |) |       |~~\
          /'  |       |  |  '  |  |       |  `\
,        /__.-:      ,|  | `-' |  |,      :-.__\       ,
|'-------(    \-''""/.|  /\___/\  |.\""''-/    )------'|
|         \_.-'\   /   '-._____.-'   \   /'-._/        |
|.---------\   /'._| _    .---. ===  |_.'\   /--------.|
'           \ /  | |\_\ _ \=v=/  _   | |  \ /          '
             `.  | | \_\_\ ~~~  (_)  | |  .'
               `'"'|`'--.__.^.__.--'`|'"'`
                   \                 /
                    `,..---'"'---..,'
                      :--..___..--:
                                          Credit: jgs
```

. . .

*Buzz:* "Brackets... Brackets everywhere."

## How it works according to the code

We omitted the definition of `Store` before, but here it is in full:

```haskell
data Store ctx = Store
  { ref :: IORef (State ctx)
  , key :: Unique
  }
```

. . .

So a `Store` maintains some `State` and a mysterious `key`.

## How it works according to the code

The `State` that is managed by the `Store`:

```haskell
data State ctx = State
  { stacks :: Map ThreadId [ctx]
  , def :: Maybe ctx
  }
```

. . .

The `stacks` field in `State` captures the essence of what `context` is
all about: **thread-indexed storage**. 🧵 + 🔑 + 📦

## How it works according to the code

The value type in `Map ThreadId [ctx]` being a list (read: stack) is
what powers each thread being able to **nest** context values. ⤵️

. . .

Here is the definition of `use`:

```haskell
use :: Store ctx -> ctx -> IO a -> IO a
use store context = Exception.bracket_ (push store context) (pop store)
```

. . .

`push` and `pop` are a couple internal helpers. Here is `push`:

```haskell
push :: Store ctx -> ctx -> IO ()
push Store { ref } context = do
  threadId <- Concurrent.myThreadId
  IORef.atomicModifyIORef' ref \state@State { stacks } ->
    case Map.lookup threadId stacks of
      Nothing ->
        (state { stacks = Map.insert threadId [context] stacks }, ())
      Just contexts ->
        (state { stacks = Map.insert threadId (context : contexts) stacks}, ())
```

## How it works according to the code

The **context retrieval** functions are all implemented in terms of
`mineMay`. 🙋‍♀️

. . .

Here is an ever-so-slightly simplified version of that function:

. . .

```haskell
mineMay :: Store ctx -> IO (Maybe ctx)
mineMay Store { ref } = do
  threadId <- Concurrent.myThreadId
  State { stacks, def } <- IORef.readIORef ref
  pure
    case Map.lookup threadId stacks of
      Nothing -> def
      Just [] -> bug "mineMay"
      Just (context : _rest) -> Just context
```

## How it works according to the code

We saw `withEmptyStore` and `withNonEmptyStore` previously, and
these two functions are both implemented in terms of the lower-level
`withStore`:

. . .

```haskell
withStore
  :: PropagationStrategy
  -> Maybe ctx
  -> (Store ctx -> IO a)
  -> IO a
```

. . .

`withStore` allows for controlling the propagation behavior of
`Context.Concurrent`, and is a great choice for library authors using
`context` as an implementation detail.

. . .

There are currently two propagation strategies available to us:

```haskell
defaultPropagation :: PropagationStrategy
noPropagation      :: PropagationStrategy
```

. . .

Diving into the gory details of `withStore` and that mysterious `key`
we saw earlier is somewhat out of scope for this talk, but these
ingredients are what enable automatic **context propagation**. 🧵💌🧵

