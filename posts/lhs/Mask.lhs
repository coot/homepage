Masking Asynchronous Exceptions
===============================

> {-# LANGUAGE ScopedTypeVariables #-}
> module Mask where

> import Control.Concurrent.MVar
> import Control.Exception
> import GHC.IO.Handle.Types (Handle__)

The base library explains asynchronous exceptions and masking quite well, but
still this is one of the topics that is often misunderstood and some of its
crucial parts like interruptible operations are not well enough documented to
slip under the radar too often.

Synchronous vs Asynchronous exceptions
--------------------------------------

There are two main ways of throwing exceptions in Haskell, either with
<code>[throwIO](https://hackage.haskell.org/package/base-4.14.1.0/docs/Control-Exception.html#v:throwIO)</code> or
<code>[throwTo](https://hackage.haskell.org/package/base-4.14.1.0/docs/Control-Exception.html#v:throwTo)</code>.
<code>throwIO</code> throws an exception in the current thread in synchronous way,
<code>[throwTo](https://hackage.haskell.org/package/base-4.14.1.0/docs/Control-Exception.html#v:throwTo)</code>
allows to throw an exception in some other thread, hence the name asynchronous
exceptions.  But let's start from the beginning, why we even need asynchronous
exceptions?  This is nicely answered in the paper [Asynchronous Exceptions in
Haskell](https://www.microsoft.com/en-us/research/wp-content/uploads/2016/07/asynch-exns.pdf):

<blockquote>
  <ul>
    <li> **Speculative computation.** A parent thread might start a child thread to compute
      some value speculatively; later the parent thread might decide that it does
      not need the value so it may want to kill the child thread.</li>

    <li> **Timeouts**: If some computation does not complete within a specified time
      budget, it should be aborted.</li>

    <li> **User interrupt.** Interactive systems often need
      to cancel a computation that has already been started, for example when the
      user clicks on the “stop” button in a web browser.</li>

    <li> **Resource exhaustion.** Most Haskell implementations use a stack and heap,
      both of which are essentially finite resources, so it seems reasonable to
      inform the program when memory is running out, in order that it can
      take remedial action.  Since such exceptions can occur at almost any program
      point, it is natural to treat them as asynchronous. </li>
  </ul>
</blockquote>

Asynchronous exceptions can interrupt almost any computation, masking is
provided as a way to make it predictable, so we can reason about it.  Let's
examine this standard example:

> withLockUnmasked :: MVar () -> IO a -> IO a
> withLockUnmasked lock k = do
>   takeMVar lock
>   a <- catch k (\(e :: SomeException) -> putMVar lock ()
>                                       >> throwIO e)
>   putMVar lock () 
>   return a

The problem with `withLockUnmasked` is that an asynchronous exception could be
thrown just after <code>takeMVar</code> is executed but before the
<code>catch</code> installs the handler.  To be able to fix this, Haskell
provides primitive operations which allow to mask asynchronous exceptions.
Each thread keeps the following masking state (original haddocks preserved):

> -- | Describes the behaviour of a thread when an asynchronous
> -- exception is received.
> data MaskingState
>   = Unmasked
>   -- ^ asynchronous exceptions are unmasked (the normal state)
>   | MaskedInterruptible
>   -- ^ the state during 'mask': asynchronous exceptions are masked, but
>   -- blocking operations may still be interrupted
>   | MaskedUninterruptible
>   -- ^ the state during 'uninterruptibleMask': asynchronous exceptions are
>   -- masked, and blocking operations may not be interrupted


Let us stress that in <code>MaskedInterruptible</code> state, which is
a result of using
<code>[mask](https://hackage.haskell.org/package/base-4.14.1.0/docs/GHC-IO.html#v:mask)</code>
function, asynchronous exceptions can be thrown, but only by interruptible
operations. In the <code>MaskedUninterruptible</code> asynchronous exceptions
cannot be thrown even by blocking / interruptible operations.
[Asynchronous Exceptions in
Haskell](https://www.microsoft.com/en-us/research/wp-content/uploads/2016/07/asynch-exns.pdf)
paper specifies interruptible operations as:

<blockquote>
Any operation which may need to wait indefinitely for a resource
(e.g.,takeMVar) may receive asynchronous exceptions even within an
enclosing mask, but only while the resource is unavailable.Such operations are
termed interruptible operations.
</blockquote>

The complete list of interruptible operations is astonishingly short:
<ul>
  <li><code>takeMVar</code> when the <code>MVar</code> is empty,</li>
  <li><code>putMVar</code> when the <code>MVar</code> is non-empty,</li>
  <li><a
href="https://hackage.haskell.org/package/stm/docs/Control-Monad-STM.html#v:retry"><code>retry :: STM a</code></a>,</li>
  <li><a href=""><code>throwTo</code></a></li>
  <li><a
href="https://downloads.haskell.org/~ghc/latest/docs/html/users_guide/exts/ffi.html#interruptible-foreign-calls">interrutible ffi calls</a>.</li>
  <li style="display: none">there are also some primitive operations in non-threaded RTS, which are interruptible, but this will be eventually fixed.</li>
</ul>
In particular none of the operations listed below are interruptible:
<ul>
  <li><code>IORef</code> operations,</li>
  <li><code>safe</code> and <code>unsafe</code> foreign calls.  There
    is a subtle difference between <code>safe</code> and <code>unsafe</code>
    ffi: when a safe ffi returns haskell thread will check if there are pending
    asynchronous exceptions while haskell rts does not know about
    <code>unsafe</code> ffi calls.</li>
</ul>

We have two ways of fixing <code>withLockUnamsked</code>:

> withLockMaskedInterruptible :: MVar () -> IO a -> IO a
> withLockMaskedInterruptible lock k = mask $ \unmask -> do
>   takeMVar lock
>   a <- catch (unmask k)
>              (\(e :: SomeException) -> putMVar lock ()
>                                     >> throwIO e)
>   putMVar lock () 
>   return a
>
> withLockMaskedUninterruptible :: MVar () -> IO a -> IO a
> withLockMaskedUninterruptible lock k = uninterruptibleMask $ \unmask -> do
>   takeMVar lock
>   a <- catch (unmask k)
>              (\(e :: SomeException) -> putMVar lock ()
>                                     >> throwIO e)
>   putMVar lock () 
>   return a

The difference between both of them is subtle.  To get to the point we need to
analyse which operations are blocking / interruptible.  As specified in
[Asynchronous Exceptions in
Haskell](https://www.microsoft.com/en-us/research/wp-content/uploads/2016/07/asynch-exns.pdf),
<code>takeMVar</code> is blocking only if <code>v :: MVar ()</code> is
empty. We have two cases:

* *<code>v</code> is non-empty:* neither of the two can raise
  asynchronous exception while executing <code>takeMVar</code>.  This means that
  both implementations will install the exception handler before an asynchronous
  exception is raised, and this is what we wanted.

* *if <code>v</code> is empty:* the semantics of <code>MVar</code> ensures that
  <code>takeMVar</code> is interruptible until it is empty, once
  <code>takeMVar</code> takes the value it becomes non-blocking.  This ensure
  that asynchronous exceptions can be raised by
  <code>withLockMaskedInterruptible</code> only when <code>takeMVar</code> is
  blocked.  This also means that <code>withLockMaskedInterruptible</code> will
  install the catch handler once <code>takeMVar</code> past the point of being
  interruptible.

The crucial difference between <code>withLockMaskedInterruptible</code> and
<code>withLockMaskedUninterruptible</code> is that the later will never throw
an async exception while <code>takeMVar</code> is blocked while the lock is
empty (e.g. taken by some other thread).

It seem that analysing the code which is using <code>mask</code> is more
difficult than when using <code>uninterruptibleMask</code>, is it really so?
The main problem with <code>withLockMaskedUninterruptible</code> is that it
can potentially introduce deadlocks; a program might become unresponsive:
interruptions like one delivered by signals (e.g. <code>CTRL-C</code>) are
delivered using asynchronous exceptions; or it could introduce undeliverable
timeouts, which in networking applications can introduce safety hazards.
Because deadlocks are non-local properties there is actually no way to analyse
if <code>withLockMaskedUninterruptible</code> is safe or not without its
context, it depends on the program where it is used.  For this reasons the
documentation of <code>uninterruptibleMask</code> says in capital letters:
THIS SHOULD BE USED WITH GREAT CARE.  In my experience, debugging asynchronous
exceptions is easier than debugging deadlocks.  When logging is done right you
can see asynchronous exceptions in the logs, but you will not see a bunch of
threads being deadlocked.

Takeaways from this are:

* [mask](https://hackage.haskell.org/package/base-4.14.1.0/docs/GHC-IO.html#v:mask)
  masks asynchronous exceptions only in non-interruptible code allowing to
  raise asynchronous exceptions in blocking operations;
* [uninterruptibleMask](https://hackage.haskell.org/package/base-4.14.1.0/docs/GHC-IO.html#v:uninterruptibleMask)
  masks asynchronous exceptions, but it can introduce deadlocks;
* <code>takeMVar</code> is only blocking if the <code>MVar</code> is empty.


'base' and 'safe-exceptions' / 'unliftio'
--------------------------------------

Both [safe-exceptions](https://hackage.haskell.org/package/safe-exceptions)
and
[unliftio](https://hackage.haskell.org/package/unliftio) are using `bracket`
implementation which is masking using `uninterruptibleMask`, while
[base](https://hackage.haskell.org/package/base) package is using `mask`.
Which one is more appropriate in a library?

This is
[base](https://hackage.haskell.org/package/base-4.14.1.0/docs/Control-Exception-Base.html#v:bracket)'s
implementation of
<code>[bracket](https://hackage.haskell.org/package/base-4.14.1.0/docs/Control-Exception-Base.html#v:bracket)</code>:

> bracket :: IO a
>         -> (a -> IO b)
>         -> (a -> IO c)
>         -> IO c
> bracket before after thing =
>   mask $ \restore -> do
>     a <- before
>     r <- restore (thing a) `onException` after a
>     _ <- after a
>     return r

The version used by
[unliftio](https://hackage.haskell.org/package/unliftio-0.2.13.1/docs/UnliftIO-Exception.html#v:bracket)
and
[safe-exceptions](https://hackage.haskell.org/package/safe-exceptions-0.1.7.1/docs/Control-Exception-Safe.html#v:bracket)
are both implemented using <code>try</code> but the crucial difference is that
they are using <code>uninterruptibleMask</code> when executing the
<code>after</code> callback.  Since they are using
<code>uninterruptibleMask</code> they need to use <code>try</code> to avoid
blocking exceptions when executing the <code>before</code> handler.   This is
to minimize time when a thread is in <code>MaskedUninterruptible</code>
state.

Let us look at the most common resource handlers:

==== File Handles

The
[base](https://hackage.haskell.org/package/base-4.14.1.0/docs/GHC-IO-Handle.html#t:Handle)
package does not export `Handle` constructors, they are an implementation
detail, so let's bring the definition here:

> data Handle
>   = FileHandle           -- A normal handle to a file
>         FilePath         -- the file (used for error messages
>                          -- only)
>         !(MVar Handle__)
> 
>   | DuplexHandle         -- A handle to a read/write stream
>         FilePath         -- file for a FIFO, otherwise some
>                          --   descriptive string (used for error
>                          --   messages only)
>         !(MVar Handle__) -- The read side
>         !(MVar Handle__)

The
<code>[hClose](https://hackage.haskell.org/package/base/docs/GHC-IO-Handle.html#v:hClose)
:: Handle -> IO ()</code> calls
<code>[hClose'](https://hackage.haskell.org/package/base/docs/src/GHC.IO.Handle.html#hClose%27)</code>
which masks exceptions while calling <code>takeMVar</code> (in
<code>[withHandle'](https://hackage.haskell.org/package/base/docs/src/GHC.IO.Handle.Internals.html#withHandle%27)</code>),
and continues (while exceptions are masked) with
<code>[hClose_help](https://hackage.haskell.org/package/base/docs/src/GHC.IO.Handle.Internals.html#hClose_help)</code>,
which does a few interesting things:
<ul>
  <li>flush a buffer,</li>
  <li>close a decoder,</li>
  <li>call <code>[GHC.IO.Device.close](https://hackage.haskell.org/package/base/docs/GHC-IO-Device.html#v:close)</code>
to close the file descriptor.
  </li>
</ul>

Flushing file handle buffer is done by either safe (non-threaded rts) or
unsafe (threaded rts) ffi call (using
<code>[c_write](https://hackage.haskell.org/package/base/docs/src/System.Posix.Internals.html#c_write)</code>
or
<code>[c_safe_write](https://hackage.haskell.org/package/base/docs/src/System.Posix.Internals.html#c_safe_write)</code>),
and thus is uninterruptible.  Closing the decoder is either a no-op
(<code>return ()</code>) or an unsafe foreign call to
<code>iconv_close</code>.  We are left with analysing
<code>[GHC.IO.Device.close](https://hackage.haskell.org/package/base/docs/GHC-IO-Device.html#v:close)</code>.
<code>Handle__</code> is using <code>IODevice</code> <code>FD</code> instance.
On systems that support either <code>epoll</code>, <code>poll</code> or
<code>kqueue</code> (e.g. on <code>Linux</code>, <code>MacOS</code>,
<code>FreeBSD</code>, and alikes) it is done via event manager.  On other
operating systems (<code>Windows</code>) closing file handle is done by
a direct foreign call (this might change in the future with the new
<code>mio</code> Windows event manager based on I\/O completion ports).  When
event manager is involved, the
<code>[closeFdWith](https://hackage.haskell.org/package/base/docs/GHC-Conc-IO.html#v:closeFdWith)</code>
is used.  I recently fixed a bug which made it interruptible, see [PR
\#4942](https://gitlab.haskell.org/ghc/ghc/-/merge_requests/4942).  However,
because calls which involve <code>epoll</code> are very fast all the blocking
operations done by <code>closeFdWith</code> would block for a very short time,
making it quite unlikely to be an issue (but if you run a service for long
enough it could be observed).  

The conclusion is that closing a file handle could only block on the
<code>TMVar</code> holding <code>Handle__</code>.  This makes it non-blocking
if file handles are not closed concurrently.

====== What about asynchronous usage of file handles?

This would mean trying to close a file handle from multiple threads.  The
question is why one would ever need that?  A good pattern for using file
handles is
[withFile](https://hackage.haskell.org/package/base-4.14.1.0/docs/System-IO.html#v:withFile).
Where the callback is only used to read and parse the content of the file and
return the result.  This is runs in a synchronous way which makes the
<code>bracket</code> used by <code>withFile</code> not leak any resources.
Another design pattern for using file handles comes from streaming libraries
like [pipes](https://hackage.haskell.org/package/pipes),
[streaming](https://hackage.haskell.org/package/streaming) or
[conduit](https://hackage.haskell.org/package/conduit).  To ensure that the
handle is correctly closed, one needs to use
[ResourceT](https://hackage.haskell.org/package/resourcet-1.2.4.2/docs/Control-Monad-Trans-Resource.html#t:ResourceT)
from [resourcet](https://hackage.haskell.org/package/resourcet) package. 
Streaming is synchronous so we are safe when using
<code>[hClose](https://hackage.haskell.org/package/base-4.14.1.0/docs/GHC-IO-Handle.html#v:hClose)</code>.


==== Sockets

The [network](https://hackage.haskell.org/package/resourcet) package
<code>[close](https://hackage.haskell.org/package/network-3.1.2.1/docs/Network-Socket.html#v:close)</code>
calls (for non-Windows, threaded RTS)
<code>[closeFdWith](https://hackage.haskell.org/package/base-4.14.1.0/docs/src/GHC.Event.Thread.html#closeFdWith)</code>
with an uninterruptible FFI function
([c_close](https://hackage.haskell.org/package/network-3.1.2.1/docs/src/Network.Socket.Types.html#line-276)).
Which we already know that is non-blocking.

For non-threaded RTS or on Windows,
<code>[Network.Socket.close](https://hackage.haskell.org/package/network/docs/Network-Socket.html#v:close)</code>
directly calls the uninterruptle
[c_close](https://hackage.haskell.org/package/network/docs/src/Network.Socket.Types.html#line-276).


=== Threads

When dealing with threads as resources the
<code>[killThread](https://hackage.haskell.org/package/base-4.14.1.0/docs/GHC-Conc-Sync.html#v:killThread)</code>
is implemented using
<code>[throwTo](https://hackage.haskell.org/package/base-4.14.1.0/docs/Control-Exception.html#v:throwTo)</code>
which is an interruptible operation.  It blocks until exception is
delivered to the target thread. When using
<code>[killThread](https://hackage.haskell.org/package/base-4.14.1.0/docs/GHC-Conc-Sync.html#v:killThread)</code>
as a resource finaliser we should use <code>uninterruptibleMask_
. killThread</code> instead (this week I fixed exactly such bug,
[see](https://github.com/input-output-hk/ouroboros-network/pull/2851)).  The
same applies when using [async](https://hackage.haskell.org/package/async)
package which exports
<code>[uninterruptibleCancel](https://hackage.haskell.org/package/async-2.2.2/docs/Control-Concurrent-Async.html#v:cancel)
:: Async a -> IO ()</code>.  Let us note that the
<code>[withAsync](https://hackage.haskell.org/package/async-2.2.2/docs/Control-Concurrent-Async.html#v:withAsync)</code>
is internally using
<code>[uninterruptibleMask_](https://hackage.haskell.org/package/base/docs/GHC-IO.html#v:uninterruptibleMask_)</code>


=== Conclusions

Back to [safe-exceptions](https://hackage.haskell.org/package/safe-exceptions)
and [unliftio](https://hackage.haskell.org/package/unliftio).  In my
opinion
the base has a better choice for <code>bracket</code>.  Debugging deadlocks, is
quite hard.  GHC has deadlock detection mechanism but it is not always 
reliable.  Both deadlocks and resource leaks can be silent (no logs), but the
latter are clearly visible when tracking state of the program with some system
tools, or just looking at <code>/proc</code> directly.

<div style="display: none">
What about linear 'bracket'.
</div>

The essential trade-offs between both implementations is which kind of
programmer's errors they allow:

* <code>base</code> implementation allows to use interruptible release
  resource function, which might not run till its completion in presence of an
  asynchronous exception;

* <code>safe-exception</code> implementation allows to make the program
  deadlock (which might even prevent a program from termination when a kill
signal is send, e.g. via `CTRL-C`).


Another common argument for
[safe-exceptions](https://hackage.haskell.org/package/safe-exceptions) is that
it allows to avoid to catch asynchronous exceptions by default.  Catching
<code>[SomeException](https://hackage.haskell.org/package/base-4.14.1.0/docs/GHC-Exception-Type.html#t:SomeException)</code>
can indeed lead to trouble, though for that reason it has been recommended to
catch exceptions which you care about not all of them.  To avoid catching
asynchronous exceptions one can always use the following snippet (though it's
still recommended to be more specific than any synchronous exception!):

```haskell
catchJust (\e ->
            case fromException e :: Maybe SomeAsyncException of
              Just _  -> Nothing
              Nothing -> Just e
          )
```

Above all, use your best judgement how to handle exceptions.  In some
applications using `Either` as a return type is the best option for dealing
with synchronous exceptions, in some critical applications catching any
exceptions at some level (the non-recommended way) is the safest way to avoid
triggering bugs which would use rare execution path.  There is just not
a single way which which suits all applications and it all depends on the
security / threat model under which one is operating.


References
----------

<ul>
  <li>
    [Asynchronous Exceptions in
      Haskell](https://www.microsoft.com/en-us/research/wp-content/uploads/2016/07/asynch-exns.pdf),
    S. Marlow, S.P. Jones, A.Morgan, J. Reppy, December 12, 2006;  Note that the
    specification is using asynchronous <code>throwTo</code> rather than
    a synchronous one as implemented in GHC, the relevant semantic changes are
    discussed in section 9.
  </li>
  <li>
    [Dealing with Asynchronous Exceptions during Resource
      Acquisition](https://www.well-typed.com/blog/97/), D. Coutts, E. de
      Vries, 2014.
  </li>
  <li>
    [Interruptible or uninterreptubile cleanup](https://github.com/fpco/safe-exceptions/issues/3),
    issue in [safe-exceptions](https://hackage.haskell.org/package/safe-exceptions) package.
  </li>
  <li>
    Uninterruptible <code>closeFdWith</code> [merge request \#4942](https://gitlab.haskell.org/ghc/ghc/-/merge_requests/4942)
  </li>
</ul>


<div style="display:none">
Further details
---------------

<code>throwTo</code> will block the current thread until the exception is
thrown in the other thread.  This is an important detail which shouldn't be
missed.  The reasons for it being blocked might be:

* the target thread is invoking a non interruptible foreign call;
* the target thread uninterruptibly masked exceptions;
* the target thread masked exceptions and it is not allocating memory; GHC
  only delivers asynchronous exception when it allocates memory. This is rare
  but can happen in tight loops.

> -- TODO: example for last point

</div>
