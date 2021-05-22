Pipelining in TypedProtocols
============================

<div class=small>

> {-# LANGUAGE BangPatterns,
>              DataKinds,
>              DerivingStrategies,
>              DerivingVia,
>              EmptyCase,
>              FlexibleInstances,
>              GADTs,
>              PatternSynonyms,
>              PolyKinds,
>              RankNTypes,
>              StandaloneDeriving,
>              StandaloneKindSignatures,
>              TypeApplications,
>              TypeFamilies,
>              TypeOperators,
>              ViewPatterns
> #-}

</div>

> module TypedProtocolPipelining where
>
> import           Data.Kind (Type)
> import           Data.Void


The [typed-protocols] is a framework for writing binary session types in
Haskell.  It has been developed by [IOHK] in collaboration with [Well-Typed].

For network applications it is common to use some form of latency hiding for
performance reasons.  Network is utilised best when constant preassure is
applied to avoid shrinking of the tcp window (e.g. tcp flow control
mechanism).  For these reasons, the _typed-protocol_ package allows to express
[protocol pipelining](https://www.wikiwand.com/en/Protocol_pipelining).  In
addition, protocol pipelining allows to make less context switches and
improves data locality (it would be quite interesting to measure these
claims).

In this blog post we will study pipelinging starting with a simple ping pong
protocol:

<figure class=small>
![PingPong Protocol](/images/ping-pong-0.png)
</figure>

Each arrow designates a message send, the colour of a state signifies its
agency, e.g. who is responsible for sending the message.  For example
`MsgPing` is send by the client (green), it also shifts agency from client to
server: in the `StBusy` state the server holds agency and thus is responsible
for sending messages.  Not all messages need to transfer agency, we will
consider such an extension to the `PingPong` protocol later.

The [typed-protocols] package restricts the way an application executing
a protocol can be build and makes it _correct by construction_.  Using
type-level programing, one can only construct applications which obey state
machine rules faithfuly represented by types.  A detailed exposition of the
package where given at [Haskell-eXchange] by Duncan Coutts, or my longer
workshop at Monadic Party: [part 1][Monadic-Party-1], [part
2][Monadic-Party-2], [part 3][Monadic-Party-3].  For example one might
construct a client which explores the following state changes:

<figure class=small>
![](/images/ping-pong-1.png)
</figure>

The client will send `MsgPing` and await for `MsgPong` before sending next
`MsgPing`.  If we'd like to pipeline all the `MsgPing` the communication would
look like:

<figure class=small>
![](/images/ping-pong-2.png)
</figure>

where now we don't wait for a response before sending next request.

The most important difference between these two diagrams is that the pipelined
version is no longer a continous flow.  Pipelining breaks the composition of
transitions. Instead, we promise to do some of the transitions at a later time.
This delayed processing needs to be managed by the protocol execution
environment.  What is worth noting that we must keep the order of transitions.
In the ping pong protocol this is not readily visible in types as we only
collect `MsgPong` messages, but one could easily imagine a more complex
protocol in which it would matter.  It is not difficult to envision that
queues enter the picture as we need the _first-in first-out_ semantics.

Our current implementation of pipelining in typed-protocols, which in
simplified form we will reconstruct for the purpose of this blog post, is using
a pair of threads communicating through a pair of queues.  The mix of
concurrency and type level programming was a neat idea by Duncan Coutts.
 
In this post we will explore how to re-implement pipelining which does not
require concurrent thread which synchronise through a pair of queues, and and
instead use a single type level queue to faithfuly represent the state of the
protocol at the type level.  As we will see this will improve the way
pipelining can be expressed, at the expense of using advanced type level
programming.


Towards non-pipelined protocol description
------------------------------------------

This is our first simplest attempt to encode a non-pipelined ping-pong client.
The client can be in either of the three states:

> data SimplePingPong where
>   StSimpleIdle :: SimplePingPong
>   StSimpleBusy :: SimplePingPong
>   StSimpleDone :: SimplePingPong

It can send either of the messages:

> data MessageSimplePingPong (st :: SimplePingPong) (st' :: SimplePingPong) where
>   MsgSimplePing :: MessageSimplePingPong StSimpleIdle StSimpleBusy
>   MsgSimplePong :: MessageSimplePingPong StSimpleBusy StSimpleIdle
>   MsgSimpleDone :: MessageSimplePingPong StSimpleIdle StSimpleDone

Now a client can be encoded as a recursive data type (although this
representation is not very useful as it does not allow to do any IO):

> data SimplePingPongClient (st :: SimplePingPong) a where
>   SendMsg    :: MessageSimplePingPong StSimpleIdle st
>              -> (SimplePingPongClient st a)
>              -> SimplePingPongClient StSimpleIdle a
>
>   RecvMsg    :: (MessageSimplePingPong StSimpleBusy StSimpleIdle
>                   -> (SimplePingPongClient StSimpleIdle a))
>              -> SimplePingPongClient StSimpleBusy a
>
>   ClientDone :: a
>              -> SimplePingPongClient StSimpleDone a

Our initial example client which sends a ping message, awaits for the
response, loops it two more time and sends the terminating message can be
written as:

> simplePingPongClient :: a -> SimplePingPongClient StSimpleIdle a
> simplePingPongClient a =
>     SendMsg MsgSimplePing
>   $ RecvMsg $ \MsgSimplePong ->  
>     SendMsg MsgSimplePing
>   $ RecvMsg $ \MsgSimplePong ->
>     SendMsg MsgSimplePing
>   $ RecvMsg $ \MsgSimplePong ->
>     SendMsg MsgSimpleDone
>   $ ClientDone a
>

How to represent pipelining
---------------------------

The `SimplePingPongClient` does not allow us to write a client which pipelines
messages.

The idea is that we can separate the sender side from the receiver.  Together
with a pipelined message we need to present a receiver and a continuation
which can send more messages.  On the type level we only need to track the
number of outstanding pipelined messages.  For this we use an inductive
natural numbers:

> -- | Type level inductive natural numbers.
> -- 
> data N = Z | S N

> data SimplePipelinedPingPongClient (st :: SimplePingPong) (n :: N) c a where
>   -- | Pipeline a single message, together with a receiver and a continuation
>   -- with incremented outstanding message counter.
>   --
>   PipelinedSendMsg :: MessageSimplePingPong StSimpleIdle st
>                    -> PingPongReceiver              StSimpleBusy StSimpleIdle c
>                    -> SimplePipelinedPingPongClient StSimpleIdle (S n) c a
>                    -> SimplePipelinedPingPongClient StSimpleIdle    n  c a
>
>   -- | Collect the receiver result.  The continuation subtracts from
>   -- outstanding pipelined message counter.
>   -- 
>   CollectResponse  :: (c -> SimplePipelinedPingPongClient StSimpleIdle n c a)
>                    -> SimplePipelinedPingPongClient StSimpleIdle (S n) c a
>
>   -- | Send terminal message; it is only allowed once we collected all the
>   -- responses.
>   --
>   SendMsgDone      :: MessageSimplePingPong StSimpleIdle StSimpleDone
>                    -> SimplePipelinedPingPongClient StSimpleDone Z c a
>                    -> SimplePipelinedPingPongClient StSimpleIdle Z c a
>
>   -- | Once terminating message was sent, return.
>   --
>   PipelinedDone    :: a
>                    -> SimplePipelinedPingPongClient StSimpleDone Z c a
>
> -- | Receiver; callback which is called on each 'MsgPong' received.
> --
> data PingPongReceiver (st :: SimplePingPong) (st' :: SimplePingPong) c where
>   RecvPipelinedMsg :: (MessageSimplePingPong StSimpleBusy StSimpleIdle -> c)
>                    -> PingPongReceiver StSimpleBusy StSimpleIdle c

> -- | Pipelined ping pong client, which for simplicty pipelines two messages
> -- 
> simplePipelinedPingPongClient
>    :: a -- ^ fixed result, for simplicity
>    -> c -- ^ fixed collected value, for simplicity
>    -> SimplePipelinedPingPongClient StSimpleIdle Z c a
> simplePipelinedPingPongClient a c =
>    PipelinedSendMsg
>      MsgSimplePing
>      (RecvPipelinedMsg $ \MsgSimplePong -> c)
>      (PipelinedSendMsg MsgSimplePing
>                        (RecvPipelinedMsg $ \MsgSimplePong -> c)
>                        (CollectResponse $ \_c0 ->
>                           CollectResponse $ \_c1 ->
>                             SendMsgDone MsgSimpleDone $
>                               PipelinedDone a        
>                        )
>      )

This is a simplified version of pipelining api implemented in
[typed-protocols](https://input-output-hk.github.io/ouroboros-network/typed-protocols/Network-TypedProtocol-Pipelined.html).
There are a few minor problems with this approach:

* the recursive flow of `SimplePingPongClient` is broken
* in complex protocols one needs to share the type `c` between all the possibly
  different pipelining scenarios

The `SimplePipelinedPingPongClient` still needs an interpreter.  Let's try to
envision how it could look like.  In the network context we can consider that
we are given a bidirectional bearer with guaranteed ordered delivery of
messages.  Let's envision its interface as:

> data PingPongChannel m = PingPongChannel {
>     sendMsg :: forall st st'. MessageSimplePingPong st st' -> m (),
>     readMsg :: forall st st'. m (MessageSimplePingPong st st') 
>   }

For the real implementation look
[here](https://input-output-hk.github.io/ouroboros-network/typed-protocols/Network-TypedProtocol-Driver.html#t:Driver).

The `SimplePipeliendPingPongClient` interpreter should be a function of this type

> runPingPongClient :: PingPongChannel m
>                   -> SimplePipelinedPingPongClient st n c a
>                   -> m a

<div class=hidden>

> runPingPongClient _channel _client = undefined

</div>

The question is how could we deal with `PipelinedSendMsg` which branches
between receiver and a continuation?  The solution in
[typed-protocols](https://input-output-hk.github.io/ouroboros-network/typed-protocols/Network-TypedProtocol-Pipelined.html)
is to run two threads and communicate between them with a pair of queues.  Once
we need to interpret `PipelinedSendMsg` we would push the receiver to the
queue, send a message through the `PingPongChannel` and continue.  There would
be another thread that would read that queue and interpret the receiver with:

> runPingPongReceiver :: PingPongChannel m
>                     -> PingPongReceiver st st' c
>                     -> m c

<div class=hidden>

> runPingPongReceiver _channel _receiver = undefined

</div>

It reads from the network, execute the receiver's callback and write the
result (of type `c`) to the other queue, from which it would be available to
`runPingPongClient` when interpreting `CollectResponse`
(ref. [runPipelinedPeerWithDriver](https://input-output-hk.github.io/ouroboros-network/typed-protocols/Network-TypedProtocol-Driver.html#v:runPipelinedPeerWithDriver)).


Protocol description
--------------------

So far we tried to present a single client in a type safe way.  In general we'd
like to be able to represent a wide class of binary protocols, and for each
protocol to be able to construct both sides: client and\/or server.  The
following
[Protocol](https://input-output-hk.github.io/ouroboros-network/typed-protocols/Network-TypedProtocol-Core.html#t:Protocol)
type class together with auxiliary types `PeerRole` and `PeerHasAgency`
defines what is needed to define a protocol and construct proofs of
correctness for client and a server.  So far we've only used `Message` type
from this type class.  Each protocol has to define which side has the
agency at each state or which states are terminating; for this the type class
has to provide `ClientHasAgency`, `ServerHasAgency` and `NobodyHasAgency`
together with some lemmas that ensure correctness of the provided associated
data instances.  The details of this go byoned this blog post.

>
> class Protocol ps where
> 
>   -- | The messages for this protocol. It is expected to be a GADT that is
>   -- indexed by the @from@ and @to@ protocol states. That is the protocol state
>   -- the message transitions from, and the protocol state it transitions into.
>   -- These are the edges of the protocol state transition system.
>   --
>   data Message ps (st :: ps) (st' :: ps)
> 
>   -- | Tokens for those protocol states in which the client has agency.
>   --
>   data ClientHasAgency (st :: ps)
> 
>   -- | Tokens for those protocol states in which the server has agency.
>   --
>   data ServerHasAgency (st :: ps)
> 
>   -- | Tokens for terminal protocol states in which neither the client nor
>   -- server has agency.
>   --
>   data NobodyHasAgency (st :: ps)
> 
>   -- | If the client has agency for a state, there are no
>   -- cases in which the server has agency for the same state.
>   --
>   exclusionLemma_ClientAndServerHaveAgency
>     :: forall (st :: ps).
>        ClientHasAgency st
>     -> ServerHasAgency st
>     -> Void
> 
>   -- | If nobody has agency for a state (the state is terminating), there
>   -- are no cases in which the client has agency for the same state.
>   --
>   exclusionLemma_NobodyAndClientHaveAgency
>     :: forall (st :: ps).
>        NobodyHasAgency st
>     -> ClientHasAgency st
>     -> Void
> 
>   -- | If nobody has agency for a state (the state is terminating), there
>   -- are no cases in which the server has agency for the same state.
>   --
>   exclusionLemma_NobodyAndServerHaveAgency
>     :: forall (st :: ps).
>        NobodyHasAgency st
>     -> ServerHasAgency st
>     -> Void
> 
> data PeerRole = AsClient | AsServer
> 
> data PeerHasAgency (pr :: PeerRole) (st :: ps) where
>     ClientAgency :: !(ClientHasAgency st) -> PeerHasAgency AsClient st
>     ServerAgency :: !(ServerHasAgency st) -> PeerHasAgency AsServer st
> 
> type WeHaveAgency   (pr :: PeerRole) st = PeerHasAgency             pr  st
> type TheyHaveAgency (pr :: PeerRole) st = PeerHasAgency (FlipAgency pr) st
> type family FlipAgency (pr :: PeerRole) where
>     FlipAgency AsClient = AsServer
>     FlipAgency AsServer = AsClient
> 

Pipelining with type level queue
--------------------------------

We already seen that pipelining requires a queue that allows to send more
messages before waiting for the replies.  But it does not neccessarily needs to
be a term level queue, equally well we could push the expected transitions on
a type level queue, and present a way to read from the queue.  We will try with
the simplest type level queue based on type level lists.

== `Queue` kind

First let us define elements that will be pushed on to the queue. When we just
send some `Message pt from to` and we want to pipeline next message
`Message from' to'`, we will push `Tr to from'` onto the queue.  The
`Tr to from' :: Trans ps` is a promoted type which allows us to track transitions
that are delayed.

> data Trans ps where
>     Tr :: forall ps. ps -> ps -> Trans ps

We represent the type level queue with type level list.  `<|` and `Empty` are
simply type aliases, we also provide a snoc operator `|>` which we will use to
push elements onto the queue.

> -- | Queue kind
> --
> type Queue ps = [Trans ps]
>
> -- | Empty type
> --
> type Empty :: Queue ps
> type Empty = '[]
>
> -- | Cons type constructor
> --
> type  (<|) :: Trans ps -> Queue ps -> Queue ps
> type a <| as = a ': as
> infixr 5 <|
>
> -- | Singleton queue
> --
> type Singleton :: Trans ps -> Queue ps
> type Singleton a = '[a]
>
> -- | Snoc operation
> --
> type (|>) :: Queue ps -> Trans ps -> Queue ps
> infixr 5 |>
> type family as |> a where
>      Empty     |> b = Singleton b
>      (a <| as) |> b = a <| (as |> b)

== `PeerPipelined`

> type PeerPipelined :: forall ps
>                    -> PeerRole
>                    -> ps
>                    -> Queue ps
>                    -> (Type -> Type)
>                    -> Type
>                    -> Type
> data PeerPipelined ps pr st tr m a where
> 
>     -- | 'Effect' allows to introduce monadic effects.
>     --
>     Effect  :: m (PeerPipelined ps pr st tr m a)
>             ->    PeerPipelined ps pr st tr m a
> 
>     -- | Non-pipelined send.  One needs to present proof of agency, message
>     -- to be send and a continuation.  One cannot send
>     -- non-pipelined messages when there are outstanding requests.  This is
>     -- enforced by requiring that the queue is empty.
>     --
>     Yield   :: !(WeHaveAgency pr st)
>             -> Message ps st st'
>             -> PeerPipelined ps pr st' Empty m a
>             -> PeerPipelined ps pr st  Empty m a
> 
>     -- | Await for a non-pipelined message.  One has to present a proof that
>     -- one does not have agency and a continuation function which can deal
>     -- with any messge that might arrive from the network.
>     --
>     Await   :: !(TheyHaveAgency pr st)
>             -> (forall st'. Message ps st st'
>                 -> PeerPipelined ps pr st' Empty m a)
>             -> PeerPipelined     ps pr st  Empty m a
> 
>     -- | Pipelined send which. Note that the continuation decides from which
>     -- state we pipeline next message, and the gap is pushed at the back of
>     -- the queue.
>     --
>     YieldPipelined
>             :: !(WeHaveAgency pr st)
>             -> Message ps st st'
>             -> PeerPipelined ps pr (st'' :: ps) (tr |> Tr st' st'') m a
>             -> PeerPipelined ps pr st tr m a
> 
>     -- | Collect pushed transition in one step.
>     --
>     Collect :: Maybe (PeerPipelined ps pr (st :: ps) (Tr st' st'' <| tr) m a)
>             -> !(TheyHaveAgency pr st')
>             -> (Message ps st' st''
>                 -> PeerPipelined ps pr (st :: ps)                    tr  m a)
>             -> PeerPipelined     ps pr (st :: ps)    (Tr st' st'' <| tr) m a
> 
>     -- | Parially collect pushed @Tr@.
>     --
>     CollectPartial
>             :: Maybe (PeerPipelined ps pr (st :: ps) (Tr st' st'' <| tr) m a)
>             -> !(TheyHaveAgency pr st')
>             -> (forall stNext. Message ps st' stNext
>                 -> PeerPipelined ps pr (st :: ps) (Tr stNext st'' <| tr) m a)
>             -> PeerPipelined     ps pr (st :: ps) (Tr st'    st'' <| tr) m a
> 
>     -- | Pop identity 'Tr' from the pipelining queue.
>     --
>     -- 'CollectDone' allows to defer poping @Tr ps st st@ from the queue after
>     -- a message is received (in 'CollectPartial' callback), unlike 'Collect'
>     -- which needs to know the transition type at compile time.
>     --
>     CollectDone
>             :: PeerPipelined ps pr (st :: ps)                tr  m a 
>             -> PeerPipelined ps pr (st :: ps) (Tr st' st' <| tr) m a
> 
>     -- | Terminate the protocol.
>     --
>     Done    :: !(NobodyHasAgency st)
>             -> a
>             -> PeerPipelined ps pr st Empty m a

`PingPong` protocol
-------------------

In this section we will formalise the ping pong protocol by providing
`Protocol` instance.

<figure class=small>
![](/images/ping-pong-0.png)
</figure>

> -- | Ping pong states
> --
> data PingPong where
>   StIdle     :: PingPong
>   StBusy     :: PingPong
>   StDone     :: PingPong

> instance Protocol PingPong where
>   -- | Ping pong messages
>   --
>   data Message PingPong from to where
>     MsgPing     :: Message PingPong StIdle StBusy
>     MsgPong     :: Message PingPong StBusy StIdle
>     MsgDone     :: Message PingPong StIdle StDone
> 
>   data ClientHasAgency st where
>     TokIdle :: ClientHasAgency StIdle
> 
>   data ServerHasAgency st where
>     TokBusy     :: ServerHasAgency StBusy
> 
>   data NobodyHasAgency st where
>     TokDone :: NobodyHasAgency StDone
> 
>   -- exclusion lemmas, which proove that n each state at most one of server, or
>   -- client has agency.
>   exclusionLemma_ClientAndServerHaveAgency TokIdle tok = case tok of {}
>   exclusionLemma_NobodyAndClientHaveAgency TokDone tok = case tok of {}
>   exclusionLemma_NobodyAndServerHaveAgency TokDone tok = case tok of {}


<div class=small>
Some auxiliary instancees.

> deriving instance Show (Message PingPong from to)
> deriving instance Show (ClientHasAgency (st :: PingPong))
> deriving instance Show (ServerHasAgency (st :: PingPong))

</div>


=== Non-pipelined ping pong client

> -- A non-pipelined PingPong.
> --
> pingPongClient :: a -> PeerPipelined PingPong AsClient StIdle Empty m a
> pingPongClient a =
>      -- send ping message
>      Yield (ClientAgency TokIdle) MsgPing
>      -- await for the response
>    $ await
>    $ Yield (ClientAgency TokIdle) MsgPing
>    $ await
>    $ -- send terminating message
>      Yield (ClientAgency TokIdle) MsgDone
>      -- return
>    $ Done TokDone a
>  where
>   -- await for all 'MsgPong' until first 'MsgPongDone'
>   await :: PeerPipelined PingPong AsClient StIdle Empty m a
>         -> PeerPipelined PingPong AsClient StBusy Empty m a
>   await k =
>     Await (ServerAgency TokBusy) $ \msg ->
>       case msg of
>         MsgPong -> k

=== Pipelined ping pong client

> -- A pipelined 'PingPong', without partial collects.
> --
> pingPongClientPipelined :: a -> PeerPipelined PingPong AsClient StIdle Empty m a
> pingPongClientPipelined a =
>      -- pipeline two pings
>      YieldPipelined (ClientAgency TokIdle) MsgPing
>    $ YieldPipelined (ClientAgency TokIdle) MsgPing
>      -- collect two pongs
>    $ Collect Nothing (ServerAgency TokBusy) $ \MsgPong ->
>      Collect Nothing (ServerAgency TokBusy) $ \MsgPong ->
>      -- send terminating message
>      Yield (ClientAgency TokIdle) MsgDone
>      -- return from the protocol
>    $ Done TokDone a

`PingPong2` protocol
--------------------

Let us consider a variation of the ping pong protocol, in which the server
might send multiple `MsgBusy` before transfering agency back to the client with
'MsgPong'.  In this version by pipelining `MsgPing`, we might need to collect
multiple `MsgBusy` until we receive `MsgPong`.  It will help us demostrate
that we can pipeline multiple `MsgPing` messages and collect all the replies.

<figure class=small>
![](/images/ping-pong-3.png)
</figure>

> -- | PingPong2 has the same state machine, although we will a different
> -- `Protocol` instance.  For that reason we provide a newtype wrapper and
> -- use type aliases.
> --
> newtype PingPong2 = Wrap PingPong
> type StIdle2 = Wrap StIdle
> type StBusy2 = Wrap StBusy
> type StDone2 = Wrap StDone

> instance Protocol PingPong2 where
>   data Message PingPong2 from to where
>     -- 'PingPong' message
>     MsgPingPong  :: Message PingPong        st            st'
>                  -> Message PingPong2 (Wrap st)     (Wrap st')
>     -- | new message
>     MsgBusy      :: Message PingPong2 (Wrap StBusy) (Wrap StBusy)
> 
>   data ClientHasAgency st where
>     WrapClient :: ClientHasAgency       st
>                -> ClientHasAgency (Wrap st)
> 
>   data ServerHasAgency st where
>     WrapServer :: ServerHasAgency       st
>                -> ServerHasAgency (Wrap st)
> 
>   data NobodyHasAgency st where
>     WrapDone   :: NobodyHasAgency       st
>                -> NobodyHasAgency (Wrap st)
> 
>   -- We haven't changed the states and their agancies, so we can reuse
>   -- 'PingPong' lemmas.
>   exclusionLemma_ClientAndServerHaveAgency (WrapClient tok) (WrapServer tok') =
>     exclusionLemma_ClientAndServerHaveAgency tok tok'
>   exclusionLemma_NobodyAndClientHaveAgency (WrapDone   tok) (WrapClient tok') =
>     exclusionLemma_NobodyAndClientHaveAgency tok tok'
>   exclusionLemma_NobodyAndServerHaveAgency (WrapDone   tok) (WrapServer tok') =
>     exclusionLemma_NobodyAndServerHaveAgency tok tok'


<div class=small>
Some auxiliary instancees.

> deriving instance Show (Message PingPong2 from to)
> deriving instance Show (ClientHasAgency (st :: PingPong2))
> deriving instance Show (ServerHasAgency (st :: PingPong2))

</div>

=== Pipelined ping pong client using `CollectPartial`

> -- | A pipelined 'PingPong' which supports partial collects using the
> -- recursive 'collect'.
> --
> pingPongClientPipelined2
>   :: a
>   -> PeerPipelined PingPong2 AsClient StIdle2 Empty m a
> pingPongClientPipelined2 a =
>       -- pipeline three 'MsgPing'
>       YieldPipelined (ClientAgency (WrapClient TokIdle)) (MsgPingPong MsgPing)
>     $ YieldPipelined (ClientAgency (WrapClient TokIdle)) (MsgPingPong MsgPing)
>     $ YieldPipelined (ClientAgency (WrapClient TokIdle)) (MsgPingPong MsgPing)
>       -- collect responses
>     $ collect
>     $ collect
>     $ collect
>     $ Yield (ClientAgency (WrapClient TokIdle)) (MsgPingPong MsgDone)
>     $ Done (WrapDone TokDone) a
>   where
>     -- recursievly collect responses, until 'MsgPongDone2' is received
>     collect :: PeerPipelined PingPong2 AsClient StIdle2      tk  m a
>             -- ^ continuation after removing @Tr StBusy2 StIdle2@ from the
>             -- queue
>             -> PeerPipelined PingPong2 AsClient StIdle2    
>                                       (Tr StBusy2 StIdle2 <| tk) m a
>     collect k =
>         CollectPartial Nothing (ServerAgency (WrapServer TokBusy))
>       $ \msg -> case msg of
>           MsgBusy               -> collect     k
>           (MsgPingPong MsgPong) -> CollectDone k

Next example is similar to the previous one but it counts the number of
`MsgBusy` received.

> pingPongClientPipelined2Counter
>     :: PeerPipelined PingPong2 AsClient StIdle2 Empty m Int
> pingPongClientPipelined2Counter =
>       -- pipeline three 'MsgPing2'
>       YieldPipelined (ClientAgency (WrapClient TokIdle)) (MsgPingPong MsgPing)
>     $ YieldPipelined (ClientAgency (WrapClient TokIdle)) (MsgPingPong MsgPing)
>     $ YieldPipelined (ClientAgency (WrapClient TokIdle)) (MsgPingPong MsgPing)
>       -- collect responses, and count received 'MsgBusy'
>     $        collect 0
>     $ \n1 -> collect n1
>     $ \n2 -> collect n2
>     $ \n3 -> Yield (ClientAgency (WrapClient TokIdle)) (MsgPingPong MsgDone)
>     $        Done (WrapDone TokDone) n3
>   where
>     -- recursievly collect responses, until 'MsgPongDone2' is received
>     collect
>         :: Int
>         -- ^ number of 'MsgBusy' received so far
>         -> (Int -> PeerPipelined PingPong2 AsClient StIdle2 tk m Int)
>         -- ^ continuation after removing @Tr StBusy2 StIdle2@ from the
>         -- queue
>         -> PeerPipelined PingPong2 AsClient StIdle2    
>                                   (Tr StBusy2 StIdle2 <| tk)   m Int
>     collect !n k =
>         CollectPartial Nothing (ServerAgency (WrapServer TokBusy))
>       $ \msg -> case msg of
>           MsgBusy               -> collect     (n+1) k
>           (MsgPingPong MsgPong) -> CollectDone      (k n)

[IOHK]:            https://github.com/input-output-hk
[Well-Typed]:      https://well-typed.com
[typed-protocols]: https://input-output-hk.github.io/ouroboros-network/typed-protocols/Network-TypedProtocol.html
[Haskell-eXChange]: https://skillsmatter.com/skillscasts/14633-45-minute-talk-by-duncan-coutts
[Monadic-Party-1]: https://www.youtube.com/watch?v=j8gza2L61nM
[Monadic-Party-2]: https://www.youtube.com/watch?v=oV6KSl1srL8
[Monadic-Party-3]: https://www.youtube.com/watch?v=nOIQCRPwmPA
