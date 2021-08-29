%Document class is set in the Makefile.

%include polycode.fmt
\arrayhs
%include forall.fmt
%include beamer.fmt
%if compile == False
%include lhs/MonoidalSynchronisation.fmt
%endif

\useinnertheme{circles}
\setbeamertemplate{navigation symbols}{}
\setbeamertemplate{frametitle}[default][center]

\usepackage[utf8]{inputenc}
\usepackage[british]{babel}

\usepackage[textwidth=4cm]{todonotes}
\usepackage{tikz}
\usetikzlibrary{matrix,arrows,calc}
\tikzset{every scope/.style={>=angle 60,thick}}

\author{Marcin Szamotulski}
\institute{\insertlogo{\includegraphics[height=1cm]{iohk-logo.png}}}
\title{Monoidal Synchronisation}
\subtitle{\small case study on \href{https://github.com/input-output-hk/ouroboros-network}{ouroboros-network}}

\begin{document}
\begin{frame}
    \titlepage
    \vfil
    \begin{center}
      \tiny
      \url{https://coot.me/presentations/monoidal-synchronisation.pdf}
    \end{center}
    \begin{flushright}
      \includegraphics[height=1.5cm]{../images/monoidal-synchronisation-qr.png}
    \end{flushright}
\end{frame}

\begin{frame}
  \begin{code}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE DeriveTraversable          #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE DerivingVia                #-}
{-# LANGUAGE GeneralisedNewtypeDeriving #-}
{-# LANGUAGE InstanceSigs               #-}
{-# LANGUAGE KindSignatures             #-}
{-# LANGUAGE RankNTypes                 #-}

module Presentation.MonoidalSynchronisation where

import Control.Applicative         (Alternative (..))
import Control.Monad               (MonadPlus (..))
import Data.Functor.Contravariant  (Contravariant (..))
import Data.List.NonEmpty          (NonEmpty (..))
import Data.Monoid                 (Ap (..), Alt (..))
import GHC.Generics
  \end{code}
\end{frame}

\part{Introduction}
\begin{frame}
  \partpage
  \tableofcontents[part=1]
\end{frame}

%%%%%%%%%%%%%%%%%%%%
\section{Semigroups}
%%%%%%%%%%%%%%%%%%%%

\begin{frame}
  \frametitle{Semigroups}
  \begin{spec}
  class Semigroup a where
    (<>) :: a -> a -> a
  \end{spec}

  \begin{examples}\small
    Free semigroup with one generator:
    \begin{spec}
    instance Semigroup (NonEmpty a) where
      (a :| as) <> (b :| bs) = a :| as ++ b : bs

    instance Semigroup [a] where
      (<>) = (++)

    instance Semigroup a => Semigroup (Maybe a) where
      Nothing  <> a       = a
      a        <> Nothing       = a
      Just a   <> Just b  = Just (a <> b)
    \end{spec}
  \end{examples}
\end{frame}

\begin{frame}
  \frametitle{Foldable1 type class}
  \small
  \(Foldable1\) type class is defined in \(semigroupoids\) package.
  \begin{code}
class Foldable t => Foldable1 t where
  foldMap1    :: Semigroup m
              => (a -> m) -> t a -> m
  fold1       :: Semigroup m => t m -> m
  fold1       = foldMap1 id
  toNonEmpty  :: t a -> NonEmpty a
  toNonEmpty  = foldMap1 (:| [])
  \end{code}
  \vspace{-5mm}
  \begin{examples}\footnotesize
    The canonical example instance is \texttt{NonEmpty}; all the instances
    require that the container is non-empty
    \footnote{This is a consequence of a \texttt{Semigroup} constraint rather
    than the \texttt{toNonEmpty} class
    member.}.
    \vspace{-2mm}
    @NonEmpty@ is a free semigroup:
    \begin{code}
instance Foldable1 NonEmpty where
  foldMap1  :: Semigroup m => (a -> m) -> NonEmpty a -> m
  foldMap1  f (a :| (b : bs))  = f a <> foldMap1 f (b :| bs)
  foldMap1  f (a :| [])        = f a
    \end{code}
  \end{examples}
\end{frame}

%%%%%%%%%%%%%%%%%
\section{Monoids}
%%%%%%%%%%%%%%%%%

\begin{frame}
  \frametitle{Monoids}
  \small
  Two sided identity:
  \begin{spec}
  class Semigroup a => Monoid a where
    mempty :: a
  \end{spec}
  which must obey: |a <> mempty == a == mempty <> a|
  \begin{examples}
    \vspace{-2mm}
    Free monoid in the class of left-strict monoids
    \begin{spec}
    instance Monoid [a] where
      mempty = []
    \end{spec}
    Left adjoint to the forgetful functor from monoids to semigroups, i.e.
    \(Maybe\,s\,\rightarrow_{monoid}\, m\ \equiv\ s\,\rightarrow_{semigroup}\,m\)
    \begin{spec}
    instance Semigroup a => Semigroup (Maybe a) where
      Nothing  <> a        = a
      a        <> Nothing  = a
      Just a   <> Just b   = Just (a <> b)
    instance Semigroup a => Monoid (Maybe a) where
      mempty = Nothing
    \end{spec}
  \end{examples}
\end{frame}

\begin{frame}
  \frametitle{Foldable type class}
  \begin{spec}
  class Foldable t where
    foldMap  :: Monoid m => (a -> m) -> t a -> m
    fold     :: Monoid m => t m -> m
    fold     = foldMap id
    toList   :: t a -> [a]
    toList   = foldMap (\a -> [a]) 
  \end{spec}

  Advantage of \texttt{foldMap} over \texttt{foldr}:

  \begin{spec}
  foldMap    :: (Foldable t, Monoid m)
             => (a -> m) -> t a -> m
  foldMap f  = foldr (\a m -> f a <> m) mempty
  \end{spec}

\end{frame}

%%%%%%%%%%%%%%%%%%%%%%
\section{FreeAlgebras}
%%%%%%%%%%%%%%%%%%%%%%

\begin{frame}
  \frametitle{Free algebras}
  \small
  The \texttt{FreeAlgebra} type class can capture the heart of \texttt{Foldable} and
  \texttt{Foldable1} classes and can be defined for other algebras than
  semigroups (\texttt{Foldable1}) or monoids (\texttt{Foldable}).
  \begin{spec}
  class FreeAlgebra (m :: Type -> Type) where
    returnFree   :: a -> m a
    foldMapFree  :: forall d a. AlgebraType  m d
                 => (a -> d) -> m a -> d
  \end{spec}
  For more details see
  \begin{itemize}
    \item \href{https://skillsmatter.com/skillscasts/13007-lightning-talk-rethinking-freeness-through-universal-algebra}{Haskell eXchange lightning talk},
    \item \href{https://hackage.haskell.org/package/free-algebras}{free-algebras}
      package on Hackage.
  \end{itemize}
\end{frame}

\section{Near Semi-Rings}
\begin{frame}
  \frametitle{Near Semi-Ring}
  \small
  \begin{definition}
    \((S, +, \cdot, 0)\) is a near semi-ring if:
    \begin{itemize}
      \item \((S, +, 0)\) is a monoid (not necessarily abelian);
      \item \((S, \cdot)\) is a semigroup;
      \item \((a + b) \cdot c = a \cdot c + b \cdot c\);
      \item \(0 \cdot a = 0\).
    \end{itemize}
  \end{definition}
  \begin{example}\small
    if \(M\) is a monoid then all its monoid homomorphisms form a near semi-ring
    with:
    \begin{itemize}
      \item \textit{multiplication}: function composition
      \item \textit{addition}: pointwise addition
      \item \textit{zero}: constant function to the identity of the monoid.
    \end{itemize}
    Note that for this \textit{near semi-ring} we have \(0\cdot a = 0 = a \cdot 0\),
    since monoid homomorphisms preserve the identity element.
  \end{example}
\end{frame}

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
\section{First-to-Finish and Last-to-Finish}
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

\begin{frame}
  \frametitle{First-to-Finish}
  \small
  Additive semigroup of the \text{LastToFinish}, \texttt{FirstToFinish} near semi-ring.
  \begin{code}
newtype  FirstToFinish m a =
         FirstToFinish { runFirstToFinish :: m a }
  deriving  ( Generic,      Generic1
            , Applicative,  Alternative
            , Monad,        MonadPlus
            , Functor,      Traversable
            )
  deriving  Semigroup      via (Alt m a)
  deriving  Monoid         via (Alt m a)
  deriving  Foldable       via (Alt m)
  deriving  Contravariant  via (Alt m)
  \end{code}

  The monoid multiplication is defined via:
  \begin{spec}
  (<|>)  :: Alternative m => m a -> m a -> m a
  \end{spec}
\end{frame}

\begin{frame}
  \frametitle{Last-to-Finish}
  \small
  Multiplicative semigroup of the \text{LastToFinish}, \texttt{FirstToFinish} near semi-ring.
  \begin{code}
newtype  LastToFinish m a =
         LastToFinish { runLastToFinish :: m a }
  deriving  (  Generic,      Generic1
            ,  Applicative,  Alternative
            ,  Monad,        MonadPlus
            ,  Functor,      Traversable
            )
  deriving Foldable via (Ap m)
  \end{code}
\end{frame}

\begin{frame}
  \frametitle{Last-to-Finish}
  \small
  \begin{code}
instance MonadPlus m => Semigroup (LastToFinish m a) where
    LastToFinish left <> LastToFinish right =
      LastToFinish $ do
        a  <-   Left   <$> left
           <|>  Right  <$> right
        case a of
          Left   {}  -> right
          Right  {}  -> left
  \end{code}
  \begin{itemize}
    \item The |LastToFinish| operator works well for the |STM| monad.
    \item For the |IO| monad: note that, if one of the actions throws an |IOError| then
          the result will throw it as well.  For |IO| the |<||>| operator is
          defined as:

          \begin{spec}
          io <|> io' = io `catch` \ (_ :: IOError) -> io'
          \end{spec}

          % An alternative |IO| instance:\\
          % {\tiny
          % \begin{code}
          % instance Semigroup (LastToFinish IO a) where
          %     LastToFinish left <> LastToFinish right =
          %       LastToFinish $ do
          %         a <-   Left   <$> left
          %           <|>  Right  <$> right
          %         catch
          %           (case a of
          %             Left   {}  -> right
          %             Right  {}  -> left)
          %           (\(_ :: IOError) -> return a)
          % \end{code}}
    \item We also use it using for a pure |STM| monad for the
          \href{https://input-output-hk.github.io/ouroboros-network/io-sim/Control-Monad-IOSim.html\#t:IOSim}{|IOSim|}
          monad.
  \end{itemize}
\end{frame}

\begin{frame}
  \frametitle{Monoidal Last-to-Finish}
  Multiplicative semigroup of the \text{LastToFinishM}, \texttt{FirstToFinish} near semi-ring.
  \begin{code}
newtype  LastToFinishM m a =
         LastToFinishM { runLastToFinishM :: m a }
  deriving  (  Generic,      Generic1
            ,  Applicative,  Alternative
            ,  Monad,        MonadPlus
            ,  Functor,      Traversable
            )
  deriving Semigroup  via (Ap m a)
  deriving Monoid     via (Ap m a)
  deriving Foldable   via (Ap m)
  \end{code}

  The monoid multiplication is given by:
  \begin{code}
(<<>>)  :: (Applicative m, Semigroup a)
        => m a -> m a -> m a
(<<>>) ma mb = (<>) <$> ma <*> mb
  \end{code}
  % This works well for `IO` and `STM`!
\end{frame}

\begin{frame}
  \frametitle{First-to-Finish and Last-to-Finish in |IO|}
  Another approach for |IO| monad is to define it using concurrency and |STM|
  instances of |FirstToFinish| or |LastToFinish| respectively:

  \begin{description}
    \item[\(\bullet\) first-to-finish]
      via |race|:
      \begin{spec}
        FirstToFinish io <> FirstToFinish io'
          =  withAsync  io   $ \th   ->
             withAsync  io'  $ \th'  ->
               atomically $ runFirstToFinish $
                     FirstToFinish (wait th)
                 <>  FirstToFinish (wait th')
      \end{spec}
    \item[\(\bullet\) last-to-finish]
      via |LastToFinish| for the |STM| monad:
      \begin{spec}
        LastToFinish io <> LastToFinish io'
          =  withAsync  io   $ \th  ->
             withAsync  io'  $ \th'  ->
               atomically $ runLastToFinish $
                     LastToFinish (wait th)
                 <>  LastToFinish (wait th')
      \end{spec}
  \end{description}
\end{frame}

\part{Case study}
\begin{frame}
  \partpage
  \tableofcontents[part=2]
\end{frame}

%%%%%%%%%%%%%%%%%%%%%%%%
\section{Ouroboros Network}
%%%%%%%%%%%%%%%%%%%%%%%%

\begin{frame}
  \frametitle{Ouroboros Network}
  \begin{center}
  \includegraphics[width=6cm]{../images/p2p-network.png}
  \end{center}
  \begin{itemize}
    \item decentralisad, real-time system
    \item blockchain consensus algorithm based on academic research
    \item core (block producing nodes), relay and end-user nodes all running
      the same core software
  \end{itemize}
\end{frame}

\begin{frame}
  \frametitle{Network Protocol}
  \begin{center}
  \includegraphics[width=6cm]{../images/node-to-node-ipc.png}
  % TODO: add keep-alive!
  \end{center}
  \begin{itemize}
    \item each node communicates through four mini-protocols:
      \textit{chain-sync}, \textit{block-fetch}, \textit{tx-submission}, \textit{keep-alive};
    \item the mini-protocols are multiplexed over a single bearer;
    \item we use duplex bearers, which means that on each bearer we
      run 8 mini-protocols: inbound (responders) and outbound (initiators).
  \end{itemize}
\end{frame}

%%%%%%%%%%%%%%%%%%%%%
\section{Multiplexer}
%%%%%%%%%%%%%%%%%%%%%

\begin{frame}
  \frametitle{Multiplexer}
  \vspace{-5mm}\small
  \begin{center}
  \includegraphics[width=8cm]{../images/multiplexer.png}
  \end{center}
  \vspace{-5mm}
  \begin{itemize}
    \item exectue a set (mini-)protocols on a single bearer (e.g. \texttt{TCP} connection, unix pipe, etc);
    \item bidirectional: allows to run two copies of the protocols: inbound (responder) / outbound (initiator);
    \item no ahead of line blocking;
    \item allows to start responders either \textit{eagerly} or \textit{on
      demand}, e.g. as soon as data arrives for that protocol;
    \item allows to track state of each mini-protocol using \texttt{STM}:
      whether an \textit{on demand} started protocol started running, or if it finished
      / errored.
  \end{itemize}
\end{frame}

\begin{frame}
  \frametitle{Multiplexer}
  \tiny
  \savesavecolumns
< monitor  ::  forall mode.
<              JobPool MuxJobResult
<          ->  TQueue (ControlCmd mode)
<          ->  StrictTVar MuxStatus
<          ->  IO ()
< monitor jobpool cmdQueue muxStatus =
<     go (MonitorCtx Map.empty)
<   where
<     go :: MonitorCtx mode -> IO ()
<     go !monitorCtx@MonitorCtx { mcOnDemandProtocols } = do
<       result <- atomically $ runFirstToFinish $
  wait for a mini-protocol thread to terminate
  \restorecolumns
<             (FirstToFinish $ EventJobResult <$> JobPool.collect jobpool)

   wait for a new control command
   \restorecolumns
<         <>  (FirstToFinish $ EventControlCmd <$> readTQueue cmdQueue)

   or wait for data to arrive on the channels that do not yet have
   responder threads running
   \restorecolumns
<         <>  foldMap
<               (\(ptclState, ptclAction) ->
<                 FirstToFinish $ do
<                   checkNonEmptyQueue (miniProtocolIngressQueue ptclState)
<                   return (EventStartOnDemand ptclState ptclAction)
<               )
<               mcOnDemandProtocols
<
<       case result of
<         ...
\end{frame}

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
\section{Inbound Protocol Governor}
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

\begin{frame}
  \frametitle{Inbound Protocol Governor}
  % TODO: I need graphics which shows architecture of all the components, this
  % will make things easier to read.
  \tiny
  \begin{itemize}
    \item track state of multiple connections,
    \item thrack state of each responder mini-protocol.
  \end{itemize}
  \savesavecolumns
  \begin{spec}
  data InboundGovernorState muxMode peerAddr a b = InboundGovernorState
    {  igsConnections :: !(Map  (ConnectionId peerAddr)
                                (ConnectionState muxMode peerAddr a b))
    ,  ...
    }

  data ConnectionState muxMode peerAddr a b = ConnectionState
    {  -- Multiplexer interface for the connection.
       --
       csMux              :: !(Mux.Mux muxMode),

       -- Static data which describes all mini-protocols supported by the
       -- connection.
       --
       csMiniProtocolMap  :: !(Map  MiniProtocolNum
                                    (MiniProtocolData muxMode a b)),

       -- Map of all running mini-protocol completion STM actions.
       --
       csCompletionMap    :: !(Map  MiniProtocolNum
                                    (STM (Either SomeException b))),

       ...
    }
  \end{spec}
\end{frame}

\begin{frame}
  \tiny
  \begin{spec}
  firstConnectionToFinish
      ::  InboundGovernorState muxMode peerAddr IO a b
      ->  STM (ConnectionId peerAddr, Maybe SomeException)
  firstConnectionToFinish InboundGovernorState { igsConnections } =
         runFirstToFinish
      .  Map.foldMapWithKey
           (\connId ConnectionState { csMux } ->
             FirstToFinish $
                    (connId,)
               <$>  (Mux.muxStopped csMux
                       :: STM (Maybe SomeException))
           )
      $  igsConnections
  \end{spec}
\end{frame}

\begin{frame}
  \tiny
  \begin{spec}
  firstMiniProtocolToFinish
      ::  InboundGovernorState muxMode peerAddr a b
      ->  STM (Terminated muxMode peerAddr a b)
  firstMiniProtocolToFinish InboundGovernorState { igsConnections } =
      runFirstToFinish $
      Map.foldMapWithKey
        (\connId ConnectionState { csMux, csMiniProtocolMap, csCompletionMap } ->
          Map.foldMapWithKey
            (\miniProtocolNum completionAction ->
                  (\tResult -> Terminated {
                        tConnId           = connId,
                        tMux              = csMux,
                        tMiniProtocolData = csMiniProtocolMap Map.! miniProtocolNum,
                        tResult
                      }
                  )
              <$> FirstToFinish completionAction
            )
            csCompletionMap
        )
        igsConnections
  \end{spec}
\end{frame}

\begin{frame}
  \tiny
  \begin{spec}
  firstPeerDemotedToCold
      :: MonadSTM m
      => InboundGovernorState muxMode peerAddr m a b
      -> STM m (Event muxMode peerAddr m a b)
  firstPeerDemotedToCold InboundGovernorState { igsConnections } = runFirstToFinish $
      Map.foldMapWithKey
        (\connId
          ConnectionState {
            csMux,
            csRemoteState
          } ->
          case csRemoteState of
            RemoteEstablished ->
                  fmap (const (WaitIdleRemote connId))
                . lastToFirstM
                $ (Map.foldMapWithKey
                    (\(_, miniProtocolDir) miniProtocolStatus ->
                      case miniProtocolDir of
                        InitiatorDir -> mempty

                        ResponderDir ->
                          LastToFinishM $ do
                            miniProtocolStatus >>= \case
                              StatusIdle          -> return ()
                              StatusStartOnDemand -> return ()
                              StatusRunning       -> retry
                    )
                    (Mux.miniProtocolStateMap csMux
                       :: Map (MiniProtocolNum, MiniProtocolDir)
                              (STM m MiniProtocolStatus)
                    )
                  )
            ...
        )
        igsConnections
  \end{spec}
\end{frame}

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
\section{Outbound Protocol Governor}
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

\begin{frame}
  \frametitle{Outbound Protocol Governor}
  \begin{itemize}
    \item governs outbound side of a connection (initiator mini-protocols)
    \item An upstream peer might be in one the three states: \textit{hot}, \textit{warm}, \textit{cold}:
      \begin{description}[labelwidth=2em]%[align=parleft,labelsep=5mm]
        \item[\textit{cold}:] a known peer, no outbound mini-protocols are running;
        \item[\textit{warm}:] warm \& established outbound mini-protocols are
          running (\textit{keep-alive} mini-protocol);
        \item[\textit{hot}:]  hot \& established outbound
          mini-protocols are running (\textit{chain-sync}, \textit{block-fetch},
          \textit{tx-submission}, \textit{keep-alive});
      \end{description}
    \item peer discovery (through a future \textit{gossip}
      mini-protocol), currently only relays registered on chain are discoverable;
    \item it is reponsible for sutaining targets of cold, warm \& hot
      peers.
  \end{itemize}
\end{frame}

\begin{frame}
  \small
  The governor is using @Guarded stm (Decision stm peeraddr peerconn)@ where 'stm' is
  the 'STM' monad, to drive its progress.
  \begin{spec}
  data Guarded stm a =
       -- 'GuardedSkip' is used to instruct that there is no action to be
       -- made by the governor.
       GuardedSkip !(Maybe (Min Time))

       -- 'Guarded' is used to provide an action through 'FirstToFinish'
       -- synchronisation to the governor main loop, possibly with
       -- a timeout.
    |  Guarded'   !(Maybe (Min Time)) (FirstToFinish stm a)
  \end{spec}

  |Guarded| constructor which  hides the use of |FirstToFinish|
  synchronisation.
  \begin{spec}
  pattern Guarded :: Maybe (Min Time) -> stm a -> Guarded stm a
  pattern Guarded a b <- Guarded' a (FirstToFinish b)
    where
      Guarded a b = Guarded' a (FirstToFinish b)
  \end{spec}
\end{frame}

\begin{frame}
  \frametitle{Guarded semigroup}
  \small
  \texttt{Guarded} constructor is absorbing in the sense that

  \begin{verbatim}
  Guarded x y <> a = Guarded x' y'
  a <> Guarded x y = Guarded x' y'
  \end{verbatim}

  In the algebraic sense, @'Guarded' (Just minBound) (return x)@ is a left
  \href{https://en.wikipedia.org/wiki/Absorbing_element}{absorbing element}
  when |stm  ~  STM|.  There is no right absorbing element since there is no
  right absorbing elemnt in |STM|.

  {\footnotesize\begin{spec}
  instance Alternative stm => Semigroup (Guarded stm a) where
    Guarded'     ta  a  <> Guarded'     tb  b  = Guarded'     (ta <> tb)  (a <> b)
    Guarded'     ta  a  <> GuardedSkip  tb     = Guarded'     (ta <> tb)  a
    GuardedSkip  ta     <> Guarded'     tb  b  = Guarded'     (ta <> tb)  b
    GuardedSkip  ta     <> GuardedSkip  tb     = GuardedSkip  (ta <> tb)
  \end{spec}}
\end{frame}

\begin{frame}
  \frametitle{Guarded decisions}
  \small
  \begin{spec}
  type  TimedDecision m peeraddr peerconn
     =  Time -> Decision m peeraddr peerconn

  guardedDecisions
    :: Time
    -> PeerSelectionState peeraddr peerconn
    -> Guarded STM (TimedDecision m peeraddr peerconn)
  guardedDecisions blockedAt st =
    -- All the alternative non-blocking internal decisions.
        RootPeers.belowTarget         actions  blockedAt  st
    <>  KnownPeers.belowTarget        actions  policy     st
    <>  KnownPeers.aboveTarget                 policy     st
    <>  EstablishedPeers.belowTarget  actions  policy     st
    <>  EstablishedPeers.aboveTarget  actions  policy     st
    <>  ActivePeers.belowTarget       actions  policy     st
    ...
  \end{spec}
\end{frame}

\begin{frame}
  \frametitle{Guarded decisions}
  \footnotesize
  \vspace{-0.8em}
  \begin{spec}
  evalGuardedDecisions  :: Time
                        -> PeerSelectionState peeraddr peerconn
                        -> IO (TimedDecision IO peeraddr peerconn)
  evalGuardedDecisions blockedAt st =
      case guardedDecisions blockedAt st of
        GuardedSkip _ ->
          -- impossible since guardedDecisions always has
          -- something to wait for
          error "impossible: nothing to do"
        Guarded Nothing decisionAction ->
          atomically decisionAction
        Guarded (Just (Min wakeupAt)) decisionAction -> do
          wakupTimeout <- newTimeout (wakeupAt `diffTime` blockedAt)
          let wakeup  =   awaitTimeout wakupTimeout
                      >>  pure (wakeupDecision st)
          timedDecision <- atomically (decisionAction <|> wakeup)
          cancelTimeout wakupTimeout
          return timedDecision
    where
      wakeupDecision st _now =
         Decision  { decisionTrace = TraceGovernorWakeup
                   , decisionState = st, decisionJobs  = [] }
  \end{spec}
  % <> ActivePeers.aboveTarget      actions     policy st

  % -- All the alternative potentially-blocking decisions.
  % <> Monitor.targetPeers          actions st
  % <> Monitor.localRoots           actions st
  % <> Monitor.jobs                 jobPool st
  % <> Monitor.connections          actions st
\end{frame}


\begin{frame}
  \frametitle{Acknowledgement}
  \begin{itemize}
    \item Duncan Coutts, \href{https://well-typed.com}{Well-Typed}
    \item Neil Davids, \href{http://pnsol.com}{PNSol}
    \item Karl Knuttson, \href{https://iohk.io}{IOHK}
    \item Peter Thompson, \href{http://pnsol.com}{PNSol}
    \item Armando Santos, \href{https://well-typed.com}{Well-Typed}
  \end{itemize}
  \vfill
  {\footnotesize
  \url{https://coot.me/presentations/monoidal-synchronisation.pdf}
  \begin{flushleft}
    \includegraphics[height=1cm]{../images/monoidal-synchronisation-qr.png}
  \end{flushleft}
  \url{https://github.com/input-output-hk/ouroboros-network/tree/master/monoidal-synchronisation}
  \begin{flushleft}
    \includegraphics[height=1cm]{../images/monoidal-synchronisation-github-qr.png}
  \end{flushleft}
  }
\end{frame}

\end{document}
