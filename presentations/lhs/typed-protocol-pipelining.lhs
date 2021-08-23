\documentclass[t,dvipsnames,hyperref={colorlinks,citecolor=NavyBlue,linkcolor=NavyBlue,anchorcolor=NavyBlue,urlcolor=NavyBlue}]{beamer}

%include lhs2TeX.sty
%include polycode.fmt
\arrayhs
%include lhs2TeX.fmt
%include forall.fmt
%include beamer.fmt
%format <| = "\triangleleft"
%format |> = "\triangleright"

\useinnertheme{circles}
\setbeamertemplate{navigation symbols}{}
\setbeamertemplate{frametitle}[default][center]

\usepackage[utf8]{inputenc}
\usepackage[british]{babel}

% \usepackage[adobe-utopia]{mathdesign}
% \usepackage[series=z]{libgreek}
% \usepackage{tgpagella}
\usepackage[textwidth=4cm]{todonotes}
% \usepackage{enumitem}
\usepackage{tikz}
\usetikzlibrary{matrix,arrows,calc}
\tikzset{every scope/.style={>=angle 60,thick}}

\author{Marcin Szamotulski}
\institute{\insertlogo{\includegraphics[height=1cm]{iohk-logo.png}}}
\title{Typed Protocol Pipelining}

\begin{document}
\begin{frame}
    \titlepage
    \vfil
    \begin{center}
      \tiny
      \url{https://coot.me/presentations/typed-protocol-pipelining.pdf}
    \end{center}
\end{frame}

\begin{frame}
  \frametitle{Ping Pong Protocol}
  \vspace{1cm}
  \begin{figure}
    \includegraphics{../images/ping-pong-0.png}
    \caption{PingPong protocol state diagram}
  \end{figure}
\end{frame}

\begin{frame}
  \frametitle{Protocol pipelining}

  \vspace{1cm}
  \begin{figure}
    \begin{minipage}{.4\textwidth}
      \includegraphics{../images/ping-pong-1.png}
      \vspace{2cm}
    \end{minipage}
    \begin{minipage}{.4\textwidth}
      \includegraphics{../images/ping-pong-2.png}
    \end{minipage}
    \caption{non-pipelined vs pipelined ping pong client}
  \end{figure}

\end{frame}

\begin{frame}
  \frametitle{Protocol pipelining}
  \vspace{1cm}
  \begin{itemize}
    \item \textbf{latency hidding}
    \item \textbf{network utilisation}
        \textit{network is utilised best when constant pressure is
         applied to avoid shrinking of the tcp window (e.g. tcp flow control
         mechanism).  Network utilisation is a balance between keeping the most
         constrained resource busy - but only just - too busy and delay increases and
         application responsiveness can drop.}
  \end{itemize}

  \begin{itemize}
    \item pipelined transitions are no longer a continous flow of matching transitions (i.e. composition of state transitions).
    \item pipelining must keep relative order of reqests and response
      transitions, but can mix both groups.
  \end{itemize}
\end{frame}

\begin{frame}
  \frametitle{Towards non-pipelined protocol description}
  \small
  \begin{code}
data PingPong where
  StIdle     :: PingPong
  StBusy     :: PingPong
  StDone     :: PingPong

data MessageSimplePingPong (st  :: PingPong) (st' :: PingPong) where
  MsgSimplePing  :: MessageSimplePingPong StIdle StBusy
  MsgSimplePong  :: MessageSimplePingPong StBusy StIdle
  MsgSimpleDone  :: MessageSimplePingPong StIdle StDone

data SimplePingPongClient (st :: PingPong) a where
  SendMsg     :: MessageSimplePingPong StIdle st
              -> (SimplePingPongClient st a)
              -> SimplePingPongClient StIdle a

  RecvMsg     :: (MessageSimplePingPong StBusy StIdle
                   -> (SimplePingPongClient StIdle a))
              -> SimplePingPongClient StBusy a

  ClientDone  :: a
              -> SimplePingPongClient StDone a
  \end{code}
\end{frame}

\begin{frame}
  \frametitle{Towards non-pipelined protocol description}
  \begin{columns}[t]
    \begin{column}{1.0\textwidth}
      \begin{code}
simplePingPongClient :: a -> SimplePingPongClient StIdle a
simplePingPongClient a =
   SendMsg MsgSimplePing
 $ RecvMsg $ \MsgSimplePong ->
   SendMsg MsgSimplePing
 $ RecvMsg $ \MsgSimplePong ->
   SendMsg MsgSimplePing
 $ RecvMsg $ \MsgSimplePong ->
   SendMsg MsgSimpleDone
 $ ClientDone a

      \end{code}
    \end{column}
  \end{columns}
\end{frame}

\begin{frame}
  \frametitle{Towards pipelined protocol description}
  \small
  \begin{code}
data N = Z | S N
data SimplePipelinedPingPongClient (st :: PingPong) (n :: N) c a where
  PipelinedSendMsg  :: MessageSimplePingPong StIdle st
                    -> PingPongReceiver              StBusy StIdle c
                    -> SimplePipelinedPingPongClient StIdle (S n) c a
                    -> SimplePipelinedPingPongClient StIdle    n  c a

  CollectResponse   :: (c -> SimplePipelinedPingPongClient StIdle n c a)
                    -> SimplePipelinedPingPongClient StIdle (S n) c a

  SendMsgDone       :: MessageSimplePingPong StIdle StDone
                    -> SimplePipelinedPingPongClient StDone Z c a
                    -> SimplePipelinedPingPongClient StIdle Z c a

  PipelinedDone     :: a
                    -> SimplePipelinedPingPongClient StDone Z c a

data PingPongReceiver (st  :: PingPong) (st' :: PingPong) c where
  RecvPipelinedMsg   :: (MessageSimplePingPong StBusy StIdle -> c)
                     -> PingPongReceiver StBusy StIdle c
  \end{code}
\end{frame}

\begin{frame}
  \frametitle{Towards pipelined protocol description}
  \begin{code}
simplePipelinedPingPongClient
   :: a  -- fixed result, for simplicity
   -> c  -- fixed collected value, for simplicity
   -> SimplePipelinedPingPongClient StIdle Z c a
simplePipelinedPingPongClient a c =
   PipelinedSendMsg
     MsgSimplePing
     (RecvPipelinedMsg $ \MsgSimplePong -> c)
     (PipelinedSendMsg
       MsgSimplePing
       (RecvPipelinedMsg $ \MsgSimplePong -> c)
       (CollectResponse $ \_c0 ->
         CollectResponse $ \_c1 ->
           SendMsgDone MsgSimpleDone $
             PipelinedDone a
       )
     )
  \end{code}
\end{frame}

\begin{frame}
  \frametitle{Towards pipelined protocol description}
  \vspace{2cm}
  Branching in \texttt{PipelinedSendMsg} requires that the interpretation of
  \texttt{SimplePipelinedPingPongClient} needs to concurrent execution: 
  \small
  \begin{code}
  PipelinedSendMsg  ::  MessageSimplePingPong StIdle st
                    ->  PingPongReceiver              StBusy StIdle c
                    ->  SimplePipelinedPingPongClient StIdle (S n) c a
                    ->  SimplePipelinedPingPongClient StIdle    n  c a
  \end{code}
\end{frame}

\begin{frame}
  \frametitle{Typed Protocol Description}
  \framesubtitle{Protocol Type Class}
  \small
  \begin{code}
  data Agency where
      ClientAgency  :: Agency
      ServerAgency  :: Agency
      NobodyAgency  :: Agency
  \end{code}

  Protocol type class provides messages and state type family.
  \begin{code}
  class Protocol ps where
    data Message ps (st :: ps) (st' :: ps)
    type StateAgency (st :: ps) :: Agency
  \end{code}

  \begin{code}
instance Protocol PingPong where
  data Message PingPong from to where
    MsgPing  :: Message PingPong StIdle StBusy
    MsgPong  :: Message PingPong StBusy StIdle
    MsgDone  :: Message PingPong StIdle StDone

  type StateAgency StIdle  = ClientAgency
  type StateAgency StBusy  = ServerAgency
  type StateAgency StDone  = NobodyAgency
  \end{code}
\end{frame}

\begin{frame}
  \frametitle{Typed Protocol Description}
  \framesubtitle{Relative Agency}
  \begin{code}
data PeerRole = AsClient | AsServer

data RelativeAgency where
    WeHaveAgency    :: RelativeAgency
    TheyHaveAgency  :: RelativeAgency
    NobodyHasAgency :: RelativeAgency

type        Relative :: PeerRole -> Agency -> RelativeAgency
type family Relative  pr a where
  Relative AsClient ClientAgency = WeHaveAgency
  Relative AsClient ServerAgency = TheyHaveAgency
  Relative AsClient NobodyAgency = NobodyHasAgency

  Relative AsServer ClientAgency = TheyHaveAgency
  Relative AsServer ServerAgency = WeHaveAgency
  Relative AsServer NobodyAgency = NobodyHasAgency
  \end{code}
\end{frame}

\begin{frame}
  \frametitle{Typed Protocol Description}
  \framesubtitle{Relative Agency}
  \small
  Type equality for 'RelativeAgency' which also carries
  information about agency.
  \begin{code}
  type ReflRelativeAgency
        :: Agency -> RelativeAgency -> RelativeAgency -> Type
  data ReflRelativeAgency a r r' where
      ReflClientAgency :: ReflRelativeAgency ClientAgency r r
      ReflServerAgency :: ReflRelativeAgency ServerAgency r r
      ReflNobodyAgency :: ReflRelativeAgency NobodyAgency r r
  \end{code}
  An evidence that both relative agencies are equal to
  'NobodyHasAgency'.
  \begin{code}
  type ReflNobodyHasAgency
        :: RelativeAgency -> RelativeAgency -> Type
  data ReflNobodyHasAgency ra ra' where
       ReflNobodyHasAgency :: ReflNobodyHasAgency
                                  NobodyHasAgency
                                  NobodyHasAgency

  \end{code}
\end{frame}

\begin{frame}
  \frametitle{Typed Protocol Description}
  \framesubtitle{Relative Agency}
  \small
  A type family which swaps the client and server roles.
  \begin{code}
  type        FlipAgency :: PeerRole -> PeerRole
  type family FlipAgency pr where
    FlipAgency AsClient = AsServer
    FlipAgency AsServer = AsClient
  \end{code}
\end{frame}

\begin{frame}
  \frametitle{Exclusion Lemmas}
  \framesubtitle{Agency}
  \begin{figure}
    \includegraphics[width=11cm]{../images/exclusion_lemmas.png}
  \end{figure}
\end{frame}

\begin{frame}
  \frametitle{Exclusion Lemmas}
  \framesubtitle{Relative Agency}
  \small
  A proof that if both @Relative pr a@ and @Relative (FlipAgency pr) a@ are
  equal then nobody has agency.  In particual this lemma excludes the
  possibility that client and server has agency at the same state.
  \begin{code}
  exclusionLemma_ClientAndServerHaveAgency
    :: forall (pr :: PeerRole) (a :: Agency)
              (ra  :: RelativeAgency).
       SingPeerRole pr
    -> ReflRelativeAgency a ra (Relative (            pr) a)
    -> ReflRelativeAgency a ra (Relative (FlipAgency  pr) a)
    -> ReflNobodyHasAgency     (Relative (            pr) a)
                               (Relative (FlipAgency  pr) a)
  exclusionLemma_ClientAndServerHaveAgency
    SingAsClient ReflNobodyAgency ReflNobodyAgency
    = ReflNobodyHasAgency
  exclusionLemma_ClientAndServerHaveAgency
    SingAsServer ReflNobodyAgency ReflNobodyAgency
    = ReflNobodyHasAgency
  \end{code}
\end{frame}

\begin{frame}
  \frametitle{Exclusion Lemmas}
  \framesubtitle{Relative Agency}
  \footnotesize
  A proof that if one side has terminated, then the other side terminated as
  well.
  \begin{code}
  terminationLemma_1
      :: SingPeerRole pr
      -> ReflRelativeAgency a ra               (Relative (            pr) a)
      -> ReflRelativeAgency a NobodyHasAgency  (Relative (FlipAgency  pr) a)
      -> ReflNobodyHasAgency                   (Relative (            pr) a)
                                               (Relative (FlipAgency  pr) a)
  terminationLemma_1
    SingAsClient ReflNobodyAgency ReflNobodyAgency
      = ReflNobodyHasAgency
  terminationLemma_1
    SingAsServer ReflNobodyAgency ReflNobodyAgency
      = ReflNobodyHasAgency
  \end{code}
\end{frame}

\begin{frame}
  \frametitle{Exclusion Lemmas}
  \framesubtitle{Relative Agency}
  \footnotesize
  \begin{code}
  terminationLemma_2
      :: SingPeerRole pr
      -> ReflRelativeAgency a ra               (Relative (FlipAgency  pr) a)
      -> ReflRelativeAgency a NobodyHasAgency  (Relative (            pr) a)
      -> ReflNobodyHasAgency                   (Relative (FlipAgency  pr) a)
                                               (Relative (            pr) a)
  terminationLemma_2
    SingAsClient ReflNobodyAgency ReflNobodyAgency
      = ReflNobodyHasAgency
  terminationLemma_2
    SingAsServer ReflNobodyAgency ReflNobodyAgency
      = ReflNobodyHasAgency
  \end{code}
\end{frame}

\begin{frame}
  \frametitle{Protocol Application API}
  \framesubtitle{singletons}
  \begin{code}
  data Trans ps where
      Tr        :: forall ps. ps -> ps -> Trans ps

  data Queue ps where
    Empty       :: Queue ps
    Cons        :: Trans ps -> Queue ps -> Queue ps

  type  (<|)    :: Trans ps -> Queue ps -> Queue ps
  type a <| as  = Cons a as
  infixr 5 <|

  type (|>)       :: Queue ps -> Trans ps -> Queue ps
  type family as  |> b where
       Empty      |> b  = Cons b Empty
       (a <| as)  |> b  = a <| (as |> b)
  infixr 5 |>
  \end{code}
\end{frame}

\begin{frame}
  \frametitle{Protocol Application API}
  \framesubtitle{deep embedding: non-pipelined primitives}
  \small
  \savesavecolumns
  \begin{code}
  data Pipelined = NonPipelined | Pipelined
  data Peer   ps
             (pr :: PeerRole)
             (pl :: Pipelined)
             (q :: Queue ps)
             (st :: ps) m a where

    Effect  ::  m (Peer ps pr pl q st m a)
            ->  Peer ps pr pl q st m a


    Done    ::  SingI st
            =>  (ReflRelativeAgency  (StateAgency st)
                                     NobodyHasAgency
                                     (Relative pr (StateAgency st)))
            ->  a
            ->  Peer ps pr pl Empty st m a
  \end{code}
\end{frame}
\begin{frame}
  \frametitle{Protocol Application API}
  \framesubtitle{deep embedding: non-pipelined primitives}
  \small
  Send a message (non-pipelined) and continue in a new state.  Requires a proof
  that the sender has agency (\texttt{WeHaveAgency}).
  \restorecolumns
  \begin{code}
    Yield   ::  SingI st
            =>  (ReflRelativeAgency  (StateAgency st)
                                     WeHaveAgency
                                     (Relative pr (StateAgency st)))
            ->  Message ps st st'
            ->  Peer ps pr pl Empty st' m a
            ->  Peer ps pr pl Empty st  m a
  \end{code}
  Receive a message (non-pipelined), and continue at a new state.  Requires an
  evidence that the remote side has agency (\texttt{TheyHaveAgency}).
  \restorecolumns
  \begin{code}
    Await  ::  SingI st
           =>  (ReflRelativeAgency  (StateAgency st)
                                    TheyHaveAgency
                                    (Relative pr (StateAgency st)))
           ->  (forall st'. Message ps st st' -> Peer ps pr pl Empty st' m a)
           ->  Peer ps pr pl Empty st  m a
  \end{code}
\end{frame}

\begin{frame}
  \frametitle{Protocol Application API}
  \framesubtitle{deep embedding: pipelined primitives}
  \small
  \vspace{5mm}
  Pipeline a message, register the expected transition in the queue of suspended
  transitions, and continue possibly pipelining more messages.
  \restorecolumns
  \begin{code}
    YieldPipelined
      :: (SingI st, SingI st')
      => (ReflRelativeAgency  (StateAgency st)
                              WeHaveAgency
                              (Relative pr (StateAgency st)))
      -> Message ps st st'
      -> Peer ps pr Pipelined (q |> Tr st' st'')  st''  m a
      -> Peer ps pr Pipelined (q               )  st    m a
  \end{code}
\end{frame}

\begin{frame}
  \frametitle{Protocol Application API}
  \framesubtitle{deep embedding: pipelined primitives}
  \small
  Receive a message as part of suspended transition.  It requires an evidence
  that the remote side has agency for the state \texttt{st'}.
  \restorecolumns
  \begin{code}
    Collect
      ::  SingI st'
      =>  (ReflRelativeAgency  (StateAgency st')
                               TheyHaveAgency
                               (Relative pr (StateAgency st')))
      ->  Maybe (Peer ps pr Pipelined (Tr st' st'' <| q)  st m a)
      ->  (forall stNext. Message ps st' stNext
           -> Peer ps pr Pipelined (Tr stNext st'' <| q)  st m a)
      ->  Peer     ps pr Pipelined (Tr st'    st'' <| q)  st m a
  \end{code}

  Collect an identity transition from the front of the queue.
  \restorecolumns
  \begin{code}
    CollectDone
      ::  Peer ps pr Pipelined  (             q)  st m a
      ->  Peer ps pr Pipelined  (Tr st st <|  q)  st m a
  \end{code}
\end{frame}

\begin{frame}
  \frametitle{Pipelined Ping Pong client}
  \small
  \begin{code}
  pingPongClientPipelined
      :: Peer PingPong AsClient Pipelined Empty StIdle m ()
  pingPongClientPipelined a =
       YieldPipelined ReflClientAgency MsgPing
     $ YieldPipelined ReflClientAgency MsgPing
     $ YieldPipelined ReflClientAgency MsgPing
     $ collect
     $ collect
     $ collect
     $ Yield ReflClientAgency MsgDone
     $ Done  ReflNobodyAgency ()
   where
     collect  ::  Peer  PingPong AsClient Pipelined q StIdle m ()
              ->  Peer  PingPong AsClient Pipelined 
                        (Tr StBusy StIdle <| q)  StIdle m ()
     collect k =
         Collect TheyHaveAgency Nothing
       $ \  msg -> case msg of
            MsgPong -> CollectDone k
  \end{code}
\end{frame}

\begin{frame}
  \frametitle{Ping Pong v2}
  \small
  \begin{figure}
    \includegraphics{../images/ping-pong-3.png}
  \end{figure}
  \begin{code}
newtype PingPong2 = Wrap PingPong
type StIdle2  = Wrap StIdle
type StBusy2  = Wrap StBusy
type StDone2  = Wrap StDone
  \end{code}
\end{frame}

\begin{frame}
  \frametitle{Ping Pong v2}
  \small
  \begin{code}
instance Protocol PingPong2 where
  data Message PingPong2 from to where
    MsgPingPong
      ::  Message PingPong   (      st)      (      st')
      ->  Message PingPong2  (Wrap  st)      (Wrap  st')
    MsgBusy
      ::  Message PingPong2  (Wrap  StBusy)  (Wrap StBusy)

    type StateAgency (Wrap StIdle)  = StateAgency StIdle
    type StateAgency (Wrap StBusy)  = StateAgency StBusy
    type StateAgency (Wrap StDone)  = StateAgency StDone
  \end{code}
\end{frame}

\begin{frame}
  \frametitle{Pipelined Ping Pong v2 Client}
  \footnotesize
  \begin{code}
pingPongClientPipeliend2
  :: Peer PingPong2 AsClient Pipelined Empty StIdle2 m Int
pingPongClientPipeliend2 =
     YieldPipelined WeHaveAgency (MsgPingPong MsgPing)
  $  YieldPipelined WeHaveAgency (MsgPingPong MsgPing)
  $  YieldPipelined WeHaveAgency (MsgPingPong MsgPing)
  $            collect 0
  $  \  n1 ->  collect n1
  $  \  n2 ->  collect n2
  $  \  n3 ->  Yield WeHaveAgency (MsgPingPong MsgDone)
  $            Done NobodyHasAgency n3
 where
   collect ::  Int
           ->  (Int -> Peer PingPong2 AsClient Pipelined q StIdle2 m Int)
           ->  Peer  PingPong2 AsClient Pipelined
                     (Tr StBusy2 StIdle2 <| q) StIdle2 m Int
   collect !n k =
        Collect TheyHaveAgency Nothing
     $  \  msg -> case msg of
           MsgBusy               ->  collect     (n+1)  k
           (MsgPingPong MsgPong) ->  CollectDone        (k n)
  \end{code}
\end{frame}

\section{Theorems}

\begin{frame}
  \frametitle{Non-pipelined Duality}
  \small
  \begin{code}
data TerminalStates ps (pr :: PeerRole) where
     TerminalStates
       :: forall ps pr (st :: ps) (st' :: ps).
           Sing st
       ->  ReflRelativeAgency  (StateAgency st)
                               NobodyHasAgency
                               (Relative (            pr)  (StateAgency st))
       ->  Sing st'
       ->  ReflRelativeAgency  (StateAgency st')
                               NobodyHasAgency
                               (Relative (FlipAgency  pr) (StateAgency st'))
       ->  TerminalStates ps pr

theorem_nonpipelined_duality
   :: forall ps (pr :: PeerRole) (initSt :: ps) m a b.
      (Monad m,  SingI pr)
   => Peer ps (            pr)  NonPipelined Empty initSt m a
   -> Peer ps (FlipAgency  pr)  NonPipelined Empty initSt m b
   -> m (a, b, TerminalStates ps)
  \end{code}
  Link to the
  \href{https://github.com/input-output-hk/ouroboros-network/blob/coot/typed-protocols-rewrite/typed-protocols/src/Network/TypedProtocol/Proofs.hs\#L83}{proof}.
  The proof relies on exclusion lemmas.
\end{frame}

\begin{frame}
  \frametitle{Removing pipelining}
  \small
  \begin{code}
theorem_unpipeline
    :: forall ps (pr :: PeerRole)
                 (pl :: Pipelined)
                 (initSt :: ps) m a.
       Functor m
    => [Bool]
    -- interleaving choices for pipelining allowed by
    -- `Collect` primitive. False values or `[]` give no
    -- pipelining.
    -> Peer ps pr pl            Empty initSt m a
    -> Peer ps pr NonPipelined  Empty initSt m a
  \end{code}

  Link to the \href{https://coot.me/posts/typed-protocol-pipelining.html\#removing-pipelining}{proof}.
\end{frame}

\begin{frame}
  \frametitle{Pipelined Duality}
  \small
  \begin{code}
theorem_duality
    :: forall ps (pr :: PeerRole)
                 (pl :: Pipelined)
                 (st :: ps) m a b.
       (  Monad m
       ,  SingI pr
       )
    =>  [Bool] -> [Bool]
    ->  Peer ps  (            pr) pl   Empty st m a
    ->  Peer ps  (FlipAgency  pr) pl'  Empty st m b
    ->  m (a, b, TerminalStates ps)
theorem_duality csA csB a b =
    theorem_nonpipelined_duality (theorem_unpipeline csA a)
                                 (theorem_unpipeline csB b)
  \end{code}
\end{frame}

\begin{frame}
  \frametitle{Remarks}
  \begin{itemize}
    \item The duality theorem relies on 1-1 encoding of protocol
      messages; Non injective encodings can lead to deadlocks, or premature
      termination.
    \item Non injective encodings are useful!  A protocol that handles
      simultaneous TCP open is an example.
    \item The presented \texttt{Peer} type was first discovered in
      \texttt{Agda}, and then re-implemented in Haskell.  \texttt{Agda}'s more
      expressive type system, and quite similar syntax to Haskell, makes it
      ideal for type level experiments which as in this case can lead to
      simpler API.
  \end{itemize}
\end{frame}

\end{document}
