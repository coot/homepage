module posts.agda.typed-protocols where

open import Relation.Binary.PropositionalEquality as Eq
open Eq using (_≡_; refl)
open import Relation.Nullary using (¬_)
open import Data.Empty using (⊥; ⊥-elim)
open import Data.List
open import Data.Nat
open import Data.Unit using (⊤; tt)
open import Function.Base using (_$_; _|>_)


-- Client / server roles
-- 
data PeerRole : Set where
  ClientRole : PeerRole
  ServerRole : PeerRole

dual-role : PeerRole → PeerRole
dual-role ClientRole = ServerRole
dual-role ServerRole = ClientRole

-- Objective agency: who is responsible for sending next message.
--
data Agency : Set where
  ClientAgency : Agency
  ServerAgency : Agency
  NobodyAgency : Agency


-- Relative agency to the current 'PeerRole'.  It answer the question: are we
-- responsible for sending next message?
--
data RelativeAgency : Set where
  WeHaveAgency    : RelativeAgency
  TheyHaveAgency  : RelativeAgency
  NobodyHasAgency : RelativeAgency


relative : PeerRole   → Agency       → RelativeAgency

relative   ClientRole   ClientAgency = WeHaveAgency
relative   ClientRole   ServerAgency = TheyHaveAgency
relative   ClientRole   NobodyAgency = NobodyHasAgency

relative   ServerRole   ClientAgency = TheyHaveAgency
relative   ServerRole   ServerAgency = WeHaveAgency
relative   ServerRole   NobodyAgency = NobodyHasAgency


-- 'relative' function obeys the following three exclusion lemmas:
--
-- * it is an absurd if client and server have agency
-- * it is an absurd if 'WeHaveAgency'   and the dual role terminated
--   ('NobodyHasAgency')
-- * it is an absurd if 'TheyHaveAgency' and the dual role terminated
--   ('NobodyHasAgency')
--
-- Note that these lemmas are provided regardless of the protocol, unlike the
-- Haskell implementation which requires to prove them for each protocol.

exclusion-lemma-client-and-server-have-agency₁
  : ∀ {agency : Agency}
      {pr     : PeerRole}
  → WeHaveAgency ≡ relative            pr  agency
  → WeHaveAgency ≡ relative (dual-role pr) agency
  → ⊥
exclusion-lemma-client-and-server-have-agency₁
  {ClientAgency}
  {ClientRole}
  refl ()
exclusion-lemma-client-and-server-have-agency₁
  {ServerAgency}
  {ServerRole}
  refl ()


exclusion-lemma-client-and-server-have-agency₂
  : ∀ {agency : Agency}
      {pr     : PeerRole}
  → TheyHaveAgency ≡ relative            pr  agency
  → TheyHaveAgency ≡ relative (dual-role pr) agency
  → ⊥
exclusion-lemma-client-and-server-have-agency₂
  {ServerAgency}
  {ClientRole}
  refl ()
exclusion-lemma-client-and-server-have-agency₂
  {ClientAgency}
  {ServerRole}
  refl ()


exclusion-lemma-we-have-agency-and-nobody-has-agency
  : ∀ {agency  : Agency}
      {pr      : PeerRole}
      {pr'     : PeerRole}
  → WeHaveAgency    ≡ relative pr  agency
  → NobodyHasAgency ≡ relative pr' agency
  → ⊥
exclusion-lemma-we-have-agency-and-nobody-has-agency
  {ClientAgency}
  {ClientRole}
  {ServerRole}
  refl ()
exclusion-lemma-we-have-agency-and-nobody-has-agency
  {ServerAgency}
  {ServerRole}
  {ClientRole}
  refl ()


exclusion-lemma-they-have-agency-and-nobody-has-agency
  : ∀ {agency : Agency}
      {pr     : PeerRole}
      {pr'    : PeerRole}
  → TheyHaveAgency  ≡ relative pr  agency
  → NobodyHasAgency ≡ relative pr' agency
  → ⊥
exclusion-lemma-they-have-agency-and-nobody-has-agency
  {ServerAgency}
  {ClientRole}
  {ServerRole}
  refl ()
exclusion-lemma-they-have-agency-and-nobody-has-agency
  {ClientAgency}
  {ServerRole}
  {ClientRole}
  refl ()


-----------
-- Peer API
--


-- We index each primitive with 'IsPipelined'
--
data IsPipelined : Set where
  Pipelined    : IsPipelined
  NonPipelined : IsPipelined


-- Promised protocol transition
--
data Trans (ps : Set) : Set where
  Tr : ps → ps → Trans ps


-- 'Peer' explicitly indexed by:
-- * message type
-- * objective protocol agency
-- * peer role
-- * return type
-- * 'IsPipelined'
-- * queue of unrealised transitions, due to pipelining
-- * current state
--
data Peer {ps     : Set}
          (msg    : ps → ps → Set)
          (agency : ps → Agency)
          (pr     : PeerRole)
          (a      : Set)
        : IsPipelined
        → List (Trans ps)
        → ps
        → Set where

  --
  -- non-pipelined primitives
  --

  -- non-pipelined send a message
  Yield : ∀ {st st' : ps}
            {pl     : IsPipelined}
        → WeHaveAgency ≡ relative pr (agency st)
        → msg st st'
        → Peer msg agency pr a pl [] st'
        → Peer msg agency pr a pl [] st

  -- non-pipelined receive a message
  Await : ∀ {st : ps}
            {pl : IsPipelined}
        → TheyHaveAgency ≡ relative pr (agency st)
        → (∀ {st' : ps}
           → msg st st'
           → Peer msg agency pr a pl [] st'
          )
        → Peer    msg agency pr a pl [] st

  -- protocol termination
  Done  : ∀ {st : ps}
            {pl : IsPipelined}
        → NobodyHasAgency ≡ relative pr (agency st)
        → a
        → Peer msg agency pr a pl [] st

  --
  -- pipelining primitives
  --

  -- pipeline a single message
  YieldPipelined
        : ∀ {st st' st'' : ps}
            {q           : List (Trans ps)}
        → WeHaveAgency ≡ relative pr (agency st)
        → msg st st'
        → Peer msg agency pr a Pipelined (q ∷ʳ Tr st' st'') st''
        → Peer msg agency pr a Pipelined  q                 st

  -- partially collect a promissed transition
  Collect
        : ∀ {st st' st'' : ps}
            {q           : List (Trans ps)}
        → TheyHaveAgency ≡ relative pr (agency st')
        → (∀ {stNext : ps}
           → msg st' stNext
           → Peer msg agency pr a Pipelined (Tr stNext st'' ∷ q) st
          )
        → Peer    msg agency pr a Pipelined (Tr st'    st'' ∷ q) st

  -- collect the identity transition
  CollectDone
        : ∀ {st : ps}
            {q  : List (Trans ps)}
        → Peer msg agency pr a Pipelined             q  st
        → Peer msg agency pr a Pipelined (Tr st st ∷ q) st
  
--------------
-- PingPong v1
--

-- Protocol states
--
data PingPong : Set where
  StIdle : PingPong
  StBusy : PingPong
  StDone : PingPong


-- Agency of PingPong states
--
pingPongAgency : PingPong → Agency
pingPongAgency StIdle = ClientAgency
pingPongAgency StBusy = ServerAgency
pingPongAgency StDone = NobodyAgency


-- Protocol messages
--
data PingPongMsg : ∀ (st st' : PingPong) → Set where
  MsgPing : PingPongMsg StIdle StBusy
  MsgPong : PingPongMsg StBusy StIdle
  MsgDone : PingPongMsg StIdle StDone

--
-- PingPong v1, examples
--


-- ping client which computes unit (tt : ⊤)
--
ping : Peer PingPongMsg pingPongAgency ClientRole ⊤ NonPipelined [] StIdle
ping =
      Yield refl MsgPing
    $ await
    $ Yield refl MsgPing
    $ await
    $ Yield refl MsgDone
    $ Done refl tt
  where
    await : Peer PingPongMsg pingPongAgency ClientRole ⊤ NonPipelined [] StIdle
          → Peer PingPongMsg pingPongAgency ClientRole ⊤ NonPipelined [] StBusy
    await k = Await refl λ {MsgPong → k}


-- pipelined client which computes unit (tt : ⊤)
--
pipelinedPing
    : Peer PingPongMsg pingPongAgency ClientRole ⊤ Pipelined [] StIdle
pipelinedPing =
      YieldPipelined refl MsgPing
    $ YieldPipelined refl MsgPing
    $ YieldPipelined refl MsgPing
    $ collect
    $ collect
    $ collect
    $ Yield refl MsgDone
    $ Done  refl tt
  where
    collect : ∀ {q : List (Trans PingPong)}
            → Peer PingPongMsg pingPongAgency ClientRole ⊤ Pipelined q  StIdle
            → Peer PingPongMsg pingPongAgency ClientRole ⊤ Pipelined
                                                 (Tr StBusy StIdle ∷ q) StIdle
    collect k = 
      Collect refl λ {MsgPong → CollectDone k}


--------------
-- PingPong v2
--
-- The same states and agency as PingPong v1, but with additional 'MsgBusy'
-- transition.


data PingPongMsg2 : ∀ (st st' : PingPong) → Set where
  MsgPingPong : ∀ {st st' : PingPong}
              → PingPongMsg st st' → PingPongMsg2 st st'
  MsgBusy     : PingPongMsg2 StBusy StBusy

-- we use unbounded recursion in 'pipelinedPing2'
{-# NON_TERMINATING #-}

-- pipelined ping client which computes the number of busy messages
--
pipelinedPing2
    : Peer PingPongMsg2 pingPongAgency ClientRole ℕ Pipelined [] StIdle
pipelinedPing2 =
      YieldPipelined refl (MsgPingPong MsgPing)
    $ YieldPipelined refl (MsgPingPong MsgPing)
    $ YieldPipelined refl (MsgPingPong MsgPing)
    $          collect 0
    $ λ { n1 → collect n1
      λ { n2 → collect n2
      λ { n3 → Yield refl (MsgPingPong MsgDone)
             $ Done  refl n3 }}}
  where
    collect : ∀ {q : List (Trans PingPong)}
            → ℕ
            → (ℕ → Peer PingPongMsg2 pingPongAgency ClientRole ℕ Pipelined q  StIdle)
            → Peer PingPongMsg2 pingPongAgency ClientRole ℕ Pipelined
                                                       (Tr StBusy StIdle ∷ q) StIdle
    collect n k =
      Collect refl 
        λ {  MsgBusy              → collect (n + 1) k
          ; (MsgPingPong MsgPong) → CollectDone (k n)
          } 

------------------------
-- Non-pipelined Duality
--

-- Termination witness
data Termination (ps     : Set)
                 (agency : ps → Agency)
                 (a      : Set)
                 (b      : Set)
               : Set where
  Terminated : ∀ {st : ps} {pr : PeerRole}
             → a
             → b
             → NobodyHasAgency ≡ relative            pr  (agency st)
             → NobodyHasAgency ≡ relative (dual-role pr) (agency st)
             → Termination ps agency a b


theorem-non-pipelined-duality
  : ∀ {ps     : Set}
      {msg    : ps → ps → Set}
      {agency : ps → Agency}
      {pr     : PeerRole}
      {a      : Set}
      {b      : Set}
      {st     : ps}
  → Peer msg agency            pr  a NonPipelined [] st
  → Peer msg agency (dual-role pr) b NonPipelined [] st
  → Termination ps agency a b

theorem-non-pipelined-duality (Yield _ msg k)  (Await _ k')     =
  theorem-non-pipelined-duality k (k' msg)

theorem-non-pipelined-duality (Await _ k)      (Yield _ msg k') =
  theorem-non-pipelined-duality (k msg) k'

theorem-non-pipelined-duality (Done termA a)   (Done termB b)   =
  Terminated a b termA termB

-- excluded cases

theorem-non-pipelined-duality (Yield weHaveAgency _ _)
                              (Yield theyHaveAgency _ _)  =
  ⊥-elim (exclusion-lemma-client-and-server-have-agency₁ weHaveAgency theyHaveAgency)

theorem-non-pipelined-duality (Await theyHaveAgency _)
                              (Await weHaveAgency _) =
  ⊥-elim (exclusion-lemma-client-and-server-have-agency₂ theyHaveAgency weHaveAgency)

theorem-non-pipelined-duality (Yield weHaveAgency _ _)
                              (Done nobodyHasAgency _) =
  ⊥-elim (exclusion-lemma-we-have-agency-and-nobody-has-agency
            weHaveAgency
            nobodyHasAgency)

theorem-non-pipelined-duality (Done nobodyHasAgency _)
                              (Yield weHaveAgency _ _) =
  ⊥-elim (exclusion-lemma-we-have-agency-and-nobody-has-agency
            weHaveAgency
            nobodyHasAgency)

theorem-non-pipelined-duality (Await theyHaveAgency _)
                              (Done nobodyHasAgency _) =
  ⊥-elim (exclusion-lemma-they-have-agency-and-nobody-has-agency
            theyHaveAgency
            nobodyHasAgency)

theorem-non-pipelined-duality (Done nobodyHasAgency _)
                              (Await theyHaveAgency _) =
  ⊥-elim (exclusion-lemma-they-have-agency-and-nobody-has-agency
            theyHaveAgency
            nobodyHasAgency)


--------------
-- Un-pipeline
--

-- Transition queue which allows to transform pipelined 'Peer' into
-- non-pipelined one.   Pipelined messages are pushed to the end together with
-- promised transitions to be collected.
--
data PrQueue {ps     : Set}
             (msg    : ps → ps → Set)
             (agency : ps -> Agency)
             (pr     : PeerRole)
           : ps
           → List (Trans ps)
           → ps
           → Set where

  ConsMsgQ : ∀ {st st' st'' : ps}
               {q           : List (Trans ps)}
           → WeHaveAgency ≡ relative pr (agency st)
           → msg st st'
           → PrQueue msg agency pr st' q st''
           → PrQueue msg agency pr st  q st''

  ConsTrQ  : ∀ {st st' st'' : ps}
               {q           : List (Trans ps)}
           → PrQueue msg agency pr st'             q  st''
           → PrQueue msg agency pr st (Tr st st' ∷ q) st''

  EmptyQ   : ∀ {st : ps}
           → PrQueue msg agency pr st [] st


snockMsgQ : ∀ {ps          : Set}
              {msg         : ps → ps → Set}
              {agency      : ps → Agency}
              {pr          : PeerRole}
              {st st' st'' : ps}
              {q           : List (Trans ps)}
          → WeHaveAgency ≡ relative pr (agency st')
          → msg st' st''
          → PrQueue msg agency pr st q st'
          → PrQueue msg agency pr st q st''
snockMsgQ tok msg (ConsMsgQ tok' msg' q) = ConsMsgQ tok' msg' (snockMsgQ tok msg q)
snockMsgQ tok msg (ConsTrQ q)            = ConsTrQ (snockMsgQ tok msg q)
snockMsgQ tok msg  EmptyQ                = ConsMsgQ tok msg EmptyQ


snockTrQ : ∀ {ps          : Set}
             {msg         : ps → ps → Set}
             {agency      : ps → Agency}
             {pr          : PeerRole}
             {st st' st'' : ps}
             {q           : List (Trans ps)}
         → PrQueue msg agency pr st  q st'
         → PrQueue msg agency pr st (q ∷ʳ Tr st' st'') st''
snockTrQ (ConsMsgQ tok msg q) = ConsMsgQ tok msg (snockTrQ q)
snockTrQ (ConsTrQ q)          = ConsTrQ (snockTrQ q)
snockTrQ  EmptyQ              = ConsTrQ EmptyQ


-- Every pipelined peer can be transformed into non-pipelined one, by
-- preserving the order of all transition.
--
theorem-unpipeline
  : ∀ {ps     : Set}
      {msg    : ps → ps → Set}
      {agency : ps → Agency}
      {pr     : PeerRole}
      {pl     : IsPipelined}
      {a      : Set}
      {stInit : ps}
  → Peer msg agency pr a pl           [] stInit
  → Peer msg agency pr a NonPipelined [] stInit
theorem-unpipeline = go EmptyQ
  where
    go : ∀ {ps     : Set}
           {msg    : ps → ps → Set}
           {agency : ps → Agency}
           {pr     : PeerRole}
           {pl     : IsPipelined}
           {a      : Set}
           {q      : List (Trans ps)}
           {st st' : ps}
      → PrQueue msg agency pr             st q  st'
      → Peer    msg agency pr a pl           q  st'
      → Peer    msg agency pr a NonPipelined [] st

    -- non-piplined primitives
    go EmptyQ (Done  tok a)     = Done tok a
    go EmptyQ (Yield tok msg k) = Yield tok msg (go EmptyQ k)
    go EmptyQ (Await tok k)     = Await tok λ {msg → go EmptyQ (k msg)}

    -- push msg and promissed transition to back of the 'PrQueue'
    go q      (YieldPipelined tok msg k) =
      go (  q
         |> snockMsgQ tok msg
         |> snockTrQ
         )
         k

    go (ConsMsgQ tok msg q) k =
      Yield tok msg (go q k)

    go (ConsTrQ q) (Collect tok k) =
      Await tok λ {msg → go (ConsTrQ q) (k msg)}

    go (ConsTrQ q) (CollectDone k) =
      go q k


----------
-- Duality
--


theorem-pipelined-duality
  : ∀ {ps     : Set}
      {msg    : ps → ps → Set}
      {agency : ps → Agency}
      {pr     : PeerRole}
      {pl     : IsPipelined}
      {a      : Set}
      {b      : Set}
      {st     : ps}
  → Peer msg agency            pr  a pl [] st
  → Peer msg agency (dual-role pr) b pl [] st
  → Termination ps agency a b
theorem-pipelined-duality a b =
  theorem-non-pipelined-duality (theorem-unpipeline a)
                                (theorem-unpipeline b)
