> {-# LANGUAGE DataKinds #-}
> {-# LANGUAGE GADTs #-}
> {-# LANGUAGE FlexibleInstances #-}
> {-# LANGUAGE FunctionalDependencies #-}
> {-# LANGUAGE KindSignatures #-}
> {-# LANGUAGE MultiParamTypeClasses #-}
> {-# LANGUAGE NamedFieldPuns #-}
> {-# LANGUAGE PolyKinds #-}
> {-# LANGUAGE RankNTypes #-}
> {-# LANGUAGE ScopedTypeVariables #-}
> {-# LANGUAGE TypeFamilies #-}

> module CategoriesWithEffects where

> import Prelude hiding (id, (.))

> import Control.Arrow (Kleisli (..))
> import Control.Category (Category (..))
> import Control.Monad (void)
> import Numeric.Natural (Natural)
> import Data.Functor (($>))
> import Data.Functor.Identity (Identity (..))
> import Data.List.NonEmpty (NonEmpty (..))
> import qualified Data.List.NonEmpty as NE

> import Control.Algebra.Free (AlgebraType, AlgebraType0, proof)
> import Control.Algebra.Free2 (FreeAlgebra2 (..))

> import Test.QuickCheck

Free category
=============

> data Cat :: (k -> k -> *) -> k -> k -> * where
>   Id    :: Cat f a a
>   (:.:) :: f b c -> Cat f a b -> Cat f a c

> instance Category (Cat f) where
>   id = Id
>   Id         . ys = ys
>   (x :.: xs) . ys = x :.: (xs . ys)

> type instance AlgebraType0 Cat f = ()
> type instance AlgebraType  Cat c = Category c
> 
> instance FreeAlgebra2 Cat where
>   liftFree2 = (:.: Id)
> 
>   foldNatFree2 _   Id          = id
>   foldNatFree2 fun (bc :.: ab) = fun bc . foldNatFree2 fun ab
> 
>   codom2  = proof
>   forget2 = proof

Categories with effects
=======================

> -- | Categories which can lift monadic actions, i.e. effectful categories.
> --
> class Category c => EffCategory c m | c -> m where
>   lift :: m (c a b) -> c a b

> instance Monad m => EffCategory (Kleisli m) m where
>   lift m = Kleisli (\a -> m >>= \(Kleisli f) -> f a)

> instance EffCategory (->) Identity where
>   lift = runIdentity

> -- | Category transformer, which adds @'EffCategory'@ instance to the
> -- underlying base category.
> --
> data FreeEffCat :: (* -> *) -> (k -> k -> *) -> k -> k -> * where
>   Base :: c a b -> FreeEffCat m c a b
>   Lift :: m (FreeEffCat m c a b) -> FreeEffCat m c a b

> instance (Functor m, Category c) => Category (FreeEffCat m c) where
>   id = Base id
>   Base f  . Base g  = Base $ f . g
>   f       . Lift mg = Lift $ (f .) <$> mg
>   Lift mf . g       = Lift $ (. g) <$> mf

> instance (Functor m, Category c) => EffCategory (FreeEffCat m c) m where
>   lift = Lift

> type instance AlgebraType0 (FreeEffCat m) c = (Monad m, Category c)
> type instance AlgebraType  (FreeEffCat m) c  = EffCategory c m
> instance Monad m => FreeAlgebra2 (FreeEffCat m) where
>   liftFree2    = Base
>   foldNatFree2 nat (Base cab)  = nat cab
>   foldNatFree2 nat (Lift mcab) = lift $ foldNatFree2 nat <$> mcab

>   codom2  = proof
>   forget2 = proof

> -- | Wrap a transition into a free category @'Cat'@ and then in
> -- @'FreeEffCat'@
> --
> -- prop> liftCat tr = Base (tr :.: Id)
> --
> liftCat :: Monad m => tr a b -> FreeEffCat m (Cat tr) a b
> liftCat = liftFree2 . liftFree2

> -- | Fold @'FreeLifing'@ category based on a free category @'Cat' tr@ using
> -- a functor @tr x y -> c x y@.
> --
> foldNatLift
>   :: (Monad m, EffCategory c m)
>   => (forall x y. tr x y -> c x y)
>   -> FreeEffCat m (Cat tr) a b
>   -> c a b
> foldNatLift nat = foldNatFree2 (foldNatFree2 nat)

> -- |  Functor from @'->'@ category to @'Kleisli' m@.  If @m@ is @Identity@ then
> -- it will respect @'lift'@ i.e. @liftKleisli (lift ar) = lift (liftKleisli <$>
> -- ar).
> --
> liftKleisli :: Applicative m => (a -> b) -> Kleisli m a b
> liftKleisli f = Kleisli (pure . f)

Example state machine
=====================

> {-------------------------------------------------------------------------------
> -- Example State Machine, inspired by:
> -- `State Machines All The Way Down` by Edwin Bradly, 2017
> -- https://www.youtube.com/watch?v=xq7ZuSRgCR4
> -------------------------------------------------------------------------------}

> data LoginResult = Success | LoginError

> -- | Type level representation of the states.
> --
> data StateType where
>   LoggedInType  :: StateType
>   LoggedOutType :: StateType

> data SStateType (a :: StateType) where
>   SLoggedIn  :: SStateType 'LoggedInType
>   SLoggedOut :: SStateType 'LoggedOutType

> -- | Term level representation of the states.
> -- @'LoggedOut'@ let one carry out a value.
> --
> data State a (st :: StateType) where
>   LoggedIn  :: State a 'LoggedInType
>   LoggedOut :: Maybe a -> State a 'LoggedOutType

> runLoggedOut :: State a 'LoggedOutType -> Maybe a
> runLoggedOut (LoggedOut a) = a


> -- | Graph of transitions in the state machine.  In abstract representation the
> -- states do not show up, the only way to record some data is to add it to the
> -- transition.  Thus @'Logout'@ can carry data.  When interpreted in some
> -- category (e.g. @'Kleisli' m@) then the data will be avalable on
> -- @'LoggedOut{} :: 'State' a st@.
> --

> data Tr a from to where
>   Login  :: SStateType to -> Tr a (State a 'LoggedOutType) (State a to)
>   Logout :: Maybe a -> Tr a (State a 'LoggedInType) (State a 'LoggedOutType)
>   Access :: Tr a (State a 'LoggedInType) (State a 'LoggedInType)

> login :: Monad m
>       => SStateType st
>       -> FreeEffCat m (Cat (Tr a)) (State a 'LoggedOutType) (State a st)
> login = liftCat . Login

> logout :: Monad m
>        => Maybe a
>        -> FreeEffCat m (Cat (Tr a)) (State a 'LoggedInType) (State a 'LoggedOutType)
> logout = liftCat . Logout

> access :: Monad m
>        => FreeEffCat m (Cat (Tr a)) (State a 'LoggedInType) (State a 'LoggedInType)
> access = liftCat Access

> type Username = String

> -- * Data representation of the state machine.

> data HandleLogin m authtoken a = HandleLogin {
>     handleLogin
>       :: m (Either (HandleLogin m authtoken a) (HandleAccess m a)),
>       -- ^ either failure with a login continuation or handle access to the
>       -- secret data
>     handleAccessDenied
>       :: m ()
>       -- ^ handle access denied
>   }

> data HandleAccess m a where
>   AccessHandler
>     :: m a                         -- access secret
>     -> (a -> m (HandleAccess m a)) -- handle secret
>     -> HandleAccess m a
>   LogoutHandler :: HandleAccess m a

> handleLoginIO
>   :: String
>   -> HandleLogin IO String String
> handleLoginIO passwd = HandleLogin
>   { handleLogin
>   , handleAccessDenied
>   }
>  where
>   handleLogin = do
>     passwd' <- putStrLn "Provide a password:" >> getLine
>     if passwd' == passwd
>       then return $ Right handleAccess
>       else return $ Left $ handleLoginIO passwd

>   handleAccess = AccessHandler (pure "Hello saylor!") $
>     \s -> do
>       putStrLn ("secret: " ++ s)
>       return LogoutHandler

>   handleAccessDenied = putStrLn "AccessDenied"

> -- pure @'HandleLogin'@ useful for testing @'accessSecret'@
> handleLoginPure
>   :: NonEmpty String -- ^ passwords to try (cyclicly, ad infinitum)
>   -> String          -- ^ authtoken
>   -> String          -- ^ secret
>   -> HandleLogin Identity String String
> handleLoginPure passwds passwd secret = HandleLogin
>   { handleLogin = handleLogin passwds
>   , handleAccessDenied = pure ()
>   }
>  where
>   handleLogin (passwd' :| rest) = 
>     if passwd' == passwd
>       then return $ Right handleAccess
>       else case rest of 
> 	[]  -> return $ Left $ handleLoginPure passwds passwd secret
> 	_   -> return $ Left $ handleLoginPure (NE.fromList rest) passwd secret

>   handleAccess = AccessHandler (pure secret) $ \_ -> return LogoutHandler

> -- | Abstract access function
> --
> accessSecret
>   :: forall m a . Monad m
>   => Natural
>   -- ^ how many times one can try to login; this could be implemented inside
>   -- @'HandleLogin'@ (with a small modifications) but this way we are able to
>   -- test it with a pure @'HandleLogin'@ (see @'handleLoginPure'@).
>   -> HandleLogin m String a
>   -> FreeEffCat m (Cat (Tr a)) (State a 'LoggedOutType) (State a 'LoggedOutType)
> accessSecret 0 HandleLogin{handleAccessDenied}         = lift $ handleAccessDenied $> id
> accessSecret n HandleLogin{handleLogin} = lift $ do
>   st <- handleLogin
>   case st of
>     -- login success
>     Right accessHandler -> return $ handle accessHandler Nothing . login SLoggedIn
>     -- login failure
>     Left handler'       -> return $ accessSecret (pred n) handler'
>  where
>   handle :: HandleAccess m a -> Maybe a -> FreeEffCat m (Cat (Tr a)) (State a 'LoggedInType) (State a 'LoggedOutType)
>   handle LogoutHandler ma = logout ma
>   handle (AccessHandler accessHandler dataHandler) _ = lift $ do
>     a <- accessHandler
>     accessHandler' <- dataHandler a
>     return $ handle accessHandler' (Just a)

> -- | Get data following the protocol defined by the state machine.
> -- 
> -- Note: in GHC-8.6.1 we'd need @'MonadFail'@ which prevents from running this in
> -- @'Identity'@ monad.  To avoid this we use the @'runLoggedOut'@ function.
> getData
>   :: forall m a . Monad m
>   => (forall x y. Tr a x y -> Kleisli m x y)
>   -> Natural
>   -> HandleLogin m String a
>   -> m (Maybe a)
> getData nat n handleLogin = case foldNatLift nat (accessSecret n handleLogin) of
>   Kleisli fn ->
>     runLoggedOut <$> fn (LoggedOut Nothing)

> -- * Interpreters
> -- To write an interpreter it is enough to supply a natural transformation from
> -- @'Tr' a from to@ to @'Kleisli' m@ for some monad @m@.

> -- | A pure natural transformation from @'Tr'@ to @'Kleisli' m@ for some
> -- @'Monad' m@.  Note, that even though @'Kleisli'@ category seems redundant
> -- here, as we don't use the monad in the transformation, we need
> -- a transformation into a category that satisfies the @'Lifing'@ constraint.
> -- This is because we will need the monad whn @'foldNatLift'@ will walk over the
> -- constructors of the '@FreeLifting'@ category.
> --
> natPure :: forall m a from to. Monad m => Tr a from to -> Kleisli m from to
> natPure = liftKleisli . nat
>  where
>   -- a natural trasformation to @'->'@
>   nat :: Tr a from to -> (from -> to)
>   nat (Login SLoggedIn)  = \_ -> LoggedIn
>   nat (Login SLoggedOut) = \_ -> LoggedOut Nothing
>   nat (Logout ma)        = \_ -> LoggedOut ma
>   nat Access             = \_ -> LoggedIn

> prop_getData
>   :: NonEmptyList String
>   -> String
>   -> String
>   -> Positive Int
>   -> Property
> prop_getData (NonEmpty passwds) passwd secret (Positive n)= 
>   let res = runIdentity $ getData natPure (fromIntegral n) (handleLoginPure (NE.fromList passwds) passwd secret)
>   in if passwd `elem` take n passwds
>     then res === Just secret
>     else res === Nothing

> -- | A trivial program, which extracts a trivial secret.
> main :: IO ()
> main = do
>   putStrLn ""
>   quickCheck prop_getData
>   putStrLn ""
>   void $ getData natPure 3 (handleLoginIO "password")
