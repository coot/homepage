{-# LANGUAGE BangPatterns #-}

module Test where

import qualified Control.Monad.ST.Lazy as Lazy
import qualified Control.Monad.ST.Lazy.Unsafe as Lazy
import qualified Data.STRef.Lazy as Lazy

import qualified Control.Monad.ST as Strict
import qualified Control.Monad.ST.Unsafe as Strict
import qualified Data.STRef as Strict
import qualified Debug.Trace as Debug


a :: Int
a = Strict.runST $ do 
  v <- Strict.newSTRef (0 :: Int)
  x <- Strict.unsafeInterleaveST $ Strict.readSTRef v
  _ <- Debug.trace "modifySTRef" $ Strict.modifySTRef v (+1)
  return $ x


b :: Int
b = Lazy.runST $ do 
  v <- Lazy.newSTRef (0 :: Int)
  x <- Lazy.unsafeInterleaveST   $ Lazy.readSTRef v
  _ <- Debug.trace "modifySTRef" $ Lazy.modifySTRef v (+1)
  !_ <- return ()
  return x

c :: Int
c = Lazy.runST $ do 
  v  <- Lazy.newSTRef (0 :: Int)
  x  <- Lazy.unsafeInterleaveST   $ Lazy.readSTRef v
  !_ <- Debug.trace "modifySTRef" $ Lazy.modifySTRef v (+1)
  return $ x
