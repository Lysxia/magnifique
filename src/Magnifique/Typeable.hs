{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}

module Magnifique.Typeable where

import Data.List (intercalate)
import Data.Proxy
import Data.Typeable
import GHC.Generics

data Zipper = Zipper Context SomeFocus

type Context = [SimpleContext]

data SimpleContext where
  Co :: IsContext ctx => ctx -> SimpleContext

data Some c where
  Some :: c a => a -> Some c

data SomeFocus where
  SomeFocus :: IsContext ctx => proxy ctx -> Full ctx -> SomeFocus

type Key = Int

class (Typeable (Full ctx), Typeable ctx) => IsContext ctx where
  type Full ctx :: *
  showRoot :: proxy ctx -> Full ctx -> String
  showHole :: ctx -> String
  key :: ctx -> Key
  down :: Full ctx -> Key -> Maybe (ctx, SomeFocus)
  cons :: ctx -> SomeFocus -> Maybe (Full ctx)

_unzip :: IsContext ctx => proxy ctx -> Full ctx -> Zipper
_unzip proxy a = Zipper [] (SomeFocus proxy a)

moveDown :: Int -> Zipper -> Maybe Zipper
moveDown n (Zipper ctx (SomeFocus (_ :: _p ctx) v)) = do
  (c, v') <- down @ctx v (toEnum n)
  return (Zipper (Co c : ctx) v')

moveUp :: Zipper -> Maybe Zipper
moveUp (Zipper ctx v) = do
  Co (c :: c) : ctx <- return ctx
  a <- cons c v
  return (Zipper ctx (SomeFocus (Proxy @c) a))

moveLeft :: Zipper -> Maybe Zipper
moveLeft (Zipper ctx v) = do
  Co (c :: c) : ctx <- return ctx
  a <- cons c v
  (c, v') <- down @c a (pred (key c))
  return (Zipper (Co c : ctx) v')

moveRight :: Zipper -> Maybe Zipper
moveRight (Zipper ctx v) = do
  Co (c :: c) : ctx <- return ctx
  a <- cons c v
  (c, v') <- down @c a (succ (key c))
  return (Zipper (Co c : ctx) v')

data NoCtx a

instance (Typeable a, Show a) => IsContext (NoCtx a) where
  type Full (NoCtx a) = a
  showRoot _ = show
  showHole = undefined
  key = undefined
  down _ _ = Nothing
  cons = undefined

newtype GenericCo x a = GenericCo (GCo (Rep a) ())

instance
  ( Typeable a, Generic a, Typeable x, HasGCo x (Rep a), ToConName (Rep a), ToConName (GCo (Rep a))
  ) => IsContext (GenericCo x a)
  where
  type Full (GenericCo x a) = a
  showRoot _ a = concat (toConName a' : replicate (gLength a') " _")
    where a' = from a
  showHole (GenericCo ctx) = toConName ctx ++ fields ++ " !" ++ fields'
    where
      (fields, fields') =
        splitAt (gKey (GCo_ @x @(Rep a) ctx))
          (concat (replicate (gLength ctx) " _"))
  key (GenericCo ctx) = gKey (GCo_ @x @(Rep a) ctx)
  down = (fmap . fmap) (\(GCo_ ctx, v) -> (GenericCo ctx, v)) . gDown @x . from
  cons (GenericCo ctx) = fmap to . gCons @x @(Rep a) (GCo_ ctx)

type family GCo (f :: * -> *) :: * -> * where
  GCo (M1 i c f) = M1 i c (GCo f)
  GCo (f :*: g)
    =   (GCo f :*: g)
    :+: (f :*: GCo g)
  GCo (f :+: g) = GCo f :+: GCo g
  GCo (K1 _ _) = U1

newtype GCo_ x f = GCo_ (GCo f ())

class GLength f where
  gLength :: f p -> Int

instance GLength f => GLength (M1 i c f) where
  gLength (M1 f) = gLength f

instance (GLength f, GLength g) => GLength (f :*: g) where
  gLength (f :*: g) = gLength f + gLength g

instance (GLength f, GLength g) => GLength (f :+: g) where
  gLength (L1 f) = gLength f
  gLength (R1 g) = gLength g

instance GLength (K1 i c) where
  gLength _ = 1

instance GLength U1 where
  gLength _ = 0

class (GLength f, GLength (GCo f)) => HasGCo x f where
  gKey :: GCo_ x f -> Int
  gDown :: f () -> Int -> Maybe (GCo_ x f, SomeFocus)
  gCons :: GCo_ x f -> SomeFocus -> Maybe (f ())

instance HasGCo x f => HasGCo x (M1 i c f) where
  gKey (GCo_ (M1 c)) = gKey @x @f (GCo_ c)
  gDown (M1 f) n = fmap (\(GCo_ c, v) -> (GCo_ (M1 c), v)) (gDown @x f n)
  gCons (GCo_ (M1 c)) v = fmap M1 (gCons @x @f (GCo_ c) v)

instance (HasGCo x f, HasGCo x g) => HasGCo x (f :*: g) where
  gKey (GCo_ (L1 (c1 :*: _))) = gKey @x @f (GCo_ c1)
  gKey (GCo_ (R1 (f :*: c2))) = gLength f + gKey @x @g (GCo_ c2)
  gDown (f :*: g) n =
    let m = n - gLength f
    in if m < 0 then do
      (GCo_ f', v) <- gDown @x f n
      return (GCo_ (L1 (f' :*: g)), v)
    else do
      (GCo_ g', v) <- gDown @x g m
      return (GCo_ (R1 (f :*: g')), v)
  gCons (GCo_ (L1 (f' :*: g))) v = do
    f <- gCons @x @f (GCo_ f') v
    return (f :*: g)
  gCons (GCo_ (R1 (f :*: g'))) v = do
    g <- gCons @x @g (GCo_ g') v
    return (f :*: g)

instance (HasGCo x f, HasGCo x g) => HasGCo x (f :+: g) where
  gKey (GCo_ (L1 cL)) = gKey @x @f (GCo_ cL)
  gKey (GCo_ (R1 cR)) = gKey @x @g (GCo_ cR)
  gDown (L1 f) = fmap (\(GCo_ c, v) -> (GCo_ (L1 c), v)) . gDown @x f
  gDown (R1 g) = fmap (\(GCo_ c, v) -> (GCo_ (R1 c), v)) . gDown @x g
  gCons (GCo_ (L1 cL)) = fmap L1 . gCons @x (GCo_ cL)
  gCons (GCo_ (R1 cR)) = fmap R1 . gCons @x (GCo_ cR)

type family CxOf x c

instance (IsContext (CxOf x c), Full (CxOf x c) ~ c) => HasGCo x (K1 i c) where
  gKey _ = 0
  gDown (K1 a) 0 = Just (GCo_ U1, SomeFocus (Proxy @(CxOf x c)) a)
  gDown _ _ = Nothing
  gCons _ (SomeFocus _ a) = K1 <$> cast a

class ToConName f where
  toConName :: f p -> String

instance ToConName f => ToConName (M1 D c f) where
  toConName (M1 c) = toConName c

instance (ToConName f, ToConName g) => ToConName (f :+: g) where
  toConName (L1 cL) = toConName cL
  toConName (R1 cR) = toConName cR

instance Constructor c => ToConName (M1 S c f) where
  toConName _ = conName (undefined :: t c f a)
