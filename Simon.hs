{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverlappingInstances #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

{-# OPTIONS_GHC -fno-warn-missing-methods #-}

module Simon where

import GHC.Prim

data Exp1 c a where
  ValueE1 :: c => a -> Exp1 c a
  CondE1  :: Exp1 c Bool -> Exp1 c a -> Exp1 c a -> Exp1 c a
  EqE1    :: Eq a => Exp1 c a -> Exp1 c a -> Exp1 c Bool

evaluate1 :: Exp1 () a -> a
evaluate1 (ValueE1 x)
  = x

evaluate1 (CondE1 p t f)
  = if evaluate1 p then evaluate1 t else evaluate1 f

evaluate1 (EqE1 e1 e2)
  = evaluate1 e1 == evaluate1 e2

class Cuda a where
  toCuda :: a -> String

instance Cuda Bool where
  toCuda x
    = "(Bool " ++ show x ++ ")"

instance Cuda Char where
  toCuda x
    = "(Char " ++ show x ++ ")"

instance Cuda Int where
  toCuda x
    = "(Int " ++ show x ++ ")"

exp1ToCuda :: Exp1 (Cuda a) a -> String
exp1ToCuda (ValueE1 x)
  = toCuda x

-- exp1ToCuda (CondE1 p t f)
--   = "(Cond " ++ exp1ToCuda p ++ " " ++ exp1ToCuda t ++ " " ++
--       exp1ToCuda f ++ ")"
--
-- exp1ToCuda (EqE1 e1 e2)
--   = "(Eq " ++ exp1ToCuda e1 ++ " " ++ exp1ToCuda e2 ++ ")"

class (c a, d a) => Both c d a where

instance (c a, d a) => Both c d a

data Exp2 c a where
  ValueE2 :: c a => a -> Exp2 c a

  CondE2  :: (c a, c Bool)
          => Exp2 c Bool -> Exp2 c a -> Exp2 c a -> Exp2 c a

  EqE2    :: (Eq a, c a, c Bool)
          => Exp2 c a -> Exp2 c a -> Exp2 c Bool

  AddE2   :: (Num a, c a)
          => Exp2 c a -> Exp2 c a -> Exp2 c a

instance (c a, Num a) => Num (Exp2 c a) where
  (+)
    = AddE2

  fromInteger
    = ValueE2 . fromInteger

class Every a

instance Every a

evaluate2 :: Exp2 Every a -> a
evaluate2 (ValueE2 x)
  = x

evaluate2 (CondE2 p t f)
  = if evaluate2 p then evaluate2 t else evaluate2 f

evaluate2 (EqE2 e1 e2)
  = evaluate2 e1 == evaluate2 e2

evaluate2 (AddE2 e1 e2)
  = evaluate2 e1 + evaluate2 e2

exp2ToCuda :: Exp2 Cuda a -> String
exp2ToCuda (ValueE2 x)
  = toCuda x

exp2ToCuda (CondE2 p t f)
  = "(Cond " ++ exp2ToCuda p ++ " " ++ exp2ToCuda t ++ " " ++
      exp2ToCuda f ++ ")"

exp2ToCuda (EqE2 e1 e2)
  = "(Eq " ++ exp2ToCuda e1 ++ " " ++ exp2ToCuda e2 ++ ")"

exp2ToCuda (AddE2 e1 e2)
  = "(Add " ++ exp2ToCuda e1 ++ " " ++ exp2ToCuda e2 ++ ")"

pickLeft :: Exp2 (Both c d) a -> Exp2 c a
pickLeft (ValueE2 x)
  = ValueE2 x

pickLeft (CondE2 p t f)
  = CondE2 (pickLeft p) (pickLeft t) (pickLeft f)

pickLeft (EqE2 e1 e2)
  = EqE2 (pickLeft e1) (pickLeft e2)

pickLeft (AddE2 e1 e2)
  = AddE2 (pickLeft e1) (pickLeft e2)

pickRight :: Exp2 (Both c d) a -> Exp2 d a
pickRight (ValueE2 x)
  = ValueE2 x

pickRight (CondE2 p t f)
  = CondE2 (pickRight p) (pickRight t) (pickRight f)

pickRight (EqE2 e1 e2)
  = EqE2 (pickRight e1) (pickRight e2)

pickRight (AddE2 e1 e2)
  = AddE2 (pickRight e1) (pickRight e2)

f2 e
  = (evaluate2 (pickLeft e), exp2ToCuda (pickRight e))

data Dict c a where
  Dict :: c a => Dict c a

data Proxy (cs :: [* -> Constraint])
  = Proxy
  deriving (Eq, Show)

class ApplyAll cs a
  =>  AllOf (cs :: [* -> Constraint]) (a :: *) where

  type ApplyAll cs a  :: Constraint

  pickOne             :: Elem c cs
                      => Proxy cs -> (Dict c a -> b) -> b

instance AllOf '[] a where
  type ApplyAll '[] a
    = ()

  pickOne Proxy (f :: Dict c a -> b)
    = seq (evidence :: Evidence c '[]) (error "Impossible")

instance (c a, AllOf cs a) => AllOf (c ': cs) a where
  type ApplyAll (c ': cs) a
    = (c a, ApplyAll cs a)

  pickOne Proxy (f :: Dict d a -> b)
    = case evidence :: Evidence d (c ': cs) of
        Head -> f Dict
        Tail -> pickOne (Proxy :: Proxy cs) f

data Exp3 cs a where
  ValueE3 :: AllOf cs a => a -> Exp3 cs a

  CondE3  :: (AllOf cs a, AllOf cs Bool)
          => Exp3 cs Bool -> Exp3 cs a -> Exp3 cs a -> Exp3 cs a

  EqE3    :: (Eq a, AllOf cs a, AllOf cs Bool)
          => Exp3 cs a -> Exp3 cs a -> Exp3 cs Bool

  AddE3   :: (Num a, AllOf cs a)
          => Exp3 cs a -> Exp3 cs a -> Exp3 cs a

instance (Num a, AllOf cs a) => Num (Exp3 cs a) where
  (+)
    = AddE3

  fromInteger
    = ValueE3 . fromInteger

evaluate3 :: Exp3 cs a -> a
evaluate3 (ValueE3 x)
  = x

evaluate3 (CondE3 p t f)
  = if evaluate3 p then evaluate3 t else evaluate3 f

evaluate3 (EqE3 e1 e2)
  = evaluate3 e1 == evaluate3 e2

evaluate3 (AddE3 e1 e2)
  = evaluate3 e1 + evaluate3 e2

class Elem x xs where
  evidence :: Evidence x xs

instance Elem x (x ': xs) where
  evidence
    = Head

instance Elem x xs => Elem x (y ': xs) where
  evidence
    = Tail

data Evidence x xs where
  Head :: Evidence x (x ': xs)
  Tail :: (Elem x xs) => Evidence x (y ': xs)

exp3ToCuda :: forall a cs. Elem Cuda cs => Exp3 cs a -> String
exp3ToCuda (ValueE3 x)
  = pickOne (Proxy :: Proxy cs) (f x)
    where
      f :: a -> Dict Cuda a -> String
      f x Dict
        = toCuda x

exp3ToCuda (CondE3 p t f)
  = "(Cond " ++ exp3ToCuda p ++ " " ++ exp3ToCuda t ++ " " ++
      exp3ToCuda f ++ ")"

exp3ToCuda (EqE3 e1 e2)
  = "(Eq " ++ exp3ToCuda e1 ++ " " ++ exp3ToCuda e2 ++ ")"

exp3ToCuda (AddE3 e1 e2)
  = "(Add " ++ exp3ToCuda e1 ++ " " ++ exp3ToCuda e2 ++ ")"

f3 e
  = (evaluate3 e, exp3ToCuda e)
