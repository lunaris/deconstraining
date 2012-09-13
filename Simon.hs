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

module Simon where

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
  CondE2  :: Exp2 c Bool -> Exp2 c a -> Exp2 c a -> Exp2 c a
  EqE2    :: Eq a => Exp2 c a -> Exp2 c a -> Exp2 c Bool

class Every a

instance Every a

evaluate2 :: Exp2 Every a -> a
evaluate2 (ValueE2 x)
  = x

evaluate2 (CondE2 p t f)
  = if evaluate2 p then evaluate2 t else evaluate2 f

evaluate2 (EqE2 e1 e2)
  = evaluate2 e1 == evaluate2 e2

exp2ToCuda :: Exp2 Cuda a -> String
exp2ToCuda (ValueE2 x)
  = toCuda x

exp2ToCuda (CondE2 p t f)
  = "(Cond " ++ exp2ToCuda p ++ " " ++ exp2ToCuda t ++ " " ++
      exp2ToCuda f ++ ")"

exp2ToCuda (EqE2 e1 e2)
  = "(Eq " ++ exp2ToCuda e1 ++ " " ++ exp2ToCuda e2 ++ ")"

data Dict c a where
  Dict :: c a => Dict c a

pickLeft :: Exp2 (Both c d) a -> Exp2 c a
pickLeft (ValueE2 x)
  = ValueE2 x

pickLeft (CondE2 p t f)
  = CondE2 (pickLeft p) (pickLeft t) (pickLeft f)

pickLeft (EqE2 e1 e2)
  = EqE2 (pickLeft e1) (pickLeft e2)

pickRight :: Exp2 (Both c d) a -> Exp2 d a
pickRight (ValueE2 x)
  = ValueE2 x

pickRight (CondE2 p t f)
  = CondE2 (pickRight p) (pickRight t) (pickRight f)

pickRight (EqE2 e1 e2)
  = EqE2 (pickRight e1) (pickRight e2)

f e
  = (evaluate2 (pickLeft e), exp2ToCuda (pickRight e))
