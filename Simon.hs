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

data Exp c a where
  ValueE  :: c a => a -> Exp (c a) a
  CondE   :: Exp c Bool -> Exp d a -> Exp e a -> Exp (c,d,e) a
  EqE     :: Eq a => Exp c a -> Exp c a -> Exp c Bool

class Any a
instance Any a

evaluate :: Exp c a -> a
evaluate (ValueE x)
  = x

evaluate (CondE p t f)
  = if evaluate p then evaluate t else evaluate f

evaluate (EqE e1 e2)
  = evaluate e1 == evaluate e2

class Cuda a where
  toCuda :: a -> String

instance Cuda Int where
  toCuda x
    = "(Int " ++ show x ++ ")"

expToCuda (ValueE x)
  = toCuda x

expToCuda (CondE b l r)
  = expToCuda b ++ expToCuda l ++ expToCuda r

{-
data Trap c a where
  Trap :: c a => Trap c a

urk :: (Cuda a ~ c a) => (Trap c a -> b) -> (Trap Cuda a -> b)
urk f Trap
  = f Trap

-}
{-
f :: Exp (Any a, Cuda a) a -> (a, String)
f e
  = (evaluate e, expToCuda e)
-}
