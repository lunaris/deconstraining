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
  ValueE  :: c => a -> Exp c a
  CondE   :: Exp c Bool -> Exp c a -> Exp c a -> Exp c a
  EqE     :: Eq a => Exp c a -> Exp c a -> Exp c Bool

evaluate :: Exp () a -> a
evaluate (ValueE x)
  = x

evaluate (CondE p t f)
  = if evaluate p then evaluate t else evaluate f

evaluate (EqE e1 e2)
  = evaluate e1 == evaluate e2

class Cuda a where
  toCuda :: a -> String

instance Cuda Char where
  toCuda x
    = "(Char " ++ show x ++ ")"

instance Cuda Int where
  toCuda x
    = "(Int " ++ show x ++ ")"

expToCuda :: Exp (Cuda a) a -> String
expToCuda (ValueE x)
  = toCuda x

-- expToCuda (CondE p t f)
--   = "(Cond " ++ expToCuda p ++ " " ++ expToCuda t ++ " " ++
--       expToCuda f ++ ")"
--
-- expToCuda (EqE e1 e2)
--   = "(Eq " ++ expToCuda e1 ++ " " ++ expToCuda e2 ++ ")"
