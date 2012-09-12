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

module Deconstraining where

data Evidence a as where
  Head :: Evidence a (a ': as)
  Tail :: (Elem a as) => Evidence a (b ': as)

class Elem a as where
  evidence :: Evidence a as

instance Elem a (a ': as) where
  evidence = Head

instance Elem a as => Elem a (b ': as) where
  evidence = Tail

class IntBool a where
  toInt :: a -> Int

instance IntBool Int  where toInt = id
instance IntBool Bool where toInt = fromEnum

data Proxy a
  = Proxy
  deriving (Eq, Show)

class AllIntBool as where
  toInt' :: (Elem b as) => Proxy as -> b -> Int

instance AllIntBool '[] where
  toInt' Proxy (x :: b)
    = seq (evidence :: Evidence b '[]) undefined

instance (IntBool a, AllIntBool as) => AllIntBool (a ': as) where
  toInt' Proxy (x :: b)
    = case evidence :: Evidence b (a ': as) of
        Head -> toInt x
        Tail -> toInt' (Proxy :: Proxy as) x

data Trap c a where
  Trap :: c a => Trap c a

class All c as where
  withElem :: (Elem b as) => Proxy as -> (Trap c b -> d) -> d

instance All c '[] where
  withElem Proxy (f :: Trap c b -> d)
    = seq (evidence :: Evidence b '[]) undefined

instance (c a, All c as) => All c (a ': as) where
  withElem Proxy (f :: Trap c b -> d)
    = case evidence :: Evidence b (a ': as) of
        Head -> f Trap
        Tail -> withElem (Proxy :: Proxy as) f

data Exp1 as a where
  ValueE1 :: Elem a as => a -> Exp1 as a

  CondE1  :: (Elem a as, Elem Bool as)
          => Exp1 as Bool -> Exp1 as a -> Exp1 as a -> Exp1 as a

  EqE1    :: (Eq a, Elem a as, Elem Bool as)
          => Exp1 as a -> Exp1 as a -> Exp1 as Bool

evaluateExp1 :: Exp1 as a -> a
evaluateExp1 (ValueE1 x)
  = x

evaluateExp1 (CondE1 p t f)
  = if evaluateExp1 p then evaluateExp1 t else evaluateExp1 f

evaluateExp1 (EqE1 e1 e2)
  = evaluateExp1 e1 == evaluateExp1 e2

class CudaType a where
  toCuda :: a -> String

instance CudaType Int     where toCuda x = "(Int " ++ show x ++ ")"
instance CudaType Float   where toCuda x = "(Float " ++ show x ++ ")"
instance CudaType Double  where toCuda x = "(Double " ++ show x ++ ")"

compileExp1ToCuda :: forall a as. All CudaType as => Exp1 as a -> String
compileExp1ToCuda (ValueE1 x)
  = withElem (Proxy :: Proxy as) (f x)
    where
      f :: b -> Trap CudaType b -> String
      f x Trap
        = toCuda x

compileExp1ToCuda (CondE1 p t f)
  = "(CondE1 " ++ compileExp1ToCuda p ++ " " ++
      compileExp1ToCuda t ++ " " ++ compileExp1ToCuda f ++ ")"

compileExp1ToCuda (EqE1 e1 e2)
  = "(EqE1 " ++ compileExp1ToCuda e1 ++ " " ++
      compileExp1ToCuda e2 ++ ")"

data Op
  = ValueO
  | CondO
  | EqO
  deriving (Eq, Show)

class CudaOp o

instance CudaOp ValueO
instance CudaOp CondO
instance CudaOp EqO

data Exp2 as os a where
  ValueE2 :: (Elem a as, Elem ValueO os) => a -> Exp2 as os a

  CondE2  :: (Elem a as, Elem Bool as, Elem CondO os)
          => Exp2 as os Bool -> Exp2 as os a -> Exp2 as os a -> Exp2 as os a

  EqE2    :: (Eq a, Elem a as, Elem Bool as, Elem EqO os)
          => Exp2 as os a -> Exp2 as os a -> Exp2 as os Bool

compileExp2ToCuda :: forall a as os. (All CudaType as, All CudaOp os)
                  => Exp2 as os a -> String

compileExp2ToCuda (ValueE2 x)
  = withElem (Proxy :: Proxy as) (f x)
    where
      f :: b -> Trap CudaType b -> String
      f x Trap
        = toCuda x

compileExp2ToCuda (EqE2 e1 e2)
  = "(EqE1 " ++ compileExp2ToCuda e1 ++ " " ++
      compileExp2ToCuda e2 ++ ")"
