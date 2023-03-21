module Simplytyped
  ( conversion
  ,    -- conversion a terminos localmente sin nombre
    eval
  ,          -- evaluador
    infer
  ,         -- inferidor de tipos
    quote          -- valores -> terminos
  )
where

import           Data.List
import           Data.Maybe
import           Prelude                 hiding ( (>>=) )
import           Text.PrettyPrint.HughesPJ      ( render )
import           PrettyPrinter
import           Common

-- conversion a términos localmente sin nombres
conversion :: LamTerm -> Term
conversion = conversion' []

conversion' :: [String] -> LamTerm -> Term
conversion' b (LVar n    )   = maybe (Free (Global n)) Bound (n `elemIndex` b)
conversion' b (LApp t u  )   = conversion' b t :@: conversion' b u
conversion' b (LAbs n t u)   = Lam t (conversion' (n : b) u)
conversion' b (LLet n t1 t2) = Let (conversion' b t1) (conversion' (n : b) t2)
conversion' b (LZero)        = Zero
conversion' b (LSuc t)       = Suc (conversion' b t)
conversion' b (LR t1 t2 t3)  = R (conversion' b t1) (conversion' b t2) (conversion' b t3)
conversion' b (LNil)         = Nil
conversion' b (LCons t1 t2)  = Cons (conversion' b t1) (conversion' b t2)
conversion' b (LRL t1 t2 t3) = RL (conversion' b t1) (conversion' b t2) (conversion' b t3)


-----------------------
--- eval
-----------------------

sub :: Int -> Term -> Term -> Term
sub i t (Bound j) | i == j    = t
sub _ _ (Bound j) | otherwise = Bound j
sub _ _ (Free n   )           = Free n
sub i t (u   :@: v)           = sub i t u :@: sub i t v
sub i t (Lam t'  u)           = Lam t' (sub (i + 1) t u)
sub i t (Let t1  t2)          = Let (sub (i) t t1) (sub (i+1) t t2)
sub i t (Zero)                = Zero
sub i t (Suc t1)              = Suc (sub i t t1)
sub i t (R t1 t2 t3)          = R (sub i t t1) (sub i t t2) (sub i t t3)
sub i t (Nil)                 = Nil
sub i t (Cons t1 t2)          = Cons (sub i t t1) (sub i t t2)
sub i t (RL t1 t2 t3)         = RL (sub i t t1) (sub i t t2) (sub i t t3)

-- evaluador de términos
eval :: NameEnv Value Type -> Term -> Value
eval _ (Bound _             ) = error "variable ligada inesperada en eval"
eval e (Free  n             ) = fst $ fromJust $ lookup n e
eval _ (Lam      t   u      ) = VLam t u
eval e (Lam _ u  :@: Lam s v) = eval e (sub 0 (Lam s v) u)
eval e (Lam t u1 :@: u2)      = let v2 = eval e u2 in eval e (sub 0 (quote v2) u1)
eval e (u        :@: v      ) = case eval e u of
  VLam t u' -> eval e (Lam t u' :@: v)
  _         -> error "Error de tipo en run-time, verificar type checker"
eval e (Let t1 t2)            = eval e (sub 0 t1 t2)
eval e (Zero)                 = VNat VZero
eval e (Suc t)                = case eval e t of
   VNat n -> VNat (VSuc n)
   _      -> error "Error de tipo en run-time, verificar type checker"
eval e (R t1 t2 t3)         = case eval e t3 of
  VNat VZero      -> eval e t1
  VNat (VSuc x)   -> eval e ((t2 :@: (R t1 t2 (quote (VNat x)))) :@: (quote (VNat x)))
  _               -> error "Error de tipo en run-time, verificar type checker"
eval e (Nil)                  = VList VNil
eval e (Cons t1 t2)           = case (eval e t1, eval e t2) of
  (VNat n, VList l) -> VList (VCons n l)
  _                 -> error "Error de tipo en run-time, verificar type checker"
eval e (RL t1 t2 t3)         = case eval e t3 of
  VList VNil         -> eval e t1
  VList (VCons n l)  -> eval e (((t2 :@: quote (VNat n)) :@: (quote (VList l))) :@: (RL t1 t2 (quote (VList l))))
  _                  -> error "Error de tipo en run-time, verificar type checker"

-----------------------
--- quoting
-----------------------

quote :: Value -> Term
quote (VLam t f)          = Lam t f
quote (VNat VZero)        = Zero
quote (VNat (VSuc x))     = Suc (quote (VNat x))
quote (VList VNil)        = Nil
quote (VList (VCons n l)) = Cons (quote (VNat n)) (quote (VList l))

----------------------
--- type checker
-----------------------

-- type checker
infer :: NameEnv Value Type -> Term -> Either String Type
infer = infer' []

-- definiciones auxiliares
ret :: Type -> Either String Type
ret = Right

err :: String -> Either String Type
err = Left

(>>=)
  :: Either String Type -> (Type -> Either String Type) -> Either String Type
(>>=) v f = either Left f v
-- fcs. de error

matchError :: Type -> Type -> Either String Type
matchError t1 t2 =
  err
    $  "se esperaba "
    ++ render (printType t1)
    ++ ", pero "
    ++ render (printType t2)
    ++ " fue inferido."

notfunError :: Type -> Either String Type
notfunError t1 = err $ render (printType t1) ++ " no puede ser aplicado."

notfoundError :: Name -> Either String Type
notfoundError n = err $ show n ++ " no está definida."

infer' :: Context -> NameEnv Value Type -> Term -> Either String Type
infer' c _ (Bound i) = ret (c !! i)
infer' _ e (Free  n) = case lookup n e of
  Nothing     -> notfoundError n
  Just (_, t) -> ret t
infer' c e (t :@: u) = infer' c e t >>= \tt -> infer' c e u >>= \tu ->
  case tt of
    FunT t1 t2 -> if (tu == t1) then ret t2 else matchError t1 tu
    _          -> notfunError tt
infer' c e (Lam t u) = infer' (t : c) e u >>= \tu -> ret $ FunT t tu
infer' c e (Let t1 t2) = infer' c e t1 >>= \typet1 -> infer' (typet1 : c) e t2
infer' c e (Zero) = Right NatT
infer' c e (Suc t) = infer' c e t >>= \typet -> case typet of
  NatT -> Right NatT
  x    -> matchError NatT x
infer' c e (R t1 t2 t3) = infer' c e t1 >>= \typet1 -> infer' c e t2 >>= \typet2 -> infer' c e t3 >>= \typet3 ->
  case typet2 of
    FunT x (FunT NatT y) -> if x == typet1 && y == typet1
                            then case typet3 of
                                    NatT -> Right typet1
                                    z    -> matchError NatT z
                            else if x /= typet1
                                 then matchError typet1 x
                                 else matchError typet1 y
    x                    -> matchError (FunT typet1 (FunT NatT typet1)) x
infer' c e (Nil) = Right (ListT NatT)
infer' c e (Cons n l) = infer' c e n >>= \typen -> infer' c e l >>= \typel ->
  case typen of
    NatT -> case typel of
      ListT NatT -> Right (ListT NatT)
      x          -> matchError (ListT NatT) x
    x    -> matchError NatT x
infer' c e (RL t1 t2 t3) = infer' c e t1 >>= \typet1 -> infer' c e t2 >>= \typet2 -> infer' c e t3 >>= \typet3 ->
  case typet2 of
    FunT NatT (FunT (ListT NatT) (FunT x y)) -> if x == typet1 && y == typet1
                                                then case typet3 of
                                                        ListT NatT -> Right typet1
                                                        z          -> matchError (ListT NatT) z
                                                else if x /= typet1
                                                     then matchError typet1 x
                                                     else matchError typet1 y
    x                                        -> matchError (FunT NatT (FunT (ListT NatT) (FunT typet1 typet1))) x


----------------------------------