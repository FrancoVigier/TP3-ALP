module Common where

  -- Comandos interactivos o de archivos
  data Stmt i = Def String i           --  Declarar un nuevo identificador x, let x = t
              | Eval i                 --  Evaluar el término
    deriving (Show)
  
  instance Functor Stmt where
    fmap f (Def s i) = Def s (f i)
    fmap f (Eval i)  = Eval (f i)

  -- Tipos de los nombres
  data Name
     =  Global  String
    deriving (Show, Eq)

  -- Entornos
  type NameEnv v t = [(Name, (v, t))]

  -- Tipo de los tipos
  data Type = EmptyT
            | NatT
            | ListT Type
            | FunT Type Type
       deriving (Show, Eq)

  -- Términos con nombres
  data LamTerm  =  LVar String
                |  LAbs String Type LamTerm
                |  LLet String LamTerm LamTerm
                |  LApp LamTerm LamTerm
                |  LZero
                |  LSuc LamTerm
                |  LR LamTerm LamTerm LamTerm
                |  LNil
                |  LCons LamTerm LamTerm
                |  LRL LamTerm LamTerm LamTerm
       deriving (Show, Eq)


  -- Términos localmente sin nombres
  data Term  = Bound Int
             | Free Name 
             | Term :@: Term
             | Lam Type Term
             | Let Term Term
             | Zero
             | Suc Term
             | R Term Term Term
             | Nil
             | Cons Term Term
             | RL Term Term Term
       deriving (Show, Eq)

  -- Valores
  data Value = VLam Type Term
             | VNat VNat
             | VList VList
          deriving (Show, Eq)

  data VNat = VZero
            | VSuc VNat
          deriving (Show, Eq)

  data VList = VNil
             | VCons VNat VList
          deriving (Show, Eq)

  -- Contextos del tipado
  type Context = [Type]
