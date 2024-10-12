{-# LANGUAGE GADTs #-}
module CNF where
import LangProp(Identifier(..), LangProp(..))

{- newtype AtomC' = AtomC' Identifier

newtype Dis' = Dis' [AtomC']

newtype Con' = Con' [Dis']

data CNF = Con Con' | Dis Dis' | AtomC AtomC'

 -}
data LangCNF ty where 
    AtomCNF :: Identifier -> LangCNF Identifier
    DisCNF  :: LangCNF Identifier -> LangCNF Identifier -> LangCNF ty
    ConCNF  :: LangCNF ty -> LangCNF ty -> LangCNF ty 


{- transform :: LangProp -> LangCNF a
transform (Atom ident) = AtomCNF ident -}

-- data Expr = Num Int | Add Expr Expr | Str String 
data Expr a where
    NumE :: Int -> Expr Int 
    StringE :: String -> Expr String
    AddE :: Expr Int -> Expr Int -> Expr a 

i :: Expr Int
i = NumE 1

s :: Expr String 
s = StringE "s"

a :: Expr Int
a = AddE i (AddE i i)

-- b = AddE i s => type error
data Val a where
    IntV :: Int -> Val Int 
    StringV :: String -> Val String

eval :: Expr a -> a
eval (NumE i) = i
eval (StringE s)  = s
eval (AddE e1 e2) = eval' e1 + eval' e2 

eval' :: Expr Int -> Int 
eval' (NumE i) = i
