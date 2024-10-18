{-# LANGUAGE GADTs #-}
{-# LANGUAGE InstanceSigs #-}

module CNF (transform) where

import Debug.Trace (trace)
import Control.Exception.Base (assert)
import LangProp (Identifier (..), LangProp (..))
import Data.List (intercalate, sort)
import qualified Data.Set as Set
-- no gadt
data LangPropCNF
  = AtomCNF Identifier
  | NotCNF LangPropCNF -- atom
  | DisCNF [LangPropCNF] -- atom or not
  | ConCNF [LangPropCNF] -- dis

instance Ord LangPropCNF where 
  (<=) :: LangPropCNF -> LangPropCNF -> Bool
  (AtomCNF ident1) <= (AtomCNF ident2) = ident1 <= ident2
  (NotCNF cnf1) <= (NotCNF cnf2) = cnf1 <= cnf2
  (DisCNF cnfs1) <= (DisCNF cnfs2) = sort 

instance Eq LangPropCNF where 
  (==) :: LangPropCNF -> LangPropCNF -> Bool
  (AtomCNF ident1) == (AtomCNF ident2) = ident1 == ident2
  (NotCNF cnf1) == (NotCNF cnf2) = cnf1 == cnf2
  (DisCNF cnfs1) == (DisCNF cnfs2) = (Set.fromList cnfs1) == (Set.fromList cnfs2)
  (ConCNF cnfs1) == (ConCNF cnfs2) = (Set.fromList cnfs1) == (Set.fromList cnfs2)
  _ == _ = False
instance Show LangPropCNF where
  show :: LangPropCNF -> String
  show (AtomCNF ident) = show ident
  show (NotCNF cnf) = "!" ++ show cnf
  show (DisCNF cnfs) = "(" ++ intercalate " | " (map show cnfs) ++ ")"
  show (ConCNF cnfs) = "(" ++ intercalate " & " (map show cnfs) ++ ")"

atomic :: LangProp -> Bool
atomic (Atom _) = True
atomic (Not (Atom _)) = True
atomic _ = False

disCnfs :: LangProp -> [LangPropCNF]
disCnfs (Atom ident) = [AtomCNF ident]
disCnfs (Not (Atom ident)) = [NotCNF (AtomCNF ident)]
disCnfs (Or p1 p2) = disCnfs p1 ++ disCnfs p2
disCnfs _ = error "disCnfs"

conCnfs :: LangProp -> [LangPropCNF]
conCnfs  (Atom ident) = [AtomCNF ident]
conCnfs (Not (Atom ident)) = [NotCNF (AtomCNF ident)]
conCnfs o@(Or _ _) = [DisCNF (disCnfs o)]
conCnfs (And p1 p2) = conCnfs p1 ++ conCnfs p2
conCnfs _ = error "conCnsf"

transform :: LangProp -> LangPropCNF
transform p = case  transform' p of
  (Atom ident) -> AtomCNF ident
  n@(Not (Atom ident)) -> assert (atomic n) (NotCNF (AtomCNF ident))
  o@(Or _ _) -> DisCNF $ disCnfs o
  a@(And _ _) -> ConCNF $ conCnfs a
  _ -> error "transform"

-- TODO: to LangPropCNF
transform' :: LangProp -> LangProp
-- p => p
transform' p@(Atom _) = p
-- !p => !p
transform' p@(Not (Atom _)) = p
-- !!P => P
transform' (Not (Not p)) = transform' p
-- !(P1 & P2) => !P1 | !P2
transform' (Not (And p1 p2)) = transform' (Or (Not p1) (Not p2))
-- !(P1 | P2) => !P1 & !P2
transform' (Not (Or p1 p2)) = transform' (And (Not p1) (Not p2))
transform' (Not (If p1 p2)) = transform' (And p1 (Not p2))
transform' (Not (Iff p1 p2)) = transform' (Or (Not (If p1 p2)) (Not (If p2 p1)))
-- P1 & P2 => P1 & P2
transform' (And p1 p2) = And (transform' p1) (transform' p2)
transform' (If p1 p2) = transform' (Or (Not p1) p2)
transform' (Iff p1 p2) = transform' (And (If p1 p2) (If p2 p1))
-- P1 | (P2 & P3) => (P1 | P2) & (p1 | P3)
transform' (Or p1 (And p2 p3)) = And (transform' (Or (transform' p1) (transform' p2))) (transform' (Or (transform' p1) (transform' p3)))
-- (P1 & P2) | P3 => (P1 | P3) & (P2 | P3)
transform' (Or (And p1 p2) p3) = And (transform' (Or (transform' p1) (transform' p3))) (transform' (Or (transform' p2) (transform' p3)))
transform' p@(Or p1 p2) =
  let p1' = transform' p1
      p2' = transform' p2
   in if p1' == p1 && p2' == p2 then p else transform' (Or p1' p2')


-- data Expr a where
--     NumE :: Int -> Expr Int
--     StringE :: String -> Expr String
--     AddE :: Expr Int -> Expr Int -> Expr Int

-- i :: Expr Int
-- i = NumE 1

-- s :: Expr String
-- s = StringE "s"

-- a :: Expr Int
-- a = AddE i (AddE i i)

-- -- b = AddE i s => type error
-- data Val a where
--     IntV :: Int -> Val Int
--     StringV :: String -> Val String

-- eval :: Expr a -> a
-- eval (NumE i) = i
-- eval (StringE s)  = s
-- eval (AddE e1 e2) = eval e1 + eval e2
