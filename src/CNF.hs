{-# LANGUAGE GADTs #-}

module CNF (transform) where

import Debug.Trace (trace)
import Control.Exception.Base (assert)
import LangProp (Identifier (..), LangProp (..))

-- no gadt
data LangPropCNF
  = AtomCNF Identifier
  | NotCNF LangPropCNF -- atom
  | DisCNF [LangPropCNF] -- atom or not
  | ConCNF [LangPropCNF] -- dis
  deriving (Show)

atomic :: LangProp -> Bool 
atomic (Atom _) = True
atomic (Not (Atom _)) = True
atomic _ = False 

-- atomics :: LangProp -> [LangProp]
-- atomics a@(Atom _) = [a]
-- atomics n@(Not _) = assert (atomic n) [n]
-- atomics o@(Or p1 p2) = atomics p1 ++ atomics p2 
-- atomics a@(And p1 p2) = atomics p2 ++ atomics p2 
-- atomics _ = error "atomics"

-- disCnfs :: LangProp -> [LangPropCNF]
-- disCnfs a@(Atom _) = [transform a]
-- disCnfs n@(Not (Atom _)) = [transform n]
-- disCnfs (Or p1 p2) = disCnfs p1 ++ disCnfs p2
-- disCnfs _ = error "disCnfs"

-- conCnsf :: LangProp -> [LangPropCNF]
-- conCnsf a@(Atom _) = [transform a]
-- conCnsf n@(Not (Atom _)) = [transform n]
-- conCnsf o@(Or _ _) = [DisCNF (disCnfs o)]  
-- conCnsf (And p1 p2) = conCnsf p1 ++ conCnsf p2
-- conCnsf _ = error "conCnsf"

transform :: LangProp -> LangPropCNF 
transform p = case  transform' p of 
  (Atom ident) -> AtomCNF ident
  n@(Not (Atom ident)) -> assert (atomic n) (NotCNF (AtomCNF ident))
  (Or p1 p2) -> DisCNF (disCnfs p1 ++ disCnfs p2)
  (And p1 p2) -> case  (transform p1, transform p2) of 
            (ConCNF subs1, ConCNF subs2) -> ConCNF [DisCNF $ subs1 ++ subs2]
            _ -> error "transform"
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
