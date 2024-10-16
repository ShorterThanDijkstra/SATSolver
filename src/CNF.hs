{-# LANGUAGE GADTs #-}

module CNF (transform) where

import Debug.Trace (trace)
import LangPropCore (Identifier (..), LangPropCore (..))

data LangPropCNF
  = AtomCNF Identifier
  | NotCNF LangPropCNF -- atom
  | DisCNF [LangPropCNF] -- atom or not
  | ConCNF [LangPropCNF] -- dis
  deriving (Show)

-- TODO: to LangPropCNF
transform :: LangPropCore -> LangPropCore
-- p => p
transform p@(Atom _) = p
-- !p => !p
transform p@(Not (Atom _)) = p
-- !!P => P
transform (Not (Not p)) = transform p
-- !(P1 & P2) => !P1 | !P2
transform (Not (And p1 p2)) = transform (Or (Not p1) (Not p2))
-- !(P1 | P2) => !P1 & !P2
transform (Not (Or p1 p2)) = transform (And (Not p1) (Not p2))
-- P1 & P2 => P1 & P2
transform (And p1 p2) = And (transform p1) (transform p2)
-- P1 | (P2 & P3) => (P1 | P2) & (p1 | P3)
transform (Or p1 (And p2 p3)) = And (transform (Or (transform p1) (transform p2))) (transform (Or (transform p1) (transform p3)))
-- (P1 & P2) | P3 => (P1 | P3) & (P2 | P3)
transform (Or (And p1 p2) p3) = And (transform (Or (transform p1) (transform p3))) (transform (Or (transform p2) (transform p3)))
transform p@(Or p1 p2) =
  let p1' = transform p1
      p2' = transform p2
   in if p1' == p1 && p2' == p2 then p else transform (Or p1' p2')


-- https://en.wikipedia.org/wiki/Tseytin_transformation
-- https://uds-psl.github.io/ba-gaeher/website/toc.html
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
