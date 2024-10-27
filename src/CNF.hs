{-# LANGUAGE GADTs #-}
{-# LANGUAGE InstanceSigs #-}

module CNF (transform, size, atomics, tseytins, LangPropCNF (..)) where

import Control.Exception.Base (assert)
import Data.List (intercalate, nub, sort)
import Debug.Trace (trace)
import LangProp (Identifier (..), LangProp (..), atomic)

-- no gadt
data LangPropCNF
  = AtomCNF Identifier
  | NotCNF LangPropCNF -- atom
  | DisCNF [LangPropCNF] -- atom or not
  | ConCNF [LangPropCNF] -- dis
  deriving (Show)

instance Ord LangPropCNF where
  (<=) :: LangPropCNF -> LangPropCNF -> Bool
  (AtomCNF ident1) <= (AtomCNF ident2) = ident1 <= ident2
  (NotCNF cnf1) <= (NotCNF cnf2) = cnf1 <= cnf2
  (DisCNF cnfs1) <= (DisCNF cnfs2) = sort cnfs1 <= sort cnfs2
  (ConCNF cnfs1) <= (ConCNF cnfs2) = sort cnfs1 <= sort cnfs2
  _ <= _ = False

instance Eq LangPropCNF where
  (==) :: LangPropCNF -> LangPropCNF -> Bool
  (AtomCNF ident1) == (AtomCNF ident2) = ident1 == ident2
  (NotCNF cnf1) == (NotCNF cnf2) = cnf1 == cnf2
  (DisCNF cnfs1) == (DisCNF cnfs2) = sort cnfs1 == sort cnfs2
  (ConCNF cnfs1) == (ConCNF cnfs2) = sort cnfs1 == sort cnfs2
  _ == _ = False

-- instance Show LangPropCNF where
--   show :: LangPropCNF -> String
--   show (AtomCNF ident) = show ident
--   show (NotCNF cnf) = "!" ++ show cnf
--   show (DisCNF [cnf]) = show cnf
--   show (DisCNF cnfs) = "(" ++ intercalate " | " (map show cnfs) ++ ")"
--   show (ConCNF [cnf]) = show cnf
--   show (ConCNF cnfs) = "(" ++ intercalate " & " (map show cnfs) ++ ")"

size :: LangPropCNF -> Int
size (AtomCNF _) = 1
size (NotCNF _) = 1
size (DisCNF cnfs) = sum $ map size cnfs
size (ConCNF cnfs) = sum $ map size cnfs

atomics :: LangPropCNF -> [LangPropCNF]
atomics a@(AtomCNF _) = [a]
atomics n@(NotCNF _) = [n]
atomics (DisCNF cnfs) = concatMap atomics cnfs
atomics (ConCNF cnfs) = concatMap atomics cnfs

disCnfs :: LangProp -> [LangPropCNF]
disCnfs (Atom ident) = [AtomCNF ident]
disCnfs (Not (Atom ident)) = [NotCNF (AtomCNF ident)]
disCnfs (Or p1 p2) = nub $ disCnfs p1 ++ disCnfs p2
disCnfs _ = error "disCnfs"

conCnfs :: LangProp -> [LangPropCNF]
conCnfs (Atom ident) = [AtomCNF ident]
conCnfs (Not (Atom ident)) = [NotCNF (AtomCNF ident)]
conCnfs o@(Or _ _) = [DisCNF (disCnfs o)]
conCnfs (And p1 p2) = nub $ conCnfs p1 ++ conCnfs p2
conCnfs _ = error "conCnsf"

-- transform :: LangProp -> LangPropCNF
-- transform p = case  transform' (tseytins p) of
--   (Atom ident) -> AtomCNF ident
--   n@(Not (Atom ident)) -> assert (atomic n) (NotCNF (AtomCNF ident))
--   o@(Or _ _) -> DisCNF $ disCnfs o
--   a@(And _ _) -> ConCNF $ conCnfs a
--   _ -> error "transform"

transform :: LangProp -> LangPropCNF
transform p = case transform' p of
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

newAtom :: Int -> LangProp
newAtom i = Atom (Identifier $ "$" ++ show i)

-- (r -> p) -> (!(q & r) -> p)

-- x1 <-> x2 -> x3
-- x2 <-> r -> p
-- x3 <-> x4 -> p
-- x4 <-> !x5
-- x5 <-> q & r

tseytins' :: LangProp -> Int -> (Int, [LangProp])
tseytins' a@(Atom _) i = (i, [Iff (newAtom i) a])
tseytins' n@(Not p) i =
  if atomic p
    then (i, [Iff (newAtom i) n])
    else
      let x1 = newAtom i
       in let x2 = newAtom $ i + 1
           in let (i1, rest) = tseytins' p $ i + 1
               in (i1, Iff x1 (Not x2) : rest)
tseytins' (If p1 p2) i = tseytinsCompound If p1 p2 i
tseytins' (And p1 p2) i = tseytinsCompound And p1 p2 i
tseytins' (Or p1 p2) i = tseytinsCompound Or p1 p2 i
tseytins' (Iff p1 p2) i = tseytinsCompound Iff p1 p2 i

tseytinsCompound :: (LangProp -> LangProp -> LangProp) -> LangProp -> LangProp -> Int -> (Int, [LangProp])
tseytinsCompound cons p1 p2 i = case (atomic p1, atomic p2) of
  (True, True) -> (i, [Iff (newAtom i) (cons p1 p2)])
  (False, True) ->
    let x1 = newAtom i
        x2 = newAtom (i + 1)
        (i1, rest) = tseytins' p1 (i + 1)
     in (i1, Iff x1 (cons x2 p2) : rest)
  (True, False) ->
    let x1 = newAtom i
        x2 = newAtom (i + 1)
        (i1, rest) = tseytins' p2 (i + 1)
     in (i1, Iff x1 (cons p1 x2) : rest)
  (False, False) ->
    let x1 = newAtom i
        x2 = newAtom (i + 1)
     in let (i1, rest1) = tseytins' p1 (i + 1)
         in let x3 = newAtom $ i1 + 1
             in let (i2, rest2) = tseytins' p2 $ i1 + 1
                 in (i2, Iff x1 (cons x2 x3) : (rest1 ++ rest2))

tseytins :: LangProp -> LangProp
tseytins p =
  let (_, ps) = tseytins' p 1
   in foldr And (newAtom 1) ps

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
