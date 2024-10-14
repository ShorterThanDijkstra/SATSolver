{-# LANGUAGE GADTs #-}
module CNF where
import LangPropCore(Identifier(..), LangPropCore(..))
import Debug.Trace (trace)

data LangCNF = AtomCNF Identifier | 
               NotCNF LangCNF | -- atom 
               DisCNF [LangCNF] | -- atom or not 
               ConCNF [LangCNF] -- dis 
               deriving Show 

-- transform :: LangPropCore -> LangCNF
-- transform p@(Atom _) = ConCNF [p]
-- transform p@(Not (Atom _)) = p
-- transform (Not (Not p)) = p
-- transform (Not (And p1 p2)) = transform (Or (Not p1) (Not p2))
-- transform (Not (Or p1 p2)) = transform (And (Not p1) (Not p2))
-- transform (And p1 p2) = And (transform p1) (transform p2)
-- transform p@(Or (Atom _) (Atom _)) = p
-- transform p@(Or (Atom _) (Not _)) = p
-- transform (Or p1@(Atom _) (And p2 p3)) = Or (And p1 (transform p2)) (And p1 (transform p3))
-- transform p@(Or (Atom _) (Or _ _)) = p

-- transform p@(Or (Not _) (Atom _)) = p
-- transform p@(Or (Not _) (Not _)) = p
-- transform p@(Or p1 (And p2 p3)) = And (transform (Or p1 p2)) (transform (Or p1 p3))
-- transform p@(Or (And p1 p2) p3) = And (transform (Or p1 p3)) (transform (Or p2 p3))
-- transform p@(Or p1 p2) = trace (show p) transform $ Or (transform p1) (transform p2)

-- l
transform :: LangPropCore -> LangPropCore
-- p => p
transform p@(Atom _) = p
-- !p => p
transform p@(Not (Atom _)) = p
-- !!P => P
transform (Not (Not p)) = transform p
-- !(P1 & P2) => !P1 | !P2
transform (Not (And p1 p2)) = transform (Or (Not p1) (Not p2))
-- !(P1 | P2) => !P1 & !P2
transform (Not (Or p1 p2)) = transform (And (Not p1) (Not p2))
-- P1 & P2 => P1 & P2
transform (And p1 p2) = And (transform p1) (transform p2)
-- p1 | p2 => p1 | p2
transform p@(Or (Atom _) (Atom _)) = p
-- p1 | !P2 =>  p1 | !P2
transform (Or p1@(Atom _) p2@(Not _)) = transform $ (Or p1 $ transform p2)
-- p1 | (P2 & P3) => (p1 | P2) & (p1 | P3)
transform (Or p1@(Atom _) (And p2 p3)) = And (transform (Or p1 (transform p2))) $ transform (Or p1 (transform p3))
-- p1 | P2 | P3 => p1 | P2 | P3
transform (Or p1@(Atom _) (Or p2 p3)) = transform (Or p1 transform (Or (transform p2) (transform p3)))
-- !P1 | p2 => !P1 | p2
transform (Or p1@(Not _) p2@(Atom _)) = transform $ Or (transform p1) p2  
-- !P1 | !P2 => !P1 | !P2
transform (Or p1@(Not _) p2@(Not _)) = transform $ Or (transform p1) (transform p2)
-- P1 | (P2 & P3) => P1 | P2
transform (Or p1@(Not _) (And p2 p3)) =  transform $ Or (And (transform p1) (transform p2)) (And (transform p1) (transform p3))
transform p@(Or (Not _) (Or _ _)) = p
-- (p1 & p2) | p3 => (p1 | p3) & (p2 | p3)
transform (Or (And p1 p2) p3@(Atom _)) = (And (Or (
transform (Or (And p1 p2) p3@(Not _)) = transform $ Or (And p1 (transform p3)) (And p2 (transform p3))
transform (Or (And p1 p2) p3@(And _ _)) = transform $ Or (And p1 (transform p3)) (And p2 (transform p3))

transform p@(Or p1 (And p2 p3)) = And (transform (Or p1 p2)) (transform (Or p1 p3))
transform p@(Or (And p1 p2) p3) = And (transform (Or p1 p3)) (transform (Or p2 p3))
transform p@(Or p1 p2) = trace (show p) transform $ Or (transform p1) (transform p2)

{- transform :: LangProp -> LangCNF 
transform (Atom ident) = AtomCNF ident
transform (Not p) = case transform p of 
                        t@(AtomCNF _) -> NotCNF t 
                        t@(NotCNF cnf) -> cnf
                        t@(DisCNF cnf1 cnf2) -> ConCNF (NotCNF cnf1) (NotCNF cnf2)
                        t@(ConCNF cnf1 cnf2) -> DisCNF (NotCNF cnf1) (NotCNF cnf2)
transform (Entail p1 p2) = transform (Or (Not p1) p2)
transform (And p1 p2) = ConCNF (transform p1) (transform p2)
transform (Or p1 p2) = case (transform p1, transform p2) of 
                        ((ConCNF cnf1 cnf2), t) -> ConCNF (transform (DisCNF cnf1 t)) (DisCNF cnf2 t)
                        (t, (ConCNF cnf1 cnf2)) -> ConCNF (DisCNF t cnf1) (DisCNF t cnf2)
 -}
-- data LangCNF ty where 
--     IdentCNF :: Identifier -> LangCNF ty
--     NotCNF  :: LangProp -> LangCNF ty
--     DisCNF  :: LangCNF Identifier -> LangCNF Identifier -> LangCNF ty
--     ConCNF  :: LangCNF ty -> LangCNF ty -> LangCNF ty 


-- transform :: LangProp -> LangCNF ty
-- transform (Atom ident) = IdentCNF ident
-- transform (Not p) = NotCNF p 
-- transform (Entail p q) = DisCNF (transform (Not p)) (transform q)
-- transform (Or p q) = DisCNF (transform p) (transform q)
-- transform (And p q) = ConCNF (transform p) (transform q)

-- p = testProp "(p & q) | p | q"

-- cnf :: LangCNF ty
-- cnf = transform p

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
