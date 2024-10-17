{-# LANGUAGE InstanceSigs #-}

module TruthTable (TruthTable, table) where

import Control.Exception.Base (assert)
import Data.List (intercalate, nub)
import LangProp (Identifier(..), LangProp(..))

import qualified Data.Bifunctor as B


type Env = [Identifier]

type Vars = [Identifier]

data TruthTable = TruthTable LangProp Vars [([Bool], Bool)]

instance Show TruthTable where
  show :: TruthTable -> String
  show (TruthTable p vars rows) =
    let width = roundM 4 $ maximum $ map (length . show) vars
        pStr = show p
        th = joinWith (inMiddle width) (map show vars) ++ " | " ++ pStr
        sepRow = "\n" ++ map (const '_') th ++ "\n"
        rows' = map (B.bimap (map showBool) showBool) rows
        rows'' = intercalate sepRow $ map (\(heads, last) -> joinWith (inMiddle width) heads ++ " | " ++ inMiddle (length pStr) last) rows'
     in th ++ sepRow ++ rows''
    where
      inMiddle width s =
        assert
          (length s <= width)
          ( let diff = width - length s
                fst = diff `div` 2
                snd = if even diff then fst else fst + 1
             in (replicate fst ' ' ++ s ++ replicate snd ' ')
          )

      joinWith f s = unwords $ map f s

      roundM m n = (n `div` m + 1) * m

      showBool True = "T"
      showBool False = "F"

union :: Vars -> Vars -> Vars
union vars1 vars2 = nub $ vars1 ++ vars2

member :: Identifier -> Env -> Bool
member = elem

freeVars :: LangProp -> Vars
freeVars (Atom ident) = [ident]
freeVars (And expr1 expr2) = freeVars expr1 `union` freeVars expr2
freeVars (Or expr1 expr2) = freeVars expr1 `union` freeVars expr2
freeVars (Not expr) = freeVars expr
freeVars (If expr1 expr2) = freeVars expr1 `union` freeVars expr2 
freeVars (Iff expr1 expr2) = freeVars expr1 `union` freeVars expr2 

eval :: Env -> LangProp-> Bool
eval env (Atom ident) = member ident env
eval env (And expr1 expr2) = eval env expr1 && eval env expr2
eval env (Or expr1 expr2) = eval env expr1 || eval env expr2
eval env (Not expr) = not $ eval env expr
eval env (If expr1 expr2) = not (eval env expr1) || eval env expr2
eval env (Iff expr1 expr2) = eval env (If expr1 expr2) && eval env (If expr2 expr1) 

worlds :: Vars -> [[Bool]]
worlds vars = go $ length vars
  where
    go :: Int -> [[Bool]]
    go 0 = [[]]
    go n =
      let rest = go $ n - 1
       in map (False :) rest
            ++ map (True :) rest

table :: LangProp -> TruthTable
table p =
  let vars = freeVars p
      ws = worlds vars
   in go vars ws
  where
    go :: Vars -> [[Bool]] -> TruthTable
    go  vars ws =
      let rows =
            map
              ( \w ->
                  let truthVars = filter snd (zip vars w)
                      env = map fst truthVars
                   in (w, eval env p)
              )
              ws
       in TruthTable p vars rows
