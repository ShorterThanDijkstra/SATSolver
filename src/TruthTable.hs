{-# LANGUAGE InstanceSigs #-}

module TruthTable (TruthTable, table, subclauses) where

import Control.Exception.Base (assert)
import Data.List (intercalate, nub)
import Debug.Trace (trace)
import LangProp (Identifier (..), LangProp (..))

type Env = [Identifier]

type Vars = [Identifier]

data TruthTable = TruthTable LangProp Vars [LangProp] [([Bool], [Bool], Bool)]

maximum' [] = 0
maximum' xs = maximum xs

instance Show TruthTable where
  show :: TruthTable -> String
  show (TruthTable p vars subs rows) =
    let widthAtom = roundM 4 $ maximum' $ map (length . show) vars
        widthSub = roundM 4 $ maximum' $ map (length . show) subs
        width = max widthAtom widthSub
        largeDiff' = largeDiff widthAtom widthSub
     in let widthAtom' = if largeDiff' then widthAtom else width
            widthSub' = if largeDiff' then widthSub else width
            subsStr = unwords (map (\sub -> inMiddle (4 + length (show sub)) (show sub)) subs)
            pStr = show p
            th = joinWith (inMiddle widthAtom') (map show vars) ++ subsStr ++ " | " ++ pStr
            sepRow = "\n" ++ map (const '_') th ++ "\n"
            rows' = map (\(world, subVals, val) -> (map showBool world, map showBool subVals, showBool val)) rows
            rows'' = intercalate sepRow $ map (\(heads, subVals, last) -> joinWith (inMiddle widthAtom') heads ++
                     unwords (zipWith (\ sub val -> inMiddle (4 + length (show sub)) val) subs subVals)
                     ++ " | " ++ inMiddle (length pStr) last) rows'
         in th ++ sepRow ++ rows''
    where
      largeDiff :: Int -> Int -> Bool
      largeDiff i1 i2 = if i1 > i2 then largeDiff i2 i1 else i1 * 2 < i2

      inMiddle :: Int -> String -> String
      inMiddle width s =
        assert
          (length s <= width)
          ( let diff = width - length s
                fst = diff `div` 2
                snd = if even diff then fst else fst + 1
             in (replicate fst ' ' ++ s ++ replicate snd ' ')
          )

      joinWith :: (String -> String) -> [String] -> String
      joinWith f s = unwords $ map f s

      roundM :: Int -> Int -> Int
      roundM m n = (n `div` m + 1) * m

      showBool True = "1"
      showBool False = "0"

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

eval :: Env -> LangProp -> Bool
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

subclauses :: LangProp -> [LangProp]
subclauses (Atom _) = []
subclauses (Not p) = reverse $ subclauses' p
subclauses (Or p1 p2) = reverse $ nub $ subclauses' p2 ++ subclauses' p1
subclauses (And p1 p2) = reverse $ nub $ subclauses' p2 ++ subclauses' p1
subclauses (If p1 p2) = reverse $ nub $ subclauses' p2 ++ subclauses' p1
subclauses (Iff p1 p2) = reverse $ nub $ subclauses' p2 ++ subclauses' p1

subclauses' :: LangProp -> [LangProp]
subclauses' (Atom _) = []
subclauses' n@(Not p) = n : nub (subclauses' p)
subclauses' o@(Or p1 p2) = o : nub (subclauses' p2 ++ subclauses' p1)
subclauses' a@(And p1 p2) = a : nub (subclauses' p2 ++ subclauses' p1)
subclauses' i@(If p1 p2) = i : nub (subclauses' p2 ++ subclauses' p1)
subclauses' i@(Iff p1 p2) = i : nub (subclauses' p2 ++ subclauses' p1)

table :: LangProp -> TruthTable
table p =
  let vars = freeVars p
      ws = worlds vars
      subs = subclauses p
   in go vars ws subs
  where
    go :: Vars -> [[Bool]] -> [LangProp] -> TruthTable
    go vars ws subs =
      let rows =
            map
              ( \w ->
                  let truthVars = filter snd (zip vars w)
                      env = map fst truthVars
                   in (w, map (eval env) subs, eval env p)
              )
              ws
       in TruthTable p vars (subclauses p) rows
