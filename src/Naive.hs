module Naive (solve, solve') where

import qualified Data.Set as Set
import Parser ( parseProp )
import LangPropCore (Identifier (..), LangPropCore (..), transform)
import CNF (transform)

type Env = Set.Set Identifier

type Vars = Set.Set Identifier

type Answer = Set.Set (Set.Set Identifier)

freeVars :: LangPropCore -> Vars
freeVars (Atom ident) = Set.singleton ident
freeVars (And expr1 expr2) = Set.union (freeVars expr1) (freeVars expr2)
freeVars (Or expr1 expr2) = Set.union (freeVars expr1) (freeVars expr2)
freeVars (Not expr) = freeVars expr

eval :: Env -> LangPropCore -> Bool
eval env (Atom ident) = Set.member ident env
eval env (And expr1 expr2) = eval env expr1 && eval env expr2
eval env (Or expr1 expr2) = eval env expr1 || eval env expr2
eval env (Not expr) = not $ eval env expr

solve :: LangPropCore -> Answer
solve expr =
  let vars = freeVars expr
      varsPower = Set.powerSet vars
   in foldr
        ( \env answer ->
            if eval env expr
              then Set.insert env answer
              else answer
        )
        Set.empty
        varsPower

solve' :: String -> [[Identifier]]
solve' input = case parseProp input of 
    Left _ -> []
    Right expr -> let answer = solve $ LangPropCore.transform expr
                 in Set.toList <$> Set.toList answer