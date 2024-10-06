module Naive (solve, solve') where

import qualified Data.Set as Set
import Parser ( parseProp )
import LangProp (Identifier (..), LangProp (..))

type Env = Set.Set Identifier

type Vars = Set.Set Identifier

type Answer = Set.Set (Set.Set Identifier)

freeVars :: LangProp -> Vars
freeVars (Atom ident) = Set.singleton ident
freeVars (Entail expr1 expr2) = Set.union (freeVars expr1) (freeVars expr2)
freeVars (And expr1 expr2) = Set.union (freeVars expr1) (freeVars expr2)
freeVars (Or expr1 expr2) = Set.union (freeVars expr1) (freeVars expr2)
freeVars (Not expr) = freeVars expr

eval :: Env -> LangProp -> Bool
eval env (Atom ident) = Set.member ident env
eval env (Entail expr1 expr2) = not (eval env expr1 && not (eval env expr2))
eval env (And expr1 expr2) = eval env expr1 && eval env expr2
eval env (Or expr1 expr2) = eval env expr1 || eval env expr2
eval env (Not expr) = not $ eval env expr

solve :: LangProp -> Answer
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
    Right expr -> let answer = solve expr
                 in Set.toList <$> Set.toList answer