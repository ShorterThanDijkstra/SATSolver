module Naive (solve, solve') where

import qualified Data.Set as Set
import Parser ( parseProp )
import LangProp (Identifier (..), LangProp (..))
import Data.Bool (Bool)


type Env = Set.Set Identifier

type Vars = Set.Set Identifier

type Answer = Set.Set (Set.Set Identifier)

freeVars :: LangProp -> Vars
freeVars (Atom ident) = Set.singleton ident
freeVars (And expr1 expr2) = Set.union (freeVars expr1) (freeVars expr2)
freeVars (Or expr1 expr2) = Set.union (freeVars expr1) (freeVars expr2)
freeVars (Not expr) = freeVars expr
freeVars (If expr1 expr2) = Set.union (freeVars expr1) (freeVars expr2)
freeVars (Iff expr1 expr2) =  Set.union (freeVars expr1)  (freeVars expr2)

eval :: Env -> LangProp -> Bool
eval env (Atom ident) = Set.member ident env
eval env (And expr1 expr2) = eval env expr1 && eval env expr2
eval env (Or expr1 expr2) = eval env expr1 || eval env expr2
eval env (Not expr) = not $ eval env expr
eval env (If expr1 expr2) = not (eval env expr1) || eval env expr2
eval env (Iff expr1 expr2) = eval env (If expr1 expr2) && eval env (If expr2 expr1) 

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
    Right p -> let answer = solve p
                 in Set.toList <$> Set.toList answer