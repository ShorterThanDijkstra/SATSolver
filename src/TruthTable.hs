module TruthTable (table) where
import Parser ( parseProp )
import LangPropCore (Identifier (..), LangPropCore (..), transform)
import Data.List (nub)
type Env = [Identifier]

type Vars = [Identifier]

data TruthTable = TruthTable LangPropCore [Identifier] [([Bool], Bool)] deriving Show

union :: Vars -> Vars -> Vars
union vars1 vars2 = nub $ vars1 ++ vars2

member :: Identifier -> Env -> Bool
member = elem

freeVars :: LangPropCore -> Vars
freeVars (Atom ident) = [ident]
freeVars (And expr1 expr2) = freeVars expr1 `union` freeVars expr2
freeVars (Or expr1 expr2) = freeVars expr1 `union` freeVars expr2
freeVars (Not expr) = freeVars expr

eval :: Env -> LangPropCore -> Bool
eval env (Atom ident) = member ident env
eval env (And expr1 expr2) = eval env expr1 && eval env expr2
eval env (Or expr1 expr2) = eval env expr1 || eval env expr2
eval env (Not expr) = not $ eval env expr

worlds :: Vars -> [[Bool]]
worlds vars = go $ length vars
    where go :: Int -> [[Bool]]
          go 0 = [[]]
          go n = let rest = go $ n - 1
                 in map (False :) rest ++
                    map (True :) rest

table :: LangPropCore -> TruthTable
table p = let vars = freeVars p
              ws = worlds vars 
	  in go vars ws 
    where go :: Vars -> [[Bool]]-> TruthTable
          go vars ws = let rows = map (\w -> let truthVars = filter (\(ident, val) -> val) (zip vars w)
						 env = map (\(ident, val) -> ident) truthVars
					     in (w, eval env p))
				  ws
                       in TruthTable p vars rows
