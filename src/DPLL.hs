module DPLL (solve) where

import CNF (LangPropCNF (..), size, transform)
import Data.Set (Set, fromList, member, union, empty)
import LangProp (Identifier (..), LangProp(..))

-- simple :: Set.Set Identifier -> LangPropCNF -> LangPropCNF
-- simple env (ConCNF clause) =

extractIdent :: LangPropCNF -> Identifier
extractIdent (AtomCNF ident) = ident
extractIdent (NotCNF (AtomCNF ident)) = ident
extractIdent _ = error "extractIdent"

extractAtomic :: LangPropCNF -> [LangPropCNF]
extractAtomic (DisCNF ps) = ps
extractAtomic a@(AtomCNF _) = [a]
extractAtomic n@(NotCNF _) = [n]
extractAtomic _ = error "extractAtomic"


pos :: LangPropCNF -> Bool
pos (AtomCNF _) = True
pos _ = False

neg :: LangPropCNF -> Bool
neg (NotCNF (AtomCNF _)) = True
neg _ = False

simpleDisClause :: Set Identifier -> Set Identifier -> LangPropCNF -> Either Bool LangPropCNF
simpleDisClause poses negs (DisCNF atoms) = go atoms []
  where
    go [] res = Right (DisCNF res)
    go (hd : rest) res
      | member (extractIdent hd) poses = Left True -- True | p
      | member (extractIdent hd) negs = go rest res -- False | p
      | otherwise = go rest (hd : res)
simpleDisClause poses negs a@(AtomCNF _) = simpleDisClause poses negs (DisCNF [a])
simpleDisClause poses negs n@(NotCNF _) = simpleDisClause poses negs (DisCNF [n])
simpleDisClause _ _ _ = error "simpleDisClause"

unitProp :: Set Identifier -> Set Identifier -> [LangPropCNF] -> Maybe LangPropCNF
unitProp poses negs clauses = go clauses []
  where
    go :: [LangPropCNF] -> [LangPropCNF] -> Maybe LangPropCNF
    go [] res = Just $ ConCNF res
    go (hd : rest) res =
      let simpled = simpleDisClause poses negs hd
       in case simpled of
            Left True -> go rest res -- True & p
            Left False -> Nothing -- False & p
            Right clause -> go rest (clause : res)

solve' :: Set Identifier -> LangPropCNF -> Maybe (Set Identifier)
solve' env (ConCNF []) = Just env
solve' env (ConCNF clauses) =
  let units = concatMap extractAtomic (filter (\dis -> size dis == 1) clauses)
   in let poses = env `union` fromList (map extractIdent $ filter pos units)
          negs = fromList $ map extractIdent $ filter neg units
       in case unitProp poses negs clauses of
            Just con -> solve' (env `union` poses) con
            Nothing -> Nothing
solve' env d@(DisCNF _) = solve' env (ConCNF [d])
solve' env p = solve' env (ConCNF [DisCNF [p]])

solve :: LangProp -> Maybe (Set Identifier)
solve p = solve' empty (transform p)