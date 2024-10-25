module DPLL where
import CNF (LangPropCNF(..), size)
import LangProp(Identifier(..))
import Data.Set (Set, fromList, union, member)

-- simple :: Set.Set Identifier -> LangPropCNF -> LangPropCNF
-- simple env (ConCNF clause) =  

extractIdent :: LangPropCNF -> Identifier
extractIdent (AtomCNF ident) = ident
extractIdent (NotCNF (AtomCNF ident)) = ident
extractIdent _ = error "extractIdent"

extractAtomic :: LangPropCNF -> [LangPropCNF]
extractAtomic (DisCNF ps) = ps
extractAtomic _ = error "extractAtomic"

pos :: LangPropCNF -> Bool
pos (AtomCNF _) = True
pos _ = False

neg :: LangPropCNF -> Bool
neg (NotCNF (AtomCNF _)) = True
neg _ = False

simpleDisClause :: Set Identifier -> Set Identifier -> LangPropCNF -> Either Bool LangPropCNF
simpleDisClause poses negs (DisCNF atoms) = go atoms []
    where go [] res = Right (DisCNF res)
          go (hd: rest) res
            | member (extractIdent hd) poses = Left True
            | member (extractIdent hd) negs = go rest res
            | otherwise = go rest (hd : res)
simpleDisClause _ _ _ = error "simpleDisClause"

unitProp :: Set Identifier -> Set Identifier -> [LangPropCNF] -> LangPropCNF
unitProp poses negs clauses = go clauses []
  where go :: [LangPropCNF] -> [LangPropCNF] -> LangPropCNF
        go [] res = ConCNF res
        go (hd: rest) res = let simpled = simpleDisClause poses negs hd
                        in case simpled of
                            Left True -> go rest res
                            Left False -> ConCNF []
                            Right clause -> go rest (clause: res)

solve :: Set Identifier -> LangPropCNF -> Maybe (Set Identifier)
solve env (ConCNF []) = Just env
solve env (ConCNF clauses) = let units = concatMap extractAtomic (filter (\dis -> size dis == 1) clauses)
                               in let poses = fromList $ map extractIdent $ filter pos units
                                      negs = fromList $ map extractIdent $ filter neg units
                                  in let env' =  union env poses
                                     in solve env' $ unitProp poses negs clauses
solve _ _ = error "solve"
