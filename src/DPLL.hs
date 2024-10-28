module DPLL where

import CNF (LangPropCNF (..), size, transform, atomics)
import Data.Set (Set, empty, fromList, member, singleton, union, difference)
import LangProp (Identifier (..), LangProp (..) )
import Data.List (maximumBy, sort, maximumBy, group)


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

simpleAtom :: Set Identifier -> Set Identifier -> LangPropCNF -> Maybe Bool
simpleAtom poses negs (AtomCNF ident)
  | member ident poses = Just True
  | member ident negs = Just False
  | otherwise = Nothing
simpleAtom poses negs (NotCNF (AtomCNF ident))
  | member ident poses = Just False
  | member ident negs = Just True
  | otherwise = Nothing
simpleAtom _ _ _ = error "simpleAtom"

simpleDisClause :: Set Identifier -> Set Identifier -> LangPropCNF -> Either Bool LangPropCNF
simpleDisClause poses negs (DisCNF atoms) = go atoms []
  where
    go [] [] = Left False
    go [] res = Right (DisCNF res)
    go (hd : rest) res = case simpleAtom poses negs hd of
      Just True -> Left True
      Just False -> go rest res
      Nothing -> go rest (hd : res)
simpleDisClause poses negs a@(AtomCNF _) = simpleDisClause poses negs (DisCNF [a])
simpleDisClause poses negs n@(NotCNF _) = simpleDisClause poses negs (DisCNF [n])
simpleDisClause _ _ _ = error "simpleDisClause"

simpleConClause :: Set Identifier -> Set Identifier -> LangPropCNF -> Either Bool LangPropCNF
simpleConClause poses negs (ConCNF clauses) = go clauses []
  where
    go :: [LangPropCNF] -> [LangPropCNF] -> Either Bool LangPropCNF
    go [] [] = Left True
    go [] res = Right $ ConCNF res
    go (hd : rest) res =
      let simpled = simpleDisClause poses negs hd
       in case simpled of
            Left True -> go rest res -- True & p
            Left False -> Left False -- False & p
            Right clause -> go rest (clause : res)
simpleConClause _ _ _ = error "simpleConClause"

findIdent :: LangPropCNF -> Identifier
findIdent p = mostFrequent $ map extractIdent (atomics p)
  where 
      compareFrequency :: (Identifier, Int) -> (Identifier, Int) -> Ordering
      compareFrequency (_, count1) (_, count2)
          | count1 > count2 = LT
          | count1 < count2 = GT
          | otherwise       = EQ

      mostFrequent ::  [Identifier] -> Identifier
      mostFrequent xs = fst $ maximumBy compareFrequency [(head g, length g) | g <- (group $ sort xs)]

pureDis :: LangPropCNF -> (Set Identifier, Set Identifier) 
pureDis (DisCNF atomics) = let poses = fromList $ map extractIdent (filter pos atomics)
                               negs = fromList $ map extractIdent (filter neg atomics)
                           in (poses `difference` negs, negs `difference` poses)
pureDis a@(AtomCNF _) = pureDis $ DisCNF [a]
pureDis n@(NotCNF (AtomCNF _)) = pureDis $ DisCNF [n]
pureDis _ = error "pureDis"

pureCon :: LangPropCNF -> (Set Identifier, Set Identifier)
pureCon (ConCNF dis) = go empty empty dis 
    where go :: Set Identifier -> Set Identifier -> [LangPropCNF] -> (Set Identifier, Set Identifier)
          go poses negs [] = (poses, negs)
          go poses negs (hd: rest) = let (poses', negs') = pureDis hd 
                                     in go ((poses `union` poses') `difference` (negs `union` negs')) 
                                            ((negs `union` negs') `difference` (poses `union` poses')) 
                                            rest
pureCon d@(DisCNF _) = pureCon $ ConCNF [d]
pureCon atomic = pureCon $ ConCNF [DisCNF [atomic]]

posesAndNegs :: LangPropCNF -> (Set Identifier, Set Identifier)
posesAndNegs c@(ConCNF clauses) =
  let units = concatMap extractAtomic (filter (\dis -> size dis == 1) clauses)
      ident = findIdent c
      pures = pureCon c
   in if null units
        then (singleton ident, empty)
        else (fromList (map extractIdent (filter pos units)), fromList $ map extractIdent $ filter neg units)
posesAndNegs _ = error "posesAndNegs"

-- Unit propagation and Pure literal elimination
solve' :: Set Identifier -> LangPropCNF -> Maybe (Set Identifier)
solve' env (ConCNF []) = Just env
solve' env c@(ConCNF _) =
  let (poses, negs) = posesAndNegs c
      newEnv = (env `union` poses)
   in case simpleConClause poses negs c of
        Right con -> solve' newEnv con
        Left True -> Just newEnv
        Left False -> Nothing
solve' env d@(DisCNF _) = solve' env (ConCNF [d])
solve' env atomic = solve' env (ConCNF [DisCNF [atomic]])

solve :: LangProp -> Maybe (Set Identifier)
solve p = solve' empty (transform p)
