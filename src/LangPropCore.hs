{-# LANGUAGE InstanceSigs #-}
module LangPropCore  (LangPropCore(..), Identifier(..), transform) where

import LangProp(Identifier(..), LangProp(..))

data LangPropCore 
  = Atom  Identifier
  | Not  LangPropCore
  | And  LangPropCore LangPropCore
  | Or  LangPropCore LangPropCore
  deriving Eq
  
instance Show LangPropCore where
  show :: LangPropCore -> String
  show (LangPropCore.Atom ident) = show ident
  show (LangPropCore.Not expr) = "!" ++ show expr
  show (LangPropCore.And expr1 expr2) = "(" ++ show expr1 ++ " & " ++ show expr2 ++ ")"
  show (LangPropCore.Or expr1 expr2) = "(" ++ show expr1 ++ " | " ++ show expr2 ++ ")"

transform :: LangProp -> LangPropCore
transform (LangProp.Atom ident) = LangPropCore.Atom ident
transform (LangProp.Not p) = LangPropCore.Not $ transform p 
transform (LangProp.Entail p1 p2) = LangPropCore.Or (LangPropCore.Not $ transform p1) $ transform p2
transform (LangProp.Iff p1 p2) = LangPropCore.And (transform (LangProp.Entail p1 p2)) $ transform (LangProp.Entail p2 p1)
transform (LangProp.And p1 p2) = LangPropCore.And (transform p1) $ transform p2 
transform (LangProp.Or p1 p2) = LangPropCore.Or (transform p1) $ transform p2