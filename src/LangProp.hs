{-# LANGUAGE InstanceSigs #-}

module LangProp (LangProp(..), Identifier (..)) where

newtype Identifier = Identifier {getId :: String}

instance Show Identifier where
  show :: Identifier -> String
  show = getId

instance Eq Identifier where
  (==) :: Identifier -> Identifier -> Bool
  (Identifier str1) == (Identifier str2) = str1 == str2 

instance Ord Identifier where
  (<=) :: Identifier -> Identifier -> Bool
  (Identifier str1) <= (Identifier str2) = str1 <= str2 
  
data LangProp
  = Atom Identifier
  | Not LangProp
  | Entail LangProp LangProp
  | And LangProp LangProp
  | Or LangProp LangProp

instance Show LangProp where
  show :: LangProp -> String
  show (Atom ident) = show ident
  show (Not expr) = "!" ++ show expr
  show (Entail expr1 expr2) = "(" ++ show expr1 ++ " -> " ++ show expr2 ++ ")"
  show (And expr1 expr2) = "(" ++ show expr1 ++ " & " ++ show expr2 ++ ")"
  show (Or expr1 expr2) = "(" ++ show expr1 ++ " | " ++ show expr2 ++ ")"

