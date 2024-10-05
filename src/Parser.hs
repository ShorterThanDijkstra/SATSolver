{-# LANGUAGE InstanceSigs #-}

module Parser (LangProp, parseProp) where

import Data.Void (Void)
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L

newtype Identifier = Identifier
  { getId :: String
  }

instance Show Identifier where
  show :: Identifier -> String
  show = getId

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
  show (And expr1 expr2) = show expr1 ++ " & " ++ show expr2
  show (Or expr1 expr2) = show expr1 ++ " | " ++ show expr2

type Parser = Parsec Void String

skipSpace :: Parser ()
skipSpace = L.space space1 (L.skipLineComment ";;") (L.skipBlockCommentNested "/*" "*/")

lexeme :: Parser a -> Parser a
lexeme = L.lexeme skipSpace

parseProp :: String -> Either String LangProp
parseProp input =
  let outputE =
        runParser
          (between skipSpace eof parseExp)
          ""
          input
   in case outputE of
        Left err -> Left $ errorBundlePretty err
        Right output -> Right output

parseExp :: Parser LangProp
parseExp = lexeme entailP

accP :: Parser a -> Parser LangProp -> LangProp -> (LangProp -> LangProp -> LangProp) -> Parser LangProp
accP startP nextP expr cons = lexeme $ do
  start <- lexeme $ optional startP
  case start of
    Nothing -> return expr
    Just _ -> lexeme $ do
      rest <- nextP
      accP startP nextP (cons expr rest) cons

entailP :: Parser LangProp
entailP = label "entail" $ lexeme $ do
  orExp <- orP
  accP (string "->") orP orExp Entail

orP :: Parser LangProp
orP = label "or" $ lexeme $ do
  andExp <- andP
  accP (char '|') andP andExp Or

andP :: Parser LangProp
andP = label "and" $ lexeme $ do
  notExp <- notP
  accP (char '&') notP notExp And

notP :: Parser LangProp
notP = label "not" $ lexeme $ do
  not <- lexeme $ optional $ char '!'
  case not of
    Nothing -> termP
    Just _ -> Not <$> termP

termP :: Parser LangProp
termP = label "term" $ lexeme $ do
  fst <- lexeme $ optional $ char '('
  case fst of
    Nothing -> Atom <$> identP
    Just _ -> do
      exp <- parseExp
      char ')'
      return exp

punc :: [Parser Char]
punc = char <$> ['=', '?']

identP :: Parser Identifier
identP = label "identifier" $ lexeme $ do
  first <- letterChar <|> char '_'
  rest <- many $ alphaNumChar <|> char '_' <|> choice punc
  return $ Identifier $ first : rest
