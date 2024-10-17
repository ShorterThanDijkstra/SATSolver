module Parser (LangProp, parseProp, parseProp') where

import Data.Void (Void)
import LangProp (Identifier(..), LangProp(..))
import Text.Megaparsec
  ( MonadParsec (eof, label),
    Parsec,
    between,
    choice,
    errorBundlePretty,
    many,
    optional,
    runParser,
    (<|>),
  )
import Text.Megaparsec.Char (alphaNumChar, char, letterChar, space1, string)
import qualified Text.Megaparsec.Char.Lexer as L

type Parser = Parsec Void String

skipSpace :: Parser ()
skipSpace = L.space space1 (L.skipLineComment ";;") (L.skipBlockCommentNested "/*" "*/")

lexeme :: Parser a -> Parser a
lexeme = L.lexeme skipSpace

parseProp' :: String -> LangProp 
parseProp' input = case parseProp input of 
  Left _ -> Atom $ Identifier "parse_error"
  Right p -> p
  
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

accP :: Parser a -> Parser LangProp -> LangProp -> (LangProp -> LangProp -> LangProp) -> Parser LangProp
accP startP nextP expr cons = lexeme $ do
  start <- lexeme $ optional startP
  case start of
    Nothing -> return expr
    Just _ -> lexeme $ do
      rest <- nextP
      accP startP nextP (cons expr rest) cons

parseExp :: Parser LangProp
parseExp = lexeme iffP

iffP :: Parser LangProp 
iffP = label "iff" $ lexeme $ do 
  ifExp <- ifP
  accP (string "<->") ifP ifExp Iff

ifP :: Parser LangProp
ifP = label "if" $ lexeme $ do
  orExp <- orP
  accP (string "->") orP orExp If

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
  fst <- lexeme $ optional $ choice [char '(', char '!']
  case fst of
    Nothing -> Atom <$> identP
    Just '(' -> do
      exp <- parseExp
      char ')'
      return exp
    Just '!' -> do 
      t <- termP
      return $ Not t


punc :: [Parser Char]
punc = char <$> ['=', '?', '>', '<', '/', '@']

identP :: Parser Identifier
identP = label "identifier" $ lexeme $ do
  first <- letterChar <|> char '_'
  rest <- many $ alphaNumChar <|> char '_' <|> choice punc
  return $ Identifier $ first : rest
