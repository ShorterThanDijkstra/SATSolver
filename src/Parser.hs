module Parser () where 

import Data.Void (Void)
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L

newtype Identifier = Identifier
  { getId :: String
  } deriving (Show)

data SExp
  = SSExp    SExp [SExp]  -- (foo 0 "hello" bar), (bar (baz 1)), (foo)
  | SInteger Integer  -- 42
  | SDouble  Double -- 42f, 3.1415f
  | SString  String  -- "hello, world"
  | SBool    Bool  -- false, true
  | SId      Identifier  -- foo
  deriving (Show)
  
type Parser = Parsec
  -- The type for custom error messages. We have none, so use `Void`.
  Void
  -- The input stream type. Let's use `String` for now, but for
  -- better performance, you might want to use `Text` or `ByteString`.
  String

bool :: Parser Bool
bool = False <$ string "false" <|> True <$ string "true"

atom :: Parser SExp
atom = lexeme $ choice [SBool <$> bool, numeric, SString <$> str, SId <$> identifier, uncurry SSExp <$> sexp]

numeric :: Parser SExp
numeric = label "number" $ lexeme $ do
  -- Left side before . or f
  left <- some numberChar

  -- .0f or f
  rightM <- optional $ choice
    [ do
        char '.'
        right <- some numberChar
        char' 'f'
        pure $ left ++ "." ++ right
    , do
        char' 'f'
        pure left
    ]

  -- If we had the right side, it's a double, otherwise an integer.
  pure $ case rightM of
    Nothing -> SInteger $ read left
    Just right -> SDouble $ read right
    
{- 
integer :: Parser Integer
integer = label "integer" $ read <$> (some numberChar <|> ((:) <$> char '-') <*> some numberChar)
 -}

str :: Parser String 
str = label "string" $ between (char '"') (char '"') (takeWhileP Nothing (/= '"'))

identifier:: Parser Identifier
identifier = label "identifier" $ do 
    first <- letterChar <|> char '_'
    rest <- many $ alphaNumChar <|> char '_' 
    return $ Identifier $ first :rest

skipSpace :: Parser ()
skipSpace = L.space space1 (L.skipLineComment ";;") (L.skipBlockCommentNested "/*" "*/")

lexeme :: Parser a -> Parser a
lexeme = L.lexeme skipSpace

sexp :: Parser (SExp, [SExp])
sexp = label "S-expression" $ lexeme $ between (lexeme (char '(')) (char ')') ((,) <$> atom <*> many atom)

parseSExp :: String -> Either String SExp 
parseSExp input = 
  let outputE = parse (between skipSpace eof atom)
                      ""        
                      input
  in case outputE of 
    Left err -> Left $ errorBundlePretty err 
    Right output -> Right output