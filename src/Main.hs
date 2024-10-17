module Main (main) where
import Naive
import Parser (parseProp)
import TruthTable ( table)
import qualified CNF as C
import qualified LangProp as LP

-- helper 
p :: (LP.LangProp -> String) -> String -> String
p f s = case parseProp s of
  Left _ -> "error"
  Right r -> f r

-- parse LangProp
lp :: String -> String
lp = p show

-- truth table
tt :: String -> String
tt = p (show . table)

-- cnf :: String -> String
-- cnf = p (show . C.transform)

s = "((p | q) & r) -> (!s)"

main :: IO ()
main = do
  putStrLn "hello world"
