module Main (main) where
import Naive
import Parser (parseProp)
import TruthTable (table, subclauses)
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

cnf :: String -> String
cnf = p (show . C.transform)

s :: String -> String
s = p (show . C.size . C.transform)

t :: String -> String 
t = p (show . C.transform)

ts :: String -> String 
ts = p (show . C.tseytins)

sub :: String -> [LP.LangProp]
sub s = case parseProp s of 
  Left _ ->  []
  Right p -> subclauses p
  
s0 = "(r -> p) -> (!(q & r) -> p)"

s1 = "((p | q) & r) -> (!s)"

s2 = "(!p | (r | q))"

s3 = "((p1 | (p2 | p3)) & (((!p4 | p5) | p6) & p8))"

s4 = "p1 <-> p2"

s5 = "p1 <-> p2 <-> p3"

s6 = "p1 <-> p2 <-> p3 <-> p4"

s7 = "p1 <-> p2 <-> p3 <-> p4 <-> p5"

s8 = "p1 <-> p2 <-> p3 <-> p4 <-> p5 <-> p6"

s9 = "(p -> q) -> (q & p)"

main :: IO ()
main = do
  putStrLn "hello world"
