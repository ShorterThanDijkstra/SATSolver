module Main (main) where
import LangProp(Identifier(..), LangProp(..))
import Naive
import Parser
import qualified CNF as C 
import qualified LangPropCore as L

test = C.transform . L.transform 
p1 = testProp "(p1 & p2) | p3"

p2 = testProp "((p1 & p2) | p3) | p4"

p3 = testProp "(A & B) | (C & D) | E"

main :: IO ()
main = do
  putStrLn "hello world"
